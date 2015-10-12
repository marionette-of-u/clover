#define _USE_MATH_DEFINES
#include <chrono>
#include <memory>
#include <thread>
#include <atomic>
#include <complex>
#include <algorithm>
#include <cmath>
#include <cstdlib>
#include <cstring>
#include <portaudio.h>
#include "audiodecoder.h"
#include "bmp.hpp"

extern std::atomic<bool> is_running;

namespace object{
    extern volatile float peek_spectrum[2][256];
}

#define PA_SAMPLE_TYPE paFloat32
using sample_t = float;

using complex_t = std::complex<float>;

// ビット長を得る
int bit_num(int t){
    int n = 0;
    while(t > 0){
        ++n;
        t >>= 1;
    }
    return n;
}

// ビット順序を反転する
int bit_rev(int a){
    if(a == 0){
        return a;
    }
    int n = bit_num(a);
    int r = 0;
    if(n % 2 == 1){
        r |= ((a >> (n / 2)) & 1) << (n / 2);
    }
    for(int i = 0; i < n / 2; ++i){
        r |= ((a >> i) & 1) << (n - 1 - i);
        r |= ((a >> (n - 1 - i)) & 1) << i;
    }
    return r;
}

// ビットリバースコピー (hann窓)
void bit_rev_copy_hann(const float *a, complex_t *A, int n){
    for(int i = 0; i < n; ++i){
        A[bit_rev(i)] = a[i] * (0.5 - 0.5 * std::cos(2.0 * M_PI * i / n));
    }
}

// ビットリバースコピー (vorbis窓)
void bit_rev_copy_vorbis(const float *a, complex_t *A, int n){
    for(int i = 0; i < n; ++i){
        float v = std::sin(M_PI * i / n);
        v *= v;
        A[bit_rev(i)] = a[i] * std::sin(M_PI * v / 2);
    }
}

// 要素数2^lg_nでFFT処理
// a[1 << lg_n] : input
// A[1 << lg_n] : output
void fft(const float *a, complex_t *A, int lg_n){
    int n = 1 << lg_n;
    bit_rev_copy_vorbis(a, A, n);
    for(int s = 1; s <= lg_n; ++s){
        int m = 1 << s;
        complex_t omega_m(std::cos(2.0 * M_PI / m), std::sin(2.0 * M_PI / m)), omega = 1.0;
        for(int j = 0; j < m / 2; ++j){
            for(int k = j; k < n; k += m){
                complex_t t = omega * A[k + m / 2], u = A[k];
                A[k] = u + t;
                A[k + m / 2] = u - t;
            }
            omega *= omega_m;
        }
    }
}

extern int ch_num;
int ch_num;

extern std::atomic<float> progress;
unsigned long progress_per_samples;

const int lg_spectrum_length = 8;
extern const int spectrum_length;
const int spectrum_length = 1 << lg_spectrum_length;

static float fft_input_signal[spectrum_length * 2];
static complex_t fft_output_signal[spectrum_length * 2];

extern float fft_max_power;
float fft_max_power = 0.0;

float power(const complex_t &c){
    return c.real() * c.real() + c.imag() * c.imag();
}

struct fft_max_power_initer_type{
    fft_max_power_initer_type(){
        for(int i = 0; i < spectrum_length * 2; ++i){
            fft_input_signal[i] = std::sin((spectrum_length / 2) * 2.0 * M_PI * static_cast<float>(i) / (spectrum_length * 2));
        }
        fft(fft_input_signal, fft_output_signal, lg_spectrum_length);
        for(int i = 0; i < spectrum_length; ++i){
            float v = power(fft_output_signal[i]);
            if(v > fft_max_power){
                fft_max_power = v;
            }
        }
    }
} fft_max_power_initer;

const int buffer_length = 1024;
volatile float volume_subst[2][buffer_length];
extern volatile float *volume[2];
volatile float *volume[2] = { volume_subst[0], volume_subst[1] };

std::atomic<bool> is_playing;

void sound_test(){
    using namespace std::literals;
    PaError err;

    std::unique_ptr<AudioDecoder> decoder(new AudioDecoder("SelfDestruction.mp3"));
    decoder->open();
    ch_num = decoder->channels();

    progress = 0.0;
    progress_per_samples = 0;
    
    PaStreamParameters outputParameters;
    outputParameters.device = Pa_GetDefaultOutputDevice();
    outputParameters.channelCount = decoder->channels();
    outputParameters.sampleFormat = PA_SAMPLE_TYPE;
    outputParameters.suggestedLatency = Pa_GetDeviceInfo(outputParameters.device)->defaultLowOutputLatency;
    outputParameters.hostApiSpecificStreamInfo = NULL;

    int (*callback)(
        const void *inputBuffer,
        void *outputBuffer,
        unsigned long framesPerBuffer,
        const PaStreamCallbackTimeInfo* timeInfo,
        PaStreamCallbackFlags statusFlags,
        void *userData
    );

    callback = [](
        const void *inputBuffer,
        void *outputBuffer,
        unsigned long frameCount,
        const PaStreamCallbackTimeInfo* timeInfo,
        PaStreamCallbackFlags statusFlags,
        void *userData
    ) -> int{
        if(!is_running){
            is_playing = false;
            return paComplete;
        }

        AudioDecoder *data = (AudioDecoder*)userData;
        sample_t *out = (sample_t*)outputBuffer;

        std::memset(out, 0, frameCount * data->channels() * sizeof(sample_t));
        int samplesRead = data->read(frameCount * data->channels(), out);
        progress_per_samples += frameCount;

        int channels = data->channels();
        int len = data->numSamples() / channels;
        progress = static_cast<float>(progress_per_samples) / static_cast<float>(len);

        int n = static_cast<int>(sizeof(fft_input_signal) / sizeof(fft_input_signal[0]));
        int m = 4;
        for(int ch = 0; ch < 2; ++ch){
            if(ch == 1 && ch_num == 1){
                break;
            }

            for(int i = 0; i < buffer_length; ++i){
                volume[ch][i] = 0.0;
            }

            for(int j = 0; j < buffer_length / m - (spectrum_length / m); ++j){
                std::memset(fft_input_signal, 0, sizeof(fft_input_signal));
                std::memset(fft_output_signal, 0, sizeof(fft_output_signal));
                for(int i = 0; i < n; ++i){
                    fft_input_signal[i] = out[j * m * channels + i * channels + ch];
                }
                fft(fft_input_signal, fft_output_signal, lg_spectrum_length);

                for(int i = 0; i < spectrum_length; ++i){
                    float w = power(fft_output_signal[i]);
                    float v = w / fft_max_power;
                    volume[ch][j * m + i] += v / m;
                }
            }
        }

        const int spectrum_size = sizeof(object::peek_spectrum[0]) / sizeof(object::peek_spectrum[0][0]);
        for(int i = 0; i < spectrum_size; ++i){
            for(int ch = 0; ch < 2; ++ch){
                if(ch == 1 && ch_num == 1){
                    break;
                }

                float sum = 0.0;
                for(int j = 0; j < buffer_length / spectrum_size; ++j){
                    sum += volume[ch][i * (buffer_length / spectrum_size) + j];
                }
                object::peek_spectrum[ch][i] = sum;
            }
        }

        if(progress_per_samples < static_cast<unsigned long>(len)){
            return paContinue;
        }else{
            return paComplete;
        }
    };

    PaStream *stream;
    err = Pa_OpenDefaultStream(
        &stream,
        0,
        decoder->channels(),
        paFloat32,
        decoder->sampleRate(),
        buffer_length,
        callback,
        decoder.get()
    );
    if(err != paNoError){
        Pa_Terminate();
        std::abort();
    }

    //再生
    Pa_StartStream(stream);
    is_playing = true;

    // スリープ
    while(is_playing){
        std::this_thread::sleep_for(1s);
    }

    Pa_CloseStream(stream);
}

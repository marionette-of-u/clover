#define _USE_MATH_DEFINES
#include <array>
#include <limits>
#include <memory>
#include <chrono>
#include <locale>
#include <random>
#include <thread>
#include <atomic>
#include <string>
#include <fstream>
#include <algorithm>
#include <functional>
#include <cmath>
#include <boost/lexical_cast.hpp>
#include <portaudio.h>
#include <DxLib.h>
#include "audiodecoder.h"
#include "bmp.hpp"

//-------- 定数
// スクリーン幅
static const int screen_width = 640;
// スクリーン高さ
static const int screen_height = 480;
// ウィンドウ幅
static const int window_width = screen_width;
// ウィンドウ高さ
static const int window_height = screen_height;
// フィールド幅
static const int field_width = 440;
// フィールド幅
static const int field_height = 440;
// フィールドの壁幅
static const int field_wall_width = 1;
// フィールド描画のオフセット
static const int offset_x = (screen_width - field_width) / 2, offset_y = (screen_height - field_height) / 2;

//-------- アプリケーションが動作しているかどうか
extern std::atomic<bool> is_running;
std::atomic<bool> is_running = true;

//-------- 三角関数テーブル
template<typename tty_arithmetic_type_ = float> struct alib{
    using arithmetic_type = tty_arithmetic_type_;
    template<typename T>
    static arithmetic_type num(T a){ return static_cast<arithmetic_type>(a); }
    static arithmetic_type epsilon(){ return std::numeric_limits<arithmetic_type>::epsilon(); }
    static arithmetic_type infinity(){ return std::numeric_limits<arithmetic_type>::infinity(); }
    static arithmetic_type pi(){ static arithmetic_type a = num(6) * std::asin(num(0.5)); return a; }
    static arithmetic_type pi2(){ return pi() * num(2); }

    template<unsigned int N, typename Alloc = std::allocator<arithmetic_type> > class trigonometric_function;

    template<unsigned int N, typename Alloc = std::allocator<arithmetic_type> > class trigonometric_function_TmpRec{
    private:
	    template<unsigned int N_> struct endholder_0{ typedef trigonometric_function<N_, Alloc> end; };
	    template<typename Alloc_> struct endholder_1{ typedef trigonometric_function<N, Alloc_> end; };

    public:
	    template<unsigned int N_>
	    struct precision_0 : public endholder_0<N_>, trigonometric_function_TmpRec<N_, Alloc>{};
	    template<typename Alloc_>
	    struct precision_1 : public endholder_1<Alloc_>, trigonometric_function_TmpRec<N, Alloc_>{};
    };

    template<unsigned int N, typename Alloc>
    class trigonometric_function : trigonometric_function_TmpRec<N, Alloc>{
    #define TTY_PI_ static_cast<arithmetic_type>(3.1415926535897932384626433832795)
    #define TTY_PI2_ static_cast<arithmetic_type>(6.283185307179586476925286766559)
    public:
	    typedef Alloc allocator_type;
	    typedef int angle_t;
	    enum{
		    pi2 = 1 << N,		//一周
		    pi = pi2 / 2,		//半周
		    mask = pi2 - 1
	    };

    private:
	    typedef trigonometric_function<N> this_t;
        arithmetic_type *sin_tan_;
	    arithmetic_type *sin_, *tan_;
	    angle_t *atan_;

    public:
	    trigonometric_function() :
            sin_tan_(new arithmetic_type[pi2 * 2 + pi / 4]),
		    sin_	(sin_tan_),
		    tan_	(sin_tan_ + pi2),
		    atan_	(new angle_t[pi2 * 2])
	    {
		    //テーブルを作成
		    const arithmetic_type
			    acc = TTY_PI2_ / static_cast<arithmetic_type>(pi2),	// sin tan の加速度
			    omega = static_cast<arithmetic_type>(1) / static_cast<arithmetic_type>(pi / 4),	// atan の加速度
			    q = TTY_PI_ / static_cast<arithmetic_type>(4);			// 180 / 4

		    arithmetic_type t = static_cast<arithmetic_type>(0);
		    for(angle_t i = 0; i < pi2; t += acc, i++){
			    sin_[i] = ::sin(t);
			    tan_[i] = ::tan(t);
		    }

		    t = 0;
		    for(angle_t i = 0; i < (pi / 4); t += omega, i++){
			    atan_[i] = static_cast<angle_t>(static_cast<arithmetic_type>(pi / 4) * ::atan(t) / q);
		    }
	    }

	    ~trigonometric_function(){
		    delete[] sin_tan_;
            delete[] atan_;
	    }

	    const arithmetic_type &sin(const angle_t &a)const{ return sin_[a & mask]; }
	    const arithmetic_type &cos(const angle_t &a)const{ return sin_[(a + pi / 2) & mask]; }
	    const arithmetic_type &tan(const angle_t &a)const{ return tan_[a & mask]; }

    public:
	    angle_t atan2(const arithmetic_type &y, const arithmetic_type &x) const{
#if 1
		    //どう頑張っても組み込みatan2の方が速いと思う場合
		    angle_t a = static_cast<angle_t>(::atan2(y, x) * static_cast<arithmetic_type>(pi) / TTY_PI_);
		    if(a < 0){ return pi2 + a; }else{ return a; }
#else
#define absolute_(a) (a < 0 ? -a : a)
		    angle_t s = 0;
		    arithmetic_type ax, ay;
		    if(x < 0){
			    ax = -x;
			    s += 1;
		    }else{ ax = x; }
		    if(y < 0){
			    ay = -y;
			    s += 2;
		    }else{ ay = y; }
		    angle_t i;
		    bool f;
		    if(ax > ay){
			    i = static_cast<angle_t>(static_cast<angle_t>(pi / 4) * ay / ax);
			    f = (s == 0 || s == 3) ? 1 : 0;
		    }else if(ay > ax){
			    i = static_cast<angle_t>(static_cast<arithmetic_type>(pi / 4) * ax / ay);
			    f = (s == 0 || s == 3) ? 0 : 1;
		    }else{//ax == ay
			    static const angle_t t[4] = {
				    pi / 4,
				    pi / 4 * 3,
				    pi / 4 * 7,
				    pi / 4 * 5,
			    };
			    return t[s];
		    }
		    static const angle_t t_[4] = {
			    0,
			    pi / 2,
			    pi / 2 * 3,
			    pi / 2 * 2,
		    };
		    if(f){
			    return atan_[i] + t_[s];
		    }else{
			    return atan_[(pi / 4) - 1] + t_[s] + (pi / 4);
		    }
#undef absolute_
#endif
	    }

	    angle_t atan2_cross(const arithmetic_type &x, const arithmetic_type &y) const{
		    return atan2(y, x);
	    }

#undef TTY_PI_
#undef TTY_PI2_
    };
};
using tri_type = alib<float>::trigonometric_function<8, std::allocator<int>>;
tri_type tri;

//-------- 乱数
std::mt19937 random(0x77777777u);

//-------- メイングラフィックハンドル
int main_graphic_handle;

//-------- FFT解析長
extern const int spectrum_length;

//-------- FFTパワー上限
extern float fft_max_power;

//-------- 周波数成分ごとの音量ピーク
extern std::atomic<float> *peek_volume[2];

//-------- 楽曲チャンネル数
extern int ch_num;

//-------- 楽曲進行率
extern std::atomic<float> progress;
std::atomic<float> progress;

//-------- FPSマネージャー
class fps_manager{
public:
    // 目標FPS
    const int target_fps = 60;

    // 修正秒率
    const int fix_time_ratio = 10;

    // 修正フレーム単位
    const int fix_frames_num = target_fps / fix_time_ratio;

    // スリープタイム
    const float sleep_time = 1000.0f / target_fps;

    void operator ()(){
        ++frame_count;
        Sleep(static_cast<int>(real_sleep_time));
        if(frame_count >= fix_frames_num){
            std::chrono::milliseconds real = std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::high_resolution_clock::now() - start);
            real_sleep_time = sleep_time * (sleep_time * fix_frames_num) / static_cast<float>(real.count());
            if(real_sleep_time >= sleep_time){
                real_sleep_time = sleep_time;
            }
            start = std::chrono::high_resolution_clock::now();
            frame_count = 0;
        }
    }

    float real_sleep_time = 16;

private:
    int frame_count = 0;
    std::chrono::high_resolution_clock::time_point start = std::chrono::high_resolution_clock::now();
};

//-------- キー定数
enum class keys : int{
    back = KEY_INPUT_BACK,
    tab = KEY_INPUT_TAB,
    ret = KEY_INPUT_RETURN,

    l_shift = KEY_INPUT_LSHIFT,
    r_shift = KEY_INPUT_RSHIFT,
    l_control = KEY_INPUT_LCONTROL,
    r_control = KEY_INPUT_RCONTROL,
    escape = KEY_INPUT_ESCAPE,
    space = KEY_INPUT_SPACE,
    page_up = KEY_INPUT_PGUP,
    page_down = KEY_INPUT_PGDN,
    end = KEY_INPUT_END,
    home = KEY_INPUT_HOME,
    left = KEY_INPUT_LEFT,
    up = KEY_INPUT_UP,
    right = KEY_INPUT_RIGHT,
    down = KEY_INPUT_DOWN,
    insert = KEY_INPUT_INSERT,
    del = KEY_INPUT_DELETE,

    minus = KEY_INPUT_MINUS,
    yen = KEY_INPUT_YEN,
    prevtrack = KEY_INPUT_PREVTRACK,
    period = KEY_INPUT_PERIOD,
    slash = KEY_INPUT_SLASH,
    l_alt = KEY_INPUT_LALT,
    r_alt = KEY_INPUT_RALT,
    scroll_lock = KEY_INPUT_SCROLL,
    semicolon = KEY_INPUT_SEMICOLON,
    colon = KEY_INPUT_COLON,
    l_bracket = KEY_INPUT_LBRACKET,
    r_bracket = KEY_INPUT_RBRACKET,
    at = KEY_INPUT_AT,
    backslash = KEY_INPUT_BACKSLASH,
    comma = KEY_INPUT_COMMA,
    kanji = KEY_INPUT_KANJI,
    convert = KEY_INPUT_CONVERT,
    noconvert = KEY_INPUT_NOCONVERT,
    kana = KEY_INPUT_KANA,
    apps = KEY_INPUT_APPS,
    capslock = KEY_INPUT_CAPSLOCK,
    print_screen = KEY_INPUT_SYSRQ,
    pause = KEY_INPUT_PAUSE,
    l_win = KEY_INPUT_LWIN,
    r_win = KEY_INPUT_RWIN,

    numlock = KEY_INPUT_NUMLOCK,
    numpad_0 = KEY_INPUT_NUMPAD0,
    numpad_1 = KEY_INPUT_NUMPAD1,
    numpad_2 = KEY_INPUT_NUMPAD2,
    numpad_3 = KEY_INPUT_NUMPAD3,
    numpad_4 = KEY_INPUT_NUMPAD4,
    numpad_5 = KEY_INPUT_NUMPAD5,
    numpad_6 = KEY_INPUT_NUMPAD6,
    numpad_7 = KEY_INPUT_NUMPAD7,
    numpad_8 = KEY_INPUT_NUMPAD8,
    numpad_9 = KEY_INPUT_NUMPAD9,
    multiply = KEY_INPUT_MULTIPLY,
    add = KEY_INPUT_ADD,
    subtract = KEY_INPUT_SUBTRACT,
    decimal = KEY_INPUT_DECIMAL,
    devide = KEY_INPUT_DIVIDE,
    numpad_enter = KEY_INPUT_NUMPADENTER,

    f1 = KEY_INPUT_F1,
    f2 = KEY_INPUT_F2,
    f3 = KEY_INPUT_F3,
    f4 = KEY_INPUT_F4,
    f5 = KEY_INPUT_F6,
    f6 = KEY_INPUT_F6,
    f7 = KEY_INPUT_F7,
    f8 = KEY_INPUT_F8,
    f9 = KEY_INPUT_F9,
    f10 = KEY_INPUT_F10,
    f11 = KEY_INPUT_F11,
    f12 = KEY_INPUT_F12,

    a = KEY_INPUT_A,
    b = KEY_INPUT_B,
    c = KEY_INPUT_C,
    d = KEY_INPUT_D,
    e = KEY_INPUT_E,
    f = KEY_INPUT_F,
    g = KEY_INPUT_G,
    h = KEY_INPUT_H,
    i = KEY_INPUT_I,
    j = KEY_INPUT_J,
    k = KEY_INPUT_K,
    l = KEY_INPUT_L,
    m = KEY_INPUT_M,
    n = KEY_INPUT_N,
    o = KEY_INPUT_O,
    p = KEY_INPUT_P,
    q = KEY_INPUT_Q,
    r = KEY_INPUT_R,
    s = KEY_INPUT_S,
    t = KEY_INPUT_T,
    u = KEY_INPUT_U,
    v = KEY_INPUT_V,
    w = KEY_INPUT_W,
    x = KEY_INPUT_X,
    y = KEY_INPUT_Y,
    z = KEY_INPUT_Z,

    n0 = KEY_INPUT_0,
    n1 = KEY_INPUT_1,
    n2 = KEY_INPUT_2,
    n3 = KEY_INPUT_3,
    n4 = KEY_INPUT_4,
    n5 = KEY_INPUT_5,
    n6 = KEY_INPUT_6,
    n7 = KEY_INPUT_7,
    n8 = KEY_INPUT_8,
    n9 = KEY_INPUT_9,

    ctrl_back_space = CTRL_CODE_BS,
    ctrl_tab = CTRL_CODE_TAB,
    ctrl_cr = CTRL_CODE_CR,
    ctrl_del = CTRL_CODE_DEL,
    ctrl_copy = CTRL_CODE_COPY,
    ctrl_paste = CTRL_CODE_PASTE,
    ctrl_cut = CTRL_CODE_CUT,
    ctrl_all = CTRL_CODE_ALL,

    ctrl_left = CTRL_CODE_LEFT,
    ctrl_right = CTRL_CODE_RIGHT,
    ctrl_up = CTRL_CODE_UP,
    ctrl_down = CTRL_CODE_DOWN,

    ctrl_home = CTRL_CODE_HOME,
    ctrl_end = CTRL_CODE_END,
    ctrl_page_up = CTRL_CODE_PAGE_UP,
    ctrl_page_down = CTRL_CODE_PAGE_DOWN,

    ctrl_esc = CTRL_CODE_ESC
};

//-------- マウスボタン定数
enum class mouse_button : int{
    mouse_left = MOUSE_INPUT_LEFT,
    mouse_right = MOUSE_INPUT_RIGHT,
    mouse_middle = MOUSE_INPUT_MIDDLE
};

//-------- 入力管理
class input_manager_type{
public:
    input_manager_type(){
        for(int i = 0; i < 0x100; ++i){
            key_buffer[i] = 0;
            prev_flag[i] = false;
        }
        GetMousePoint(&cursor_x[0], &cursor_y[0]);
        cursor_x[1] = cursor_x[0];
        cursor_y[1] = cursor_y[0];
        mouse_state = prev_mouse_state = GetMouseInput();
    }

    void operator ()(){
        for(int i = 0; i < 0x100; ++i){
            prev_flag[i] = key_buffer[i] == 1 ? true : false;
        }
        GetHitKeyStateAll(key_buffer);
        cursor_x[1] = cursor_x[0];
        cursor_y[1] = cursor_y[0];
        GetMousePoint(&cursor_x[0], &cursor_y[0]);
        prev_mouse_state = mouse_state;
        mouse_state = GetMouseInput();
    }

    int x() const{
        return cursor_x[0];
    }

    int prev_x() const{
        return cursor_x[1];
    }

    int y() const{
        return cursor_y[0];
    }

    int prev_y() const{
        return cursor_y[1];
    }

    bool press(keys n) const{
        return key_buffer[static_cast<int>(n)] == 1;
    }

    bool push(keys n) const{
        return key_buffer[static_cast<int>(n)] == 1 && !prev_flag[static_cast<int>(n)];
    }

    bool release(keys n) const{
        return key_buffer[static_cast<int>(n)] != 1 && prev_flag[static_cast<int>(n)];
    }

    bool mouse_press(mouse_button n) const{
        return (mouse_state & static_cast<int>(n)) != 0;
    }

    bool mouse_push(mouse_button n) const{
        return (mouse_state & static_cast<int>(n)) != 0 && !((prev_mouse_state & static_cast<int>(n)) != 0);
    }

    bool mouse_release(mouse_button n) const{
        return !((mouse_state & static_cast<int>(n)) != 0) && (prev_mouse_state & static_cast<int>(n)) != 0;
    }

private:
    char key_buffer[0x100];
    bool prev_flag[0x100];
    int cursor_x[2], cursor_y[2];
    int mouse_state, prev_mouse_state;
} input_manager;

//-------- pattern surface
class patter_surface_type{
public:
    patter_surface_type(const char *path){
        pattern_surface = LoadGraph(path);
        if(pattern_surface == -1){
            std::abort();
        }
        GetGraphSize(pattern_surface, &width_, &height_);
    }

    int get() const{
        return pattern_surface;
    }

    int width() const{
        return width_;
    }

    int height() const{
        return height_;
    }

private:
    int pattern_surface;
    int width_, height_;
};

// グラフィックパターン
namespace pattern{
    std::unique_ptr<patter_surface_type> title;
    std::unique_ptr<patter_surface_type> hitcount;
    std::array<std::unique_ptr<patter_surface_type>, 3> player_left, player_right, player_up, player_down;
    std::array<std::unique_ptr<patter_surface_type>, 10> num;

    //グラフィックパターンをロードする
    void load_graphic(){
        title.reset(new patter_surface_type("d/title.bmp"));
        hitcount.reset(new patter_surface_type("d/hitcount.bmp"));
        for(int i = 0; i < 3; ++i){
            player_left[i].reset(new patter_surface_type(("d/player_left_" + boost::lexical_cast<std::string>(i) + ".bmp").c_str()));
            player_right[i].reset(new patter_surface_type(("d/player_right_" + boost::lexical_cast<std::string>(i) + ".bmp").c_str()));
            player_up[i].reset(new patter_surface_type(("d/player_up_" + boost::lexical_cast<std::string>(i) + ".bmp").c_str()));
            player_down[i].reset(new patter_surface_type(("d/player_down_" + boost::lexical_cast<std::string>(i) + ".bmp").c_str()));
        }
        for(int i = 0; i < 10; ++i){
            num[i].reset(new patter_surface_type(("d/" + boost::lexical_cast<std::string>(i) + ".bmp").c_str()));
        }
    }
}

//-------- 座標
using coord_type = float[2];

//-------- ユークリッドノルム
float euclid_norm(float x, float y){
    return std::sqrt(x * x + y * y);
}

//-------- サウンド
namespace sound_effect{
    extern PaStream *hit_stream;
    PaStream *hit_stream;
    extern std::unique_ptr<AudioDecoder> hit;
    std::unique_ptr<AudioDecoder> hit;
}

void play_sound();
void play_hit_sound();

//-------- ゲーム中で使われるオブジェクト
namespace object{
    template<class T>
    struct task{
        T obj;
        task *next, *prev;
        int thread;
    };

    template<class T>
    struct object{
        // タスク生成時にコール
        virtual void ctor(){}

        // タスク破棄時にコール
        virtual void dtor(){}

        // 描画時にコール
        virtual void draw() const{}

        // タスク呼び出し時にコール
        virtual void update(task<T>*&) = 0;
    };

    template<class T, std::size_t N>
    class tasklist{
    private:
        using task = task<T>;
        std::size_t size_ = 0;
        task active_ , *free_;
        std::array<task, N> arr;

    public:
        task &active = active_;

        tasklist(){
            for(int i = 0; i < N - 1; ++i){
                arr[i].next = &arr[i + 1];
            }
            arr[N - 1].next = nullptr;
            free_ = &arr[0];

            active_.next = &active_;
            active_.prev = &active_;
        }

        ~tasklist() = default;

        // タスクを生成する
        task *create_task(){
            if(size_ == N){
                return nullptr;
            }
            ++size_;
            task *t = free_;
            free_ = free_->next;
            active_.next->prev = t;
            t->next = active_.next;
            active_.next = t;
            t->prev = &active_;
            t->obj.ctor();
            return t;
        }

        // タスクを削除する
        task *delete_task(task *t){
            t->obj.dtor();
            --size_;
            task *r = t->prev, *s = t->next;
            t->prev->next = t->next;
            if(t->next){
                t->next->prev = t->prev;
            }
            t->next = free_;
            free_ = t;
            return r;
        }

        // クリア
        void clear(){
            if(size_ == 0){
                return;
            }
            while(active_.next != &active_){
                delete_task(active_.next);
            }
            active_.next = &active_;
            active_.prev = &active_;
            size_ = 0;
        }

        // 空か
        bool empty() const{
            return size_ == 0;
        }

        // 現在のサイズ
        std::size_t size() const{
            return size_;
        }

        // タスクを回す
        void update(){
            task *t = active_.next;
            while(t != &active_){
                static_cast<object<T>*>(&t->obj)->update(t);
                if(t != &active_){
                    t = t->next;
                }
            }
        }

        // 描画タスクを回す
        void draw() const{
            task *t = active_.next;
            while(t != &active_){
                static_cast<object<T>*>(&t->obj)->draw();
                t = t->next;
            }
        }
    };

    struct point_type{
        coord_type  coord;
        tri_type::angle_t omega;
    };

    point_type make_point(float x, float y){
        point_type r;
        r.coord[0] = x;
        r.coord[1] = y;
        r.omega = tri.atan2(y, x);
        return r;
    }

    // 火花
    struct spark : public object<spark>{
        // 基本パーティクル数
        static const int particle_num = 6;

        using tasklist_type = tasklist<spark, 1024 * particle_num>;
        static tasklist_type &tasklist(){
            static tasklist_type t;
            return t;
        }

        // 座標
        coord_type coord;

        // 速度
        coord_type speed;

        // 長さ
        static const int length = 3;

        // 寿命
        int count;
        // 最大寿命
        static const int count_max = 30;

        // 色
        unsigned int color;

        void ctor() override{
            count = 0;

            static const float r = 25.0f / count_max;
            tri_type::angle_t omega = tri_type::pi * ((random() % 64) - 32) / 32;
            speed[0] = tri.cos(omega) * r;
            speed[1] = tri.sin(omega) * r;
        }

        void draw() const override{
            DrawLine(
                static_cast<int>(offset_x + coord[0]),
                static_cast<int>(offset_y + coord[1]),
                static_cast<int>(offset_x + coord[0] + speed[0] * length),
                static_cast<int>(offset_y + coord[1] + speed[1] * length),
                color
            );
            DrawPixel(
                static_cast<int>(offset_x + coord[0] + speed[0] * length),
                static_cast<int>(offset_y + coord[1] + speed[1] * length),
                color
            );
        }

        void update(task<spark> *&t) override{
            ++count;
            if(count >= count_max){
                t = tasklist().delete_task(t);
                return;
            }

            coord[0] += speed[0];
            coord[1] += speed[1];
        }
    };

    // バレットアローフレーム
    volatile point_type bullet_arrow_frame[4] = {
        make_point(+7 * 1.25, +0),
        make_point(-3 * 1.25, +5 * 1.25),
        make_point(-5 * 1.25, +0),
        make_point(-3 * 1.25, -5 * 1.25)
    };

    // バレットアローを描画する
    void basic_bullet_arrow_draw(const coord_type &c, tri_type::angle_t omega, unsigned int color){
        float r = euclid_norm(bullet_arrow_frame[0].coord[0], bullet_arrow_frame[0].coord[1]);
        float px = r * tri.cos(bullet_arrow_frame[0].omega + omega), py = r * tri.sin(bullet_arrow_frame[0].omega + omega), qx, qy;
        for(std::size_t i = 1; i < sizeof(bullet_arrow_frame) / sizeof(bullet_arrow_frame[0]); ++i){
            r = euclid_norm(bullet_arrow_frame[i].coord[0], bullet_arrow_frame[i].coord[0]);
            qx = r * tri.cos(bullet_arrow_frame[i].omega + omega), qy = r * tri.sin(bullet_arrow_frame[i].omega + omega);
            DrawLine(static_cast<int>(offset_x + c[0] + px), static_cast<int>(offset_y + c[1] + py), static_cast<int>(offset_x + c[0] + qx), static_cast<int>(offset_y + c[1] + qy), color);
            DrawPixel(static_cast<int>(offset_x + c[0] + qx), static_cast<int>(offset_y + c[1] + qy), color);
            px = qx, py = qy;
        }
        r = euclid_norm(bullet_arrow_frame[0].coord[0], bullet_arrow_frame[0].coord[1]);
        qx = r * tri.cos(bullet_arrow_frame[0].omega + omega), qy = r * tri.sin(bullet_arrow_frame[0].omega + omega);
        DrawLine(static_cast<int>(offset_x + c[0] + px), static_cast<int>(offset_y + c[1] + py), static_cast<int>(offset_x + c[0] + qx), static_cast<int>(offset_y + c[1] + qy), color);
        DrawPixel(static_cast<int>(offset_x + c[0] + qx), static_cast<int>(offset_y + c[1] + qy), color);
    }

    // バレットバッファ
    std::array<std::array<bool, 19>, 19> bullet_buffer;

    // 線を引く
    void draw_line_bullet_buffer(int x0, int y0, int x1, int y1){
        int dx, dy, sx, sy, a, i;
        if(x1 > x0){
            dx = x1 - x0;
            sx = 1;
        }else{
            dx = x0 - x1;
            sx = -1;
        }
        if(y1 > y0){
            dy = y1 - y0;
            sy = 1;
        }else{
            dy = y0 - y1;
            sy = -1;
        }
        if(dx > dy){
            a = -dx;
            for(i = 0; i <= dx; ++i){
                bullet_buffer[y0][x0] = true;
                x0 += sx;
                a += 2 * dy;
                if(a >= 0){
                    y0 += sy;
                    a -= 2 * dx;
                }
            }
        }else{
            a = -dy;
            for(i = 0; i <= dy; ++i){
                bullet_buffer[y0][x0] = true;
                y0 += sy;
                a += 2 * dx;
                if(a >= 0){
                    x0 += sx;
                    a -= 2 * dy;
                }
            }
        }
    }

    // バレットアローを描画する
    // 塗りつぶし
    void fill_bullet_arrow_draw(const coord_type &c, tri_type::angle_t omega, unsigned int color){
        for(int i = 0; i < bullet_buffer.size(); ++i){
            for(int j = 0; j < bullet_buffer.size(); ++j){
                bullet_buffer[i][j] = false;
            }
        }

        int draw_offset =  bullet_buffer.size() / 2;
        float r = euclid_norm(bullet_arrow_frame[0].coord[0], bullet_arrow_frame[0].coord[1]);
        float px = r * tri.cos(bullet_arrow_frame[0].omega + omega), py = r * tri.sin(bullet_arrow_frame[0].omega + omega), qx, qy;
        for(std::size_t i = 1; i < sizeof(bullet_arrow_frame) / sizeof(bullet_arrow_frame[0]); ++i){
            r = euclid_norm(bullet_arrow_frame[i].coord[0], bullet_arrow_frame[i].coord[0]);
            qx = r * tri.cos(bullet_arrow_frame[i].omega + omega), qy = r * tri.sin(bullet_arrow_frame[i].omega + omega);
            draw_line_bullet_buffer(
                static_cast<int>(px + (c[0] - (int)c[0])) + draw_offset,
                static_cast<int>(py + (c[1] - (int)c[1])) + draw_offset,
                static_cast<int>(qx + (c[0] - (int)c[0])) + draw_offset,
                static_cast<int>(qy + (c[1] - (int)c[1])) + draw_offset);
            px = qx, py = qy;
        }
        r = euclid_norm(bullet_arrow_frame[0].coord[0], bullet_arrow_frame[0].coord[1]);
        qx = r * tri.cos(bullet_arrow_frame[0].omega + omega), qy = r * tri.sin(bullet_arrow_frame[0].omega + omega);
        draw_line_bullet_buffer(
            static_cast<int>(px + (c[0] - (int)c[0])) + draw_offset,
            static_cast<int>(py + (c[1] - (int)c[1])) + draw_offset,
            static_cast<int>(qx + (c[0] - (int)c[0])) + draw_offset,
            static_cast<int>(qy + (c[1] - (int)c[1])) + draw_offset
        );

        std::function<void(int, int)> recursive_fill = [&](int x, int y){
            bullet_buffer[x][y] = true;
            if(x > 0 && !bullet_buffer[x - 1][y]){
                recursive_fill(x - 1, y);
            }
            if(x < 18 && !bullet_buffer[x + 1][y]){
                recursive_fill(x + 1, y);
            }
            if(y > 0 && !bullet_buffer[x][y - 1]){
                recursive_fill(x, y - 1);
            }
            if(y < 18 && !bullet_buffer[x][y + 1]){
                recursive_fill(x, y + 1);
            }
        };
        recursive_fill(bullet_buffer.size() / 2, bullet_buffer.size() / 2);

        for(int i = 0; i < bullet_buffer.size(); ++i){
            for(int j = 0; j < bullet_buffer.size(); ++j){
                int x = offset_x + static_cast<int>(c[0]) - bullet_buffer.size() / 2 + i;
                int y = offset_y + static_cast<int>(c[1]) - bullet_buffer.size() / 2 + j;
                if(bullet_buffer[j][i]){
                    DrawPixel(x, y, color);
                }else if(
                    i > 0 &&
                    i < bullet_buffer.size() - 1 &&
                    j > 0 &&
                    j < bullet_buffer.size() - 1 &&
                    bullet_buffer[j - 1][i] && bullet_buffer[j + 1][i] &&
                    bullet_buffer[j][i - 1] && bullet_buffer[j][i + 1]
                ){
                    DrawPixel(x, y, color);
                }
            }
        }
    }

    // バレットアロー
    struct bullet_arrow : public object<bullet_arrow>{
        using tasklist_type = tasklist<bullet_arrow, 256 * 3>;
        static tasklist_type &tasklist(){
            static tasklist_type t;
            return t;
        }

        // 座標
        coord_type coord;

        // 速度
        coord_type speed;

        // 角度
        tri_type::angle_t omega;

        // 相
        int phase;
        static const int phase_max = 1;

        // 色
        static unsigned int color(){ return GetColor(192, 0, 0); }

        // 角度を設定する
        void set_omega(){
            omega = tri.atan2(speed[1], speed[0]);
        }

        void ctor() override{
            phase = 0;
        }

        void draw() const override{
            fill_bullet_arrow_draw(coord, omega, color());
        }

        void update(task<bullet_arrow> *&t) override{
            coord[0] += speed[0];
            coord[1] += speed[1];

            if(coord[0] < 0.0 || coord[0] >= field_width || coord[1] < 0.0 || coord[1] >= field_height){
                task<spark> *v[spark::particle_num];
                for(int i = 0; i < spark::particle_num; ++i){
                    v[i] = spark::tasklist().create_task();
                    if(v[i]){
                        v[i]->obj.color = color();
                    }
                }

                ++phase;
                if(phase >= phase_max){
                    t = tasklist().delete_task(t);
                }else{
                    bool ref[2] = { false };
                    if(coord[0] < 0.0){
                        ref[0] = true;
                        coord[0] = 0.0;
                    }else if(coord[0] >= field_width){
                        ref[0] = true;
                        coord[0] = field_width;
                    }
                    if(coord[1] < 0.0){
                        ref[1] = true;
                        coord[1] = 0.0;
                    }else if(coord[1] >= field_width){
                        ref[1] = true;
                        coord[1] = field_width;
                    }
                    for(int i = 0; i < 2; ++i){
                        if(ref[i]){
                            speed[i] = -speed[i];
                        }else{
                            speed[i] = +speed[i];
                        }
                        set_omega();
                    }
                }

                for(int i = 0; i < spark::particle_num; ++i){
                    if(v[i]){
                        v[i]->obj.coord[0] = coord[0];
                        v[i]->obj.coord[1] = coord[1];
                    }
                }
            }
        }
    };

    // サブバレットアロー
    struct sub_bullet_arrow : public object<sub_bullet_arrow>{
        using tasklist_type = tasklist<sub_bullet_arrow, 256>;
        static tasklist_type &tasklist(){
            static tasklist_type t;
            return t;
        }

        // 座標
        coord_type coord;

        // 速度
        coord_type speed;

        // 速度係数
        static float speed_coe(){
            return 1.75;
        }

        // 角度
        tri_type::angle_t omega;

        // 色
        static unsigned int color(){ return GetColor(96, 96, 192); }

        // 角度を設定する
        void set_omega(){
            omega = tri.atan2(speed[1], speed[0]);
        }

        void draw() const override{
            basic_bullet_arrow_draw(coord, omega, color());
        }

        void update(task<sub_bullet_arrow> *&t) override{
            coord[0] += speed[0];
            coord[1] += speed[1];
            
            if(coord[0] < 0.0 || coord[0] >= field_width || coord[1] < 0.0 || coord[1] >= field_height){
                t = tasklist().delete_task(t);

                task<bullet_arrow> *u = bullet_arrow::tasklist().create_task();
                task<spark> *v[spark::particle_num];
                for(int i = 0; i < spark::particle_num; ++i){
                    v[i] = spark::tasklist().create_task();
                    if(v[i]){
                        v[i]->obj.color = color();
                    }
                }
                if(u){
                    u->obj.coord[0] = coord[0];
                    u->obj.coord[1] = coord[1];

                    bool ref[2] = { false };
                    if(u->obj.coord[0] < 0.0){
                        ref[0] = true;
                        u->obj.coord[0] = 0.0;
                    }else if(u->obj.coord[0] >= field_width){
                        ref[0] = true;
                        u->obj.coord[0] = field_width;
                    }
                    if(u->obj.coord[1] < 0.0){
                        ref[1] = true;
                        u->obj.coord[1] = 0.0;
                    }else if(u->obj.coord[1] >= field_width){
                        ref[1] = true;
                        u->obj.coord[1] = field_width;
                    }
                    for(int i = 0; i < 2; ++i){
                        if(ref[i]){
                            u->obj.speed[i] = -speed[i];
                        }else{
                            u->obj.speed[i] = +speed[i];
                        }
                    }
                    u->obj.set_omega();

                    for(int i = 0; i < spark::particle_num; ++i){
                        if(v[i]){
                            v[i]->obj.coord[0] = u->obj.coord[0];
                            v[i]->obj.coord[1] = u->obj.coord[1];
                        }
                    }
                }
            }
        }
    };

    // ヒットカウント
    struct hitcount_type{
        int count;

        void ctor(){
            count = 0;
        }

        void draw() const{
            int p[4];
            int c = count >= 9999 ? 9999 : count;
            for(int i = 0; i < 4; ++i){
                p[i] = c % 10; c /= 10;
            }
            DrawGraph(0, 0, pattern::hitcount->get(), TRUE);
            for(int i = 0; i < 4; ++i){
                DrawGraph(pattern::num[0]->width() * i, pattern::hitcount->height() + 2, pattern::num[p[4 - 1 - i]]->get(), TRUE);
            }
        }
    } hitcount;

    // プレイヤー
    struct player_type{
        // walk_count_max / 2 frameで一コマのアニメーション
        static const int walk_count_max = 4 * 2;
        int walk_count;

        // 座標
        coord_type coord;

        // プレイヤーの向いている方向
        enum class dir_t{
            up, down, left, right
        };
        dir_t dir;

        // 移動方向
        // 斜めにも対応できるように
        int move_dir;

        void ctor(){
            dir = dir_t::down;
            coord[0] = field_width / 2;
            coord[1] = field_height / 2;
            move_dir = 0;
            walk_count = 0;
        }

        void collision(){
            auto &list_manager_bullet_arrow = bullet_arrow::tasklist();
            auto *t = list_manager_bullet_arrow.active.next;
            bool hit_flag = false;
            while(t != &list_manager_bullet_arrow.active){
                bool d = t->obj.coord[0] >= coord[0] && t->obj.coord[0] - 1.0 <= coord[0];
                d = d || t->obj.coord[0] >= coord[0] - 1.0 && t->obj.coord[0] - 1.0 <= coord[0] - 1.0;
                bool e = t->obj.coord[1] >= coord[1] && t->obj.coord[1] - 1.0 <= coord[1];
                e = e || t->obj.coord[1] >= coord[1] - 1.0 && t->obj.coord[1] - 1.0 <= coord[1] - 1.0;
                if(d && e){
                    t = list_manager_bullet_arrow.delete_task(t);
                    ++hitcount.count;
                    hit_flag = true;
                    task<spark> *v[spark::particle_num];
                    for(int i = 0; i < spark::particle_num; ++i){
                        v[i] = spark::tasklist().create_task();
                        if(v[i]){
                            v[i]->obj.coord[0] = coord[0];
                            v[i]->obj.coord[1] = coord[1];
                            v[i]->obj.color = bullet_arrow::color();
                        }
                    }
                }
                t = t->next;
            }
            if(hit_flag){
                play_hit_sound();
            }
        }

        void move(){
            bool k[] = {
                input_manager.press(keys::left) || input_manager.press(keys::a),
                input_manager.press(keys::right) || input_manager.press(keys::d),
                input_manager.press(keys::up) || input_manager.press(keys::w),
                input_manager.press(keys::down) || input_manager.press(keys::s)
            };

            if(!(k[2] || k[3])){
                if(k[0]){
                    dir = dir_t::left;
                }else if(k[1]){
                    dir = dir_t::right;
                }
            }

            if(!(k[0] || k[1])){
                if(k[2]){
                    dir = dir_t::up;
                }else if(k[3]){
                    dir = dir_t::down;
                }
            }

            if(k[0] && k[1]){
                // empty
            }else{
                if(k[0]){
                    move_dir |= 0b0001;
                }else if(!k[0]){
                    move_dir &= 0b1110;
                }
                if(k[1]){
                    move_dir |= 0b0010;
                }else if(!k[1]){
                    move_dir &= 0b1101;
                }
            }

            if(k[2] && k[3]){
                // empty
            }else{
                if(k[2]){
                    move_dir |= 0b0100;
                }else if(!k[2]){
                    move_dir &= 0b1011;
                }
                if(k[3]){
                    move_dir |= 0b1000;
                }else if(!k[3]){
                    move_dir &= 0b0111;
                }
            }

            static const float speed = 1.25;
            coord_type s;
            s[0] = 0.0, s[1] = 0.0;
            bool x = false, y = false;
            if((move_dir & 0b0001) > 0){
                s[0] = -speed;
                x = true;
            }else if((move_dir & 0b0010) > 0){
                s[0] = +speed;
                x = true;
            }

            if((move_dir & 0b0100) > 0){
                s[1] = -speed;
                y = true;
            }else if((move_dir & 0b1000) > 0){
                s[1] = +speed;
                y = true;
            }

            if(x || y){
                ++walk_count;
                if(walk_count >= walk_count_max){
                    walk_count = 0;
                }
            }else{
                walk_count = 0;
            }

            if(x && y){
                static const float sqrt = 1.0 / std::sqrt(2.0);
                s[0] *= sqrt;
                s[1] *= sqrt;
            }

            for(int i = 0; i < 2; ++i){
                coord[i] += s[i];
            }

            if(coord[0] < 0.0){
                coord[0] = 0.0;
            }else if(coord[0] >= field_width){
                coord[0] = field_width - 1;
            }

            if(coord[1] < 0.0){
                coord[1] = 0.0;
            }else if(coord[1] >= field_height){
                coord[1] = field_height - 1;
            }
        }

        void update(){
            collision();
            move();
            if(sound_effect::hit_interval > 0){
                --sound_effect::hit_interval;
            }
        }

        void draw(){
            patter_surface_type *ptr;
            if(move_dir == 0){
                switch(dir){
                case dir_t::left:
                    ptr = pattern::player_left[0].get();
                    break;

                case dir_t::right:
                    ptr = pattern::player_right[0].get();
                    break;

                case dir_t::up:
                    ptr = pattern::player_up[0].get();
                    break;

                case dir_t::down:
                    ptr = pattern::player_down[0].get();
                    break;
                }
            }else{
                switch(dir){
                case dir_t::left:
                    ptr = pattern::player_left[1 + (walk_count / (walk_count_max / 2))].get();
                    break;

                case dir_t::right:
                    ptr = pattern::player_right[1 + (walk_count / (walk_count_max / 2))].get();
                    break;

                case dir_t::up:
                    ptr = pattern::player_up[1 + (walk_count / (walk_count_max / 2))].get();
                    break;

                case dir_t::down:
                    ptr = pattern::player_down[1 + (walk_count / (walk_count_max / 2))].get();
                    break;
                }
            }
            DrawGraph(offset_x + static_cast<int>(coord[0]) - ptr->width() / 2, offset_y + static_cast<int>(coord[1]) - ptr->height() / 2, ptr->get(), TRUE);
        }
    } player;

    // ピークスペクトラム
    extern volatile float peek_spectrum[2][256];
    volatile float peek_spectrum[2][256] = { 0.0 };

    // 最近のフレームのスペクトラムのキャッシュ
    const int spectrum_cache_num = 3;
    std::unique_ptr<float[]> spectrum_cache[2][spectrum_cache_num];

    void update_spectrum_cache(const coord_type &origin){
        for(int i = 0; i < spectrum_cache_num - 1; ++i){
            for(int j = 0; j < 256; ++j){
                spectrum_cache[0][i].get()[j] = spectrum_cache[0][i + 1].get()[j];
                spectrum_cache[1][i].get()[j] = spectrum_cache[1][i + 1].get()[j];
            }
        }
        for(int i = 0; i < 256; ++i){
            spectrum_cache[0][spectrum_cache_num - 1].get()[i] = peek_spectrum[0][i];
            spectrum_cache[1][spectrum_cache_num - 1].get()[i] = peek_spectrum[1][i];
        }

        for(int spectrum_count = 0; spectrum_count < 256; ++spectrum_count){
            for(int i = 0; i < 2; ++i){
                float orth_spectrum[spectrum_cache_num];
                for(int j = 0; j < spectrum_cache_num; ++j){
                    orth_spectrum[j] = spectrum_cache[i][j].get()[spectrum_count];
                }
                std::sort(orth_spectrum, orth_spectrum + spectrum_cache_num - 1);
                float v = std::log(peek_spectrum[i][spectrum_count]) / std::log(orth_spectrum[0]);
                if(orth_spectrum[0] > 0.0 && v >= 2.0){
                    task<sub_bullet_arrow> *t = sub_bullet_arrow::tasklist().create_task();
                    if(t){
                        t->obj.coord[0] = origin[0];
                        t->obj.coord[1] = origin[1];

                        t->obj.speed[0] = (i == 0 ? +1 : -1) * tri.cos(spectrum_count) * sub_bullet_arrow::speed_coe();
                        t->obj.speed[1] = (i == 0 ? +1 : -1) * tri.sin(spectrum_count) * sub_bullet_arrow::speed_coe();

                        t->obj.set_omega();
                    }
                }
            }
        }
    }
}

//-------- ゲームループ
class game_loop{
public:
    game_loop() = default;
    virtual ~game_loop() = default;
    virtual bool update() = 0;
};

namespace clover_system{
    std::atomic<bool> now_playing;
    bool on_dd = false;
    std::unique_ptr<AudioDecoder> decoder;
    PaStream *stream = nullptr;
    std::thread sound_thread;
    fps_manager fps;
    std::unique_ptr<game_loop> current_loop;
    std::vector<std::function<void()>> action_queue;
}

// スペクトラムの描画
void draw_spectrum(){
    for(int i = 0; i < 256; ++i){
        DrawLine(
            0,
            (screen_height - 256) / 2 + i,
            static_cast<int>(object::peek_spectrum[0][i] * screen_width / 2),
            (screen_height - 256) / 2 + i,
            GetColor(0xE0, 0xE0, 0xE0)
        );
        DrawLine(
            screen_width - 1,
            (screen_height - 256) / 2 + i,
            screen_width - static_cast<int>(object::peek_spectrum[1][i] * screen_width / 2) - 1,
            (screen_height - 256) / 2 + i,
            GetColor(0xE0, 0xE0, 0xE0)
        );
    }
}

// フィールドの描画
void draw_field(){
    DrawLine(offset_x, offset_y, offset_x + field_width, offset_y, GetColor(0, 0, 0));
    DrawLine(offset_x + field_width, offset_y, offset_x + field_width, offset_y + field_height, GetColor(0, 0, 0));
    DrawLine(offset_x + field_width, offset_y + field_height, offset_x, offset_y + field_height, GetColor(0, 0, 0));
    DrawLine(offset_x, offset_y + field_height, offset_x, offset_y, GetColor(0, 0, 0));

    DrawLine(
        offset_x,
        (screen_height - field_height) / 2 + field_height,
        offset_x,
        screen_height,
        GetColor(0, 0, 0)
    );
    DrawLine(
        offset_x + field_width,
        (screen_height - field_height) / 2 + field_height,
        offset_x + field_width,
        screen_height,
        GetColor(0, 0, 0)
    );
}

// プログレスの描画
void draw_progress(){
    DrawBox(
        offset_x,
        (screen_height - field_height) * 3 / 4 + field_height,
        offset_x + static_cast<int>(progress.load() * field_width),
        (screen_height - field_height) + field_height,
        GetColor(0, 0, 0),
        TRUE
    );
}

// ロゴの描画
void draw_logo(){
    DrawGraph((screen_width - pattern::title->width()) / 2, (screen_height - pattern::title->height()) / 2, pattern::title->get(), FALSE);
}

//-------- シーン
namespace scene{
    // タイトル
    class title : public game_loop{
    public:
        title(){
            object::sub_bullet_arrow::tasklist().clear();
            object::bullet_arrow::tasklist().clear();
            object::spark::tasklist().clear();
        }

        bool update() override{
            input_manager();
            if(input_manager.push(keys::escape) || ProcessMessage() == -1){
                return false;
            }

            //-------- proc
            object::sub_bullet_arrow::tasklist().update();
            object::bullet_arrow::tasklist().update();
            object::spark::tasklist().update();

            ++count;
            omega += tri.pi * 2 / 256;
            if(omega >= tri.pi * 2){
                omega = 0;
            }
            if(count >= 2){
                count = 0;
                for(int i = 0; i < 2; ++i){
                    object::task<object::sub_bullet_arrow> *t = object::sub_bullet_arrow::tasklist().create_task();
                    if(t){
                        t->obj.coord[0] = field_width / 2.0;
                        t->obj.coord[1] = field_height / 2.0;

                        t->obj.speed[0] = (i == 0 ? +1 : -1) * tri.cos(omega - tri.pi) * object::sub_bullet_arrow::speed_coe();
                        t->obj.speed[1] = (i == 0 ? +1 : -1) * tri.sin(omega - tri.pi) * object::sub_bullet_arrow::speed_coe();

                        t->obj.set_omega();
                    }
                }
            }

            //-------- draw
            // クリア
            SetDrawScreen(main_graphic_handle);
            DrawBox(0, 0, screen_width, screen_height, GetColor(0xFF, 0xFF, 0xFF), TRUE);

            draw_field();
            object::sub_bullet_arrow::tasklist().draw();
            object::bullet_arrow::tasklist().draw();
            object::spark::tasklist().draw();
            draw_logo();

            SetDrawScreen(DX_SCREEN_FRONT);
            DrawExtendGraph(0, 0, window_width, window_height, main_graphic_handle, FALSE);
            clover_system::fps();

            return true;
        }

    private:
        int count = 0;
        tri_type::angle_t omega = 0;
    };

    // ゲームループ
    class game_main : public game_loop{
    public:
        game_main(){
            object::sub_bullet_arrow::tasklist().clear();
            object::bullet_arrow::tasklist().clear();
            object::spark::tasklist().clear();
            object::hitcount.ctor();
            object::player.ctor();
        }

        bool update() override{
            input_manager();
            if(input_manager.push(keys::escape) || ProcessMessage() == -1){
                return false;
            }

            //-------- proc
            object::player.update();
            object::sub_bullet_arrow::tasklist().update();
            object::bullet_arrow::tasklist().update();
            object::spark::tasklist().update();

            object::update_spectrum_cache(object::player.coord);

            //-------- draw
            // クリア
            SetDrawScreen(main_graphic_handle);
            DrawBox(0, 0, screen_width, screen_height, GetColor(0xFF, 0xFF, 0xFF), TRUE);

            draw_spectrum();
            draw_field();
            draw_progress();

            // オブジェクトの描画
            object::hitcount.draw();
            object::sub_bullet_arrow::tasklist().draw();
            object::player.draw();
            object::bullet_arrow::tasklist().draw();
            object::spark::tasklist().draw();

            // FPSの表示
            //{
            //    char str[6] = { 0 };
            //    str[0] = '0' + (static_cast<int>(clover_system::fps.real_sleep_time / 10) % 10);
            //    str[1] = '0' + (static_cast<int>(clover_system::fps.real_sleep_time) % 10);
            //    str[2] = '.';
            //    str[3] = '0' + (static_cast<int>(clover_system::fps.real_sleep_time * 10) % 10);
            //    str[4] = '0' + (static_cast<int>(clover_system::fps.real_sleep_time * 100) % 10);
            //    DrawString(0, 0, str, GetColor(0x00, 0x00, 0x00));
            //}

            SetDrawScreen(DX_SCREEN_FRONT);
            DrawExtendGraph(0, 0, window_width, window_height, main_graphic_handle, FALSE);
            clover_system::fps();

            return true;
        }
    };
}

LRESULT CALLBACK window_proc(HWND hwnd, UINT msg, WPARAM wp, LPARAM lp){
    switch(msg){
    case WM_DROPFILES:
        {
            HDROP hdrop = (HDROP)wp;
            UINT u_fileno = DragQueryFile(hdrop, 0xFFFFFFFF, NULL, 0);
            if(u_fileno > 0){
                char path[4192];
                DragQueryFile(hdrop, 0, path, sizeof(path));
                clover_system::now_playing = false;
                if(clover_system::sound_thread.joinable()){
                    clover_system::sound_thread.join();
                }
                clover_system::decoder.reset(new AudioDecoder(path));
                clover_system::on_dd = clover_system::decoder->open() != -1;
                if(!clover_system::on_dd){
                    clover_system::action_queue.push_back([](){
                        clover_system::current_loop.reset(new scene::title());
                    });
                }else{
                    WINDOWINFO info;
                    GetWindowInfo(GetMainWindowHandle(), &info);
                    SetWindowPos(GetMainWindowHandle(), HWND_TOP, 0, 0, 0, 0, SWP_NOSIZE | SWP_NOMOVE);
                    SetFocus(GetMainWindowHandle());
                    clover_system::sound_thread = std::move(std::thread(play_sound));
                    clover_system::action_queue.push_back([](){
                        clover_system::current_loop.reset(new scene::game_main());
                    });
                }
            }
            DragFinish(hdrop);
        }
        break;

    default:
        return DefWindowProc(hwnd, msg, wp, lp);
    }

    return 0;
}

//-------- WinMain
int WINAPI WinMain(HINSTANCE handle, HINSTANCE prev_handle, LPSTR lp_cmd, int n_cmd_show){
    for(int i = 0; i < object::spectrum_cache_num; ++i){
        object::spectrum_cache[0][i].reset(new float[256]{ 0.0 });
        object::spectrum_cache[1][i].reset(new float[256]{ 0.0 });
    }

    // init PortAudio
    if(Pa_Initialize() != paNoError){
        return -1;
    }

    // load sound
    sound_effect::hit.reset(new AudioDecoder("d/hit.mp3"));
    if(sound_effect::hit->open() != 0){
        return -1;
    }

    // init DxLib
    if(
        SetOutApplicationLogValidFlag(FALSE) != 0 ||
        ChangeWindowMode(TRUE) != DX_CHANGESCREEN_OK ||
        SetGraphMode(window_width, window_height, 32) != DX_CHANGESCREEN_OK ||
        SetMainWindowText("音射閉域") != 0 ||
        DxLib_Init() != 0
    ){ return -1; }

    SetHookWinProc(window_proc);

    // init D&D
    DragAcceptFiles(GetMainWindowHandle(), TRUE);

    // init Draw
    SetTransColor(0xFF, 0, 0xFF);
    pattern::load_graphic();
    main_graphic_handle = MakeScreen(screen_width, screen_height);

    // scoped guard
    struct scoped_guard_type{
        ~scoped_guard_type(){
            Pa_CloseStream(sound_effect::hit_stream);
            DxLib_End();
            Pa_Terminate();
        }
    } scoped_guard;

    clover_system::current_loop.reset(new scene::title());
    
    // main loop
    while(clover_system::current_loop->update()){
        for(auto &i : clover_system::action_queue){
            i();
        }
        clover_system::action_queue.clear();
    }

    return 0;
}

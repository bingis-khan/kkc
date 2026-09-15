#include <stddef.h>
#include <stdint.h>
#include <locale.h>
int _global_argc;
const char** _global_argv;
static  size_t   from_dash_integral0 (    size_t  x68 ) {
    return (  x68 );
}

static  int32_t   from_dash_integral2 (    size_t  x50 ) {
    return ( (int32_t ) (  x50 ) );
}

static  int32_t   lc_dash_ctype1 (  ) {
    return (  from_dash_integral2 ( 0 ) );
}

static  const char*   from_dash_string3 (    uint8_t *  ptr86 ,    size_t  dref87 ) {
    return ( ( (const char* ) (  ptr86 ) ) );
}

struct Array_5 {
    uint8_t _arr [32];
};

struct Termios_4 {
    uint32_t  f_c_dash_iflag;
    uint32_t  f_c_dash_oflag;
    uint32_t  f_c_dash_cflag;
    uint32_t  f_c_dash_lflag;
    uint8_t  f_c_dash_line;
    struct Array_5  f_c_dash_cc;
    uint32_t  f_c_dash_ispeed;
    uint32_t  f_c_dash_ospeed;
};

int main(int argc, const char **argv) {
	_global_argc = argc; _global_argv = argv;
    size_t  starting_dash_size2447 = ( (  from_dash_integral0 ( 8 ) ) );
    size_t  growth_dash_factor2448 = ( (  from_dash_integral0 ( 2 ) ) );
    size_t  shrink_dash_factor2449 = ( (  from_dash_integral0 ( 8 ) ) );
    ( ( setlocale ) ( ( (  lc_dash_ctype1 ) ( ) ) ,  ( (  from_dash_string3 ) ( ( (uint8_t*)"" ) ,  ( 0 ) ) ) ) );
    struct Termios_4  temp6;
    struct Termios_4  x3381 = ( (  temp6 ) );
}

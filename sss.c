#include <stddef.h>
#include <locale.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
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

enum Unit_5 {
    Unit_5_Unit,
};

struct Slice_7 {
    uint8_t *  f_ptr;
    size_t  f_count;
};

struct StrView_6 {
    struct Slice_7  f_contents;
};

struct Char_10 {
    uint8_t *  f_ptr;
    size_t  f_num_dash_bytes;
};

struct StrConcat_9 {
    struct StrView_6  field0;
    struct Char_10  field1;
};

static struct StrConcat_9 StrConcat_9_StrConcat (  struct StrView_6  field0 ,  struct Char_10  field1 ) {
    return ( struct StrConcat_9 ) { .field0 = field0 ,  .field1 = field1 };
};

struct StrViewIter_13 {
    struct StrView_6  f_ds;
    size_t  f_i;
};

enum EmptyIter_15 {
    EmptyIter_15_EmptyIter,
};

struct AppendIter_14 {
    enum EmptyIter_15  f_it;
    struct Char_10  f_elem;
    bool  f_appended;
};

struct StrConcatIter_12 {
    struct StrViewIter_13  f_left;
    struct AppendIter_14  f_right;
};

static  struct StrConcatIter_12   into_dash_iter17 (    struct StrConcatIter_12  self1290 ) {
    return (  self1290 );
}

struct Maybe_18 {
    enum {
        Maybe_18_None_t,
        Maybe_18_Just_t,
    } tag;
    union {
        struct {
            struct Char_10  field0;
        } Maybe_18_Just_s;
    } stuff;
};

static struct Maybe_18 Maybe_18_Just (  struct Char_10  field0 ) {
    return ( struct Maybe_18 ) { .tag = Maybe_18_Just_t, .stuff = { .Maybe_18_Just_s = { .field0 = field0 } } };
};

enum Ordering_22 {
    Ordering_22_LT,
    Ordering_22_EQ,
    Ordering_22_GT,
};

static size_t builtin_size_tcmp (size_t l, size_t r) {
    return (l < r) ? 0 : (l == r) ? 1 : 2;
}

static  enum Ordering_22   cmp21 (    size_t  l193 ,    size_t  r195 ) {
    return ( builtin_size_tcmp( (  l193 ) , (  r195 ) ) );
}

static  int64_t   op_dash_mul24 (    int64_t  l213 ,    int64_t  r215 ) {
    return ( (  l213 ) * (  r215 ) );
}

static  uint8_t *   offset_dash_ptr23 (    uint8_t *  x377 ,    int64_t  count379 ) {
    uint8_t  temp25;
    return ( (uint8_t * ) ( ( (void*) (  x377 ) ) + (  op_dash_mul24 ( (  count379 ) , ( (int64_t ) ( sizeof( ( (  temp25 ) ) ) ) ) ) ) ) );
}

static  uint8_t *   cast26 (    uint8_t *  x395 ) {
    return ( (uint8_t * ) (  x395 ) );
}

static  int64_t   size_dash_i6427 (    size_t  x672 ) {
    return ( (int64_t ) (  x672 ) );
}

static uint8_t builtin_uint8_tcmp (uint8_t l, uint8_t r) {
    return (l < r) ? 0 : (l == r) ? 1 : 2;
}

static  enum Ordering_22   cmp30 (    uint8_t  l178 ,    uint8_t  r180 ) {
    return ( builtin_uint8_tcmp( (  l178 ) , (  r180 ) ) );
}

static  uint8_t   from_dash_integral31 (    size_t  x62 ) {
    return ( (uint8_t ) (  x62 ) );
}

static  size_t   next_dash_char29 (    uint8_t *  p1263 ) {
    uint8_t  pb1264 = ( * (  p1263 ) );
    if ( (  cmp30 ( (  pb1264 ) , (  from_dash_integral31 ( 128 ) ) ) == 0 ) ) {
        return (  from_dash_integral0 ( 1 ) );
    } else {
        if ( (  cmp30 ( (  pb1264 ) , (  from_dash_integral31 ( 240 ) ) ) != 0 ) ) {
            return (  from_dash_integral0 ( 4 ) );
        } else {
            if ( (  cmp30 ( (  pb1264 ) , (  from_dash_integral31 ( 224 ) ) ) != 0 ) ) {
                return (  from_dash_integral0 ( 3 ) );
            } else {
                if ( (  cmp30 ( (  pb1264 ) , (  from_dash_integral31 ( 192 ) ) ) != 0 ) ) {
                    return (  from_dash_integral0 ( 2 ) );
                } else {
                    const char*  temp32 = ( (  from_dash_string3 ) ( ( (uint8_t*)"(TODO) invalid byte (handle this better...)" ) ,  ( 43 ) ) );
                    printf("%s\n", temp32);
                    abort ( );
                    ( Unit_5_Unit );
                }
            }
        }
    }
}

static  struct Char_10   scan_dash_from_dash_mem28 (    uint8_t *  p1267 ) {
    size_t  clen1268 = ( (  next_dash_char29 ) ( (  p1267 ) ) );
    if ( (  cmp21 ( (  clen1268 ) , (  from_dash_integral0 ( 4 ) ) ) == 2 ) ) {
        const char*  temp33 = ( (  from_dash_string3 ) ( ( (uint8_t*)"UTF8 STRING TOO LONG (is this correct utf8 string?)" ) ,  ( 51 ) ) );
        printf("%s\n", temp33);
        abort ( );
        ( Unit_5_Unit );
    }
    return ( (struct Char_10) { .f_ptr = (  p1267 ) , .f_num_dash_bytes = (  clen1268 ) } );
}

static  size_t   op_dash_add34 (    size_t  l289 ,    size_t  r291 ) {
    return ( (  l289 ) + (  r291 ) );
}

static  struct Maybe_18   next20 (    struct StrViewIter_13 *  self1318 ) {
    if ( (  cmp21 ( ( ( * (  self1318 ) ) .f_i ) , ( ( ( ( * (  self1318 ) ) .f_ds ) .f_contents ) .f_count ) ) != 0 ) ) {
        return ( (struct Maybe_18) { .tag = Maybe_18_None_t } );
    }
    uint8_t *  char_dash_ptr1319 = ( ( (  offset_dash_ptr23 ) ( ( (  cast26 ) ( ( ( ( ( * (  self1318 ) ) .f_ds ) .f_contents ) .f_ptr ) ) ) ,  ( (  size_dash_i6427 ) ( ( ( * (  self1318 ) ) .f_i ) ) ) ) ) );
    struct Char_10  char1320 = ( (  scan_dash_from_dash_mem28 ) ( (  char_dash_ptr1319 ) ) );
    (*  self1318 ) .f_i = (  op_dash_add34 ( ( ( * (  self1318 ) ) .f_i ) , ( (  char1320 ) .f_num_dash_bytes ) ) );
    return ( ( Maybe_18_Just ) ( (  char1320 ) ) );
}

static  struct Maybe_18   next36 (    enum EmptyIter_15 *  dref800 ) {
    return ( (struct Maybe_18) { .tag = Maybe_18_None_t } );
}

static  struct Maybe_18   next35 (    struct AppendIter_14 *  self1047 ) {
    struct Maybe_18  dref1048 = ( (  next36 ) ( ( & ( ( * (  self1047 ) ) .f_it ) ) ) );
    if ( dref1048.tag == Maybe_18_Just_t ) {
        return ( ( Maybe_18_Just ) ( ( dref1048 .stuff .Maybe_18_Just_s .field0 ) ) );
    }
    else {
        if ( dref1048.tag == Maybe_18_None_t ) {
            if ( ( ! ( ( * (  self1047 ) ) .f_appended ) ) ) {
                (*  self1047 ) .f_appended = ( true );
                return ( ( Maybe_18_Just ) ( ( ( * (  self1047 ) ) .f_elem ) ) );
            }
            return ( (struct Maybe_18) { .tag = Maybe_18_None_t } );
        }
    }
}

static  struct Maybe_18   next19 (    struct StrConcatIter_12 *  self1293 ) {
    struct Maybe_18  dref1294 = ( (  next20 ) ( ( & ( ( * (  self1293 ) ) .f_left ) ) ) );
    if ( dref1294.tag == Maybe_18_Just_t ) {
        return ( ( Maybe_18_Just ) ( ( dref1294 .stuff .Maybe_18_Just_s .field0 ) ) );
    }
    else {
        if ( dref1294.tag == Maybe_18_None_t ) {
            return ( (  next35 ) ( ( & ( ( * (  self1293 ) ) .f_right ) ) ) );
        }
    }
}

static  enum Unit_5   for_dash_each11 (    struct StrConcatIter_12  iterable1099 ,    enum Unit_5 (*  fun1101 )(    struct Char_10  ) ) {
    struct StrConcatIter_12  temp16 = ( (  into_dash_iter17 ) ( (  iterable1099 ) ) );
    struct StrConcatIter_12 *  it1102 = ( &temp16 );
    while ( ( true ) ) {
        struct Maybe_18  dref1103 = ( (  next19 ) ( (  it1102 ) ) );
        if ( dref1103.tag == Maybe_18_None_t ) {
            return ( Unit_5_Unit );
        }
        else {
            if ( dref1103.tag == Maybe_18_Just_t ) {
                ( (  fun1101 ) ( ( dref1103 .stuff .Maybe_18_Just_s .field0 ) ) );
            }
        }
    }
    return ( Unit_5_Unit );
}

static  struct StrViewIter_13   into_dash_iter40 (    struct StrView_6  self1312 ) {
    return ( (struct StrViewIter_13) { .f_ds = (  self1312 ) , .f_i = (  from_dash_integral0 ( 0 ) ) } );
}

static  struct StrViewIter_13   chars39 (    struct StrView_6  self1326 ) {
    return ( (  into_dash_iter40 ) ( (  self1326 ) ) );
}

static  enum EmptyIter_15   into_dash_iter45 (    enum EmptyIter_15  self798 ) {
    return (  self798 );
}

static  struct AppendIter_14   append44 (    enum EmptyIter_15  it1031 ,    struct Char_10  e1033 ) {
    return ( (struct AppendIter_14) { .f_it = ( (  into_dash_iter45 ) ( (  it1031 ) ) ) , .f_elem = (  e1033 ) , .f_appended = ( false ) } );
}

static  struct AppendIter_14   cons43 (    enum EmptyIter_15  it1036 ,    struct Char_10  e1038 ) {
    return ( (  append44 ) ( (  it1036 ) ,  (  e1038 ) ) );
}

static  enum EmptyIter_15   nil46 (  ) {
    return ( EmptyIter_15_EmptyIter );
}

static  struct AppendIter_14   single42 (    struct Char_10  e1041 ) {
    return ( (  cons43 ) ( ( (  nil46 ) ( ) ) ,  (  e1041 ) ) );
}

static  struct AppendIter_14   chars41 (    struct Char_10  self1286 ) {
    return ( (  single42 ) ( (  self1286 ) ) );
}

static  struct StrConcatIter_12   into_dash_iter38 (    struct StrConcat_9  dref1297 ) {
    return ( (struct StrConcatIter_12) { .f_left = ( (  chars39 ) ( ( dref1297 .field0 ) ) ) , .f_right = ( (  chars41 ) ( ( dref1297 .field1 ) ) ) } );
}

static  struct StrConcatIter_12   chars37 (    struct StrConcat_9  self1308 ) {
    return ( (  into_dash_iter38 ) ( (  self1308 ) ) );
}

struct Array_49 {
    uint8_t _arr [4];
};

struct Scalar_51 {
    uint32_t  f_value;
};

struct CharDestructured_50 {
    enum {
        CharDestructured_50_Ref_t,
        CharDestructured_50_Scalar_t,
    } tag;
    union {
        struct {
            struct Char_10  field0;
        } CharDestructured_50_Ref_s;
        struct {
            struct Scalar_51  field0;
        } CharDestructured_50_Scalar_s;
    } stuff;
};

static struct CharDestructured_50 CharDestructured_50_Ref (  struct Char_10  field0 ) {
    return ( struct CharDestructured_50 ) { .tag = CharDestructured_50_Ref_t, .stuff = { .CharDestructured_50_Ref_s = { .field0 = field0 } } };
};

static struct CharDestructured_50 CharDestructured_50_Scalar (  struct Scalar_51  field0 ) {
    return ( struct CharDestructured_50 ) { .tag = CharDestructured_50_Scalar_t, .stuff = { .CharDestructured_50_Scalar_s = { .field0 = field0 } } };
};

static  bool   eq53 (    uint8_t  l125 ,    uint8_t  r127 ) {
    return ( (  l125 ) == (  r127 ) );
}

static  uint8_t   size_dash_u855 (    size_t  x720 ) {
    return ( (uint8_t ) (  x720 ) );
}

static  size_t   op_dash_div56 (    size_t  l304 ,    size_t  r306 ) {
    return ( (  l304 ) / (  r306 ) );
}

static  uint8_t   get_dash_ms_dash_byte_dash_of_dash_pointer54 (    uint8_t *  ptr783 ) {
    return ( (  size_dash_u855 ) ( (  op_dash_div56 ( ( ( (size_t ) (  ptr783 ) ) ) , (  from_dash_integral0 ( 72057594037927936 ) ) ) ) ) );
}

static  uint32_t   size_dash_u3257 (    size_t  x714 ) {
    return ( (uint32_t ) (  x714 ) );
}

static  size_t   cast58 (    uint8_t *  x395 ) {
    return ( (size_t ) (  x395 ) );
}

static  struct CharDestructured_50   destructure52 (    struct Char_10  c1244 ) {
    if ( (  eq53 ( ( (  get_dash_ms_dash_byte_dash_of_dash_pointer54 ) ( ( (  c1244 ) .f_ptr ) ) ) , (  from_dash_integral31 ( 103 ) ) ) ) ) {
        return ( ( CharDestructured_50_Scalar ) ( ( (struct Scalar_51) { .f_value = ( (  size_dash_u3257 ) ( ( ( (  cast58 ) ( ( (  c1244 ) .f_ptr ) ) ) ) ) ) } ) ) );
    } else {
        return ( ( CharDestructured_50_Ref ) ( (  c1244 ) ) );
    }
}

static uint32_t builtin_uint32_tcmp (uint32_t l, uint32_t r) {
    return (l < r) ? 0 : (l == r) ? 1 : 2;
}

static  enum Ordering_22   cmp60 (    uint32_t  l173 ,    uint32_t  r175 ) {
    return ( builtin_uint32_tcmp( (  l173 ) , (  r175 ) ) );
}

static  uint32_t   from_dash_integral61 (    size_t  x53 ) {
    return ( (uint32_t ) (  x53 ) );
}

static  uint8_t   cast64 (    uint32_t  x395 ) {
    return ( (uint8_t ) (  x395 ) );
}

static  uint8_t   u32_dash_u863 (    uint32_t  x741 ) {
    return ( (  cast64 ) ( (  x741 ) ) );
}

static  struct Array_49   from_dash_listlike65 (    struct Array_49 *  self369 ) {
    return ( * (  self369 ) );
}

static  struct Array_49   unscalarize59 (    struct Scalar_51  scalar1247 ) {
    if ( (  cmp60 ( ( (  scalar1247 ) .f_value ) , (  from_dash_integral61 ( 128 ) ) ) == 2 ) ) {
        const char*  temp62 = ( (  from_dash_string3 ) ( ( (uint8_t*)"(unscalarize) non-ascii characters not supported for now" ) ,  ( 56 ) ) );
        printf("%s\n", temp62);
        abort ( );
        ( Unit_5_Unit );
    }
    uint8_t  b1248 = ( (  u32_dash_u863 ) ( ( (  scalar1247 ) .f_value ) ) );
    struct Array_49  temp66 = ( (struct Array_49) { ._arr = { (  b1248 ) , (  from_dash_integral31 ( 0 ) ) , (  from_dash_integral31 ( 0 ) ) , (  from_dash_integral31 ( 0 ) ) } } );
    return ( (  from_dash_listlike65 ) ( ( &temp66 ) ) );
}

static  uint8_t *   cast_dash_ptr67 (    struct Array_49 *  p398 ) {
    return ( (uint8_t * ) (  p398 ) );
}

static  struct Char_10   regularize48 (    struct Char_10  c1251 ,    struct Array_49 *  possible_dash_scalar_dash_mem1253 ) {
    struct CharDestructured_50  dref1254 = ( (  destructure52 ) ( (  c1251 ) ) );
    if ( dref1254.tag == CharDestructured_50_Ref_t ) {
        return ( dref1254 .stuff .CharDestructured_50_Ref_s .field0 );
    }
    else {
        if ( dref1254.tag == CharDestructured_50_Scalar_t ) {
            (*  possible_dash_scalar_dash_mem1253 ) = ( (  unscalarize59 ) ( ( dref1254 .stuff .CharDestructured_50_Scalar_s .field0 ) ) );
            return ( (struct Char_10) { .f_ptr = ( (  cast_dash_ptr67 ) ( (  possible_dash_scalar_dash_mem1253 ) ) ) , .f_num_dash_bytes = ( (  c1251 ) .f_num_dash_bytes ) } );
        }
    }
}

static  int32_t   size_dash_i3270 (    size_t  x711 ) {
    return ( (int32_t ) (  x711 ) );
}

static  enum Unit_5   printf_dash_char47 (    struct Char_10  c1259 ) {
    struct Array_49  temp69;
    struct Array_49  temp68 = (  temp69 );
    struct Char_10  c1260 = ( (  regularize48 ) ( (  c1259 ) ,  ( &temp68 ) ) );
    ( ( printf ) ( ( (  from_dash_string3 ) ( ( (uint8_t*)"%.*s" ) ,  ( 4 ) ) ) ,  ( (  size_dash_i3270 ) ( ( (  c1260 ) .f_num_dash_bytes ) ) ) ,  ( (  c1260 ) .f_ptr ) ) );
    return ( Unit_5_Unit );
}

static  enum Unit_5   print8 (    struct StrConcat_9  s1335 ) {
    ( (  for_dash_each11 ) ( ( (  chars37 ) ( (  s1335 ) ) ) ,  (  printf_dash_char47 ) ) );
    return ( Unit_5_Unit );
}

static  bool   eq72 (    size_t  l135 ,    size_t  r137 ) {
    return ( (  l135 ) == (  r137 ) );
}

static  struct Char_10   from_dash_charlike71 (    uint8_t *  ptr1271 ,    size_t  num_dash_bytes1273 ) {
    struct Char_10  le_dash_char1274 = ( (  scan_dash_from_dash_mem28 ) ( (  ptr1271 ) ) );
    if ( ( !  eq72 ( ( (  le_dash_char1274 ) .f_num_dash_bytes ) , (  num_dash_bytes1273 ) ) ) ) {
        const char*  temp73 = ( (  from_dash_string3 ) ( ( (uint8_t*)"invalid char given. number of scanned bytes should match the given num-bytes." ) ,  ( 77 ) ) );
        printf("%s\n", temp73);
        abort ( );
        ( Unit_5_Unit );
    }
    return (  le_dash_char1274 );
}

static  enum Unit_5   println4 (    struct StrView_6  s1338 ) {
    ( (  print8 ) ( ( ( StrConcat_9_StrConcat ) ( (  s1338 ) ,  ( (  from_dash_charlike71 ) ( ( (uint8_t*)"\n" ) ,  ( 1 ) ) ) ) ) ) );
    return ( Unit_5_Unit );
}

static  struct StrView_6   from_dash_string74 (    uint8_t *  ptr90 ,    size_t  count92 ) {
    return ( (struct StrView_6) { .f_contents = ( (struct Slice_7) { .f_ptr = (  ptr90 ) , .f_count = (  count92 ) } ) } );
}

int main(int argc, const char **argv) {
	_global_argc = argc; _global_argv = argv;
    size_t  starting_dash_size2447 = ( (  from_dash_integral0 ( 8 ) ) );
    size_t  growth_dash_factor2448 = ( (  from_dash_integral0 ( 2 ) ) );
    size_t  shrink_dash_factor2449 = ( (  from_dash_integral0 ( 8 ) ) );
    ( ( setlocale ) ( ( (  lc_dash_ctype1 ) ( ) ) ,  ( (  from_dash_string3 ) ( ( (uint8_t*)"" ) ,  ( 0 ) ) ) ) );
    ( (  println4 ) ( ( (  from_dash_string74 ) ( ( (uint8_t*)"kupsko" ) ,  ( 6 ) ) ) ) );
}

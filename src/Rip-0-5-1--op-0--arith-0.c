
 
#include "Rip-0-5-1.h"

int
  Rippaddr_ipv6_not(
    uint64_t *ipv6, uint64_t *res
);
 
int
  Rippaddr_ipv6_add_int64(
    uint64_t *ipv6, int64_t addend, uint64_t *res
);

 
___RIP_inline
int
  Rippaddr_ipv4_neg(
    IPv4 ipv4, IPv4 *res
){
   
 *res = (~ipv4) + 1L;
   
  return 1;
}
 
RIP_OP1_ARITH_IP(v4, neg, Rippaddr_ipv4_neg);

___RIP_inline
int
  Rippaddr_ipv6_neg(
    uint64_t *ipv6, uint64_t *res
){
   
  uint64_t tmp[2];
  int64_t  one=1L;
   
  tmp[0] = ~ipv6[0] ;
   
  tmp[1] = ~ipv6[1] ;

  Rippaddr_ipv6_add_int64( 
      tmp
    , one
    , res
  );
 
   
  return 1;
}
 
 
RIP_OP1_ARITH_IP(v6, neg, Rippaddr_ipv6_neg);

___RIP_inline
int
  Rippaddr_ipv4_add_int32(
    IPv4 ipv4, int addend, IPv4 *res
){
   
  int64_t addend64 = (int64_t) addend;
  int64_t result   = (int64_t) ipv4 + addend64;
   
  if (
    (
      (addend64 < 0) != (result < ipv4)
    )
    || (
      result != (int64_t)(IPv4)result
    )
  ){
     
    return 0;
  }
 *res = result;

  return 1;
}
 
 
RIP_OP2_ARITH_NUM(v4, int32, add32, Rippaddr_ipv4_add_int32);

RIP_OP2_ARITH_NUM_1(v4cache, int32, v4cache, add32, Rippaddr_ipv4_add_int32);

___RIP_inline
int
  Rippaddr_ipv4_add_fl64(
    IPv4 ipv4, double addendfl64, IPv4 *res
){
  if( addendfl64>0xffffffff ) return 0;
  return Rippaddr_ipv4_add_int32(ipv4, (double) addendfl64, res );
}
 
RIP_OP2_ARITH_NUM(v4, float64, addfl64, Rippaddr_ipv4_add_fl64);

#if 0

 
int
  uint128_addc_uint64_asm1(
    uint64_t *ipv6, int64_t *addend, uint64_t *res
){
   
  int carry=-1;
   
   
  res[0] = 0;
  res[1] = *addend;

  __asm__ (
     
    "mov     (%2)  ,  %%rbx \n\t" 
     
    "mov    8(%2)  ,  %%rcx \n\t" 
     
    "mov    8(%1)  , %%rax \n\t"  
     
    "mov    %%rax  , %%r8 \n\t"   
     
    "shr    $0x3f  , %%rax \n\t"  
     
    "add    %%r8   ,  %%rcx \n\t" 
    "setc   %%dl   \n\t"
 
    "xor    %%al  ,%%dl  \n\t" 
     
    "test    %%dl ,  %%dl \n\t"
 
    "je equal \n\t"
    "STC \n\t"
    "equal: \n\t"
     
    "adc     (%1)  ,  %%rbx \n\t" 
     
    "setc   %%dl \n\t"
     
    "movzbl %%dl, %0  \n\t" 
     
    "mov   %%rbx,  (%1)  \n\t"
    "mov   %%rcx, 8(%1) \n\t"
    : "=r" (carry) , "=g" (res) 
    :  "g" (ipv6), "1" (res) 
     
    :"cc", "" "%rax",  "%rbx", "%rcx", "%edx", "r8"
    , "memory"
  );

  return carry;
}

 
___RIP_inline
int
  Rippaddr_ipv6_add_int32_asm1(
    uint64_t *ipv6, int addend, uint64_t *res
){
   
  int64_t __addend = (int64_t) addend;
  return !uint128_addc_uint64_asm1(ipv6, &__addend, res);
}
 
RIP_OP2_ARITH_NUM(v6, int32, addc32_1, Rippaddr_ipv6_add_int32_asm1);

int
  uint128_addc_asm0(
    uint64_t *ipv6, int64_t *addend, uint64_t *res
){
   
  int carry=-1;
   
  res[0] = 0;
  res[1] = *addend;

  __asm__ (
    "mov     (%2)  ,  %%rbx \n\t"  
    "mov    8(%2)  ,  %%rcx \n\t"  
    "add    8(%1)  ,  %%rcx \n\t"  
    "adc     (%1)  ,  %%rbx \n\t"  
     
    "setc   %%dl \n\t"
    "movzbl %%dl, %0  \n\t"  
     
    "mov   %%rbx,  (%1)  \n\t"
    "mov   %%rcx, 8(%1) \n\t"
    : "=r" (carry) , "=g" (res)  
     
    :  "g" (ipv6), "g" (res) 
    : "%rbx", "%rcx", "%edx", "memory" 
  );
 
   
  return carry;
}

 
int
  uint128_subc_asm0(
    uint64_t *ipv6, int64_t *addend, uint64_t *res
){
   
  int carry=0;
 
Rprintf("  %" PRIu64 ",%" PRIu64 , ipv6[0], ipv6[1] ) ;
   
  res[0] = 0;
  res[1] = * (uint64_t *) addend;
 
Rprintf(" %d\n", *addend ) ;
   
  __asm__ (
    "mov     (%1)  ,  %%rbx \n\t"
    "mov    8(%1)  ,  %%rcx \n\t"
    "sub    8(%2)  ,  %%rcx \n\t"
    "sbb     (%2)  ,  %%rbx \n\t"
     
    "setc   %%dl \n\t"
    "movzbl    %%dl, %0  \n\t"
     
    "mov   %%rbx,  (%2)  \n\t"
    "mov   %%rcx, 8(%2) \n\t"
    : "=r" (carry), "=g" (res)  
    :  "g" (ipv6), "g" (res)
    : "%rbx", "%rcx", "%edx",  "memory" 
  );
   
  return carry;
}

___RIP_inline
int
  Rippaddr_ipv6_add_int64_asm0(
    uint64_t *ipv6, int64_t addend, uint64_t *res
){
   
  int carry = 0;
   
  if (addend >= 0){

    carry = uint128_addc_asm0(ipv6, &addend, res);
  }
  else{

    int64_t __addend = (int64_t)(-addend);
    carry = uint128_subc_asm0(ipv6, &__addend, res);
  }
 
   
  return !carry;
} 

___RIP_inline
int
  Rippaddr_ipv6_add_int32_asm0(
    uint64_t *ipv6, int addend, uint64_t *res
){
   
  int valid = Rippaddr_ipv6_add_int64_asm0( ipv6, (int64_t) addend, res);
  return valid;
}
 
RIP_OP2_ARITH_NUM(v6, int32, addc32, Rippaddr_ipv6_add_int32_asm0);


#endif

___RIP_inline
int
  Rippaddr_ipv6_add_int64(
    uint64_t *ipv6, int64_t addend, uint64_t *res
){
   
  if (addend >= 0){
    res[1] = ipv6[1] + addend;
    res[0] = ipv6[0] + (res[1] < ipv6[1]);
  }
  else{
    res[1] = ipv6[1] - (uint64_t)(-addend);
    res[0] = ipv6[0] - (res[1] > ipv6[1]);
  }
  if ((addend < 0) != Ripaddr_ipv6_cmp_lt(res,ipv6) ){
    return 0;
  }
 
#if 0
Rprintf("    (%" PRIu64 ",%" PRIu64 ") + %" PRId64 " =(%" PRIu64 ",%" PRIu64 ")\n", ipv6[0], ipv6[1] , addend, res[0], res[1] );
 
Rprintf("    %d\n", ipv6[1]==res[1]);
char ipstringbuff[IP6_STRING_SZMAX]; 
ipv6_raw_output(ipv6, (char*) &ipstringbuff,IP6_STRING_SZMAX);     
Rprintf ("  %16s", ipstringbuff); 
ipv6_raw_output(res, (char*) &ipstringbuff,IP6_STRING_SZMAX);     
Rprintf ("  %16s\n", ipstringbuff);
#endif
   
  return 1;
} 
 
RIP_OP2_ARITH_NUM(v6, int64, add64, Rippaddr_ipv6_add_int64);

___RIP_inline
int
  Rippaddr_ipv6_add_int32(
    uint64_t *ipv6, int addend, uint64_t *res
){
  return Rippaddr_ipv6_add_int64( ipv6, (int64_t) addend, res);
}
 
RIP_OP2_ARITH_NUM(v6, int32, add32, Rippaddr_ipv6_add_int32);

___RIP_inline
int
  Rippaddr_ipv6_add_float64(
    uint64_t *ipv6, double addend, uint64_t *res
){
  if( fabs(addend)>9223372036854775807. ) return 0;
  return Rippaddr_ipv6_add_int64( ipv6, (int64_t) addend, res);
}
 
RIP_OP2_ARITH_NUM(v6, float64, addfl64, Rippaddr_ipv6_add_float64);

___RIP_inline
int
  Rippaddr_ipv4_sub_int32(
    IPv4 ipv4, int subtrahend, IPv4 *res
){
   
 *res   =  ipv4 - (IPv4) subtrahend;
 
   
  if (
     
    (subtrahend > 0) != ( *res  < ipv4)
  ){
     
    return 0;
  }
 
   
  return 1;
}
 
RIP_OP2_ARITH_NUM(v4, int32, sub32, Rippaddr_ipv4_sub_int32);

___RIP_inline
int
  Rippaddr_ipv4_sub_float64(
    IPv4 ip, double addend, IPv4 *res
){
  return Rippaddr_ipv4_sub_int32( ip, (int32_t) addend, res);
}
 
RIP_OP2_ARITH_NUM(v4, float64, subfl64, Rippaddr_ipv4_sub_float64);
 
 
___RIP_inline
int
  Rippaddr_ipv6_sub_int64(
    uint64_t *ipv6, int64_t subtrahend, uint64_t *res
){
   
  uint64_t res_lo;
  if (subtrahend >= 0){
    res_lo = ipv6[1] - (uint64_t)(subtrahend);
    res[0] = ipv6[0] - (res_lo > ipv6[1]);
  }
  else{
    res_lo = ipv6[1] + (uint64_t)(-subtrahend);
    res[0] = ipv6[0] + (res_lo < ipv6[1]);
  }
   
	res[1] = res_lo;
   
  if ((subtrahend > 0) != Ripaddr_ipv6_cmp_lt(res,ipv6)){
    return 0;
  }
   
  return 1;
}
 
RIP_OP2_ARITH_NUM(v6, int64, sub64, Rippaddr_ipv6_sub_int64);

 
___RIP_inline
int
  Rippaddr_ipv6_sub_int32(
    uint64_t *ipv6, int addend, uint64_t *res
){
  return Rippaddr_ipv6_sub_int64( ipv6, (int64_t) addend, res);
}
 
RIP_OP2_ARITH_NUM(v6, int32, sub32, Rippaddr_ipv6_sub_int32);

 
___RIP_inline
int
  Rippaddr_ipv6_sub_float64(
    uint64_t *ipv6, double addend, uint64_t *res
){
  if( fabs(addend)>9223372036854775807. ) return 0;
  return Rippaddr_ipv6_sub_int64( ipv6, (int64_t) addend, res);
}
 
RIP_OP2_ARITH_NUM(v6, float64, subfl64, Rippaddr_ipv6_sub_float64);

___RIP_inline 
int
  Rippaddr_ipv4_add_uint32(
    IPv4 ipv4, uint32_t addend, IPv4 *res
){
   
  if ( ipv4 > ( UINT_MAX - addend) ){
     
    return 0;
  }
   
 *res   =  ipv4 + addend;

  return 1;
}
 
 
RIP_OP2_ARITH_IP(v4, addv4, Rippaddr_ipv4_add_uint32);

RIP_OP2_ARITH_IP_1(v4cache, v4cache, v4cache, addv4, Rippaddr_ipv4_add_uint32);
 
RIP_OP2_ARITH_IP_1(v4cache, v4, v4cache, addv4, Rippaddr_ipv4_add_uint32);

___RIP_inline
int
  Rippaddr_ipv6_add_ipv6(
    uint64_t *ipv6, uint64_t *addend, uint64_t *res
){
 
   
  res[1] = ipv6[1] + addend[1];
  res[0] = ipv6[0] + addend[0] + ( res[1] < ipv6[1] ); 
 
   
  if(
     Ripaddr_ipv6_cmp_lt(res,ipv6)
  ){
    return 0;
  }
  return 1;
}
 
RIP_OP2_ARITH_IP(v6, addv6, Rippaddr_ipv6_add_ipv6);

 
___RIP_inline
int
  uint128_addc_uint128_asm0(
    uint64_t *x, uint64_t *addend, uint64_t *res
){
   
  int carry=-1;
   
  res[0] = addend[0];
  res[1] = addend[1];
 
  __asm__ __volatile__(
     
    "mov     (%2)  ,  %%rbx \n\t" 
     
    "mov    8(%2)  ,  %%rcx \n\t" 
     
    "add    8(%0)  ,  %%rcx \n\t" 
     
    "adc     (%0)  ,  %%rbx \n\t" 
    "setc   %%dl \n\t"
     
    "movzbl %%dl, %1  \n\t" 
     
    "mov   %%rbx,  (%0)  \n\t"
    "mov   %%rcx, 8(%0) \n\t"
    : "+r" (res) , "=r" (carry) 
    :  "r" (x)
    : "%rbx", "%rcx", "%edx", "memory", "cc"
  );

  return !carry;
}

RIP_OP2_ARITH_IP(v6, addc_v6_0, uint128_addc_uint128_asm0);

 
___RIP_inline
int
  Rippaddr_ipv4_sub_uint32(
    IPv4 ipv4, uint32_t addend, IPv4 *res
){
   
  if (
    (ipv4 < addend)
  ){
     
    return 0;
  }
   
 *res   =  ipv4 - addend;

  return 1;
}
 
RIP_OP2_ARITH_IP(v4, subv4, Rippaddr_ipv4_sub_uint32);

 
___RIP_inline
int
  Rippaddr_ipv6_sub_ipv6(
    uint64_t *ipv6, uint64_t *subtrahend, uint64_t *res
){
   
   
  res[1] = ipv6[1] - subtrahend[1];
   
  res[0] = ipv6[0] - subtrahend[0] - ( res[1] > ipv6[1] ); 
   
  if(
      
     Ripaddr_ipv6_cmp_lt(ipv6, res)
  ){
    return 0;
  }
  return 1;
}
 
RIP_OP2_ARITH_IP(v6, subv6, Rippaddr_ipv6_sub_ipv6);

___RIP_inline
int
  Rippaddr_ipv4_rshift(
    IPv4 ipv4, int n, IPv4 *res
){
   
  if( n<0 ){
    return 0;
  }
   
 *res = ipv4 >> n;
   
  return 1;
}
 
RIP_OP2_ARITH_NUM(v4, int32, rshift, Rippaddr_ipv4_rshift);
 
 
___RIP_inline
int
  Rippaddr_ipv4_lshift(
    IPv4 ipv4, int n, IPv4 *res
){ 
   
  if( n<0 ){
    return 0;
  }
   
 *res = ipv4 << n;
   
  return 1;
}
 
RIP_OP2_ARITH_NUM(v4, int32, lshift, Rippaddr_ipv4_lshift);

___RIP_inline
int
  Rippaddr_ipv6_not(
    uint64_t *ipv6, uint64_t *res
){
   
  res[0] = ~ipv6[0];
   
  res[1] = ~ipv6[1];

  return 1;
}
 
RIP_OP1_ARITH_IP(v6, not, Rippaddr_ipv6_not);

___RIP_inline
int
  Rippaddr_ipv6_rshift(
    uint64_t *ipv6, int n, uint64_t *res
){
   
  uint64_t mask, bits;
   
  if( (n<0) | (n>=128) ){
    return 0;
  }
  if( n>=64 ){
 
 
    res[0] = 0;
    res[1] = ipv6[0] >> ( n - 64 );
 
    return 1;
  }
   
  mask = ( 1U << (n) ) - 1U;
   
  bits = (ipv6[0] & mask) << (64 - n);
   
  res[0] = ipv6[0] >> n;
  res[1] = ipv6[1] >> n;
   
  res[1] |= bits;
 
#if 0
 
Rprintf("%" PRIu64 " %" PRIu64 "\n", ipv6[0], ipv6[1]);
 
Rprintf("%d %" PRIu64 " %" PRIu64 "\n", n, mask, (ipv6[0] & mask));
 
Rprintf("%" PRIu64 " %" PRIu64 "\n", res[0], res[1]);
 
Rprintf("%f %f \n", (ipv6[0]*18446744073709551616.) + ipv6[1], (res[0]*18446744073709551616.) + res[1]);
 
#endif
   
  return 1;
}
 
RIP_OP2_ARITH_NUM(v6, int32, rshift, Rippaddr_ipv6_rshift);
 
 
___RIP_inline
int
  Rippaddr_ipv6_lshift(
    uint64_t *ipv6, int n, uint64_t *res
){
   
  uint64_t mask, bits;
   
  if( (n<0) | (n>=128) ){
    return 0;
  }
  if( n>=64 ){
 
 
    res[0] = ipv6[1] << ( n - 64 );
    res[1] = 0;
 
    return 1;  
  }
   
  mask = ( ( (uint64_t)1U << (n) ) - 1U) << (64-n);  
   
  bits = (ipv6[1] & mask) >> (64-n);
   
  res[0] = ipv6[0] << n;
  res[1] = ipv6[1] << n;
   
  res[0] |= bits ;
 
#if 0
 
Rprintf("%" PRIu64 " %" PRIu64 "\n", ipv6[0], ipv6[1]);
 
Rprintf("%d %" PRIu64 " %" PRIu64 "\n", n, mask, (ipv6[1] & mask));
 
Rprintf("%" PRIu64 " %" PRIu64 "\n", res[0], res[1]);
 
Rprintf("%f %f \n", (ipv6[0]*18446744073709551616.) + ipv6[1], (res[0]*18446744073709551616.) + res[1]);
 
#endif
   
  return 1;
}
 
RIP_OP2_ARITH_NUM(v6, int32, lshift, Rippaddr_ipv6_lshift);


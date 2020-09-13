 
 
#ifndef RIP_0_H
#define RIP_0_H

#include <math.h>
 
#include <float.h>
 
#include <immintrin.h>
 
#include <inttypes.h>
 
#include <ctype.h>
 
#include <strings.h>
 
#include <immintrin.h>

#include <sys/socket.h>  
 
#include <arpa/inet.h>   
 
 
#include <R.h>
 
#include <Rinternals.h>
 
#include <Rdefines.h>
 
 
#include "Rip-0-5-1--R-macros-0.h"
 
#include "Rip-0-5-1--IP-macros-0.h"
 
#include "Rip-0-5-1--itermacros-0.h"

typedef uint32_t IPv4;
 
typedef struct IPv4r {
    IPv4 lo;
    IPv4 hi;
} IPv4r;
 
 
typedef struct IPv6 {
    uint64_t ipv6[2];
} IPv6;
 
typedef struct IPv6r {
  IPv6 lo;
  IPv6 hi;
} IPv6r;
 
 
#define R_AF_INET (AF_INET + 0)
#define R_AF_INET6 (AF_INET6 + 0) 
 
#define IP4_STRING_SZMAX (sizeof("255.255.255.255"))
#define IP4R_STRING_SZMAX (2*IP4_STRING_SZMAX)
 
#define IP6_STRING_SZMAX (sizeof("ffff:ffff:ffff:ffff:ffff:ffff:255.255.255.255")+2)
#define IP6R_STRING_SZMAX (2*IP6_STRING_SZMAX)
 
#define IPv4_STRING_SZMAX (sizeof("255.255.255.255"))
#define IPv4R_STRING_SZMAX (2*IP4_STRING_SZMAX)
 
#define IPv6_STRING_SZMAX (sizeof("ffff:ffff:ffff:ffff:ffff:ffff:255.255.255.255")+2)
#define IPv6r_STRING_SZMAX (2*IP6_STRING_SZMAX)
 
 
#define ___RIP_inline __attribute__((always_inline)) inline  

SEXP
  Rip_dataSlotSym, Rip_ipfSym, Rip_idxSym
  , Rip_ipv4Sym, Rip_ipv6Sym
  , Rip_ipv4rSym, Rip_ipv6rSym
  , Rip_ipSym
  , Rip_iprSym
  , Rip_idSym
  , Rip_lenSym
   
  , host_hostnameSym
  , host_ipv4ptrSym
  , host_ipv6ptrSym
   
;

uint32_t hostmask(unsigned masklen);
 
uint32_t netmask(unsigned masklen);
 
unsigned  masklen(uint32_t lo, uint32_t hi);
 
uint64_t ipv6_hostmask_hi(unsigned masklen);
 
uint64_t ipv6_hostmask_lo(unsigned masklen);
 
uint64_t ipv6_netmask_hi(unsigned masklen);
 
uint64_t ipv6_netmask_lo(unsigned masklen);
 
unsigned ipv6_masklen64(uint64_t lo, uint64_t hi, int offset);
 
unsigned ipv6_masklen(IPv6 *lo, IPv6 *hi);
 
int ipv6_valid_netmask(uint64_t maskhi, uint64_t masklo);

int ipv4_raw_input(const char *osrc, uint32_t *dst);
 
int ipv4_raw_output(uint32_t ip, char *str, int len);
 
int ipv6_raw_input(const char *osrc, uint64_t *dst);
 
int ipv6_raw_output(uint64_t *ip, char *str, int len);

int  ipv4r_from_str(char *str, IPv4r *ipr);
 
int   ipv4r_raw_input(const char *str, IPv4r *ipr);
 
int ipv4_to_str(uint32_t ip, char *str, int len);

 
int ipv4r_to_str(IPv4 *ipr, char *str, int slen);

int ipv6r_raw_input( const char *str, IPv6r *ipr);
 
int ipv6r_to_str(IPv6r *ipr, char *str, int slen);
 
int ipv6r_raw_output(IPv6r *ipr, char *str, int slen);
 
void RIP_ipv4r_Rprintf_0(void* ipv6);
void RIP_ipv6_Rprintf_0(void* ipv6);

int 
  Ripaddr_ipv4_cmp_eq(
    IPv4 ip1, IPv4 ip2
);
 
int 
  Ripaddr_ipv4_cmp_lt(
   IPv4 ip1, IPv4 ip2
);
 
int 
  Ripaddr_ipv4_cmp_gt(
    IPv4 ip1, IPv4 ip2
);

int 
  Ripaddr_ipv4r_cmp_neq(
    IPv4 *ip1, IPv4 *ip2
);
 
 
int Ripaddr_ipv6_cmp_eq(uint64_t *ip1, uint64_t *ip2);
 
int Ripaddr_ipv6_cmp_lt(uint64_t *ip1, uint64_t *ip2);
 
int Ripaddr_ipv6_cmp_le(uint64_t *ip1, uint64_t *ip2);
 
int 
  Ripaddr_ipv6_cmp_gt(
  uint64_t *ip1, uint64_t *ip2
);
int 
  Ripaddr_ipv6_cmp_ge(
  uint64_t *ip1, uint64_t *ip2
);
 
 
int
  Rippaddr_ipv4_add_int32(
    IPv4 ipv4, int addend, IPv4 *res
);
 
int
  Rippaddr_ipv6_add_ipv6(
    uint64_t *ipv6, uint64_t *addend, uint64_t *res
);
int
  Rippaddr_ipv6_sub_ipv6(
    uint64_t *ipv6, uint64_t *subtrahend, uint64_t *res
);
int
  Rippaddr_ipv6_sub_int64(
    uint64_t *ipv6, int64_t subtrahend, uint64_t *res
);
 
int
  Rippaddr_ipv6_rshift(
    uint64_t *ipv6, int n, uint64_t *res
);
int
  Rippaddr_ipv6_lshift(
    uint64_t *ipv6, int n, uint64_t *res
);
 
 
uint64_t *
  Rippaddr_ipv6_and(
    uint64_t* ip1, uint64_t* ip2, uint64_t* res
);
 
 
SEXP arraycp(
  SEXP x
  , int xnc, int xnr
  , int nr
);

#ifdef RIP_HASH_DBG
#else
  #define RIP_HASH_DBG 0
#endif
 
 
typedef struct RIP_h {
  int  *htb;
  int   htb_sz;
  int   htb_nh;
  void *iptb;
  int   ip_len;
  int  *iptb_idx;
  int   iptb_i;
 
  int   ncoll;
  int   ncoll_ins;
  int   ncoll_lkup;
 
} RIP_hash;
 
typedef struct RIP_h32mpi {
  RIP_hash h;
  uint32_t shift;
} RIP_h32mulpi;
 
typedef struct RIP_h32dh {
  RIP_hash h;
  uint32_t M1;
  uint32_t M2;
} RIP_h32dblh;

int
  Rip_h32dblh_csearch_0_0(
      RIP_h32dblh  *hip
    , IPv4          ip
    , int          *hidx
);

 
typedef struct RIP_h64dhlem {
  RIP_hash h;
  uint32_t M2;
  uint64_t a0;
  uint64_t b0;
  uint64_t c0;
} RIP_h64dblh_lemire;
 
typedef struct RIP_h128dhlem {
  RIP_hash h;
  uint32_t M2;
  uint64_t a0;
  uint64_t b0;
  uint64_t c0;
  uint64_t a1;
  uint64_t b1;
  uint64_t c1;
} RIP_h128dblh_lemire;

IPv4 *RIP_cache_ipv4_val;
IPv4  RIP_cache_ipv4_nval;
int   RIP_cache_ipv4_val_i;
 
int  *RIP_cache_ipv4_htb;
 
 
int   RIP_cache_ipv4_htb_shift;
int   RIP_cache_ipv4_htb_nh;
 
int   RIP_cache_ipv4_ins_ncoll;
int   RIP_cache_ipv4_lkup_ncoll;

 
typedef struct RIPv4_h_t {
  IPv4    val;
  uint32_t set;
} RIPv4_h;
 
RIPv4_h *RIPv4_h_tb;

#define RIP_CACHE_NVAL ( (6000119)  )

int
  Rip_cache_ipv4_insert_0_0(
      IPv4 *ip
    , int   nip
    , int   *idx
);
int
  Rip_cache_ipv4_insertVal_0_0(
      IPv4 *ip
    , int  *idx
);

#endif
 

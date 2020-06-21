 
 
#include "Rip-0-5-1.h"

void
  Rip_init()
{
  Rip_dataSlotSym = install(".Data"); 
  Rip_ipfSym      = install("ipf"); 
  Rip_idxSym      = install("idx"); 
  Rip_ipv4Sym     = install("ipv4");
  Rip_ipv6Sym     = install("ipv6");
   
  Rip_ipv4rSym     = install("ipr");
  Rip_ipv6rSym     = install("ipr");
   
  Rip_ipSym       = install("ip"); 
  Rip_iprSym      = install("ipr"); 
   
  Rip_idSym      = install("id");
  Rip_lenSym      = install("length");
   
   
  host_hostnameSym  = install("hostname");
  host_ipv4ptrSym  = install("ipv4.hptr");
  host_ipv6ptrSym  = install("ipv6.hptr");

}

 
SEXP arraycp(
  SEXP x
  , int xnr, int xnc
  , int nr
){
   
  SEXP Res;
   
  switch (TYPEOF(x)){
    case INTSXP: 
       
      if( xnc>1 ){
 
         
        PROTECT(Res = allocMatrix(TYPEOF(x), nr, xnc));
      } 
      else{
 
        PROTECT(Res = allocVector(TYPEOF(x), nr));
      }  
       
      for(int j=0;j<xnc;j++){ 
        int *resptr = INTEGER( Res );
        int *xptr   = INTEGER( x );
        resptr = resptr + (nr*j);
        xptr   = xptr   + (xnr*j);
         
        memcpy( (void*) resptr , (void*) xptr , nr * sizeof(int) ); 
      }
    break;   
    case REALSXP:
       
      if( xnc>1 ){
 
         
        PROTECT(Res = allocMatrix(TYPEOF(x), nr, xnc));
      } 
      else{
 
        PROTECT(Res = allocVector(TYPEOF(x), nr));
      }  
       
      for(int j=0;j<xnc;j++){ 
        double *resptr = REAL( Res );
        double *xptr   = REAL( x );
        resptr = resptr + (nr*j);
        xptr   = xptr   + (xnr*j);
         
        memcpy( (void*) resptr , (void*) xptr , nr * sizeof(double) ); 
      }
    break; 
    default:
      error("unemplemented type");
  }
   
  UNPROTECT(1);
  return Res;
}

 
SEXP Rarraycp_0(
  SEXP x
  , int xnr, int xnc
  , int nr
){
   
  SEXP Res;
   
  switch (TYPEOF(x)){
     
    case INTSXP: 
       
      if( xnc>1 ){
 
         
        PROTECT(Res = allocMatrix(TYPEOF(x), nr, xnc));
      } 
      else{
 
        PROTECT(Res = allocVector(TYPEOF(x), nr));
      }  
       
      for(int j=0;j<xnc;j++){ 
        int *resptr = INTEGER( Res );
        int *xptr   = INTEGER( x );
        resptr = resptr + (nr*j);
        xptr   = xptr   + (xnr*j);
         
        memcpy( (void*) resptr , (void*) xptr , nr * sizeof(int) ); 
      }
    break;   
    case REALSXP:
       
      if( xnc>1 ){
 
         
        PROTECT(Res = allocMatrix(TYPEOF(x), nr, xnc));
      } 
      else{
 
        PROTECT(Res = allocVector(TYPEOF(x), nr));
      }  
       
      for(int j=0;j<xnc;j++){ 
        double *resptr = REAL( Res );
        double *xptr   = REAL( x );
        resptr = resptr + (nr*j);
        xptr   = xptr   + (xnr*j);
         
        memcpy( (void*) resptr , (void*) xptr , nr * sizeof(double) ); 
      }
    break; 
    default:
      error("unemplemented type");
  }
   
  UNPROTECT(1);
  return Res;
}

void RIP_ipv6_Rprintf_0(void* ipv6){
  char ipstringbuff[IP6_STRING_SZMAX]; 
  ipv6_raw_output((uint64_t*)ipv6, (char*) &ipstringbuff,IP6_STRING_SZMAX); 
  Rprintf("%s", ipstringbuff); 
}

 
___RIP_inline 
uint32_t  hostmask(unsigned masklen)
{
    return (masklen) ? ( (((uint32_t)(1U)) << (32-masklen)) - 1U ) : 0xFFFFFFFFU;
}
 
 ___RIP_inline 
uint32_t netmask(unsigned masklen)
{
    return ~hostmask(masklen);
}
 
___RIP_inline
unsigned  masklen(uint32_t lo, uint32_t hi)
{
    uint32_t d = (lo ^ hi) + 1;
     
    int fbit = ffs(d);
    switch (fbit)
    {
        case 0: return (lo == 0 && hi == ~0) ? 0 : ~0;
        case 1: return (lo == hi) ? 32 : ~0;
        default:
            if ( ((uint32_t)(1U) << (fbit-1)) == d )
            {
                uint32_t mask = hostmask(33-fbit);
                if ((lo & mask) == 0 && (hi & mask) == mask)
                    return 33-fbit;
            }
            return ~0;
    }
}
 
#if 0
 
uint64 Ripaddr_ipv6_hostmask_hi(unsigned masklen)
{
    if (masklen >= 64)
        return 0;
    if (masklen == 0)
        return ~((uint64)0);
    return (((uint64)(1U)) << (64-masklen)) - 1U;
}

 
uint64_t Ripaddr_ipv6_hostmask_lo(unsigned masklen)
{
    if (masklen <= 64)
        return ~((uint64)0);
    return (((uint64)(1U)) << (128-masklen)) - 1U;
}

 
uint64_t Ripaddr_ipv6_netmask_hi(unsigned masklen)
{
    return ~Ripaddr_ipv6_hostmask_hi(masklen);
}

 
uint64_t Ripaddr_ipv6_netmask_lo(unsigned masklen)
{
    return ~Ripaddr_ipv6_hostmask_lo(masklen);
}
#endif

#define RIP_IP_INIT(___IPv__) \
SEXP Rip_ip##___IPv__##_init_0( \
  SEXP Rip##___IPv__, SEXP Ripstrings \
){ \
   \
  int nprotected=0, dbg=0; \
  RIP_ITER1_DCL \
  nip = LENGTH( Ripstrings ); \
  if( !nip ){return  Rip##___IPv__;} \
if (dbg>0) Rprintf( STRINGIFY2(___IPv__) " init %d\n", nip)  ; \
  RIP##___IPv__##_SLOTS_ALLOC(Rip##___IPv__,nip)\
    \
  for (i=0 ; i <nip ; i++){ \
    int valid; \
 \
    RIP##___IPv__##_RES_DCL(res) \
      \
    valid = ip##___IPv__##_raw_input( CHAR(STRING_ELT(Ripstrings, i)) , resptr); \
    if(valid){ \
      RIP##___IPv__##_ITER_SET( Rip##___IPv__, i, res) \
  \
    } \
    else{ \
      Rip##___IPv__##_ip_idxptr[i] = NA_INTEGER; \
    } \
  } \
  RIP##___IPv__##_IS_NA_WARN_REPROTECT( Rip##___IPv__, nip, "init IP" STRINGIFY2(___IPv__) ) \
  RIP##___IPv__##_SLOTS_SET( Rip##___IPv__ ) \
  RIP_IP_ID_CP(Rip##___IPv__, Ripstrings) \
  UNPROTECT(nprotected); \
  return Rip##___IPv__; \
} \
 
RIP_IP_INIT(v4);
 
RIP_IP_INPUT( v4, string, _input_, init, ipv4_raw_input);

RIP_IP_INIT(v4r);
 
RIP_IP_INPUT( v4r, string, _input_, init, ipv4r_raw_input);
 
 
RIP_IP_INIT(v6);
 
RIP_IP_INIT(v6r);
 
RIP_IP_INPUT( v6r, string, _input_, init, ipv6r_raw_input);

 
#define RIP_IP_INIT_1(___IPv__) \
SEXP Rip_ip##___IPv__##_init_1( \
    SEXP Rip##___IPv__, SEXP Ripstrings \
){ \
  int nprotected=0; \
  RIP_ITER1_DCL \
  nip = LENGTH( Ripstrings ); \
  if( !nip ){return  Rip##___IPv__;} \
 \
  RIP##___IPv__##_SLOTS_TMP_ALLOC(Rip##___IPv__,nip) \
  for (i=0 ; i <nip ; i++){ \
    int valid; \
  \
    RIP##___IPv__##_RES_DCL(res) \
      \
    valid = ip##___IPv__##_raw_input( CHAR(STRING_ELT(Ripstrings, i)) , resptr); \
    if(valid){ \
      RIP##___IPv__##_ITER_SET( Rip##___IPv__, i, res) \
  \
    } \
    else{ \
      Rip##___IPv__##_ip_idxptr[i] = NA_INTEGER; \
    } \
  } \
   \
  RIP##___IPv__##_SLOTS_TMP_SET( Rip##___IPv__ , nip,  "init") \
  UNPROTECT(nprotected); \
  return Rip##___IPv__; \
} \
 
RIP_IP_INIT_1(v4);
 
RIP_IP_INIT_1(v4r);

 
#define RIP_IP_AS_CHARACTER(___IPv__) \
SEXP Rip_ip##___IPv__##_as_character_0(SEXP Rip ){ \
  SEXP Ripstrings;int i, nip=0, nprotected=0; \
  RIP##___IPv__##_SLOTS_GET( Rip ) \
  nip = Rip_nip; \
  PROTECT( Ripstrings = allocVector(STRSXP, nip ) );\
  nprotected++;\
  RIP_BEGIN \
  for (i=0 ; i <  nip; i++){ \
  \
    if( \
      Rip##_ip_idxptr[i]!=NA_INTEGER \
    ){ \
      char ipstringbuff[IP##___IPv__##_STRING_SZMAX];\
      RIP_CHECK_IDX(Rip##_ip_idxptr , i, nip) \
      RIP##___IPv__##_ELT_PTR_DCL(Rip, i) \
 \
      ip##___IPv__##_raw_output(Rip_ip_elt_ptr, (char*) &ipstringbuff, IP##___IPv__##_STRING_SZMAX); \
 \
      SET_STRING_ELT( Ripstrings, i, mkChar(ipstringbuff)); \
    }else{ \
      SET_STRING_ELT( Ripstrings, i, NA_STRING); \
    } \
  } \
  RIP_END \
  RIP_Rvec_IDSLOT_CP(Ripstrings, Rip) \
 \
  UNPROTECT( nprotected ); \
  return Ripstrings; \
} \
 
RIP_IP_AS_CHARACTER(v4);
 
RIP_IP_AS_CHARACTER(v6);

#define RIP_float64_ALLOC(___Rnum__, ___nip__) \
  SEXP ___Rnum__; \
  PROTECT( ___Rnum__ = allocVector(REALSXP, ___nip__ ) ); \
  double *  ___Rnum__##ptr = REAL(___Rnum__);   \
 
#define RIP_float64_SET( ___Rnum__, ___i__, ___val__ ) \
  ___Rnum__##ptr[___i__] = ___val__; \
 
#define RIP_float64_RES_SET( ___Rnum__, ___i__, ___fn__, ___arg__ ) \
  ___Rnum__##ptr[___i__] = ___fn__(___arg__); \
 
#define RIP_float64_NA_SET( ___Rnum__, ___i__) \
  ___Rnum__##ptr[___i__] = NA_REAL; \

#define RIP_fl64nx2_ALLOC(___Rnum__, ___nip__) \
  SEXP ___Rnum__; \
  PROTECT( ___Rnum__      = allocMatrix(REALSXP, ___nip__, 2 ) ); \
  int      ___Rnum__##nr  = ___nip__; \
  double * ___Rnum__##ptr = REAL(___Rnum__);   \
 
  \
 
#define RIP_fl64nx2_RES_SET( ___Rnum__, ___i__, ___fn__, ___arg__ ) \
  double ___Rnum__##res[2]; \
  ___fn__(___arg__, (double*) &___Rnum__##res); \
  ___Rnum__##ptr[___i__]               = ___Rnum__##res[0]; \
  ___Rnum__##ptr[___i__+___Rnum__##nr] = ___Rnum__##res[1]; \
 
#define RIP_fl64nx2_NA_SET( ___Rnum__, ___i__) \
  ___Rnum__##ptr[___i__]               = NA_REAL; \
  ___Rnum__##ptr[___i__+___Rnum__##nr] = NA_REAL; \

 
#define RIP_int32nx4_ALLOC(___Rint__, ___nip__) \
  SEXP ___Rint__; \
  PROTECT( ___Rint__      = allocMatrix(INTSXP, ___nip__, 4 ) ); \
  int      ___Rint__##nr  = ___nip__; \
  int * ___Rint__##ptr = INTEGER(___Rint__);   \
 
  \
 
#define RIP_int32nx4_RES_SET( ___Rint__, ___i__, ___fn__, ___arg__ ) \
  int ___Rint__##res[4]; \
  ___fn__(___arg__, (int*) &___Rint__##res); \
  ___Rint__##ptr[___i__                ]  = ___Rint__##res[0]; \
  ___Rint__##ptr[___i__+   ___Rint__##nr] = ___Rint__##res[1]; \
  ___Rint__##ptr[___i__+ 2*___Rint__##nr] = ___Rint__##res[2]; \
  ___Rint__##ptr[___i__+ 3*___Rint__##nr] = ___Rint__##res[3]; \
 
#define RIP_int32nx4_NA_SET( ___Rint__, ___i__) \
  ___Rint__##ptr[___i__]                  = NA_INTEGER; \
  ___Rint__##ptr[___i__+   ___Rint__##nr] = NA_INTEGER; \
  ___Rint__##ptr[___i__+ 2*___Rint__##nr] = NA_INTEGER; \
  ___Rint__##ptr[___i__+ 3*___Rint__##nr] = NA_INTEGER; \

#define RIP_int32vec_ALLOC(___Rint__, ___nip__) \
  SEXP ___Rint__; \
  PROTECT( ___Rint__ = allocVector(INTSXP, ___nip__ ) ); \
  int *  ___Rint__##ptr = INTEGER(___Rint__);   \
 
  \
 
#define RIP_int32vec_RES_SET( ___Rint__, ___i__, ___fn__, ___arg__ ) \
  ___Rint__##ptr[___i__] = ___fn__(___arg__); \
 
#define RIP_int32vec_NA_SET( ___Rint__, ___i__) \
  ___Rint__##ptr[___i__] = NA_INTEGER; \

#define RIP_IP_AS(___IPv__, ___R_t__, ___name__, ___fn__) \
SEXP Rip_ip##___IPv__##_##___name__##_0(SEXP Rip ){ \
  int i, nip=0, nprotected=0; \
    \
  \
  RIP##___IPv__##_SLOTS_GET( Rip ) \
  nip = Rip_nip; \
  \
    \
  RIP_##___R_t__##_ALLOC(Res, nip); \
  nprotected++;\
   \
  RIP_BEGIN \
  for (i=0 ; i <  nip; i++){ \
  \
 \
    if( \
      Rip##_ip_idxptr[i]!=NA_INTEGER \
    ){ \
      RIP_CHECK_IDX(Rip##_ip_idxptr , i, nip) \
       \
 \
       \
        \
      RIP##___IPv__##_ELT_PTR_DCL(Rip, i) \
      RIP_##___R_t__##_RES_SET(Res, i, ___fn__, Rip_ip_elt_ptr);\
       \
    }else{ \
       \
      RIP_##___R_t__##_NA_SET(Res, i); \
    } \
  } \
  RIP_END \
  RIP_Rvec_IDSLOT_CP(Res, Rip) \
  UNPROTECT( nprotected ); \
  return Res; \
} \
 
 
#define RIP_IP_AS_1(___IPv__, ___R_t__, ___name__, ___fn__) \
SEXP Rip_ip##___IPv__##_##___name__##_0( SEXP Rip ){ \
  int i, nip=0, nprotected=0; \
    \
  \
  RIP##___IPv__##_SLOTS_GET( Rip ) \
  nip = Rip_nip; \
  \
    \
  RIP_##___R_t__##_ALLOC(Res, nip); \
  nprotected++;\
   \
  RIP_BEGIN \
  for (i=0 ; i <  nip; i++){ \
  \
 \
    if( \
      Rip##_ip_idxptr[i]!=NA_INTEGER \
    ){ \
        \
      RIP##___IPv__##_ELT_PTR_DCL(Rip, i) \
      RIP_##___R_t__##_RES_SET(Res, i, ___fn__, Rip_ip_elt_ptr);\
       \
    }else{ \
       \
      RIP_##___R_t__##_NA_SET(Res, i); \
    } \
  } \
  RIP_END \
  RIP_Rvec_IDSLOT_CP(Res, Rip) \
  UNPROTECT( nprotected ); \
  return Res; \
} \
 
 
RIP_IP_AS(v4r, string, as_character, ipv4r_to_str);
 
RIP_IP_AS(v6r, string, as_character, ipv6r_to_str);
 
 
RIP_IP_AS_1(v4cache, string, as_character, ipv4_to_str);

double
  Rippaddr_ipv4_cvt_float64(
    IPv4 ipv4
){
  return (double) ipv4;
}
 
RIP_IP_AS(v4, float64, cvtfl64, Rippaddr_ipv4_cvt_float64);

double *
  Rippaddr_ipv4r_cvt_fl64(
    IPv4 *ipv4, double *res
){
  res[0] = (double) ipv4[0];
  res[1] = (double) ipv4[1];
  return res;
}
 
RIP_IP_AS(v4r, fl64nx2, cvtfl64, Rippaddr_ipv4r_cvt_fl64);

double
  Rippaddr_ipv6_cvt_float64(
    uint64_t *ipv6
){
   
  return ( ( double) ipv6[0] *  18446744073709551615. ) +  (double) ipv6[1];
}
 
RIP_IP_AS(v6, float64, cvtfl64, Rippaddr_ipv6_cvt_float64);

double*
  Rippaddr_ipv6_cvt_xprecfl64(
    uint64_t *ipv6, double *res
){

  res[0] = (double) ipv6[0] * 18446744073709551616. ;
   
  res[1] = (double) ipv6[1] ;
   
  return res;
}
 
RIP_IP_AS(v6, fl64nx2, cvtxprecfl64, Rippaddr_ipv6_cvt_xprecfl64);

 
int *
Rippaddr_ipv6_cvt_int32nx4(
    uint64_t *ipv6, int *res
){
   
  int *ipv6_int32ptr= (int*)ipv6;
   
  res[0] = ipv6_int32ptr[1] ;
   
  res[1] = ipv6_int32ptr[0] ;
   
  res[2] = ipv6_int32ptr[3] ;
   
  res[3] = ipv6_int32ptr[2] ;
   
  return res;
}
 
RIP_IP_AS(v6, int32nx4, cvtint32nx4, Rippaddr_ipv6_cvt_int32nx4);

double
  Rippaddr_ipv4r_range_float64(
    IPv4 *ip4r
){
  return (double) (  ip4r[1] - ip4r[0]  );
}
 
RIP_IP_AS(v4r, float64, range, Rippaddr_ipv4r_range_float64);

 
int
  Rippaddr_ipv4r_cidr_prefix(
    IPv4 *ip4r
){
   
  return (int) masklen( ip4r[0],  ip4r[1]);
}
 
RIP_IP_AS(v4r, int32vec, cidr_prefix, Rippaddr_ipv4r_cidr_prefix);

double
  Rippaddr_ipv6r_range_fl64(
    IPv6r *ipr
){
   
  uint64_t res[2];
  Rippaddr_ipv6_sub_ipv6((uint64_t *)&ipr->hi,(uint64_t *)&ipr->lo, (uint64_t *)&res);
 
#if 0
Rprintf("\nrange:\n");
Rprintf("ipr : %" PRIu64 " %" PRIu64 "\n" "      %" PRIu64 " %" PRIu64 "\n", ipr->lo.ipv6[0],ipr->lo.ipv6[1], ipr->hi.ipv6[0], ipr->hi.ipv6[1]);
Rprintf("cvt : %f\n" "      %f\n", Rippaddr_ipv6_cvt_float64( ( uint64_t *) &ipr->hi ) , Rippaddr_ipv6_cvt_float64( ( uint64_t *) &ipr->lo ));
Rprintf("res : %" PRIu64 " %" PRIu64 "\n" "resf: %f\n",res[0],res[1],  Rippaddr_ipv6_cvt_float64(res));
Rprintf("diff: %f\n", Rippaddr_ipv6_cvt_float64( ( uint64_t *) &ipr->hi ) - Rippaddr_ipv6_cvt_float64( ( uint64_t *) &ipr->lo ));
 
Rprintf("msk : %u\n", ipv6_masklen(&ipr->lo, &ipr->hi) );
 
#endif
   
   
  return Rippaddr_ipv6_cvt_float64(res);
}
 
RIP_IP_AS(v6r, float64, range, Rippaddr_ipv6r_range_fl64);

double*
  Rippaddr_ipv6r_cvt_float64(
    IPv6r *ipr, double *res 
){
   
  res[0] = Rippaddr_ipv6_cvt_float64( ( uint64_t *) &ipr->lo );
   
  res[1] = Rippaddr_ipv6_cvt_float64( ( uint64_t *) &ipr->hi );
   
  return res;
}
 
RIP_IP_AS(v6r, fl64nx2, cvtfl64, Rippaddr_ipv6r_cvt_float64);

#if 0
 
SEXP 
  Rip_IPv6_as_character_0(
    SEXP Ripv6
){
   
   
  SEXP Ripstrings, Ripv6_idx, Ripv6ip;
   
  int nipv6, *ipv6_idx;
  int i, nprotected=0;
   
  uint64_t *ipv6hiptr=NULL,*ipv6loptr=NULL;
   
   
  Ripv6_idx = GET_SLOT(Ripv6, Rip_dataSlotSym );
  nipv6     = XLENGTH( Ripv6_idx );
  ipv6_idx  = INTEGER( Ripv6_idx );
   
  int ipv6len = *INTEGER(GET_SLOT(Ripv6, Rip_lenSym ));
   
  Ripv6ip   = GET_SLOT(Ripv6, Rip_ipv6Sym );
  ipv6hiptr   = ( !isNull( Ripv6ip ) ) ? (uint64_t *) REAL( Ripv6ip )  : NULL;
  ipv6loptr   = ( !isNull( Ripv6ip ) ) ? (uint64_t *) REAL( Ripv6ip )+ipv6len  : NULL;
   
   
  PROTECT( Ripstrings = allocVector(STRSXP, nipv6 ) );
  nprotected=1;
   
   
  for (i=0 ; i <nipv6 ; i++){
     
    if(
      ipv6_idx[i] != NA_INTEGER
    ){
       
      char ipstringbuff[IP6_STRING_SZMAX];
       
      IPv6 ipv6;
      ipv6.ipv6[0] = ipv6hiptr[ ipv6_idx[i] ];
      ipv6.ipv6[1] = ipv6loptr[ ipv6_idx[i] ]; 
       
      ipv6_raw_output(&ipv6.ipv6, (char*) &ipstringbuff, IP6_STRING_SZMAX);
 
  Rprintf ("  [%d] <ipv6> %16s %16s %" PRId64 " %" PRId64 "\n", i, CHAR(STRING_ELT(Ripstrings, i)), ipstringbuff, ipv6.ipv6[0], ipv6.ipv6[1]);   
 
       
      SET_STRING_ELT( 
        Ripstrings
          , i
          , mkChar(ipstringbuff)  
      );
       
    }else{
       
      SET_STRING_ELT( 
        Ripstrings
          , i
          , NA_STRING
      );
    }
  }
   
   
  UNPROTECT( nprotected );
   
  return Ripstrings;
}
#endif

int
  Rippaddr_ipv6_input_int32(
    int in, uint64_t *ipv6
){
 
   
  ipv6[0] = 0;
   
  ipv6[1] = (uint64_t) in;
   
  return 1;
} 
 
RIP_IP_INPUT( v6, int32, _cvt_, input_int32, Rippaddr_ipv6_input_int32);

int
  Rippaddr_ipv4_netmask_int32(
    int prfxlen, IPv4 *res
){
   
  if(prfxlen < 0 || prfxlen > 32) return 0;
   
 *res = netmask( prfxlen );
   
  return 1;
}
 
RIP_IP_INPUT( v4, int32, _mask_, netmask, Rippaddr_ipv4_netmask_int32);
 
 
int
  Rippaddr_ipv4_hostmask_int32(
    int prfxlen, IPv4 *res
){
   
  if(prfxlen < 0 || prfxlen > 32) return 0;
   
 *res = hostmask( prfxlen );
   
  return 1;
}
 
RIP_IP_INPUT( v4, int32, _mask_, hostmask, Rippaddr_ipv4_hostmask_int32);
 
 
int
  Rippaddr_ipv6_netmask_int32(
    int prfxlen, uint64_t *res
){
   
  if (prfxlen < 0 || prfxlen > 128) return 0;
   
  res[0] = ipv6_netmask_hi(prfxlen);
  res[1] = ipv6_netmask_lo(prfxlen);
   
  return 1;
}
 
RIP_IP_INPUT(v6, int32, _mask_, netmask, Rippaddr_ipv6_netmask_int32);

int
  Rippaddr_ipv6_hostmask_int32(
    int prfxlen, uint64_t *res
){
   
  if (prfxlen < 0 || prfxlen > 128) return 0;
   
  res[0] = ipv6_hostmask_hi(prfxlen);
  res[1] = ipv6_hostmask_lo(prfxlen);
   
  return 1;
}
 
RIP_IP_INPUT(v6, int32, _mask_, hostmask, Rippaddr_ipv6_hostmask_int32);
 

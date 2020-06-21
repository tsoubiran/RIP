
 
#include "Rip-0-5-1.h"

#define INT32_SWAP2(___x__, ___y__) \
  int tmp; \
  tmp      = ___x__; \
  ___x__   = ___y__; \
  ___y__   = tmp; \

void Rippaddr_ipv4_selection_sort(int *idx, IPv4 *ip, int n){
  for (int i=0; i<n; ++i){
    IPv4 v = ip[idx[i]];
    int  m = i;  
    int  j = n;
    while ( --j > i ){  
      if ( ip[idx[j]] <v ){
        m = j;
        v = ip[idx[m]];
      }
    }
    INT32_SWAP2(idx[i], idx[m]);
  }
}
void Rippaddr_ipv4_selection_sort0(int *idx, IPv4 *ip, int idx0, int n){
  for (int i=0; i<n; ++i){
    IPv4 v = ip[idx[i+idx0]];
    int  m = i;  
    int  j = n;
    while ( --j > i ){  
      if ( ip[idx[j+idx0]] <v ){
        m = j;
        v = ip[idx[m+idx0]];
      }
    }
    INT32_SWAP2(idx[idx0+i], idx[idx0+m]);
  }
}

 
#define Rippaddr_ipv6_selection_sort_1 Rippaddr_ipv6_selection_sort
 
void Rippaddr_ipv6_selection_sort(int *idx, uint64_t *ip, int idx0, int n, int nip){
   
  for (int i=0; i<n; ++i){
     
    uint64_t v[2];
    v[0] = ip[ idx[ idx0 + i ] ];
    v[1] = ip[ nip + idx[ idx0 + i ] ];

    int  m = i;  
    int  j = n;
    while ( --j > i ){  
      uint64_t vnext[2];
      vnext[0] = ip[ idx[ idx0 + j ] ];
      vnext[1] = ip[ nip + idx[  idx0 + j ] ];

      if ( 
         Ripaddr_ipv6_cmp_lt(vnext, v)  
      ){
        m = j;
         
        v[0] = vnext[0];  
        v[1] = vnext[1];  
      }
    }
    INT32_SWAP2(idx[ idx0 + i ], idx[ idx0 + m ]);
  }
}

 
#define Rippaddr_ipv6_selection_sort_decr_1 Rippaddr_ipv6_selection_sort_decr
 
void Rippaddr_ipv6_selection_sort_decr(int *idx, uint64_t *ip, int idx0, int n, int nip){
   
  for (int i=0; i<n; ++i){
     
    uint64_t v[2];
    v[0] = ip[ idx[ idx0 + i ] ];
    v[1] = ip[ nip + idx[ idx0 + i ] ];

    int  m = i;  
    int  j = n;
    while ( --j > i ){  
      uint64_t vnext[2];
      vnext[0] = ip[ idx[ idx0 + j ] ];
      vnext[1] = ip[ nip + idx[  idx0 + j ] ];

      if ( 
         Ripaddr_ipv6_cmp_gt(vnext, v)  
      ){
        m = j;
         
        v[0] = vnext[0];  
        v[1] = vnext[1];  
      }
    }
    INT32_SWAP2(idx[ idx0 + i ], idx[ idx0 + m ]);
  }
}

 
SEXP Rip_ipv6_selection_sort_0(SEXP Rip ){ 
   
  ; 
  SEXP      Rip_DataSlot    =  GET_SLOT(Rip, Rip_dataSlotSym ); 
  int       Rip_nip         =  LENGTH( Rip_DataSlot ); 
  int      *Rip_ip_idxptr   =  INTEGER( Rip_DataSlot ); 
  SEXP      Ripv6           =  GET_SLOT(Rip, Rip_ipv6Sym );
  uint64_t *ipv6_ptr        = (uint64_t *) REAL( Ripv6 );
   
  RIP_BEGIN 
   
  Rippaddr_ipv6_selection_sort(Rip_ip_idxptr, ipv6_ptr, 0, Rip_nip, Rip_nip);
   
  RIP_END 
   
  return Rip_DataSlot; 
}

void  EXCH( int *___a__, int ___i__, int ___j__){
  
  int ___tmp; 
  ___tmp              = ___a__[___i__]; 
  ___a__[___i__ ]  = ___a__[___j__]; 
  ___a__[___j__ ]  = ___tmp; 
}

void Rippaddr_ipv4_qsort0(int *a, IPv4 *ip, int nip, int lo, int hi ) { 
   
  if (hi <= lo) return;

  int n;
  if ( ( n = hi-lo +1)<8 ){  
 
    Rippaddr_ipv4_selection_sort0(a, ip, lo, n);
    return;
  }
   
  EXCH(a, lo, (lo + hi) / 2);   
  int last = lo;
   
  IPv4 v;
  v = ip[ a[ lo ] ];
   
   
  for (int i = lo + 1; i <= hi; i++){
     
    IPv4 vnext;
    vnext = ip[ a[ i ] ];
     
     
    if (
       
      Ripaddr_ipv4_cmp_lt( vnext, v)
    ){
      EXCH(a, ++last, i);
    }
  }
  EXCH(a, lo, last);
  Rippaddr_ipv4_qsort0(a, ip, nip, lo    , last-1);
  Rippaddr_ipv4_qsort0(a, ip, nip, last+1, hi);
}

 
void Rippaddr_ipv4_qsort0_dec0(int *a, IPv4 *ip, int nip, int lo, int hi ) { 
   
  if (hi <= lo) return;

  EXCH(a, lo, (lo + hi) / 2);   
  int last = lo;
   
  IPv4 v;
  v = ip[ a[ lo ] ];
 
   
  for (int i = lo + 1; i <= hi; i++){
 
     
    IPv4 vnext;
    vnext = ip[ a[ i ] ];
 
     
    if (
      Ripaddr_ipv4_cmp_gt( vnext, v)
    ){
      EXCH(a, ++last, i);
    }
  }
  EXCH(a, lo, last);
 
  Rippaddr_ipv4_qsort0_dec0(a, ip, nip, lo    , last-1);
  Rippaddr_ipv4_qsort0_dec0(a, ip, nip, last+1, hi);
}
 
SEXP Rip_ipv4_qsort0(SEXP Rip, SEXP Rdecr ){ 
   
  int nprotected=0; 
  SEXP Rip_DataSlot;
   
   
  PROTECT(Rip_DataSlot      =  duplicate( GET_SLOT(Rip, Rip_dataSlotSym ) ) ); 
  nprotected++;
  int       Rip_nip         =  LENGTH( Rip_DataSlot ); 
  int      *Rip_ip_idxptr   =  INTEGER( Rip_DataSlot ); 
  SEXP      Ripv4           =  GET_SLOT(Rip, Rip_ipv4Sym );
  IPv4     *ipv4_ptr        = (IPv4 *) INTEGER( Ripv4 );

  RIP_BEGIN 
   
  if( *INTEGER(Rdecr) ) 
    Rippaddr_ipv4_qsort0_dec0(Rip_ip_idxptr, ipv4_ptr, Rip_nip, 0, Rip_nip-1);
  else 
    Rippaddr_ipv4_qsort0(Rip_ip_idxptr, ipv4_ptr, Rip_nip, 0, Rip_nip-1);
   
  RIP_END 
   
  UNPROTECT( nprotected ); 
   
  return Rip_DataSlot; 
}

 
void Rippaddr_ipv6_qsort0(int *a, uint64_t *ip, int nip, int lo, int hi ) { 
   
  if (hi <= lo) return;

  int n;
  if ( ( n = hi-lo +1)<8 ){  
 
    Rippaddr_ipv6_selection_sort_1(a, ip, lo, n, nip);
    return;
  }
   
  EXCH(a, lo, (lo + hi) / 2);   
  int last = lo;
   
  uint64_t v[2];
  v[0] = ip[ a[ lo ] ];
  v[1] = ip[ nip + a[ lo ] ];
   
  for (int i = lo + 1; i <= hi; i++){
     
    uint64_t vnext[2];
    vnext[0] = ip[ a[ i ] ];
    vnext[1] = ip[ nip + a[ i ] ];
     
    if (
       
      Ripaddr_ipv6_cmp_lt( (uint64_t *) &vnext, (uint64_t *) &v)
    ){
      EXCH(a, ++last, i);
    }
  }
  EXCH(a, lo, last);
  Rippaddr_ipv6_qsort0(a, ip, nip, lo    , last-1);
  Rippaddr_ipv6_qsort0(a, ip, nip, last+1, hi);
}

void Rippaddr_ipv6_qsort0_dec0(int *a, uint64_t *ip, int nip, int lo, int hi ) { 
   
  if (hi <= lo) return;
   
   
  EXCH(a, lo, (lo + hi) / 2);   
  int last = lo;
   
  uint64_t v[2];
  v[0] = ip[ a[ lo ] ];
  v[1] = ip[ nip + a[ lo ] ];
   
  for (int i = lo + 1; i <= hi; i++){
     
    uint64_t vnext[2];
    vnext[0] = ip[ a[ i ] ];
    vnext[1] = ip[ nip + a[ i ] ];
     
    if (
      Ripaddr_ipv6_cmp_gt( (uint64_t *) &vnext, (uint64_t *) &v)
    ){
      EXCH(a, ++last, i);
    }
  }
  EXCH(a, lo, last);
  Rippaddr_ipv6_qsort0_dec0(a, ip, nip, lo    , last-1);
  Rippaddr_ipv6_qsort0_dec0(a, ip, nip, last+1, hi);
}

 
SEXP Rip_ipv6_qsort0(SEXP Rip, SEXP Rdecr ){ 
   
  int nprotected=0; 
  SEXP Rip_DataSlot;
   
  PROTECT(Rip_DataSlot      =  REFCNT(Rip)==0 ? GET_SLOT(Rip, Rip_dataSlotSym ): duplicate( GET_SLOT(Rip, Rip_dataSlotSym ) ) ); 
  nprotected++;
  int       Rip_nip         =  LENGTH( Rip_DataSlot ); 
  int      *Rip_ip_idxptr   =  INTEGER( Rip_DataSlot ); 
  SEXP      Ripv6           =  GET_SLOT(Rip, Rip_ipv6Sym );
  uint64_t *ipv6_ptr        = (uint64_t *) REAL( Ripv6 );

  RIP_BEGIN 
   
  if( *INTEGER(Rdecr) ) 
    Rippaddr_ipv6_qsort0_dec0(Rip_ip_idxptr, ipv6_ptr, Rip_nip, 0, Rip_nip-1);
  else 
    Rippaddr_ipv6_qsort0(Rip_ip_idxptr, ipv6_ptr, Rip_nip, 0, Rip_nip-1);
   
  RIP_END 
   
  UNPROTECT( nprotected ); 
   
  return Rip_DataSlot; 
}
 
#if 1
 
void Rippaddr_ipv6addr_qsort0(int *a, uint64_t *ip, int nip, int lo, int hi ) { 
   
  if (hi <= lo) return;

  EXCH(a, lo, (lo + hi) / 2);   
  int last = lo;
 
   
  uint64_t *v = ip + a[ lo ]*2;

  for (int i = lo + 1; i <= hi; i++){
 
     
    uint64_t *vnext = ip + a[ i ]*2;

    if (
       
      Ripaddr_ipv6_cmp_lt( vnext, v)
    ){
      EXCH(a, ++last, i);
    }
  }
  EXCH(a, lo, last);
  Rippaddr_ipv6addr_qsort0(a, ip, nip, lo    , last-1);
  Rippaddr_ipv6addr_qsort0(a, ip, nip, last+1, hi);
}
 
SEXP Rip_ipv6addr_qsort_0(SEXP Rip ){ 
   
  int  nprotected=0;
 
  ; 
  SEXP      Rip_DataSlot    =  PROTECT(GET_SLOT(Rip, Rip_dataSlotSym )); nprotected++;
  int       Rip_nip         =  LENGTH( Rip_DataSlot ); 
  int      *Rip_ip_idxptr   =  INTEGER( Rip_DataSlot ); 
  SEXP      Ripv6           =  GET_SLOT(Rip, Rip_ipv6Sym );
  uint64_t *ipv6_ptr        = (uint64_t *) REAL( Ripv6 );
   
   
  Rippaddr_ipv6addr_qsort0(Rip_ip_idxptr, ipv6_ptr, Rip_nip, 0, Rip_nip-1);
   
  UNPROTECT( nprotected ); 
  return Rip_DataSlot; 
}
#endif

 
uint64_t* Rippaddr_ipv6_ptr(int *idx, uint64_t *ip, int nip, int i, uint64_t *ptr){
  ptr[0] = (ip[ idx[ i ] ]);  
   
  ptr[1] = (ip[ nip + idx[  i ] ]);  
  return ptr;
}
 
#define NI 16
static const int incs[NI + 1] = {
    1073790977, 268460033, 67121153, 16783361, 4197377, 1050113,
    262913, 65921, 16577, 4193, 1073, 281, 77, 23, 8, 1, 0
};
 
void Rippaddr_ipv6_shellsort_0(int *idx, uint64_t *ip, int nip){  
  int i, j, h, t;
  for (t = 0; incs[t] > nip; t++);
 
  for (h = incs[t]; t < NI; h = incs[++t]){
 
    for (i = h; i < nip; i++) {  
	     
      uint64_t v[2], vnext[2];
      int vidx = idx[i];
      v[0] = ip[ idx[ i ] ];  
      v[1] = ip[ nip + idx[  i ] ];  

      j = i;

      while (
        j >= h 
        && Ripaddr_ipv6_cmp_gt( Rippaddr_ipv6_ptr(idx, ip, nip, j-h, (uint64_t *) &vnext), (uint64_t *) &v) 
      ){ 
        idx[j] = idx[j - h]; 
        j     -= h; 

      } 
	    idx[j] = vidx; 
    }
  }
  
}  

 
SEXP Rip_ipv6_shellsort_0( SEXP Rip ){ 
   
  int nprotected=0; 
  SEXP      Rip_DataSlot    =  PROTECT(GET_SLOT(Rip, Rip_dataSlotSym ));  nprotected++;
  int       Rip_nip         =  LENGTH( Rip_DataSlot ); 
  int      *Rip_ip_idxptr   =  INTEGER( Rip_DataSlot ); 
  SEXP      Ripv6           =  GET_SLOT(Rip, Rip_ipv6Sym );
  uint64_t *ipv6_ptr        =  (uint64_t *) REAL( Ripv6 );
   
  RIP_BEGIN 
   
  Rippaddr_ipv6_shellsort_0(Rip_ip_idxptr, ipv6_ptr, Rip_nip);
   
  RIP_END 
  UNPROTECT( nprotected ); 
   
  return Rip_DataSlot; 
}
 
 

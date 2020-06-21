
 
#include <netdb.h>
#include <netinet/in.h>
#include <arpa/inet.h>
 
#include "Rip-0-5-1.h"

#define __BSWAP_64(x)                        \
  ((((x) & 0xff00000000000000ull) >> 56)        \
   | (((x) & 0x00ff000000000000ull) >> 40)        \
   | (((x) & 0x0000ff0000000000ull) >> 24)        \
   | (((x) & 0x000000ff00000000ull) >> 8)        \
   | (((x) & 0x00000000ff000000ull) << 8)        \
   | (((x) & 0x0000000000ff0000ull) << 24)        \
   | (((x) & 0x000000000000ff00ull) << 40)        \
   | (((x) & 0x00000000000000ffull) << 56))
 

#if 1

SEXP Rip_ipv4_gethostbyaddr_0(SEXP Rip ){ 
 
#if defined (__unix__) || (defined (__APPLE__) && defined (__MACH__))
  int i, nip=0, nprotected=0; 
    
  
  RIPv4_SLOTS_GET( Rip ) 
  nip = Rip_nip; 
  
    
  RIP_string_ALLOC(Res, nip);  
  nprotected++;
   
   
  RIP_BEGIN 
  for (i=0 ; i <  nip; i++){ 
  
 
    if( 
      Rip_ip_idxptr[i]!=NA_INTEGER 
    ){ 
       
      RIP_CHECK_IDX(Rip_ip_idxptr , i, nip) 
       
      struct hostent *hent;
       
      RIPv4_ELT_PTR_DCL(Rip, i) 

      IPv4 ip4n=htonl(Rip_ip_elt_ptr);

      hent = gethostbyaddr(&ip4n, sizeof ip4n, AF_INET);
       
      if( hent!=NULL){
 
        SET_STRING_ELT( Res, i, mkChar(hent->h_name));
      }
      else{
        RIP_string_NA_SET(Res, i); 
      }
       
    }else{ 
      RIP_string_NA_SET(Res, i); 
    } 
  } 
   
  RIP_END 
  UNPROTECT( nprotected ); 
  return Res; 
#else
  error("unavailable " __func__ " function");
  return ScalarLogical(0);
#endif
} 

SEXP Rip_ipv6_gethostbyaddr_0(SEXP Rip ){ 
 
#if defined (__unix__) || (defined (__APPLE__) && defined (__MACH__))
  int i, nip=0, nprotected=0; 
  RIPv6_SLOTS_GET( Rip ) 
  nip = Rip_nip; 
  
  RIP_string_ALLOC(Res, nip);  
  nprotected+=1;
   
  RIP_BEGIN 
  for (i=0 ; i <  nip; i++){ 
     
    if( 
      Rip_ip_idxptr[i]!=NA_INTEGER 
    ){ 
       
      RIP_CHECK_IDX(Rip_ip_idxptr , i, nip) 
       
      struct hostent *hent=NULL;
       
      RIPv6_ELT_PTR_DCL(Rip, i) 

      uint64_t ip6n[2]={
        __BSWAP_64(Rip_ip_elt_ptr[0])
        , __BSWAP_64(Rip_ip_elt_ptr[1])
      };

      hent = gethostbyaddr(
         
        ip6n  , sizeof ip6n 
         
        , AF_INET6
      );
       
      if( hent!=NULL){
 
         
        SET_STRING_ELT( Res, i, mkChar(hent->h_name));
      }
      else{
        RIP_string_NA_SET(Res, i); 
      }
    }else{ 
      RIP_string_NA_SET(Res, i); 
    } 
  } 
   
  RIP_END 
  UNPROTECT( nprotected ); 
  return Res; 
#else
  error("unavailable " __func__ " function");
  return ScalarLogical(0);
#endif
} 

SEXP Rip_getaddrinfo_0(SEXP Rhostnames ){ 
 
#if defined (__unix__) || (defined (__APPLE__) && defined (__MACH__))
   
  int i,j, nhosts 
    , nprotected=0
  ; 
   
  SEXP Rhost
    , Ripv4_hptr, Ripv4, Ripv4_ip
    , Ripv6_hptr, Ripv6, Ripv6_ip
  ;
   
  PROTECT_INDEX Ripv4_ip_protidx, Ripv6_ip_protidx;
  IPv4     *ipv4_ip_ptr;
  uint64_t *ipv6_ip_ptr;
  int nipv4, *ipv4_hptr, ipv4_nacc=0
    , nipv6, *ipv6_hptr, ipv6_nacc=0
  ;
   
  nhosts = LENGTH( Rhostnames );
   
  nipv6 = nipv4 = (int) ceil( (double) nhosts * 1.4);
   
  PROTECT_WITH_INDEX( Ripv4_ip = allocVector(INTSXP, nipv4 ), &Ripv4_ip_protidx); 
  nprotected++;
  ipv4_ip_ptr = (IPv4 *) INTEGER( Ripv4_ip ); 
   
  PROTECT( Ripv4_hptr = allocVector(INTSXP, nhosts+1 ) ); 
  nprotected++;
  ipv4_hptr = INTEGER(Ripv4_hptr);
  ipv4_hptr[0]=0;
   
   
  PROTECT_WITH_INDEX( Ripv6_ip = allocMatrix(REALSXP, nipv6, 2 ), &Ripv6_ip_protidx); 
  nprotected++;
  ipv6_ip_ptr = (uint64_t *) REAL( Ripv6_ip ); 
   
  PROTECT( Ripv6_hptr = allocVector(INTSXP, nhosts+1 ) ); 
  nprotected++;
  ipv6_hptr = INTEGER(Ripv6_hptr);
  ipv6_hptr[0]=0;
   
   
  for ( i=0 ; i<nhosts ; i++ ){
     
    int rc;
    const char     *hostname;
     
     
    ipv4_hptr[i+1] = ipv4_hptr[i];
    ipv6_hptr[i+1] = ipv6_hptr[i];
     
     
    if( ( STRING_ELT( Rhostnames, i) )!=NA_STRING ){
       
      hostname = translateChar( STRING_ELT( Rhostnames, i) );  

      struct addrinfo hints, *res, *resptr;
       
      memset(&hints, 0, sizeof hints);
       
      hints.ai_family   = AF_UNSPEC;  
      hints.ai_socktype = SOCK_STREAM;  
       
      if (
        ( rc = getaddrinfo(hostname, NULL, &hints, &res) ) != 0
      ) {

        continue;
      }
       
      for( resptr = res, j = 0; resptr != NULL; resptr = resptr->ai_next, j++ ) {
         
         
        void *addr;
         
         
        if ( resptr->ai_family == AF_INET) {  
           
          struct sockaddr_in *ipv4 = (struct sockaddr_in *) resptr->ai_addr;
           
          addr = &(ipv4->sin_addr);
           
          IPv4 ripv4 = ntohl( ipv4->sin_addr.s_addr );
#if 0
char ipstringbuff[IPv4_STRING_SZMAX]; 
 
inet_ntop(AF_INET, addr, ipstringbuff, INET_ADDRSTRLEN);
Rprintf("IPv4: %s %d\n", ipstringbuff, ipv4_nacc);
Rprintf("      %u\n", ipv4->sin_addr);
 
ipv4_raw_output(ripv4, (char*) &ipstringbuff,IP4_STRING_SZMAX);   
Rprintf("      %s\n", ipstringbuff);
 
uint32_t nth = ntohl(ipv4->sin_addr.s_addr);
ipv4_raw_output(nth, (char*) &ipstringbuff,IP4_STRING_SZMAX);   
Rprintf("      %s\n", ipstringbuff);
#endif

          ipv4_ip_ptr[ipv4_nacc] = ripv4;
           
           
          ++ipv4_nacc;
           
          if( nipv4==ipv4_nacc){
 
            nipv4 = (int) ceil( ( (double) nipv4 )*1.3);
            REPROTECT( Ripv4_ip = lengthgets(Ripv4_ip, ipv4_nacc), Ripv4_ip_protidx);
          }
          
        } else {  
           
          struct sockaddr_in6 *ipv6 = (struct sockaddr_in6 *) resptr->ai_addr;
           
          addr = &(ipv6->sin6_addr);

#if 0      
char ip6stringbuff[IPv6_STRING_SZMAX];
inet_ntop(AF_INET6, addr, ip6stringbuff, INET6_ADDRSTRLEN);
Rprintf("IPv6: %s\n", ip6stringbuff);  
Rprintf("      %" PRIu64 " %" PRIu64 "\n"
  , ( (IPv6*) (&ipv6->sin6_addr) )->ipv6[0], ( (IPv6*) (&ipv6->sin6_addr) )->ipv6[1]
);
#endif
   
  #ifdef RNET_DBG	        
    ipvers = "IPv6";
  #endif
           
           
          uint64_t ip6h[2]={
            __BSWAP_64( ( (IPv6*) ( addr ) )->ipv6[0] )
            , __BSWAP_64( ( (IPv6*) ( addr ) )->ipv6[1] )
          };

          ipv6_ip_ptr[ipv6_nacc]       = ip6h[0];
          ipv6_ip_ptr[ipv6_nacc+nipv6] = ip6h[1];
           
          ++ipv6_nacc;
           
          if( nipv6==ipv6_nacc){
            nipv6 = (int) ( (double) nipv6 )*1.3;
            REPROTECT( Ripv6_ip = arraycp(Ripv6_ip, nipv6, 2, ipv6_nacc), Ripv6_ip_protidx);
          }
        }  
         
         
      }  
       
      freeaddrinfo(res);
       
      ipv4_hptr[i+1] = ipv4_nacc;
       
      ipv6_hptr[i+1] = ipv6_nacc;
     
    }  
  }  
   
   
  Rhost = PROTECT(
    NEW_OBJECT(
      PROTECT(
        MAKE_CLASS("host")
      )
    )
  );
  nprotected+=2;
   
  Ripv4 = PROTECT(
    NEW_OBJECT(
      PROTECT(
        MAKE_CLASS("IPv4")
      )
    )
  );
  nprotected+=2;
   
  Ripv6 = PROTECT(
    NEW_OBJECT(
      PROTECT(
        MAKE_CLASS("IPv6")
      )
    )
  );
  nprotected+=2;
   
  Rhost = SET_SLOT(Rhost, host_hostnameSym, duplicate( Rhostnames ) );

  if( ipv4_nacc<nipv4 ){
 
    REPROTECT( Ripv4_ip = lengthgets(Ripv4_ip, ipv4_nacc), Ripv4_ip_protidx);
  }
   
  Ripv4 = SET_SLOT(Ripv4, Rip_ipv4Sym, Ripv4_ip ); 
   
  SET_SLOT(Ripv4, Rip_lenSym, ScalarInteger( ipv4_nacc ) );
   
  Rhost = SET_SLOT(Rhost, Rip_ipv4Sym, Ripv4 );
   
  Rhost = SET_SLOT(Rhost, host_ipv4ptrSym, Ripv4_hptr );
  
   
  if( ipv6_nacc<nipv6 ){
 
    REPROTECT( Ripv6_ip = arraycp(Ripv6_ip, nipv6, 2, ipv6_nacc), Ripv6_ip_protidx);
  }
   
  Ripv6 = SET_SLOT(Ripv6, Rip_ipv6Sym, Ripv6_ip ); 
   
  SET_SLOT(Ripv6, Rip_lenSym, ScalarInteger( ipv6_nacc ) );
   
  Rhost = SET_SLOT(Rhost, Rip_ipv6Sym, Ripv6 );
   
  Rhost = SET_SLOT(Rhost, host_ipv6ptrSym, Ripv6_hptr );
   
  UNPROTECT( nprotected ); 
   
  return Rhost;
#else
  error("unavailable " __func__ " function");
  return ScalarLogical(0);
#endif
}

#include <ifaddrs.h>
 
SEXP Rip_ifaddrs_0(){ 
 
#if defined (__unix__) 
     
    struct ifaddrs * ifAddrStruct=NULL;
    struct ifaddrs * ifa=NULL;
     
    SEXP Rip=NULL,Ripv4=NULL, Ripv4_idSlot=NULL
      , Ripv6=NULL, Ripv6_idSlot=NULL;
    RIPv4_SLOTS_DCL(Ripv4)
    RIPv6_SLOTS_DCL(Ripv6)
     
    int nip=0, nprotected=0,rc=0;
    Ripv4_nipv4 = Ripv6_nipv6 = 0;
     
    RIP_BEGIN 
     
    if ( (rc = getifaddrs(&ifAddrStruct) )== -1) {
        
       error("getifaddrs %s",  strerror(rc) );
    }
     
    for (ifa = ifAddrStruct; ifa != NULL; ifa = ifa->ifa_next) {
         
        if (!ifa->ifa_addr) {
            continue;
        }
        if (ifa->ifa_addr->sa_family == AF_INET) {  
           
          Ripv4_nipv4++;
           

        } else if (ifa->ifa_addr->sa_family == AF_INET6) {  
           
          Ripv6_nipv6++;
           
        } 
    }
     
    nip = Ripv4_nipv4 + Ripv6_nipv6;
 
     
    Ripv4 = PROTECT(
      NEW_OBJECT(
        PROTECT(
          MAKE_CLASS("IPv4")
        )
      )
    );
    nprotected+=2;
     
    ___RIPv4_SLOTS_ALLOC(Ripv4, nip, Ripv4_nipv4)
     
    PROTECT( Ripv4_idSlot = allocVector(STRSXP, Ripv4_nip ) );
    nprotected++;    
     
    Ripv6 = PROTECT(
      NEW_OBJECT(
        PROTECT(
          MAKE_CLASS("IPv6")
        )
      )
    );
    nprotected+=2;
     
    ___RIPv6_SLOTS_ALLOC(Ripv6, nip, Ripv6_nipv6)
     
    PROTECT( Ripv6_idSlot = allocVector(STRSXP, Ripv6_nip ) );
    nprotected++;
     
    int i=-1;
     
    for (ifa = ifAddrStruct; ifa != NULL; ifa = ifa->ifa_next) {
         
        void *addrptr;     

        if (!ifa->ifa_addr) {
            continue;
        }
        if (ifa->ifa_addr->sa_family == AF_INET) {  
 
           
          i++;
           
          addrptr=&((struct sockaddr_in *)ifa->ifa_addr)->sin_addr;

          RIPv4_ITER_SET(Ripv4, i, ntohl( *(IPv4*) addrptr))
           
          SET_STRING_ELT( Ripv4_idSlot, i, mkChar(ifa->ifa_name));
           
          Ripv6_ip_idxptr[i] = NA_INTEGER;

        } else if (ifa->ifa_addr->sa_family == AF_INET6) {  
 
           
          i++;
           
          addrptr=&((struct sockaddr_in6 *)ifa->ifa_addr)->sin6_addr;
           
          uint64_t ip6h[2]={
            __BSWAP_64( ( (IPv6*) ( addrptr ) )->ipv6[0] )
            , __BSWAP_64( ( (IPv6*) ( addrptr ) )->ipv6[1] )
          };

          RIPv6_ITER_SET(Ripv6, i, ip6h)
           
          SET_STRING_ELT( Ripv6_idSlot, i, mkChar(ifa->ifa_name));
           
          Ripv4_ip_idxptr[i] = NA_INTEGER;

        } 
    }
   
  RIPv4_SLOTS_SET(Ripv4)
  Ripv4 = SET_SLOT(Ripv4, Rip_idSym, Ripv4_idSlot );
   
  RIPv6_SLOTS_SET(Ripv6)
  Ripv6 = SET_SLOT(Ripv6, Rip_idSym, Ripv6_idSlot );
   
  Rip = PROTECT(
    NEW_OBJECT(
      PROTECT(
        MAKE_CLASS("IP")
      )
    )
  );
  nprotected+=2;
  Rip = SET_SLOT(Rip, Rip_ipv4Sym, Ripv4 );
  Rip = SET_SLOT(Rip, Rip_ipv6Sym, Ripv6 );
   
  if (ifAddrStruct!=NULL) freeifaddrs(ifAddrStruct);
   
  RIP_END 
   
  UNPROTECT( nprotected ); 
   
  return Rip;
#else
  error("unavailable " __func__ " function");
  return ScalarLogical(0);
#endif
}

#endif

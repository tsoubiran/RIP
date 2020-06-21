 

 
#include "Rip-0-5-1.h"

___RIP_inline
int 
  Ripaddr_ipv4_cmp_eq(
    IPv4 ip1, IPv4 ip2
){
   
  return ip1 == ip2;
}
 
 
RIP_OP2_BOOL(v4, eq, Ripaddr_ipv4_cmp_eq);
 
 
RIP_OP2_BOOL_1(v4cache, v4cache, eq, Ripaddr_ipv4_cmp_eq);
 
RIP_OP2_BOOL_1(v4cache, v4, eq, Ripaddr_ipv4_cmp_eq);

___RIP_inline
int 
  Ripaddr_ipv4r_cmp_eq(
    IPv4 *ip1, IPv4 *ip2
){
   
   
  return ( ip1[0] == ip2[0] ) & ( ip1[1] == ip2[1] );
}
 
RIP_OP2_BOOL(v4r, eq, Ripaddr_ipv4r_cmp_eq);
 
 
___RIP_inline
int 
  Ripaddr_ipv6_cmp_eq(
  uint64_t *ip1, uint64_t *ip2
){
   
  return (ip1[0] == ip2[0]) && (ip1[1] == ip2[1]);
}
 
RIP_OP2_BOOL(v6, eq, Ripaddr_ipv6_cmp_eq);
 
 
___RIP_inline
int 
  Ripaddr_ipv6r_cmp_eq(
    IPv6r *ip1, IPv6r *ip2
){
   
  return Ripaddr_ipv6_cmp_eq(
      (uint64_t *) &ip1->lo, (uint64_t *) &ip2->lo
    ) && Ripaddr_ipv6_cmp_eq(
      (uint64_t *) &ip1->hi, (uint64_t *) &ip2->hi
  );
} 
 
RIP_OP2_BOOL(v6r, eq, Ripaddr_ipv6r_cmp_eq);

___RIP_inline
int 
  Ripaddr_ipv4_cmp_neq(
    IPv4 ip1, IPv4 ip2
){
   
  return ip1 != ip2;
}
 
RIP_OP2_BOOL(v4, neq, Ripaddr_ipv4_cmp_neq);
 
 
___RIP_inline
int 
  Ripaddr_ipv4r_cmp_neq(
    IPv4 *ip1, IPv4 *ip2
){
   
   
  return ( ip1[0] != ip2[0] ) & ( ip1[1] != ip2[1] );
}
 
RIP_OP2_BOOL(v4r, neq, Ripaddr_ipv4r_cmp_neq);
 
 
___RIP_inline
int 
  Ripaddr_ipv6_cmp_neq(
  uint64_t *ip1, uint64_t *ip2
){
   
  return (ip1[0] != ip2[0]) && (ip1[1] != ip2[1]);
}
 
RIP_OP2_BOOL(v6, neq, Ripaddr_ipv6_cmp_neq);
 
 
___RIP_inline
int 
  Ripaddr_ipv6r_cmp_neq(
    IPv6r *ip1, IPv6r *ip2
){
   
  return Ripaddr_ipv6_cmp_neq(
      (uint64_t *) &ip1->lo, (uint64_t *) &ip2->lo
    ) && Ripaddr_ipv6_cmp_neq(
      (uint64_t *) &ip1->hi, (uint64_t *) &ip2->hi
  );
} 
 
RIP_OP2_BOOL(v6r, neq, Ripaddr_ipv6r_cmp_neq);

___RIP_inline
int 
  Ripaddr_ipv4_cmp_lt(
    IPv4 ip1, IPv4 ip2
){
 
   
  return ip1 < ip2;
}
 
RIP_OP2_BOOL(v4, lt, Ripaddr_ipv4_cmp_lt);
 
 
___RIP_inline
int 
  Ripaddr_ipv6_cmp_lt(
    uint64_t *ip1, uint64_t *ip2
){
 
   
  return ( 
    ( ip1[0] < ip2[0] )
    || ( ( ip1[0] == ip2[0] ) && ( ip1[1] < ip2[1] ) ) 
  );

}
 
RIP_OP2_BOOL(v6, lt, Ripaddr_ipv6_cmp_lt);

___RIP_inline
int 
  Ripaddr_ipv4_cmp_le(
    IPv4 ip1, IPv4 ip2
){
   
  return ip1 <= ip2;
}
 
RIP_OP2_BOOL(v4, le, Ripaddr_ipv4_cmp_le);
 
___RIP_inline
int 
  Ripaddr_ipv6_cmp_le(
  uint64_t *ip1, uint64_t *ip2
){

  return !Ripaddr_ipv6_cmp_gt(ip1,ip2);
}
 
RIP_OP2_BOOL(v6, le, Ripaddr_ipv6_cmp_le);

___RIP_inline
int 
  Ripaddr_ipv4_cmp_ge(
    IPv4 ip1, IPv4 ip2
){
   
  return ip1 >= ip2;
}
 
RIP_OP2_BOOL(v4, ge, Ripaddr_ipv4_cmp_ge);
 
 
___RIP_inline
int 
  Ripaddr_ipv6_cmp_ge(
  uint64_t *ip1, uint64_t *ip2
){
   
  return !Ripaddr_ipv6_cmp_lt(ip1,ip2);
}
 
RIP_OP2_BOOL(v6, ge, Ripaddr_ipv6_cmp_ge);

___RIP_inline
int 
  Ripaddr_ipv4_cmp_gt(
    IPv4 ip1, IPv4 ip2
){
   
  return ip1 > ip2;
}
 
RIP_OP2_BOOL(v4, gt, Ripaddr_ipv4_cmp_gt);
 
 
___RIP_inline
int 
  Ripaddr_ipv6_cmp_gt(
  uint64_t *ip1, uint64_t *ip2
){
#if 0
   
  return !Ripaddr_ipv6_cmp_lt(ip1,ip2) & !Ripaddr_ipv6_cmp_eq(ip1,ip2);
#else
   
  return ( 
    ( ip1[0] > ip2[0] )
    || ( ( ip1[0] == ip2[0] ) && ( ip1[1] > ip2[1] ) ) 
  );
#endif
}
 
RIP_OP2_BOOL(v6, gt, Ripaddr_ipv6_cmp_gt);

___RIP_inline
int 
  Ripaddr_ipv4r_cmp_intersects(
    IPv4 *ip1, IPv4 *ip2
){
   
  return ( ip2[0] < ip1[0] ) & ( ip2[1] < ip1[1] );
}
 
RIP_OP2_BOOL(v4r, intersects, Ripaddr_ipv4r_cmp_intersects);

 

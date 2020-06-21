The RIP package provides classes and methods to work with IP addresses in R. Both IPv4 and IPv6 are supported as well as arbitrary IP ranges. It is based on the [ip4r](https://github.com/RhodiumToad/ip4r) PostgreSQL extension. The RIP package defines vectors-like objects that provides for

 - IP addresses and range parsing and validation
 - vectorized operations such as arithmetic, logical and bitwise operations
 - IP matching and lookup
 - (reverse) DNS lookup and whois databases query

## Quick start

The following code snippet shows how to create IPv4 and IPv4 addresses in addition to arithmetic, compare and bit manipulation methods usage as well as address lookup :

``` r
library(RIP)
##  lo part of the range
l <- ipv4("192.168.0.0")
##  hi part  of the range
h <- l + (2^16 -1)
## range
pn <-ipv4r(l, h)
## build range using CIDR notation
pn==ipv4r("192.168.0.0/16")
## build range using dash notation
pn==ipv4r("192.168.0.0-255.255")
##
pn==ipv4r( l, l + ( ipv4(1L) %<<% 16L ) - 1)
##
pn==ipv4r( l, l | ipv4.hostmask( ceiling(log2(ip.range(pn))) ) )
##
## IP lookup
##
## \in
ipv4.reserved()[ ip.index(ipv4.reserved())( l + 0:9 )]
## ==
ipv4.reserved()[ ip.match(pn, ipv4.reserved() ) ]
## \in
ipv4.reserved()[ip.index(ipv4.reserved())(
  ipv4r("192.168.0.0/24")
)]
```

What follows shows how to get (confusing) informations about hosts

``` r
## DNS lookup
rhost     <- host('r-project.org')
## Reverse DNS lookup : "cran.wu-wien.ac.at"
rhost.hnm <- host.info(ipv4(rhost))
## primary domain : "ac.at"
fqdn(rhost.hnm)
## RIR : ARIN (not quite...)
ipv4.rir()[ip.match(ipv4(rhost), ipv4.rir())]
## address was not recovered
ip.match(ipv4(rhost), ipv4.recovered())
## domain name info 
rdom.whois   <- whois('r-project.org', output=1)
## "AT"
rdom.whois[['r-project.org']]['Registrant Country']
## host info
rhost.whois <- whois(ipv4(rhost),verbose = 2, output=1)
## "RIPE Network Coordination Centre (RIPE)" 
rhost.whois[['r-project.org']]['Organization']
```

The results of those queries may look a little bit confusing. The whois queries tells us that r-project.org site is hosted by the XXX in Austria (and so does the extension of the primary domain --".at") and that its address is accordingly managed by the RIPE-NCC. But RIR lookup on the address of the server tells us that its address falls into a range managed by ARIN which serves North America.  What's happening here is that some address ranges were assigned by ARIN in the 80's to European organizations such as universities before RIPE NCC began its operations in 1992. Those ranges were later transferred to the RIPE NCC as shown by

``` r
## "Early Registrations, Transferred to RIPE NCC"
rhost.whois[['r-project.org']]['NetType']
```

but this range still belongs to the ARIN address space.

## Further reading

Please look [here](https://numa.hypotheses.org/?p=2694) for more examples.

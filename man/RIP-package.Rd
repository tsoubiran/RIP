%%
%%
%%
\name{RIP-package}
%%
\docType{package}
%%
\alias{RIP-package}
%%
\alias{RIP}
%%
%%
%%
\title{Classes and methods for IP addresses}
%%
\description{
  Classes and methods for IP addresses
}
%%
\details{
  %%
  The RIP package provides vector-like classes and methods for Internet Protocol (IP) addresses. %%
  It is based on the ip4r PostgreSQL extension available at \url{https://github.com/RhodiumToad/ip4r}.
  %%
  An IP address is a numerical label assigned to each device connected to a computer network that uses the Internet Protocol for communication. %
  The Internet Protocol uses those labels to identify nodes such as host or network interface for relaying datagrams between them across network boundaries.

  Internet Protocol version 4 (IPv4) defines an IP address as an unsigned 32-bit number. However, because of the growth of the Internet and the depletion of available IPv4 addresses, a new version of IP (IPv6) using 128 bits for the IP address was developed from 1995 on. IPv6 deployment has been ongoing since the mid-2000s. Note that there is no IPv5 address. In addition, IPv4 and IPv6 protocols differ in many respects besides IP addresses representation.

  IP addresses are usually written and displayed in human-readable notations, such as "192.168.0.1" in IPv4, and "fe80::3b13:cff7:1013:d2e7" in IPv6. %%
  Ranges can be represented using two IP addresses separated by a dash or using the Classless Inter-Domain Routing (CIDR) notation. CIDR suffixes an address with he size of the routing prefix of the address which is the number of significant bits. For instance, "192.168.0.0/16" is a private network with subnet mask "255.255.0.0" and is equivalent to "192.168.0.0-192.168.255.255". 
  %%The size of the routing prefix of the address is designated in Classless Inter-Domain Routing (CIDR) notation by suffixing the address with the number of significant bits, e.g., "192.168.0.0/16", which is a private network with subnet mask "255.255.0.0". %%
  %%
  Currently, the RIP package supports the following object types implemented using S4 classes :
  %%
  \itemize{
    %%
    \item the IPv4 class stores IP version 4 addresses
    %%
    \item the IPv4r class stores IP version 4 addresses ranges
    %%
    \item the IPv6 class stores IP version 6 addresses
    %%
    \item the IPv6r class stores IP version 6 addresses ranges
    %%
    \item the IP class stores both IPv4 and IPv6 addresses
    %%
    \item the IPr class stores both IPv4r and IPv6r addresses
    %%
    \item the (still experimental) host class holds the result of DNS lookup
  }%%
  The RIP package also provides methods for arithmetic, comparison and bitwise unary and binary operations in addition to sorting and lookup and querying information about IP addresses and domain names. %% 
  All operators are not available for all classes mostly by design but a few are still missing because they have not been implemented yet. IP objects can also be subseted or stored in a \code{data.frame} and serialized. 
  %%
  The IP and IPr classes are only convenience containers for instances when addresses must be created from vectors mixing both protocols. %%
  The IPv4 and IPv6 protocols and their corresponding IP representation are indeed very different in many respects so only a subset of methods are available for them. In addition, methods for those containers tend to run slower because, at the moment, they need to make two passes (one for IPv4* and one for IPv6* objects). %%
  Use the \code{ipv4(IP)} (resp. \code{ipv4r(IPr)}) and \code{ipv6(IP)} (resp. \code{ipv6r(IP)}) getters to work with v4 and v6 objects separately.
  
  %%
  \subsection{Design considerations }{
  %%
  IP objects were designed to behave as much as possible like base R atomic vectors. %%
  % Many fuctions calls work out-the-box table(IP*), factor(IP*),\ldots
  % But there are a few caveats.
  % In some case, it is necessary to coerce to character, for instance when using the
  % \code{match()} function which is unfortunenately not generic. So 
  % merge() by one IP objects. There is no need for coercion when there are two or more keys.
  But, in order to do so, objects must inherit a base R vector class for which many, many methods are available (from base R or other packages). %%
  Hence, \bold{calling a method that is not exported from the RIP package namespace on a RIP object may not yield what you want} (but some do work -- see next paragraph). %%
  Reason is that the vector from which RIP objects inherit from the atomic vector class %%
  does not hold IP* addresses but an integer zero-based index to them. When calling a non-RIP method, R will first look for a method for this particular object. If none is found, it will try to find one for the class this object inherits from. Hence, the call will operate on the index, and not on the object as a hole. %%
  For instance, the RIP does not provide a \code{`*`} operator at the moment. In earlier versions of the package, multiplying an IP* by any number ended up multiplying the index by a number and eventually just mess the index up. %%
  Also \code{ipv4("0.0.0.0")==0L} and \code{ipv4("0.0.0.1")==0L} both returned \code{TRUE} because comparison between an IP object and an integer is not provided by the package. As a consequence, comparison defaulted to , \code{.Data} part of the object which holds the index. 
  This kind of unwanted effects of inheritance has been fixed for a number of base \R functions but the problem remains for a number of \R functions (see \code{match()}) and functions defined in other packages.%%
  In this case, further operations on the object will raise an error but there are instances where this will silently fail.
  
  Indeed, some functions calls  such as \code{length()}, \code{is.na()}, \code{table()}, \code{factor()},\code{rev()},\ldots work out-the-box as intended. %
  But there are a few caveats. %
  In some case, it is necessary to coerce to character. For instance when calling functions that the \code{match()}, match will operate on the index of the IP* object, not on the object as a whole.  
  This is why the \code{ip.match()} method is provided as a replacement. %
  To avoid some unwanted effects, the RIP package also makes \code{match()} a generic function but this won't change the behavior of function calling it in other packages or base \R.
  
  As a consequence, calling \code{merge()} on two \code{data.frame} using one single IP object as the by variable will merge the \code{data.frame} by IP* index, not by IP. %
  But, on the other end, there is no need for coercion when there are two or more IP* keys or when mixing IP* objects with other vectors types, because, in this case, keys are all converted to character and pasted before matching. % 
  
  So, when calling a non-RIP method on any IP object, \bold{either convert to character or first check the result before further processing}.
  
  The reasons for using an index are twofold. First, each IP address space use the entire 32 (resp. 128) bits integer range. Thus, no value can be used for NA. %%
  For instance, R defines \code{NA_integer_} as \eqn{2^{31}} which a perfectly valid IP v4 address ("128.0.0.0"). %%
  Second reason is IP words size. An IPv4 address uses 32 bits and thus can be stored using an integer vector (and IPv4 address ranges uses 64 bits and could be stored using a numeric vector). But an IP v6 address uses 128 bits and an IP v6 address range uses 256 bits and currently no \R built-in atomic vectors are wide enough to hold them. IP addresses other than IPv4 have to be stored in a separate matrix and an index is needed to retrieve their value.
  
  Therefore, each IP* object has an index which either points to the IP location in a table or mark the value as NA. %%
  This way \R believes it is dealing with a regular vector but at the cost of increased memory consumption. %%
  The memory footprint is a function of the number of NA.
  }
  
  %%\bold{Data protection}
  %%
  \subsection{Data protection }{
  %%
  One last caveat. In certain countries such as EU members countries, IP addresses are considered personal data (see Article 29 Working Party Opinion 4/2007 and ECJ ruling dated 19 October 2016 --ref.: C582/14). IP processing must therefore be done in accordance to the applicable laws and regulations.
  }
}
%%
\keyword{ package }
%%\seealso{ \code{\link{}}  }
% \examples{

% }

%%
%%
%%
\name{IP range classes}
%%
%%
%%
\docType{class}
%%
%%
%%
\keyword{ classes }
%%
%%
%%
\alias{IPv4r-class}
\alias{IPv6r-class}
\alias{IPr-class}
%%
\alias{ipv4r}
\alias{ipv4r-method}
% \alias{ipv4r,missing-method}
% \alias{ipv4r,character-method}
% \alias{ipv4r,list-method}
\alias{ipv4r,IPr,missing-method}
\alias{ipv4r,IPv4,IPv4-method}
\alias{ipv4r,IPv4,.__intFP__.-method}
\alias{ipv4r,character,missing-method}
\alias{ipv4r,list,missing-method}
\alias{ipv4r,missing,missing-method}
%%
\alias{ipv6r}
\alias{ipv6r-method}
% \alias{ipv6r,missing-method}
% \alias{ipv6r,character-method}
% \alias{ipv6r,list-method}
\alias{ipv6r,IPr,missing-method}
\alias{ipv6r,IPv6,IPv6-method}
\alias{ipv6r,IPv6,.__intFP__.-method}
\alias{ipv6r,character,missing-method}
\alias{ipv6r,list,missing-method}
\alias{ipv6r,missing,missing-method}
%%
\alias{ipr}
\alias{ipr-method}
\alias{ip,IPr-method}
\alias{ipr,IP,IP-method}
\alias{ipr,IP,.__intFP__.-method}
\alias{ipr,IPv4r,IPv6r-method}
\alias{ipr,character,missing-method}
\alias{ipr,list,missing-method}
\alias{ipr,missing,missing-method}

%%
%%
%%\alias{ip.version}
% \alias{ip.version-methods}
\alias{ip.version,IPr-method}
%%
%%
%%
\alias{print,IPv4r-method}
\alias{print,IPv6r-method}
\alias{print,IPr-method}
%%
\alias{show,IPv4r-method}
\alias{show,IPv6r-method}
\alias{show,IPr-method}
%%
%%
%%
\alias{[,IPv4r,ANY-method}
\alias{[,IPv4r,.__subscript__.-method}
\alias{[,IPv6r,.__subscript__.-method}
%%
\alias{[,IPr,.__subscript__.-method}
%%
\alias{[,.__IPvr__.,character-method}
\alias{[,.__IPvr__.,missing-method}
%%
\alias{[<-,IPv4r,.__subscript__.-method}
\alias{[<-,IPv4r,ANY,ANY,ANY-method}
\alias{[<-,IPv6r,.__subscript__.-method}
\alias{[<-,IPv6r,ANY,ANY,ANY-method}
\alias{[<-,IPr,.__subscript__.,ANY,IPr-method}
%%
%%
%%
\alias{lo}
%%
\alias{lo,IPv4r-method}
%%
\alias{lo,IPv6r-method}
%%
\alias{lo,IPr-method}
%%
%%
%%
\alias{hi}
%%
\alias{hi,IPv4r-method}
%%
\alias{hi,IPv6r-method}
%%
\alias{hi,IPr-method}
%%
%%
%%
\alias{ipv4,IPv4r-method}
%%
\alias{ipv6,IPv6r-method}
%%
%%
%%
\alias{ip.range}
%%
\alias{ip.range-methods}
\alias{ip.range,IPv4r-method}
\alias{ip.range,IPv6r-method}
%%
%% 
%%
% \alias{names}
%%
\alias{names-method}
%%
\alias{names.IPv4r}
\alias{names.IPv6r}
\alias{names.IPr}
%%
% \alias{names<-}
%%
\alias{names<--method}
%%
\alias{names<-.IPv4r}
\alias{names<-.IPv6r}
\alias{names<-.IPr}
%%
%% 
%%\alias{as.data.frame}
% \alias{as.data.frame-method}
%%
\alias{as.data.frame.IPv4r}
\alias{as.data.frame.IPv6r}
\alias{as.data.frame.IPr}
%%
%%
%%
% \alias{c}
%%
\alias{c.IPv4r}
\alias{c.IPv6r}
%%
%%
%%
% \alias{rbind2-method}
%%
\alias{rbind2,IPv4r,IPv4r-method}
\alias{rbind2,IPv6r,IPv6r-method}
\alias{rbind2,IPr,IPr-method}
%%
%%
%%
%%\alias{is.numeric}
% \alias{is.numeric-method}
%%
\alias{is.numeric,IPv4r-method}
\alias{is.numeric,IPv6r-method}
\alias{is.numeric,IPr-method}
%%
%%
%%
\title{IPv4, IPv6 and IP ranges classes}
%%
\description{classes for IPv4 and IPv6 ranges addresses}
% \arguments{\item{drop}{}}
%%
\details{
  %%
  IPv4 and IPv6 ranges may be created from character vector using either range or Classless Inter-Domain Routing (CIDR) notation. %%
  Range notation represents ranges by using first and last address separated by a dash (\code{"<ipr-start/>-<ipr-end/>"}). %%
  CIDR notation uses a network prefix and a network identifier separated by a slash (\code{"<net-prefix/>/<identifier/>"}). %
  The network identifier is a decimal number which counts the number of leading 1 bits in the subnet mask (see \code{hostmask()}).
  
  %% 
  the lo() and hi() methods extract the low and high ends of ip ranges. When extracting IPv4r or IPv6r parts from IPr objects, use the drop argument to remove all NA values.
}
%%
%%
%%
\examples{
  ##
  ## Range notation
  ##
  ipv4r("192.0.0.0-192.0.0.10")
  ##
  ## CIDR notation
  ##
  ## The entire IPv4 address space
  ipv4(ipv4r('0.0.0.0/0'))
  ## Is there life on Mars ? (Martian packets)
  ipv4r("100.64.0.0/10")
  ##
  ip4 <- ipv4("192.0.0.0")
  ## power of 2 
  ipv4r( print(sprintf("\%s-\%s", ip4,  ip4 + ( 2^8-1) ) ))
  ## not a power of 2
  ipv4r( print(sprintf("\%s-\%s", ip4,  ip4 + 10 ) ))
  ##
  ## Network classes
  ##
  ip.class <- data.frame(
    name = paste('class', LETTERS[1:5])
    , class = ipv4r(
    c(
          '0.0.0.0/8'                 ## Class A
        , '128.0.0.0/16'              ## Class B
        , '192.0.0.0/24'              ## Class C
        , '224.0.0.0-239.255.255.255' ## Class D
        , '240.0.0.0-255.255.255.255' ## Class E
      )
    )
  )
  ##
  ## extract IP range start and end
  ##
  (class.ip <- ipv4(ip.class$class))
  ##
  lo(ip.class$class)==class.ip$lo
  ##
  hi(ip.class$class)==class.ip$hi
  ##
  ## # of hosts on this network
  ##
  ip.range(ip.class$class)
  ## this is ok for IP v4 but may cause loss of precision for IPv6
  ip.range(ip.class$class)==as.numeric(class.ip$hi - class.ip$lo)
}

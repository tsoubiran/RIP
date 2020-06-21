%%
%%
%%
\name{Comparison}
%%
\docType{methods}
%%
%%
%%
\alias{Compare-methods}
%%
\alias{Compare,IP,IP-method}
%%
%%
%%
\alias{==}
\alias{==-method}
%%
\alias{==,IPv4,IPv4-method}
\alias{==,IPv4r,IPv4r-method}
\alias{==,IPv6,IPv6-method}
\alias{==,IPv6r,IPv6r-method}
%%
%%
%%
\alias{!=}
\alias{!=-method}
%%
\alias{!=,IPv4,IPv4-method}
\alias{!=,IPv4r,IPv4r-method}
\alias{!=,IPv6,IPv6-method}
\alias{!=,IPv6r,IPv6r-method}
%%
%%
%%
\alias{<}
\alias{<-method}
%%
\alias{<,IPv4,IPv4-method}
%%\alias{<,IPv4r,IPv4r-method}
\alias{<,IPv6,IPv6-method}
%%\alias{<,IPv6r,IPv6r-method}
%%
%%
%%
\alias{>}
\alias{>-method}
%%
\alias{>,IPv4,IPv4-method}
%%\alias{>,IPv4r,IPv4r-method}
\alias{>,IPv6,IPv6-method}
%%\alias{>,IPv6r,IPv6r-method}
%%
%%
%%
\alias{<=}
\alias{<=-method}
%%
\alias{<=,IPv4,IPv4-method}
%%\alias{<=,IPv4r,IPv4r-method}
\alias{<=,IPv6,IPv6-method}
%%\alias{<=,IPv6r,IPv6r-method}
%%
%%
%%
\alias{>=}
\alias{>=-method}
%%
\alias{>=,IPv4,IPv4-method}
%%\alias{>=,IPv4r,IPv4r-method}
\alias{>=,IPv6,IPv6-method}
%%\alias{>=,IPv6r,IPv6r-method}
%%
%% Function \code{Compare}
%%
\title{ Methods for IP Comparison }
%%
\description{
  Methods for IP binary comparison 
}
  
  % \method{==}{IPv6}(e1,e2)
  % \method{==}{IP}(e1,e2)
%%
% \usage{
  % %%e1 == e2
  % \S4method{==}{IPv4,IPv4}(e1,e2)
  %%\method{==}{IPv4}(e1,e2)
  % e1 != e2
  % e1 > e2
  % e1 < e2
  % e1 >= e2
  % e1 <= e2
% }
\section{Methods}{
  \describe{
    \item{x == y }{ }
    \item{x != y }{ }
    \item{x > y }{ }
    \item{x < y }{ }
    \item{x >= y }{ }
    \item{x <= y }{ }
  }
}
%%
\arguments{
  \item{e1, e2}{objects of either class 'IPv4', 'IPv6'or 'IP'}
  %%\item{y}{ an atomic vector of class 'IPv4', 'IPv6'or 'IP'}
}
%%
\value{
  a logical vector
}
%%
\details{
  %%
  Only the \code{==} operator is supported for ranges
}
%%
\keyword{ methods }
% \keyword{ manip }
% \seealso{ \code{\link{format.integer64}} \code{\link{integer64}}  }
\examples{
  ##
  ip1 <- ip(c("192.0.0.1", "fd00::1")) + rep(c(0:2),each=2)
  ##
  ip2 <- ip1 + rep(c(1,-1,0), each=2)
  ##
  data.frame(
    ip1, ip2
    , lt = ip1<ip2
    , le = ip1<=ip2
    , eq = ip1==ip2
    , ge = ip1>=ip2
    , gt = ip1>ip2
  )
}

%%
%%
%%
\name{Varia}
%%
\docType{methods}
%%
%%
%%
\alias{length}
\alias{length,IPv4-method}
\alias{length,IPv4r-method}
\alias{length,IPv6-method}
\alias{length,IPv6r-method}
%%
%%
%%
\alias{is.na}
\alias{is.na,IPv4-method}
\alias{is.na,IPv4r-method}
\alias{is.na,IPv6-method}
\alias{is.na,IPv6r-method}
%%
%%
%%
% \alias{is.numeric}
% \alias{is.numeric,IPv4-method}
% \alias{is.numeric,IPv6-method}
%%
%%
%%
%%
%%
\alias{unique}
%%
\alias{unique.IPv4}
\alias{unique.IPv6}
\alias{unique.IPv4r}
\alias{unique.IP}
%%
%%
%%
\alias{xtfrm}
\alias{xtfrm-methods}
%%
\alias{xtfrm,IPv4-method}
\alias{xtfrm,IPv4r-method}
%%
\alias{xtfrm,IPv6r-method}
\alias{xtfrm,IPv6-method}
%%
%%
%%
\alias{ip.order}
\alias{ip.order-method}
\alias{ip.order,IPv4-method}
\alias{ip.order,IPv6-method}
%%
%%
%%
\alias{match}
\alias{match-methods}
%%
\alias{match,IPv4,IPv4-method}
\alias{match,IPv4,IPv4r-method}
\alias{match,IPv4r,IPv4r-method}
%%
\alias{match,IPv6,IPv6-method}
\alias{match,IPv6,IPv6r-method}
\alias{match,IPv6r,IPv6r-method}
%%
\alias{ip.match}
\alias{ip.match-method}
%%
\alias{ip.match,IPv4,IPv4-method}
\alias{ip.match,IPv4,IPv4r-method}
\alias{ip.match,IPv4r,IPv4r-method}
%%
\alias{ip.match,IPv6,IPv6-method}
\alias{ip.match,IPv6,IPv6r-method}
\alias{ip.match,IPv6r,IPv6r-method}
%%
%%
%%
\alias{ip.index}
\alias{ip.index-method}
\alias{ip.index,IPv4r-method}
\alias{ip.index,IPv6r-method}
%%
%%
%%
\title{Miscellaneous methods et functions for IP classes}
%%
\description{
  %%
  Mostly IP counterparts of base \R methods and functions for atomic vectors. Namely,
  %%
  \itemize{
    %%
    \item sorting : ip.order(), xtfrm() which is called internally by order() and sort()
    %%
    \item matching : match(), ip.match()
    %%
    \item unique()
    %%
    \item length(), is.na()
  }
}
% \usage{
  % unique(x,\ldots)
% }
%%
\details{  %%
  \subsection{Sorting}{
  
  IP object may be sorted through call to \R generic functions \code{order()} and \code{sort()}. The RIP package also provides the \code{ip.order()} methods for ordering IP which are experimental and should not be used (except maybe for IPv6, \code{ip.order()} being faster than \code{order()} at the moment).
  }
  %%
  \subsection{Lookup}{
  
  This part is still \bold{experimental} and might be subject to change in the future.
  
  \code{match()} and \code{ip.match()} do IP lookup like base \code{match()}. The RIP package make \code{match()} generic to avoid unwanted effects of method dispatch in code using the package. But note that, unfortunately, this won't change the behaviour of \code{match()} in other packages (see caveat section in the package description).

  \code{match()} and \code{ip.match()} behave differently according to their signature. When table is of class IPv4 or IPv6, \code{ip.match()} does a table lookup like base \code{match()}. But when table is an IP range and the x argument is not, both look for the range x lies into. If you want to test wether a range lies within another range, use the function returned by the \code{ip.index()} method (see example). There is no method for testing for overlap at the moment.
  
  When arguments are of the same class, \code{match()} simply call base \code{match()} on the character representation while \code{ip.match()} uses hash tables. Range search uses a binary search tree. Beware that binary search only handle non overlapping IP* ranges for the moment. See example for testing for overlap.
  
  %% By default, the hash table is dropped after the look up is completed. Use the keep.hash=T argument to keep it so that the addresses need not be re-hashed every time \code{ip.match()} is called on the object.
  
  The incomparable argument for \code{match()} or \code{unique()} is not implemented yet.
  
  }
}
%%
%%
%%
\examples{
  ## 
  x  <- ipv4(0L) + sample.int(10)
  x[order(x)]
  sort(x)
  ##
  x  <- ipv6(0L) + sample.int(10)
  ## a tad faster than order()
  x[ip.order(x)]
  \dontrun{
  ##
  ## matching the address space of a wifi interface on a GNU/Linux box
  ## note: the name of the interface might change depending on the distribution you're using among other things
  ## 
  ipv4.reserved()[match(ipv4(localhost.ip())['wlp2s0'], ipv4.reserved() )]
  ## alternatively, if tables has to be looked up several time
  m <- ip.index(ipv4.reserved())
  m(ipv4(localhost.ip())['wlp2s0']
  })
  ## overlap
  ##
  ## ip.match() and ip.index() comparison
  ##
  ## index the table
  ## 
  bidx <- ip.index(ipv4.reserved())
  ## "169.254.0.0/16"
  x <- ipv4.reserved()['Link Local']
  ## match
  ip.match(x, ipv4.reserved() )
  ## match
  ipv4.reserved()[bidx(x)]
  ## a range that lies within "169.254.0.0/16"
  x <- ipv4r("169.254.0.0/24")
  ## no match ("169.254.0.0/24"!="169.254.0.0/16")
  ip.match(x, ipv4.reserved() )
  ## match ("169.254.0.0/24" \in "169.254.0.0/16")
  ipv4.reserved()[bidx(x)]
}


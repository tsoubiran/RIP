%%
\name{Hostinfo}
%% methods
\docType{class}
%%
\keyword{classes}
%%
%%
%%
\alias{host-class}
%%
% \alias{host.info-class}
%%
%%
%%
\alias{host}
%% 
\alias{show,host-method}
%%
%%
%%
\alias{host.info}
\alias{host.info-methods}
%%
\alias{host.info,IPv4-method}
\alias{host.info,IPv6-method}
\alias{host.info,IP-method}
%%
% \alias{ipv4-method}
\alias{ipv4,host-method}
%%
% \alias{ipv6-method}
\alias{ipv6,host-method}
%%
%%
%%
\alias{localhost.ip}
%%
%%
%%
\alias{fqdn}
%%
\alias{is.fqdn}
%%
%%
%%
\alias{rir.names}
%%
\alias{ipv4.rir}
\alias{ipv6.rir}
%% 
\alias{ipv4.addr.space}
\alias{ipv6.addr.space}
%%
\alias{ipv4.reserved}
\alias{ipv6.reserved}
%%
\alias{ipv4.recovered}
%%
%%
%%
\alias{whois}
%%
%%
%%
%%
%%
%%
%%
\title{ host-info }
%%
\description{
  Methods for querying informations about hosts (DNS) or IP (address spaces)
}
%%
%% 
%%
\usage{
  %% 
  host(object,\dots)
  %% 
  host.info(host,\dots)
  %% 
  localhost.ip(\dots)
  %% 
  fqdn(hostname) 
  %%
  is.fqdn(hostname)
  %%
  whois(domain, referer , output, verbose)
  %%  
  rir.names() 
  %%
  ipv4.rir() 
  %%
  ipv6.rir() 
  %%
  ipv4.addr.space()  
  %%
  ipv6.addr.space()
  %%
  ipv4.recovered()
}
%%
\arguments{
  %%
  \item{host}{
     a vector of either or IPv4, IPv6, IP addresses
  }
  % \item{host.info}{
    % either a vector string or an IPv4, IPv6, IP address
  % }
  \item{object}{
    a character vector of domain names
  }
  %%
  \item{\dots}{
    further arguments. Only host.info (default:FALSE) for host() at the moment
  }
  %%
  \item{hostname}{
    A character vector of host names
  }
  %%
  \item{domain}{
    A character vector of domain names
  }  
  %%
  \item{referer}{
    An optional referrer to be queried
  } 
  %%
  \item{output}{
    An integer specifying whether to returns the raw response from the referrer (0) or a parsed key-value named vector (1). The latter is still experimental due to the heterogeneity of responses.
  } 
  %%
  \item{verbose}{
    An integer specifying the level of verbosity
  }
}
%%
%%
%%
\details{
  %%
  \subsection{Methods et functions for querying informations about hosts}{
  %%
  \itemize{
    %%
    \item \code{host()} takes a character vector of domain names as arguments for DNS lookup. Addresses can be extracted with the corresponding methods (ipv4(), ipv6(), ip())
    %%
    \item \code{host.info()} takes either IPv4, IPv6 or IP objects for reverse DNS lookup and returns the corresponding domain name (or NA if not found)
    %%
    \item \code{localhost.ip()} retrieves the host's interfaces IP adresses
    %%
    \item \code{fqdn()} extracts the fully qualified name of a domain name, -eg to query \code{whois} databases
    %%
    \item \code{is.fqdn()} tests whether strings in a character vector qualify as fully qualified names
    %%
    \item \code{whois()} queries \code{whois} databases for a vector of fully qualified domain names. 
  }
  %%
  Since \code{host()}, \code{host.info()} and \code{localhost.ip()} need  OS specific system call, they are only available for \bold{POSIX compliant OS} at the moment.
  %%
  In addition, the \code{host()} and \code{host.info()} methods are still \bold{very experimental} and might change in the future.
  
  %%
  \code{whois} databases typically contain informations such as registrars' names \ldots %
  Note that responses are not standardized at all and may require an ad hoc parser. %  nuts-and-bolts unmaintainable 
  This is why the \code{whois()} function returns either a (still buggy at the moment) named vector of key-value pairs or the raw responses from the referrers.
  %%
  The relevant referrer url can be determined automatically (default) or passed as an argument.
  }
  %%
  %% bold
  %%2 - 
  \subsection{Functions returning IP addresses assignments from IANA registries}{
  %%
  \itemize{
  %%
  \item \code{ipv4.addr.space()} and \code{ipv6.addr.space()} : return the corresponding IP address space
  %%
  \item \code{ipv4.reserved()} and \code{ipv6.reserved()} : return the corresponding IP reserved address space 
  %%
  \item \code{ipv6.unicast()} : IPv6 unicast addresses
  %%
  \item \code{ipv4.recovered()} : pool of IPv4 addresses recovered by IANA from RIRs
  %%  \item \code{ipv6.unicast()} : .
  %%
  \item \code{ipv4.rir()} and \code{ipv6.rir()} : returns the RIRs IP address spaces
  %%
  \item \code{rir.names()} : Regional Internet Registry names
  }
  %%
  The IP address spaces is divided into many ranges with specific purposes. For instance, IP addresses can be assigned to organizations. Some addresses are otherwise reserved for special purposes such as loopback, subnetting, local communications within a private network, multicasting, broadcasting,\ldots
  %%
  The IP address space is managed globally by the Internet Assigned Numbers Authority (IANA), and locally by five regional Internet registries (RIRs) :  
  %%
  \itemize{
    %%
    \item The African Network Information Center (AFRINIC) serves Africa
    %%
    \item  The American Registry for Internet Numbers (ARIN) serves Antarctica, Canada, parts of the Caribbean, and the United States
    %%
    \item The Asia-Pacific Network Information Centre (APNIC) serves East Asia, Oceania, South Asia, and Southeast Asia
    %%
    \item The Latin America and Caribbean Network Information Centre (LACNIC) serves most of the Caribbean and all of Latin America
    %%
    \item The Réseaux IP Européens Network Coordination Centre (RIPE NCC) serves Europe, Central Asia, Russia, and West Asia
  }
  RIRs are responsible in their designated territories for assignment to end users and local Internet registries, such as Internet service providers. %%In addition to geographic or organisation assignments, various IP addresses are reserved for special purposes such as loopback, subnetting, local communications within a private network, multicasting, broadcasting,\ldots
  
  %%
  Note differences in ouptut between \code{ipv4.addr.space()} and \code{ipv6.addr.space()}. %%
  RIRs IPv4 and Ipv6 assignments are stored by IANA in tables with different naming scheme (corresponding to \code{ipv4.addr.space()} and \code{ipv6.unicast()}). %%
  In the early days of IPv4 deployment, addresses were assigned directly to end user organizations. Therefore, \code{ipv4.addr.space()} also mixes RIR and end user organizations assignments. %%
  To find the corresponding RIR, use \code{ipv4.rir()} and \code{ipv6.rir()} instead.
  %%In addition,
  Also note that some lookups may be misleading because some IPv4 ranges have been transferred from one RIR to an another (see example). For instance, some address ranges were assigned by ARIN in the 80's to European organizations such as universities before RIPE-NCC began its operations in 1992. Those ranges were later transferred to the RIPE NCC but still belong to the ARIN address space.
  %%
  Likewise, some IPv4 addresses have been recovered by IANA from RIRs in order to delay IPv4 pool exhaustion and were later reassigned to other RIRs (see \code{\link[=ipv4.recovered]{ipv4.recovered}}).
  }
}
%%
%%
%%
\value{
  %%
  \item{host}{an host object}
  %%
  \item{host.info}{a character vector}
  %%
  \item{localhost.ip}{an IP}
}
%%
%%
%%
\examples{
  ##
  \dontrun{
  host.info(
    ipv4(
      c("127.0.0.1")
    )
  )
  }
  \dontrun{
  ##
  h <- host(c(
    "icann.org", "iana.org"
  ))
  ##
  host.info(ipv4(h))
  }
  \dontrun{
  ## French country-code top-level domains (ccTLD)
  tld <- whois(
    c(
      "fr", "re", "tf", "wf", "pm", "yt"
      , "nc", "mq", "gp", "gf", "pf"
    ), verbose = 1
  )
  ##
  sapply(tld, function(x)x['organisation'])
  }
  \dontrun{
  ##
  ##
  ##
  rhost     <- host('r-project.org')
  ## hostname       : "cran.wu-wien.ac.at"
  rhost.hnm <- host.info(ipv4(rhost))
  ## primary domain : "ac.at"
  fqdn(rhost.hnm)
  ## ARIN 
  ipv4.rir()[ip.match(ipv4(rhost), ipv4.rir())]
  ##
  ip.match(ipv4(rhost), ipv4.recovered())
  ## domain name info 
  rdom.wh   <- whois('r-project.org', output=1)
  ## "AT"
  rdom.wh[['r-project.org']]['Registrant Country']
  ## host
  rhost.wh0 <- whois(ipv4(rhost),verbose = 2, output=1)
  }
}

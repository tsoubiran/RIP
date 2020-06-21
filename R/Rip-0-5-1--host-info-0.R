##____________________________________________________________________________________________________________
##
##
##
##____________________________________________________________________________________________________________
##
##
##
##
setMethod(
  "initialize"
  ##
  , signature(.Object="host") 
  ##
  , function(.Object, hostname=NULL, host.info=F){
    ##
    if(is.null(hostname)) return(.Object)
    ##
    if(!is.character( hostname ) ) stop("hostname should be of type character")
    ##
    hn <- unique(hostname)
    ##
    hn <- hn[!is.na(hn)]
    ##
    .Object = .Call("Rip_getaddrinfo_0", hn)
    ##
    .Object@hostname <- hn
    ##
    .Object@.Data = match(hostname, hn)
    ##
#     print(str(.Object))
#     cat('len:', length( .Object@ipv4 ), nrow( .Object@ipv4 ) , '\n')
    ## ipv4.hptr
    if( (len <- .Object@ipv4@length )>0 ) .Object@ipv4@.Data <- 0:(len-1)
#     if( length( .Object@ipv4@ipv4 ) ){
#       len <- .Object@ipv4@length ##<- length(.Object@ipv4@ipv4)
#       if(len>0) .Object@ipv4@.Data <- 0:(len-1)
#     }
    ## 
    if( (len<-.Object@ipv6@length)>0 ) .Object@ipv6@.Data <- 0:(len-1)
#     if( nrow( .Object@ipv6 @ipv6) ){
#       len <- .Object@ipv6@length ## <- nrow(.Object@ipv6@ipv6)
#       if(len>0) .Object@ipv6@.Data <- 0:(len-1)
#     }
    ##
    ## !!!
    ##
    if( host.info ){
      .Object@ipv4.hostinfo <- host.info(.Object@ipv4)
    }
    ##
    .Object
  }
)
##
##
##
host <- function(object=NULL,...) new('host', object,...)
##____________________________________________________________________________________________________________
##
##
##
setMethod(
  "as.character"
  , "host"
  , function(x){
    hip <- sapply(
      x@.Data
      , function(i){
        ##
        lo <- x@ipv4.hptr[c(i)]
        hi <- x@ipv4.hptr[c(i+1)]
        ip4 <- if( lo!=hi) paste( as.character( x@ipv4[ (lo+1):hi ] ) , collapse = ",") else ""
        ##
        lo <- x@ipv6.hptr[c(i)]
        hi <- x@ipv6.hptr[c(i+1)]
        ip6 <- if( lo!=hi) paste( as.character( x@ipv6[ (lo+1):hi ] ) , collapse = ",")  else ""
        ##
        paste(ip4, ip6, sep=ifelse( ip4!="" & ip6!="", "--" , "") )
        ##
      }
    )
    ##
    names(hip) <- x@hostname
    ##
    hip
  }
)
##
setMethod(
  "show"
  , "host"
  , function(object) print(as.character(object))
)
##____________________________________________________________________________________________________________
##
##
##
setMethod(
  "ipv4"
  , "host"
  , function(object,...){
    ##
    ip <- object@ipv4
    ##
    nip <- object@ipv4.hptr[-1] - object@ipv4.hptr[-length(object@ipv4.hptr)]
    ##
    names(ip) <- object@hostname[
      rep(1:length(object@hostname) , nip)
      ]
    ##
    ip
  }
)
##
##
##
setMethod(
  "ipv6"
  , "host"
  , function(object,...){
    ##
    ip <- object@ipv6
    ##
    nip <- object@ipv6.hptr[-1] - object@ipv6.hptr[-length(object@ipv6.hptr)]
    ##
    names(ip) <- object@hostname[
      rep(1:length(object@hostname) , nip)
      ]
    ##
    ip
  }
)
##
##
##
setMethod(
  "ip"
  , signature(e1="host",e2="missing")
  , function(e1,...){
    ##ip(list(ipv4=ipv4(e1),ipv6=ipv6(e1)),append=T)
    ##
    ip(ipv4(e1),ipv6(e1),append=T)
  }
)
##____________________________________________________________________________________________________________

##____________________________________________________________________________________________________________
##
## id(host) <- host.name
##
setMethod(
  "host.info"
  ## 
  , signature(host = "IPv4")
  ##
  , function(host){
    ##
    ip        <- unique(host)
    ##
    host.name <- .Call("Rip_ipv4_gethostbyaddr_0", ip )
    ##
    host.name <- host.name[match(host, ip)]
    ##
    if( !is.null(host@id) ) names( host.name ) <- host@id
    ## 
    host.name
  }
)
##____________________________________________________________________________________________________________
##
## id(host) <- host.name
##
setMethod(
  "host.info"
  ## 
  , signature(host = "IPv6")
  ##
  , function(host){
    ##
    ip        <- unique(host)
    ##
    host.name <- .Call("Rip_ipv6_gethostbyaddr_0", ip )
    ##
    host.name <- host.name[match(host, ip)]
    ##
    if( !is.null(host@id) ) names( host.name ) <- host@id
    ## 
    host.name
  }
)
##____________________________________________________________________________________________________________
##
##
## !!!: fixme !!!
##
setMethod(
  "host.info"
  ## 
  , signature(host = "IP")
  ##
  , function(host){
    ##
    ip4 <- callGeneric(host@ipv4)
    ##
    ip6 <- callGeneric(host@ipv6)
    ##
    ifelse(ip.version(host)==4, ip4, ip6)
  }
)
##_____________________________________________________________________________________________________________
##
##
##
localhost.ip <- function(...){
  ##
  ip <- .Call('Rip_ifaddrs_0')
  ##
#   print(str(ip))
  ## print
  ip@.Data = (ifelse(
    !is.na(ip@ipv4)
    , 4L
    , ifelse(!is.na(ip@ipv6), 6L, NA)
  ))
  ##
  ip
}
##_____________________________________________________________________________________________________________

##____________________________________________________________________________________________________________
##
##
##
##
##____________________________________________________________________________________________________________
##
##
##
##
whois <- function(domain, referer=NA, output=1,verbose=0){
  ##
  whois.query <- function(domain,referer){
    ##
    domain <-as.character(domain) ##  domain ## 
    ##
    if(verbose>0) cat('whois:', domain, (referer),"\n")
    ##
    resp0 <- ""
    ##
    if( is.na(referer) ){
      ##
      co <- socketConnection(
        host="whois.iana.org", port = 43, blocking=TRUE, server=FALSE, open="r+"
      )
      ##
      ## catch
      ##
      writeLines(domain, co)
      # while( resp <- readLines(co) ) print(resp)
      resp0 <- readLines(co)
      ##
      close(co)
      ##
      if(verbose>2) cat("resp:", resp0, "\n")
      ##
      referer <- (stringr::str_match(resp0, "refer:\\s*(.+?)$"))[grep("refer", resp0, value=F),2] 
    }
    ##
    if( all(is.na(referer)) ){
      ## TLD : tolower(domain)!=tolower(resp0["domain"])
      fv <- stringr::str_match(resp0 , "^([A-Za-z\\s]+)\\s*:\\s+(.+)$")
      ##print(fv)
      ##
      i <- which(tolower(fv[,2])=="domain")
      ##
      if( length(i)==0 ){
        warning("missing domain field in whois.iana.org response:\n", resp0)
        return( NA ) 
      }
      ##
      if( any(tolower(fv[i,3])!=tolower(domain)) ){
        warning("no referer found : ", domain)
        return( NA ) 
      }
      ##
      resp1 <- resp0
    } 
    else{
      ##
      if(verbose>1) cat("referer:", referer, "\n")
      ##
      referer <- referer[
        which( !is.na(referer) )[1]
      ]
      ##
      co <- socketConnection(
        host=referer, port = 43, blocking=TRUE, server=FALSE, open="r+"
      )
      ##
      writeLines(domain, co)
      # while( resp <- readLines(co) ) print(resp)
      resp1 <- readLines(co)
      ##
      close(co)
    }
    ##
    if(!output) return(resp1)
    ##
    ##
    ##
    resp1 <- resp1[!grepl("^#", resp1)]
    ##
    fv <- stringr::str_match(resp1[nchar(resp1)>0] , "^([A-Za-z\\s]+)\\s*:\\s+(.+)$")
    ##
    res <- ifelse( is.na(fv[,3]), fv[,1], fv[,3])
    ##
    names(res) <- fv[,2]
    ##
    res[!is.na(res)]
  }
  ## 
  ## 
  ##
  mapply(whois.query, as.character(domain), as.character(referer), SIMPLIFY = F)
}
##_____________________________________________________________________________________________________________

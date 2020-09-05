##________________________________________________________________________________________________________________________
##
##
##
##________________________________________________________________________________________________________________________
##
##
##
##
##
##
##
setMethod(
  "initialize"
  ##
  , signature(.Object="IP") 
  ## ipv4=NULL, ipv6=NULL
  , function(.Object, ipstrings=NULL,ip=NULL,ip4=NULL,ip6=NULL,append=FALSE){
    ##
    ## Quick return
    ##
    if( 
        is.null(ipstrings) 
      & ( ip.null <-is.null(ip) ) & ( ip4.null <- is.null(ip4) ) 
    ){
      ##
      return(.Object)
    } 
    ##
    ## Input from strings
    ##
    if ( length(ipstrings) ){    
      ##
      if( typeof( ipstrings )!='character' ){
        ##
        stop('ip should be of type character')
      }
      ##
      suppressWarnings(
        .Object@ipv4 <- new('IPv4', ipstrings )
      )
      ##
      suppressWarnings(
        .Object@ipv6 <- new('IPv6', ipstrings )
      )
    ##
    }else{
      ##
      ##
      ##
      if( 
        !ip.null ## !is.null(ip) 
      ){
        ##
        if( !ip4.null ) stop('both ip & ip4 args are set' )
        ##
        if( !is.null(ip6) ) stop('both ip & ip6 args are set' )
        ## !is.list(ip) stop('ip should be a list' )
        ip4 <- ip[['ipv4']] 
        ip6 <- ip[['ipv6']]
        ##
#           print(ip4);print(ip6)
      }
      ##
      ## is.list(ip) & ( names(ip %in% c('ipv4', 'ipv6'))
      ##
      if(
        ( 
          class( ip4  )=='IPv4' | class( ip6 )=='IPv6'
        )
      ){
        ##
        ## 
        ##
        if( append==F ){
            ##
            nip4 <- length(ip4)
            ##print(nip4)
            ##
            ip4 <- if( is.null(ip4) ) ipv4(rep(NA_character_,length(ip6))) else ip4
            ##
            ip6 <- if( is.null(ip6) ) ipv6(rep(NA_character_, nip4)) else ip6
            ##, na.rm=T 
            if( length(ipv4@.Data)!=length(ipv6@.Data) ) stop( "ips should have the same length")
            ##
  #           if( 
  #             any( (!is.na(ipv4@.Data))==(!is.na(ipv6@.Data)) )
  #           ) stop( "mixed ip families")
          
          }else{
            ##
            nip4 <- length(ip4)
            ##
            ip4@.Data <- c(ip4@.Data, rep(NA_integer_, length(ip6) ) )
            ##
            ip6@.Data <- c( rep(NA_integer_, nip4), ip6@.Data )
            ##return(.Object)
          }
        ##
        .Object@ipv4 <- ip4
        ##
        .Object@ipv6 <- ip6
      ##
      }else{
        ##
        stop('malformed ip arg')
      }
    } ## !ipstrings
    ##
    ##
    ##
    .Object@.Data <- ifelse(
      !is.na(.Object@ipv4)
      , 4L
      , ifelse(
        !is.na(.Object@ipv6)
        , 6L
        , NA_integer_
      )
    )
    ## 
    if( na <-length(which(is.na(.Object@.Data) ) ) )warning( length(.Object@.Data) - ( na ) )
    ##
    .Object    

  }
)
##
setMethod(
  "ip"
  , signature(e1="missing",e2="missing")
  , function(e1,e2,...) new('IP', ...)
)
##
##
##
setMethod(
  "ip"
  , signature(e1 ="character", e2="missing")
  , function(e1, e2,...) new('IP', ipstrings=e1,...)
)
##
##
##
setMethod(
  "ip"
  , signature(e1 ="integer", e2="missing")
  , function(e1, e2,...){
    ip <- new('IP', ...)
    ##
    suppressWarnings(
      ip@ipv4  <- ipv4(e1)
    )
    ##
    suppressWarnings(
      ip@ipv6  <- ipv6(e1)
    )
    ##print(str(e1@ipv6))
    ip@.Data <- ifelse(
      is.na(ip@ipv4) 
      ##
      , ifelse(
          is.na(ip@ipv6)
        , NA_integer_
        , 6L
      )
      , 4L
    )
    ##
    ip
  }
)
##
##
##
setMethod(
  "ip"
  , signature(e1 ="IPv4", e2="IPv6")
  , function(e1, e2,...) new('IP', ip4=e1,ip6=e2,...)
)
##
##
##
setMethod(
  "ip"
  , signature(e1 ="list", e2="missing")
  , function(e1, e2,...) new('IP', ip=e1,...)
)

##
##
##
setMethod(
  "ipv4"
  , signature(object ="IP")
  , function(object, drop=F,...){
    ##
    idx <- ifelse( drop==F, rep(T,length(object)), object@.Data==4L )
    ##
    object@ipv4[idx]
  }
)
##
##
##
setMethod(
  "ipv6"
  , signature(object ="IP")
  , function(object, drop=F,...){
    ##
    idx <- ifelse( drop==F, rep(T,length(object)), object@.Data==6L )
    ##
    object@ipv6[idx]
  }
)
##
##
##
##
setMethod(
  "ip.version"
  , "IP"
  , function(ip,...) ip@.Data
)
##
##
##
setMethod(
  "is.numeric"
  ## 
  , signature(x = "IP")
  , function(x) FALSE
)
##
##
##
##________________________________________________________________________________________________________________________

##________________________________________________________________________________________________________________________
##
##
##
##
##
##
##
setMethod(
  "initialize"
  ##
  , signature(.Object="IPr") ## 
  ##
  , function(.Object, ipstrings=NULL,ipr=NULL,ip4r=NULL,ip6r=NULL,lo=NULL, hi = NULL, nip=NULL,append=FALSE){
    ##
    ##
    if( is.null(ipstrings) & is.null(ip) & is.null(ip4r) ){
      ##
      return(.Object)
    } 
    ##
    ##
    ##
    if ( length(ipstrings) ){    
      ##
      if( typeof( ipstrings )!='character' ){
        ##
        stop('ip should be of type character')
      }
      ##
      suppressWarnings(
        .Object@ipv4r <- new('IPv4r', ipstrings )
      )
      ##
      suppressWarnings(
        .Object@ipv6r <- new('IPv6r', ipstrings )
      )
      
    }else {
      ##
#         print(c(class( ip4r ), class( ip6r )))
      ## 
      ##
      if(
        class( lo )=='IP' & ( class( nip  ) %in% c('integer','numeric') )
      ){
        ## cat("nip\n")
        ##
        hi <- lo + nip
        ##print(hi)
        ##
        return( ipr(lo,hi) )
        
      } else if(
        ( 
          class( lo )=='IP' & class( hi )=='IP'
        )
      ){
#         if(
#           ( 
#             class( lo )=='IP' & (
#               ( class( hi )=='IP' ) | ( class( hi <- lo + nip )=='IP' ) 
#             )
#           )
#         ){
        ##
#           cat("lo-hi\n")
#           print(lo)
#           print(hi)
        ##
        ip4r <- ipv4r(ipv4(lo),ipv4(hi))
        ##
        ip6r <- ipv6r(ipv6(lo),ipv6(hi))
        
      }else if( !is.null(ipr) ) {
        ##
        ip4r <- ipr[[1]] 
        ip6r <- ipr[[2]] 
      }
      ##
      ## is.list(ip) & ( names(ip %in% c('ipv4', 'ipv6'))
      ##
      if(
        ( 
          ##class( ip4r )=='IPv4r' & class( ip6r )=='IPv6r'
          ##
          (class( ip4r )=='IPv4r') | (class( ip6r )=='IPv6r')
        )
      ){
        ##
#           cat("ip4-6r\n")
        ## 
        ##
        if( append==F ){
          ##
#             nip4 <- length(ip4r)
          ##
          ipv4r <- if( is.null(ip4r) ) ipv4r(rep(NA_character_,length(ip6r))) else ip4r
          ##
          ipv6r <- if( is.null(ip6r) ) ipv6r(rep(NA_character_,length(ip4r))) else ip6r
          ##, na.rm=T 
          if( length(ip4r@.Data)!=length(ip6r@.Data) ) stop( "ips should have the same length")
          ##
          ## all( !is.na(ip4r) ==  is.na(ip6r) )
          ##
#             ipv4r <- ip4r
#             ipv6r <- ip6r 
          ##
          #           if( 
          #             any( (!is.na(ipv4@.Data))==(!is.na(ipv6@.Data)) )
          #           ) stop( "mixed ip families")
          
        }else{
          ## if ( is.null(ip4r) | is.null(ip6r) ) error()
          ##rbind2(ipv4r(NA_character_),ipv4r)
          ##
          nip4 <- length(ip4r)
          ipv4r <- ip4r[c(1:length(ip4r), rep(NA,length(ip6r)))]
          ipv6r <- ip6r[c( rep(NA,nip4) , 1:length(ip6r) )]
#             ipv4@.Data <- c(ipv4@.Data, rep(NA_integer_, length(ipv6) ) )
#             ##
#             ipv6@.Data <- c( rep(NA_integer_, length(ipv4)), ipv6@.Data )
          ##return(.Object)
        }
        ##
        .Object@ipv4r <- ipv4r
        ##
        .Object@ipv6r <- ipv6r
      }else{
        ##
        stop('malformed ip arg')
      }
    }
    ##
    ##
    ##
    .Object@.Data <- ifelse(
      !is.na(.Object@ipv4r)
      , 4L
      , ifelse(
        !is.na(.Object@ipv6r)
        , 6L
        , NA_integer_
      )
    )
    ## 
    if( na <-length(which(is.na(.Object@.Data) ) ) )warning( length(.Object@.Data) - ( na ) )
    ##
    .Object    
    
  }
)
##
##
##
setMethod(
  "ipr"
  , signature(e1="character",e2="missing")
  , function(e1,e2,...) new('IPr', ipstrings=e1,...)
)
##
##
##
setMethod(
  "ipr"
  , signature(e1="IPv4r",e2="IPv6r")
  , function(e1,e2,...) new('IPr', ip4r=e1,ip6r=e2,...)
)
##
##
##
setMethod(
  "ipr"
  , signature(e1="IP",e2="IP")
  , function(e1,e2,...) new('IPr', lo=e1,hi=e2,...)
)
##
##
##
setMethod(
  "ipr"
  , signature(e1="IP",e2=".__intFP__.")
  , function(e1,e2,...) new('IPr', lo=e1,nip=e2,...)
)
##
##
##
setMethod(
  "ipr"
  , signature(e1="list",e2="missing")
  , function(e1,e2,...) new('IPr', ipr=e1,...)
)
##
setMethod(
  "ipr"
  , signature(e1="missing",e2="missing")
  , function(e1,e2,...) new('IPr', ...)
)
##
##
##
##
##
##
setMethod(
  "ip"
  , signature(e1 ="IPr",e2="missing")
  , function(e1,...){
    ##
    ipv4 <- ipv4(e1@ipv4r)
    ipv6 <- ipv6(e1@ipv6r)
    ##
    lo <- ip(
      list(ipv4=ipv4[[1]], ipv6=ipv6[[1]])##,append=F
    )
    hi <- ip(
      list(ipv4=ipv4[[2]], ipv6=ipv6[[2]])##,append=F
    )
    ##
    list(lo=lo,hi=hi)
  }
)
##  
##  
##
setMethod(
  "lo"
  , "IPr"
  , function(e1,...){
    ##
    ip(
      ip4=callNextMethod(e1@ipv4r,...), ip6=callNextMethod(e1@ipv6r,...) ##,append=F
    )
  }
)
##  
##  
##
setMethod(
  "hi"
  , "IPr"
  , function(e1,...){
    ##
    ip(
      ip4=callNextMethod(e1@ipv4r,...), ip6=callNextMethod(e1@ipv6r,...) ##,append=F
    )
  }
)
##
##
##
setMethod(
  "ipv4r"
  , signature(e1="IPr",e2="missing")
  , function(e1,e2,drop=F,...){
    ##
    idx <- ifelse( rep(drop==F,length(e1)), T, e1@.Data==4 )
    ##
    e1@ipv4r[idx]
  } 
)
##
##
##
setMethod(
  "ipv6r"
  , signature(e1="IPr",e2="missing")
  , function(e1,e2,drop=F,...){
    ##
    idx <- ifelse( rep(drop==F,length(e1)), T, e1@.Data==6 )
    ##
    e1@ipv6r[idx]
  } 
)
##
##
##
##
setMethod(
  "ip.version"
  , "IPr"
  , function(ip,...) ip@.Data
)
##
##
##
setMethod(
  "is.numeric"
  ## 
  , signature(x = "IPr")
  , function(x) FALSE
)
##
##
##
##
##________________________________________________________________________________________________________________________

##________________________________________________________________________________________________________________________
##
##
##
##________________________________________________________________________________________________________________________
##
##
## 
##
##
# setMethod(
#   "ip"
#   , "character"
#   , function(ip) new('IP', ip)
# )
##________________________________________________________________________________________________________________________
##
## 
## 
setMethod(
  "["
  , signature(x="IP", i='.__subscript__.')
  , function(x, i, ...){
    if(!is.null(x@ipv4@ipv4 )) x@ipv4  <- x@ipv4[i]
    if(!is.null(x@ipv6@ipv6 )) x@ipv6  <- x@ipv6[i]
    x@.Data <- x@.Data[i]
    x
  }
)
##
##
##
## 
##
# setGeneric("ipv4<-", function(x,i,value){
#     standardGeneric("ipv4<-")
# })
## 
setMethod(
  "[<-"
  , signature(x="IP", i=".__subscript__.", value='IP')
  ## 
  , function(x, i, j, ..., value){
    ##
    x@ipv4[i] <- value@ipv4
    ##
    x@ipv6[i] <- value@ipv6
    ##
    x@.Data <- ifelse(
      !is.na(x@ipv4)
      , 4L
      , ifelse(
        !is.na(x@ipv6)
        , 6L
        , NA_integer_
      )
    )
    ##
    x
  }
)
##
## !!!FIXME!!!
## 
setMethod(
  "[<-"
  , signature(x="IP", i='.__subscript__.', value='IPv4')
  ## x, i, value
  , function(x, i, j, ..., value){
    ##
    x@ipv4[i] <- value
    ##
    ##
    ##
    idx <- x@ipv6@.Data[i] ## is.na(x@ipv6@.Data[i]) 
    ##
    if( any(!is.na(idx) ) ){
      ##
      ## x@ipv6[idx] <- NA
      ##
      x@ipv6@ipv6   <- matrix(x@ipv6@ipv6,ncol=2 )[-(idx+1), ]
      ##
      x@ipv6@length <- nrow(x@ipv6@ipv6)
      ##
      x@ipv6@.Data[ i ] <- NA
      ##
      ## 
      nna <- !is.na(x@ipv6@.Data)
      ##
      x@ipv6@.Data[(nna)] <- (cumsum( nna[nna] )) - 1L
#       ##idx <- (1:length(x@ipv6@.Data)) >max(i)
#       ##x@ipv6@.Data[ idx ] <- x@ipv6@.Data[ idx ]-1L
      ##
      x@.Data[i] <- ifelse(!is.na(value),4,NA)
    }
    ##
    x
  }
)
## TODO
# setMethod(
#   "[<-"
#   , signature(x="IP", i='.__subscript__.', value='IPv6')
#   , function(x, i, j, ..., value){
#     
#   }
# )
##
## !!! : integer value does not make sense (what is v4 and what is v6 ?)
## 
setMethod(
  "[<-"
  , signature(x="IP", i='.__subscript__.', value='ANY')
  , function(x, i, j, ..., value){
    stop("unimplemented assign method for IP object")
  }
)
##
##
##
setMethod(
  "id"
  , signature(x = "IP")
  , function(x){
    ##
    ifelse(ip.version(x)==4L, id(x@ipv4), id(x@ipv6))
  }
)
##
setMethod(
  "id<-"
  , signature(x = "IP")
  , function(x,value){
    ##
    id( x@ipv4) <- ifelse(ip.version(x)==4L, value, NA)
    ##
    id( x@ipv6) <- ifelse(ip.version(x)==6L, value, NA)
    ##
    x
  }
)
##
##
##
names.IP <- function(x){
  ##
  if( 
    is.null( ip4.nm <- ip.get.id(x@ipv4) ) &  is.null( ip6.nm <- ip.get.id(x@ipv6)) 
  ) return(NULL)
  ##
  nm <- if( is.null( ip4.nm ) ) rep(NA_character_, length(x)) else ip4.nm
  ##
  ifelse(
    ip.version(x)==6
    , if( is.null( ip6.nm ) ) rep(NA_character_, length(x)) else ip6.nm
    , nm
  )
#   ##
#   if( is.null(x@ipv4@id) &  is.null(x@ipv6@id) ) return(NULL)
#   ##
#   nm <- if( is.null(x@ipv4@id) ) rep(NA_character_, length(x)) else ip.get.id(x@ipv4)
#   ##
#   ifelse(
#     ip.version(x)==6
#     , if( is.null(x@ipv6@id) ) rep(NA_character_, length(x)) else ip.get.id(x@ipv6)
#     , nm
#   )
} 
##ip.set.id
'names<-.IP' <- function(x,value){ 
  x@ipv4 <- IP_setId(x@ipv4,value)
  x@ipv6 <- IP_setId(x@ipv6,value)
  x
}
##________________________________________________________________________________________________________________________
##
##
##
setMethod(
  "[<-"
  , signature(x="IPr", i='.__subscript__.', value='IPr')
  , function(x, i, j, ..., value){
    ##
    x@ipv4r[i]   <- value@ipv4r
    ##
    x@ipv6r[i]   <- value@ipv6r
    ##
    ##
    ##
    x@.Data <- ifelse(
      !is.na(x@ipv4r)
      , 4L
      , ifelse(
        !is.na(x@ipv6r)
        , 6L
        , NA_integer_
      )
    )
    ##
    x
  }
)

##____________________________________________________________________________________________________________________
##
##
## TODO: `c.IP` <- function(...) 
##
##
##
setMethod(
  "rbind2"
  , signature(x = "IP", y="IP")
  , function(x, y, ...){
    ##
    x@ipv4   <- rbind2(x@ipv4, y@ipv4)
    ##
    x@ipv6   <- rbind2(x@ipv6, y@ipv6)
    ##
    x@.Data <- ifelse(
        !is.na(x@ipv4)
        , 4L
        , ifelse(
          !is.na(x@ipv6)
          , 6L
          , NA_integer_
        )
      )
    ##
    x
  }
)
##____________________________________________________________________________________________________________________

##____________________________________________________________________________________________________________________
##
##
##
##
##________________________________________________________________________________________________________________________
##
## 
## 
setMethod(
  "["
  , signature(x="IPr", i='.__subscript__.')
  , function(x, i, ...){
    if(!is.null( x@ipv4r@ipr )) x@ipv4r  <- x@ipv4r[i]
    if(!is.null( x@ipv6r@ipr )) x@ipv6r  <- x@ipv6r[i]
    x@.Data <- x@.Data[i]
    x
  }
)
##
##
##
setMethod(
  "id"
  , signature(x = "IPr")
  , function(x){
    ##
    ifelse(ip.version(x)==4L, id(x@ipv4r), id(x@ipv6r))
  }
)
##
setMethod(
  "id<-"
  , signature(x = "IPr")
  , function(x,value){
    ##
    id( x@ipv4r) <- ifelse(ip.version(x)==4L, value, NA)
    ##
    id( x@ipv6r) <- ifelse(ip.version(x)==6L, value, NA)
    ##
    x
  }
)
##
##
##
##
names.IPr <- function(x){
  ##
  if( 
    is.null( ip4.nm <- ip.get.id(x@ipv4r) ) &  is.null( ip6.nm <- ip.get.id(x@ipv6r)) 
  ) return(NULL)
  ##
  nm <- if( is.null( ip4.nm ) ) rep(NA_character_, length(x)) else ip4.nm
  ##
  ifelse(
    ip.version(x)==6
    , if( is.null( ip6.nm ) ) rep(NA_character_, length(x)) else ip6.nm
    , nm
  )
} 
##
'names<-.IPr' <- function(x,value){ 
  x@ipv4r <- ip.set.id(x@ipv4r,value)
  x@ipv6r <- ip.set.id(x@ipv6r,value)
  x
}
##
##
##
setMethod(
  "rbind2"
  , signature(x = "IPr", y="IPr")
  , function(x, y, ...){
    ##
    x@ipv4r   <- rbind2(x@ipv4r, y@ipv4r)
    ##
    x@ipv6r   <- rbind2(x@ipv6r, y@ipv6r)
    ##
    x@.Data <- ifelse(
        !is.na(x@ipv4r)
        , 4L
        , ifelse(
          !is.na(x@ipv6r)
          , 6L
          , NA_integer_
        )
      )
    ##
    x
  }
)
## 
# rbind.IPr <- function(..., deparse.level = 1, make.row.names = TRUE, stringsAsFactors = default.stringsAsFactors()){
#   ##
#   cat("s3:rbind\n")
#   args <- list(...)
#   rbind2(args[[1]],args[[2]])
# }
##________________________________________________________________________________________________________________________

##________________________________________________________________________________________________________________________
##
##
##
##________________________________________________________________________________________________________________________
##
##
##
##
setMethod(
  "as.character"
  , "IP"
  , function(x,...){
    ## .Call("Rip_ip_as_character_0", x)
    res <- rep( NA_character_, length(x) ) ## vector('character', length(x) )
#     ## callNextMethod
#     res[which(x@.Data==4)] <- as.character(x@ipv4)
#     ##
#     res[which(x@.Data==6)] <- as.character(x@ipv6)
    ## 
    if(!is.null(x@ipv4@ipv4 )){
      idx      <- which(x@.Data==4L)
      res[idx] <- as.character(x@ipv4)[idx]
    }
    ##
    if(!is.null(x@ipv6@ipv6 )){
      idx      <- which(x@.Data==6L)
      res[idx] <- as.character(x@ipv6)[idx]
    }
    ##
    ##
    if( !is.null( nm <- names(x) ) ) names(res) <- nm
    ##
    res
  } 
)
##
##
##
##
setMethod(
  "print"
  , "IP"
  , function(x,...){
    ##.Call("Rip_ip_as_character_0", x) 
    ##
    print(as.character(x,...))
    ##
    invisible(x)
  }
)
##
setMethod(
  "show"
  , "IP"
  , function(object){
     ##.Call("Rip_ip_as_character_0",object)
     ##
     print(object)
     invisible()
  }
)
##
## 
##
format.IP <- function(x
  , trim = FALSE, digits = NULL, nsmall =0L, justify = c("left","right", "centre", "none"), width = NULL, na.encode = TRUE,scientific = NA, big.mark = "", big.interval = 3L
  , small.mark = "", small.interval = 5L, decimal.mark = ".", zero.print = NULL, drop0trailing = FALSE
  , ...) as.character(x,...) ##.Call("Rip_IPv4_as_character_0", x) ## 
##
toString.IP <- function(x,...) as.character(x,...) ##.Call("Rip_IPv4_as_character_0", x) ##
##
##
##
setMethod(
  "as.numeric"
  , "IP"
  , function(x){
    ##
     ifelse( ip.version(x)==4, as.numeric(x@ipv4), as.numeric(x@ipv6))
  }
)
##
##
##
unique.IP <- function(
  x,...
){
  ##
  new('IP', unique(as.character(x),...))
}
##
##
##
# setMethod(
#   "ipv4"
#   , "IP"
#   , function(ip) ip@ipv4
# )
##________________________________________________________________________________________________________________________
##
##
##
##
##
##
##
setMethod(
  "as.character"
  , "IPr"
  , function(x,...){
    ## 
    res <- rep( NA_character_, length(x) ) ## vector('character', length(x) )
    ## 
    if(!is.null(x@ipv4r@ipr )){
      idx      <- which(x@.Data==4L)
      res[idx] <- as.character(x@ipv4r)[idx]
    }
    ##
    if(!is.null(x@ipv6r@ipr )){
      idx      <- which(x@.Data==6L)
      res[idx] <- as.character(x@ipv6r)[idx]
    }
    ##
    ##
    if( !is.null( nm <- names(x) ) ) names(res) <- nm
    ##
    res
  } 
)
##
##
##
##
setMethod(
  "show"
  , "IPr"
  , function(object){
    ##
    print(as.character(object))
    ##
    invisible()
  }
)
##
setMethod(
  "print"
  , "IPr"
  , function(x,...){
    ##
    print(as.character(x,...))##
    ##
    invisible(x)
  }
)
##
## 
##
format.IPr <- function(x, ...) as.character(x,...) ##
##
toString.IPr <- function(x,...) as.character(x,...) ##
##
##________________________________________________________________________________________________________________________

##________________________________________________________________________________________________________________________
##
##
##
##________________________________________________________________________________________________________________________
##
## ?`group generic`
##
##
setMethod(
  "Arith"
  ##
  , signature(e1 = "IP" , e2 = ".__intFP__.")
  , function(e1,e2){
    ##print(.Generic)
    ##
    if(
      .Generic %in% c(  "*",   "^",   "%%",  "%/%", "/" )
    ){
      ##
      stop( .Generic, " invalid operator for ip class" )
    }
    ##print(str(e1@ipv4))
    ##
    suppressWarnings(
      e1@ipv4  <- callGeneric(e1@ipv4, e2)
    )
    ##
    suppressWarnings(
      e1@ipv6  <- callGeneric(e1@ipv6, e2)
    )
    ##
    e1@.Data <- ( ifelse(
      (is.na(e1@ipv4) & is.na(e1@ipv6)) 
      , NA_integer_
      , e1@.Data
    ))
    ##
    e1
  }
)
setMethod(
  "Arith"
  ## 
  , signature(e1 = "IP", e2 = "IP")
  , function(e1,e2){
    ##print(.Generic)
    ##
    if(
      .Generic %in% c(  "*",   "^",   "%%",  "%/%", "/" )
    ){
      ##
      stop( .Generic, " invalid operator for ip class" )
    }
    ##
    suppressWarnings(
      e1@ipv4  <- callGeneric(e1@ipv4, e2@ipv4)
    )
    ##
    suppressWarnings(
      e1@ipv6  <- callGeneric(e1@ipv6, e2@ipv6)
    )
    ##
    e1@.Data <- ( ifelse(
      (is.na(e1@ipv4) & is.na(e1@ipv6)) 
      , NA_integer_
      , e1@.Data
    ))
    ##
    e1
  }
)
setMethod(
  "Arith"
  ## 
  , signature(e1 = "IP", e2 = "missing")
  , function(e1,e2){
    ##print(.Generic)
    ## "^", 
    if(
      .Generic %in% c(  "*",    "%%",  "%/%", "/" )
    ){
      ##
      stop( .Generic, " invalid operator for ip class" )
    }
    ##
    suppressWarnings(
      e1@ipv4  <- callGeneric(e1@ipv4)
    )
    ##
    suppressWarnings(
      e1@ipv6  <- callGeneric(e1@ipv6)
    )
    ##
    e1@.Data <- ( ifelse(
      (is.na(e1@ipv4) & is.na(e1@ipv6)) 
      , NA_integer_
      , e1@.Data
    ))
    ##
    e1
  }
)
##
##
# getGroupMembers("Compare")
# "==" ">"  "<"  "!=" "<=" ">="
##
## all(ipsign.ip.0==ipsign.ip.app.0)
##
setMethod(
  "Compare"
  ## 
  , signature(e1 = "IP", e2 = "IP")
  , function(e1,e2){
    ##print(.Generic)
    ##
    idx <- !is.na(e1@.Data) | !is.na(e2@.Data)
    ##
    if(
      any( e1@.Data[idx]!=e2@.Data[idx] )
    ){
      stop(.Generic, "mixed ip family")
    }
    ##
    suppressWarnings(
      v4 <- callGeneric(e1@ipv4, e2@ipv4)
    )
    ##
    suppressWarnings(
      v6  <- callGeneric(e1@ipv6, e2@ipv6)
    )
    ##
    res <- ifelse( ip.version(e1)==4, v4, v6) ## v4|v6
    ##
    ## !!!ID!!!
    ##
    res
  }
)
##
##
#  getGroupMembers("Logic")
# "&" "|"
##
## où est "!" ? d'après la doc avec Logic (?groupGeneric, ce qui n'est pas...hmmm, logique) mais getGroup('!') ne retourne rien
## getGroup("!") : list()
## 
setMethod(
  "Logic"
  ##
  , signature(e1 = "IP" , e2 = "IP")
  , function(e1,e2){
    ##print(.Generic)
    ##
    idx <- which( !is.na(e1@ipv4) | !is.na(e2@ipv4) )
    ##
    if(
      any( e1@ipv4@.Data[idx]!=e2@ipv4@.Data[idx] )
    ){
      stop(.Generic, "mixed ip family")
    }
    ##
    suppressWarnings(
      e1@ipv4  <- callGeneric(e1@ipv4, e2@ipv4)
    )
    ##print(e1@ipv4)
    suppressWarnings(
      e1@ipv6  <- callGeneric(e1@ipv6, e2@ipv6)
    )
    ##print(str(e1@ipv6))
    e1@.Data <- ( ifelse(
      (is.na(e1@ipv4) & is.na(e1@ipv6)) 
      , NA_integer_
      , e1@.Data
    ))
    ## e1@.Data
    ##
    e1
  }
)
##________________________________________________________________________________________________________________________

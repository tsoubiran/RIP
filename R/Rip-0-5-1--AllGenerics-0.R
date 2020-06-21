##________________________________________________________________________________________________________________________
##
##
##
##________________________________________________________________________________________________________________________
##
##
## 
##
setClassUnion(".__IPvr__.", c("IPv4", "IPv6", "IP", "IPv4r", "IPv6r", "IPr"))
##
setClassUnion(".__IP__.", c("IPv4", "IPv6", "IP"))
##
setClassUnion(".__IPr__.", c("IPv4r", "IPv6r", "IPr"))
## 
##
##
setClassUnion(".__subscript__.", c("numeric", "integer", "logical"))
## 
setClassUnion(".__intFP__.", c("numeric", "integer"))
##________________________________________________________________________________________________________________________
##
## 
##
##
setGeneric("ipv4", function(object,...){
    standardGeneric("ipv4")
})
##
##
##
setGeneric("ipv4r", function(e1,e2,...){
    standardGeneric("ipv4r")
})
##
##
##
setGeneric("ipv6", function(object,...){
    standardGeneric("ipv6")
})
##
setGeneric("ipv6r", function(e1,e2,...){
    standardGeneric("ipv6r")
})
##
## 
## 
setGeneric("ip", function(e1,e2,...){
    standardGeneric("ip")
})
## 
##
##
setGeneric("ipr", function(e1,e2,...){
    standardGeneric("ipr")
})
##
## 
##
setGeneric("lo", function(e1,...){
    standardGeneric("lo")
})
##
setGeneric("hi", function(e1,...){
    standardGeneric("hi")
})
##
## 
##
setGeneric("ip.version", function(ip,...){
    standardGeneric("ip.version")
})
##
##
##
setGeneric("id", function(x){
  standardGeneric("id")
})
##
setGeneric("id<-", function(x,value){
  standardGeneric("id<-")
})
##
##
##
setGeneric("ip.range", function(ipr) {
    standardGeneric("ip.range")
})
##________________________________________________________________________________________________________________________
##
##
##
##
##
##
setMethod(
  "Ops" ##
  ## 
  , signature(e1 = ".__IPvr__.", e2 = "ANY")
  , function(e1,e2){
    ##
    stop("Ops IPvr : unimplemented method ", .Generic, " for classes ", class(e1), " and ",  class(e2) )
  }
)
##
## 
##
`Summary..__IPvr__.` <- function(...){
    stop("Summary IPvr : unimplemented method ", .Generic, " for classes ", class( as.list(...)[[1]]) )
  }
##
##
##
setMethod(
  "Math" ##
  ## 
  , signature(x= ".__IPvr__.")
  , function(x){
    ##
    stop("Math IPvr : unimplemented method ", .Generic, " for classes ", class(x) )
  }
)
##
##
##
# setMethod(
#   "Math2" ##
#   ## 
#   , signature(e1 = ".__IPvr__.", e2 = "ANY")
#   , function(e1,e2){
#     ##
#     stop("Math2 IPvr : unimplemented method ", .Generic, " for classes ", class(e1), " and ",  class(e2) )
#   }
# )
##
##
## !!! -- TODO : bitw* -- !!!
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
setGeneric("ip.xor", function(e1, e2) {
    standardGeneric("ip.xor")
})
##
## rshift
setGeneric("%>>%", function(e1, e2) {
  standardGeneric("%>>%")
})
## lshift
setGeneric("%<<%", function(e1, e2) {
  standardGeneric("%<<%")
})
##
## 
##
# ##
# setGeneric("netmask", function(n){
#     standardGeneric("netmask")
# })
##
# setGeneric("hostmask", function(n){
#     standardGeneric("hostmask")
# })
##
##
##
setGeneric("host.info", function(host,...){
    standardGeneric("host.info")
})
##________________________________________________________________________________________________________________________


##________________________________________________________________________________________________________________________
##
##
##
##
setGeneric("ip.order", function(x,...){
    standardGeneric("ip.order")
})
##
##
##
if( !isGeneric("match") ){
  ## ,package="base"
  setGeneric("match")
}
##
##
##
setGeneric("ip.match", function(x,table,...){
    standardGeneric("ip.match")
})
##
##
##
setGeneric("ip.index", function(table,...){
    standardGeneric("ip.index")
})
##________________________________________________________________________________________________________________________

##________________________________________________________________________________________________________________________
##
##
##
##________________________________________________________________________________________________________________________
##
##
##
setMethod(
  "["
  ## 
  , signature(x = ".__IPvr__.", i='missing' )
  ##
  , function(x, i, ...) x
)
##
##
##
setMethod(
  "["
  ## 
  , signature(x = ".__IPvr__.", i='character' )
  ##
  , function(x, i, ...) { 
    ##
    if( !is.null(
      nm <- names(x) 
    )){
      ##
      idx <- match(i, nm)
      ##
      return(x[idx])
    }
    ##
    return(
      new(class(x), rep(NA_character_,length(i)))
    )
  }
)
##
##________________________________________________________________________________________________________________________
##
##
## check.cl=T
## 
`IP_concat` <- function(...) {
  ##
  x   <- list(...)
  ##
  cl0 <- class(x[[1]])
  cl <- unlist(lapply(
    x, function(x) class(x)
  ))
  ##
  if( length( neq <- which(cl!=cl0) )>0 ) stop(
    "class mismatch: expected ", cl0, " but also got ", paste(unique(cl[neq]))
  )
  ##
  ip <- switch(
    cl0
    ## ? IPv4
    , IPv4r = ipv4r()
    , IPv6  = ipv6()
    , IPv6r = ipv6r()
    , IP    = ip()
  )
  ##
  nna  <- unlist(lapply(x,function(x) if(length(x)>0) !is.na(x) else NULL))
  ##
  if( length(nna)==0 ) return(ip)
  ##
  ip@.Data       <- cumsum(nna) - 1L
  ip@.Data[!nna] <- NA
  ipSlotname     <- ip.slotname(cl0)## slotnames[cl0] ## 
#   cat(cl0 ,'ipSlotname', ipSlotname, '\n')
  ##
  if( 
    ##
    sum(nna)>0
  ){
    ## IPv4 : c
    slot(ip, ipSlotname)  <-  do.call('rbind', lapply(x,function(x) (slot(x,ipSlotname))))
    ip@length             <- nrow(slot(ip, ipSlotname)) ## ==nnna
  }
  else ip@length <- 0L
  ## names
  nm        <- unlist(lapply(x,function(x) !is.null( x@id )))
  ##
  if( any(nm==T) ){
    ip@id <- unlist(lapply(
       x
       , function(x){
         if( !is.null( x@id ) ) x@id
         else rep("",length(x) )
       }
    ))
  }
  ##
  ip
}
##________________________________________________________________________________________________________________________
##
##
##
IP_uniq <- function(x,...){
  ##
  cl0 <- class(x)
  ##
  ipSlotname     <- ip.slotname(cl0)
  ##
  val <- unique( slot(x, ipSlotname) )
  ##
  ip <- switch(
    cl0
    ## 
    , IPv4  = ipv4()
    , IPv4r = ipv4r()
    , IPv6  = ipv6()
    , IPv6r = ipv6r()
  )
  ##
  na <- any(is.na(x))
  ##
  len <- if( is.vector(val) ) length(val) else nrow(val)
  ##
  ip@.Data              <- if( !na ) 1:len else c(1:len, NA)
  ##
  slot(ip, ipSlotname)  <- val
  ##
  ip@length             <- len + na
  ##
  ip
}
##________________________________________________________________________________________________________________________
##
##
##
##
as.data.frame.ipvr <- function (x, row.names = NULL, optional = FALSE, ..., nm = paste(deparse(substitute(x), 
    width.cutoff = 500L), collapse = " ")) 
{
    force(nm)
    nrows <- length(x)
    if (!(is.null(row.names) || (is.character(row.names) && length(row.names) == 
        nrows))) {
        warning(gettextf("'row.names' is not a character vector of length %d -- omitting it. Will be an error!", 
            nrows), domain = NA)
        row.names <- NULL
    }
    if (is.null(row.names)) {
        if (nrows == 0L) 
            row.names <- character()
        else if (length(row.names <- names(x)) != nrows || anyDuplicated(row.names)) 
            row.names <- .set_row_names(nrows)
    }
#     if (!is.null(names(x))) 
#         names(x) <- NULL
    value <- list(x)
    if (!optional) 
        names(value) <- nm
    structure(value, row.names = row.names, class = "data.frame")
}
##
as.data.frame.IPv4 <- function (x,...){
  as.data.frame.ipvr(x,...)
}
##
as.data.frame.IPv4r <- function (x,...){
  as.data.frame.ipvr(x,...)
}
##
as.data.frame.IPv6 <- function (x,...){
  as.data.frame.ipvr(x,...)
}
##
as.data.frame.IPv6r <- function (x,...){
  as.data.frame.ipvr(x,...)
}
##
as.data.frame.IP <- function (x,...){
  as.data.frame.ipvr(x,...)
}
##
as.data.frame.IPr <- function (x,...){
  as.data.frame.ipvr(x,...)
}
##________________________________________________________________________________________________________________________

##________________________________________________________________________________________________________________________
##
##
##
##
##
##
# setMethod(
#   "ip.index"
#   ## 
#   , signature(table = ".__IPvr__.")
#   ##
#   , function(table,...){
#     ##
#     f <- function(x,nomatch=NA_integer_,...){
#       ##
#       if( (kl <-class(x))=='IPv6' )
#         .Call(
#           ##
#           'Rip_bsearch_ipv6_in_ipv6r_0'
#           , x
#           , table
#           , idx
#           , nomatch
#         )+1
#       else if( kl=='IPv6r' )
#         .Call(
#           ##
#           'Rip_bsearch_ipv6r_in_ipv6r_0'
#           , x
#           , table
#           , idx
#           , nomatch
#         )+1
#       else stop('bsearch not unimplemented for object of class', kl )
#     }
#     ##
#     idx <- order( 
#       ##ipv6(table)[['lo']]  
#       with( 
#         ipv6(table), lo + ( hi - lo ) %>>%1L
#       )
#     ) - 1L
#     ##
#     tb.clname <- if( (kl <-class(table))=='IPv4r' ) "v4r"
#       else if( kl=='IPv6r' ) "v6r"
#       else stop('bsearch not unimplemented for object of class', kl )
#     ##
#     return(
#       f
#     )
#   }
# )
##________________________________________________________________________________________________________________________

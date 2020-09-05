
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
  "initialize"
  ##
  , signature(.Object="IPv4")
  ##
  , function(.Object, ipstring=NULL){
    ##
    if(is.null(ipstring)) return(.Object)
    ##
    if( typeof( ipstring )!='character' ){
      ##
      stop('ip should be of type character')
    }
    ##
    if ( length(ipstring) ){
      #
      .Object <- .Call(
        ##
        'Rip_ipv4_init_0'
        ##
        , .Object
        , ipstring
      )
    }
    ##
    .Object
  }
)
##
##
##
setValidity(
  "IPv4"
  , function(object){
    ##
    if( 
      ##
      xor( null0 <- is.null(object@ipv4),is.null(object@length) )
    ) return( "both slots should NULL or set")
    ##
    if(
      !null0 &  (length(object@ipv4)==object@length )
    ) T
    else{
      paste("unequal length")
    }
  }
)
##
##
##
setMethod(
  "ipv4"
  , signature(object ="missing")
  , function(object,...) new('IPv4', ...)
)
##
setMethod(
  "ipv4"
  , signature(object ="character")
  , function(object,...) new('IPv4', object,...)
)
##
## 
##
setMethod(
  "ipv4"
  , signature(object ="integer")
  , function(object,...){
    ip4 <- new('IPv4', ...)
    if( any( object < 0 , na.rm=T ) ) warning('negative values')
    nna        <- !( na         <- is.na(object)  )
    if( any(nna) ){
      ip4@ipv4   <- object[nna]
      ip4@.Data  <- ifelse( nna, cumsum(nna) -1L, NA )
      ip4@length <- sum(nna)
    }else ip4@.Data <- object
    ##
    if( !is.null(nm<-names(object)) ) ip4@id <- nm
    ##
    ip4
  }
)
##
setMethod(
  "ipv4"
  , signature(object ="logical")
  , function(object,...){
    ipv4(as.integer(object))
  }
)
##
##
##
# setMethod(
#   "ip.version"
#   , "IPv4"
#   , function(ip,...) ifelse(!is.na(ip@.Data), 4L, NA_integer_)
# )
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
##
setMethod(
  "initialize"
  ##
  , signature(.Object="IPv4r") 
  ## 
  , function(.Object, ipstring=NULL, ipv4=NULL, lo=NULL,hi=NULL,nip=NULL){
    ##
    if(is.null(ipstring)){
      ##
      if(
        class( lo )=='IPv4' & ( class( nip  ) %in% c('integer','numeric') )
      ){
        ##
        hi <- lo + nip
        ## names
        
      } else if( !is.null(ipv4) ) {
        lo <- ipv4[[1]] 
        hi <- ipv4[[2]] 
      }
      ##
      if( 
        (
          class( lo )=='IPv4' & class( hi  )=='IPv4'
        ) 
      ){
        ##
        if( length(lo@.Data)!=length(hi@.Data) ) stop( "ipv4s should have the same length")
        ##
        if( any( lo@.Data!=hi@.Data, na.rm = T ) ) stop( "ipv4s should have the same NA")
        ##
        if( any(lo > hi, na.rm = T) ) stop( "lo > hi")
        ##
        ##
        ## !!! nna !!! -see: IPv6r
        ##
        .Object@.Data  <- lo@.Data
        ##
        ## 
        if( !is.null(lo@ipv4) | !is.null(lo@ipv4 )){ 
          ##
          .Object@ipr    <- matrix(c(lo@ipv4,hi@ipv4),ncol=2)
          ##
          .Object@length <- nrow(.Object@ipr)
        }
        else .Object@length <- 0L
        ##
        if(!is.null(lo@id) ) .Object@id <- lo@id
      }
      ## ?else: err -ie: manque un argument
      ##
      return(.Object)
    }
    ##
    if( typeof( ipstring )!='character' ){
      ##
      stop('ip should be of type character')
    }
    ##
    if ( !length(ipstring) ){
      ##
      return(.Object)
    }
    ##
    return(.Call(
      ##
      'Rip_ipv4r_init_0', .Object, ipstring
      ##'Rip_ipv4r_input_init_0', ipstring##, .Object
    ))
  }
)
## !!! TODO !!!
# setValidity(
#   "IPv4"
#   , function(object){
#   }
# )
##
##
##
##
setMethod(
  "ipv4r"
  , signature(e1="character",e2="missing")
  , function(e1,e2,...) new('IPv4r', e1,...)
)
##
setMethod(
  "ipv4r"
  , signature(e1 = "IPv4", e2 = "IPv4")
  , function(e1,e2,...) new('IPv4r', lo=e1,hi=e2,...)
)
##
setMethod(
  "ipv4r"
  , signature(e1 = "IPv4", e2 = ".__intFP__.")
  , function(e1,e2,...) new('IPv4r', lo=e1,nip=e2,...)
)
##
setMethod(
  "ipv4r"
  , signature(e1="list",e2="missing")
  , function(e1,...) new('IPv4r', ipv4=e1,...)
)
##
setMethod(
  "ipv4r"
  , signature(e1="missing",e2="missing")
  , function(e1,e2,...) new('IPv4r', ...)
)
##  
##  
##
setMethod(
  "lo"
  , "IPv4r"
  ##
  , function(e1,...){
    ##
    lo <- ipv4()
    ##
    lo@.Data <- e1@.Data
    ##
    lo@ipv4 <- e1@ipr[,1] ## matrix(e1@ipr,ncol=2) ## au cas où
    ##
    if( length(e1@id) ){
      lo@id  <- e1@id
    }
    ##
    lo@length <- e1@length
    ##
    if( !is.null(e1@id) ){
      lo@id <- e1@id
    }
    ##
    lo
  }
)
##  
##  
##
setMethod(
  "hi"
  , "IPv4r"
  ##
  , function(e1,...){
    ##
    hi <- ipv4()
    ##
    hi@.Data <- e1@.Data
    ##
    hi@ipv4 <- e1@ipr[,2] ## matrix(e1@ipr[,2],ncol=2) ## au cas où
    ##
    if( length(e1@id) ){
      hi@id  <- e1@id
    }
    ##
    hi@length <- e1@length
    ##
    if( !is.null(e1@id) ){
      hi@id <- e1@id
    }
    ##
    hi
  }
)
##
## 
##
setMethod(
  "ipv4"
  , "IPv4r"
  , function(object,...){
    ##
    ip1 <-ip2 <- ipv4()
    ##
    ip1@.Data <- ip2@.Data <- object@.Data
    ##
    ip1@ipv4 <- object@ipr[,1] ## matrix(object@ipr,ncol=2)[,1]
    ip2@ipv4 <- object@ipr[,2] ## matrix(object@ipr,ncol=2)[,2]
    ##
    if( !is.null(object@id) ){
      ip1@id <- ip2@id <- object@id
    }
    ##
    ip1@length <- ip2@length <- object@length
    ##
    list(lo=ip1,hi=ip2)
  }
)
  
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
  "length"
  , "IPv4"
  , function(x) length(x@.Data)
)
##
##
##
##
setMethod(
  "is.numeric"
  ## 
  , signature(x = "IPv4")
  , function(x) FALSE
)
##
##
##
setMethod(
  "is.na"
  , "IPv4"
  , function(x) is.na(x@.Data)
)
##
##
##
setMethod(
  "print"
  , "IPv4"
  , function(x,...){
      ##
      ip.strings <- .Call("Rip_ipv4_as_character_0",x)
      ##names(ip.strings) <- names(x)
      print(ip.strings)
      x
  }
)
##
setMethod(
  "show"
  , "IPv4"
  , function(object){
    ##
    print(object)
    invisible()
  }
)
##
## 
##
format.IPv4 <- function(x
  , trim = FALSE, digits = NULL, nsmall =0L, justify = c("left","right", "centre", "none"), width = NULL, na.encode = TRUE,scientific = NA, big.mark = "", big.interval = 3L
  , small.mark = "", small.interval = 5L, decimal.mark = ".", zero.print = NULL, drop0trailing = FALSE
  , ...) .Call("Rip_ipv4_as_character_0", x) ## as.character(x,...)
##
toString.IPv4 <- function(x,...) .Call("Rip_ipv4_as_character_0", x) ##as.character(x,...)
##
##
##
##
setMethod(
  "as.character"
  , "IPv4"
  , function(x) .Call("Rip_ipv4_as_character_0", x)
)
## utile ?
setAs("IPv4", "character", function(from) .Call("Rip_ipv4_as_character_0", from)) 
##
##
##
setMethod(
  "as.numeric"
  , "IPv4"
  , function(x) .Call("Rip_ipv4_cvtfl64_0", x)
)
##
## table <- factor <- unique + match
##
unique.IPv4 <- function(
  x,...
){
  ##
  htb.sz  <- as.integer(length(x)*1.5)+1L
  ##
  idx <- .Call("Rip_h_ipv4_hash_0_0", x, c(htb.sz = htb.sz, M1 = htb.sz, M2 = 7L))
  ##
  x[idx]
}
##________________________________________________________________________________________________________________________
##
##
##
setMethod(
  "length"
  , "IPv4r"
  , function(x) length(x@.Data)
)
##
##
##
setMethod(
  "is.na"
  , "IPv4r"
  , function(x) is.na(x@.Data)
)
##
##
##
setMethod(
  "is.numeric"
  ## 
  , signature(x = "IPv4r")
  , function(x) FALSE
)
##
##
##
##
setMethod(
  "print"
  , "IPv4r"
  , function(x,...){
      ##
      ip.strings <- .Call("Rip_ipv4r_as_character_0",x)
      ##
      print(ip.strings)
      ##
      x
  }
)
##
setMethod(
  "show"
  , "IPv4r"
  , function(object){
    ##
    print(object)
    ##
    invisible()
  }
)
##
## 
##
format.IPv4r <- function(x
  , trim = FALSE, digits = NULL, nsmall =0L, justify = c("left","right", "centre", "none"), width = NULL, na.encode = TRUE,scientific = NA, big.mark = "", big.interval = 3L
  , small.mark = "", small.interval = 5L, decimal.mark = ".", zero.print = NULL, drop0trailing = FALSE
  , ...) .Call("Rip_ipv4r_as_character_0", x) ## 
##
toString.IPv4r <- function(x,...) .Call("Rip_ipv4r_as_character_0", x) ##
##
##
##
##
setMethod(
  "as.character"
  , "IPv4r"
  , function(x) .Call("Rip_ipv4r_as_character_0", x)
)
##
##
##
##
setMethod(
  "as.numeric"
  , "IPv4r"
  , function(x){
    ##
    .Call('Rip_ipv4r_cvtfl64_0', x)
  }
)
##
## table <- factor <- unique + match
##
## !!!TODO!!! hash
##
unique.IPv4r <- function(
  x,...
){
  ##
  new('IPv4r', unique(.Call("Rip_ipv4r_as_character_0", x)) )
}
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
  , signature(x = "IPv4", i='.__subscript__.' )
  ##
  , function(x, i, ...) {  
    ##
    ip <- new("IPv4")
    ##
    ip@.Data <- x@.Data[i]
    ##
    nna <- !is.na(ip@.Data)
    ## print
    ip@ipv4 <- x@ipv4[ 
      (ip@.Data[
        which( nna )
      ]+1)
    ] 
    ## re-idx
    idx           <- cumsum(nna) - 1L
    ip@.Data[nna] <- idx[nna]
    ##
    if( !is.null(x@id) ) ip@id <- x@id[i]
    ##
    ip@length <-length(ip@ipv4)
    ##
    ip
  }
)
##
## !!!fix pour all(value==NA)) dans polelec-apache
##
## !!!idx<=0
##
setMethod(
  "[<-"
  , "IPv4"
  , function (x, i, j, ..., value){
    ##
    if( class(value)!='IPv4' ) value <- ipv4(value) ## 
    ## xpd
    ipv4    <- x@ipv4[x@.Data+1]
    ##
    v.na <- is.na(value)
    ##
    if (all(v.na==T)){
      x@.Data[i] <- NA
    }else{
      ## grow matrix if necesary
      if( 
        ( d <- max(i) - length(ipv4) )>0
      ){
        ipv4 <- c(ipv4, rep(NA_integer_, length(ipv4)+d, ncol=2))
      }
      ##
      ipv4[i] <- value@ipv4[value@.Data+1]
      ##
      x@.Data[i] <- value@.Data
    }
    ## re-idx
    nna          <- !is.na(x@.Data)
    ##
    x@ipv4       <- ipv4[ nna ]
    ##
    idx          <- cumsum(nna) - 1L
    ##
    x@.Data[nna] <- idx[nna]
    ##
    x@length     <- length(x@ipv4)
    ##
    ## !!!
    ## replace_setId
    x <- IP_setId_replace(x,i,value)
    ##
    x
  }
)
##
## NAMES (infra)
##
##____________________________________________________________________________________________________________________
##
##
##
setMethod(
  "rbind2"
  , signature(x = "IPv4", y="IPv4")
  , function(x, y, ...){
    ##
    x@.Data  <- c( x@.Data, y@.Data+length( x@ipv4 ) )
    ##
    x@ipv4   <- c(x@ipv4, y@ipv4)
    ##
    if( !is.null( x@id ) ){
      ##
      if( !is.null( y@id ) ) x@id <- c( x@id , y@id  )
      else x@id <- c( x@id , rep("",length(y) ) )
    }else if( !is.null( y@id ) ) x@id <- c( rep("",length(x) ) , y@id  )
    ##
    x@length <- length(x@ipv4)
    ##
    x
  }
)
##____________________________________________________________________________________________________________________
##
##
##
##
##
setMethod(
  "id"
  , signature(x = "IPv4")
  , function(x)  ip.get.id (x)
)
##
setMethod(
  "id<-"
  , signature(x = "IPv4")
  , function(x,value) ip.set.id(x,value) 
)
##
names.IPv4 <- function(x) ip.get.id(x)
##
'names<-.IPv4' <- function(x,value){ 
  ip.set.id(x,value)
}
##________________________________________________________________________________________________________________________
##
## 
## 
setMethod(
  "["
  ## , i='.__subscript__.' 
  , signature(x = "IPv4r")
  ##
  , function(x, i, ...) {  
    ##
    ip <- new("IPv4r")
    ##
    ip@.Data <- x@.Data[i]
    ##
    nna <- !is.na(ip@.Data)
    ##
    idx <-c(
      ip@.Data[
        which( nna )
      ]
      , ip@.Data[
        which( nna )
      ]+ x@length ##
    )+1
    ## 
    ip@ipr <- matrix(
      x@ipr[idx]
      , ncol=2
    )
    ## 
#     ip@ipr <- matrix(
#       x@ipr
#       , ncol=2
#     )[ip@.Data[nna]+1,]
    ##
#     print( ip@ipr )
#     ##
#     idx <-ip@.Data[
#         which( nna )
#     ]+1
#     ##
#     print(idx)
#     ##
#     ip@ipr <- print(matrix(matrix(x@ipr, ncol=2)[idx,], ncol=2))
    ## re-idx
    idx           <- cumsum(nna) - 1L
    ip@.Data[nna] <- idx[nna]
    ##
    if( !is.null(x@id) ) ip@id <- x@id[i]
    ##
    ip@length <-nrow(ip@ipr)
    ##
    ip
  }
)
##
## 
## 
# setMethod(
#   "[<-"
#   ## , i='subset.idx' 
#   , signature(x = "IPv4r")
#   ##
#   , function(x, i, ...) {  
#   }
# )
##
## ??? fix nna ???
##
setMethod(
  "[<-"
  , "IPv4r"
  , function (x, i, j, ..., value){
    ##
    if( class(value)!='IPv4r' ) value <- ipv4r(value) 
    ##
    ## xpd
    ##
    ipr     <- as.matrix(x@ipr, ncol=2)[x@.Data+1,]
    ##
    v.na <- is.na(value)
    ##
    if (all(v.na==T)){
      ##
      x@.Data[i] <- NA
      
    }else{
      ## 
      ## replace
      ##
      if( 
        ( d <- max(i) - nrow(ipr) )>0
      ){
        ipr <- rbind(ipr, matrix(NA_integer_, nrow(ipr)+d, ncol=2))
      }
      ##
      ipr[i,] <- as.matrix(value@ipr, ncol=2)[value@.Data+1,]
      ## 
      ## cp
      ##
      x@.Data[i]   <- value@.Data
    }
    ## re-idx
    nna          <- !is.na(x@.Data)
    ##
    x@ipr        <- matrix(ipr[which(nna),],ncol=2)
    ##
    idx          <- cumsum(nna) - 1L
    x@.Data[nna] <- idx[nna]
    ##
    x@length     <- nrow(x@ipr)
    ##
    ##
    x <- IP_setId_replace(x,i,value)
    ##
    x
  }
)
##
##
##
##
setMethod(
  "id"
  , signature(x = "IPv4r")
  , function(x) ip.get.id (x)
)
##
setMethod(
  "id<-"
  , signature(x = "IPv4r")
  , function(x,value) ip.set.id(x,value) 
)
##
##
##
names.IPv4r <- function(x) ip.get.id(x)
##
'names<-.IPv4r' <- function(x,value){ 
  ip.set.id(x,value)
}
##____________________________________________________________________________________________________________________


##________________________________________________________________________________________________________________________##
## 
##
## 
##
`c.IPv4` <- function(...) {
  ##
  x   <- list(...)
  ##
  cl <- unlist(lapply(
    x, function(x) class(x)
  ))
  ## s3 dispatches only on 1st arg
  if( length( neq <- which(cl!="IPv4") )>0 ) stop(
    "class mismatch: expected ", "IPv4", " but also got ", paste(unique(cl[neq]))
  )
  ##
  nna  <- unlist(lapply(x,function(x) if(length(x)>0) !is.na(x) else NULL))
  ##
  if( length(nna)==0 ) return(ipv4())
  ##
  ip4             <- ipv4()
  ip4@.Data       <- cumsum(nna) - 1L
  ip4@.Data[!nna] <- NA
  ##
  if( sum(nna)>0 ){
    ##
    ip4@ipv4   <- do.call('c', lapply(x,function(x) x@ipv4))
    ip4@length <- length(ip4@ipv4)
  }
  else ip4@length <- 0L
  ## names
  nm        <- unlist(lapply(x,function(x) !is.null( x@id )))
  ##
  if( any(nm==T) ){
    ip4@id <- unlist(lapply(
       x
       , function(x){
         if( !is.null( x@id ) ) x@id
         else rep("",length(x) )
       }
    ))
  }
  ##
  ip4
}
##
`c.IPv4r` <- function(...) IP_concat(...)
##
## union: IPv4r,IPv6r
##
setMethod(
  "rbind2"
  , signature(x = "IPv4r", y="IPv4r")
  , function(x, y, ...){
    ## print("bind")
    ##
    x@id <- if( !is.null( x@id ) ){
      ##
      if( !is.null( y@id ) )     c( x@id , y@id  )
      else                       c( x@id , rep("",length(y) ) )
    }else if( !is.null( y@id ) ) c( rep("",length(x) ), y@id  )
    ##
    x@.Data  <- c( x@.Data, y@.Data+x@length )
    ## 
    x@ipr    <- if( !is.null( x@ipr ) ){
      ##
      if( !is.null( y@ipr ) )     rbind(x@ipr, y@ipr)
      else                        x@ipr
    }else if( !is.null( y@ipr ) ) y@ipr
    ##
    x@length <- nrow(x@ipr)
    ##
    x
  }
)
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
  "=="
  , signature(e1 = "IPv4", e2 = "IPv4")
  ##
  , function(e1,e2){
    .Call("Rip_ipv4_op2_bool_eq_0", e1, e2 )
  }
)
##
##
##
setMethod(
  "!="
  , signature(e1 = "IPv4", e2 = "IPv4")
  ##
  , function(e1,e2){
    .Call("Rip_ipv4_op2_bool_neq_0", e1, e2 )
  }
)
##
##
##
setMethod(
  "<"
  , signature(e1 = "IPv4", e2 = "IPv4")
  ## 
  , function(e1,e2){
    .Call("Rip_ipv4_op2_bool_lt_0", e1, e2 )
  }
)
##
##
##
setMethod(
  "<="
  , signature(e1 = "IPv4", e2 = "IPv4")
  ## 
  , function(e1,e2){
    .Call("Rip_ipv4_op2_bool_le_0", e1, e2 )
  }
)
##
##
##
setMethod(
  ">"
  , signature(e1 = "IPv4", e2 = "IPv4")
  ## 
  , function(e1,e2){
    .Call("Rip_ipv4_op2_bool_gt_0", e1, e2 )
  }
)
##
##
##
setMethod(
  ">="
  , signature(e1 = "IPv4", e2 = "IPv4")
  ## 
  , function(e1,e2){
    .Call("Rip_ipv4_op2_bool_ge_0", e1, e2 )
  }
)
##________________________________________________________________________________________________________________________
##
##
##
##
setMethod(
  "=="
  , signature(e1 = "IPv4r", e2 = "IPv4r")
  ## 
  , function(e1,e2){
    .Call("Rip_ipv4r_op2_bool_eq_0", e1, e2 )
  }
)
## 
##
##
setMethod(
  "!="
  , signature(e1 = "IPv4r", e2 = "IPv4r")
  ## 
  , function(e1,e2){
    .Call("Rip_ipv4r_op2_bool_neq_0", e1, e2 )
  }
)
## 
## intersection
## intersects : Ripaddr_ipv4r_cmp_intersects
##
## setdiff,setequal
## union
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
setMethod(
  "+"
  , signature(e1 = "IPv4", e2 = "integer")
  ## 
  , function(e1,e2){
    .Call(
      ##
      "Rip_ipv4_op2_arith_add32_0" ## "Rip_ipv4_op2_arith_add32_1"
      , e1, e2
    )
  }
)
##
##
##
setMethod(
  "+"
  , signature(e1 = "IPv4", e2 = "numeric")
  ## 
  , function(e1,e2){
    .Call(
      "Rip_ipv4_op2_arith_addfl64_0"
      , e1, e2

    )
  }
)
##
##
##
setMethod(
  "+"
  , signature(e1 = "IPv4", e2 = "IPv4")
  ## 
  , function(e1,e2){
    .Call(
      "Rip_ipv4_op2_arith_addv4_0"
      , e1, e2
    )
  }
)
##
##
##
setMethod(
  "-"
  , signature(e1 = "IPv4", e2='missing')
  ## 
  , function(e1){
    .Call(
      "Rip_ipv4_op1_arith_neg_0"
      , e1
    )
  }
)
##
##
##
setMethod(
  "-"
  , signature(e1 = "IPv4", e2 = "integer")
  ## 
  , function(e1,e2){
    .Call(
      "Rip_ipv4_op2_arith_sub32_0"
      , e1, e2
    )
  }
)
##
##
##
setMethod(
  "-"
  , signature(e1 = "IPv4", e2 = "numeric")
  ## 
  , function(e1,e2){
    .Call(
      "Rip_ipv4_op2_arith_subfl64_0"
      , e1, e2
    )
  }
)
##
##
##
setMethod(
  "-"
  , signature(e1 = "IPv4", e2 = "IPv4")
  ## 
  , function(e1,e2){
    .Call(
      "Rip_ipv4_op2_arith_subv4_0"
      , e1, e2
    )
  }
)
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
  "&"
  , signature(e1 = "IPv4", e2 = "IPv4")
  ## 
  , function(e1,e2){
    .Call(
      "Rip_ipv4_op2_mask_and_0"
      , e1, e2
    )
  }
)
##
setMethod(
  "|"
  , signature(e1 = "IPv4", e2 = "IPv4")
  ## 
  , function(e1,e2){
    .Call(
      "Rip_ipv4_op2_mask_or_0"
      , e1, e2
    )
  }
)
## 
setMethod(
  "!"
  , signature(x = "IPv4")
  ## 
  , function(x){
    ##
    x@ipv4 <- bitwNot(x@ipv4)
    ##
    x
  }
)
##
##
##
setMethod(
  "ip.xor"
  , signature(e1 = "IPv4", e2 = "IPv4")
  ##
  , function(e1, e2){
    .Call(
      "Rip_ipv4_op2_mask_xor_0"
      , e1, e2
    )
  }
)
##
setMethod(
  "^"
  , signature(e1 = "IPv4", e2 = "IPv4")
  , function(e1, e2){
    .Call(
      "Rip_ipv4_op2_mask_xor_0"
      , e1, e2
    )
  }
)
##
##
##
setMethod(
  "%>>%"
  , signature(e1='IPv4', e2='integer')
  , function(e1, e2){
    .Call(
      "Rip_ipv4_op2_arith_rshift_0"
      , e1, e2
    )
  }
)
##
setMethod(
  "%<<%"
  , signature(e1='IPv4', e2='integer')
  , function(e1, e2){
    .Call(
      "Rip_ipv4_op2_arith_lshift_0"
      , e1, e2
    )
  }
)
##
## 
## 
##
ipv4.netmask <- function(n){
  ##
  n <- as.integer(n)
  ##
  .Call(
    "Rip_ipv4_mask_netmask_0"
    , n
  )
}
##
ipv4.hostmask <- function(n){
  ##
  n <- as.integer(n)
  ##
  .Call(
    "Rip_ipv4_mask_hostmask_0"
    , n
  )
}
##________________________________________________________________________________________________________________________


##________________________________________________________________________________________________________________________
##
##
##
##________________________________________________________________________________________________________________________
##
## TODO : getGroupMembers("Summary")
## "max"   "min"   "range" "prod"  "sum"   "any"   "all" 
##
## 
## na.rm = T ?
## 
setMethod(
  "ip.range"
  ## 
  , "IPv4r"
  , function(ipr){
     .Call('Rip_ipv4r_range_0', ipr)
  }
)
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
  "ip.order"
  ## 
  , "IPv4"
  ## method 
  , function(x, na.last = TRUE, decreasing = FALSE){
    ##    
    idx <- .Call(
      ##
      "Rip_ipv4_qsort0"
      , x[ !(naidx <- is.na(x)) ]
      , decreasing ## 
    )+1L
    ##
    if( is.na(na.last) ) idx 
    else{
      idx <- ((1:length(x))[!naidx])[idx]
      if(na.last)       c(idx         , which(naidx) )
      else if(!na.last) c(which(naidx), idx          )
    }
    ##
  }
)
## 
## !!!TODO
##
# setMethod(
#   "ip.order"
#   ## 
#   , "IPv4r"
#   ## method 
#   , function(x, na.last = TRUE, decreasing = FALSE){
#   }
# )
##________________________________________________________________________________________________________________________
##
## 
## 
##
setMethod(
  "xtfrm"
  ## 
  , "IPv4"
  , function(x){
    ##
     .Call( 'Rip_ipv4_cvtfl64_0', x)
  }
)
##________________________________________________________________________________________________________________________
##
## 
## 
##
setMethod(
  "xtfrm"
  ## 
  , "IPv4r"
  , function(x){
     ##
     .Call( 'Rip_ipv4_cvtfl64_0', lo(x))
  }
)
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
  "match"
  ## 
  , signature(x = "IPv4", table = "IPv4", nomatch='ANY', incomparables='ANY')
  ##
  , function(x,table,nomatch=NA_integer_, incomparables = NULL){
    ##cat("ip4 match\n")  
    ## 
    match(
      .Call( 'Rip_ipv4_cvtfl64_0', x)
      , .Call( 'Rip_ipv4_cvtfl64_0', table )
      , nomatch=nomatch, incomparables=incomparables
    )
  }
)
##
##
##
setMethod(
  "ip.match"
  ## 
  , signature(x = "IPv4", table = "IPv4")
  ##
  , function(x,table,nomatch=NA_integer_, incomparables = NULL){
    ##
    ##
    if( is.null(attr(table@ipv4,"htb")) ){
      ##
      htb.sz <- .Call("nextPrime_MillerRabin", as.integer(length(table)*2))
      ##
      .Call("Rip_h_ipv4_hash_0_0", table, c(htb.sz = htb.sz, M1 = htb.sz, M2 = 7L))
    }
    ##
    .Call("Rip_h_ipv4_lookup_0_0", x, table)
  }
)
## 
##
## "ip.hash<-"
##
##
##
setMethod(
  "ip.index"
  ## 
  , signature(table = "IPv4r")
  ##
  , function(table,...){
    ##
    bsearch <- function(x=NULL,nomatch=NA_integer_,value=F,...){
      ##
      if( is.null(x) ) x <- table
      ##
      ## "polymorphisme"
      ##
      x.clnm <- if( ( kl <-class(x)) %in% c('IPv4', 'IPv4r' ) ){
        tolower(kl)
      }else stop('bsearch not implemented for object of class ', kl , ' and table ', class(table))
      ##
      midx <- .Call(
          ## "dispatch"
          sprintf('Rip_bsearch_%s_in_%s_0', x.clnm, tb.clnm)
          ##
          , x 
          , table
          , idx
          , nomatch
      )+1L
      ##
      if( value ) return(table[midx])
      ##
      midx
    }
    ##
    table <- table[!is.na(table)]
    ##
    tb.clnm <- tolower(class(table)) ##'ipv4r'
    ## ip.
    idx <- order( 
      ##
      ipv4(table )[['lo']] 
      ##
      , na.last= NA
    ) - 1L
    ##
    return(
      bsearch
    )
  }
)
##
setMethod(
  "match"
  ## 
  , signature(x = "IPv4", table = "IPv4r", nomatch='ANY', incomparables='ANY')
  ##
  , function(x,table,nomatch=NA_integer_, incomparables = NULL){
    ##
    ip.index(table)(x,nomatch=nomatch, incomparables = incomparables)
  }
)
##
setMethod(
  "ip.match"
  ## 
  , signature(x = "IPv4", table = "IPv4r")
  ##
  , function(x,table,nomatch=NA_integer_, incomparables = NULL){
    ##
    ip.index(table)(x, nomatch=nomatch, incomparables = incomparables )
  }
)
##
setMethod(
  "match"
  ## 
  , signature(x = "IPv4r", table = "IPv4r", nomatch='ANY', incomparables='ANY')
  ##
  , function(x,table,nomatch=NA_integer_, incomparables = NULL){
    ##
    match(as.character(x), as.character(table),  nomatch, incomparables)
  }
)
##
setMethod(
  "ip.match"
  ## 
  , signature(x = "IPv4r", table = "IPv4r")
  ##
  , function(x,table,nomatch=NA_integer_, incomparables = NULL){
    ##
    if( is.null(attr(table@ipr,"htb")) ){
      htb.sz  <- as.integer(length(table)*1.5)+1L
      ## Rip_h_ipv4r_hash_0_0
      .Call("Rip_h_ipv4r_h64dblh_lemire_hash_0_0", table, c(htb.sz = htb.sz, M2 = 7L))
    }
    ## Rip_h_ipv4r_lookup_0_0
    .Call("Rip_h_ipv4r_h64dblh_lemire_lookup_0_0", x, table)
  }
)
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
ipv4.addr.space <- function() ipv4.addrspace.ipr
## 
ipv4.reserved <- function() ipv4.reserved.ipr
##
ipv4.rir <- function() ipv4.addrspace.ipr[(ipv4.addrspace.ipr@id %in% rir.names)]
##
ipv4.recovered <- function() ipv4.recovered.ipr
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

##________________________________________________________________________________________________________________________

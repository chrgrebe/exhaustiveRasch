### as soon as the pairwise package exports its residuals.pers and logLik.pers
### functions, these functions will be removed from the package.

expscore <- function(pers_obj, na_treat=NA){
  # function adapted from the package pairwise by Joerg-Henrik Heine

  #' returns a matrix with dims like resp matrix with expected scores and more ...
  #' func. by joerg-henrik heine jhheine(at)googlemail.com
  #' needs func. \code{pvx} in i.pvx.R and \code{pvx.matrix} in i.pvx.matrix.R
  #' Notation and formulas see:  Wright & Masters 1982 p.100
  #' in a revised form  (korrigendum) see http://www.rasch.org/rmt/rmt34e.htm
  #' @param pers_obj an object of class "pers"
  #' @param na_treat internal
  #' @return internal
  #' @export
  #' @keywords internal
  nitems <- dim(pers_obj$pair$threshold)[1]
  emp_resp <- pers_obj$pair$resp # empirical responses
  N <- dim(pers_obj$pair$resp)[1]

  # Expected Mean Eni (of xni)-----------------
  foo1 <- function(theta, pers_obj=pers_obj, nitems=nitems){
    #theta: ein theta wert
    catL <- lapply(1:nitems,function(x){(0:length(stats::na.omit(pers_obj$pair$threshold[x,])))})
    pL <- lapply(1:nitems,function(x){pvx(theta=theta,thres=stats::na.omit(pers_obj$pair$threshold[x,]))})
    exp_scor_theta <- mapply(FUN=function(o1,o2){sum(o1*o2)}, o1=catL, o2=pL)
    return(exp_scor_theta)
  }
  Eni <- t(sapply(pers_obj$pers$WLE, foo1, pers_obj ,nitems))
  dimnames(Eni) <- dimnames(emp_resp)
  ### replace cells missing in emp_resp by NA in Eni new approach!----
  # affects Wni and Cni and all the other _ni's too
  Eni[is.na(emp_resp)] <- NA
  # Eni[is.na(emp_resp)] <- 0 # nicht nötig passiert weiter unten

  # matplot(Eni[order(pers_obj$pers$WLE),],type="l") # ggf. noch machen
  # return(Eni)

  ## Variance Wni (of xni)--------
  foo2 <- function(i, pers_obj=pers_obj, Eni=Eni){
    theta_v <- pers_obj$pers$WLE # empirical theta vector
    thres <- stats::na.omit(pers_obj$pair$threshold[i,]) # thurst. thresholds item i
    kat <- 0:length(stats::na.omit(pers_obj$pair$threshold[i,])) # categories item i
    xm_v <- pers_obj$pair$resp[,i]+1  # responses item i | +1 ist wichtig es heißt 1 und 2 kategorie nicht 0 und 1
    pnik <- (pvx.matrix(theta_v,thres)); dimnames(pnik)[[2]] <- names(xm_v)
    Lpnik <- as.list(data.frame(t(pnik)))
    Leni <- lapply(kat, function(x){  (x-Eni[,i])^2  })# Eni[,i] # expected scores item i
    wni <- rowSums(mapply(function(p,e){e*p}, p=Lpnik, e=Leni,SIMPLIFY = TRUE))
    return(wni)
  }
  Wni <- sapply(1:nitems, foo2, pers_obj, Eni)
  dimnames(Wni) <- dimnames(emp_resp)
  # return(Wni)
  Wni[is.na(emp_resp)] <- na_treat # new

  ## Kurtosis Cni (of xni)--------
  foo3 <- function(i, pers_obj=pers_obj, Eni=Eni){
    theta_v <- pers_obj$pers$WLE # empirical theta vector
    thres <- stats::na.omit(pers_obj$pair$threshold[i,]) # thurst. thresholds item i
    kat <- 0:length(stats::na.omit(pers_obj$pair$threshold[i,])) # categories item i
    xm_v <- pers_obj$pair$resp[,i]+1  # responses item i | +1 ist wichtig es heißt 1 und 2 kategorie nicht 0 und 1
    pnik <- (pvx.matrix(theta_v,thres)); dimnames(pnik)[[2]] <- names(xm_v)
    Lpnik <- as.list(data.frame(t(pnik)))
    Leni <- lapply(kat, function(x){  (x-Eni[,i])^4  })# Eni[,i] # expected scores item i
    cni <- rowSums(mapply(function(p,e){e*p}, p=Lpnik, e=Leni,SIMPLIFY = TRUE))
    return(cni)
  }
  Cni <- sapply(1:nitems, foo3, pers_obj, Eni)
  dimnames(Cni) <- dimnames(emp_resp)
  # return(Eni)

  ###############------------------------------------------------------------

  ## Score Residual  Yni--------
  Yni <- emp_resp-Eni
  Yni[is.na(emp_resp)] <- na_treat

  ## Standardised Residual  Zni--------
  Zni <- Yni / sqrt(Wni)
  Zni[is.na(emp_resp)] <- na_treat

  ## Score Residual Squared Y2ni--------
  Y2ni <- Wni* (Zni)^2

  ## Standardised Residual Squared Z2ni--------
  Z2ni <- (Zni)^2

  return(list(Eni=Eni, Wni=Wni, Cni=Cni, Yni=Yni, Zni=Zni, Y2ni=Y2ni, Z2ni=Z2ni))
}



########################### hier die residual method fuer pers #############################
residuals.pers<-function(object, res="sr", na_treat=0, ...){
  # function adapted from the package pairwise by Joerg-Henrik Heine

  #' @title S3 residuals for Object of class "pers"
  #' @exportS3Method residuals pers
  #' @keywords methods
  #' @method residuals pers
  #' @description S3 residuals method to extract the (Rasch) residuals for object of class\code{"pers"}
  #' @param object object of class\code{"pers"}
  #' @param res a character string defining which type of (rasch–) residual to return. This must be (exactly) one of the strings "exp" for expected scores "sr" for score residuals (default), "stdr" for standardised residuals, "srsq" for score residuals squared, or "stdrsq" for standardised residuals squared. The default is set to res="sr".
  #' @param na_treat value to be assigned to residual cells which have missing data in the original response matrix. Default is set to na_treat=0 to set the residuals to 0, which implys that they are imputed as 'fitting data', i.e., zero residuals. This can attenuate contrasts (see. http://www.rasch.org/rmt/rmt142m.htm). An option is to set it to na_treat=NA.
  #' @param ... not used jet.
  #' @keywords internal

  obj <- expscore(pers_obj=object, na_treat=na_treat) # calls internal function for residuals
  Eni <- obj$Eni # expected scores
  Yni <- obj$Yni # "sr" - score residual
  Zni <- obj$Zni # "stdr" - standardised residual
  Y2ni <- obj$Y2ni # "srsq" - score Residual Squared
  Z2ni <- obj$Z2ni # "stdrsq" - standardised residual squared
  # check of arguments
  if( !(any(res==c("exp","sr","stdr","srsq","stdrsq"))) ){stop("wrong type of residuals selected","\n", "check argument 'res'","\n")}
  #assign the selected residual type
  if(res=="exp"){retur <- Eni}
  if(res=="sr"){retur <- Yni}
  if(res=="stdr"){retur <- Zni}
  if(res=="srsq"){retur <- Y2ni}
  if(res=="stdrsq"){retur <- Z2ni}
  class(retur) <- c("residuals","matrix")
  return(retur)
}


pvx.super <- function(theta_v, thres=NULL, dat=NULL){
  # function adapted from the package pairwise by Joerg-Henrik Heine

  # func. by joerg-henrik heine jhheine(at)googlemail.com
  # theta_v: ein vector oder zahl; oder ein pers_obj
  # thres_m: matrix thurstonian thresholds der items ggf sind NAs drin
  # dat : benutz daten um die jeweilige P der gewählten antwort auszugeben
  # funct. needs pvx.matrix in i.pvx.matrix.R

  #' @title internal pvx.super function"
  #' @description internal
  #' @param theta_v internal
  #' @param thres internal
  #' @param dat internal
  #' @keywords internal
  #' @export

  resp <- NULL

  if(any( (class(theta_v)=="pers") & (is.null(thres)) ) ){
    ## wenn nur pers_obj übergeben wird -- OK
    resp  <- theta_v$pair$resp
    thres <- (theta_v$pair$threshold)
    namen <- rownames(theta_v$pair$resp)
    theta_v <- (theta_v$pers$WLE)
    names(theta_v) <- namen

  }
  if(any( (class(theta_v)=="pers") & (!is.null(thres)) ) ){
    ## wenn pers_obj übergeben wird  mit separatem threshold
    resp  <- theta_v$pair$resp
    thres <- thres
    theta_v <- (theta_v$pers$WLE)
  }

  # hier könnte man noch weiter bauen 21.11.2015
  #   if((class(theta_v)=="numeric") & (length(thres)!=0) ){
  #     ## wenn
  #     theta_v <- theta_v
  #     thres <- thres
  #     if(class(dat)!="logical"){resp <- dat}
  #   }

  thresL <- lapply(1:nrow(thres), function(i) {stats::na.omit(thres[i,])})
  names(thresL) <- rownames(thres)
  # do.call(cbind , lapply(thresL,function(x){t(pvx.matrix(theta_v,x ))}) )


  #   if(length(resp)==0){
  #     suppressWarnings(erg <- data.frame(lapply(thresL,function(x){t(pvx.matrix(theta_v,x ))}), row.names=names(theta_v),check.rows=F) )
  #   } currently not supported

  if(length(resp)!=0){
    respL <- lapply(1:ncol(resp), function(i) {(resp[,i])})
    names(respL) <- colnames(resp)
    #erg <- mapply(function(x,y){t(pvx.matrix(theta_v,x ,y+1))}, x=thresL, y=respL)
    erg <- mapply(function(x,y){t(pvx.matrix(theta_v = theta_v,thres = x ,xm_v = (y) ))}, x=thresL, y=respL) ## corrected 25-04-2019: this was wrong: xm_v = (y+1)
  }

  return(erg)
}

########################### hier die logLik method fuer pers #############################
logLik.pers<-function(object, sat=FALSE, p=FALSE, ...){
  # function adapted from the package pairwise by Joerg-Henrik Heine

  #' @title S3 logLik for Object of class "pers"
  #' @exportS3Method logLik pers
  #' @keywords methods
  #' @method logLik pers
  #' @description S3 logLik method to extract the log-likelihood for object of class\code{"pers"}
  #' @param object object of class\code{"pers"}
  #' @param sat a "logical" with default set to \code{sat=FALSE} to return the Log-Likelihood of the data for the unrestricted modell based on parameters estimated with function \code{\link{pers}}. If set to \code{sat=TRUE} the Log-Likelihood of the saturated model is returned instead.
  #' @param p a "logical" with default set to \code{p=FALSE} to return the category propabilities for the empirical data.
  #' @param ... not used jet.
  #' @export

  # compute m_v - number of categories per item as vector - used in both cases
  #m_v <-sapply(1:nrow(object$pair$threshold), function(i) {length(na.omit(object$pair$threshold[i,]))+1})
  m_v <- object$pair$m # new 03-12-2015 m is in any case part of "pair"

  if(sat==FALSE){
    # estimated model # NEW and correct since 21.11.2015 checked against WinMira (dichotom)
    #str.pattern(object$pair$resp)
    P <- pvx.super(theta_v=object) # probabilities for hole dataset

    #     pat_ind <- rownames(object$pair$resp)%in%(rownames(unique(object$pair$resp))) # index for unique pattern
    #     pat_fre <- (table(str.pattern(object$pair$resp))) # frequencies for pattern
    #     pat_lik <- (rowSums(log(P[pat_ind,]),na.rm=T)) # likelihood for (unique) pattern
    #     Log_Likelihood <- sum( pat_lik * pat_fre) # likelihood data (conditional?)
    #     # old things prior 21.1.2015
    #     #P <- matrix(rep(.1,5000),ncol = 5,nrow = 1000) #test
    #     sum(log(P),na.rm=T)
    Log_Likelihood <- sum(log(P),na.rm=T) # evtl. : P[complete.cases(P),]
    #     pat_fre[1] <- 0
    #     pat_fre[30] <- 0
    #
    #     sum( pat_lik * pat_fre)

    # neu auslesen der WLE geschätzten likelihood
    # Log_Likelihood <- sum(unique(object$pers$WLL)) #sum(as.numeric(names(table(object$pers$WLL))))


    # object$pair$threshold
    df <- sum(m_v-1)+sum(m_v-1)-1 # changed 27-3-2015 (acc. WINMIRA) (sum(m_v-1)-1)*2 # number of free model parameters (df) (number of scorgroups - 1 ): sum(m_v-1)  +  (numper of free itemparameter): sum(m_v-1)-1
    #     df_k <- sum(m_v-1)-1
    #     df_k_1 <- sum(m_v-1)-1
    nall <- dim(object$pair$resp)[1]
    nobs <- nall ### ev. check this !!! OK
    #structure(-213.902975399879, nall = 108L, nobs = 108, df = 8, class = "logLik")
    result <- structure(Log_Likelihood, nall = nall, nobs = nobs, df = df, class = "logLik")
  }

  if(sat==TRUE){
    # saturated model
    # tested against WinMira 20-04-2013 --> OK!!!
    x<-object$pair$resp
    #df.sat<-(m^k)-1 # degrees of fredom for saturated model
    df <- prod(m_v)-1# changed 27-3-2015 (acc. WINMIRA) prod(m_v) # changed for polytomies
    b<-dim(x)[2]
    l<-dim(x)[1]
    ppaste <- function(z){paste(z, collapse = "")} # creates pattern strings
    zaehl<-(as.matrix(table(apply(x, 1, ppaste))))[ ,1] # compute values for numerator and exponent
    nen<-rep(l,times=length(zaehl))     # compute values for denominator
    lik.sat<-sum(log((zaehl/nen)^zaehl)) # compute log likelihood of saturated model
    result <- structure(lik.sat, nall = l, nobs = l, df = df, class = "logLik")
  }
  if(p==TRUE){
    result <- P
  }

  return(result)
}

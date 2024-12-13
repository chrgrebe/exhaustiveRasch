
########################################################

LRtest.psy <- function(model, modelType, splitcr="median", splitseed=NULL){
  # function adapted from the package 'pairwise' by Joerg-Henrik Heine and from
  # function 'LRtest.Rm' of the package 'eRm' by Patrick Mair, Thomas Rusch,
  # Reinhold Hatzinger, Marco J. Maier & Rudolf Debelak
  #' itemfit statistics for 'psychotools'
  #' Anderson's likelihood ratio test for 'psychotools'
  #' @param model an object of 'psychotools' class 'raschmodel', 'pcmodel' or
  #' 'rsmodel'  (a model previously fit using the 'psychotools' package)
  #'  matching the value of modelType.
  #' @param modelType a character value defining the type of Rasch model.
  #'  Possible values: "RM", "PCM", "RSM"
  #' @param splitcr Split criterion for subject raw score splitting for
  #'  test_LR. "median" uses the median as split criterion, "mean" performs a
  #'    mean split, "random" performs a random split (in this case, the seed
  #'    can be set with the "splitseed" argument. splitcr can also be a
  #'    vector which assigns each person to a certain subgroup (typically an
  #'    external criterion). This vector can be numeric, character or a factor.
  #' @param splitseed seed for random split
  #' @return the p-value of the likelihood-ratio test.
  #' @export
  #' @examples
  #'  model <- psychotools::raschmodel(ADL[c(6,7,12,14,15)])
  #'  LRtest.psy(model=model, modelType="RM", splitcr="random", splitseed=332)


  X <- model$data

  #### split criteria
  if (!length(splitcr)>1){
    if(splitcr=="random"){
      if(is.numeric(splitseed)){set.seed(splitseed)}
      split<-as.numeric(cut(sample(1:(dim(X)[1])),2))
    }
    if(splitcr=="mean"){
      X<-as.matrix(X)
      rscore<-rowSums(X,na.rm = TRUE)
      split<-factor(ifelse(rscore > round(mean(rscore)),"above mean" ,"mean and below" ))
    }
    if(splitcr=="median"){
      X<-as.matrix(X)
      rscore<-rowSums(X,na.rm = TRUE)
      split<-factor(ifelse(rscore > stats::median(rscore),"above median" ,"median and below" ))
    }
  }
  if((is.integer(splitcr)) | (is.numeric(splitcr)) | (is.factor(splitcr))){
    if( (dim(X)[1])!=length(splitcr) ){stop("length of argument 'split' dose not match with 'data'")}
    split<-splitcr
  }
  if (is.matrix(X)) X = data.frame(X)

  # check valid category frequencies
  subpatterns <- lapply(X,function(x) table(x,split))
  nullcats <- which(sapply(subpatterns, function(x) any(x==0)))
  length(nullcats)
  if (length(nullcats) > 0) {
    stop("\nCalculations stopped! The following items have different categories across subgroups:\n",colnames(X)[nullcats],"\n\n")
  }

  subsamp <- names(table(split))
  datalist<-vector("list",length=length(subsamp))
  for (i in 1:length(datalist)){
    datalist[[i]]<-X[which(split==subsamp[i]),]
  }
  names(datalist) <- paste(subsamp,"sample")
  if (modelType=="RM"){
    submodels <- lapply(datalist, psychotools::raschmodel, hessian=FALSE,
                        nullcats="ignore")
  } else if(modelType=="RSM"){
    submodels <- lapply(datalist, psychotools::rsmodel, hessian=FALSE,
                        nullcats="ignore")
  } else{
    submodels <- lapply(datalist, psychotools::pcmodel, hessian=FALSE,
                        nullcats="ignore")
  }

  ch2 <-2*(sum(sapply(submodels, function(x) x$loglik))-model$loglik)
  df <- sum(sapply(submodels, function(x) x$df))-model$df
  p <- 1 - stats::pchisq(ch2,df)
  return(p)
}

########################################################

waldtest.psy <- function(model, modelType, splitcr="median", splitseed=NULL, icat=FALSE){
  #  function adapted from the package pairwise by Joerg-Henrik Heine
  #' Fischer and Scheiblechner's "wald-like" S-statistic for psychotools
  #' @param model an object of 'psychotools' class 'raschmodel', 'pcmodel' or
  #' 'rsmodel'  (a model previously fit using the 'psychotools' package)
  #'  matching the value of modelType.
  #' @param modelType a character value defining the type of Rasch model.
  #'  Possible values: "RM", "PCM", "RSM"
  #' @param splitcr split criterion for subject raw score splitting. "median"
  #'  uses the median as split criterion, "mean" performs a mean-split,
  #'  "random" performs a random split (in this case, the seed can be set
  #'  with the "splitseed" argument. Optionally splitcr can also be a
  #'  dichotomous vector which assigns each person to a certain subgroup
  #'  (typically an external criterion). This vector can be numeric, character
  #'  or a factor.
  #' @param splitseed seed for random split
  #' @param icat a boolean value defining wether to use item parameters
  #' ('psychotools' function 'itempar', if TRUE) or item category parameters
  #' ('psychotools' function 'threshpar')
  #' @return a vector containing the p-values of the Scheiblechner's
  #'  "wald-like" S-statistic for the items (if icat=FALSE) or for the item
  #'  categories (if icat=TRUE).
  #' @export
  #' @examples
  #'  model <- psychotools::raschmodel(ADL[c(6,7,12,14,15)])
  #'    waldtest.psy(model=model, modelType="RM", splitcr="random", splitseed=332,
  #'    icat=FALSE)

  X <- model$data
  #### split criteria
  if (!length(splitcr)>1){
    if(splitcr=="random"){
      if(is.numeric(splitseed)){set.seed(splitseed)}
      split<-as.numeric(cut(sample(1:(dim(X)[1])),2))
    }
    if(splitcr=="mean"){
      X<-as.matrix(X)
      rscore<-rowSums(X,na.rm = TRUE)
      split<-factor(ifelse(rscore > round(mean(rscore)),"above mean" ,"mean and below" ))
    }
    if(splitcr=="median"){
      X<-as.matrix(X)
      rscore<-rowSums(X,na.rm = TRUE)
      split<-factor(ifelse(rscore > stats::median(rscore),"above median" ,"median and below" ))
    }
  }
  if((is.integer(splitcr)) | (is.numeric(splitcr)) | (is.factor(splitcr))){
    if( (dim(X)[1])!=length(splitcr) ){stop("length of argument 'split' dose not match with 'data'")}
    split<-splitcr
  }
  ####
  if (is.matrix(X)) X = data.frame(X)
  # check valid category frequencies
  subpatterns <- lapply(X,function(x) table(x,split))
  nullcats <- which(sapply(subpatterns, function(x) any(x==0)))
  #length(nullcats)
  if (length(nullcats) > 0) {
    stop("\nCalculations stopped! The following items have different categories across subgroups:\n",colnames(X)[nullcats],"\n\n")
  }

  subsamp <- names(table(split))
  datalist<-vector("list",length=length(subsamp))
  for (i in 1:length(datalist)){
    datalist[[i]]<-X[which(split==subsamp[i]),]
  }
  names(datalist) <- paste(subsamp,"sample")
  if (modelType=="RM"){
    submodels <- lapply(datalist, psychotools::raschmodel, hessian=TRUE,
                        nullcats="ignore")
  } else if(modelType=="RSM"){
    submodels <- lapply(datalist, psychotools::rsmodel, hessian=TRUE,
                        nullcats="ignore")
  } else{
    submodels <- lapply(datalist, psychotools::pcmodel, hessian=TRUE,
                        nullcats="ignore")
  }

  cat_fullmod <- unlist(lapply(model$categories, length))
  cat_sub1 <- unlist(lapply(submodels[[1]]$categories, length))
  cat_sub2 <- unlist(lapply(submodels[[2]]$categories, length))
  if (any(cat_sub1!=cat_fullmod) | any(cat_sub2!=cat_fullmod)){
    stop(paste0("\n", "Aborted. There are items with null categories in the submodels."))
  }

  if (!icat){
    ipar1 <- psychotools::itempar(submodels[[1]], vcov=TRUE)
    ipar2 <- psychotools::itempar(submodels[[2]], vcov=TRUE)
  } else{
    ipar1 <- psychotools::threshpar(submodels[[1]], vcov=TRUE, type="mode")
    ipar2 <- psychotools::threshpar(submodels[[2]], vcov=TRUE, type="mode")
  }
  se1 <- as.numeric(sqrt(diag(stats::vcov(ipar1))))
  se2 <- as.numeric(sqrt(diag(stats::vcov(ipar2))))
  if (icat){
    ipar1 <- unlist(ipar1)
    ipar2 <- unlist(ipar2)
  }
  numerator <- ipar1 - ipar2
  denumerator <- sqrt(se1^2 + se2^2)
  teststat <- numerator/denumerator
  pvalues <- (1-stats::pnorm(abs(teststat)))*2
  if (is.na(pvalues[1])){ pvalues <- NULL}
  return(pvalues)

}

########################################################

mloef.psy <- function(model, modelType, splitcr="median", splitseed=NULL){
  # code adapted from function person.parameter.eRm of the package eRm by
  # Patrick Mair, Thomas Rusch, Reinhold Hatzinger, Marco J. Maier &
  # Rudolf Debelak
  #  function adapted from the package pairwise by Joerg-Henrik Heine
  #' Martin-Loef test for psychotools
  #' @param model an object of 'psychotools' class 'raschmodel', 'pcmodel' or
  #' 'rsmodel'  (a model previously fit using the 'psychotools' package)
  #'  matching the value of modelType.
  #' @param modelType a character value defining the type of Rasch model.
  #'  Possible values: "RM", "PCM", "RSM"
  #' @param splitcr Split criterion to define the item groups. "median" and
  #'  "mean" split items in two groups based on their items' raw scores median
  #'  or mean. "random" performs a random split (in this case, the seed can be
  #'  set with the "splitseed" argument. splitcr can also be a vector of length
  #'  k (where k denotes the number of items) that takes two or more distinct
  #'  values to define groups used for the Martin-Löf Test.
  #' @param splitseed seed for random split
  #' @return a list containing the test statistic, the degrees of freedom and
  #'  the p-value of the Martin-Löf test.
  #' @export
  #' @examples
  #'  model <- psychotools::raschmodel(ADL[c(6,7,12,14,15)])
  #'  mloef.psy(model=model, modelType="RM", splitcr="random", splitseed=332)


  X <- model$data
  #### split criteria
  if(splitcr=="random"){
    if(is.numeric(splitseed)){set.seed(splitseed)}
    split<-as.numeric(cut(sample(1:(dim(X)[2])),2))
  }
  if(splitcr=="mean"){
    X<-as.matrix(X)
    rscore<-colSums(X,na.rm = TRUE)
    split<-factor(ifelse(rscore > round(mean(rscore)),"above mean" ,"mean and below" ))
  }
  if(splitcr=="median"){
    X<-as.matrix(X)
    rscore<-colSums(X,na.rm = TRUE)
    split<-factor(ifelse(rscore > stats::median(rscore),"above median" ,"median and below" ))
  }
  if((is.integer(splitcr)) | (is.numeric(splitcr)) | (is.factor(splitcr))){
    if( (dim(X)[2])!=length(split) ){stop("length of argument 'split' does not match with 'data'")}
    split<-splitcr
  }
  ####

  subsamp <- names(table(split))

  datalist<-vector("list",length=length(subsamp))
  for (i in 1:length(datalist)){
    datalist[[i]]<-X[,which(split==subsamp[i])]
  }
  names(datalist) <- paste(subsamp,"sample")

  if (modelType=="RM"){
    submodels <- lapply(datalist, psychotools::raschmodel, hessian=FALSE, nullcats="ignore")
  } else if(modelType=="RSM"){
    submodels <- lapply(datalist, psychotools::rsmodel, hessian=FALSE, nullcats="ignore")
  } else{
    submodels <- lapply(datalist, psychotools::pcmodel, hessian=FALSE, nullcats="ignore")
  }

  logLik_subsamples  <- c(submodels[[1]]$loglik, submodels[[2]]$loglik)

  ### calculating the numerator and denominator (from eRm)
  sub.tabs <- as.data.frame(sapply(submodels, function(M){
    rowSums(M$data, na.rm=TRUE)
  }))
  sub.tabs <- table(sub.tabs)

  sub.term <- sub.tabs * (log(sub.tabs) - log(nrow(X)))
  sub.term <- sum(stats::na.omit(as.numeric(sub.term)))
  i.grps <- lapply(datalist, colnames)

  sub.max <- lapply(i.grps, function(g){ sum(apply(X[,g], 2, max)) })

  full.tab  <- table(rowSums(X, na.rm=TRUE))
  full.term <- sum(stats::na.omit(as.numeric( full.tab * (log(full.tab) - log(nrow(X))) )))

  ML.LR <- 2 * (
    sub.term  + sum(logLik_subsamples)
    - full.term - model$loglik
  )

  df <- prod(unlist(sub.max)+1) - (sum(apply(X, 2, max))+1) - length(unique(split)) + 1
  p.value <- 1 - stats::pchisq(ML.LR, df)

  if (!is.na(p.value)){
    return(list("teststat"=ML.LR, "df"=df, "p.value"=p.value))
  }

}

########################################################

pvx<-function(theta,thres,xm=NULL){
  # function adapted from the package pairwise by Joerg-Henrik Heine
  # func. by joerg-henrik heine jhheine(at)googlemail.com
  # nichts geändert aber zum merken theta: einzelne zahl; thres: thurstonian threshold eines items
  # korrigierte formel aus markus buch seite 330 siehe auch s. 522 2006

    # This is an internal function that is not intended to be called by users.
  # It is nevertheless exported so that it can be run in the parallelization
  # workers. However, the function is not documented in the manual.

  #' this is an internal function for expscore()
  #' @param thres internal
  #' @param xm internal
  #' @return internal
  #' @export
  #' @keywords internal

  s<-0:length(thres)
  thres<-c(0,thres)
  oben<- exp((s*theta)-cumsum(thres))
  unten<- sum(exp((s*theta)-cumsum(thres)))
  px<-oben / unten
  names(px)<-0:(length(thres)-1)
  P<-sum(oben / unten)
  #if(length(xm)==0){return(list(px=px,P=P))}
  if(length(xm)==0){return(px)}
  if(length(xm)!=0){px[xm]}
}

pvx.matrix<-function(theta_v,thres,xm_v=NULL){
  # function adapted from the package pairwise by Joerg-Henrik Heine
  # func. by joerg-henrik heine jhheine(at)googlemail.com
  # ein dimension dazu und zum merken
  # theta_v: ein vector oder zahl;
  # thres: thurstonian thresholds eines items
  # xm_v: vector welche kategorie prob jeweils ausgegeben werden soll
  # korrigierte formel aus markus buch seite 330

  # This is an internal function that is not intended to be called by users.
  # It is nevertheless exported so that it can be run in the parallelization
  # workers. However, the function is not documented in the manual.

  #' pvx matrix
  #' @param theta_v internal
  #' @param thres internal
  #' @param xm_v internal
  #' @return internal
  #' @export
  #' @keywords internal

  s<-0:length(thres)
  thres0<-c(0,thres)
  oben_v <- exp(apply((s%o%theta_v),2,function(x){x-cumsum(thres0)})) # ok - für theta_v als vector oder zahl
  unten_v<- apply( exp(apply((s%o%theta_v),2,function(x){x-cumsum(thres0)})) , 2 ,sum) # ok - für theta_v als vector oder zahl
  px_v <- mapply(FUN=function(o,u){  o / u }, o=as.list(as.data.frame(oben_v)), u=as.list(unten_v) ) # u as list etc --> korrigiert am 10-3-2015 # ok - für theta_v als vector oder zahl
  rownames(px_v)<-paste("cat",0:(length(thres0)-1),sep=".")
  colnames(px_v)<-theta_v
  P_v <- apply(px_v,2,sum) # ok - für theta_v als vector oder zahl kontrolle alle 1
  if(length(xm_v)==0){erg <-(px_v)}
  if(length(xm_v)!=0){erg <- mapply(function(p,ic){p[ic]}, p=(as.list(as.data.frame(px_v))), ic=(xm_v+1))}
  return(erg)
}


expscore.psy <- function(X, thres, ppar, na_treat=NA){
  # function adapted from the package pairwise by Joerg-Henrik Heine

  # This is an internal function that is not intended to be called by users.
  # It is nevertheless exported so that it can be run in the parallelization
  # workers. However, the function is not documented in the manual.

  #' returns a matrix with dims like resp matrix with expected scores and more ...
  #' func. by joerg-henrik heine jhheine(at)googlemail.com
  #' needs func. \code{pvx} in i.pvx.R and \code{pvx.matrix} in i.pvx.matrix.R
  #' Notation and formulas see:  Wright & Masters 1982 p.100
  #' in a revised form  (korrigendum) see http://www.rasch.org/rmt/rmt34e.htm
  #' @param X internal
  #' @param thres internal
  #' @param ppar internal
  #' @param na_treat internal
  #' @return internal
  #' @export
  #' @keywords internal


  nitems <- dim(thres)[1]
  emp_resp <- X # empirical responses
  N <- dim(X)[1]

  # Expected Mean Eni (of xni)-----------------
  foo1 <- function(theta, thres, nitems=nitems){
    #theta: ein theta wert
    catL <- lapply(1:nitems,function(x){(0:length(stats::na.omit(thres[x,])))})
    pL <- lapply(1:nitems,function(x){pvx(theta=theta,thres=stats::na.omit(thres[x,]))})
    exp_scor_theta <- mapply(FUN=function(o1,o2){sum(o1*o2)}, o1=catL, o2=pL)
    return(exp_scor_theta)
  }
  Eni <- t(sapply(ppar, foo1, thres, nitems))
  dimnames(Eni) <- dimnames(emp_resp)
  ### replace cells missing in emp_resp by NA in Eni new approach!----
  # affects Wni and Cni and all the other _ni's too
  Eni[is.na(emp_resp)] <- NA
  # Eni[is.na(emp_resp)] <- 0 # nicht nötig passiert weiter unten

  # matplot(Eni[order(pers_obj$pers$WLE),],type="l") # ggf. noch machen
  # return(Eni)

  ## Variance Wni (of xni)--------
  foo2 <- function(i, X, thres, ppar, Eni=Eni){
    theta_v <- ppar # empirical theta vector
    thres <- stats::na.omit(thres[i,]) # thurst. thresholds item i
    kat <- 0:length(thres) # categories item i
    xm_v <- X[,i]+1  # responses item i | +1 ist wichtig es heißt 1 und 2 kategorie nicht 0 und 1
    pnik <- (pvx.matrix(theta_v,thres)); dimnames(pnik)[[2]] <- names(xm_v)
    Lpnik <- as.list(data.frame(t(pnik)))
    Leni <- lapply(kat, function(x){  (x-Eni[,i])^2  })# Eni[,i] # expected scores item i
    wni <- rowSums(mapply(function(p,e){e*p}, p=Lpnik, e=Leni,SIMPLIFY = TRUE))
    return(wni)
  }
  Wni <- sapply(1:nitems, foo2, X, thres, ppar, Eni)
  dimnames(Wni) <- dimnames(emp_resp)
  # return(Wni)
  Wni[is.na(emp_resp)] <- na_treat # new

  ## Kurtosis Cni (of xni)--------
  foo3 <- function(i, X, thres, ppar, Eni=Eni){
    theta_v <- ppar # empirical theta vector
    thres <- stats::na.omit(thres[i,]) # thurst. thresholds item i
    kat <- 0:length(thres) # categories item i
    xm_v <- X[,i]+1  # responses item i | +1 ist wichtig es heißt 1 und 2 kategorie nicht 0 und 1
    pnik <- (pvx.matrix(theta_v,thres)); dimnames(pnik)[[2]] <- names(xm_v)
    Lpnik <- as.list(data.frame(t(pnik)))
    Leni <- lapply(kat, function(x){  (x-Eni[,i])^4  })# Eni[,i] # expected scores item i
    cni <- rowSums(mapply(function(p,e){e*p}, p=Lpnik, e=Leni,SIMPLIFY = TRUE))
    return(cni)
  }
  Cni <- sapply(1:nitems, foo3, X, thres, ppar, Eni)
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

ppar.psy <- function(model=NULL){
  # code adapted from function person.parameter.eRm of the package eRm by
  # Patrick Mair, Thomas Rusch, Reinhold Hatzinger, Marco J. Maier &
  # Rudolf Debelak
  #  function adapted from the package pairwise by Joerg-Henrik Heine
  #' itemfit statistics for psychotools
  #' @param model an object of 'psychotools' class 'raschmodel', 'pcmodel' or
  #' 'rsmodel'  (a model previously fit using the 'psychotools' package)
  #'  matching the value of modelType.
  #' @return an object containing person parameters, residuals and PSI
  #' @export
  #' @examples
  #'  model <- psychotools::raschmodel(ADL[c(6,7,12,14,15)])
  #'  ppar.psy(model)


  X <- model$data
  if (!"thresholds" %in% names(model)){
    model$thresholds  <- psychotools::threshpar(model, type="mode")
  }
  thres <- t(simplify2array(model$thresholds))
  if (dim(thres)[1]==1){ #dichotomous case
    thres <- as.matrix((simplify2array(model$thresholds)))
  }
  try(suppressWarnings({ppar <- psychotools::personpar(
    model, personwise = TRUE, vcov=FALSE)}), silent=TRUE)
  if (exists("ppar")==TRUE){
    ppar_unique <- psychotools::personpar(model, vcov=TRUE)
    se_unique <- as.numeric(sqrt(diag(stats::vcov(ppar_unique))))
    unique_tab <- cbind(ppar_unique, se_unique)

    ### exclude persons with 0/full raw scores
    max.it <- apply(X, 2L, max, na.rm = TRUE)                           #maximum item raw score without NA
    rp     <- rowSums(X, na.rm = TRUE)                                  #person raw scores
    maxrp  <- apply(X, 1L, function(x.i){ sum(max.it[!is.na(x.i)]) })   #maximum item raw score for person i
    TFrow  <- ((rp==maxrp) | (rp==0))
    pers.ex       <- (1L:nrow(X))[TFrow]       #persons excluded from estimation due to 0/full
    pers.ex.names <- rownames(X)[pers.ex]
    pers.in        <- (1L:nrow(X))[-pers.ex]   #persons in estimation
    if(length(pers.ex) > 0L){   #data matrix persons (full/0) excluded)
      X <- X[-pers.ex,]
      ppar <- ppar[-pers.ex]
    }
    se <- unique_tab[match(ppar, unique_tab),2]

    obj <- expscore.psy(X, thres, ppar, na_treat=NA) # calls internal function
    emp_resp <- X
    Eni <- obj$Eni # expected scores (Expected Mean of ...) gegencheck eRm OK
    Wni <- obj$Wni # Variance of ... gegencheck eRm OK
    Cni <- obj$Cni # Kurtosis of ... gegencheck eRm OK
    Yni <- obj$Yni # score residual ... gegencheck eRm OK
    Zni <- obj$Zni # standardised residual ... gegencheck eRm (st.res in itemfit.ppar) OK
    Y2ni <- obj$Y2ni
    Z2ni <- obj$Z2ni #standardised residual squared ... gegencheck eRm (sq.res in itemfit.ppar) OK
    #-----------------------------------------------------------------
    # Nna_v <- colSums(!is.na(Z2ni))
    Nna_v <- colSums(!is.na(emp_resp))

    Chi <- colSums(Z2ni,na.rm=TRUE) # ... gegencheck eRm (ifit in itemfit.ppar) OK
    df <- Nna_v-1 # sowieso besser als eRm, da wird das -1 vergessen
    pChi <- 1-stats::pchisq(Chi, df) # p-value
    #loc.chi.square.p<-1-pchisq(loc.chi.square,loc.df)
    #-----------------------------------------------------------------

    ## Variance Uq2i of -> Unweighted Mean Square Ui () -------
    Uq2i  <-  (colSums( (Cni / (Wni^2)), na.rm = TRUE) / (Nna_v)^2 - (1/Nna_v)   ) # ... gegencheck eRm (qsq.outfitMSQ in itemfit.ppar) OK
    Uqi <- sqrt(Uq2i)

    ## Unweighted Mean Square Ui (OUTFIT.MEANSQ)-------
    # so macht es eRm als alternative (dritte stelle hintem komma versch.):   i.outfitMSQ <- Chi/df
    #colSums(!is.na(Z2ni))
    Ui <- colSums(Z2ni, na.rm = TRUE)/Nna_v   # nicht N wegen missings!
    Uikorr <- Ui+1-mean(Ui) # korr. outfit --> siehe oRM.pdf seite 8 oben und oRM.R Zeile 115 und Ordinales_Rasch_Modell.pdf seite 68

    ## Standardised (Un)weighted Mean Square Ti (OUTFIT.ZSTD)-------
    UTi <- ( ( (Ui^(1/3)) -1) * (3/Uqi) ) + (Uqi/3) # ... gegencheck eRm (i.outfitZ in itemfit.ppar) formel stimmt - werte leicht unterschiedlich - in eRm werden perfecte resp. vorher rausgeworfen OK
    UTikorr <- ( ( (Uikorr^(1/3)) -1) * (3/Uqi) ) + (Uqi/3)

    #-----------------------------------------------------------------

    ## Variance Vq2i of -> Weighted Mean Square Vi (INFIT) -------
    Vq2i  <- colSums( (Cni - (Wni^2)), na.rm = TRUE) / (colSums(Wni, na.rm = TRUE)^2) # ... gegencheck eRm (qsq.infitMSQ in itemfit.ppar) OK
    Vqi <- sqrt(Vq2i)

    ## Weighted Mean Square Vi (INFIT.MEANSQ)-------
    # Vi <- colSums(Z2ni*Wni, na.rm = TRUE)/colSums(Wni, na.rm = TRUE) # eRm style -> identisch
    Vi <- colSums(Y2ni, na.rm = TRUE) / colSums(Wni, na.rm = TRUE) # ... gegencheck eRm (i.infitMSQ in itemfit.ppar) OK
    Vikorr <- Vi+1-mean(Vi) # korr. outfit --> siehe oRM.pdf seite 8 oben und oRM.R Zeile 115 und Ordinales_Rasch_Modell.pdf seite 68

    ## Standardised Weighted Mean Square Ti (INFIT.ZSTD)-------
    VTi <- ( (Vi^(1/3)-1) * (3/Vqi) ) + (Vqi/3) # ... gegencheck eRm (i.infitZ in itemfit.ppar) unterschiede! aber formel stimmt OK
    VTikorr <- ( (Vikorr^(1/3)-1) * (3/Vqi) ) + (Vqi/3)

    #-----------------------------------------------------------------
    val_fit <- as.data.frame(list(Chi=round(Chi,4),
                                  df=df, p=round(pChi,4), OUTFIT.MSQ=round(Ui,4),
                                  OUTFIT.ZSTD=round(UTi,4), INFIT.MSQ=round(Vi,4),
                                  INFIT.ZSTD=round(VTi,4),
                                  OUTFIT.MSQ.REL=round(Uikorr,4),
                                  OUTFIT.ZSTD.REL=round(UTikorr,4),
                                  INFIT.MSQ.REL=round(Vikorr,4),
                                  INFIT.ZSTD.REL=round(VTikorr,4)))
    class(val_fit) <- c("pifit","data.frame")
    val_res <- list(res_score= obj$Yni, res_scoreSq= Y2ni, res_std= Zni,
                    res_stdSq= Z2ni)

    #----------- Person Sepqeation Reliability
    pers.ssd <- stats::var(ppar)
    MSE <- sum((se)^2) / length(se)
    PSI <- (pers.ssd-MSE) / pers.ssd

    #------------ return object
    val_full <- list("theta"=ppar, "itemfit"=val_fit, "residuals"= val_res, "PSI"=PSI)
    class(val_full) <- c("ppar.psy", "list")
    return(val_full)
  }
}



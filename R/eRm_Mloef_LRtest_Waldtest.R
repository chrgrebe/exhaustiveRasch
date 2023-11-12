# The functions in this file implement model tests. They are internal functions,
# calling by the user is not indicated. They are nevertheless exported in order
# to work in parallelization. However, it is not included in the package
# documentation (roxygen2 keyword 'internal').


Mloef <- function(robj, splitcr="median", estimation_param)
{
  #' This code is identical to the function of the eRM package (version 0.1.4)
  #' of the same name. Authors of the code are Patrick Mair, Thomas Rusch,
  #' Reinhold Hatzinger, Marco J. Maier & Rudolf Debelak.
  #' The only difference is that it calls the fit_rasch() function for model
  #' fitting instead of the respective functions of eRM. Missing values are
  #' not allowed.
  #' @param robj object of class RM, PCM or RSM (package eRm)
  #' @param splitcr a character value that defines the splitting criterion for
  #'  two groups. "median" (default) and "mean" split items in two groups
  #'  according to the median/mean or item raw scores. A vector of length k
  #'  (number of items) containing two different elements signifying group
  #'  membership of items can be supplied.
  #' @param estimation_param a list of arguments for estimation constrol as
  #' returned by the estimation_control() function.
  #' @return a list containing the same parameters the respective function of
  #' package eRm would return.
  #' @export
  #' @keywords internal

  if(any(is.na(robj$X))) stop("Martin-Loef Test with NA currently not available\n")

  wrning <- NULL   # initialize an object for warnings

  if(length(splitcr) == 1){   # generate split-vector if "mean" or "median"
    if(splitcr == "median"){
      raw.scores <- colSums(robj$X,na.rm=T)
      numsplit <- as.numeric(raw.scores > stats::median(raw.scores,na.rm=T))
      ## removed for the time being 2011-09-08 rh
      #if( any(raw.scores == median(raw.scores,na.rm=T)) ){   # Only if one item's raw score == the median, a warning is issued
      #  wrning <- which(raw.scores == median(raw.scores,na.rm=T))   # append a warning-slot to the object for print and summary methods
      #  cat("Item(s)",paste(names(wrning),collapse=", "),"with raw score equal to the median assigned to the lower raw score group!\n")
      #}
    }
    if(splitcr=="mean"){
      raw.scores <- colSums(robj$X,na.rm=T)
      numsplit <- as.numeric(raw.scores > mean(raw.scores,na.rm=T))
      ## removed for the time being 2011-09-08 rh
      #if( any(raw.scores == mean(raw.scores,na.rm=T)) ){   # Only if one item's raw score == the mean, a warning is issued
      #  wrning <- which(raw.scores == mean(raw.scores,na.rm=T))   # append a warning-slot to the object for print and summary methods
      #  cat("Item(s)",paste(names(wrning),collapse=", "),"with raw score equal to the mean assigned to the lower raw score group!\n")
      #}
    }
  } else {   # check if the submitted split-vector is appropriate
    if(length(splitcr) != ncol(robj$X)) stop("Split vector too long/short.")
    #    if(length(unique(splitcr)) > 2) stop("Only two groups allowed.")
    if(length(unique(splitcr)) < 2) stop("Split vector must contain at least two groups.")
    numsplit <- splitcr
  }
  sp.groups <- unique(numsplit)
  i.groups <- sapply(sp.groups, function(g){ which(numsplit == g) }, simplify=F)

  # check if any group countains less than 2 items
  if( any(unlist(lapply(i.groups, length)) < 2) ){
    stop("Each group of items must contain at least 2 items.")
  }

  # check if one group contains subject with <=1 valid responses
  if(any(unlist(lapply(i.groups, function(g){ any(rowSums(!is.na(robj$X[,g])) <= 1) })))) stop("Groups contain subjects with less than two valid responses.")

  ### possible missing patterns and classification of persons into groups
  #  MV.X <- apply(matrix(as.numeric(is.na(robj$X01)),ncol=ncol(robj$X01)),1,paste,collapse="")
  #  MV.p <- sort(unique(MV.X))
  #  MV.g <- numeric(length=length(MV.X))
  #  g <- 1
  #  for(i in MV.p){
  #    MV.g[MV.X == i] <- g;
  #    g <- g + 1
  #  }
  #  na.X01 <- list()
  #  for(i in 1:length(MV.p)){
  #    na.X01[[i]] <- matrix(robj$X01[which(MV.g == i),], ncol=ncol(robj$X01))
  #  }

  #  res1 <- RM(robj$X01[,i.groups[[1]]])
  #  res2 <- RM(robj$X01[,i.groups[[2]]])

  # fitting the submodels
  #estimation_param$se <- F
  #estimation_param$datacheck <- F
  subModels <- lapply(i.groups, function(g){ fit_rasch(robj$X[,g],
                                                       modelType="PCM",
                                                       estimation_param) })

  ### calculating the numerator and denominator
  sub.tabs <- as.data.frame(sapply(subModels, function(M){
    rowSums(M$X, na.rm=T)
  }))
  sub.tabs <- table(sub.tabs)

  sub.term <- sub.tabs * (log(sub.tabs) - log(nrow(robj$X)))
  sub.term <- sum(stats::na.omit(as.numeric(sub.term)))

  sub.max <- lapply(i.groups, function(g){ sum(apply(robj$X[,g], 2, max)) })

  full.tab  <- table(rowSums(robj$X, na.rm=T))
  full.term <- sum(stats::na.omit(as.numeric( full.tab * (log(full.tab) - log(nrow(robj$X))) )))

  ML.LR <- 2 * (
    sub.term  + sum(unlist(lapply(subModels, `[[`, "loglik")))
    - full.term - robj$loglik
  )

  df <- prod(unlist(sub.max)+1) - (sum(apply(robj$X, 2, max))+1) - length(sp.groups) + 1

  #  ml.num <- ml.den <- df <- numeric()

  #  for(i in 1:length(MV.p)){
  #    .temp.num <- table(rowSums(na.X01[[i]],na.rm=T))
  #    .temp.num <- .temp.num[.temp.num > 0]   ### rh
  #    ml.num[i] <- sum( (log(.temp.num)-log(sum(.temp.num)))*.temp.num )
  #
  #    if(nrow(na.X01[[i]]) > 1){
  #      .temp.den <- table(rowSums(na.X01[[i]][,i.groups[[1]]],na.rm=T),
  #                         rowSums(na.X01[[i]][,i.groups[[2]]],na.rm=T))
  #    }
  #    else{
  #      .temp.den <- table(sum(na.X01[[i]][,i.groups[[1]]],na.rm=T),
  #                         sum(na.X01[[i]][,i.groups[[2]]],na.rm=T))
  #    }
  #    .temp.den <- .temp.den[.temp.den > 0]   ### rh
  #    ml.den[i] <- sum( (log(.temp.den)-log(sum(.temp.den)))*.temp.den )
  #
  #    k1 <- sum(!is.na(na.X01[[i]][1,i.groups[[1]]])) ### rh
  #    k2 <- sum(!is.na(na.X01[[i]][1,i.groups[[2]]])) ### rh
  #    df[i] <- k1 * k2 -1                              ### rh
  #  }
  #
  #  a <- sum(ml.num)
  #  b <- sum(ml.den)
  #  k <- c(length(i.groups[[1]]),length(i.groups[[2]]))
  #
  #  ML.LR <- -2*( (a + robj$loglik) - (b + res1$loglik + res2$loglik) )
  #  DF <- prod(k) - 1
  p.value <- 1 - stats::pchisq(ML.LR, df)

  result <- list(LR=ML.LR, df=df, p.value=p.value,
                 fullModel=robj, subModels=subModels,
                 Lf=robj$loglik,  Ls=lapply(subModels, `[[`, "loglik"),
                 #                 theta.table.RM=table(rowSums(robj$X01)),                        # both used for the plotting
                 #                 theta.table.MLoef=table(rowSums(res1$X01),rowSums(res2$X01)),   # routine plot.MLoef
                 i.groups=i.groups,
                 #                 items1=i.groups[[1]], items2=i.groups[[2]], k=k,
                 splitcr=splitcr, split.vector=numsplit, warning=wrning, call=match.call())
  class(result) <- "MLoef"
  return(result)
}


Waldtest <- function(object, splitcr = "median", estimation_param){
  #' This code is identical to the function of the eRM package (version 0.1.4)
  #' of the same name. Authors of the code are Patrick Mair, Thomas Rusch,
  #' Reinhold Hatzinger, Marco J. Maier & Rudolf Debelak.
  #' The only difference is that it calls the fit_rasch() function for model
  #' fitting instead of the respective functions of eRM.
  #' @param object object of class RM, PCM or RSM (package eRm)
  #' @param splitcr a character value that defines the splitting criterion for
  #'  two groups. "median" (default) and "mean" split items in two groups
  #'  according to the median/mean or item raw scores. A vector of length n
  #'  for group split can also be submitted.
  #' @param estimation_param a list of arguments for estimation constrol as
  #' returned by the estimation_control() function.
  #' @return a list containing the same parameters the respective function of
  #' package eRm would return.
  #' @export
  #' @keywords internal

  # performs item-based Wald test (Fischer & Molenaar, p.90)
  # object... object of class RM
  # splitcr... splitting criterion for LR-groups. "median" to a median raw score split,
  #            "mean" corobjectponds to the mean raw score split.
  #            optionally also a vector of length n for group split can be submitted.

  prettyPaste <- function(...){
    paste(strwrap(paste0(..., collapse = ""), width = getOption("width")), sep="\n", collapse="\n")
  }


  call<-match.call()

  spl.gr<-NULL

  X.original<-object$X
  if (length(splitcr)>1 && is.character(splitcr)){    # if splitcr is character vector, treated as factor
    splitcr<-as.factor(splitcr)
  }
  if (is.factor(splitcr)){
    spl.nam<-deparse(substitute(splitcr))
    spl.lev<-levels(splitcr)
    spl.gr<-paste(spl.nam,spl.lev,sep=" ")
    splitcr<-unclass(splitcr)
  }

  numsplit<-is.numeric(splitcr)
  if (any(is.na(object$X))) {
    if (!numsplit && splitcr=="mean") {                                   #mean split
      spl.gr<-c("Raw Scores < Mean", "Raw Scores >= Mean")
      X<-object$X
      # calculates index for NA groups
      # from person.parameter.eRm
      dichX <- ifelse(is.na(X),1,0)
      strdata <- apply(dichX,1,function(x) {paste(x,collapse="")})
      gmemb <- as.vector(data.matrix(data.frame(strdata)))
      gindx<-unique(gmemb)
      rsum.all<-rowSums(X,na.rm=TRUE)
      grmeans<-tapply(rsum.all,gmemb,mean)      #sorted
      ngr<-table(gmemb)                         #sorted
      m.all<-rep(grmeans,ngr)                   #sorted,expanded
      rsum.all<-rsum.all[order(gmemb)]
      spl<-ifelse(rsum.all<m.all,1,2)
      splitcr<-spl
      object$X<-X[order(gmemb),]
    }
    if (!numsplit && splitcr=="median") {                                   #median split
      spl.gr<-c("Raw Scores <= Median", "Raw Scores > Median")
      #removed rh 2010-12-17
      #cat("Warning message: Persons with median raw scores are assigned to the lower raw score group!\n")
      X<-object$X
      # calculates index for NA groups
      # from person.parameter.eRm
      dichX <- ifelse(is.na(X),1,0)
      strdata <- apply(dichX,1,function(x) {paste(x,collapse="")})
      gmemb <- as.vector(data.matrix(data.frame(strdata)))
      gindx<-unique(gmemb)
      rsum.all<-rowSums(X,na.rm=TRUE)
      grmed<-tapply(rsum.all,gmemb,stats::median)      #sorted
      ngr<-table(gmemb)                         #sorted
      m.all<-rep(grmed,ngr)                     #sorted,expanded
      rsum.all<-rsum.all[order(gmemb)]
      spl<-ifelse(rsum.all<=m.all,1,2)
      splitcr<-spl
      object$X<-X[order(gmemb),]
    }
  }


  if (is.numeric(splitcr)){
    spl.nam<-deparse(substitute(splitcr))
    if (length(table(splitcr)) > 2) stop("Dichotomous person split required!")
    if (length(splitcr) != dim(object$X)[1]) {
      stop("Mismatch between length of split vector and number of persons!")
    } else {
      rvind <- splitcr
      Xlist <- by(object$X,rvind, function(x) x)
      names(Xlist) <- as.list(sort(unique(splitcr)))
      if(is.null(spl.gr)){
        spl.lev<-names(Xlist)
        spl.gr<-paste(spl.nam,spl.lev,sep=" ")
      }
    }}

  if (!is.numeric(splitcr)) {
    if (splitcr=="median") {                                   #median split
      rv <- apply(object$X,1,sum,na.rm=TRUE)
      rvsplit <- stats::median(rv)
      rvind <- rep(0,length(rv))
      rvind[rv > rvsplit] <- 1                                 #group with high raw score object
      Xlist <- by(object$X,rvind,function(x) x)
      names(Xlist) <- list("low","high")
    }

    if (splitcr=="mean") {                                     #mean split
      rv <- apply(object$X,1,sum,na.rm=TRUE)
      rvsplit <- mean(rv)
      rvind <- rep(0,length(rv))
      rvind[rv > rvsplit] <- 1                                 #group with highraw scoobject
      Xlist <- by(object$X,rvind,function(x) x)
      names(Xlist) <- list("low","high")
    }

  }

  del.pos.l <- lapply(Xlist, function(x) {
    it.sub <- datcheck.LRtest(x,object$X,object$model)  #items to be removed within subgroup
  })

  del.pos <- unique(unlist(del.pos.l))
  if ((length(del.pos)) >= (dim(object$X)[2]-1)) {
    stop("\nNo items with appropriate response patterns left to perform Wald-test!\n")
  }

  if(length(del.pos) > 0){
    warning(paste0(
      "\n",
      prettyPaste("The following items were excluded due to inappropriate response patterns within subgroups:"),
      "\n",
      paste(colnames(object$X)[del.pos], collapse=" "),
      "\n\n",
      prettyPaste("Subgroup models are estimated without these items!")
    ), immediate.=TRUE)
  }

  if(length(del.pos) > 0){
    X.el <- object$X[,-(del.pos)]
  } else {
    X.el <- object$X
  }
  Xlist.n <- by(X.el,rvind,function(y) y)
  names(Xlist.n) <- names(Xlist)


  likpar <- sapply(Xlist.n,function(x) {                       #matrix with loglik and npar for each subgroup
    objectg <- fit_rasch(x, modelType= object$model,
                         estimation_param= estimation_param)
    parg <- objectg$etapar
    seg <- objectg$se.eta
    list(parg,seg,objectg$betapar,objectg$se.beta)
  })

  betapar1 <- likpar[3,][[1]]
  beta1.se <- likpar[4,][[1]]
  betapar2 <- likpar[3,][[2]]
  beta2.se <- likpar[4,][[2]]
  num <- (betapar1-betapar2)
  denom <- sqrt(beta1.se^2 + beta2.se^2)
  W.i <- num/denom
  pvalues <- (1-stats::pnorm(abs(W.i)))*2

  coef.table <- cbind(W.i,pvalues)
  dimnames(coef.table) <- list(names(betapar1),c("z-statistic","p-value"))

  result <- list(coef.table=coef.table,betapar1=betapar1,se.beta1=beta1.se,betapar2=betapar2,
                 se.beta2=beta2.se, spl.gr=spl.gr, call=call, it.ex = del.pos)
  class(result) <- "wald"
  result

}

LRtest <-  function(object, splitcr = "median", se = TRUE, estimation_param){
  #' Performs Andersen LR-test.
  #' This code is identical to the function of the eRM package (version 0.1.4)
  #' of the same name. Authors of the code are Patrick Mair, Thomas Rusch,
  #' Reinhold Hatzinger, Marco J. Maier & Rudolf Debelak.
  #' The only difference is that it calls the fit_rasch() function for model
  #' fitting instead of the respective functions of eRM.
  #' @param object object of class RM, PCM or RSM (package eRm)
  #' @param splitcr splitting criterion for LR-groups. "all.r" corresponds to a complete
  #   raw score split (r=1,...,k-1), "median" to a median raw score split,
  #   "mean" corresponds to the mean raw score split.
  #   optionally also a vector of length n for group split can be submitted.
  #' @param se whether standard errors should be computed
  #' @param estimation_param a list of arguments for estimation control as
  #' returned by the estimation_control() function.
  #' @return a list containing the same parameters the respective function of
  #' package eRm would return.
  #' @export
  #' @keywords internal

  prettyPaste <- function(...){
    paste(strwrap(paste0(..., collapse = ""), width = getOption("width")), sep="\n", collapse="\n")
  }


  call<-match.call()

  spl.gr<-NULL

  X.original<-object$X
  if((length(splitcr) > 1) & is.character(splitcr)){    # if splitcr is character vector, treated as factor
    splitcr<-as.factor(splitcr)
  }
  if(is.factor(splitcr)){
    spl.nam<-deparse(substitute(splitcr))
    spl.lev<-levels(splitcr)
    spl.gr<-paste(spl.nam,spl.lev,sep=" ")
    splitcr<-unclass(splitcr)
  }

  numsplit<-is.numeric(splitcr)
  if (any(is.na(object$X))) {
    if (!numsplit && splitcr=="mean") {                                   #mean split
      spl.gr<-c("Raw Scores < Mean", "Raw Scores >= Mean")
      X<-object$X
      # calculates index for NA groups
      # from person.parameter.eRm
      dichX <- ifelse(is.na(X),1,0)
      strdata <- apply(dichX,1,function(x) {paste(x,collapse="")})
      gmemb <- as.vector(data.matrix(data.frame(strdata)))
      gindx<-unique(gmemb)
      rsum.all<-rowSums(X,na.rm=T)
      grmeans<-tapply(rsum.all,gmemb,mean)      #sorted
      ngr<-table(gmemb)                         #sorted
      m.all<-rep(grmeans,ngr)                   #sorted,expanded
      rsum.all<-rsum.all[order(gmemb)]
      spl<-ifelse(rsum.all<m.all,1,2)
      splitcr<-spl
      object$X<-X[order(gmemb),]
    }
    if (!numsplit && splitcr=="median") {                                   #median split
      spl.gr<-c("Raw Scores <= Median", "Raw Scores > Median")
      # cat("Warning message: Persons with median raw scores are assigned to the lower raw score group!\n")
      X<-object$X
      # calculates index for NA groups
      # from person.parameter.eRm
      dichX <- ifelse(is.na(X),1,0)
      strdata <- apply(dichX,1,function(x) {paste(x,collapse="")})
      gmemb <- as.vector(data.matrix(data.frame(strdata)))
      gindx<-unique(gmemb)
      rsum.all<-rowSums(X,na.rm=T)
      grmed<-tapply(rsum.all,gmemb,stats::median)      #sorted
      ngr<-table(gmemb)                         #sorted
      m.all<-rep(grmed,ngr)                     #sorted,expanded
      rsum.all<-rsum.all[order(gmemb)]
      spl<-ifelse(rsum.all<=m.all,1,2)
      splitcr<-spl
      object$X<-X[order(gmemb),]
    }
  }

  if (!is.numeric(splitcr)) {
    if (splitcr=="all.r") {                               #full raw score split   ### begin MjM 2012-03-18
      rvind <- rowSums(object$X, na.rm=TRUE)              #person raw scoobject
      excl_0_k <- (rvind > 0) & (rvind < sum(apply(object$X, 2, max, na.rm=T)))
      Xlist <- by(object$X[excl_0_k,], rvind[excl_0_k], function(x) x)
      names(Xlist) <- as.list(paste("Raw Score =", sort(unique(rvind[excl_0_k]))))
      spl.gr <- unlist(names(Xlist))
    }                                                                             ### end MjM 2012-03-18

    if (splitcr=="median") {                                   #median split
      spl.gr<-c("Raw Scores <= Median", "Raw Scores > Median")
      #removed rh 2010-12-17
      #cat("Warning message: Persons with median raw scores are assigned to the lower raw score group!\n")
      rv <- apply(object$X,1,sum,na.rm=TRUE)
      rvsplit <- stats::median(rv)
      rvind <- rep(0,length(rv))
      rvind[rv > rvsplit] <- 1                                 #group with highraw scoobject
      Xlist <- by(object$X,rvind,function(x) x)
      names(Xlist) <- list("low","high")
    }

    if (splitcr=="mean") {                                     #mean split
      spl.gr<-c("Raw Scores < Mean", "Raw Scores >= Mean")
      rv <- apply(object$X,1,sum,na.rm=TRUE)
      rvsplit <- mean(rv)
      rvind <- rep(0,length(rv))
      rvind[rv > rvsplit] <- 1                                 #group with highraw scoobject
      Xlist <- by(object$X,rvind,function(x) x)
      names(Xlist) <- list("low","high")
    }
  }

  if (is.numeric(splitcr)) {                                 #manual raw score split
    spl.nam<-deparse(substitute(splitcr))
    if (length(splitcr)!=dim(object$X)[1]) stop("Mismatch between length of split vector and number of persons!")
    if (any(is.na(splitcr))) stop("Split vector should not contain NA's")

    rvind <- splitcr
    Xlist <- by(object$X,rvind, function(x) x)
    names(Xlist) <- as.list(sort(unique(splitcr)))
    if(is.null(spl.gr)){
      spl.lev<-names(Xlist)
      spl.gr<-paste(spl.nam,spl.lev,sep=" ")
    }

  }

  #----------item to be deleted---------------
  del.pos.l <- lapply(Xlist, function(x) {
    it.sub <- datcheck.LRtest(x,object$X,object$model)  #items to be removed within subgroup
  })

  del.pos <- unique(unlist(del.pos.l))
  if (length(del.pos) >= (ncol(object$X)-1)) {
    stop("\nNo items with appropriate response patterns left to perform LR-test!\n")
  }

  if(length(del.pos) > 0){                                                        ### begin MjM 2013-01-27
    warning(paste0(
      "\n",
      prettyPaste("The following items were excluded due to inappropriate response patterns within subgroups:"),
      "\n",
      paste(colnames(object$X)[del.pos], collapse=" "),
      "\n\n",
      prettyPaste("Full and subgroup models are estimated without these items!")
    ), immediate.=TRUE)
  }                                                                               ### end MjM 2013-01-27


  if (length(del.pos) > 0) {
    X.el <- object$X[,-(del.pos)]
  } else {
    X.el <- object$X
  }

  if(ifelse(length(splitcr) == 1, splitcr != "all.r", TRUE)){   ### begin MjM 2012-03-18   # for all cases except "all.r"
    Xlist.n <- by(X.el, rvind, function(y) y)
    names(Xlist.n) <- names(Xlist)
    if (length(del.pos) > 0) Xlist.n <- c(Xlist.n,list(X.el)) # X.el added since we must refit whole group without del.pos items
  } else {
    Xlist.n <- by(X.el[excl_0_k,], rvind[excl_0_k], function(y) y)
    names(Xlist.n) <- names(Xlist)
    Xlist.n <- c(Xlist.n,list(X.el[excl_0_k,])) # X.el added since we must refit whole group without del.pos items
  }                         ### end MjM 2012-03-18

  likpar <- sapply(Xlist.n,function(x) {                       #matrix with loglik and npar for each subgroup
    objectg <- fit_rasch(x, modelType=object$model,
                         estimation_param= estimation_param)
    likg <- objectg$loglik
    nparg <- length(objectg$etapar)
    # betalab <- colnames(objectg$X)
    list(likg,nparg,objectg$betapar,objectg$etapar,objectg$se.beta,outobj=objectg)   # rh outobj added
    ###list(likg,nparg,objectg$betapar,objectg$etapar,objectg$se.beta)   # rh outobj added
  })

  ## extract fitted splitgroup models  # rh 02-05-2010
  if(ifelse(length(splitcr) == 1, splitcr != "all.r", TRUE)){   ### begin MjM 2012-03-18
    fitobj <- likpar[6, 1:length(unique(rvind))]
  } else {
    fitobj <- likpar[6, 1:length(unique(rvind[excl_0_k]))]
  }                         ### end MjM 2012-03-18
  likpar <- likpar[-6,]

  if((length(del.pos) > 0) | ifelse(length(splitcr) == 1, splitcr == "all.r", FALSE)) {                  #re-estimate full model   ### MjM 2012-03-18
    pos <- length(Xlist.n)                    #position of the full model
    loglik.all <- likpar[1,pos][[1]]          #loglik full model
    # etapar.all <- rep(0,likpar[2,pos])         #etapar full model (filled with 0 for df computation)
    etapar.all <- rep(0, unlist(likpar[2,pos]))         #etapar full model (filled with 0 for df computation)
    likpar <- likpar[,-pos]
    Xlist.n <- Xlist.n[-pos]
  } else {
    loglik.all <- object$loglik
    etapar.all <- object$etapar
  }

  loglikg <- sum(unlist(likpar[1,]))                    #sum of likelihood value for subgroups
  LR <- 2*(abs(loglikg-loglik.all))                  #LR value
  df = sum(unlist(likpar[2,]))-(length(etapar.all))  #final degrees of freedom
  pvalue <- 1 - stats::pchisq(LR, df)                             #pvalue

  betalist <- likpar[3,]                                #organizing betalist


  result <- list(X=X.original, X.list=Xlist.n, model=object$model,LR=LR,
                 df=df, pvalue=pvalue, likgroup=unlist(likpar[1,],use.names=FALSE),
                 betalist=betalist, etalist=likpar[4,],selist=likpar[5,], spl.gr=spl.gr, call=call, fitobj=fitobj)  ## rh fitobj added
  class(result) <- "LR"

  return(result)

}

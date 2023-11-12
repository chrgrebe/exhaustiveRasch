datcheck <- function(X, W, mpoints, groupvec, model){
  #' This function makes non-exported functions from the package eRm
  #' (prettyPaste(), component.dist(), reachability(), geodist(), symmetrize(),
  #' available. The code is identical to eRm version 1.0.4 and has only been
  #' combined within this function. Authors of the code are Patrick Mair,
  #' Thomas Rusch, Reinhold Hatzinger, Marco J. Maier & Rudolf Debelak.
  #' @param X data matrix with response categories
  #' @param W design matrix
  #' @param mpoints number of meaurement points for repeated measures. Not
  #'  important for exhaustiveRasch, because only RM, PCM and RSM models
  #'  are supported.
  #' @param groupvec vector defining groups for group contrasts. Not
  #'  important for exhaustiveRasch, because only RM, PCM and RSM models
  #'  are supported.
  #' @param model an object of class RM, PCM or RSM.
  #' @return a list containing a dfata matrix without excluded items and a
  #'  value for groupcev.
  #' @export
  #' @keywords internal
  #' @useDynLib exhaustiveRasch


  # This function implements the datachecks that are executed in advance of
  # fitting eRm models, called by the fit_rasch() function. It is an internal
  # function, a call by the user is not indicated. It is nevertheless exported
  # in order to work in parallelization. However, it is not included in the
  # package documentation (roxygen2 keyword 'internal').


  ######################################################
  prettyPaste <- function(...){
    paste(strwrap(paste0(..., collapse = ""), width = getOption("width")), sep="\n", collapse="\n")
  }

  component.dist<-
    function (dat, connected = c("strong", "weak", "unilateral",
                                 "recursive"))
    {
      #   dat <- as.sociomatrix.sna(dat)
      #   if (is.list(dat))
      #       return(lapply(dat, component.dist, connected = connected))
      #   else if (length(dim(dat)) > 2)
      #       return(apply(dat, 1, component.dist, connected = connected))
      n <- dim(dat)[2]
      if (any(dat != t(dat)))
        dat <- switch(match.arg(connected), weak = symmetrize(dat,
                                                              rule = "weak"), unilateral = reachability(dat), strong = symmetrize(reachability(dat),
                                                                                                                                  rule = "strong"), recursive = symmetrize(dat, rule = "strong"))
      #   if (match.arg(connected) == "unilateral")
      #       if (any(dat != t(dat)))
      #           warning("Nonunique unilateral component partition detected in component.dist.  Problem vertices will be arbitrarily assigned to one of their components.\n")
      membership <- rep(0, n)
      membership <- .C("component_dist_R", as.double(dat), as.double(n),
                       membership = as.double(membership), PACKAGE="exhaustiveRasch")$membership
      o <- list()
      o$membership <- membership
      o$csize <- vector()
      for (i in 1:max(membership)) o$csize[i] <- length(membership[membership ==
                                                                     i])
      o$cdist <- vector()
      for (i in 1:n) o$cdist[i] <- length(o$csize[o$csize == i])
      o
    }

  #reachability - Find the reachability matrix of a graph.
  reachability<-function(dat,geodist.precomp=NULL){
    #Pre-process the raw input
    #   dat<-as.sociomatrix.sna(dat)
    #   if(is.list(dat))
    #     return(lapply(dat,reachability,geodist.precomp=geodist.precomp))
    #   else if(length(dim(dat))>2)
    #     return(apply(dat,1,reachability,geodist.precomp=geodist.precomp))
    #     return(unlist(apply(dat,1,function(x,geodist.precomp){list(reachability(x, geodist.precomp=geodist.precomp))},geodist.precomp=geodist.precomp),recursive=FALSE))
    #End pre-processing
    #Get the counts matrix
    if(is.null(geodist.precomp))
      cnt<-geodist(dat)$counts
    else
      cnt<-geodist.precomp$counts
    #Dichotomize and return
    apply(cnt>0,c(1,2),as.numeric)
  }

  #geodist - Find the numbers and lengths of geodesics among nodes in a graph
  #using a BFS, a la Brandes (2000).  (Thanks, Ulrik!)
  geodist<-function(dat,inf.replace=Inf){
    #Pre-process the raw input
    #   dat<-as.sociomatrix.sna(dat)
    #   if(is.list(dat))
    #     return(lapply(dat,geodist,inf.replace=inf.replace))
    #   else if(length(dim(dat))>2)
    #     return(apply(dat,1,geodist,inf.replace=inf.replace))
    #End pre-processing
    n<-dim(dat)[2]
    #Initialize the matrices
    sigma<-matrix(0,nrow=n,ncol=n)
    gd<-matrix(Inf,nrow=n,ncol=n)
    #Perform the calculation
    geo<-.C("geodist_R",as.double(dat),as.double(n),gd=as.double(gd), sigma=as.double(sigma),NAOK=TRUE,PACKAGE="exhaustiveRasch")
    #Return the results
    o<-list()
    o$counts<-matrix(geo$sigma,n,n)
    o$gdist<-matrix(geo$gd,n,n)
    o$gdist[o$gdist==Inf]<-inf.replace  #Patch Infs, if desired
    o
  }

  #symmetrize - Convert a graph or graph stack to a symmetric form.  Current rules
  #for symmetrizing include "upper" and "lower" diagonals, "weak" connectedness
  #rule, and a "strong" connectedness rule.
  symmetrize<-function(mats,rule="weak"){
    #Pre-process the raw input
    #   mats<-as.sociomatrix.sna(mats)
    #   if(is.list(mats))
    #     return(lapply(mats,symmetrize,rule=rule))
    #End pre-processing
    #Build the input data structures
    #   if(length(dim(mats))>2){
    #      m<-dim(mats)[1]
    #      n<-dim(mats)[2]
    #      o<-dim(mats)[3]
    #      d<-mats
    #   }else{
    m<-1
    n<-dim(mats)[1]
    o<-dim(mats)[2]
    d<-array(dim=c(1,n,o))
    d[1,,]<-mats
    #   }
    #Apply the symmetry rule
    for(i in 1:m){
      if(rule=="upper"){
        #         temp<-d[i,,]
        #         for(j in 1:n)
        #            temp[j:n,j]<-temp[j,j:n]
        #         d[i,,]<-temp
        #      }else if(rule=="lower"){
        #         temp<-d[i,,]
        #         for(j in 1:n)
        #            temp[j,j:n]<-temp[j:n,j]
        #         d[i,,]<-temp
        #      }else if(rule=="weak"){
        #         d[i,,]<-matrix(as.numeric(d[i,,]|t(d[i,,])),nrow=n,ncol=o)
      }else if(rule=="strong"){
        d[i,,]<-matrix(as.numeric(d[i,,]&t(d[i,,])),nrow=n,ncol=o)
      }
    }
    #Return the symmetrized matrix
    if(m==1)
      out<-d[1,,]
    else
      out<-d
    out
  }
  ######################################################


  if(is.data.frame(X)){
    X <- as.matrix(X)   # X as data frame allowed
  }

  if(is.null(colnames(X))){                                 #determine item names
    if(mpoints > 1){
      mpind <- paste("t",rep(1:mpoints,each=(ncol(X)/mpoints),1),sep="") #time points
      itemind <- paste("I",1:(ncol(X)/mpoints),sep="")
      colnames(X) <- paste(itemind,mpind)
    } else {
      colnames(X) <- paste("I",1:ncol(X),sep="")                         #item labels
    }
  }
  if(is.null(rownames(X))) rownames(X) <- paste0("P", seq_len(nrow(X)))   #person labels

  #----------------------- check groupvec --------------------------

  if((length(groupvec) > 1L) && (length(groupvec) != nrow(X))){
    stop("Wrong specification of groupvec!")
  }

  if(min(groupvec) != 1L){
    stop("Group specification must start with 1!")
  }

  if(length(unique(groupvec)) != (max(groupvec))){
    stop("Group vector is incorrectly specified (perhaps a category is missing)!")   # rh 2011-03-03
  }

  if((max(groupvec) > 1L) && (mpoints == 1)){
    stop(paste0("\n", prettyPaste("Model not identifiable! Group contrasts can only be imposed for repeated measurement designs.")))
  }

  #  if ((length(groupvec) > 1) && any(is.na(X))) {
  #    stop("Model with repeated measures, group specification and NAs cannot be computed!") }

  #----------------------- check X --------------------------------
  allna.vec <- apply(X,2,function(y) {all(is.na(y))})                 #eliminate items with all NA's
  if (any(allna.vec)) {stop("There are items with full NA responses which must be deleted!")}

  allna.vec <- apply(X,1,function(y) {all(is.na(y))})                 #eliminate items with all NA's
  if (any(allna.vec)) {stop("There are persons with full NA responses which must be deleted!")}

  allna.vec <- apply(X,1,function(y) {sum(is.na(y))})
  if (any(allna.vec == (ncol(X)-1L))) {stop("Subjects with only 1 valid response must be removed!")}

  ri.min <- apply(X,2,min,na.rm=TRUE)                                 #if no 0 responses
  if(any(ri.min > 0)){
    warning(paste0(
      "\n",
      prettyPaste("The following items have no 0-responses:"),
      "\n",
      paste(colnames(X)[ri.min > 0], collapse=" "),
      "\n",
      prettyPaste("Responses are shifted such that lowest category is 0.")
    ), call. = FALSE, immediate.=TRUE)
  }
  X <- t(apply(X,1,function(y) {y-ri.min}))                           #shift down to 0

  ri <- apply(X,2,sum,na.rm=TRUE)                                     #item raw scores
  n.NA <- colSums(apply(X,2,is.na))                                   #number of NA's per column
  maxri <- (dim(X)[1]*(apply(X,2,max,na.rm=TRUE)))-n.NA               #maximum item raw scores with NA
  TFcol <- ((ri==maxri) | (ri==0))
  X.n <- X[,!TFcol]                                                   #new matrix with excluded items
  item.ex <- (seq_len(ncol(X)))[TFcol]                                     #excluded items
  if(length(item.ex) > 0) {
    if(mpoints == 1){
      warning(paste0(
        "\n",
        prettyPaste("The following items were excluded due to complete 0/full responses:"),
        "\n",
        paste(colnames(X)[item.ex], collapse=" ")
      ), call. = FALSE, immediate.=TRUE)
    } else {
      stop(paste0(
        "\n",
        "The following items show complete 0/full responses:",
        "\n",
        paste(colnames(X)[item.ex], collapse=" "),
        "\n",
        prettyPaste("Estimation cannot be performed! Delete the corresponding items for the other measurement points as well!")
      ), call. = FALSE)
    }
  }

  if ((model=="PCM") || (model=="LPCM")) {                         #check if there are missing categories for PCM (for RSM doesn't matter)
    tablist <- apply(X,2,function(x) list(as.vector(table(x))))
    tablen <- sapply(tablist,function(x) length(x[[1]]))
    xmax <- apply(X,2,max)+1
    indwrong <- which(tablen != xmax)
    if(length(indwrong) > 0){
      warning(paste0(
        "\n",
        prettyPaste("The following items do not have responses on each category:"),
        "\n",
        paste(colnames(X)[indwrong], collapse=" "),
        "\n",
        prettyPaste("Estimation may not be feasible. Please check data matrix!")
      ), call. = FALSE, immediate.=TRUE)
    }
  }


  #-------------------------- ill conditioned for RM and LLTM --------------
  if ((model=="RM") || (model=="LLTM")) {
    if (length(table(X.n)) != 2L) stop("Dichotomous data matrix required!")
    k.t   <- dim(X.n)[2L]/mpoints                                    #check for each mpoint separately
    t.ind <- rep(seq_len(mpoints), 1L, each=k.t)
    X.nlv <- split(t(X.n),t.ind)                                  #split X due to mpoints
    cn.lv <- split(colnames(X.n),t.ind)
    X.nl  <- lapply(X.nlv,matrix,ncol=k.t,byrow=TRUE)
    for(i in seq_len(length(X.nl))) colnames(X.nl[[i]]) <- cn.lv[[i]]

    for(l in seq_len(mpoints)){                                       #check within mpoints
      X.nll <- X.nl[[l]]
      k <- ncol(X.nll)
      adj <- matrix(0, ncol=k, nrow=k)
      for(i in seq_len(k)) for(j in seq_len(k)) {
        adj[i,j]<- 1*any(X.nll[,i] > X.nll[,j], na.rm = TRUE)
      }
      cd  <- component.dist(adj, connected = "strong")
      cm  <- cd$membership
      cmp <- max(cm)
      if(cmp > 1L) {
        cmtab <- table(cm)
        maxcm.n <- as.numeric(names(cmtab)[cmtab!=max(cmtab)])
        suspcol <- (seq_len(length(cm)))[tapply(cm, seq_len(length(cm)), function(x){ any(maxcm.n == x) })]
        n.suspcol <- colnames(X.nll)[suspcol]
        stop(paste0(
          "\n",
          prettyPaste(" Estimation stopped due to ill-conditioned data matrix X! Suspicious items:"),
          "\n",
          paste(n.suspcol, collapse=" ")
        ), call. = FALSE)
      }
    }
  }
  #----------------------- end ill-conditioned check -------------------------------

  return(list(X = X.n, groupvec = groupvec))

}


datcheck.LRtest <- function(x, X, model){
  #' Sanity checks for LRtest. This function makes the non-exported function
  #' LRtest() from the package eRm available. The code is identical to eRm
  #' version 1.0.4 and has only been combined within this function. Authors of
  #' the code are Patrick Mair, Thomas Rusch, Reinhold Hatzinger,
  #' Marco J. Maier & Rudolf Debelak.
  #' @param x submatrix (splitted with "splitcr" and called within Xlist)
  #' @param X original data matrix (from model fit)
  #' @param model model of type RM, PCM or RSM
  #' @return vector with items to be eliminated
  #' @export
  #' @keywords internal


  exclude <- NULL                                             #vector with items to be excluded

  #----check full/0 responses------
  n.NA <- colSums(apply(X,2,is.na))                                   #number of NA's per column
  maxri <- (dim(X)[1]*(apply(X,2,max,na.rm=TRUE)))-n.NA               #maximum item raw scores with NA
  ri <- apply(x,2,sum,na.rm=TRUE)                              #item raw scores
  exclude <- c(exclude,which((ri==maxri) | (ri==0)))

  #----check full(-1) NA's---------
  allna.vec <- apply(x,2,function(y) {
    naTF <- is.na(y)
    (sum(naTF) >= length(y-1))
  })
  exclude <- c(exclude,which(allna.vec))

  #----minimum category = 0--------
  ri.min <- apply(x,2,min,na.rm=TRUE)                                 #if no 0 responses
  exclude <- c(exclude,which(ri.min!=0))

  #----RSM-checks for same number of categories--------
  if ((model == "RSM") || (model == "LRSM")) {
    highcat <- max(X, na.rm=TRUE)                    #highest category in original data
    highcat.sub <- apply(x,2,max,na.rm=TRUE)             #RSM check for equal number of categories
    exclude <- c(exclude,which(highcat.sub != highcat))
  }

  #---PCM checks for all categories responses---------
  if ((model=="PCM") || (model=="LPCM")) {                         #check if there are missing categories for PCM (for RSM doesn't matter)
    cat.data <- apply(X,2,function(y) list(unique(stats::na.exclude(y)))) #categories of orginal data
    cat.sub <- apply(x,2,function(y) list(unique(stats::na.exclude(y))))  #categories of subgroup data
    catcomp <- mapply(function(y.s,y.d) {
      (length(y.s[[1]]) == (length(y.d[[1]])))
    },cat.sub,cat.data)
    exclude <- c(exclude,which(!catcomp))
  }

  return(unique(exclude))             #return vector with items to be eliminated
}

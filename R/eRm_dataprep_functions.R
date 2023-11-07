dataprep <- function(X,W,sum0, modelType){
  #' This function makes non-exported functions from the package eRm
  #' (datprep_RM(), datprep_PCM(), datprep_RSM() available. The code is
  #' identical to eRm version 1.0.4 and has only been
  #' combined within this function using if-clauses for the respective model
  #' types. Authors of the code are Patrick Mair, Thomas Rusch, Reinhold
  #' Hatzinger, Marco J. Maier & Rudolf Debelak.
  #' @param X data matrix with response categories to be converted into
  #' 0/1 matrix
  #' @param W Design matrix
  #' @param sum0 If TRUE, the parameters are normed to sum-0 by specifying an
  #' appropriate W. If FALSE, the first parameter is restricted to 0.
  #' @param modelType a character value defining the rasch model to fit.
  #'  Possible values: RM, PCM, RSM
  #' @return a list containig the values for X, X01,mt_vek W)
  #' @export
  #' @keywords internal
  if (modelType=="RM"){
    X01 <- X                                        #X is already X(0,1)

    mt_vek <- rep(1,dim(X01)[2])                    #number of categories for each item
    K <- length(mt_vek)

    #automatized generation of the design matrix W
    if (length(W)==1) {
      W1 <- diag(1,(K-1))                           #build up design matrix
      if (sum0) {
        w1 <- rep(-1,(K-1))                         #sum0 restriction
      } else {
        w1 <- rep(0,(K-1))                          #first item parameter set to 0
      }
      W <- rbind(w1,W1)                             #RM design matrix
      colnames(W) <- NULL
      rownames(W) <- NULL
    }
  }else if (modelType=="PCM"){
    #converting into 0/1 matrix
    N <- dim(X)[1]                                  #number of persons
    mt_vek <- apply(X,2,max,na.rm=TRUE)             #number of categories - 1 for each item
    mt_vek_0 <- mt_vek+1                            #number of categories for each item
    X01_0 <- matrix(rep(0,(N*sum(mt_vek_0))),nrow=N)#empty 0/1 matrix
    K <- length(mt_vek)                             #number of items
    cummt0 <- c(0,cumsum(mt_vek_0)[1:(K-1)])+1      #index vector for 0th category
    indmatp <- apply(X,1,function(xi) {xi+cummt0})  #preparing index matrix for 1 responses
    imp1 <- as.vector(indmatp)
    imp2 <- rep(1:N,rep(K,N))
    indmat <- cbind(imp2,imp1)                      #final index matrix for 1 responses
    X01_0[indmat] <- 1                              #0/1 matrix with 0th category

    NAindmat <- rbind(imp2,rep(1:K,N),c(t(X)))         #impose NA structure
    rownames(NAindmat) <- NULL
    NAind <- t(NAindmat[1:2,is.na(NAindmat[3,])])      #index matrix for NA's in X

    if (length(NAind) > 0) {
      NAindlist <- apply(NAind,1,function(x){
        co <- seq(cummt0[x[2]],cummt0[x[2]]+mt_vek[x[2]])
        NAind01 <- cbind(rep(x[1],length(co)),co)
        data.frame(NAind01,row.names=NULL)                                               #list with NA indices
      })
      indmatNA <- matrix(unlist(lapply(NAindlist, function(x) {t(as.matrix(x))})),ncol=2,byrow=TRUE)   #matrix with NA indices
      X01_0[indmatNA] <- NA
    }

    X01 <- X01_0[,-cummt0]                          #delete 0-category answers --> final 0/1 pattern matrix (dim N*sum(mt_vek))


    #automatized generation of the design matrix W
    if (length(W)==1) {
      W1 <- diag(1,(sum(mt_vek)-1))                   #build up design matrix
      if (sum0) {
        w1 <- rep(-1,(sum(mt_vek)-1))                         #sum0 restriction
      } else {
        w1 <- rep(0,(sum(mt_vek)-1))                          #first item parameter set to 0
      }
      W <- rbind(w1,W1)                               #PCM design matrix
      colnames(W) <- NULL
      rownames(W) <- NULL
    }

  }else if (modelType=="RSM"){
    max.it <- apply(X,2,max,na.rm=TRUE)             #RSM check for equal number of categories
    if (length(table(max.it)) > 1) stop("RSM can not be computed since number of categories are not the same for each item!\n")

    N <- dim(X)[1]                                  #number of persons
    K <- dim(X)[2]                                  #number of items
    hmax <- max(X,na.rm=TRUE)                       #highest category
    mt_vek <- rep(hmax,K)                           #vector with number of categories - 1 for each item

    mt_vek_0 <- mt_vek+1                            #number of categories for each item
    X01_0 <- matrix(rep(0,(N*sum(mt_vek_0))),nrow=N) #empty 0/1 matrix
    K <- length(mt_vek)
    cummt0 <- c(0,cumsum(mt_vek_0)[1:(K-1)])+1      #index vector for 0th category
    indmatp <- apply(X,1,function(xi) {xi+cummt0})  #preparing index matrix for 1 responses
    imp1 <- as.vector(indmatp)
    imp2 <- rep(1:N,rep(K,N))
    indmat <- cbind(imp2,imp1)                      #final index matrix for 1 responses
    X01_0[indmat] <- 1                              #0/1 matrix with 0th category

    NAindmat <- rbind(imp2,rep(1:K,N),c(t(X)))         #impose NA structure
    rownames(NAindmat) <- NULL
    NAind <- t(NAindmat[1:2,is.na(NAindmat[3,])])      #index matrix for NA's in X

    if (length(NAind) > 0) {
      NAindlist <- apply(NAind,1,function(x){
        co <- seq(cummt0[x[2]],cummt0[x[2]]+mt_vek[x[2]])
        NAind01 <- cbind(rep(x[1],length(co)),co)
        data.frame(NAind01,row.names=NULL)                                               #list with NA indices
      })
      indmatNA <- matrix(unlist(lapply(NAindlist, function(x) {t(as.matrix(x))})),ncol=2,byrow=TRUE)   #matrix with NA indices
      X01_0[indmatNA] <- NA
    }

    X01 <- X01_0[,-cummt0]                          #delete 0-category answers --> final 0/1 pattern matrix (dim N*sum(mt_vek))

    #automatized generation of the design matrix W
    if (length(W)==1) {
      e_it <- gl(K,hmax)                              #factor for item parameters
      e_cat <- gl(hmax,1,K*hmax)                      #factor for category par

      if (sum0) {
        Xm <- stats::model.matrix(~e_it+e_cat)[,-1]          #dummy coding
        Xm[1:hmax,1:(K-1)] <- -1                      #first item to be sum0 normalized
      } else {
        Xm <- stats::model.matrix(~e_it+e_cat)[,-1]          #design matrix with 0/1 contrasts (without intercept)
      }

      catvek <- 1:hmax                                #preparing the item design vectors
      e_itnew <- catvek*Xm[,1:(K-1)]
      Xm[,1:(K-1)] <- e_itnew
      W <- Xm                                         #final design matrix
      colnames(W) <- NULL
      rownames(W) <- NULL
    }


    #Output: X01      ... 0/1 response matrix of dimension N*rtot
    #        mt_vek   ... vector of length K with number of categories - 1 (for each item)
    #        W        ... design matrix of dimension sum(mt_vek)*((K-1)+(hmax-1))
  }
  return(list(X=X,X01=X01,mt_vek=mt_vek,W=W))
}


parallized_tests <- function(dset,
                             modelType,
                             combos,
                             models,
                             na.rm,
                             testfunction,
                             itemfit_param,
                             splitcr=NULL,
                             alpha,
                             bonf,
                             DIFvars,
                             gap_prop,
                             max_contrast,
                             extremes,
                             ignoreCores,
                             ...){
  #' conducts and controls the parallelisation of the tests, Intentionally,
  #'  there are no defauklt values for the parameters, as this internal
  #'   function is called by \link{exhaustive_tests} that also defines the
  #'    default values for this function.
  #' @param dset a data.frame containing the data
  #' @param modelType a character value defining the rasch model to fit.
  #'  Possible values: RM, PCM, RSM
  #' @param combos either 1) a list of item combinations to be tested, e.g.
  #'  from \link{apply_combo_rules} or the passed_combos slot of an object
  #'   of \link{passed_exRa-class} from a previous call to this function.
  #'    Or 2) an object of \link{passed_exRa-class}. In this case, the
  #'     previously fit models froms its passed_Models slot will also be used
  #'      and will be passed to the test functions. Thhis will speed up the
  #'       analysis. If the parameter is NULL, all possible combinations of
  #'        the items (columns) in dset will be tested
  #' @param na.rm a boolean value. If TRUE, in the respective item combination
  #'  all cases with any NA are removed (na.omit). If FALSE, only cases with
  #'   full NA responses are removed. NOTE: \link{test_mloef} currently does
  #'    not allow for missing values (because erm::MLoef doesn't).
  #'     If \link{test_mloef} is under the tests to perform, na.rm will
  #'      automatically be set TRUE for ALL tests.
  #' @param testfunction a character defining the actual test (the internal
  #'  testfunction) to perform. Possible values: all_rawscores, test_itemfit,
  #'   test_LR, test_mloef, test_waldtest, threshold_order, test_DIFtree,
  #'    test_personsItems, test_respca.
  #' @param itemfit_param a list from \link{itemfit_control} with options
  #'  for \link{test_itemfit}
  #' @param splitcr the split criterion to use, if the actual testfunction
  #'  is test_LR or test_waldtest.Split criterion for subject raw score
  #'   splitting. "all.r" corresponds to a full raw score split, "median"
  #'    uses the median as split criterion, "mean" performs a mean split.
  #'     Optionally splitcr can also be a vector which assigns each person
  #'      to a certain subgroup (e.g., following an external criterion).
  #'       This vector can be numeric, character or a factor.
  #' @param alpha a numeric value for the alpha level. Will be ignored for
  #'  \link{test_itemfit} if use.pval in \link{itemfit_control} is FALSE
  #' @param bonf a boolean value wheter to use a Bonferroni correction.
  #'  Will be ignored if use.pval is FALSE
  #' @param DIFvars a data.frame containing the variables and their data to use
  #'  for differential item functioning analysis with \link{test_DIFtree}
  #' @param gap_prop a numeric value between 0 and 1 that sets the criterion
  #'  for the minimum proportion of neighboring person parameters with an
  #'   item/threshold location in between. If set to 0, this criterion will not
  #'    be checked (used in test_personsItems only)
  #' @param extremes a boolean value indicating if a check for the
  #'  item/threshold locations left of the 2nd lowest and right of the
  #'   2nd highest person parameter (used in test_personsItems only).
  #' @param max_contrast a numeric value defining the maximum loading of a
  #'  factor in the principal components analysis of the standardised residuals.
  #'  Only relevant, if test_respca is one of the tests.
  #' @param ignoreCores a numeric value for the number of cpu cores to hold out
  #'  in parallelizing the test run.
  #' @param ... options for \link{itemfit_control} can be passed directly
  #'  to this function.
  #' @return a list containing a) a list of item combinations that passed the
  #'  actual test; and b) a list containing the fit models
  #'   of type RM, PCM or RSM.
  #' @export
  #' @keywords internal

  arguments <- list(...)
  # catch, if no itemcombinations are handed over or if a combos is a character
  # that indicates a warning message.

  if (length(combos)==0 | is.character(combos)){
    warning(paste("No item combinations left to perform ", testfunction,
                  ". Aborted.", sep=""))
  } else{
    chk <- Sys.getenv("_R_CHECK_LIMIT_CORES_", "")
    if (nzchar(chk) && chk == "TRUE") {
      # use 2 cores in CRAN/Travis/AppVeyor
      cl <- parallel::makePSOCKcluster(2L)
    } else {
      # use all cores in devtools::test()
      cl <- parallel::makePSOCKcluster(parallel::detectCores()- ignoreCores)
    }
    parallel::setDefaultCluster(cl)
    parallel::clusterExport(cl, testfunction)
    parallel::clusterEvalQ(cl, library(eRm))
    parallel::clusterEvalQ(cl, library(psych))
    parallel::clusterEvalQ(cl, library(psychotree))

    if (!is.null(models)){
      modelcombo_pairs <- lapply(seq_len(length(combos)), function(x){
        list(combos[[x]], models[[x]])})
      param1 <- list(cl=cl, X=modelcombo_pairs, dset=dset,
                     modelType=modelType, na.rm=na.rm, fun= testfunction)
    } else{
      param1 <- list(cl=cl, X=combos, dset=dset, modelType=modelType,
                     na.rm=na.rm, fun= testfunction)
    }

    if (testfunction=="test_itemfit"){
      param1$control <- itemfit_param
    }
    if (!is.null(splitcr) & (testfunction=="test_mloef" | testfunction==
                             "test_LR" | testfunction=="test_waldtest")){
      param1$splitcr <- splitcr
    }
    if (testfunction %in% c("test_LR", "test_waldtest")){
      param1$alpha <- alpha
      param1$bonf <- bonf
    }
    if (testfunction=="test_DIFtree"){
      param1$DIFvars <- DIFvars
    }
    if (testfunction=="test_personsItems"){
      param1$gap_prop <-gap_prop
      param1$extremes <- extremes
    }
    a <- do.call(parallel::parLapply, param1)
    parallel::stopCluster(cl)
    a[sapply(a, is.null)] <- NULL
    return(a)
  }
}

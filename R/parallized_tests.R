parallized_tests <- function(dset,
                             modelType,
                             combos,
                             models,
                             p.par,
                             na.rm,
                             testfunction,
                             itemfit_param,
                             splitcr=NULL,
                             icat_wald,
                             alpha,
                             bonf,
                             DIFvars,
                             gap_prop,
                             max_contrast,
                             PSI,
                             extremes,
                             ignoreCores,
                             estimation_param,
                             tests_count,
                             verbose=TRUE,
                             ...){
  # This is an internal function that is not intended to be called by users.
  # It is nevertheless exported so that it can be run in the parallelization
  # workers. However, the function is not documented in the manual.

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
  #'      and will be passed to the test functions. This will speed up the
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
  #' @param icat_wald a boolean value indicating if the waldtest will be
  #' conducted on item level (TRUE, default value) or on item category level.
  #' This parameter only effects estimations using psychotools or pairwise and
  #' will be ignored for eRm estimations.
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
  #' @param estimation_param options for parameter estimation using
  #' \link{estimation_control}
  #' @param verbose a boolean value. If set to FALSE, all output during the
  #'  analysis will be suppressed.
  #' @return a list containing 3 elements is returned: a list of the item
  #'  combinations, a list of the models (depending on modelType and
  #'  estimation_param$est) with the fit models and a list with the person
  #'  parameter objects (depending on estimation_param$est).
  #' @export
  #' @keywords internal

  # This function implements the parallilization of the tests. It is an internal
  # function, a call by the user is not indicated. It is nevertheless exported
  # in order to work in parallelization. However, it is not included in the
  # package documentation (roxygen2 keyword 'internal').

  arguments <- list(...)
  # catch, if no item combinations are handed over or if a combos is a character
  # that indicates a warning message.

  if (length(combos)==0 | is.character(combos)){
    warning(paste("No item combinations left to perform ", testfunction,
                  ". Aborted.", sep=""))
  } else{
    if (isTRUE(as.logical(Sys.getenv("_R_CHECK_LIMIT_CORES_")))){
      # on cran
      ncores <- 2L
      # use 2 cores in CRAN/Travis/AppVeyor
      cl <- parallel::makePSOCKcluster(2L)
    } else {
      # use all cores in devtools::test()
      cl <- parallel::makePSOCKcluster(parallel::detectCores()- ignoreCores)
    }
    on.exit(parallel::stopCluster(cl))
    parallel::setDefaultCluster(cl)
    #    parallel::clusterExport(cl, c(testfunction, "fit_rasch", "Mloef",
    #                                  "datcheck", "dataprep", "LRtest",
    #                                  "Waldtest", "datcheck.LRtest",
    #                                  "ppar.psy", "expscore", "pvx.matrix", "pvx",
    #                                  "mloef.psy", "waldtest.psy"))
    parallel::clusterExport(cl, c(testfunction, "fit_rasch",
                                  "ppar.psy", "expscore", "pvx.matrix", "pvx",
                                  "mloef.psy", "waldtest.psy", "LRtest.psy",
                                  "expscore.psy", "pvx.super"))

    parallel::clusterEvalQ(cl, library(eRm))
    parallel::clusterEvalQ(cl, library(psych))
    parallel::clusterEvalQ(cl, library(psychotree))
    parallel::clusterEvalQ(cl, library(psychotools))
    parallel::clusterEvalQ(cl, library(pairwise))


    if (!is.null(models)){
      modelcombo_pairs <- lapply(seq_len(length(combos)), function(x){
        list(combos[[x]], models[[x]], p.par[[x]])})
      param1 <- list(cl=cl, X=modelcombo_pairs, dset=dset,
                     modelType=modelType, na.rm=na.rm,
                     estimation_param=estimation_param,
                     FUN= testfunction)
    } else{
      param1 <- list(cl=cl, X=combos, dset=dset, modelType=modelType,
                     na.rm=na.rm, estimation_param=estimation_param,
                     FUN= testfunction)
    }

    if (testfunction=="test_itemfit"){
      param1$control <- itemfit_param
    }
    if (!is.null(splitcr) & (testfunction=="test_mloef" | testfunction==
                             "test_LR" |
                             testfunction=="test_waldtest" |
                             testfunction=="test_Qtest")){
      param1$splitcr <- splitcr
    }
    if (testfunction %in% c("test_itemfit", "test_waldtest")){
      param1$bonf <- bonf
    }
    if (testfunction %in% c("test_LR", "test_waldtest", "test_mloef",
                            "test_Qtest")){
      param1$alpha <- alpha
    }
    if (testfunction=="test_waldtest" & estimation_param$est !="eRm"){
      param1$icat <-icat_wald
    }
    if (testfunction=="test_DIFtree"){
      param1$DIFvars <- DIFvars
    }
    if (testfunction=="test_personsItems"){
      param1$gap_prop <-gap_prop
      param1$extremes <- extremes
    }
    if (testfunction=="test_PSI"){
      param1$PSI <-PSI
    }
    if (verbose){
      print(paste("computing process ", tests_count[1],"/", tests_count[2],": ",
                testfunction, sep=""))
    }

    if (verbose){
      pbapply::pboptions(type="txt")
    } else{
      pbapply::pboptions(type="none")
    }
    a <- do.call(pbapply::pblapply, param1)
    a[sapply(a, is.null)] <- NULL
    return(a)
  }
}

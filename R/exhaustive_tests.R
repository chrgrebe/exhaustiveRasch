exhaustive_tests <- function(dset,
                             modelType="PCM",
                             combos=NULL,
                             scale_length=4:length(dset),
                             na.rm=TRUE,
                             tests=c("no_test"),
                             splitcr_mloef=NULL,
                             splitcr_LR=NULL,
                             splitcr_wald=NULL,
                             #splitcr_Q=NULL,
                             icat_wald=FALSE,
                             alpha=0.1,
                             bonf=FALSE,
                             DIFvars=NULL,
                             gap_prop=0,
                             extremes=TRUE,
                             max_contrast=NULL,
                             PSI=0.8,
                             ICs=FALSE,
                             keep=FALSE,
                             ignoreCores=1,
                             silent=FALSE,
                             ...,
                             itemfit_param=NULL,
                             estimation_param=NULL
){
  #' (main function) Runs exhaustive tests
  #' @param dset a data.frame containing the data
  #' @param modelType a character value defining the rasch model to fit.
  #'  Possible values: RM, PCM, RSM
  #' @param combos either 1) a list of item combinations to be tested,
  #'  e.g. from \link{apply_combo_rules} or the passed_combos slot of an object
  #'   of \link{passed_exRa-class} from a previous call to this function. Or 2)
  #'    an object of \link{passed_exRa-class}. In this case, the previously fit
  #'     models froms its passed_Models slot will also be used and will be
  #'      passed to the test functions. This will speed up the analysis. If
  #'       the parameter is NULL, all possible combinations of the items
  #'        (columns) in dset will be tested
  #' @param scale_length a numeric vector defining the length of the item
  #'  combinations to test
  #' @param na.rm a boolean value. If TRUE, in the respective item combination
  #'  all cases with any NA are removed (na.omit). If FALSE, only cases
  #'   with full NA responses are removed. NOTE: \link{test_mloef} currently
  #'   does not allow for missing values (because erm::MLoef doesn't). If
  #'      \link{test_mloef} is under the tests to perform, na.rm will
  #'       automatically be set TRUE for ALL tests.
  #' @param tests a vector of characters defining the tests to perform.
  #'  Possible values: all_rawscores, test_itemfit, test_LR, test_mloef,
  #'   test_waldtest, threshold_order, test_DIFtree, test_personsItems,
  #'    test_respca. Tests will be performed in the given order.
  #' @param splitcr_LR as defined by eRm::LRtest. Split criterion for subject
  #'  raw score splitting. "all.r" corresponds to a full raw score split,
  #'   "median" uses the median as split criterion, "mean" performs a
  #'    mean split. Optionally splitcr can also be a vector which assigns each
  #'     person to a certain subgroup (e.g., following an external criterion).
  #'      This vector can be numeric, character or a factor.
  #' @param splitcr_mloef as defined by eRm::MLoef: Split criterion to define
  #'  the item groups. "median" and "mean" split items in two groups based on
  #'   their items' raw scores. splitcr can also be a vector of length k
  #'    (where k denotes the number of items) that takes two or more distinct
  #'     values to define groups used for the Martin-Löf Test.
  #' @param splitcr_wald as defined by eRm::Waldtest: Split criterion for
  #'  subject raw score splitting. median uses the median as split criterion,
  #'   mean performs a mean-split. Optionally splitcr can also be a dichotomous
  #'    vector which assigns each person to a certain subgroup (e.g., following
  #'     an external criterion). This vector can be numeric, character or
  #'      a factor.
  #' @param icat_wald a boolean value indicating if the waldtest will be
  #' conducted on item level (TRUE, default value) or on item category level.
  #' This parameter only effects estimations using psychotools or pairwise and
  #' will be ignored for eRm estimations.
  #' @param alpha a numeric value for the alpha level. Will be ignored
  #' for \link{test_itemfit} if use.pval in \link{itemfit_control} is FALSE
  #' @param bonf a boolean value whether to use a Bonferroni correction.
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
  #' @param PSI a numeric value defining the minimum value for the person-
  #' separation-index (separation reliablility).
  #' @param ICs a boolean value defining whether to compute information criteria
  #' for the remaining models. You can add these later to the object of class
  #'  \link{passed_exRa-class} by using the \link{add_ICs} function.
  #' @param keep a boolean value difining whether des person parameters will be
  #'  part of the \link{passed_exRa-class} results object (TRUE) or not (FALSE).
  #'  Keeping the person parameters will result in shorter runtimes, if several
  #'  tests that   #'  need these parameters are used. On the other hand there
  #'  will be a largeramount of memeory usage.
  #' @param ignoreCores a numeric value for the number of cpu cores to hold out
  #'  in parallelizing the test run.
  #' @param silent a boolean value. If set to TRUE, all output during the
  #'  analysis will be suppressed.
  #' @param ... arguments for \link{itemfit_control}
  #'  and \link{estimation_control}can be passed directly to this function.
  #' @param itemfit_param a list from \link{itemfit_control} with options
  #' for \link{test_itemfit}
  #' @param estimation_param options for parameter estimation using
  #' \link{estimation_control}
  #' @return an object of \link{passed_exRa-class}.
  #' @export
  #' @examples \dontrun{
  #'   passed <- exhaustive_tests(ADL[c(1,4,6,7,10,14,15)],
  #'     modelType = "RM", scale_length = 4:5, upperMSQ=1.5, lowerMSQ=0.5,
  #'     tests=c("test_itemfit", "test_mloef", "test_respca"))
  #'  }

  # get lowest and highest category
  maxcat<-apply(dset,2,function(x){max(x,na.rm=TRUE)})
  mincat <-apply(dset,2,function(x){min(x,na.rm=TRUE)})
  # if necessary, shift all categories to start with 0
  if (min(mincat)>0){
    dset <- as.data.frame(apply(dset,2,function(x){x-min(mincat)}))
    cat(paste("item-categories did not start with 0. Categories were shifted to
              set the first category 0. Consider inspecting your data matrix",
              "\n"))
  }

  # pass optional arguments to itemfit_control() and estimation_control()
  extraArgs <- list(...)
  if (length(extraArgs)) {
    allowed_args <- names(c(formals(itemfit_control),
                            formals(estimation_control)))
    indx <- match(names(extraArgs), allowed_args, nomatch = 0L)
    if (any(indx == 0L))
      cat(paste(gettextf("Argument %s was ignored. It is neither an argument
        of itemfit_control() nor of estimation_control() and could not be
                           matched", names(extraArgs)[indx == 0L],
                         domain = NA)), "\n")
  }
  itemfit_extraArgs <- extraArgs[intersect(
    names(extraArgs), names(formals(itemfit_control)))]
  estimation_extraArgs <- extraArgs[intersect(
    names(extraArgs), names(formals(estimation_control)))]

  if (missing(itemfit_param)){
    itemfit_param <- do.call(itemfit_control, itemfit_extraArgs)
  }
  if (missing(estimation_param)){
    estimation_param <- do.call(estimation_control, estimation_extraArgs)
  }

  # some checks for not allowed test
  if (na.rm==FALSE & "test_mloef" %in% tests){
    na.rm <- TRUE
    cat(paste("test_mloef is part of the test. This test does not currently
    allow for missing values, so na.rm was set TRUE for all tests.", "\n"))
  }

  if ("threshold_order" %in% tests & modelType=="RM"){
    tests <- tests[-which("threshold_order" %in% tests)]
    cat(paste("threshold_order is part of the test. This test is not meaningful
            for the dichotomous rasch model and was removed from the list of
            tests.", "\n"))
  }

  if(estimation_param$est=="pairwise" & "test_mloef" %in% tests){
    tests <- tests[-which("test_mloef" %in% tests)]
    cat(paste("test_mloef is part of the test. This test is currently not
            supported for 'pairwise' estimation and was removed from 'tests'.",
              "\n"))
  }


  if (length(tests)>0){ # stop execution, if no test is left
    # loop over item combinations with scale length j
    passed_models <- list()
    passed_combos <- list()
    passed_p.par <- list()
    process <- data.frame()


    #    if(!estimation_param$est=="eRm" & modelType=="RSM"){
    #      estimation_param$est <- "eRm"
    #      cat(paste("estimation of RSM models using est='psychotools' is not yet
    #              supported. Argument 'est' was set to 'eRm'",
    #                "\n"))
    #    }


    if (!is.null(combos)){
      scale_length <-1:1
      if (inherits(combos,"passed_exRa")){
          current_models <- combos@passed_models
          current_p.par <- combos@passed_p.par
          combos <- combos@passed_combos
      }else{
        current_models <- NULL
        current_p.par <- NULL
      }
    }

    timings <- data.frame(matrix(vector(), 0, 3))
    for (j in scale_length){
      information_criteria <- list()
      if (!is.null(combos)){
        if (silent==FALSE){
          cat("Scale-Length ")
          cat(unlist(unique(lapply(seq_len(length(combos)),
                                   function(x) length(combos[[x]])))))
          cat("; pre-defined set of item combinations
              ('combos' parameter was used)", "\n")
        }
      } else{
        if (silent==FALSE){cat("Scale-Length: ", j, "\n", sep="")}
      }

      # list of all item combinations
      if (is.null(combos)){
        c <- utils::combn(length(dset), j, simplify = FALSE)
        current_models <- NULL
      } else{
        c <- combos
      }

      combos_process <- length(c)
      if (silent==FALSE){cat("initial combos: ", combos_process, "\n", sep="")}
      current_combos <- c


      for (l in seq_len(length(tests))){
        tests_count <- c((match(j, scale_length)-1)*length(
          tests)+l, # current_number
          length(
            scale_length)*length(tests)) # sum of test processes
        tictoc::tic.clearlog()
        tictoc::tic()
        splitcr <- NULL
        if (tests[l]=="test_mloef"){splitcr <- splitcr_mloef}
        if (tests[l]=="test_LR"){splitcr <- splitcr_LR}
        if (tests[l]=="test_waldtest"){splitcr <- splitcr_wald}
        #if (tests[l]=="test_Qtest"){splitcr <- splitcr_Q}
        current_return <- parallized_tests(dset=dset,
                                           combos=current_combos,
                                           models=current_models,
                                           p.par=current_p.par,
                                           modelType=modelType,
                                           testfunction=tests[l],
                                           itemfit_param=itemfit_param,
                                           splitcr=splitcr,
                                           icat_wald = icat_wald,
                                           na.rm=na.rm,
                                           alpha=alpha,
                                           bonf=bonf,
                                           DIFvars=DIFvars,
                                           gap_prop=gap_prop,
                                           extremes=extremes,
                                           max_contrast=max_contrast,
                                           PSI=PSI,
                                           ignoreCores=ignoreCores,
                                           estimation_param=estimation_param,
                                           tests_count)
        if (length(current_return)>0 & !is.character(current_return)){
          if (tests[l] %in% c("all_rawscores", "test_pca", "rel_coefs")){
            current_combos <- current_return
            current_models <-  current_models[which(current_combos %in%
                                                      current_return)]
          } else{
            current_models <- unlist(
              lapply(seq_len(length(current_return)),
                     function(x) current_return[[x]][2]), recursive=FALSE)
            current_combos <- unlist(lapply(seq_len(
              length(current_return)),
              function(x) current_return[[x]][1]), recursive=FALSE)
            current_p.par <- unlist(lapply(seq_len(
              length(current_return)),
              function(x) current_return[[x]][3]), recursive=FALSE)
          }
          combos_process <- c(combos_process, length(current_return))
        } else{
          combos_process <- c(combos_process, 0)
          current_combos <- NULL
        }
        tictoc::toc(log = TRUE, quiet = TRUE)
        log.lst <- tictoc::tic.log(format = FALSE)
        tim <- round(unlist(lapply(log.lst, function(x) x$toc - x$tic)),2)
        timings <- rbind(timings, c(j, tests[l], tim))
        if (silent==FALSE){
          cat("Item combinations that passed ", tests[l], ": ",
              length(current_combos), "\n", sep="")
          cat("--- Runtime: ", tim, " seconds", "\n", sep="")
        }

      }

      # add remaining item combinations and models to the list

      tictoc::tic.clear()
      tictoc::tic.clearlog()
      tictoc::tic(l)

      if (silent==FALSE){cat("Fit: ", length(current_combos), "\n", sep="")}
      if (length(current_combos)>0){
        passed_combos <- append(passed_combos, current_combos)
        passed_models <- append(passed_models, current_models)
        passed_p.par <- append(passed_p.par, current_p.par)

      }
      newrow <- c(j, combos_process)
      process <- rbind(process, newrow)
    }

    if (length(process)>0){colnames(process) <- c(
      "Scale-Length", "Combinations", tests)}

    colnames(timings) <- c("Scale length", "Test", "Runtime")
    final_list <- methods::new("passed_exRa", process=process,
                               passed_combos=passed_combos,
                               passed_models=passed_models,
                               passed_p.par=passed_p.par,
                               "IC"=data.frame(),
                               "data"=dset, "timings"=timings)
    if (length(passed_combos)>0 & length(passed_models)>0 & ICs==TRUE){
      final_list <- add_ICs(final_list, ignoreCores=ignoreCores)
    }

    tictoc::toc(log = TRUE, quiet = TRUE)
    log.lst <- tictoc::tic.log(format = FALSE)
    tim <- round(unlist(lapply(log.lst, function(x) x$toc - x$tic)),2)
    timings <- rbind(timings, c(0, "constructing passed_exRa objekt", tim))
    final_list@timings <- timings

    return(final_list)
  }


}

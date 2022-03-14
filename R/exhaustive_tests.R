exhaustive_tests <- function(dset, modelType="RM", combos=NULL, scale_length=3:length(dset), na.rm=T,
                             tests=c("all_rawscores", "test_mloef", "test_itemfit"), itemfit_param=NULL,
                             splitcr_mloef=NULL, splitcr_LR=NULL, splitcr_wald=NULL, alpha=0.1, bonf=F,
                             DIFvars=NULL, gap_prop=0, extremes=T, ignoreCores=0, ...){
  #' (main function) Runs exhaustive tests
  #' @param dset a data.frame containing the data
  #' @param modelType a character value defining the rasch model to fit. Possible values: RM, PCM, RSM
  #' @param combos a list of item combinations to be tested, e.g. from \link{apply_combo_rules} or from $passed_combos of the value of a previous call to this function. If NULL, all possible combinations of the items (columns) in dset will be tested
  #' @param scale_length a numeric vector defining the length of the item combinations to test
  #' @param na.rm a boolean value. If TRUE, in the respective pattern all cases with any NA are removed (na.omit). If FALSE, only cases with full NA responses are removed. NOTE: \link{test_mloef} currently does not allow for missing values (because erm::MLoef doesn't). If  \link{test_mloef} is under the tests to perform, na.rm will automatically be set TRUE for ALL tests.
  #' @param tests a vector of characters defining the tests to perform. Possible values: all_rawscores, test_itemfit, test_LR, test_mloef, test_pca, test_waldtest, threshold_order, test_DIFtree. Tests will be performed in the given order.
  #' @param itemfit_param a list from \link{itemfit_control} with options for \link{test_itemfit}
  #' @param splitcr_LR as defined by eRm::LRtest. Split criterion for subject raw score splitting. "all.r" corresponds to a full raw score split, "median" uses the median as split criterion, "mean" performs a mean split. Optionally splitcr can also be a vector which assigns each person to a certain subgroup (e.g., following an external criterion). This vector can be numeric, character or a factor.
  #' @param splitcr_mloef as defined by eRm::MLoef: Split criterion to define the item groups. "median" and "mean" split items in two groups based on their items' raw scores. splitcr can also be a vector of length k (where k denotes the number of items) that takes two or more distinct values to define groups used for the Martin-Löf Test.
  #' @param splitcr_wald as defined by eRm::Waldtest: Split criterion for subject raw score splitting. median uses the median as split criterion, mean performs a mean-split. Optionally splitcr can also be a dichotomous vector which assigns each person to a certain subgroup (e.g., following an external criterion). This vector can be numeric, character or a factor.
  #' @param alpha a numeric value for the alpha level. Will be ignored for \link{test_itemfit} if use.pval in \link{itemfit_control} is FALSE
  #' @param bonf a boolean value wheter to use a Bonferroni correction. Will be ignored if use.pval is FALSE
  #' @param DIFvars a data.frame containing the variables and their data to use for differential item functioning analysis with \link{test_DIFtree}
  #' @param ignoreCores a numeric value for the number of cpu cores to hold out in parallelizing the test run.
  #' @param ... options for \link{itemfit_control} can be passed directly to this function.
  #' @return a list containing 4 lists: the process log, the item combinations that passed the test circuit, the corresponding RM/PCM/RSM models and their information criteria (AIC, BIC, cAIC)
  #' @export
  #' @examples
  #' data(ADL)
  #' passed <- exhaustive_tests(ADL, modelType = "RM", scale_length = c(4:5))
  #'
  #' data(ADL)
  #' passed <- exhaustive_tests(ADL[c(1,4,6,7,10,14,15)], modelType = "RM", scale_length = c(4:6),
  #'      tests=c("test_mloef", "test_itemfit", "test_LR"))

  if (na.rm==F & "test_mloef" %in% tests){
    na.rm <- T
    print("test_mloef is part of the test. This test does currently not allow for missing values, so na.rm was set TRUE for all tests.")
  }

  # Schleife ueber Kombinationen mit Laenge j
  passed_models <- list()
  passed_combos <- list()
  process <- data.frame()

  # pass optional arguments for itemfir to itemfit_control()
  extraArgs <- list(...)
  if (length(extraArgs)) {
    allowed_args <- names(formals(itemfit_control))
    indx <- match(names(extraArgs), allowed_args, nomatch = 0L)
    if (any(indx == 0L))
      stop(gettextf("Argument %s not matched",
                    names(extraArgs)[indx == 0L]),
           domain = NA)
  }
  itemfit_param <- itemfit_control(...)
  if (!missing(itemfit_param)){itemfit_param[names(itemfit_param)] <- itemfit_param}

  if (!is.null(combos)){scale_length <-1:1}

  for (j in scale_length){
    information_criteria <- list()
    print(paste("Scale-Length", j))
    # Liste alle Itemkombinationen
    if (is.null(combos)){c <- utils::combn(length(dset), j, simplify = FALSE)} else{c <- combos}
    combos_process <- length(c)
    print(paste("initial combos:", combos_process))
    current_combos <- c
    current_models <- NULL

    for (l in 1:length(tests)){
      splitcr <- NULL
      if (tests[l]=="test_mloef"){splitcr <- splitcr_mloef}
      if (tests[l]=="test_LR"){splitcr <- splitcr_LR}
      if (tests[l]=="test_waldtest"){splitcr <- splitcr_wald}
      current_return <- parallized_tests(dset=dset, combos=current_combos, models=current_models, modelType=modelType,
                                         testfunction=tests[l], itemfit_param=itemfit_param, splitcr=splitcr,
                                         na.rm=na.rm, alpha=alpha, bonf=bonf, DIFvars=DIFvars, gap_prop=gap_prop,
                                         extremes=extremes, ignoreCores=ignoreCores)
      if (length(current_return)>0 & !is.character(current_return)){
        if (tests[l] %in% c("all_rawscores", "test_pca")){
          current_combos <- current_return
        } else{
          current_models <- unlist(lapply(1: length(current_return),
                                          function(x) current_return[[x]][2]), recursive=F)
          current_combos <- unlist(lapply(1: length(current_return),
                                          function(x) current_return[[x]][1]), recursive=F)
        }
        combos_process <- c(combos_process, length(current_return))
      } else{
        combos_process <- c(combos_process, 0)
        current_combos <- NULL
      }
    }

    # ?brig gebliebene Kombinationen und Modelle den Listen hinzuf?gen
    print(paste("Fit:", length(current_combos)))
    if (length(current_combos)>0){
      passed_combos <- append(passed_combos, current_combos)
      passed_models <- append(passed_models, current_models)
    }
    newrow <- c(j, combos_process)
    process <- rbind(process, newrow)
  }
  if (length(process)>0){colnames(process) <- c("Scale-Length", "Combinations", tests)}
  if (length(passed_combos)>0){
    information_criteria <- lapply(1:length(passed_models),
                                   function(x) eRm::IC(eRm::person.parameter(passed_models[[x]]))$ICtable[3,3:5])
  }
  return(list("process"=process, "passed_combos"=passed_combos, "passed_models"=passed_models,
              "IC"=as.data.frame(do.call(rbind, information_criteria))))
}


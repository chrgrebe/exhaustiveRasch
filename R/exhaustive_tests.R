exhaustive_tests <- function(dset, modelType="RM", patterns=NULL, scale_length=4:length(patterns),
                             tests=c("all_rawscores", "test_mloef", "test_itemfit"),
                             lower=0.5, upper=1.5, p.val=F){
  # Schleife ?ber Kombinationen mit L?nge j
  passed_models <- list()
  passed_patterns <- list()
  process <- data.frame()

  if (!is.null(patterns)){scale_length <-1:1}

  for (j in scale_length){
    information_criteria <- list()
    print(paste("Scale-Length", j))
    # Liste alle Itemkombinationen
    if (is.null(patterns)){c <- combn(length(dset), j, simplify = FALSE)} else{c <- patterns}
    patterns_process <- length(c)
    print(paste("initial Patterns:", patterns_process))
    current_patterns <- c
    current_models <- list()

    for (l in 1:length(tests)){
      current_return <- parallized_tests(dset=dset, combos=current_patterns,
                                         testfunction=tests[l], upper=upper,
                                         lower=lower, p.val=p.val)
      if (length(current_return)>0){
        if (tests[l] %in% c("all_rawscores", "test_pca")){
          current_patterns <- current_return
        } else{
          current_models <- unlist(lapply(1: length(current_return),
                                          function(x) current_return[[x]][2]), recursive=F)
          current_patterns <- unlist(lapply(1: length(current_return),
                                            function(x) current_return[[x]][1]), recursive=F)
        }
        patterns_process <- c(patterns_process, length(current_return))
      } else{
        patterns_process <- c(patterns_process, 0)
        current_patterns <- NULL
      }
    }

    # ?brig gebliebene Kombinationen und Modelle den Listen hinzuf?gen
    print(paste("Fit:", length(current_patterns)))
    if (is.character(current_models)==F){passed_models <- append(passed_models, current_models)}
    if (is.character(current_patterns)==F){passed_patterns <- append(passed_patterns, current_patterns)}
    newrow <- c(j, patterns_process)
    process <- rbind(process, newrow)
  }
  if (length(process)>0){colnames(process) <- c("Scale-Length", "Combinations", tests)}
  if (length(passed_models)>0){
    information_criteria <- lapply(1:length(passed_models),
                                   function(x) IC(person.parameter(passed_models[[x]]))$ICtable[3,3:5])
  }
  return(list("process"=process, "passed_patterns"=passed_patterns, "passed_models"=passed_models,
              "IC"=as.data.frame(do.call(rbind, information_criteria))))
}

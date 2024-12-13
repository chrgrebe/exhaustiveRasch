apply_combo_rules <- function(full,
                              combo_length=4:length(full),
                              forced_items=NULL,
                              rules=NULL, ignoreCores=1){

  #' selects item combinations based on defined rules
  #' @param full a numeric vector containing the the source for the
  #'  combinations, typically the indices of the items in the referring dataset
  #' @param combo_length a numeric vector with the allowed lengths of the
  #'  selected combinations (scale lengths)
  #' @param forced_items a numeric vector of items that are forced to occur
  #'  in every selected combination
  #' @param ignoreCores number of cpu cores (threads) to be ignored during
  #' parallized processing.
  #' @param rules a list defining rules for combination selection
  #' @return a list of numeric vectors containing the selected item combinations
  #'  that match the defined rules of forced_items and/or rules.
  #' @export
  #' @examples
  #' data(ADL)
  #' forced <- c(1)
  #' rules_object <- list() # rules-Object
  #' rules_object[[1]] <- list("min", 1, 8:9)
  #' rules_object[[2]] <- list("min", 1, 14:15)
  #' rules_object[[3]] <- list("max", 2, 2:6)
  #' rules_object[[4]] <- list("forbidden", c(8,9))
  #' final_combos <- apply_combo_rules(combo_length = 5:7,
  #'                                   full=1:length(ADL),
  #'                                    forced_items = forced,
  #'                                    rules= rules_object)

  if (isTRUE(as.logical(Sys.getenv("_R_CHECK_LIMIT_CORES_")))) {
    # on cran
    cores=2L
    # use 2 cores in CRAN/Travis/AppVeyor
  } else {
    cores <- parallel::detectCores()- ignoreCores
    # use all cores in devtools::test()
  }
  on.exit(parallel::stopCluster(cl))
  combo_list <- unlist(lapply(combo_length,
                              function(x) as.list(
                                data.frame(t(arrangements::combinations(
                                  length(full), x))))),
                       recursive=FALSE)
  # forced items
  if (!is.null(forced_items)){
    forced_list <- combo_list[lapply(1:length(combo_list),
                                     function(x) all(
                                       forced_items %in% combo_list[[x]]))==T]
  } else{
    forced_list <- combo_list
  }
  cl <- parallel::makePSOCKcluster(cores)
  parallel::setDefaultCluster(cl)
  parallel::clusterExport(cl, c("check_combo_rules"))
  param1 <- list(cl=cl, X=forced_list, rules=rules,
                 fun= check_combo_rules)
  final_list <- do.call(parallel::parLapply, param1)
  final_list[sapply(final_list, is.null)] <- NULL
  #final_list <- final_list[which(!sapply(final_list, is.null))]

  return(final_list)
}

apply_combo_rules <- function(full,
                              combo_length=4:length(full),
                              forced_items=NULL,
                              rules=NULL){

  #' selects item combinations based on defined rules
  #' @param full a numeric vector containing the the source for the
  #'  combinations, typically the indices of the items in the referring dataset
  #' @param combo_length a numeric vector with the allowed lengths of the
  #'  selected combinations (scale lengths)
  #' @param forced_items a numeric vector of items that are forced to occur
  #'  in every selected combination
  #' @param rules a list defining rules for combination selection
  #' @return a list of numeric vectors containing the selected item combinations
  #'  that match the defined rules of forced_items and/or rules.
  #' @export
  #' @examples
  #' data(ADL)
  #' forced <- c(6,22)
  #' rules_object <- list() # rules-Object
  #' rules_object[[1]] <- list("min", 1, 8:9)
  #' rules_object[[2]] <- list("min", 1, 14:15)
  #' rules_object[[3]] <- list("max", 3, 1:6)
  #' rules_object[[4]] <- list("forbidden", c(8,9))
  #' final_combos <- apply_combo_rules(combo_length = 5:7,
  #' full=1:length(ADL), forced_items = forced, rules= rules_object)

  check_combo_rules <- function(full,
                                forced_items=NULL,
                                rules=NULL){

    passed_forced <- FALSE; passed_max_rules <- FALSE; passed_min_rules <- FALSE
    passed_forbidden_rules <- FALSE
    max_rules <- rules[which((sapply(rules,'[[',1)=="max"))]
    min_rules <- rules[which((sapply(rules,'[[',1)=="min"))]
    forbidden_rules <- rules[which((sapply(rules,'[[',1)=="forbidden"))]
    if (length(forced_items)>0){
      if (length(intersect(forced_items,full))==length(forced_items)
      ){passed_forced <- TRUE}
    } else{passed_forced <-TRUE} # if no rule defined: passed=TRUE
    if (length(max_rules)>0){
      if (sum(sapply(seq_len(length(max_rules)),
                     function(x) if (length(intersect(max_rules[[x]][[3]],full)
                     )<max_rules[[x]][[2]]+1){1
                     }else{0}))==length(max_rules)){passed_max_rules <- TRUE}
    } else{passed_max_rules <- TRUE}
    if (length(min_rules)>0){ # if no rule defined: passed=TRUE
      if (sum(sapply(seq_len(length(min_rules)),
                     function(x) if (length(intersect(min_rules[[x]][[3]],full)
                     )>min_rules[[x]][[2]]-1){1
                     }else{0}))==length(min_rules)){passed_min_rules <- TRUE}
    } else{passed_min_rules <- TRUE} # if no rule defined: passed=TRUE
    if (length(forbidden_rules)>0){
      if (sum(sapply(seq_len(length(forbidden_rules)),
                     function(x) if (length(intersect(
                       forbidden_rules[[x]][[2]],full))==length(
                         forbidden_rules[[x]][[2]]))
                     {1}else{0}))==0){passed_forbidden_rules <- TRUE}
    } else{passed_forbidden_rules <- TRUE} # if no rule defined: passed=TRUE

    if (passed_forced==TRUE & passed_max_rules==TRUE & passed_min_rules==
        TRUE & passed_forbidden_rules==TRUE){
      return(full)
    }
  }

  combo_list <- unlist(lapply(combo_length,
                              function(x) utils::combn(length(full), x,
                                                       simplify = FALSE)),
                       recursive=FALSE)
  final_list <- lapply(seq_len(length(combo_list)),
                       function(x) check_combo_rules(
                         full=combo_list[[x]], rules=rules,
                         forced_items = forced_items))
  final_list <- final_list[which(!sapply(final_list, is.null))]
  return(final_list)
}

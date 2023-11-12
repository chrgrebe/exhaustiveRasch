check_combo_rules <- function(full,
                              rules=NULL){
  #' selects item combinations based on defined rules
  #' @param full a numeric vector containing the the source for the
  #'  combinations, typically the indices of the items in the referring dataset
  #' @param rules a list defining rules for combination selection
  #' @return a list of numeric vectors containing the selected item combinations
  #'  that match the defined rules of forced_items and/or rules.
  #' @export
  #' @keywords internal

  # This function implements the checks for the min-, max- and forbidden rules
  # and is calles from apply_combo_rules. It is an internal function, a
  # call by the user is not indicated. It is nevertheless exported in order to
  # work in parallelization. However, it is not included in the package
  # documentation (roxygen2 keyword 'internal').

  passed_max_rules <- FALSE; passed_min_rules <- FALSE
  passed_forbidden_rules <- FALSE
  max_rules <- rules[which((sapply(rules,'[[',1)=="max"))]
  min_rules <- rules[which((sapply(rules,'[[',1)=="min"))]
  forbidden_rules <- rules[which((sapply(rules,'[[',1)=="forbidden"))]
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

  if (passed_max_rules==TRUE & passed_min_rules==
      TRUE & passed_forbidden_rules==TRUE){
    return(full)
  }
}

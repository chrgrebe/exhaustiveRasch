apply_combo_rules <- function(full, combo_length=3:length(full), forced_items=NULL, rules=NULL){

  #' selects item combinations based on defined rules
  #' @param full a numeric vector containing the the source for the combinations, typically the indices of the items in the referring dataset
  #' @param combo_length a numeric vector with the allowed lengths of the selected combinations (scale lengths)
  #' @param forced_items a numeric vector of items that are forced to occur in every selected combination
  #' @param rules a list defining rules for combination selection
  #' @return a list of numeric vectors containing the selected item combinations that match the defined rules of forced_items and/or rules.
  #' @export
  #' @examples
  #' data(ADL)
  #' forced <- c(6,22)
  #' rules_object <- list() # rules-Object
  #' rules_object[[1]] <- list("min", 1, 8:9)
  #' rules_object[[1]] <- list("min", 1, 14:15)
  #' rules_object[[1]] <- list("max", 3, 1:6)
  #' rules_object[[7]] <- list("forbidden", c(8,9))
  #' final_combos <- apply_combo_rules(combo_length = 5:7,
  #'   full=1:length(ADL), forced_items = forced, rules= rules_object)


  check_combo_rules <- function(full, forced_items=NULL, rules=NULL){

    passed_forced=F; passed_max_rules=F; passed_min_rules=F; passed_forbidden_rules=F
    max_rules <- rules[which((sapply(rules,'[[',1)=="max"))]
    min_rules <- rules[which((sapply(rules,'[[',1)=="min"))]
    forbidden_rules <- rules[which((sapply(rules,'[[',1)=="forbidden"))]
    if (length(forced_items)>0){
      if (length(intersect(forced_items,full))==length(forced_items)){passed_forced=T}
    } else{passed_forced <-T} # falls keine Regel: passed=T
    if (length(max_rules)>0){
      if (sum(sapply(1: length(max_rules), function(x) if (length(intersect(max_rules[[x]][[3]],full))<max_rules[[x]][[2]]+1){
        1}else{0}))==length(max_rules)){passed_max_rules <- T}
    } else{passed_max_rules <- T}
    if (length(min_rules)>0){ # falls keine Regel: passed=T
      if (sum(sapply(1: length(min_rules), function(x) if (length(intersect(min_rules[[x]][[3]],full))>min_rules[[x]][[2]]-1){
        1}else{0}))==length(min_rules)){passed_min_rules <- T}
    } else{passed_min_rules <- T} # falls keine Regel: passed=T
    if (length(forbidden_rules)>0){
      if (sum(sapply(1: length(forbidden_rules), function(x)  if (length(intersect(forbidden_rules[[x]][[2]],full))==length(forbidden_rules[[x]][[2]]))
      {1}else{0}))==0){passed_forbidden_rules <- T}
    } else{passed_forbidden_rules <- T} # falls keine Regel: passed=T

    if (passed_forced==T & passed_max_rules==T & passed_min_rules==T & passed_forbidden_rules==T){
      return(full)
    }
  }

  combo_list <- unlist(lapply(combo_length, function(x) utils::combn(length(full), x, simplify = F)), recursive=F)
  #unlist(combo_list, recursive = F)
  final_list <- lapply(1:length(combo_list), function(x) check_combo_rules(full=combo_list[[x]], rules=rules, forced_items = forced_items))
  final_list <- final_list[which(!sapply(final_list, is.null))]
  return(final_list)
}

check_combo_rules <- function(full, forced_items=NULL, rules=NULL){

  passed_forced=F; passed_max_rules=F; passed_min_rules=F; passed_forbidden_rules=F
  max_rules <- rules[which((sapply(rules_object,'[[',1)=="max"))]
  min_rules <- rules[which((sapply(rules,'[[',1)=="min"))]
  forbidden_rules <- rules[which((sapply(rules,'[[',1)=="forbidden"))]
  if (length(forced_items)>0){
    if (length(intersect(forced_items,full))==length(forced_items)){passed_forced=T}
  } else{passed_forced <-T} # falls keine Regel: passed=T
  if (length(max_rules)>0){
    if (sum(sapply(1: length(max_rules), function(x) if (length(intersect(max_rules[[x]][[3]],full))<rules[[x]][[2]]+1){
      1}else{0}))==length(max_rules)){passed_max_rules <- T}
  } else{passed_max_rules <- T}
  if (length(min_rules)>0){ # falls keine Regel: passed=T
    if (sum(sapply(1: length(min_rules), function(x) if (length(intersect(min_rules[[x]][[3]],full))>rules[[x]][[2]]-1){
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

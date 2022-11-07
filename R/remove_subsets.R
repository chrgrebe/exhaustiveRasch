remove_subsets <- function(obj, keep_longest=T){
  #' removes subsets or supersets of item combinations
  #' @param obj either a list of vectors of item combinations (typically the passed_combos slot of an object of class passed_exRa or an object of class passed_exRa. If combos is an object of class passed_exRa, all process data (the process slot) and for any removed item combination also the fit models in the passed_models slot will be removed.
  #' @param keep_longest boolean; if TRUE, the respective superset is kept (and all subsets of these supersets are removed), if FALSE all subsets is kept (and supersets of these subsets are removed).
  #' @return depending of the class of obj, either a list of numeric vectors containing the remaining item combinations or an object of class passed_exRa with the remaining item combinations, fit models and information criteria, but without the process information.
  #' @export

  if (inherits(obj,"passed_exRa")){
    combos <- obj@passed_combos
  } else{
    combos <- obj
  }
  scale_lengths <- unique(lapply(1:length(combos), function(x) length(combos[[x]])))
  if (keep_longest==F){
    scale_lengths <- rev(scale_lengths)
  }
  i <- length(scale_lengths)
  if (keep_longest==T){
    comp_combs <- combos[lengths(combos)<scale_lengths[i]]
  }else{
    comp_combs <- combos[lengths(combos)>scale_lengths[i]]
  }
  while(length(comp_combs)>0 & i>1){
    ref_combs <- combos[lengths(combos)==scale_lengths[i]]
    if (keep_longest==T){
      comp_combs <- combos[lengths(combos)<scale_lengths[i]]
    }else{
      comp_combs <- combos[lengths(combos)>scale_lengths[i]]
    }
    j <- 1
    while(j< length(ref_combs)+1 & length(comp_combs)>0){
      if (keep_longest==T){
        subs <- comp_combs[unlist(lapply(1:length(comp_combs), function(x) sum(comp_combs[[x]] %in% ref_combs[[j]])==length(comp_combs[[x]])))==T]
      }else{
        subs <- comp_combs[unlist(lapply(1:length(comp_combs), function(x) sum(ref_combs[[j]] %in% comp_combs[[x]])==length(ref_combs[[j]])))==T]
      }

      if (length(subs)>0){
        comp_combs <- comp_combs[-which(comp_combs %in% subs)]
        combos <- combos[-which(combos %in% subs)]
      }
      j <- j+1
    }
    i <- i-1
  }
  if (inherits(obj,"passed_exRa")){
    obj@passed_models <- obj@passed_models[which(obj@passed_combos %in% combos)]
    obj@process <- data.frame()
    obj@IC <- obj@IC[which(obj@passed_combos %in% combos),]
    obj@passed_combos <- combos
    return(obj)
  } else{
    return(combos)
  }

}

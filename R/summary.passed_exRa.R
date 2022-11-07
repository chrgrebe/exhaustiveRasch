#' summary function for class passed_exRa
#' @param object an object of class passed_exRa
#' @export
methods::setMethod("summary", "passed_exRa", function(object){

  cat("-----------------------", "\n")
  if (length(object@process)>0){
    if (object@process[[1]][[1]]==1){
      scale_lengths <- paste(unique(lapply(1:length(object@passed_combos), function(x) length(object@passed_combos[[x]]))), "; pre-defined set of item combinations ('combos' parameter was used)")
    } else{
      scale_lengths <- object@process[[1]]
    }
    cat("Tested Scale lengths:", scale_lengths, sep=" ")
    cat("\n")
    cat("Total number of tested item combinations:", sum(object@process[[2]]), sep=" ")
    cat("\n")
    cat("Applied tests (in order of usage):", colnames(object@process)[3:length(colnames(object@process))], sep=" ")
    cat("\n")
    cat("-----------------------", "\n")
    cat("Number of passed item combinations by test (total):", "\n")
    for (i in 3:length(object@process)){
      cat(colnames(object@process[i]), ":", sum(object@process[[i]]), sep=" ")
      cat("\n")
    }
    cat("-----------------------", "\n")
  } else{
    cat("passed_exRa object does not contain process information. This typically happens, if remove_subsets was used with on the passed_exRa object.", "\n")
    cat("-----------------------", "\n")
  }

  cat("Item importance:", "\n")
  imp <- (sort(table(unlist(object@passed_combos)), decreasing=T))
  for (i in 1:length(imp)){
    cat("Item #", names(imp[i]), " (", colnames(object@data)[as.numeric(names(imp)[i])], "): ", imp[i],"x", " (", round(imp[i]/length(object@passed_combos)*100),"%)", "\n", sep="")
  }
  cat("-----------------------", "\n")
  cat("Total runtime (tests): ", sum(as.numeric(object@timings$Runtime[1:length(object@timings$Runtime)-1])), " seconds", "\n", sep="")
  cat("Total runtime (constructing passed_exRa object): ", as.numeric(object@timings$Runtime[length(object@timings$Runtime)]), " seconds", "\n", sep="")
  cat("-----------------------", "\n")
}
)

save_passed_xlsx <- function(resultData, filename, itemNames=NULL){
  #' saves an Excel file with the results of exhaustive_tests()
  #' @param resultData the list object with the result of \link{exhaustive_tests}
  #' @param filename path and filename of the Excel file to be saved
  #' @param itemNames the item labels to be used in the Excel sheet, e.g. the columnNames of the data.set that was tested with exhaustive_tests()
  #' @return no value is returned. This function saves an Excel file containing 3 sheets: 1) The process information of exhaustive_tests(); 2) a tabulated sheet with all patterns that passed the test circuit of exhaustive_tests(); 3) the information criteria (AIC, BIC, cAIC) of the models that were fit using the respective item patterns.
  #' @export

  passed_NA_filled <- lapply(1:length(resultData@passed_patterns),
                             function(x) fill_NAs(resultData@passed_patterns[[x]],
                                                  length(itemNames)))
  passed_NA_filled <- as.data.frame(do.call(rbind, passed_NA_filled))
  if (!is.null(itemNames)){colnames(passed_NA_filled) <- itemNames}

  excel <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(excel, "Process")
  openxlsx::addWorksheet(excel,"Models")
  openxlsx::addWorksheet(excel,"Information Criteria")
  openxlsx::writeData(excel, sheet="Process", resultData@process)
  openxlsx::writeData(excel, sheet="Models", passed_NA_filled)
  openxlsx::writeData(excel, sheet="Information Criteria", resultData@IC)
  openxlsx::saveWorkbook(excel, paste(filename), overwrite=TRUE)
}

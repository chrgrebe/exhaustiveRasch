save_passed_xlsx <- function(resultData, filename, itemNames=NULL){
  passed_NA_filled <- lapply(1:length(resultData$passed_patterns),
                             function(x) fill_NAs(resultData$passed_patterns[[x]],
                                                  length(ds_full)))
  passed_NA_filled <- as.data.frame(do.call(rbind, passed_NA_filled))
  if (!is.null(itemNames)){colnames(passed_NA_filled) <- itemNames}

  excel <- createWorkbook()
  addWorksheet(excel, "Process")
  addWorksheet(excel,"Models")
  addWorksheet(excel,"Information Criteria")
  writeData(excel, sheet="Process", resultData$process)
  writeData(excel, sheet="Models", passed_NA_filled)
  writeData(excel, sheet="Information Criteria", resultData$IC)
  saveWorkbook(excel, paste(filename), overwrite=TRUE)
}

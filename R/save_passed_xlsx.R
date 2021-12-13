save_passed_xlsx <- function(dset, resultData, filename, itemNames=NULL){
  passed_NA_filled <- lapply(1:length(resultData$passed_patterns),
                             function(x) fill_NAs(resultData$passed_patterns[[x]],
                                                  length(dset)))
  passed_NA_filled <- as.data.frame(do.call(rbind, passed_NA_filled))
  if (!is.null(itemNames)){colnames(passed_NA_filled) <- itemNames}

  excel <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(excel, "Process")
  openxlsx::addWorksheet(excel,"Models")
  openxlsx::addWorksheet(excel,"Information Criteria")
  openxlsx::writeData(excel, sheet="Process", resultData$process)
  openxlsx::writeData(excel, sheet="Models", passed_NA_filled)
  openxlsx::writeData(excel, sheet="Information Criteria", resultData$IC)
  openxlsx::saveWorkbook(excel, paste(filename), overwrite=TRUE)
}

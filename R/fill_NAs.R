fill_NAs <- function(vec, final_length){
  tmp_vec <- c(1:final_length)
  NAs_bool <- tmp_vec %in% vec
  NAs_pos <- which(NAs_bool==F)
  tmp_vec[which(tmp_vec %in% vec==F)] <- NA
  return(tmp_vec)
}

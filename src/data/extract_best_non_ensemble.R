extract_best_non_ensemble <- function(aml) {
  aml_model_ids <- as.data.frame(aml@leaderboard$model_id)[,1];
  index_bne <- min(which(!str_detect(aml_model_ids, "Stacked") == TRUE))
  aml_bne <- h2o.getModel(grep(aml_model_ids[index_bne], aml_model_ids, value = TRUE)[1]);
  return(aml_bne)
}
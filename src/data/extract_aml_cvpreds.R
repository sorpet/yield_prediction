extract_aml_cvpreds <- function(aml) {
  aml_model_ids <- as.data.frame(aml@leaderboard$model_id)[,1]; 
  if (!str_detect(aml_model_ids[1], "Stacked")) {
    aml_cvpreds_id <- aml@leader@model$cross_validation_holdout_predictions_frame_id$name
    aml_cvpreds <- h2o.getFrame(aml_cvpreds_id)
  }
  else {
    aml_leader <- h2o.getModel(grep(aml_model_ids[1], aml_model_ids, value = TRUE)[1]); 
    aml_metalearner <- h2o.getModel(aml_leader@model$metalearner$name)
    aml_cvpreds_id <- aml_metalearner@model$cross_validation_holdout_predictions_frame_id$name
    aml_cvpreds <- h2o.getFrame(aml_cvpreds_id)
  }
  return(as.vector(aml_cvpreds))
}
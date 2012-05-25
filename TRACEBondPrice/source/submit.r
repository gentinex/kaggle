source('random_forest_benchmark.r')
source('simple_regression.r')
source('constant.r')
source('kalman_filter.r')

Submit <- function(submit.type){
  submission.file.name <- paste('../output/', submit.type, '.csv', sep='')
  if(submit.type=='RandomForestBenchmark')
    predictions_df <- RandomForestBenchmark()
  else if(submit.type=='SimpleRegression')
    predictions_df <- SimpleRegression()
  else if(submit.type=='Constant')
    predictions_df <- Constant()
  else if(submit.type=='SimpleKalmanFilter')
    predictions_df <- KalmanFilter(simple=TRUE)
  else stop('invalid submission type')
  write.csv(predictions_df, file = paste('../output/', submission.file.name, sep=''), row.names = FALSE)
}

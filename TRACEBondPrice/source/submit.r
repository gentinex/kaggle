source('random_forest_benchmark.r')
source('simple_regression.r')

Submit <- function(submit.type){
  submission.file.name <- paste('../output/', submit.type, '.csv', sep='')
  if(submit.type=='RandomForestBenchmark')
    predictions_df <- RandomForestBenchmark()
  else if(submit.type=='SimpleRegression')
    predictions_df <- SimpleRegression()
  else stop('invalid submission type')
  write.csv(predictions_df, file = paste('../output/', submission.file.name, sep=''), row.names = FALSE)

}

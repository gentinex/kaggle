##### Constant
# just set the price equal to the last traded price

Constant <- function(){
  test <-read.csv( "../data/test.csv",  header = TRUE, na.strings = "NA")
  predictions_df <- data.frame(id=test$id, trade_price=test$trade_price_last1)
  return(predictions_df)
}

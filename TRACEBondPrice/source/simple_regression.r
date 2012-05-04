##### Simple Regression
# split the data into ten subsets, depending on
# whether it has 1 to 10 past trade data points
# run regression separately on each subset,
# then use the results of this to predict

# room for improvement:
# could include curve_based_price history
# how to make the code more elegant?

# obviously has issues with the fact that a trade with
# two past data points means that the previous trade
# has one past data point (i.e., there are cross-relationships
# among the disjoint subsets)

SimpleRegression <- function(){
  print("Reading in data")
  train <-read.csv( "../data/train.csv",  header = TRUE, na.strings = "NA")
  test <-read.csv( "../data/test.csv",  header = TRUE, na.strings = "NA")

  print("Subsetting training set")
  train.1 <- train[is.na(train$trade_price_last2),]
  train.2 <- train[!is.na(train$trade_price_last2) & is.na(train$trade_price_last3),]
  train.3 <- train[!is.na(train$trade_price_last3) & is.na(train$trade_price_last4),]
  train.4 <- train[!is.na(train$trade_price_last4) & is.na(train$trade_price_last5),]
  train.5 <- train[!is.na(train$trade_price_last5) & is.na(train$trade_price_last6),]
  train.6 <- train[!is.na(train$trade_price_last6) & is.na(train$trade_price_last7),]
  train.7 <- train[!is.na(train$trade_price_last7) & is.na(train$trade_price_last8),]
  train.8 <- train[!is.na(train$trade_price_last8) & is.na(train$trade_price_last9),]
  train.9 <- train[!is.na(train$trade_price_last9) & is.na(train$trade_price_last10),]
  train.10 <- train[!is.na(train$trade_price_last10),]

  print("Running the ten regressions")
  lm.1 <- lm(train.1$trade_price ~ train.1$trade_price_last1)
  lm.2 <- lm(train.2$trade_price ~ train.2$trade_price_last1 + train.2$trade_price_last2)
  lm.3 <- lm(train.3$trade_price ~ train.3$trade_price_last1 + train.3$trade_price_last2 + train.3$trade_price_last3)
  lm.4 <- lm(train.4$trade_price ~ train.4$trade_price_last1 + train.4$trade_price_last2 + train.4$trade_price_last3 + train.4$trade_price_last4)
  lm.5 <- lm(train.5$trade_price ~ train.5$trade_price_last1 + train.5$trade_price_last2 + train.5$trade_price_last3 + train.5$trade_price_last4 + train.5$trade_price_last5)
  lm.6 <- lm(train.6$trade_price ~ train.6$trade_price_last1 + train.6$trade_price_last2 + train.6$trade_price_last3 + train.6$trade_price_last4 + train.6$trade_price_last5 + train.6$trade_price_last6)
  lm.7 <- lm(train.7$trade_price ~ train.7$trade_price_last1 + train.7$trade_price_last2 + train.7$trade_price_last3 + train.7$trade_price_last4 + train.7$trade_price_last5 + train.7$trade_price_last6 + train.7$trade_price_last7)
  lm.8 <- lm(train.8$trade_price ~ train.8$trade_price_last1 + train.8$trade_price_last2 + train.8$trade_price_last3 + train.8$trade_price_last4 + train.8$trade_price_last5 + train.8$trade_price_last6 + train.8$trade_price_last7 + train.8$trade_price_last8)
  lm.9 <- lm(train.9$trade_price ~ train.9$trade_price_last1 + train.9$trade_price_last2 + train.9$trade_price_last3 + train.9$trade_price_last4 + train.9$trade_price_last5 + train.9$trade_price_last6 + train.9$trade_price_last7 + train.9$trade_price_last8 + train.9$trade_price_last9)
  lm.10 <- lm(train.10$trade_price ~ train.10$trade_price_last1 + train.10$trade_price_last2 + train.10$trade_price_last3 + train.10$trade_price_last4 + train.10$trade_price_last5 + train.10$trade_price_last6 + train.10$trade_price_last7 + train.10$trade_price_last8 + train.10$trade_price_last9 + train.10$trade_price_last10)

  # hack: we would normally want to name the following subsetted files
  # test.1, test.2, etc., but the problem is that we then can't run predict(),
  # which is expecting variable names train.1$x, train.1$y, train.2$x, and so
  # on. so we instead just name the subsets train.1, train.2, and so on.
  print("Subsetting test set")
  train.1 <- test[is.na(test$trade_price_last2),]
  train.2 <- test[!is.na(test$trade_price_last2) & is.na(test$trade_price_last3),]
  train.3 <- test[!is.na(test$trade_price_last3) & is.na(test$trade_price_last4),]
  train.4 <- test[!is.na(test$trade_price_last4) & is.na(test$trade_price_last5),]
  train.5 <- test[!is.na(test$trade_price_last5) & is.na(test$trade_price_last6),]
  train.6 <- test[!is.na(test$trade_price_last6) & is.na(test$trade_price_last7),]
  train.7 <- test[!is.na(test$trade_price_last7) & is.na(test$trade_price_last8),]
  train.8 <- test[!is.na(test$trade_price_last8) & is.na(test$trade_price_last9),]
  train.9 <- test[!is.na(test$trade_price_last9) & is.na(test$trade_price_last10),]
  train.10 <- test[!is.na(test$trade_price_last10),]

  print("Predicting on test set")
  test.1.trade.price <- predict(lm.1, train.1)
  test.2.trade.price <- predict(lm.2, train.2)
  test.3.trade.price <- predict(lm.3, train.3)
  test.4.trade.price <- predict(lm.4, train.4)
  test.5.trade.price <- predict(lm.5, train.5)
  test.6.trade.price <- predict(lm.6, train.6)
  test.7.trade.price <- predict(lm.7, train.7)
  test.8.trade.price <- predict(lm.8, train.8)
  test.9.trade.price <- predict(lm.9, train.9)
  test.10.trade.price <- predict(lm.10, train.10)

  print("Creating the predictions")
  predictions_df = data.frame(id=c(), trade_price=c())
  predictions_df = rbind(predictions_df, data.frame(id=train.1$id, trade_price=test.1.trade.price))
  predictions_df = rbind(predictions_df, data.frame(id=train.2$id, trade_price=test.2.trade.price))
  predictions_df = rbind(predictions_df, data.frame(id=train.3$id, trade_price=test.3.trade.price))
  predictions_df = rbind(predictions_df, data.frame(id=train.4$id, trade_price=test.4.trade.price))
  predictions_df = rbind(predictions_df, data.frame(id=train.5$id, trade_price=test.5.trade.price))
  predictions_df = rbind(predictions_df, data.frame(id=train.6$id, trade_price=test.6.trade.price))
  predictions_df = rbind(predictions_df, data.frame(id=train.7$id, trade_price=test.7.trade.price))
  predictions_df = rbind(predictions_df, data.frame(id=train.8$id, trade_price=test.8.trade.price))
  predictions_df = rbind(predictions_df, data.frame(id=train.9$id, trade_price=test.9.trade.price))
  predictions_df = rbind(predictions_df, data.frame(id=train.10$id, trade_price=test.10.trade.price))
  predictions_df  <- predictions_df[with(predictions_df, order(id)),]
  return(predictions_df)
}

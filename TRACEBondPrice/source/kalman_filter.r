# trying a Kalman filter approach..
# in first attempt, try a filter which only
# uses trade prices to determine state
# in second attempt, try a filter which uses trade prices
# and curve prices to determine state
# for second attempt, use the training data to somehow
# determine how to weight between last trade price and
# curve price (presumably based on 

UpdateTime <- function(kf, state.var){
  kf$prior <- kf$state.weight * kf$state + kf$control.weight * kf$control
  kf$error.var.prior <- kf$state.weight * kf$error.var * kf$state.weight + state.var
  return(kf)
}

UpdateMeasurement <- function(kf, observation, measurement.var){
  gain <- (kf$error.var.prior * kf$xform) / (kf$xform * kf$error.var.prior * kf$xform + measurement.var)
  kf$state <- kf$prior + gain * (observation - kf$xform * kf$prior)
  kf$error.var <- (1 - gain * kf$xform)* kf$error.var.prior
  return(kf)
}

Predict <- function(price.names, curve.names, timediff.names, row){
  prior.prices <- t(row[, price.names])
  prior.curves <- t(row[, curve.names])
  prior.timediffs <- t(row[, timediff.names])
  prior.info <- data.frame(prices=prior.prices, curves=prior.curves, timediffs=prior.timediffs)
  prior.info <- subset(prior.info, !is.na(prices) & !is.na(timediffs))

  # add top row just to start the filter
  top.row <- prior.info[1,]
  top.row$timediffs <- 0
  prior.info <- cbind(top.row, prior.info)

  kf = list(state=prior.info$prices[1], error.var=prior.info$timediffs[1],
    prior=prior.info$prices[1], error.var.prior=prior.info$timediffs[1],
    state.weight=1, control.weight=0, control=0, xform=1)

  for(i in 1:nrow(prior.info)){
    kf = UpdateMeasurement(kf, prior.info$prices[i], prior.info$timediffs[i])
    kf = UpdateTime(kf, prior.info$timediffs[i])
  }

  return(kf$state)
}

KalmanFilter <- function(simple=TRUE){
  print("Reading the data")
  #train <-read.csv( "../data/train.csv",  header = TRUE, na.strings = "NA")
  test <-read.csv( "../data/test.csv",  header = TRUE, na.strings = "NA")

  print("Making predictions")
  price.names <- c('trade_price_last10', 'trade_price_last9',
                   'trade_price_last8', 'trade_price_last7',
                   'trade_price_last6', 'trade_price_last5', 
                   'trade_price_last4', 'trade_price_last3',
                   'trade_price_last2', 'trade_price_last1')
  curve.names <- c('curve_based_price_last10', 'curve_based_price_last9',
                   'curve_based_price_last8', 'curve_based_price_last7',
                   'curve_based_price_last6', 'curve_based_price_last5', 
                   'curve_based_price_last4', 'curve_based_price_last3',
                   'curve_based_price_last2', 'curve_based_price_last1')
  timediff.names <- c('received_time_diff_last10', 'received_time_diff_last9',
                      'received_time_diff_last8', 'received_time_diff_last7',
                      'received_time_diff_last6', 'received_time_diff_last5', 
                      'received_time_diff_last4', 'received_time_diff_last3',
                      'received_time_diff_last2', 'received_time_diff_last1')
  BoundPredict <- function(row) return(Predict(price.names, curve.names, timediff.names, row))
  predictions <- apply(test, 1, BoundPredict)
  
  print("Creating the predictions")
  predictions_df  <- data.frame(test$id, predictions)
  names(predictions_df)  <- c("id", "trade_price")
  return(predictions_df)
}

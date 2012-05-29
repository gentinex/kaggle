# trying a Kalman filter approach..
# in first attempt, try a filter which only
# uses trade prices to determine state
# in second attempt, try a filter which uses trade prices
# and curve prices to determine state
# for second attempt, use the training data to somehow
# determine how to weight between last trade price and
# curve price (presumably based on the time diff)

UpdateTime <- function(state, state.weight, control, control.weight, error.var, state.var){
  prior <- state.weight * state + control.weight * control
  error.var.prior <- state.weight * error.var * state.weight + state.var
  return(c(prior, error.var.prior))
}

UpdateMeasurement <- function(prior, error.var.prior, xform, observation, measurement.var){
  gain <- (error.var.prior * xform) / (xform * error.var.prior * xform + measurement.var)
  state <- prior + gain * (observation - xform * prior)
  error.var <- (1 - gain * xform)* error.var.prior
  return(c(state, error.var))
}

Predict <- function(price.names, curve.names, timediff.names, timemap, row){
  prior.prices <- row[price.names]
  prior.curves <- row[curve.names]
  prior.timediffs <- timemap(row[timediff.names])
  prior.info <- cbind(prior.prices, prior.curves, prior.timediffs)
  prior.info <- subset(prior.info, !is.na(prior.info[, 1]) & !is.na(prior.info[, 3]))

  # add top row just to start the filter at its first known point
  top.row <- prior.info[1, ]
  top.row[3] <- 1e-6
  prior.info <- cbind(top.row, prior.info)

  prior <- prior.info[1, 1]
  error.var.prior <- prior.info[1, 3]
  state.weight <- 1
  xform <- 1
  control <- 0
  control.weight <- 0

  for(i in 1:nrow(prior.info)){
    # measurement update
    observation <- prior.info[i, 1]
    measurement.var <- prior.info[i, 3]
    gain <- (error.var.prior * xform) / (xform * error.var.prior * xform + measurement.var)
    state <- prior + gain * (observation - xform * prior)
    error.var <- (1 - gain * xform)* error.var.prior

    # time update
    state.var <- measurement.var
    prior <- state.weight * state + control.weight * control
    error.var.prior <- state.weight * error.var * state.weight + state.var
  }

  return(prior)
}

KalmanFilter <- function(test, simple=TRUE, timemap=identity){
  price.names <- sapply(10:1, function(idx) paste('trade_price_last', idx, sep=''))
  curve.names <- sapply(10:1, function(idx) paste('curve_based_price_last', idx, sep=''))
  timediff.names <- sapply(10:1, function(idx) paste('received_time_diff_last', idx, sep=''))

  BoundPredict <- function(row) return(Predict(price.names, curve.names, timediff.names, timemap, row))
  predictions <- apply(test, 1, BoundPredict)
  
  predictions_df  <- data.frame(test$id, predictions)
  names(predictions_df)  <- c("id", "trade_price")
  return(predictions_df)
}

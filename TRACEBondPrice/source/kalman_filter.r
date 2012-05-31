# trying a Kalman filter approach..
# in first attempt, try a filter which only
# uses trade prices to determine state
# in second attempt, try a filter which uses trade prices
# and curve prices to determine state
# for second attempt, use the training data to somehow
# determine how to weight between last trade price and
# curve price (presumably based on the time diff)

minisculeVar <- 1e-10

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
  used.timediffs <- c(row[timediff.names], 0)
  prior.timediffs <- timemap(-diff(used.timediffs))
  prior.info <- cbind(prior.prices, prior.curves, prior.timediffs)
  prior.info <- subset(prior.info, !is.na(prior.info[, 1]) & !is.na(prior.info[, 3]))
  num.prior.points <- nrow(prior.info)
  
  state <- prior.info[1, 1]
  state.var <- 0
  state.error.var <- prior.info[1, 3]
  measurement.var <- minisculeVar
  state.weight <- 1
  xform <- 1
  control <- 0
  control.weight <- 0
  
  if(num.prior.points > 1){
    for(i in 2:num.prior.points){
      # time update
      prior <- state.weight * state + control.weight * control
      state.error.var.prior <- state.weight * state.error.var * state.weight + state.var
      
      # measurement update
      observation <- prior.info[i, 1]
      gain <- (state.error.var.prior * xform) / (xform * state.error.var.prior * xform + measurement.var)
      state <- prior + gain * (observation - xform * prior)
      state.error.var <- (1 - gain * xform) * state.error.var.prior

      # state and measurement var - lot of flexibility here..
      # intuition is that state var reflects the uncertainty of our estimate of the state,
      # while measurement var reflects the uncertainty of the latest measurement.
      # so reasonable to assume that
      # (1) state var should be proportional to timediff[n-1] (which led to the
      # current observation), i.e., if it took a long time to get to the current
      # observation, then there's a lot of uncertainty around that number;
      # (2) measurement var should be inversely proportional to timediff[n]
      # (which leads to the next observation), i.e., if there's a long time to the next
      # observation, then this is an informative observation
      # if we add the req that timediff[n] = timediff[n-1] implies measurement var
      # equals state var, then one formulation would be
      # measurement var = state var X (timediff[n-1] / timediff[n])

      # add small constants to measurement var so that we avoid
      # measurement var = infinity and gain = 0/0
      state.var <- prior.info[i - 1, 3]
      measurement.var <- state.var * prior.info[i - 1, 3] / (prior.info[i, 3] + minisculeVar) + minisculeVar
    }
  }

  return(state)
}

KalmanFilter <- function(test, simple=TRUE, timemap=identity){
  price.names <- sapply(10:1, function(idx) paste('trade_price_last', idx, sep=''))
  curve.names <- sapply(10:1, function(idx) paste('curve_based_price_last', idx, sep=''))
  timediff.names <- sapply(10:1, function(idx) paste('received_time_diff_last', idx, sep=''))

  BoundPredict <- function(row) return(Predict(price.names, curve.names, timediff.names, timemap, row))
  predictions <- apply(test, 1, BoundPredict)
  
  predictions_df  <- data.frame(test$id, predictions)
  names(predictions_df)  <- c('id', 'trade_price')
  return(predictions_df)
}

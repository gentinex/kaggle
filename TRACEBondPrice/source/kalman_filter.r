# trying a Kalman filter approach..
# in first attempt, try a filter which only
# uses trade prices to determine state
# in second attempt, try a filter which uses trade prices
# and curve prices to determine state
# for second attempt, use the training data to somehow
# determine how to weight between last trade price and
# curve price (presumably based on the time diff)

# TODO:
# (1) try minor mods to the existing algorithm:
#     (a) use the value of the use.control flag to determine
#         what scale to use for the measurement var
#     (b) instead of comparing control to state, compare
#         control to prior (after calcing the prior based on state)
#         to determine whether or not to use the control
# (2) figure out how to incorporate trade sizes into the price
#     adjustments - only doing for variances so far..
#     when incorporating into variances, seems to work better on
#     curve-based than on simple KF
# (3) is there some way in which we can use the received time diffs?
#     perhaps we should construct some sort of visualization relating
#     received time diff to price changes (or variances of price changes)

# a thought on measurement var:
# -run the kalman filter over all training examples with some fixed
#  measurement var, determine the empirical measurement var,
#  then rerun with this var. continue until the measurement var
#  stabilizes. so basically, an EM algorithm
# -should we do this per-security, or just in aggregate??
# -does this make sense mathematically? would have thought that
#  maybe setting the measurement var a certain way means the
#  estimates will reflect that. or if not, it could just
#  represent how well the model reflects reality.
# -still, would be better to find a better way to dynamically
#  adjust the measurement var rather than use a static one throughout

# use minisculeVar to get around division by zero issues
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

Predict <- function(price.names, curve.names, timediff.names, size.names,
                    timemap, state.weight, control.weight, row){
  prior.prices <- row[price.names]
  prior.curves <- row[curve.names]
  used.timediffs <- c(row[timediff.names], 0)
  prior.timediffs <- timemap(-diff(used.timediffs))
  prior.sizes <- row[size.names]
  prior.info <- cbind(prior.prices, prior.curves, prior.timediffs, prior.sizes)
  prior.info <- subset(prior.info, !is.na(prior.info[, 1]) & !is.na(prior.info[, 3]))
  num.prior.points <- nrow(prior.info)
  
  state <- prior.info[1, 1]
  state.error.var <- 1
  xform <- 1
  use.control <- FALSE
  
  if(num.prior.points > 1){
    for(i in 2:num.prior.points){
      # state and measurement var - lot of flexibility here..
      # tried setting up a system in which the variances were
      # proportional / inversely proportional to the the
      # time from the previous trade, but this performed poorly
      # compared to using constant variances
      # for constant variances, using a measurementVar : stateVar
      # ratio around 10 seemed to work well
      # when using curve-based prices as a control, 1:1 works better
      state.var <- 1 / (prior.info[i - 1, 4] + minisculeVar)
      measurement.var <- 10 / (prior.info[i, 4] + minisculeVar)
      
      # time update
      # note the selective choice to not use the control if current state is closer
      # to the new observation than the control is
      control <- prior.info[i, 2]
      observation <- prior.info[i, 1]
      use.control <- !(control.weight==0 || is.na(control) || (abs(state - observation) < abs(control - observation)))
      if(!use.control)
        prior <- state
      else
        prior <- state.weight * state + control.weight * control
      state.error.var.prior <- state.weight * state.error.var * state.weight + state.var
      
      # measurement update
      gain <- (state.error.var.prior * xform) / (xform * state.error.var.prior * xform + measurement.var)
      state <- prior + gain * (observation - xform * prior)
      state.error.var <- (1 - gain * xform) * state.error.var.prior
    }
  }

  control <- row['curve_based_price']
  if(!use.control)
    final.state <- state
  else
    final.state <- state.weight * state + control.weight * control
  return(final.state)
}

KalmanFilter <- function(train, test, simple=TRUE, timemap=identity){
  price.names <- sapply(10:1, function(idx) paste('trade_price_last', idx, sep=''))
  curve.names <- sapply(10:1, function(idx) paste('curve_based_price_last', idx, sep=''))
  timediff.names <- sapply(10:1, function(idx) paste('received_time_diff_last', idx, sep=''))
  size.names <- sapply(10:1, function(idx) paste('trade_size_last', idx, sep=''))

  # in non-simple (i.e., curve) case, run a regression of trade price to
  # curve-based price and last trade price
  if(!simple){
    response <- train$trade_price
    impulse <- cbind(train$trade_price_last1, train$curve_based_price)
    fit <- lm.fit(impulse, response) #lm.fit runs much faster than lm, but requires inputs in matrix format
    state.weight <- fit$coefficients[1]
    control.weight <- fit$coefficients[2]
  }
  else{
    state.weight <- 1
    control.weight <- 0
  }

  BoundPredict <- function(row) return(Predict(price.names, curve.names, timediff.names, size.names,
                                               timemap, state.weight, control.weight, row))
  predictions <- apply(test, 1, BoundPredict)

  predictions_df  <- data.frame(test$id, predictions)
  names(predictions_df)  <- c('id', 'trade_price')
  return(predictions_df)
}

DisplayRow <- function(table, row.idx, simple=TRUE){
  row <- table[row.idx, ]
  price.names <- sapply(10:1, function(idx) paste('trade_price_last', idx, sep=''))
  size.names <- sapply(10:1, function(idx) paste('trade_size_last', idx, sep=''))
  type.names <- sapply(10:1, function(idx) paste('trade_type_last', idx, sep=''))
  curve.names <- sapply(10:1, function(idx) paste('curve_based_price_last', idx, sep=''))
  timediff.names <- sapply(10:1, function(idx) paste('received_time_diff_last', idx, sep=''))

  prior.prices <- row[price.names]
  prior.sizes <- row[size.names]
  prior.types <- row[type.names]
  prior.curves <- row[curve.names]
  prior.timediffs <- row[timediff.names]
  used.timediffs <- -diff(c(t(prior.timediffs), 0))
  history <- data.frame(t(prior.prices), t(prior.sizes), t(prior.types),
                        t(prior.curves), used.timediffs)
  names(history) <- c('prices','sizes','types','curves','timediffs')
  history <- rbind(history, c(row$trade_price, row$trade_size, row$trade_type,
                                    row$curve_based_price, NA))
  row.names(history) <- 10:0

  display <- list()
  display$bond_id <- row$bond_id
  if(!simple){
    display$id <- row$id
    display$current_coupon <- row$current_coupon
    display$time_to_maturity <- row$time_to_maturity
    display$is_callable <- row$is_callable
    display$reporting_delay <- row$reporting_delay
  }
  display$history <- history
  return(display)  
}

WeightedMSE <- function(prediction, actual){
  diffsq <- (prediction$trade_price - actual$trade_price) ** 2
  return(sum(diffsq * actual$weight) / sum(actual$weight))
}

RecordTime <- function(time){
  print(paste('Took', round((proc.time() - time)[['elapsed']], 2), 'seconds'))
  return(proc.time())
}

AveragePredictions <- function(prediction.list, weights=1){
  num.predictions <- length(prediction.list)
  stopifnot(length(weights) %in% c(1, num.predictions))
  if(length(weights) == 1)
    weights <- rep(weights, num.predictions)

  average.prediction <- data.frame(id=c(), trade_price=c())
  average.prediction.name <- ''

  for(i in 1:num.predictions){
    prediction <- read.csv(paste('../predict/', prediction.list[i], '.csv', sep=''), header=TRUE)
    if(i == 1){
      average.prediction <- prediction
      average.prediction$trade_price <- average.prediction$trade_price * weights[i]
      average.prediction.name <- prediction.list[1]
    }
    else{
      average.prediction$trade_price <- average.prediction$trade_price + prediction$trade_price * weights[i]
      average.prediction.name <- paste(average.prediction.name, prediction.list[i], sep='_')
    }
  }

  average.prediction$trade_price <- average.prediction$trade_price / sum(weights)
  write.csv(average.prediction, paste('../predict/Average_', average.prediction.name, '.csv', sep=''),
            quote=FALSE, row.names=FALSE)
}

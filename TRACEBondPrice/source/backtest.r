# scratch file to test out ideas
source('utils.r')
source('constant.r')
source('kalman_filter.r')

BacktestUnsupervised <- function(){
  time <- proc.time()
  print('Reading the data...')
  train <-read.csv( '../data/train.csv',  header = TRUE, na.strings = 'NA')[1:10,]
  time <- RecordTime(time)

  print('Making constant prediction...')
  predictions.constant = Constant(train)
  mse.constant <- mean((predictions.constant$trade_price - train$trade_price) ** 2)
  print(paste('MSE is', mse.constant))
  write(mse.constant, '../backtest/mse.constant')
  time <- RecordTime(time)

  print('Making basic Kalman Filter prediction...')
  prediction.kalman.id = KalmanFilter(train, simple=TRUE)
  print(prediction.kalman.id)
  print(train$trade_price)
  mse.kalman.id <- mean((prediction.kalman.id$trade_price - train$trade_price) ** 2)
  print(paste('MSE is', mse.kalman.id))
  write(mse.kalman.id, '../backtest/mse.kalman.id')
  time <- RecordTime(time)

  print('Making sqrt Kalman Filter prediction...')
  prediction.kalman.sqrt = KalmanFilter(train, timemap=sqrt, simple=TRUE)
  mse.kalman.sqrt <- mean((prediction.kalman.sqrt$trade_price - train$trade_price) ** 2)
  print(paste('MSE is', mse.kalman.sqrt))
  write(mse.kalman.sqrt, '../backtest/mse.kalman.sqrt')
  time <- RecordTime(time)

  print('Making square Kalman Filter prediction...')
  prediction.kalman.square = KalmanFilter(train, timemap=function(x) return(x*x), simple=TRUE)
  mse.kalman.square <- mean((prediction.kalman.square$trade_price - train$trade_price) ** 2)
  print(paste('MSE is', mse.kalman.square))
  write(mse.kalman.square, '../backtest/mse.kalman.square')
  time <- RecordTime(time)

  print('Making log Kalman Filter prediction...')
  prediction.kalman.log = KalmanFilter(train, timemap=log, simple=TRUE)
  mse.kalman.log <- mean((prediction.kalman.log$trade_price - train$trade_price) ** 2)
  print(paste('MSE is', mse.kalman.log))
  write(mse.kalman.log, '../backtest/mse.kalman.log')
  time <- RecordTime(time)
}


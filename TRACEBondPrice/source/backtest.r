# scratch file to test out ideas
source('utils.r')
source('constant.r')
source('kalman_filter.r')
source('simple_regression.r')

BacktestUnsupervised <- function(){
  time <- proc.time()
  print('Reading the data...')
  train <-read.csv( '../data/train.csv',  header = TRUE, na.strings = 'NA')
  time <- RecordTime(time)

  print('Making constant prediction...')
  predictions.constant = Constant(train)
  print(paste('Weighted MSE is', WeightedMSE(predictions.constant, train)))
  time <- RecordTime(time)

  print('Making basic Kalman Filter prediction...')
  predictions.kalman.id = KalmanFilter(train, train, simple=TRUE)
  print(paste('Weighted MSE is', WeightedMSE(predictions.kalman.id, train)))
  time <- RecordTime(time)

  print('Making sqrt Kalman Filter prediction...')
  predictions.kalman.sqrt = KalmanFilter(train, train, timemap=sqrt, simple=TRUE)
  print(paste('Weighted MSE is', WeightedMDS(predictions.kalman.sqrt, train)))
  time <- RecordTime(time)

  print('Making square Kalman Filter predictions...')
  predictions.kalman.square = KalmanFilter(train, train, timemap=function(x) return(x*x), simple=TRUE)
  print(paste('Weighted MSE is', WeightedMDS(predictions.kalman.square, train)))
  time <- RecordTime(time)
}

# fold a data set into training and test sets
# example: say we are foldting a set with 34 elements,
# and we fold into 10 test sets
# floor(34/10) = 3
# 3x + 4y = 34 and x + y = 10 is uniquely
# solved by x = 6, y = 4, so we fold the data by making
# the first 6 test sets each of size 3, and the last 4
# test sets each of size 4
# once we've determined a test set, the remaining data forms
# the training set
FoldData <- function(data, num.folds, fold.idx){
  num.entries <- nrow(data)
  stopifnot(fold.idx <= num.folds && fold.idx >= 1 && num.entries >= num.folds)
  num.entries.base <- floor(num.entries / num.folds)
  size.cutoff <- (num.entries.base + 1) * num.folds - num.entries
  if(fold.idx <= size.cutoff){
    num.test.entries <- num.entries.base
    start.test.index <- 1 + num.test.entries * (fold.idx - 1)
    end.test.index <- num.test.entries * fold.idx
  }
  else{
    num.test.entries <- num.entries.base + 1
    start.test.index <- 1 + size.cutoff * num.entries.base + num.test.entries * (fold.idx - size.cutoff - 1)
    end.test.index <- size.cutoff * num.entries.base + num.test.entries * (fold.idx - size.cutoff)
  }

  test <- data[start.test.index:end.test.index, ]
  train <- data[-start.test.index:-end.test.index, ]

  return(list(train=train, test=test))
}

CrossValidate <- function(num.folds){
  time <- proc.time()
  print('Reading the data...')
  train <-read.csv( '../data/train.csv',  header = TRUE, na.strings = 'NA')
  time <- RecordTime(time)

  for(i in 1:num.folds){
    print(paste("Building cross validation set", i))
    fold.data <- FoldData(train, num.folds, i)
    used.train <- fold.data$train
    used.test <- fold.data$test
    time <- RecordTime(time)

    print('Making Kalman filter sqrt prediction', i)
    predictions.kalman.sqrt <- KalmanFilter(train, used.test, timemap=sqrt, simple=TRUE)
    time <- RecordTime(time)

    print('Making Kalman filter identity prediction', i)
    predictions.kalman.identity <- KalmanFilter(train, used.test, timemap=identity, simple=TRUE)
    time <- RecordTime(time)

    #print('Making simple linear regression prediction', i)
    #predictions.regression <- SimpleRegression(used.train, used.test)
    #time <- RecordTime(time)

    print(paste('Kalman sqrt weighted MSE is', WeightedMSE(predictions.kalman.sqrt, used.test)))
    print(paste('Kalman identity weighted MSE is', WeightedMSE(predictions.kalman.identity, used.test)))
    #print(paste('Regression weighted MSE is', WeightedMSE(predictions.regression, used.test)))
    time <- RecordTime(time)
  }

}

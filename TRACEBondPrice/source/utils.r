RecordTime <- function(time){
  print(paste('Took', round((proc.time() - time)[['elapsed']], 2), 'seconds'))
  return(proc.time())
}

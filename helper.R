get.posixlt <- function(date) {
  return (as.POSIXlt(date, tz="", format=c('%d/%m/%Y %H:%M')));
}

get.difftimeInHours <- function(timeStart, timeEnd) {
  return (as.numeric(difftime(timeEnd, timeStart, units="hours")));
}


get.approxNaValues <- function(dataset) {  
  dateprev <- get.posixlt(dataset$ReadingDateTime[1]);
  values <- c(dataset$Value[1]);
  
  for(i in 2 : length(dataset$ReadingDateTime)) {
    date <- get.posixlt(dataset$ReadingDateTime[i]);
    
    if(is.na(date)) {
      # add 1 hour to date
      date <- dateprev + 60
    }
    
    if(abs(get.difftimeInHours(dateprev, date)) != 1) {
      emptyDataCount <- abs(get.difftimeInHours(dateprev, date))-1;
      prevIndex <- i-emptyDataCount-1;
      mean <- abs(( (dataset$Value[i]) - (dataset$Value[i-1]) ) / (emptyDataCount+1));
      for(e in 1 : emptyDataCount) {
        value <- values[length(values)] + mean
        if(is.nan(value) || is.infinite(value) || is.na(value)) {
          value <- values[length(values)-1]
          print(value)
        }
        values <- c(values, value);
      }
    }
    
    values <- c(values, dataset$Value[i]);
    dateprev <- date;
  }
  
  return (values);
}
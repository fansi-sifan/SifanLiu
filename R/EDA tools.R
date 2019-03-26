

# add leading zeros
padz <- function(x, n=max(nchar(x)))gsub(" ", "0", formatC(x, width=n))

# Share of NA values
na.share <- function(x)(sum(is.na(x))/length(x))

# quickly summarize factor levels
sfactor <- function(x)summary(as.factor(as.character(x)))

# read all csv files in a folder and bind_rows
read_all <- function(file.location){
  map_df(list.files(file.location, full.names = T), read.csv)
}





# add leading zeros
padz <- function(x, n=max(nchar(x)))gsub(" ", "0", formatC(x, width=n))

# Share of NA values
na.share <- function(x)(sum(is.na(x))/length(x))

# check distribution
range <- function(x)(summary(as.factor(x)))

# quickly summarize factor levels
sfactor <- function(x)summary(as.factor(as.character(x)))




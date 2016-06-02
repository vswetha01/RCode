
highest_correlation <- function(data, var) {
  # data is a dataframe
  # var is a column name
 
  numericCols <-sapply(data, is.numeric)
  
  dataSubset <- data[numericCols]
  ds <- dataSubset[ , -which(names(dataSubset) %in% var)]
  data2 <- sapply(ds, cor, x = dataSubset[var], use="pairwise.complete.obs")
  idx <- which.max(abs(data2))
  idx
  df <- (data2[idx])
  as.data.frame(df)
  name <- names(df)
  dResult<- data.frame( name,df)
  names(dResult)[1] <- "var_name"
  names(dResult)[2] <- "cor"
  dResult
 
  # Returns a data frame (with names: var_name, cor)
  # the first column is the variable name
  # the second column is the value of correlation
}
#cooking.data <- read.csv("cooking_game_popularity.csv")

#head(cooking.data) 
#numeric.cols <-sapply(cooking.data, is.numeric)

#cooking.data.subset <- cooking.data[numeric.cols]

#n <- ncol(cooking.data.subset)
#d2 <- cooking.data.subset[,c(2:n)]
#d101 <- highest_correlation(cooking.data, "popularity")

#str(d3)

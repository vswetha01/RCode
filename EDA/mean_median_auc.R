

mean_median_auc <- function(directory, id=1) {
  # "directory" is a character vector of length one indicating
  # the location of the CSV files  ￼￼￼￼￼
  # "id" is numeric vector of length one indicating the id of the campaign
  filePath1 <- paste(id, "_exp.csv", sep="")
  dirPath1 <- paste(getwd(), directory, sep="/")
  fPath <-file.path(dirPath1, filePath1)
  aucTreeIntial <- read.csv(fPath)
  
  aucTReeMean <- aggregate(aucTreeIntial, by= aucTreeIntial["num_trees"], mean) 
  aucTReeMean
  names(aucTReeMean)[names(aucTReeMean) == 'auc'] <- 'mean_auc'
  
  aucTReeMean<- subset(aucTReeMean, select = c("num_trees", "mean_auc" ))
  aucTReeMedian <- aggregate(aucTreeIntial, by= aucTreeIntial["num_trees"], median) 
  
  names(aucTReeMedian)[names(aucTReeMedian) == 'auc'] <- 'median_auc'
  aucTReeMedian <- subset(aucTReeMedian, select = c("num_trees", "median_auc"))
  aucTreeFinal <- merge(aucTReeMean, aucTReeMedian, by="num_trees")
  #aucTreeFinal <- subset(aucTreeFinal, select= c("num_trees", "mean_auc", "median_auc"))
  aucTreeFinal
  # Returns a data frame with the following columns
  # num_trees,mean_auc,median_auc
}

#mean_median_auc("exp-auc",2)

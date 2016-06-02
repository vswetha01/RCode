#getwd()
#setwd("/MSAN_USF/Courses_Bootcamp/ExploratoryDataAnalysis/Assignment")
mean_auc <- function(directory, campaings=1:47)
{
  cl <-paste(directory, "//",campaings, "_exp.csv", sep="")#list of files
  d1 <- sapply(cl, read.csv, header=TRUE, dec=".")

  C1 <- NULL
  C2 <- NULL

  for (i in campaings)
  {
    x <- 1
    m <- mean(d1[[x]])
    
    C1[i] <- i
    C2[i] <- m
    x <- x+5
  }
d100 <- cbind(C1, C2)
colnames(d100)[1] <- "campaign"
colnames(d100)[2] <- "mean_auc"
d104 <- data.frame(d100)
d104
}

#mean_auc("exp-auc", c(1:20))

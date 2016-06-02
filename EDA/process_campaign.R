process_campaign <- function(directory, id=1) {
  # "directory" is a character vector of length one indicating
  
  filePath1 <- paste(id, "_exp.csv", sep="")
  dirPath1 <- paste(getwd(), directory, sep="/")
  zz <-file.path(dirPath1, filePath1)
  campaignDataA <- read.csv(zz)
  campaignDataA$campaign <- id
  
  #id_ci.csv
  filePathCI <- paste(id, "_ci.csv", sep="")
  dirPathCI <- paste(getwd(), directory, sep="/")
  zz1 <-file.path(dirPathCI, filePathCI)
  campaignDataB <- read.csv(zz1)
  campaignDataB$campaign <- id

  
  campaignDataB <- subset(campaignDataB, select = c("low_ci", "high_ci", "campaign")) 
  #campaignDataA[,c("low ci", "high ci", "campaign")]
  campaignDataC <- merge(campaignDataA, campaignDataB, by="campaign")
  campaignDataC
  campaignDataD <- subset(campaignDataC, (campaignDataC$low_ci< campaignDataC$auc  & campaignDataC$auc < high_ci))
 
  # Returns a data frame with the following columns
  # campaign,auc,num_trees,depth shrinkage,low_ci,high_ci
}

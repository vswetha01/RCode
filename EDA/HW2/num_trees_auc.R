  num_trees_auc <- function(directory, threshold=0.5)
  {
    # this function outputs a data frame with columns
    # num_trees,mean_auc
    
    fileList <- list.files(directory, pattern="_exp.csv")
    dataList <- vector("list",length(fileList))
    for (i in 1:length(fileList))
    {
      filep <- paste(directory, "//", fileList[i], sep="")
      campaign.data <- read.csv(filep)
      campaign.data$c_id <-sub("_exp.csv","",fileList[i])
      dataList[[i]] <- campaign.data
    }
    
    data_all <- rbindlist(dataList)
    data_all <- as.data.frame(data_all)
    data_by_auc <- aggregate(data_all["auc"], data_all["c_id"], mean) 
    colnames(data_by_auc)[2] <- "mean_auc"
    data_filter_by_threshold <- subset(data_by_auc,data_by_auc$mean_auc > threshold)
    data_by_cid <- subset(data_all, data_all$c_id %in% unique(data_filter_by_threshold$c_id))
    data_by_num_trees <-aggregate(data_by_cid["auc"], data_by_cid["num_trees"], mean)
    names(data_by_num_trees)[1] <- "mean_auc"
    data_by_num_trees
  }
  d <- num_trees_auc("exp-auc", 0.8)
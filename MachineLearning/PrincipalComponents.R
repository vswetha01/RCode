

install.packages("png", repos='http://cran.us.r-project.org')
install.packages("grid", repos='http://cran.us.r-project.org')
library(png, quietly = TRUE)  
library(grid, quietly = TRUE)

#####Question 1
directory <- "/MSAN_USF/courses_fall/621-MachineLearning/David_Uminsky.png"
#import the image as an array where each dimension of the array represents a different level of the RGB color scheme
David <- readPNG(directory)
#convert the image to a matrix whose entries are strings that represent color identifiers. This can be used for plotting
David_plot <- as.raster(David)  
dim(David_plot)
#plot the image
grid.raster(David_plot)

David_red_only <- David[, , 1]
#plot histogram
hist(David_red_only,main="Histogram for pixel intensity", 
     xlab="pixel", 
     border="blue", 
     col="green",
     las=1, 
     breaks=15)
###code to calculate the percent variance
pr.out <- prcomp(David_red_only, scale = TRUE)
dim(pr.out$rotation)
pr.var <- pr.out$sdev^2
pve <- pr.var / sum(pr.var)
par(mfrow = c(1,2))

ret = 0
for(i in 1:length(cumsum(pve))){
  if(round(cumsum(pve)[i], 5) >= 0.90){
    ret = i
    break
  }
}
plot(pve, xlab = "Principal Component", ylab = "Proportion of Variance Explained", ylim = c(0, 0.05), xlim = c(1, 250))

plot(cumsum(pve), xlab = "Principal Component", ylab = "Cumulative Proportion of 
     Variance Explained", ylim = c(0, 1), type = "b", xlim = c(1, 50))

abline(h=0.9, v=ret,col = "red")

######Question 1.c
W <- pr.out$rotation #the loading matrix
pc.image <- list()
num.pcs <- c(1,5,10,15,20,30,50,100,200)

#scale the original image
Image <- scale(David_red_only)
for(j in 1:length(num.pcs)){
  u.proj <- W
  #we will only use the first num.pcs PC loadings so set the remaining to 0
  u.proj[, (num.pcs[j] + 1) : 322] <- 0 
  
  #Make the projection
  projection <- (Image%*%u.proj)%*%t(u.proj)
  
  #to draw an image, values need to be between 0 and 1
  scaled <- (projection - min(as.numeric(projection)))
  scaled <- scaled / max(as.numeric(scaled))
  pc.image[[j]] <- as.raster(scaled)
}

#plot each of the images
grid.raster(pc.image[[1]])
#plot each of the images
grid.raster(pc.image[[2]])
#plot each of the images
grid.raster(pc.image[[3]])
#plot each of the images
grid.raster(pc.image[[4]])
#plot each of the images
grid.raster(pc.image[[5]])
#plot each of the images
grid.raster(pc.image[[6]])
#plot each of the images
grid.raster(pc.image[[7]])
#plot each of the images
grid.raster(pc.image[[8]])
#plot each of the images
grid.raster(pc.image[[9]])
#########################Question2 ###################

setwd("/MSAN_USF/courses_fall/621-MachineLearning/")
geneData <- read.table("TCGA_example.txt", header=TRUE, sep="")
#first set a seed for reproducability

#########################Question2.a ###################
#choose the training and test set randomly

#choose the training and test set randomly
x1 <- model.matrix(geneData$Gene.1~., data=geneData)[,-1]
dim(x1)
set.seed(1)
y1 <- geneData$Gene.1
train1 <- sample(1:nrow(x1), nrow(x1)*0.8)
test1 <- (-train1)
gdtrain  <- y1[train1]
tumr <- x1[train1,1]
gdtest <- y1[test1]

pr.out1 <- prcomp(x1[train1,], scale = TRUE)
#look at the names of the results of applying PCA
names(pr.out1)
pr.out1$rotation
#plot of PC1 against PC2 (the so-called bi-plot)

pr.var1 <- pr.out1$sdev^2
pve <- pr.var1 / sum(pr.var1)
par(mfrow = c(1,2))
ret = 0
for(i in 1:length(cumsum(pve))){
  if(round(cumsum(pve)[i], 5) >= 0.85){
    ret = i
    break
  }
}

plot(pve, xlab = "Principal Component", ylab = "Proportion of Variance Explained", ylim = c(0, 0.05), xlim = c(1, 250))
plot(cumsum(pve), xlab = "Principal Component", ylab = "Cumulative Proportion of 
     Variance Explained", ylim = c(0, 1.2), type = "b", xlim = c(1, 170))
abline(h=0.85, v=ret,col = "red")
#########################Question2.b ###################
##########################

dim(pr.out1$x)

pairs(pr.out1$x[,c(1,2,3,4)], pch = 21, 
      bg = c("red", "blue")[as.factor(tumr)])
par(xpd=TRUE)


legend(0.8, 0.1,as.vector(as.factor(c("Basal", "Normal"))), 
       fill = c("red","blue"))
#fit PCR across a number of PCs
pcr.fit <- pcr(Gene.1 ~., data = geneData, subset = train1, scale = TRUE, validation = "CV")
#look at the validation plot to choose the number of PCs
validationplot(pcr.fit, val.type = "MSEP")
summary(pcr.fit)
#look at the validation plot to choose the number of 
#########################Question2.c ###################
##########################
##calculate the MSPE on the test set using 114 PCs from above                
#predict the values on the test set
pcr.pred <- predict(pcr.fit, x1[test1, ], ncomp = 114)

#calculate the MSPE
mean((pcr.pred - gdtest)^2)
#final model
pcr.final.model <- pcr(y1 ~ x1, scale = TRUE, ncomp = 114)
summary(pcr.final.model)
coef(pcr.final.model)

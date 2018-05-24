set.seed(12345)
library(plyr)
library(randomForest)
data <- read.csv("TrainingSet.csv",header=TRUE,row.names = 1) # Read Data
colnames(data)[1] <- "Class"  # assign response class 
# split data in to predictors and response
dataX <- data[,2:length(data)] 
dataY <- as.factor(data[,1])

# handle missing values to median. Can also use mean , mode 
impute.value <- function(x) replace(x, is.na(x), median(x, na.rm = TRUE))
data.new <- sapply(dataX, function(x){
    if(is.numeric(x)){
            impute.value(x)
        } else {
            x
        }
    }
)
 dataX <- as.data.frame(data.new) # create data frame



# split dataX into training and test sets



 #############################################################################################
 #! MODEL WITHOUT CROSS-VALIDATION !#
 # ""ntree"" represents number of trees allowed to grow set accoring to your convienience 
 # ""nodesize"" regulates the depth of search in this case search depth is '5'.

 data.train.rf = randomForest(x=dataX,y=as.factor(dataY),ntree=1000, importance=TRUE,nodesize=10)
 data.train.rf
 Accuracy <- (1 - (sum(as.numeric(format(data.train.rf$confusion[,3],digits=7,nsmall=7))) / 2))
 
 paste("prediction accuracy is :",format(Accuracy,nsmall=6))
 Error.rate <- format(1 - Accuracy,nsmall = 6)
 paste("OOB estimate of  error rate:",Error.rate )

### save model for furthr usage
save(data.train.rf,file="rf.model.RData")
 
 #############################################################################################
 #!MODEL WITH CROSS-VALIDATION!#

data <- cbind(dataX,dataY)
colnames(data)[length(data)] <- "Class"


## Using built-in function ## PREFFER THIS METHOD 
K = 10
rf.cv <- rfcv(dataX,dataY,cv.fold=K,step=0.7,ntree=1000)


performance <- as.data.frame(cbind((1 - rf.cv$error.cv),rf.cv$error.cv))
colnames(performance) <- c("Accuarcy","Error.rate")
#print("accuracy and error rate per cross validation")
t(performance)
avg.accuracy <- format(mean(1 - rf.cv$error.cv),nsmall =4)
sd.accuracy <- format(sd(1 - rf.cv$error.cv),nsmall = 4) 
paste("Average Accuacy is :",avg.accuracy,"±",sd.accuracy,"for",K,"fold cross validation")

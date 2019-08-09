## Harvard Data Science Project
## Jerry Xia
## Third Assignment
## 07/25/2019

library(foreign)
library(ggm)
library(ROCR)
library(caret)
library(e1071)

# Read in Demographic data
DEMO <- read.xport("C:/Users/apple/Downloads/DEMO_I.XPT")

DEMO <- apply(DEMO,2, function(DEMO){
  if(sum(is.na(DEMO))>0){
    DEMO[is.na(DEMO)]<- quantile(DEMO,na.rm = T, probs = 0.5)
  }
  DEMO
})

# Read in Dietary Data
DR1TOT <- read.xport("C:/Users/apple/Downloads/DR1TOT_I.XPT")

DR2TOT <- read.xport("C:/Users/apple/Downloads/DR2TOT_I.XPT")

DR2FF <- read.xport("C:/Users/apple/Downloads/DR2IFF_I.XPT")

DR1FF <- read.xport("C:/Users/apple/Downloads/DR1IFF_I.XPT")

# Read in Mental Health data
DPQ <- read.xport("C:/Users/apple/Downloads/DPQ_I.XPT")

DPQ <- apply(DPQ,2, function(DPQ){
  if(sum(is.na(DPQ))>0){
    DPQ[is.na(DPQ)]<- quantile(DPQ,na.rm = T, probs = 0.5)
  }
  DPQ
})

DPQsum <- NULL
for (i in 1:5735){
  DPQsum[i] <- sum(DPQ[i,2:11])
}

DPQ_new <- data.frame(DPQ,DPQsum)

DPQ_new$DPQsum[which(DPQ_new[,12]==1)] <- 0
DPQ_new$DPQsum[which(DPQ_new[,12]> 1)] <- 1


DPQsum[which(DPQsum==1)]<- 0
DPQsum[which(DPQsum> 1)]<- 1

# Sample 10% of DR1FF 
set.seed(123456)
hs <- nrow(DR1FF)
specimen <- sample(hs,0.1*hs)
reDR1 <- DR1FF[specimen,]

# Sample 10% of DR2FF
set.seed(111111)
ht <- nrow(DR2FF)
speciman <- sample(hs,0.1*ht)
reDR2 <- DR2FF[speciman,]

# Merge Dietary data into one
FOOD <- list(DR1TOT,DR2TOT,reDR2,reDR1)
Dietary <- Reduce(function(x,y) merge(x,y,all=T),FOOD)

# Merge DEMO and Dietary Data
var <- merge(DEMO,Dietary,by="SEQN")

var <- apply(var,2, function(var){
  if(sum(is.na(var))>0){
    var[is.na(var)]<- quantile(var,na.rm = T, probs = 0.5)
  }
  var
})

dat <- merge(DPQ_new,var)

dat <- dat[,-1:-11]

matrix <- cor(dat)


# Divide Mental Health data into Training & Testing set
set.seed(5555)
hm <- nrow(dat)
sss <- sample(hm,0.7*hm)

training <- dat[sss,]
testing <- dat[-sss,]



m <- glm(DPQsum~., data=training, family = binomial(link = logit) )
summary(m)

n <- predict(m,newdata=testing,type="response")

l <- prediction(n,testing$DPQsum)

k <- performance(l,'auc')
k@y.values


# Confusion Matrix

predictions = unlist(l@predictions) 
labels = unlist(l@labels) 
pre = as.integer(predictions > 0.5) 
Confumatrix <- table(pre,labels) 

confusionMatrix(Confumatrix, positive = "1") 







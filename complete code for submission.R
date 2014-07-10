#This file includes codes generating prediction we submitted online
# We submit 8 csv files (one for each method) online
# Accuracy:
# 1)Tree                        74.632%              
# 2)Random Forest               76.546%
# 3)Boosting                    76.357%
# 4)Gaussian svm                61.119%
# 5)Knn    k=1                  65.548%
#          k=5                  64.507%
#          k=11                 63.342%
# 6)Majority vote 
# (Tree,Randomforest,Boosting)  77.313%

library(rattle)  # This is a package for data mining in R
library(data.table)
library(e1071)
library(randomForest)
library(sqldf)
library(kernlab)
library(MASS)
library(klaR)
library(gbm)
library(tree)
library(glmnet)


#We use fread in package "data.table" to read csv file directly, 
#  which will take around 1 min each for test and train dataset



train<-fread("train.csv", sep="auto", sep2="auto", nrows=-1, header="auto", na.strings="NA", stringsAsFactors=FALSE)
save(train,file="train.RData")

test<-fread("test.csv", sep="auto", sep2="auto", nrows=-1, header="auto", na.strings="NA", stringsAsFactors=FALSE)
save(test,file="test.RData")




######################
Dev=unique(train$Device) #lengthof Dev is 387

d=list()
for( i in Dev){
  d[[i]]=train[train$Device==i,]
}
#save(d,file="seperationofDevice.RData")


#generate 1000 starting points 
id=list()
for(i in Dev){
  id[[i]]=sample(nrow(d[[i]])-300,1000,replace= TRUE)
}

#save(id,file="id.RData")

namestr=c("MeanX","MeanY","MeanZ",
          "VarX","VarY","VarZ",
          "MinX","MinY","MinZ",
          "quan25_X","quan25_Y","quan25_Z",
          "MedianX","MedianY","MedianZ",
          "quan75_X","quan75_Y","quan75_Z",
          "MaxX","MaxY","MaxZ",
          "RangeX","RangeY","RangeZ",
          "SkewnessX","SkewnessY","SkewnessZ",
          "KurtosisX","KurtosisY","KurtosisZ",
          "CorrXY","CorrXZ","CorrYZ",
          "Lag1X","Lag1Y","Lag1Z",
          "Lag1XY","Lag1XZ","Lag1YZ",
          "Lag2X","Lag2Y","Lag2Z",
          "Lag2XY","Lag2XZ","Lag2YZ",
          "Lag5X","Lag5Y","Lag5Z",
          "Lag5XY","Lag5XZ","Lag5YZ",
          "Lag10X","Lag10Y","Lag10Z",
          "Lag10XY","Lag10XZ","Lag10YZ",
          "Lag15X","Lag15Y","Lag15Z",
          "Lag20X","Lag20Y","Lag20Z",
          "Lag30X","Lag30Y","Lag30Z",
          "CorrTX","CorrTY","CorrTZ",
          "Timerange"
          )

firstquantile<-function(value){
  quantile(value,prob=0.25)
}

thirdquantile<-function(value){
  quantile(value,prob=0.75)  
}

getrange<-function(value){
  max(value)-min(value)
}  

replaceNA<-function(value){
  value[is.na(value)]<-mean(value,na.rm=T)
  value
}

sample=data.frame()


for(k in Dev[1:387]){
  
extract=matrix(NA,nrow=1000,ncol=70)
  
for(i in 1:1000){
tmp=d[[k]][id[[k]][i]:(id[[k]][i]+299),]

extract[i,1:3]=as.numeric(colMeans(tmp[,2:4]))        #mean
extract[i,4:6]=as.numeric(apply(tmp[,2:4],2,var))     #Variance
extract[i,7:9]=as.numeric(apply(tmp[,2:4],2,min))
extract[i,10:12]=as.numeric(apply(tmp[,2:4],2,firstquantile))
extract[i,13:15]=as.numeric(apply(tmp[,2:4],2,median))
extract[i,16:18]=as.numeric(apply(tmp[,2:4],2,thirdquantile))
extract[i,19:21]=as.numeric(apply(tmp[,2:4],2,max))
extract[i,22:24]=as.numeric(apply(tmp[,2:4],2,getrange))
extract[i,25:27]=as.numeric(apply(tmp[,2:4],2,skewness))
extract[i,28:30]=as.numeric(apply(tmp[,2:4],2,kurtosis))
extract[i,31]=cor(tmp$X,tmp$Y)
extract[i,32]=cor(tmp$X,tmp$Z)
extract[i,33]=cor(tmp$Y,tmp$Z)

extract[i,34]=cor(tmp$X[1:295],tmp$X[6:300]) #lag 1
extract[i,35]=cor(tmp$Y[1:295],tmp$Y[6:300])
extract[i,36]=cor(tmp$Z[1:295],tmp$Z[6:300])

extract[i,37]=cor(tmp$X[1:295],tmp$Y[6:300])
extract[i,38]=cor(tmp$X[1:295],tmp$Z[6:300])
extract[i,39]=cor(tmp$Y[1:295],tmp$Z[6:300])

extract[i,40]=cor(tmp$X[1:290],tmp$X[11:300]) #lag 2
extract[i,41]=cor(tmp$Y[1:290],tmp$Y[11:300])
extract[i,42]=cor(tmp$Z[1:290],tmp$Z[11:300])

extract[i,43]=cor(tmp$X[1:290],tmp$Y[11:300])
extract[i,44]=cor(tmp$X[1:290],tmp$Z[11:300])
extract[i,45]=cor(tmp$Y[1:290],tmp$Z[11:300])

extract[i,46]=cor(tmp$X[1:275],tmp$X[26:300]) #lag 5
extract[i,47]=cor(tmp$Y[1:275],tmp$Y[26:300])
extract[i,48]=cor(tmp$Z[1:275],tmp$Z[26:300])

extract[i,49]=cor(tmp$X[1:275],tmp$Y[26:300])
extract[i,50]=cor(tmp$X[1:275],tmp$Z[26:300])
extract[i,51]=cor(tmp$Y[1:275],tmp$Z[26:300])

extract[i,52]=cor(tmp$X[1:250],tmp$X[51:300]) #lag 10
extract[i,53]=cor(tmp$Y[1:250],tmp$Y[51:300])
extract[i,54]=cor(tmp$Z[1:250],tmp$Z[51:300])

extract[i,55]=cor(tmp$X[1:250],tmp$Y[51:300]) 
extract[i,56]=cor(tmp$X[1:250],tmp$Z[51:300])
extract[i,57]=cor(tmp$Y[1:250],tmp$Z[51:300])


extract[i,58]=cor(tmp$X[1:225],tmp$X[76:300]) #lag 15
extract[i,59]=cor(tmp$Y[1:225],tmp$Y[76:300])
extract[i,60]=cor(tmp$Z[1:225],tmp$Z[76:300])


extract[i,61]=cor(tmp$X[1:200],tmp$X[101:300]) #lag 20
extract[i,62]=cor(tmp$Y[1:200],tmp$Y[101:300])
extract[i,63]=cor(tmp$Z[1:200],tmp$Z[101:300])

extract[i,64]=cor(tmp$X[1:150],tmp$X[151:300]) #lag 30
extract[i,65]=cor(tmp$Y[1:150],tmp$Y[151:300])
extract[i,66]=cor(tmp$Z[1:150],tmp$Z[151:300])



extract[i,67]=cor(tmp$T,tmp$X)
extract[i,68]=cor(tmp$T,tmp$Y)
extract[i,69]=cor(tmp$T,tmp$Z)

extract[i,70]=tmp$T[300]-tmp$T[1]

}

extract=data.frame(extract)

names(extract)=namestr

extract$Device=k
  
sample=rbind(sample,extract)

}



#deal with missing in unscaled sample
nomissing=apply(sample[,1:70],2,replaceNA)
nomissing=data.frame(cbind(nomissing,sample$Device))
sample=nomissing
names(sample)[71]="Device"

#save(sample,file="unscaled sample without NA.RData")



#########################################################################
# take out sample for 387 binary classifiers
########################################################################

for(k in Dev){
  t1=sample[sample$Device==k,]
  t2=sample[!sample$Device==k,]
  ID=sample(nrow(t2),1000)
  t3=sample[ID,]
  t3$Device=-k
  train=rbind(t1,t3)
  train$Device=as.factor(train$Device)
  
 # save 387 RData in folder "traindata" 
  save(train,file=paste("traindata/train",k,".RData",sep=""))
}


################################################################
# get ready testing set
##################################################################
Seq=unique(test$SequenceId)

extract=matrix(NA,nrow=length(Seq),ncol=71)

for(i in 1:90024){
  tmp=test[((i-1)*300+1):(i*300),]
  
  extract[i,1:3]=as.numeric(colMeans(tmp[,2:4]))        #mean
  extract[i,4:6]=as.numeric(apply(tmp[,2:4],2,var))     #Variance
  extract[i,7:9]=as.numeric(apply(tmp[,2:4],2,min))
  extract[i,10:12]=as.numeric(apply(tmp[,2:4],2,firstquantile))
  extract[i,13:15]=as.numeric(apply(tmp[,2:4],2,median))
  extract[i,16:18]=as.numeric(apply(tmp[,2:4],2,thirdquantile))
  extract[i,19:21]=as.numeric(apply(tmp[,2:4],2,max))
  extract[i,22:24]=as.numeric(apply(tmp[,2:4],2,getrange))
  extract[i,25:27]=as.numeric(apply(tmp[,2:4],2,skewness))
  extract[i,28:30]=as.numeric(apply(tmp[,2:4],2,kurtosis))
  
  
  extract[i,31]=cor(tmp$X,tmp$Y)
  extract[i,32]=cor(tmp$X,tmp$Z)
  extract[i,33]=cor(tmp$Y,tmp$Z)
  
  extract[i,34]=cor(tmp$X[1:295],tmp$X[6:300]) #lag 1
  extract[i,35]=cor(tmp$Y[1:295],tmp$Y[6:300])
  extract[i,36]=cor(tmp$Z[1:295],tmp$Z[6:300])
  
  extract[i,37]=cor(tmp$X[1:295],tmp$Y[6:300])
  extract[i,38]=cor(tmp$X[1:295],tmp$Z[6:300])
  extract[i,39]=cor(tmp$Y[1:295],tmp$Z[6:300])
  
  extract[i,40]=cor(tmp$X[1:290],tmp$X[11:300]) #lag 2
  extract[i,41]=cor(tmp$Y[1:290],tmp$Y[11:300])
  extract[i,42]=cor(tmp$Z[1:290],tmp$Z[11:300])
  
  extract[i,43]=cor(tmp$X[1:290],tmp$Y[11:300])
  extract[i,44]=cor(tmp$X[1:290],tmp$Z[11:300])
  extract[i,45]=cor(tmp$Y[1:290],tmp$Z[11:300])
  
  extract[i,46]=cor(tmp$X[1:275],tmp$X[26:300]) #lag 5
  extract[i,47]=cor(tmp$Y[1:275],tmp$Y[26:300])
  extract[i,48]=cor(tmp$Z[1:275],tmp$Z[26:300])
  
  extract[i,49]=cor(tmp$X[1:275],tmp$Y[26:300])
  extract[i,50]=cor(tmp$X[1:275],tmp$Z[26:300])
  extract[i,51]=cor(tmp$Y[1:275],tmp$Z[26:300])
  
  extract[i,52]=cor(tmp$X[1:250],tmp$X[51:300]) #lag 10
  extract[i,53]=cor(tmp$Y[1:250],tmp$Y[51:300])
  extract[i,54]=cor(tmp$Z[1:250],tmp$Z[51:300])
  
  extract[i,55]=cor(tmp$X[1:250],tmp$Y[51:300]) 
  extract[i,56]=cor(tmp$X[1:250],tmp$Z[51:300])
  extract[i,57]=cor(tmp$Y[1:250],tmp$Z[51:300])
  
  
  extract[i,58]=cor(tmp$X[1:225],tmp$X[76:300]) #lag 15
  extract[i,59]=cor(tmp$Y[1:225],tmp$Y[76:300])
  extract[i,60]=cor(tmp$Z[1:225],tmp$Z[76:300])
  
  
  extract[i,61]=cor(tmp$X[1:200],tmp$X[101:300]) #lag 20
  extract[i,62]=cor(tmp$Y[1:200],tmp$Y[101:300])
  extract[i,63]=cor(tmp$Z[1:200],tmp$Z[101:300])
  
  extract[i,64]=cor(tmp$X[1:150],tmp$X[151:300]) #lag 30
  extract[i,65]=cor(tmp$Y[1:150],tmp$Y[151:300])
  extract[i,66]=cor(tmp$Z[1:150],tmp$Z[151:300])
  
  extract[i,67]=cor(tmp$T,tmp$X)
  extract[i,68]=cor(tmp$T,tmp$Y)
  extract[i,69]=cor(tmp$T,tmp$Z)
  
  extract[i,70]=tmp$T[300]-tmp$T[1]
  
  extract[i,71]=Seq[i]
}

te=data.frame(extract)


names(te)[1:70]=namestr
names(te)[71]="SequenceId"

# Merge question with transformed test
merge_test_question=merge(te,question,by.x="SequenceId",by.y="SequenceId")
te=merge_test_question[,c(2:71,1,72,73)]


#save(te,file="unscaled transformtest.RData")


#############################################################
# Train 387 random Forests
#############################################################
rf=list()
for(k in Dev){

load(paste("traindata/train",k,".RData",sep=""))
  
rf[[k]]<-randomForest(Device~.,data=train,importance=T,mtry=8,ntree=50,na.action=na.omit)
}

#save(rf,file="rf.RData")

######################################
# Predict with 387 Random Forests
#####################################
rf.result=rep(NA,90024)

for(s in 1:90024){
k=te$Device[s]
pred=as.numeric(as.character(predict(rf[[k]],te)))
rf.result[s]=ifelse(pred>0,1,0)
}

rfsubmit=cbind(question$QuestionId, rf.result)
rfsubmit=data.frame(rfsubmit)
names(rfsubmit)=c("QuestionId","IsTrue")
write.csv(rfsubmit,"rfsubmit.csv")

#########################################
# Train 387 trees
#########################################
TREE=list()

for(k in Dev){
  
  load(paste("traindata/train",k,".RData",sep=""))
  
  bigtree=tree(Device~., train, mindev=0.005, minsize=2)
  cv=cv.tree(bigtree, K=5)
  bestsize=cv$size[which.min(cv$dev)] 
  TREE[[k]]<-prune.tree(bigtree,best=bestsize,method="misclass")
}

#save(TREE,file="tree.RData")

#######################################
# Predict with 387 trees
#######################################
tree.result=rep(NA,90024)

for(s in 1:90024){
  k=te$Device[s]
  pred=as.numeric(as.character(predict(TREE[[k]],te[s,],type="class")))
  tree.result[s]=ifelse(pred>0,1,0)
}

treesubmit=cbind(question$QuestionId, tree.result)
treesubmit=data.frame(treesubmit)
names(treesubmit)=c("QuestionId","IsTrue")
write.csv(treesubmit,"treesubmit.csv",row.names=F)
#########################################
# Train 387 gaussian kernel SVMs
########################################
gsvm=list()
for(k in Dev){
  
  load(paste("traindata/train",k,".RData",sep=""))
  
 gsvm[[k]]<-ksvm(Device~.,data=train,type="C-svc", kernel='rbf',scaled=c())
  
}

#save(gsvm,file="unscaled gsvm.RData")
#########################################
# Predict with 387 gaussian kernel SVMs
########################################
gsvm.result=rep(NA,90024)

for(s in 1:90024){
  k=te$Device[s]
  
  pred=as.numeric(as.character(predict(gsvm[[k]], te[s,1:70])))
  gsvm.result[s]=ifelse(pred>0,1,0)
}

gsvmsubmit=cbind(question$QuestionId, gsvm.result)
gsvmsubmit=data.frame(gsvmsubmit)
names(gsvmsubmit)=c("QuestionId","IsTrue")
write.csv(gsvmsubmit,"gsvmsubmit.csv",row.names=F)

####################################
# Train 387 Boosting classifier
####################################
boost=list()
size=rep(NA,387)
 
for(k in Dev){
  
load(paste("traindata/train",k,".RData",sep=""))
train$Device=ifelse(as.numeric(as.character(train$Device))==k,1,0)
boost[[k]]=gbm(Device~.,train,distribution="adaboost",n.tree=1000,shrinkage=0.05,bag.fraction=0.5,cv.folds= 5)
size[k]=gbm.perf(boost[[k]],method="cv")

train$Device=ifelse(train$Device==1,k,-k)
}

#save(boost,size,file="boost.RData")
#####################################
# Predict with 387 Boosting classifier
######################################
boosting.result=rep(NA,90024)

for(s in 1:90024){
  k=te$Device[s]
  pred=predict.gbm(boost[[k]],n.tree=size[k],newdata=te[s,1:70])  
  boosting.result[s]=ifelse(pred>0,1,0)
}

boostsubmit=cbind(question$QuestionId, boosting.result)
boostsubmit=data.frame(boostsubmit)
names(boostsubmit)=c("QuestionId","IsTrue")
write.csv(boostsubmit,"boostsubmit.csv",row.names=F)


#########################
# train 387 Knn and predict
#########################
knn.result=rep(NA,90024)

for(s in 1:90024){
  
k=te[s,73]

load(paste("traindata/train",k,".RData",sep=""))  

knn.result[s]= knn(train[,1:70], te[s,1:70],train$Device, k=1)

}

# knn.result=2, then <-1
# knn.result=1  then<-0

knn.pred=ifelse(as.numeric(knn.result)==2,1,0)

knnsubmit=cbind(question$QuestionId, knn.pred)
knnsubmit=data.frame(knnsubmit)
names(knnsubmit)=c("QuestionId","IsTrue")
write.csv(knnsubmit,"knnsubmit.csv",row.names=F)



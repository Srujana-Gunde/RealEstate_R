library(randomForest)
library(ggplot2)
library(dplyr)
library(tree)
library(cvTools)

ld_train=read.csv("C:/Users/user/Downloads/housing_train.csv",stringsAsFactors = F)

ld_test= read.csv("C:/Users/user/Downloads/housing_test.csv",stringsAsFactors = F)


ld_test$Price=NA
str(ld_train)

ld_train$data='train'
ld_test$data='test'
ld_all=rbind(ld_train,ld_test)


lapply(ld_all,function(x) sum(is.na(x)))


ld_all$CouncilArea=NULL
ld_all$YearBuilt=NULL
ld_all$Method=NULL


ld_all=ld_all[!(is.na(ld_all$Distance)),]

for(col in names(ld_all)){
  
  if(sum(is.na(ld_all[,col]))>0 & !(col %in% c("data","Price"))){
    
    ld_all[is.na(ld_all[,col]),col]=mean(ld_all[,col],na.rm=T)
  }
  
}


View(ld_all)

CreateDummies=function(data,var,freq_cutoff=0){
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  
  for( cat in categories){
    name=paste(var,cat,sep="_")
    name=gsub(" ","",name)
    name=gsub("-","_",name)
    name=gsub("\\?","Q",name)
    name=gsub("<","LT_",name)
    name=gsub("\\+","",name)
    
    data[,name]=as.numeric(data[,var]==cat)
  }
  
  data[,var]=NULL
  return(data)
}

glimpse(ld_all)

ld_all=CreateDummies(ld_all ,"Suburb",500)
ld_all=CreateDummies(ld_all,"Address",500)
ld_all=CreateDummies(ld_all ,"SellerG",500)
ld_all=CreateDummies(ld_all ,"Type",500)





ld_all$Bedroom2=as.integer(ld_all$Bedroom2)
ld_all$Bathroom=as.integer(ld_all$Bathroom)
ld_all$Car=as.integer(ld_all$Car)


View(ld_all)

ld_train=ld_all %>% filter(data=='train') %>% select(-data)
ld_test=ld_all %>% filter(data=='test') %>% select(-data,-Price)

##

set.seed(2)
s=sample(1:nrow(ld_train),0.8*nrow(ld_train))
ld_train1=ld_train[s,]
ld_train2=ld_train[-s,]


params=list(mtry=c(5,10),ntree=c(100,500),
            maxnodes=c(15,20),nodesize=(c(2,5)))

expand.grid(params)

## paramter values that we want to try out

param=list(mtry=c(5,10,15,20,25),
           ntree=c(50,100,200,500,700),
           maxnodes=c(5,10,15,20,30,50),
           nodesize=c(1,2,5,10))


## Function for selecting random subset of params

subset_paras=function(full_list_para,n=10){
  
  all_comb=expand.grid(full_list_para)
  
  s=sample(1:nrow(all_comb),n)
  
  subset_para=all_comb[s,]
  
  return(subset_para)
}

## 

num_trials=10
my_params=subset_paras(param,num_trials)
# Note: A good value for num_trials is around 10-20% of total possible 
# combination. It doesnt have to be always 50

## cvtuning for regression
## this code might take too long to run
## no need to execute completely in class
myerror=9999999

for(i in 1:num_trials){
  print(paste0('starting iteration:',i))
  # uncomment the line above to keep track of progress
  params=my_params[i,]
  
  k=cvTuning(randomForest,Price~.,
             data =ld_train,
             tuning =params,
             folds = cvFolds(nrow(ld_train), K=10, type = "random"),
             seed =2
  )
  score.this=k$cv[,2]
  
  if(score.this<myerror){
    print(params)
    # uncomment the line above to keep track of progress
    myerror=score.this
    print(myerror)
    # uncomment the line above to keep track of progress
    best_params=params
  }
  
  print('DONE')
  # uncomment the line above to keep track of progress
}

## from another run following values were obtained

 #myerror=1.870957
best_params=data.frame(mtry=20,
                       ntree=200,
                       maxnodes=50,
                       nodesize=10)

## Final model with obtained best parameters

ld.rf.final=randomForest(Price~.,
                         mtry=best_params$mtry,
                         ntree=best_params$ntree,
                         maxnodes=best_params$maxnodes,
                         nodesize=best_params$nodesize,
                         data=ld_train)

test.pred=predict(ld.rf.final,newdata = ld_test)
write.csv(test.pred,"mysubmission.csv",row.names = F)

ld.rf.final

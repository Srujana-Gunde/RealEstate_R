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
s=sample(1:nrow(ld_train),0.7*nrow(ld_train))
ld_train1=ld_train[s,]
ld_train2=ld_train[-s,]

### Building A Deecison Tree

ld.tree=tree(Price~.,data=ld_train1)

## Tree in text format

ld.tree

## Visual Format

plot(ld.tree)
text(ld.tree)

## Performance on validation set

val.IR=predict(ld.tree,newdata = ld_train2)

rmse_val=((val.IR)-(ld_train2$Price))^2 %>% mean() %>% sqrt()
rmse_val

ld.tree.final=tree(Price~.,data=ld_train)
test.pred=predict(ld.tree.final,newdata=ld_test)
write.csv(test.pred,"mysubmission.csv",row.names = F)


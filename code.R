rm(list=ls())
setwd("/media/patil/Others/Pankaj Patil/Analytics/AV Hackathon/Data Supremacy")
library(dplyr)
library(caret)
library(xgboost)
library(mice)
library(ggplot2)
train <- read.csv("train.csv",na.strings = "")
test <- read.csv("test.csv",na.strings = "")
plotter=0
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
# x <- train
## Visualization ##
if(plotter!=0){
  print(ggplot(data=x) + geom_bar(aes(x=x$city,fill=x$target, y=(..count..)/sum(..count..)))) # City_103 has most number of candidates
  print(ggplot(data=x) + stat_density(aes(x=x$city_development_index,fill=x$target, y=(..count..)/sum(..count..)))) #0.9+ cities have most  candidates
  print(ggplot(data=x) + geom_bar(aes(x=x$gender,fill=x$target, y=(..count..)/sum(..count..)))) # Male Candidates are plenty
  print(ggplot(data=x) + geom_bar(aes(x=x$relevent_experience,fill=x$target, y=(..count..)/sum(..count..)))) # Candidates with experience qualify more often
  print(ggplot(data=x) + geom_bar(aes(x=x$enrolled_university,fill=x$target, y=(..count..)/sum(..count..)))) # no_enrollment candidates form a large chunk
  print(ggplot(data=x) + geom_bar(aes(x=x$education_level,fill=x$target, y=(..count..)/sum(..count..)))) # Graduates are the most common demographic
  print(ggplot(data=x) + geom_bar(aes(x=x$major_discipline,fill=x$target, y=(..count..)/sum(..count..))))  # STEM
  print(ggplot(data=x) + geom_bar(aes(x=x$experience,fill=x$target, y=(..count..)/sum(..count..)))) # >20 are major demographic
  print(ggplot(data=x) + geom_bar(aes(x=x$company_size,fill=x$target, y=(..count..)/sum(..count..)))) # Fairly even distribution
  print(ggplot(data=x) + geom_bar(aes(x=x$company_type,fill=x$target, y=(..count..)/sum(..count..)))) # Pvt Ltd
  print(ggplot(data=x) + geom_bar(aes(x=x$last_new_job,fill=x$target, y=(..count..)/sum(..count..)))) # 1 accounts for more than 40%
  print(ggplot(data=x) + stat_density(aes(x=x$training_hours,fill=x$target, y=(..count..)/sum(..count..)))) # 75% under 89
  print(ggplot(data=x) + geom_bar(aes(x=x$target,fill=x$target, y=(..count..)/sum(..count..)))) #85% don't look for new job
}
missing <- c('enrolled_university', 'gender', 'education_level', 'major_discipline', 'experience', 'company_size', 'company_type','last_new_job')
process <- function(x){
  ## Most people are unenrolled in any university
  x$enrolled_university[is.na(x$enrolled_university)] <- 'no_enrollment'
  ## Most people are  STEM graduates
  x$education_level[is.na(x$education_level)] <- 'Graduate'
  x$major_discipline[is.na(x$major_discipline)] <- 'STEM'
  x$experience <- as.numeric(x$experience)
  x$experience[x$experience==">20"] <- 21
  x$experience[x$experience=="<1"] <- 0.5
  
  #There is some correlation between relevant exp and exp columns as verified by chisq test. 
  noxp <- 14
  haxp <- 2
  x$experience[is.na(x$experience) & x$relevent_experience=="Has relevent experience"] <- haxp
  x$experience[is.na(x$experience) & x$relevent_experience=="No relevent experience"] <- noxp
  x$experience <-  sapply(x$experience,function(z){
    if(z<1){z="A"}
    else if(z<5){z = "B"}
    else if (z<10){z = "C"}
    else if (z<15){z = "D"}
    else if (z<20){z = "E"}
    else {z = "F"}
  })
  x$experience <- as.factor(x$experience)
  x$company_size <- as.character(x$company_size)
  x$company_size[x$company_size=='100-500'] <- "D" #100
  x$company_size[x$company_size=='<10'] <-"A"       #1
  x$company_size[x$company_size=='50-99'] <- "C"    #50
  x$company_size[x$company_size=='10000+'] <- "G"            #10000
  x$company_size[x$company_size=='1000-4999'] <-"E"     # 1000
  x$company_size[x$company_size=='5000-9999'] <-"F"     # 5000
  x$company_size[x$company_size=='500-999'] <- "D"    # 500
  x$company_size[x$company_size=='10/49'] <-"B"  #10
  x$company_size[is.na(x$company_size)] <- "C"
  x$company_size <- factor(x$company_size,ordered=TRUE,levels=c("A","B","C","D","E","F","G"))
  ##Looking at the company size and company type table. ESS="C",FS=""
  a <- table(x$company_size,x$company_type)
  sums <- colSums(a)
  vec <- data.frame()
  j=1
  for (j in 1:dim(a)[1]){
    for (i in 1:dim(a)[2]){
      vec[j,i] = a[j,i]/sums[i]
    }
  }
  colnames(vec) <- colnames(a)
  rownames(vec) <- rownames(a)
  x$company_type[is.na(x$company_type) & x$company_size=='A'] <- 'Early Stage Startup'
  x$company_type[is.na(x$company_type) & x$company_size=='B'] <- 'Early Stage Startup'
  x$company_type[is.na(x$company_type) & x$company_size=='C'] <- 'Funded Startup'
  x$company_type[is.na(x$company_type) & x$company_size=='D'] <- 'NGO'
  x$company_type[is.na(x$company_type) & x$company_size=='E'] <- 'Public Sector'
  x$company_type[is.na(x$company_type) & x$company_size=='F'] <- 'Public Sector'
  x$company_type[is.na(x$company_type) & x$company_size=='G'] <- 'Pvt Ltd'
  x$company_type <-  sapply(x$company_type,function(z){
    if(z=="Early Stage Startup"){z="A"}
    else if(z=="Funded Startup"){z = "B"}
    else if (z=="NGO"){z = "C"}
    else if (z=="Other"){z = "D"}
    else if (z=="Pvt Ltd"){z = "E"}
    else {z = "F"}
  })
  
  
  x$last_new_job <- gsub(">4","5",x$last_new_job)
  x$last_new_job <- gsub("never","0",x$last_new_job)
  
  x$last_new_job[is.na(x$last_new_job)] <- 1
  x$city <- gsub("city_","",x$city)
  x$city <- as.factor(x$city)
  x$relevent_experience <- factor(x$relevent_experience,levels = c("No relevent experience","Has relevent experience"),ordered=TRUE)
  x$education_level <- factor(x$education_level,levels=c("Primary School" ,"High School" ,"Graduate","Masters", "Phd" ),ordered=TRUE)
  x$gender <- NULL
  x$major_discipline <- NULL
  
    return(x)
}
train <- process(train)
test  <- process(test)
g <- (table(train$city,train$target))
pass_perc <- data.frame(city = rownames(g),passrate = g[,2]/(g[,1]+g[,2]))
vec <- summary(pass_perc$passrate)
train <- merge(train,pass_perc,by="city",all.x=TRUE,all.y=FALSE)
test <- merge(test,pass_perc,by="city",all.x=TRUE,all.y=FALSE)
train$city <- NULL
test$city <- NULL

enrollee_id <- test[,1]
train <- train[,-1]
test <- test[,-1]
dummies <- dummyVars("~.",data=train)
data2 <- data.frame(predict(dummies,newdata=train,fullrank=TRUE))
dummies <- dummyVars("~.",data=test)
data3 <- data.frame(predict(dummies,newdata=test,fullrank=TRUE))

#======================================================================================================================================================#
data2 <- sapply(data2,as.numeric)
data3 <- sapply(data3,as.numeric)
param_list = list(
  eta=0.01,
  gamma = 1,
  max_depth=10,
  subsample=0.8,
  colsample_bytree=0.5
)
dtrain <- xgb.DMatrix(data=data2[,-34],label=data2[,34])
# xgb.cv(params = param_list,
#        eval_metric="auc",
#        data=dtrain,
#        nrounds = 2000,
#        nfold = 5,
#        print_every_n = 10,
#        early_stopping_rounds = 30,
# maximize = T)
classifier <- xgb.train(data=dtrain,params = param_list,nrounds=49)
var_imp = xgb.importance(feature_names=colnames(dtrain),model=classifier)
xgb.plot.importance(var_imp)
# data3 <- data3[,vec]
dtest <- xgb.DMatrix(data=data3)
y_pred <- predict(classifier,newdata=(dtest))

# classifier2 <- step(glm(formula=target~.,data=as.data.frame(data2)))
# y_pred2 <- predict(classifier2,newdata=as.data.frame(data3))
req.cols <- c("relevent_experience.L","enrolled_university.no_enrollment","education_level.L","education_level.Q",
              "education_level.4","company_size.C","company_size.4","company_typeA","company_typeB","company_typeC",
              "company_typeE","last_new_job0","last_new_job2","last_new_job4","passrate","target")
req.cols2 <- c("relevent_experience.L","enrolled_university.no_enrollment","education_level.L","education_level.Q",
              "education_level.4","company_size.C","company_size.4","company_typeA","company_typeB","company_typeC",
              "company_typeE","last_new_job0","last_new_job2","last_new_job4","passrate")

temp <- as.data.frame(data2)
data2 <- select(temp,req.cols)
temp <- as.data.frame(data3)
data3 <- select(temp,req.cols2)
data3 <- as.matrix(data3)
data2 <- as.matrix(data2)
dtrain <- xgb.DMatrix(data=data2[,-16],label=data2[,16])
dtest <- xgb.DMatrix(data=data3)
param_list = list(
  eta=0.01,
  gamma = 1,
  max_depth=10,
  subsample=0.8,
  colsample_bytree=0.8
)

# xgb.cv(params = param_list,
#        eval_metric="auc",
#        data=dtrain,
#        nrounds = 2000,
#        nfold = 5,
#        print_every_n = 10,
#        early_stopping_rounds = 50,
#        maximize = T)
classifier3 <- xgb.train(data=dtrain,params = param_list,nrounds=239)
y_pred3 <- predict(classifier3,newdata=data3)

# classifier4 <- xgb.train(data=dtrain,params = param_list,nrounds=500)
# y_pred4 <- predict(classifier4,newdata=data3)


final_pred <- y_pred*0.5 + y_pred3*0.5 
results <- data.frame(enrollee_id,target=final_pred)



write.csv(results,"em2.csv",row.names=FALSE)


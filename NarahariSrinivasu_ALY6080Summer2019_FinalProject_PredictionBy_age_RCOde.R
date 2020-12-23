
##install.packages("xlsx")
#install.packages("sqldf")
##install.packages("imputeTS")
##install.packages("randomForest")
library(randomForest)
library(imputeTS)
library(sqldf)
masterdata_age  = read.csv("C:/naraharitransactions/AddmissionsInfo/ALY6080 90571 Integrated Experiential Learn SEC 02 Summer 2019 CPS/Week5/video_age_gender.csv",1)
metadata = read.csv("C:/naraharitransactions/AddmissionsInfo/ALY6080 90571 Integrated Experiential Learn SEC 02 Summer 2019 CPS/Week5/video_meta.csv",1)

##age_data =  sqldf("select *  from masterdata_age ins  join metadata  meta using(hID)")
facebook_age_data =  sqldf("select meta.*,[U.35.44],[U.55.64],[F.25.34],[M.18.24],[M.55.64] from masterdata_age ins  join metadata  meta using(hID) where meta.Is_episode = 1")
##View(tvchannel_age_data)

tvchannel_age_data =  sqldf("select meta.*, [U.35.44],[U.55.64],[F.25.34],[M.18.24],[M.55.64]  from masterdata_age ins  join metadata  meta using(hID) where meta.Is_episode = 0")

facebook_age_data[c(1, 2)] <- list(NULL)
tvchannel_age_data[c(1,2)] <- list(NULL)

## Alternative Hypothesis Ha : muf > mut
t.test(facebook_age_data,tvchannel_age_data,mu = 0,alternate="greater")

facebook_age_data = na_replace(facebook_age_data,0)
tvchannel_age_data = na_replace(tvchannel_age_data,0)

set.seed(100)
train <- sample(nrow(facebook_age_data), 0.7*nrow(facebook_age_data), replace = FALSE)
TrainSet <- facebook_age_data[train,]
ValidSet <- facebook_age_data[-train,]
#View(TrainSet)


model1  =  randomForest(U.35.44 ~ ., data=TrainSet, importance = TRUE)
model1
model2  = randomForest(U.35.44 ~ ., data=TrainSet, nTree = 500, mTry = 6 , importance = TRUE)
model2

predictTrainSet = predict(model1,TrainSet,Type = "class")

##table(predictTrainSet, TrainSet$U.35.44)
prdedictvalidSet = predict(model2,ValidSet, Type = "class")

#table(prdedictvalidSet, ValidSet$total_video_views_unique)

## Predicting with TV Channels Data

PredictTvChannelsData =  predict(model2,tvchannel_age_data,Type = "class")
##PredictTvChannelsData

install.packages("rpart")
install.packages("rpart.plot") #그래프의 형태로 너를 도와줄것이다.
data2<-read.csv("dataset_0814.csv")
data<-data2[,c(4,6,7,8,9,19)]
str(data)


set.seed(1234)
library(caret)

##train/ test데이터 나누기#
# 종속변수 비율이 비슷하게 데이터 나누기 위해서 createDataPartition
index <- createDataPartition(y=data$ac_attend, p=0.7,list =FALSE)
train <- data[index,]
test <- data[-index,]

# createDataPartition이 잘 기능했는 지 확인
table(train$ac_attend)/sum(table(train$ac_attend))
table(test$ac_attend)/sum(table(test$ac_attend))

#rpart패키지
# cart(불순도 지표 : 지니 계수) 사용하면서 진행

library("rpart")
library("rpart.plot")
library("ggplot2")
rpart_tree <- rpart(ac_attend~.,train)

par(mfrow=c(1,1))
plot(rpart_tree);text(rpart_tree)
rpart.plot(rpart_tree,cex=0.7)
# cex = 글자 크기 


rpart_pred <- predict(rpart_tree, test, type="class")
# type ="class"를 하면 예측값(yes/no)를 뽑아준다.
confusionMatrix(rpart_pred,test$ac_attend)
# 정확도 나옴



##가지치기##
printcp(rpart_tree) #cp = 0.01일 때 xerror가 가장 작다
plotcp(rpart_tree)

rpart.prune <- prune(rpart_tree, cp=0.01,"CP")
rpart.plot(rpart.prune, cex=0.7)

rpart_pred2 = predict(rpart.prune,test, type="class")
confusionMatrix(rpart_pred2,test$rank)
#정확도 나옴


#rpart 모델로 test셋의 AHD를 예측하고 정확성을 평가해 보겠습니다.
rpartpred<-predict(ptree, test, type='class')
confusionMatrix(rpartpred, test$AHD)


#모델링 
RandomIndex<-sample(x=1:nrow(mydata),size=round(0.7*nrow(mydata)), replace = FALSE)
Traindat<-mydata[RandomIndex,]
Testdat<-mydata[-RandomIndex,]
model<-randomForest(y~.,data=Traindat,importance=TRUE)
randomforest_pred<-predict(model, Testdat)

RMSE<-sqrt(mean(Testdat$y - randomforest_pred)^2)
RMSE
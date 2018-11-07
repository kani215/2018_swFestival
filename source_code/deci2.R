
library(gmodels)
library(caret)
library(randomForest)
library(party)
library(datasets)
library(data.table)
# test, train 데이터 
DATA<-read.csv("dataset_0814.csv")
set.seed(2018)
RandomIndex<-sample(x=1:nrow(DATA),size=round(0.7*nrow(DATA)),replace=F)
Train<-DATA[RandomIndex,]
Test<-DATA[-RandomIndex,]
Train<-Train[,c(7,8,9,17)]              #<< 사용할 변수만 불러온다.
Test<-Test[,c(7,8,9,17)]
str(Test)

#1. 모델 결측치 처리
Train2 <- Train
Test2 <- Test

Train2$ac_attend[is.na(Train2$ac_attend)] <-0
Train2$attend[is.na(Train2$attend)] <-0
Train2$screen[is.na(Train2$screen)] <-0
Train2$act_point[is.na(Train2$act_point)] <-0
Train2$profit_line[is.na(Train2$profit_line)] <-0

Test2$ac_attend[is.na(Test2$ac_attend)] <-0
Test2$attend[is.na(Test2$attend)] <-0
Test2$screen[is.na(Test2$screen)] <-0
Test2$act_point[is.na(Test2$act_point)] <-0
Test2$profit_line[is.na(Test2$profit_line)] <-0

str(Test2)
Train2
Test2
Testa<-Test2[,c(2)]
Testa
#2. formula 생성 : Y변수 연속형
# ------------------------------

party_tree <- ctree(ac_attend~screen+attend+act_point,Train2)
plot(party_tree)

#3. 모델 그래프 시각화
party_pred <- predict(party_tree,Test2,type="response")
party_pred

cor(party_pred, Test$ac_attend)
#관객 예측 정확률 :  예측결과/실제결과
party_pred_per <-(1-(abs(party_pred - Testa)/Testa))*100
party_pred_per
#4. 모델링과 RMSE 지수  

model<-randomForest(ac_attend~attend+screen,data=Train2,importance=TRUE)
randomforest_pred<-predict(model, Test3)      #x 정의역 변수를 넣으면
RMSE<-sqrt(mean(Test3$ac_attend - randomforest_pred)^2)
RMSE


#5. 모델 평가 작업 - 정의역 변수 1개 데이터 값 대입 후 실제 누적관객수와 비교 
# ex. 실제 영화인 검은사제들의 데이터값을 대입 
Test3 <-c(2675618)
Test3 <-cbind(Test3,1124)
Test3 <-cbind(Test3,0)
Test3 <-cbind(Test3,1299451)
colnames(Test3)<-c("ac_attend","screen","act_point","attend")
Test3
Test3<-as.data.table(Test3)

randomforest_pred<-predict(model, Test3)      
randomforest_pred

RMSE<-sqrt(mean(Test3$ac_attend - randomforest_pred)^2)
RMSE


#CrossTable
CrossTable(x=Test3$ac_attend, y=model, prop.chisq =F)





install.packages("neuralnet")
# test, train 데이터 
DATA<-read.csv("dataset_0814.csv",header=TRUE)
set.seed(2018)
RandomIndex<-sample(x=1:nrow(DATA),size=round(0.7*nrow(DATA)),replace=F)
Train<-DATA[RandomIndex,]
Test<-DATA[-RandomIndex,]
Train<-Train[,c(7,8,9)]              #<< 사용할 변수만 불러온다.
Test<-Test[,c(7,8,9)]
Test
str(Test)
library("neuralnet")
model <-neuralnet(ac_attend~screen+attend , 
                  data=Train,hidden=c(5,3))

#사전에 분류한 Traindataset에서 모델링될 ac_attend(누적 관람객) 를 사용한다.
# 인공신경망에 screen+attend 를 학습시킨다.
# 데이터는 사전에 분류한 Train을 이용하여, 은닉층은 3개를 사용하낟.

model_result <-compute(model,Test[,c("attend","screen")])

#model_result 에는 사전에 학습된 model을 이용해 Test파일에 있는
# screen, attend 변수를 이용해 예측해봅니다.

predict_attend <-model_result$net.result

#Test의 attend 예측값을 predict_attend변수에 대입.

cor(predict_attend, Test$ac_attend)

#predict_attend 예측값 과 실제 값 사이의 상관계수 

plot(Test$ac_attend,predict_attend,xlab="실제 값",ylab="예측 값",)
#x축을 예측값, y축을 실제값을 넣어 그래프를 그린다.
abline(a=0,b=1,col="red")

#선형함수로 제시

# Attribute 값 추가

model <-neuralnet(ac_attend~screen+attend +dir_score,
                  data=Train,hidden=c(5,3))

#사전에 분류한 Traindataset에서 모델링될 ac_attend(누적 관람객) 를 사용한다.
# 인공신경망에 screen+attend+dir_score 감독 평가 점수 추가하여여 학습시킨다.
# 데이터는 사전에 분류한 Train을 이용하여, 은닉층은 3개를 사용하낟.

model_result <-compute(model,Test[,c("attend","screen","dir_score")])

#model_result 에는 사전에 학습된 model을 이용해 Test파일에 있는
# screen, attend 변수를 이용해 예측해봅니다.

predict_attend <-model_result$net.result

#Test의 attend 예측값을 predict_attend변수에 대입.

cor(predict_attend, Test$ac_attend)

#predict_attend 예측값 과 실제 값 사이의 상관계수 

plot(Test$ac_attend,predict_attend,xlab="실제 값",ylab="예측 값",)
#x축을 예측값, y축을 실제값을 넣어 그래프를 그린다.
abline(a=0,b=1,col="red")

#선형함수로 제시

# Attribute 값 추가

model <-neuralnet(ac_attend~screen+attend+ dir_score+act_point,
                  data=Train,hidden=c(5,3))

#사전에 분류한 Traindataset에서 모델링될 ac_attend(누적 관람객) 를 사용한다.
# 인공신경망에 screen+attend+dir_score 감독 평가 점수 추가하여여 학습시킨다.
# 데이터는 사전에 분류한 Train을 이용하여, 은닉층은 3개를 사용하낟.

model_result <-compute(model,Test[,c("attend","screen","dir_score","act_point")])

#model_result 에는 사전에 학습된 model을 이용해 Test파일에 있는
# screen, attend 변수를 이용해 예측해봅니다.

predict_attend <-model_result$net.result

#Test의 attend 예측값을 predict_attend변수에 대입.

cor(predict_attend, Test$ac_attend)

#predict_attend 예측값 과 실제 값 사이의 상관계수 

plot(Test$ac_attend,predict_attend,xlab="실제 값",ylab="예측 값",)
#x축을 예측값, y축을 실제값을 넣어 그래프를 그린다.
abline(a=0,b=1,col="red")

#선형함수로 제시

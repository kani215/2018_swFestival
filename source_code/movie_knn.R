# test, train 데이터 

DATA<-read.csv("dataset_0814.csv")
library("class")
set.seed(2018)
RandomIndex<-sample(x=1:nrow(DATA),size=round(0.7*nrow(DATA)),replace=F)
Train<-DATA[RandomIndex,]
Test<-DATA[-RandomIndex,]
Train<-Train[,c(7,8,9,17)]              #<< 사용할 변수만 불러온다.
Test<-Test[,c(7,8,9,17)]
str(Test)

model<-knn.reg(Train,Test,Train$ac_attend,35)
#모델 생성 knn.reg (훈련,테스트,y,k개수) /일반적으로 훈련 데이터 원소의 제곱근


#추가 Attribute 수집 과정 SNS 데이터를 활용하려 시도
for(i in 1:1781) {
  url<-paste(url_base1,search[i,1],url_base2)
  movie<-read_htm1(url)
  content<-htm1_nodes(movie,".search_tag")
  
  content1<-html_nodes(content,".td_col.result_td01")
  content1<-gsub("<.+?>|\t","",content1)
  content1<-gsub("\r","",content1)
  content1<-gsub("\n","",content1)
  title<-c(title,content1) #제목 추출
  
  content2<-html_nodes(content,".td_col.result_td02")
  content2<-gsub("<.+?>|\t","",content2)
  post<-c(post,content2) #누적 게시물 수 추추
  
  content3<-html_nodes(content,".td_col.result_td03")
  content3<-gsub("<.+?>|\t","",content3)
  content3<-gsub("\n","",content3)
  like<-c(like,content3) #평균 좋아요 누른 수 추출
  
  print(i)
  
}

#Season 변수 활용

first<-cbind(Test$screen,model$pred)
first<-as.data.table(first)
colnames(first)<-c("screen","ac_attend")
ggplot(data=first,aes(x=screen,y=ac_attend))+geom_line(size=1.5,color="navy")

second<-cbind(Test$screen,Test$ac_attend)
second<-as.data.table(first)
colnames(first)<-c("screen","ac_attend")
ggplot(data=first,aes(x=screen,y=ac_attend))+geom_line(size=1.5,color="navy")

RorM<-rep("Model",nrow(first))
str(RorM)
first<-cbind(first,RorM)
colnames(first)<-c("screen","ac_attend","RorM")
colnames(second)<-c("screen","ac_attend","RorM")
ans<-rbind(first,second)
ggplot(data=ans,aes(x=screen,y=ac_attend,color=RorM,group=RorM))+geom_line(size=1.5,stat_smooth(method=lm))

#모델값과 실제 영화 관객 수 값을 비교
RMSE<-sqrt(mean((Test$ac_attend-model$pred)^2))
RMSE
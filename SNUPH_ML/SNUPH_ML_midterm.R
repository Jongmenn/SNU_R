#라이브러리 
pacman::p_load(ggplot2,dplyr,class,gridExtra,readxl,logistf,MASS,boot,moments)

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#R code1: KNN (K-nearest neighbor)
x = runif(100,0,4)    #0과 4 사이 값 100개 난수 생성
e = rnorm(100,0,0.15) #평균0, 표준편차 0.15인 정규분포 난수 생성

y=x+e  ## y = sin(x)+e

#simple linear regression
fit<-lm(y~x)

newd   =data.frame(x)
knnfit = FNN::knn.reg(train = x, test = newd, y = y, k = 50)

#선형회귀모형 적합시 vs KNN으로 예측한 결과 비교하고할떄, 여기서는 k수가 많을때 예시임 
#KNN은 K수가 너무 많아지면 많은 이웃의 평균을 취하게 되어, 개별 데이터 포인트의 차이가 희석됨
#즉, 모델이 너무 일반화되어서 패턴을 잘 못찾아내어서 예측력이 떨어짐 (과소적합)
#K가 너무 적으면 작은 변동에 민감해서 과적합을 초래함 
#일반 Plot으로 그릴 때
x11();plot(x,y,cex.lab=1.4,cex.axis=1.4,cex=1.5,xlab="X",ylab="Y")
ORD = order(newd$x)
lines(newd$x[ORD],knnfit$pred[ORD],col="red")
lines(newd$x[ORD],fitted(fit)[ORD],col="blue")

#ggplot으로 그릴때
knn_ex<-data.frame(cbind(x,y,knnfit$pred))
x11();ggplot(knn_ex,aes(x,y))+geom_point()+
  geom_line(aes(x,V3),col="red",size=1)+
  stat_smooth(method=lm,se=F,size=1)


#추가적으로 k 숫자에 따라 비교할 때 
knnfit1 = FNN::knn.reg(train = x, test = newd, y = y, k = 1)
knnfit2 = FNN::knn.reg(train = x, test = newd, y = y, k = 3)
knnfit3 = FNN::knn.reg(train = x, test = newd, y = y, k = 5)
knnfit4 = FNN::knn.reg(train = x, test = newd, y = y, k = 10)
knnfit5 = FNN::knn.reg(train = x, test = newd, y = y, k = 20)
knnfit6 = FNN::knn.reg(train = x, test = newd, y = y, k = 30)
knnfit7 = FNN::knn.reg(train = x, test = newd, y = y, k = 50)
knnfit8 = FNN::knn.reg(train = x, test = newd, y = y, k = 80)

knn_ex2<-rbind(data.frame(x,y,pred=knnfit1$pred,k=1),
               data.frame(x,y,pred=knnfit2$pred,k=3),
               data.frame(x,y,pred=knnfit4$pred,k=10),
               data.frame(x,y,pred=knnfit5$pred,k=20),
               data.frame(x,y,pred=knnfit6$pred,k=30),
               data.frame(x,y,pred=knnfit7$pred,k=50),
               data.frame(x,y,pred=knnfit8$pred,k=80))

x11();ggplot(knn_ex2,aes(x,y))+geom_point()+
  geom_line(aes(x,pred),col="red")+facet_wrap(~k)+theme_gray(base_size=20)

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#R code2: SeparableCase

#Slide 26
#logistic regression : separable case

#example data
x<-seq(1:20) 
y<-c(rep(0,10),rep(1,10)) #일부로 데이터를 이렇게 생성함

#separable case: 두 범주가 하나의 선형함수로 분리 가능한 경우 예시
#한 클래스가 다른 클래스와 겹치지 않는 경우, 로지스틱 회귀는 완벽한 분류를 수행함
#이 때 결정 경계는 두 클래스 데이터를 완벽하게 나눠버림; 
#이러한 경우 파라미터 추정값이 신뢰할 수 없는 결과로 이어질 수 있음;
#즉, 오즈비가 추정은 되는데 값이 너무 크고, 불안정한 추정으로 인핸 신뢰구간이 매우 넓거나 극단적일 수 있음
fit<-glm(y~x,family="binomial")
summary(fit)

fitted(fit)
table(y,as.numeric(fitted(fit)>0.5)) #결과가 안벽히 분류하니 accuracy도 100으로 산출

#### lda
library(MASS)
#LDA 예시, 각 클래스의 사전 확률이 1/2, 1/2이라고 하자
res.lda <- lda(y ~ x , prior = c(1,1)/2)
res.lda <- lda(y ~ x , prior = c(0.5, 0.5)) #위랑 같은 애기

#마찬가지로 separable case에서 LDA 결과 비교
predict(res.lda)$class
table(y,predict(res.lda)$class) 

# Firth's correction
#완전 분리 문제를 해결하기위해 사용하는 방법
#Firth의 수정은 Jeffreys prior라는 베이지안 접근 방식을 사용하여 
#로지스틱 회귀의 우도함수(likelihood function)에 수정 항을 추가. 
#이 수정은 최대 우도 추정(MLE)의 성질을 유지하면서 파라미터 추정의 안정성을 높이는 효과
library(logistf)
fit<-logistf(y~x)
summary(fit)
table(y,as.numeric(fit$predict>0.5))

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#R code3: knn
pacman::p_load(ISLR)
data(Default) #ISLR 교재 예제 자료인듯

#Default: 신용카드 대금 연체여부 예측하는 분류문제 사용되는 예시 문제
#고객 특성을 바탕으로 신용카드 대금 연체(default)여부 예측이 주 목표
#student: 독립변수, 고객이 학생인지 아닌지 (No/Yes)
#balance: 독립변수, 고객이 월간 결제 후에 신용카드 잔액(평균), 값이 클수록 연체가능성이 높음
#         값이 높으면 카드 사용 후 결제하지 않은 금액이 많다는 것을 의미
#income : 독립변수, 고객의 연소득, 소득 수준이 높을수록 연체가능성이 낮아질 수 있음


#비복원 추출로 sample 1~10000사이 중 7000개 추출
set.seed(1234)
train.row <- sample(1:10000, 7000,replace=F)

head(Default)

#balance랑 income 변수만 샘플링해서 train, test 추출
#단위는 1000으로 조정해줌
trainD<-Default[train.row ,3:4]/1000 
testD <-Default[-train.row,3:4]/1000

trainY<-Default[train.row,1]
testY <-Default[-train.row,1]

library(class)
#KNN을 이용한 분류모델 적용
#k는 이웃의 수
#c는 클래스
#prob은 예측된 클래스의 확률 반환할지 말지
fit.tr<-knn(trainD,trainD,c=trainY,k=1) ##default: prob=FALSE
fit.te<-knn(trainD,testD ,c=trainY,k=1) ##default: prob=FALSE

## confusion matrix
#k가 1이면 과적합이 심함
table(fit.tr,trainY) #자기자신 데이터로 자기자신 예측
table(fit.te,testY)  #훈련데이터로 테스트 데이터 예측


## different k ?
fit.tr<-knn(trainD,trainD,c=trainY,k=5) ##default: prob=FALSE
fit.te<-knn(trainD,testD ,c=trainY,k=5) ##default: prob=FALSE

## confusion matrix
table(fit.tr,trainY) #accuracy=0.975
table(fit.te,testY)  #accuracy=0.965

## prob=TRUE, 확률로 반환시 예시
fit.te<-knn(trainD,testD,c=trainY,k=5,prob=TRUE)
attr(fit.te,"prob")

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#R code4: LDA & QDA

library(MASS)
head(iris)
set.seed(1234)
train <- sample(1:150, 75) #전체 데이터 중에서, 75개 샘플 추출
table(iris[,5][train])     #샘플링 이후 Y변수 테이블

### split into training and test sets
#이 예시는 Y의 클래스가 3개 이상임, 각 사전확률을 1/3로 고정
res.lda <- lda(Species ~ Sepal.Length+Sepal.Width+Petal.Length+Petal.Width,
               iris, prior = c(1,1,1)/3, subset = train)

table(iris[train,5] ,predict(res.lda, iris[train, ])$class)  #훈련 데이터에 적용 결과 비교
table(iris[-train,5],predict(res.lda, iris[-train, ])$class) #테스트 데이터에 적용 결과 비교

#### LOOCV
#전체 데이터 다 이용해서 LOOCV 적용후에 비교
res.lda.cv <- lda(Species ~ Sepal.Length+Sepal.Width+Petal.Length+Petal.Width , iris, prior = c(1,1,1)/3, CV=TRUE)
table(iris[,5],res.lda.cv$class)

##### QDA
#lda() in r이랑 코드 라인은 똑같음, 함수명만 달라짐
#이 예시는 Y의 클래스가 3개 이상임, 각 사전확률을 1/3로 고정
res.qda <- qda(Species ~ Sepal.Length+Sepal.Width+Petal.Length+Petal.Width, 
               iris, prior = c(1,1,1)/3, subset = train)
table(iris[train,5],predict(res.qda, iris[train, ])$class)
table(iris[-train,5],predict(res.qda, iris[-train, ])$class)

#### LOOCV
res.qda.cv <- qda(Species ~ Sepal.Length+Sepal.Width+Petal.Length+Petal.Width, 
                  iris, prior = c(1,1,1)/3, CV=TRUE)

table(iris[,5],res.qda.cv$class)

#공통 단점: 둘다 LOOCV를 고려시 계산 비용이 크고, 높은 불산과 불균형 데이터에 민감할 수 있음
#QDA는 LDA보다 복잡한 모델이므로, 샘플을 하나 제거할 때 예측 성능이 더 많이 변할 가능성이 큼
#또한 LDA는 클래스마다 공분산이 동일하다고 가정하는 것에 비해, QDA는 클래스마다 다른 공분산 행렬을 
#추정하기 때문에 샘플수가 적을 때 불안정성이 더 클 수 있음

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#R code5: CV warining

#시드 고정
set.seed(1234)

#행 50, 열 5000개인 랜덤 데이터 생성
x<-matrix(rnorm(5000*50),50,5000)

#이분범주 자료 50개 생성, cut-off=0.5, (0 또는 1)
y<-rbinom(50,size=1,p=0.5)

#pvalue 값 for문 반환해서 넣어주려고
pval<-rep(0,5000)
#pval=NULL 해도 됨

for (i in 1:5000){
  
  #5000개 변수 생성한 것들을 하나씩 넣어서 로지스틱회귀분석 수행
  #하고 결과 반환
  fit<-glm(y~x[,i],family="binomial")
  res<-summary(fit)
  
  #P-value만 선택해서 저장
  pval[i]<-res$coefficients[2,4]}

# p-value 가장 작은 값 indexing
# 예제 자료에서 5000개 변수 중에 연관성 높은 10개 변수만 가져와서 모델에 돌릴려고
order(pval)[1:10]
sel.index<-order(pval)[1:10]

#가상의 subdata 생성, 변수 10개 선택한 자료
fdat<-as.data.frame(cbind(y,x[,sel.index]))

#예측시 결과 저장할 list 만듬
pr.est<-rep(0,50)

#반복문으로 CV 수행
for (j in 1:50){
  
  #50개 자료 중 n-fold cross-validation 예 (leave-one-out에 해당함)
  fit.final<-glm(y~.,family="binomial",data=fdat[-j,])
  xnew<-fdat[j,]
  pr.est[j]<-predict(fit.final,xnew)
}

#table(예측 결과, 실제값)
#여기서 나온 pvalue는 각 LOO 방식으로 검증해서 나온 p-value
#데이터가 overlapping 될 수 있음, 즉 high-correlation됨
table( (pr.est>0.5), y) ## high cross-validated accuracy ???
(12+9)/(12+19+10+9)

############################
#여기는 비교차 새로운 데이터 생성
xx<-matrix(rnorm(10*50),50,10)
yy<-rbinom(50,size=1,p=0.5)

fdat2<-as.data.frame(cbind(yy,xx))

#new data를 넣어서 모델 결과 비교 (여기 결과는 test data에 모델 적용한 결과로 생각)
#결과반환하면 훈련 데이터에서 교차검증시 성능이 좋지 못함 
pr.est2<-predict(fit.final,fdat2)
table( (pr.est2>0.5), yy)
(15+9)/50

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#R code6: Bootstrap

# Drawing ECDF (경험적 누적 분포함수, emprical cumulative distribution function)
# 서로다른 표본들의 분포 비교시 많이 사용함, 각 집단의 백분위 추정 목적
n<-1000
x<-rnorm(n)
# 표준 정규분포를 따르는 1000개의 샘플에 대한 누적분포를 나타내는 함수
F <- ecdf(x)
summary(F)

x11();plot(F)
#ECDF랑 표준정규분포의 누적분포함수랑 비슷하게 그려지더라 
x11();plot(F, verticals= TRUE, do.points = FALSE)
curve(pnorm, lty=2,add=TRUE,col="red")           #정규분포의 누적분포함수의 값

######## Its confidence band (based on DKW theorem)
#DKW 정리(Dvoretzky-Kiefer-Wolfowitz theorem)는 
#통계에서 경험적 누적 분포 함수(ECDF)와 모집단의 누적 분포 함수(CDF) 사이의 차이에 대한 
#신뢰 구간을 제공하는 중요한 결과. 
#이 정리는 주어진 샘플 크기에서 ECDF가 모집단의 진짜 분포와 얼마나 가까운지를 확률적으로 보장해줌

#표준오차와 같은 역할
e<-sqrt(1/(2*n)*log(2/0.05))

L<-function(x){
  pmax(F(x)-e,0)
}

U<-function(x){
  pmin(F(x)+e,1)
}

x11();plot(F)
x11();plot(F, verticals= TRUE, do.points = FALSE)
curve(U, lty=2,add=TRUE,col="red") #상한 값
curve(L, lty=2,add=TRUE,col="red") #하한 값 

##################################################
### Example: calculating se for median
set.seed(1201)
data<-rnorm(100,5,3)
mean(data)
sd(data)

#평균이 5, 표준편차가 3인 정규분포 난수를 100개 생성
#생성된 자료에서 부스트랩 1000번 시행해서, 1000개의 부스트랩 데이터 생성, 반복 허용
#각 1000개 추출된 데이터의 중앙값 계산
B<-1000
b.samples<-lapply(1:B,function(i) sample(data,replace=T))
b.median <-sapply(b.samples,median)

#각 표본 데이터에서 추출된 자료의 중앙값 분포
x11();hist(b.median)

#중앙값에 대한 표준편차
sqrt(var(b.median))

## in theory,
## p*(1-p)/(n*f(5)^2)=1/(100*4*f(5)^2)=0.1414  (f(5)=dnorm(0,0,3))
## sqrt(0.1414)=0.376

##################################################

### Example: calculating se for skewness
library(moments)
set.seed(1201)
data<-rnorm(100,5,3)

B<-1000
b.samples <-lapply(1:B,function(i) sample(data,replace=T))
b.skewness<-sapply(b.samples,skew)


x11();hist(b.skewness) #기본적으로 3이 기준임
sqrt(var(b.skewness))

##################################################

library(boot)
data(bigcity) 
head(bigcity)
str(bigcity) #49, 2

## we want to know the mean ratio of the populations,
## i.e. pop 1930/pop 1920
# u: The 1920 population
# x: The 1930 population

row.bigcity<-dim(bigcity)[1]

boots.bigcity<-function(index){
  b.bigcity<-bigcity[index,] #indexing해서 찾아내기 (부스트랩시 샘플 인덱싱)
  b.ratio<-sum(b.bigcity$x)/sum(b.bigcity$u)
  return(b.ratio)
}

B<-1000
b.samples<-lapply(1:B,function(i) sample(c(1:row.bigcity),replace=T))
b.ratio  <-sapply(b.samples,boots.bigcity)

x11();hist(b.ratio)
median(b.ratio)
sqrt(var(b.ratio))


#### As an alternative, you may use "boot".
#### Before calling boot,
#### you need to define a function that will return the statistic
#### that you want to bootstrap.

library(boot)
ratio <- function(d, indices) sum(d$x[indices])/sum(d$u[indices])
RES.city<-boot(bigcity, ratio, R = 999)

#norm: 부트스트랩 표본의 분포가 정규분포가 따를것이라고 가정; 정규분포가 이닐경우 정확하지 않을 수 있음
#basic: 부트스트랩 추정치의 중심에서 원래 추정치를 대칭적으로 조정 (대칭 신뢰구간); 분포가 비대칭이면 별로
#per: percentile이용, 부트스트랩 표본에서 직접적으로 신뢰구간을 추정하는 방법; 비대칭 분포에 유리 (대칭성에 의존하지 않음)
#bca (Bias-Corrected and Accelerated): 부트스트랩 신뢰구간 중 가장 발전된 방법: 편향과 분포 비대칭성 보정함
#    계산이 복잡하고, 작은 샘플에서 부정확할 수 있음
boot.ci(RES.city,type=c("norm","basic","perc","bca"))


############################################################
#### Bootstrap confidence interval

### Example: calculating se for median
set.seed(1201)
data<-rnorm(100,5,3)

B<-1000
b.samples<-lapply(1:B,function(i) sample(data,replace=T))
b.median<-sapply(b.samples,median)


hist(b.median)

sqrt(var(b.median))

#아래는 직접 손으로 계산할 떄 예시임 
### normal interval
c(mean(b.median)-2*sqrt(var(b.median)),mean(b.median)+2*sqrt(var(b.median)))

### percentile interval
c(quantile(b.median,0.025),quantile(b.median,0.975))

### basic interval
c(2*median(data)-quantile(b.median,0.975),2*median(data)-quantile(b.median,0.025))


#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

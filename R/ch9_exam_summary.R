#' ---
#' title: "회귀기말대비(Ch9)"
#' author: "doellomdoel"
#' date: "2024-06-10"
#' output: html_document
#' ---
#' 
## ----setup, include=FALSE-----------------------------
# R code 출력, warning 출력X, 패키지 로드 메세지 출력X

knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE)

#' 
#' ## 0. 데이터 로드 powercell
## -----------------------------------------------------
tab9.1<-read.table("http://www.stat.ufl.edu/~rrandles/sta4210/Rclassnotes/data/textdatasets/KutnerData/Chapter%20%209%20Data%20Sets/CH09TA01.txt")
names(tab9.1)<-c("X1","X2","X3","X4","X5","X6","X7","X8","Y","logY")

head(tab9.1)
tail(tab9.1)

# 차원
dim(tab9.1)

# 요약
summary(tab9.1)

#' 
#' ## 1. 시각화 histogram, ggpairs
## -----------------------------------------------------
par(mfrow=c(1,2))
hist(tab9.1$Y)
hist(tab9.1$logY)

library(ggplot2); library(GGally)
ggpairs(tab9.1[,c('logY','X1','X2','X3','X4')])

#' 
#' ## 2. 로그 변환 다중 선형회귀 모델
#' ## 2. Log-Transformed Multiple Linear Regression Model
#' #### 2-1. logY~X1+X2+X3+X4
## -----------------------------------------------------
reg1<-lm(logY~X1+X2+X3+X4, data=tab9.1)

summary(reg1)
summary(reg1)$r.squared
summary(reg1)$adj.r.squared

#' 
#' #### 2-2. logY~X1+X2+X3
## -----------------------------------------------------
reg2<-lm(logY~X1+X2+X3, data=tab9.1)

summary(reg2)
summary(reg2)$r.squared
summary(reg2)$adj.r.squared


#' - 두 모델의 R-squared 및 Adjusted R-squared 값이 거의 유사
#' - 즉, X4가 포함된 reg1과 X4를 제외한 reg2 모델 간에는 설명력의 큰 차이가 없음
#' - X4는 모델의 성능 향상에 큰 기여를 하지 않는 변수
#' 
#' 
#' ## 3. 통계적 평가지표(statistical criterion)
#' #### 3-1. AIC(Akaike Information Criterion)
#' - 모델의 적합도와 모델 복잡성을 모두 고려
#' - 복잡도 패널티: $2k$
#' - 과적합 방지, 값이 작을수록 긍정적
#' $$
#' \text{AIC} = -2 \cdot \log(\text{likelihood}) + 2k
#' $$
#' - $k$: 모델의 자유도(추정 파라미터 수)
#' 
#' 
#' #### 3-2. BIC(Bayesian Information Criterion)
#' - AIC와 유사하나 모델 복잡성에 엄격(큰 표본일수록 단순 모델 선호)
#' - 복잡도 패널티: $k⋅log(n)$
#' - 포함관계가 없어도 모델 비교 가능, 값이 작을수록 긍정적
#' $$
#' \text{BIC} = -2 \cdot \log(\text{likelihood}) + k \cdot \log(n)
#' $$
#' 
#' - $n$: 표본 크기
#' - $k$: 모델의 자유도
#' 
#' #### 3-3. PRESS(Prediction Sum of Squares)
#' - 각 관측치를 제외하고 모델을 적합(fit)한 뒤, 제외한 관측치에 대한 예측오차를 제곱한 합계
#' - 회귀 모델의 교차검증 성격을 가진 지표로, 모델의 일반화 능력을 평가
#' - 포함관계가 없어도 모델 비교 가능,값이 작을수록 긍정적
#' $$
#' \text{PRESS} = \sum_{i=1}^{n} \left( y_i - \hat{y}_{i(-i)} \right)^2
#' $$
#' 
#' - $y_i$: 실제 관측값  
#' - $\hat{y}_{i(-i)}$: $i$번째 관측치를 제외하고 적합한 모델로 예측한 값  
#' - $n$: 관측치 수
#' 
#' #### 3-4. 요약
#' - AIC, BIC: 기존 데이터에 대한 적합도 + 복잡도
#' - PRESS: 새로운 데이터에서의 오차 추정(정확도)
## -----------------------------------------------------
#AIC
extractAIC(reg1)
extractAIC(reg2)

#BIC
extractAIC(reg1, k = log(dim(tab9.1)[1]))
extractAIC(reg2, k = log(dim(tab9.1)[1]))

#PRESS
sum((reg1$residuals/(1-hatvalues(reg1)))^2)
sum((reg2$residuals/(1-hatvalues(reg2)))^2)

#' - reg2일때 성능이 더 좋게 나옴
#' 
#' 
#' ## 4. CP 통계량을 사용하여 최적의 변수 조합 찾기
#' 
## -----------------------------------------------------

library(leaps)

X<-cbind(1, tab9.1$X1, tab9.1$X2, tab9.1$X3, tab9.1$X4)
logY<-tab9.1$logY

outs <- leaps(X, logY, int = FALSE, method="Cp")
head(outs$which)
head(outs$Cp)

plot(outs$size, outs$Cp, xlab = "p", ylab = expression(C[p]))

# 독립 변수의 수(p)에 따른 조정된 결정 계수의 변화
outs <- leaps(X, logY, int = FALSE, method="adjr2")
plot(outs$size, outs$adjr2,  xlab = "p", ylab = "adjr2")

# 독립 변수의 수(p)에 따른 단순한 결정 계수의 변화
outs <- leaps(X, logY, int = FALSE, method="r2")
plot(outs$size, outs$r2,    xlab = "p", ylab = "r2")


# 가장 작은 CP 값을 찾습니다.
min_cp <- min(outs$Cp)

# 가장 작은 CP 값이 속한 인덱스를 찾습니다.
best_index <- which(outs$Cp == min_cp)

# 이 인덱스를 사용하여 변수 선택의 수를 결정합니다.
best_num_variables <- outs$size[best_index]


#' 
#' ## 5. 부분 데이터에 대한 회귀분석
#' #### 5-1. 데이터 분할
## -----------------------------------------------------

X<-tab9.1[,1:8]
head(X)

subset<-regsubsets(X, logY,nbest=1)

#' #### 5-2. 선택된 변수 조합
## -----------------------------------------------------
summary(subset)$which

#' - TRUE인 부분이 선택된 부분으로 제안된 조합은 8가지
#' - 1. Y ~ X3
#' - 2. Y ~ X2 + X3
#' - 3. Y ~ X2 + X3 + X8
#' - 4. Y ~ X1 + X2 + X3 + X8
#' - 5. Y ~ X1 + X2 + X3 + X6 + X8
#' - 6. Y ~ X1 + X2 + X3 + X5 + X6 + X8
#' - 7. Y ~ X1 + X2 + X3 + X5 + X6 + X7 + X8
#' - 8. Y ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8
#' 
#' #### 5-3. 결정계수 R²
## -----------------------------------------------------
summary(subset)$rsq

#' - 변수 개수가 늘어날수록 R²는 점진적으로 증가
#' - 최대값은 0.8461로 약 85%의 설명력을 가짐
#' 
#' #### 5-4. 잔차제곱합 RSS
## -----------------------------------------------------
summary(subset)$rss

#' - 변수 개수가 늘어날수록 RSS는 점진적으로 감소 (적합도가 개선됨)
#' - 그러나 감소 폭은 점점 줄어듦 → 추가 변수가 주는 개선 효과는 점점 작아짐
#' 
#' #### 5-5. Mallows' Cp 과적합 평가
## -----------------------------------------------------
summary(subset)$cp

#' - 좋은 모델의 특징: Cp 값 ≈ 변수 개수 + 1 (=p)
#' - 변수 1~3개의 경우 기준값과 차이가 매우 큼(설명력이 부족, 과소적합)
#' - 4변수 이상부터는 Cp가 기준값(5~9)에 근접
#' - cf. Cp ≫ p : 과소적합, Cp ≪ p : 과적합 
#' #### 5-6. BIC 적합도 평가
## -----------------------------------------------------
summary(subset)$bic

#' - 변수를 4개까지 추가했을 때 성능이 개선되고, 변수 4개인 모델에서 최소값 -75.70을 가진 이후에는 변수를 추가할수록 오히려 BIC가 증가하여 과적합 가능성이 높아짐
#' 
#' #### 5-7. 후진 제거법으로 최적 변수 조합 선택
## -----------------------------------------------------
library(MASS)
step <- step(reg1, direction="backward") 

#' - 4변수 모형과 5변수 모형이 가장 적절
#' 
#' 
#' ## 6. model test
#' #### 6-1. 테스트 데이터 로드
## -----------------------------------------------------
test<-read.table("http://www.stat.ufl.edu/~rrandles/sta4210/Rclassnotes/data/textdatasets/KutnerData/Chapter%20%209%20Data%20Sets/CH09TA05.txt")
names(test)<-c("X1","X2","X3","X4","X5","X6","X7","X8","Y","logY")
head(test)
dim(test)

#' #### 6-2. 훈련 데이터의 잔차제곱평균 계산(MSE on Training Data) 
## -----------------------------------------------------
reg1<-lm(logY~X1+X2+X3+X8, data=tab9.1)
reg2<-lm(logY~X1+X2+X3+X6+X8, data=tab9.1)
reg3<-lm(logY~X1+X2+X3+X5+X6+X8,data=tab9.1)


MSE1<- sum((tab9.1$logY-predict(reg1, newdata=tab9.1))^2)/(54-5)
MSE2<- sum((tab9.1$logY-predict(reg2, newdata=tab9.1))^2)/(54-6)
MSE3<- sum((tab9.1$logY-predict(reg3, newdata=tab9.1))^2)/(54-7)

MSE1;MSE2;MSE3

#' - 훈련 데이터 MSE: 모델 적합도 평가
#' - 모델이 복잡해질수록 MSE가 약간씩 감소
#' 
#' #### 6-3. 테스트 데이터의 잔차제곱평균 계산(MSE on Test Data)
## -----------------------------------------------------
reg1.new<-lm(logY~X1+X2+X3+X8, data=test)
reg2.new<-lm(logY~X1+X2+X3+X6+X8, data=test)
reg3.new<-lm(logY~X1+X2+X3+X5+X6+X8, data=test)


MSPR1<- sum((test$logY-predict(reg1, newdata=test))^2)/54
MSPR2<- sum((test$logY-predict(reg2, newdata=test))^2)/54
MSPR3<- sum((test$logY-predict(reg3, newdata=test))^2)/54

MSPR1; MSPR2; MSPR3

#' - 테스트 데이터 MSE: 모델 예측력 평가
#' - 가장 단순한 모델과 두번째 모델은 성능이 비슷하고, 가장 복잡한 모델의 MSE가 가장 높음
#' - reg3은 적합도가 가장 크고, 예측력이 가장 낮으므로 과적합 가능성이 있음
#' 
#' ## The End

#' ---
#' title: "ch8_exam_summary"
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
powercell<-read.table("https://users.stat.ufl.edu/~rrandles/sta4210/Rclassnotes/data/textdatasets/KutnerData/Chapter%20%208%20Data%20Sets/CH08TA01.txt")

names(powercell)<-c("Y","X1","X2")

# 정규화된 데이터를 새로운 열에 저장
powercell$x1<-(powercell$X1-1)/0.4
powercell$x2<-(powercell$X2-20)/10

powercell

# 차원
dim(powercell)

# 요약
summary(powercell)

#' 
#' ## 1. 시각화 pairs, pairs2
## -----------------------------------------------------
# 시각화
pairs(powercell)


library(ggplot2)
library(GGally)
ggpairs(powercell)

#' 
#' ## 2. 회귀 모델 비교
#' #### 2-1. 다항 회귀 모형(Polynomial Regression Model)
## -----------------------------------------------------
lm_poly<-lm(Y~x1+x2+I(x1^2)+I(x2^2)+x1*x2, data=powercell)
summary(lm_poly)

## -----------------------------------------------------
library(ggplot2)
library(ggfortify)
autoplot(lm_poly)

#'   - 유의미한 계수: x1, x2
#'   - 유의하지 않은 계수: I(x1^2), I(x2^2), x1:x2
#'   - 모델 전체는 통계적으로 유의(F = 10.57, p = 0.01086)하지만, 추가된 3개 항은 개별적으로 통계적 유의성이 없음
#'   
#' #### 2-2. 선형회귀 vs 다항회귀(Linear Regression vs Polynomial Regression)
## -----------------------------------------------------
lm_base<-lm(Y~x1+x2, data=powercell)

anova(lm_base,lm_poly)

#' - 회귀 계수 중에서 x1과 x2만 유의미
#' - 모델 2에서 모델 1로의 추가된 설명 변수 (다항식 항목)에 대한 F-통계량의 p-value는 0.5527으로 유의 수준인 0.05보다 크므로, 다항식 모델은 통계적으로 유의미하지 않습니다.
#' 
#' 
#' ## 3. 회귀 계수의 신뢰구간 계산
#' 
#' #### 3-1. 데이터 로드 insurance
## -----------------------------------------------------

insurance<-read.table("https://users.stat.ufl.edu/~rrandles/sta4210/Rclassnotes/data/textdatasets/KutnerData/Chapter%20%208%20Data%20Sets/CH08TA02.txt")
names(insurance)<-c("Y","X1","X2")
insurance$X2<-as.factor(insurance$X2)
head(insurance)


#' 
#' #### 3-1. Y~X1+X2
## -----------------------------------------------------

fit<-lm(Y~X1+X2, data=insurance)
summary(fit)

# CI
summary(fit)$coef
summary(fit)$coef[2,1] - qt(0.975, 17)*summary(fit)$coef[2,2] 
summary(fit)$coef[2,1] + qt(0.975, 17)*summary(fit)$coef[2,2] 

confint(fit)


#' - qt(0.975, 17)는 자유도가 17인 t-분포에서 상위 2.5%에 해당하는 t 값
#' 
#' #### 3-2. Y~X1*X2
## -----------------------------------------------------
fit2<-lm(Y~X1*X2, data=insurance)
summary(fit2)


#' - 모형의 설명력 (Adjusted R-squared)은 0.8754로, 모형이 데이터의 변동을 약 87.54% 정도 설명
#' F-statistic의 p-value는 4.675e-08으로, 모형이 통계적으로 유의미한지 여부를 검정
#' 
#' 
#' ## 4. 선형 회귀 모델 비교
#' 
#' #### 4-1. 데이터 로드 soap
## -----------------------------------------------------
soap<-read.table("https://users.stat.ufl.edu/~rrandles/sta4210/Rclassnotes/data/textdatasets/KutnerData/Chapter%20%208%20Data%20Sets/CH08TA05.txt")
names(soap)<-c("Y","X1","X2")
soap$X2<-as.factor(soap$X2)
head(soap)

#' 
#' #### 4-2. X1과 Y 사이의 관계를 X2의 수준에 따라 시각화
## -----------------------------------------------------
g<- ggplot(soap, aes(x=X1, y=Y, colour = X2)) + geom_point()
g+geom_smooth(method=lm, se=FALSE)

#' 
#' #### 4-3. 상호작용 모형(Interaction Model)
## -----------------------------------------------------
# Y~X1*X2

model_43<-lm(Y~X1*X2, data=soap)
summary(model_43)

#' 
#' #### 4-4. 단일 변수 모형 vs 상호작용 모형(Single-Variable Model vs. Interaction Model)
## -----------------------------------------------------
# Y~X1

model_44<-lm(Y~X1, data=soap)
anova(model_44, model_43)

#' - 모델 44과 모델43 간의 차이가 통계적으로 유의미함 (F-statistic은 22.646이고, p-value는 3.669e-06)
#' 
#' #### 4-5. 다변량 모형 vs 상호작용 모형(Multivariable Model vs. Interaction Model)
## -----------------------------------------------------
# Y~X1+X2

model_45<-lm(Y~X1+X2, data=soap)
anova(model_45, model_43)

#' - 모델 45와 모델 43 간의 차이가 통계적으로 유의미하지 않음 (F-statistic은 1.8802이고, p-value는 0.183)
#' 
#' 
#' ## The End

#' ---
#' title: "ch6_exam_summary"
#' author: "doellomdoel"
#' date: "2024-06-10"
#' output: html_document
#' ---
#' 
## ----setup, include=FALSE----------------------------
# R code 출력, warning 출력X, 패키지 로드 메세지 출력X

knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE)

#' 
#' ## 0. 데이터 로드 dwaine
## ----------------------------------------------------
# 데이터 읽어오기
# url이거나 txt파일인 경우 -> read.table
# csv 파일인 경우 -> read.csv
# 엑셀 파일인 경우 -> read.excel

dwaine<-read.table(
  "http://www.stat.ufl.edu/~rrandles/sta4210/Rclassnotes/data/textdatasets/KutnerData/Chapter%20%206%20Data%20Sets/CH06FI05.txt")


# 컬럼 지정
names(dwaine)<-c("X1","X2","Y")

# 데이터 확인
# 일부
head(dwaine)

# 차원
dim(dwaine)

# 요약
summary(dwaine)

#' 
#' ## 1. 시각화 pairs, pairs2, 3d
## ----------------------------------------------------
pairs(dwaine)

## ----------------------------------------------------
library(ggplot2)
library(GGally)
ggpairs(dwaine)

## ----------------------------------------------------
library(dplyr)
library(plotly)
p <- plot_ly(dwaine, x = ~X1, y = ~X2, z = ~Y,
             colors = c('#BF382A')) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'X1'),
                      yaxis = list(title = 'X2'),
                      zaxis = list(title = 'Y')))
p

#' 
#' ## 2. 선형 회귀 모델 추정
#' #### 2-1. 회귀 계수(bhat) 계산
## ----------------------------------------------------
Y<-dwaine$Y
X<-cbind(1,dwaine$X1, dwaine$X2)
bhat<-solve(t(X)%*%X)%*%t(X)%*%Y
bhat

#' 
#' #### 2-2. lm 함수로 회귀 모델 추정
## ----------------------------------------------------
lm.dwaine<- lm(Y~X1+X2,data=dwaine)
summary(lm.dwaine)

#' 
#' #### 2-3. 예측값(yhat)계산
## ----------------------------------------------------
Yhat<-fitted(lm.dwaine)
Yhat.1<-X%*% bhat

H<-X%*%solve(t(X)%*%X)%*%t(X)
Yhat.2<-H%*%Y

head(cbind(Yhat, Yhat.1, Yhat.2))

plot(Yhat~Y)

plot(Yhat ~ Y,
     xlab = "Observed Y", 
     ylab = "Predicted Y", 
     pch = 19, 
     xlim = c(130, 250),
     ylim = c(130, 250),
     col = "gray30",
     cex = 1.5)

abline(0, 1, col = "steelblue", lwd = 1)
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dashed")

#' 
#' #### 2-4. 잔차(예측값(yhat)과 실제값(y) 차이) 계산
## ----------------------------------------------------
Y-Yhat
residuals(lm.dwaine)

#' 
#' #### 2-5. 잔차(residuals) 시각화
## ----------------------------------------------------
library(ggfortify)
autoplot(lm.dwaine)

#' 
#' #### 2-6. 분산분석(ANOVA)
## ----------------------------------------------------
anova(lm.dwaine)

#' 
#' #### 2-7. 회귀 모델 요약
## ----------------------------------------------------
summary(lm.dwaine)

#' 
#' 
#' ## 3. 예측 구간과 신뢰 구간 계산 
#' #### 3-1. Confidence Interval for Mean Response
#' 
#' Define the vector $X_h$:
#' 
#' $$
#' X_h =
#' \begin{bmatrix}
#' 1 \\
#' X_{h,1} \\
#' \vdots \\
#' X_{h,p-1}
#' \end{bmatrix}, \quad
#' E\{Y_h\} = X_h^T \beta, \quad
#' \hat{Y}_h = X_h^T b
#' $$
#' 
#' Variance of $\hat{Y}_h$:
#' 
#' $$
#' \sigma^2\{\hat{Y}_h\} = \sigma^2 X_h^T (X^T X)^{-1} X_h
#' $$
#' 
#' Estimated variance:
#' 
#' $$
#' s^2\{\hat{Y}_h\} = MSE \, (X_h^T (X^T X)^{-1} X_h) = X_h^T s^2\{b\} X_h
#' $$
#' 
#' The $(1-\alpha)$ confidence limits for $E\{Y_h\}$ are:
#' 
#' $$
#' \hat{Y}_h \; \pm \; t(1-\alpha/2; n-p) \; s\{\hat{Y}_h\}
#' $$
#' 
## ----------------------------------------------------
head(predict(lm.dwaine, newdata=dwaine, interval = "confidence"))

predict.lm(lm.dwaine, newdata=data.frame(X1=65.4, X2=17.6), interval="confidence", level=0.95)

#' 
#' #### 3-2. Prediction Interval for a New Observation
#' 
#' 
#' The $(1 - \alpha)$ prediction limits for a new observation $Y_{h}^{(new)}$
#' corresponding to $X_h$, the specified values of the $X$ variables, are:
#' 
#' $$
#' \hat{Y}_h \; \pm \; t(1-\alpha/2; \, n-p) \, s\{pred\}
#' $$
#' 
#' where:
#' 
#' $$
#' s^2\{pred\} = MSE + s^2\{\hat{Y}_h\} 
#' = MSE \Big( 1 + X_h^T (X^T X)^{-1} X_h \Big)
#' $$
#' 
## ----------------------------------------------------
head(predict(lm.dwaine, newdata=dwaine, interval = "prediction"))

predict.lm(lm.dwaine, newdata=data.frame(X1=65.4, X2=17.6), interval="prediction", level=0.95)

#' 
#' 
#' ## The End
#' 
## ----export, include=FALSE---------------------------
knitr::purl("ch6_exam_summary.Rmd", output = "ch6_exam_summary.R", documentation = 2)


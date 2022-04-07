# 앙상블 기법
# 1. 랜덤포레스트
install.packages("randomForest")
install.packages("caret")
library(caret)
library(randomForest)
# 데이터 생성 
data(iris)
iris
# 학습데이터, 검정데이터 구분
idx <- sample(2, nrow(iris), replace = T, prob = c(0.7, 0.3))
trdata <- iris[idx == 1, ]
nrow(trdata)
tedata <- iris[idx == 2, ]
nrow(tedata)

# 1-3 랜덤포레스트 실행(100개의 tree를 다양한 방법(proximity = T)으로 생성)
install.packages('randomForest')
library(randomForest)

RFmodel <- randomForest(Species ~ ., data = trdata, ntree = 100, proximity = T)
RFmodel

# 1-4 시각화
windows()
plot(RFmodel, main = 'RandomForest Model of iris')

# 1-5 모델에 사용된 변수 중 중요한 것 확인
importance(RFmodel)

# 1-6 중요한 것 시각화
windows()
varImpPlot(RFmodel)

# 1-7 실제값과 예측값 비교
confusionMatrix(trdata$Species,predict(RFmodel))
table(trdata$Species, predict(RFmodel))

# 1-8 테스트데이터로 예측
pred <- predict(RFmodel, newdata = tedata)
table(tedata$Species, pred)

# 1-9 시각화
windows()
plot(margin(RFmodel, tedata$Species))

# 2. 랜덤포레스트 모델 생성
library(randomForest)
data(iris)

model <- randomForest(Species ~ ., data = iris)
model

# 3. 파리미터 조정 - 트리 갯수 300개, 변수 갯수 4개 지정
model2 <- randomForest(Species ~ ., data = iris, ntree = 300, mtry = 4, na.action = na.omit)
model2

# 3-1 중요변수 생성하여 랜덤 포레스트 모델 생성
model3 <- randomForest(Species ~ ., data = iris, importance = T, na.action = na.omit)
model3

# 3-1-1 중요 변수 보기
importance(model3)

# 3-2 중요변수 시각화
windows()
varImpPlot(model3)

# 4. stagec data set 이용 randomforest기법 적용해보기
library(rpart)
data('stagec')
str(stagec)

# 4-1 결측치 제거
stagec1 <- subset(stagec, !is.na(g2))
stagec2 <- subset(stagec1, !is.na(gleason))
stagec3 <- subset(stagec2, !is.na(eet))
str(stagec3)

set.seed(1234)
ind <- sample(2, nrow(stagec3), replace = TRUE, prob = c(0.7, 0.3))
ind

traindata <- stagec3[ind == 1, ] # n = 102개
testdata <- stagec3[ind == 2, ] # n = 32개개

library(randomForest)
rf <- randomForest(ploidy ~ ., data = traindata, ntree = 100, proximity = TRUE)

table(predict(rf), traindata$ploidy)

# 트리수에 따른 종속변수의 범주별 오분류율
print(rf)

# 변수의 중요도 측정
importance(rf)
#Mean decrease Gini
#각 tree에서 해당 feature를 기준으로 분류(split)하는 지점에서의
#decrease of impurity(불순도)의 총 합을 계산한 후, 
#이모든 tree의 값들의 평균을 낸 것이 Mean decrease Gini이다. 
#그 feature가 모델이 분류를 잘하는데
#중요하게 작용할 수록 이 값은 커질 것이다.

windows()
varImpPlot(rf)

# 테스트 자료에 대한 예측
rf.pred <- predict(rf, newdata = testdata)
table(rf.pred, testdata$ploidy)


# 더 알아보기 (엔트로피 : 불확실성 척도)
x1 <- 0.5; x2 <- 0.5
e1 <- -x1*log2(x1) - x2*log2(x2)
e1

x1 <- 0.7; x2 <- 0.3
e2 <- -x1*log2(x1) - x2*log2(x2)
e2

# 실습 (최적의 파라미터(ntree, mtry)찾기)
# 속성값 생성
ntree <- c(400, 500, 600)
mtry <- c(2:4)
param <- data.frame(n = ntree, m = mtry)
param

# 이중 for()이용 모델 생성
for(i in param$n){
  cat('ntree = ', i, '\n')
  for(j in param$m){
    cat('mtry = ', j, '\n')
    model_iris <- randomForest(Species ~ ., data = iris, ntree = i, mtry = j, na.action = na.omit)
    print(model_iris)
  }
}

# stagec 데이터도 이용해보기!!

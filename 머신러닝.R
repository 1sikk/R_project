install.packages('doBy')
library(doBy)
iris_train <- sampleBy(~Species, frac = 0.7, data = iris)
dim(iris_train)
table(iris_train$Species)
iris_train

# 베이지안 p(A|B) = p(a 교집합 b) / p(b)
install.packages('e1071')
install.packages('caret')

library(e1071)
setwd('E:/BigDate/R programing/dataset4/dataset4')
data <- read.csv(file = "heart.csv", header = T)
data

head(data)
str(data)

library(caret)
set.seed(1234)
tr_data <- createDataPartition(y = data$AHD, p = 0.7, list = FALSE)
tr <- data[tr_data,]
te <- data[-tr_data,]

Bayes <- naiveBayes(AHD ~ ., data = tr)
Bayes

predicted <- predict(Bayes, te, type = 'class')
table(predicted, te$AHD)

AHD <- as.factor(te$AHD)
confusionMatrix(predicted, AHD)

# 의사결정나무(Decision Tree)
# 1. 패키지 설치
install.packages('party')
library(party)

# 1-1 airquality 데이터셋 로딩
library(datasets)
str(airquality)

# 1-2 formula 생성
formula <- Temp ~ Solar.R + Wind + Ozone

# 1-3 분류모델 생성 - formula를 이용 분류모델 생성
air_ctree <- ctree(formula, data = airquality)
air_ctree
#  7) Ozone <= 65, criterion=0.971, statistic = 6.691
# (1)     (2)          (3)            (4)
# 첫번째: 반응변수(종속변수)에 대해서 설명변수(독립변수)가 영향을 미치는 중요 변수의 척도. 수치가
# 작을수록 영향을 미치는 정도가 높고, 순서는 분기되는 순서를 의미
# 두번째: 의사결정 트리의 노드명
# 세번째: 노드이 분기기준(criterion)이 되는 수치.
# 네번째: 반응변수(종속변수)의 통계량(statistic). 
# *마지막 노드이거나 또 다른 분기 기준이 있는 경우에는 세번째와 네 번째 수치는 표시되지 않는다.

# 1-4 분류분석 결과
windows()
plot(air_ctree)

# 2. 학습데이터와 검정데이터 샘플링으로 분류분석
# 2-1 학습데이터 검정데이터 샘플링
# set.seed(1234)
idx <- sample(1:nrow(iris), nrow(iris)*0.7)
train <- iris[idx,]
test <- iris[-idx,]

# 2-2 formula 생성
formula <- Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width

# 2-3 학습데이터이용 분류모델 생성
iris_ctree <- ctree(formula, data = iris)
iris_ctree

# 2-4 분류모델 플로팅
# 2-4-1 시각화
windows()
plot(iris_ctree, type = 'simple')

# 2-4-2 의사결정 트리로 결과 플로팅
windows()
plot(iris_ctree)

# 2-5 분류모델 평가
# 2-5-1 모델의 예측치 생성과 혼돈 매트릭스 생성
pred <- predict(iris_ctree, test)
table(pred, test$Species)

# 2-5-2 분류 정확도
(14 + 16 + 13) / nrow(test)
# setosa(14) + virginica(16) + versicolor(14-1) / nrow(test)

# 3. K겹 교차 검정 샘플링으로 분류분석
# 3-1 k겹 교차 검정을 위한 샘플링
library(cvTools)
cross <- cvFolds(nrow(iris), K = 3, R = 2)

# 3-2 k겹 교차 검정 데이터 보기
str(cross)
cross
length(cross$which)
dim(cross$subsets)
table(cross$which)

# 3-3 k겹 교차 검정 수행
R = 1:2
K = 1:3
CNT = 0
ACC <- numeric()
for(r in R){
  cat('\n R = ', r, '\n')
  for(k in K){
    
    datas_ids <- cross$subsets[cross$which == k,r]
    test <- iris[datas_ids, ]
    cat('test : ', nrow(test), '\n')
    
    formula <- Species ~ .
    train <- iris[datas_ids, ]
    cat('train : ', nrow(train), '\n')
    
    model <- ctree(Species ~ ., data = train)
    pred <- predict(model, test)
    t <- table(pred, test$Species)
    print(t)
    
    CNT <- CNT + 1
    ACC[CNT] <- (t[1, 1] + t[2, 2] + t[3, 3]) / sum(t)
  }
}

CNT

# 3-4 교차 검정 모델 평가
ACC
length(ACC)

result_acc <- mean(ACC, na.rm = T)
result_acc

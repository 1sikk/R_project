# 인공신경만 분류 분석
# nnet패키지에서 제공하는 nnet()함수
# 형식: nnet(formula, data, weights, size)
# formula: y ~ x 형식으로 반응변수와 설명변수 식
# data: 모델 생성에 사용될 데이터 셋
# weights: 각 case에 적용할 가중치(기본값: 1)
# size: 은닉층(hidden layer)의 수 지정

# 1. 인공신경망 모델 생성
# 1-1 패키지 설치
install.packages('nnet')
install.packages("devtools")
install.packages("NeuralNetTools")
library(NeuralNetTools)
library(devtools)
library(nnet)

# 1-2 데이터 셋 생성
# 데이터 프레임 생성 - 입력변수(x)와 출력변수(y)
df = data.frame(
  x2 = c(1:6),
  x1 = c(6:1),
  y = factor(c('no', 'no', 'no', 'yes', 'yes', 'yes'))
)
str(df)

# 1-3 인공신경망 모델 생성
model_net = nnet(y ~ ., df, size = 1)

# 1-4 모델 결과 변수 보기
model_net

# 1-5 가중치 보기
summary(model_net)

# 1-6 분류모델의 적합값 보기
model_net$fitted.values

# 1-7 분류모델의 예측치 생성과 분류 정확도
p <- predict(model_net, df, type = 'class')
table(p, df$y)

# 2. iris데이터 셋을 이용한 인공신경망 모델 생성
# 2-1 데이터 생성
data(iris)
idx = sample(1:nrow(iris), 0.7*nrow(iris))
training = iris[idx,]
testing = iris[-idx,]
nrow(training); nrow(testing)

# 2-2 인공신경망 모델(은닉층 1개와 3개) 생성
model_net_iris1 = nnet(Species ~ ., training, size = 1)
model_net_iris1
model_net_iris3 = nnet(Species ~ ., training, size = 3)
model_net_iris3
plotnet(model_net_iris1)
plotnet(model_net_iris3)

# 입력 변수의 값들이 일정하지 않거나 값이 큰 경우에는 신경망 모델이 정상적으로 만들어지지
# 않기 때문에 입력 변수를 대상으로 정규화 과정이 필요하다.

# 2-3 가중치 네트워크 보기 - 은닉층 1개 신경망 모델
summary(model_net_iris1)

# 2-4 가중치 네트워크 보기 - 은닉층 3개 신경망 모델
summary(model_net_iris3)

# 2-5 분류모델 평가
pred1 <- predict(model_net_iris1, testing, type = 'class')
pred3 <- predict(model_net_iris3, testing, type = 'class')
table(predict(model_net_iris1, testing, type = 'class'), testing$Species)
table(predict(model_net_iris3, testing, type = 'class'), testing$Species)
# classificationMetrics() 함수: 분류 metrics 생성
install.packages('performanceEstimation')
library(performanceEstimation)
classificationMetrics(testing$Species, pred3)

# 3. neuralnet 패키지를 이용한 인공신경망 모델 생성
# neuralnet()함수
# 형식: neuralnet(formula, data, hidden = 1, threshold = 0.01, stepmax = 1e+05, rep = 1, startweights = NULL, learningrate.limit = NULL, algorithm = "rprop+")
# formula: y ~ x형식으로 반응변수와 설명변수 식
# data: 모델 생성에 사용될 데이터 셋
# hidden = 1: 은닉층(hidden layer)의 수 지정
# threshold = 0.01: 경계값 지정
# stepmax = 1e+05: 인공신경망 학습을 위한 최대 스텝 지정
# rep = 1: 인공신경망의 학습을 위한 반복 수 지정
# startweights = NULL: 랜덤으로 초기화된 가중치를 직접 지정
# learningrate.limit = NULL: backpropagation 알고리즘에서 사용될 학습비율을 지정
# algorithm = "rprop+": backpropagation과 같은 알고리즘 적용을 위한 속성

# 3-1 패키지 설치
install.packages('neuralnet')
library(neuralnet)

# 3-2 데이터 셋 생성
data(iris)
idx = sample(1:nrow(iris), 0.7*nrow(iris))
training_iris = iris[idx, ]
testing_iris = iris[-idx, ]
dim(training_iris); dim(testing_iris)

# 3-3 수치형으로 컬럼 생성
training_iris$Species2[training_iris$Species == 'setosa'] <- 1
training_iris$Species2[training_iris$Species == 'versicolor'] <- 2
training_iris$Species2[training_iris$Species == 'virginica'] <- 3
training_iris$Species <- NULL
head(training_iris)

testing_iris$Species2[testing_iris$Species == 'setosa'] <- 1
testing_iris$Species2[testing_iris$Species == 'versicolor'] <- 2
testing_iris$Species2[testing_iris$Species == 'virginica'] <- 3
testing_iris$Species <- NULL
head(testing_iris)

# 3-4 데이터 정규화
# 3-4-1 정규화 함수 정의
normal <- function(x){
  return((x - min(x)) / (max(x) - min(x)))
}

# 3-4-2 정규화 함수를 이용하여 학습데이터/검정데이터 정규화
training_nor <- as.data.frame(lapply(training_iris, normal))
summary(training_nor)
testing_nor <- as.data.frame(lapply(testing_iris, normal))
summary(testing_nor)

# 3-5 인공신경망 모델 생성 - 은닉노드 1개
model_net_iris = neuralnet(Species2 ~ ., data = training_nor, hidden = 1)
model_net_iris
model_net_iris = neuralnet(Species2 ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
                           data = training_nor, hidden = 1)
model_net_iris
# 출력은 되나 값이 다르게 출력이 됨.
windows()
plot(model_net_iris)

# 3-6 분류모델 성능 평가
# 3-6-1 모델의 예측치 생성 - compute 함수이용
model_result <- compute(model_net_iris, testing_nor[c(1:4)])
model_result$net.result

# 3-6-2 상관관계 분석 - 상관계수로 두 변수 간 선형관계의 강도 측정
cor(model_result$net.result, testing_nor$Species2)

# 3-7 분류모델 성능 향상 - 은닉층 2개지정, backprop속성 적용
# 3-7-1 인공신경망 모델 생성
model_net2 = neuralnet(Species2 ~ Sepal.Length + Sepal.Width +
                         Petal.Length + Petal.Width, data = training_nor,
                       hidden = 2, algorithm = 'backprop', learningrate = 0.01)
model_net2
model_net3 = neuralnet(Species2 ~ ., data = training_nor,
                       hidden = 2, algorithm = 'backprop', learningrate = 0.01)
model_net3

# 3-7-2 분류모델 예측치 생성과 평가
model_result1 <- compute(model_net_iris, testing_nor[c(1:4)])
model_result1$neurons
cor(model_result1$net.result, testing_nor$Species2)


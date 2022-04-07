# 데이터 불러오기
getwd()
setwd('/Users/sik/Desktop/dataset4')
install.packages('ROCR')
library(ROCR)

# 1. 날씨 관련 요인 변수로 비(rain) 유무 예측
weather = read.csv('weather.csv', stringsAsFactors = F)
dim(weather)
head(weather)
str(weather)

# 1-1 변수 선택과 더미 변수 생성
weather_df <- weather[, c(-1, -6, -8, -14)]
weather_df
str(weather_df)
weather_df$RainTomorrow

weather_df$RainTomorrow[weather_df$RainTomorrow == 'Yes'] <- 1
weather_df$RainTomorrow[weather_df$RainTomorrow == 'No'] <- 0
weather_df$RainTomorrow <- as.numeric(weather_df$RainTomorrow)
head(weather_df$RainTomorrow)

# X, Y 변수 설정 / Y변수를 대상으로 더미 변수 생성 후 로지스틱 회귀분석 환경 설정
# 1-2 학습데이터와 검정데이터 생성 (홀드아웃 7:3)
idx <- sample(1:nrow(weather_df), nrow(weather_df)*0.7)
train <- weather_df[idx, ]
test <- weather_df[-idx, ]
train
test
train = na.omit(train)
test = na.omit(train)
nrow(test)
test[1:110, ]
tail(test, 30)
# 데이터 생성 후 NA값의 위치를 확인 후 회귀모델을 생성.
# 양쪽다 위치 할 경우 다시 돌리며, 한쪽에 위치할 경우 어디에 위치했는지 파악할 것.

# 1-3 로지스틱 회귀모델 생성
# glm()함수
# 형식: glm(y~x, data, family)
# family = ‘binomial’ 속성: y변수가 이항형
weather_model <- glm(RainTomorrow ~ ., data = train, family = 'binomial', na.action = na.omit)
weather_model # train 셋에 NA값이 위치 할 경우 
summary(weather_model)
# 로지스틱 회귀모델의 결과는 선형 회귀모델과 동일하게 x변수의 유의성 검정을 제공
# 하지만 F-검정 통계량과 모델의 설명력은 제공되지 않는다.

# 1-4 로지스틱 회귀모델 예측치 생성
is.na(test$RainTomorrow)

pred <- predict(weather_model, newdata = test, type = 'response')
pred
result_pred <- ifelse(pred >= 0.5, 1, 0)
result_pred
table(result_pred)
# type=”response”속성: 에측 결과를 0-1사이의 확률값으로 예측치를 얻기 위해서 지정
# 모델 평가를 위해서 예측치가 확률값으로 제공되기 때문에 이를 이항형으로 변환하는 과정이
# 필요 -> Ifelse()함수를 이용하여 예측치의 벡터변수(pred)를 입력으로 이항형의 벡터
# 변수(result_pred)를 생성

# 1-5 모델평가 - 분류정확도 계산
table(result_pred, test$RainTomorrow) # 분류정확도는 데이터에 따라 상이
acc = (200+30) / (200 + 16 + 7 + 30)
acc 
# 약 89%의 분류정확도를 갖는것을 볼수있음.
# 1-6 ROC Curve를 이용한 모델평가
pr <- prediction(pred, test$RainTomorrow) 
prf <- performance(pr, measure = 'tpr', x.measure = 'fpr')
plot(prf)

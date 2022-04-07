# 카르트 방법
# rpart 패키지 이용 분류분석

# 1. rpart()함수를 이용한 의사결정 트리 생성
# rpart()함수
# 형식: rpart(반응변수(종속) ~ 설명변수(독립), data)

# 1-1 패키지 설치 및 로딩
install.packages('rpart')
library(rpart)
install.packages('rpart.plot')
library(rpart.plot)

# 1-2 데이터 로딩
data(iris)

# 1-3 rpart()함수를 이용한 분류분석
rpart_model <- rpart(Species ~ ., data = iris)
rpart_model

# 1-4 분류분석 시각화
windows()
rpart.plot(rpart_model)

# 2. 날씨 데이터를 이용하여 비(rain)유무 예측
getwd()
setwd('E:/BigDate/R programing/dataset4/dataset4')

# 2-1 데이터 가져오기
weather <- read.csv('weather.csv', header = TRUE)

# 2-2 데이터 특성보기
str(weather) # 데이터셋 366개 관측치 / 15개의 변수
head(weather)

# 2-3 분류분석 데이터 가져오기
weather.df <- rpart(RainTomorrow ~ ., data = weather[, c(-1, -14)], cp = 0.01)

# 2-4 분류분석 시각화
windows()
rpart.plot(weather.df)

# 2-5 예측치 생성과 코딩변경
# 2-5-1 예측치 생성
weather_pred <- predict(weather.df, weather)
weather_pred

# 2-5-2 y의 범주로 코딩 변환
weather_pred2 <- ifelse(weather_pred[, 2] >= 0.5, 'Yes', 'No')

# 2-6 모델 평가
table(weather_pred2, weather$RainTomorrow)
(278 + 53) / nrow(weather)
# 비가 안온다 했을 때 실제 안온 값(278), 비가 온다 했을 때 실제 온 값(53) / nrow(weather)

# 3. 의사결정 나무 / CART 
# iris 데이터 사용

# 3-1 의사결정나무 생성
CARTTree <- rpart(Species ~ ., data = iris)
CARTTree

# 3-2 의사결정나무 시각화
windows()
plot(CARTTree, margin = 0.2)
text(CARTTree, cex = 1)

# 3-3 CARTTree를 이용하여 iris데이터 종 전체를 대상으로 예측
predict(CARTTree, newdata = iris, type = 'class')

# 3-4 3-3의 결과 저장
predicted <- predict(CARTTree, newdata = iris, type = 'class') 

# 3-5 예측정확도
sum(predicted == iris$Species) / NROW(predicted)

# 3-6 실제값과 예측값의 비교
real <- iris$Species
table(real, predicted)

# 결과해석
# 전체데이터 = 150
# y의 값을 setosa로 하면 100개를 설명할 수 없음
# 각 종마다 확률은 0.3333으로 동일
# Petal.Length가 2.45보다 작은 것이 50개 있고 y의 값을 setosa로 했을 때, 설명되지 않는 부분은 없음
# Petal.Length가 2.45보다 크거나 같은 것이 100개. y의 값을 versicolor로 했을 때, 50개가 설명되지 않음
# 의사결정나무를 보면 species(종)인 setosa, versicolor, virginica를 식별하기 위하여 네개의 항목 중
# Petal.Length와 Petal.Width만을 기준으로 사용. --> 두개의 기준으로 충분히 분리가 가능
# 결과로 부터 해석:
# Petal.Length < 2.45 : setosa로 분류
# Petal.Length > 2.45 & Petal_Width < 1.75 : vericolor로 분류
# 나머지: verginica로 분류
# 예측정확도: 96%
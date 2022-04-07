# 단순선형 회귀분석
getwd()
setwd('C:/Temp/Rwork/dataset2/dataset2')
product <- read.csv('product.csv', header = TRUE)
str(product)

# 2. 독립변수와 종속변수 생성
y = product$제품_만족도
x = product$제품_적절성
df <- data.frame(x,y)

# 단순 선형회귀 분석은 lm()함수 이용
# 형식: lm(formula = Y ~ X, data)
# X: 독립변수
# Y: 종속변수
# data: 데이터프레임

# 3. 단순선형회귀 모델 생성
result.lm <- lm(formula = y ~ x, data = df)
# 4. 회귀분석의 절편과 기울기
result.lm
# 5. 모델의 적합값과 잔차 보기
names(result.lm)
# 5-1. 적합값 보기
fitted.values(result.lm)[1:4]
# 5-2 관측값 보기
head(df, 3)
# 5-3 회귀방정식을 적용하여 모델의 적합값 계산
Y = 0.7789 + 0.7393*4
Y
# 5-4 잔차(오차)계산
3 - 3.735963
# 5-5 모델의 잔차 보기
residuals(result.lm)[1:4]
# 5-6 모델의 잔차와 회귀방정식에 의한 적합값으로부터 관측값 계산
-0.735963 + 3.735963

# 선형회귀분석 모델 시각화
# 1. xy산점도
plot(formula = y ~ x, data = product)
# 2. 선형회귀모델 생성
result.lm <- lm(formula = y ~ x, data = product)
result.lm
# 3. 회귀선
abline(result.lm, col = 'red')
# 선형회귀분석 결과보기
summary(result.lm)

# 다중회귀분석
# 1. 변수 모델링
y = product$제품_만족도
x1 = product$제품_친밀도
x2 = product$제품_적절성
df <- data.frame(x1, x2, y)
# 2. 다중 회귀분석
result.lm <- lm(formula = y ~ x1 + x2, data = df)
result.lm
# 다중 공선성 문제 확인
# 1. 패키지 불러오기
install.packages('car')
library(car)
# 2. 분산팽창요인(VIF)
vif(result.lm)
# 다중회귀분석 결과보기
summary(result.lm)

# 다중 공선성 문제 해결과 모델 성능 평가
# 실습순서 다중공선성문제해결 -> 회귀모델 생성 -> 예측치 생성 -> 모델 성능평가
# 1.다중 공선성 문제확인
# 1-1 패키치 설치 및 데이터 로딩
library(car)
data(iris)
# 1-2 iris 데이터 셋으로 다중 회귀분석
model <- lm(formula = Sepal.Length ~ Sepal.Width +
              Petal.Length + Petal.Width, data = iris)
vif(model)
sqrt(vif(model)) > 2
# 1-3 iris 변수간의 상관계수 구하기
cor(iris[, -5])

# 2. 회귀모델 생성
# 2-1 학습데이터와 검정데이터 표본 추출
x <- sample(1:nrow(iris), 0.7*nrow(iris))
train <- iris[x, ]
test <- iris[-x, ]
# 2-2 변수 제거 및 다중 회귀분석
model <- lm(formula = Sepal.Length ~ Sepal.Width + Petal.Length, data = train)
model
summary(model)

# 3. 회귀방정식 도출
# 3-1 회귀방정식을 위한 절편과 기울기 보기
model
# 3-2 회귀방정식 도출
head(train, 2) # sample data에 따라 회귀방정식은 상이할 수 있음.
Y = 2.1120 + 0.6289*3.0 + 0.4885*0.3
Y
6.6 - Y

# 4. 예측치 생성
# predict()함수
# 형식: predict(model, data)
# model: 회귀모델(회귀분석 결과가 저장된 객체)
# data: 독립변수(x)가 존재하는 검정데이터 셋
pred <- predict(model, test)
pred # 데이터에 따라 예측치는 상이할 수 있음.

# 5. 회귀모델 평가
cor(pred, test$Sepal.Length)

# 기본 가정 충족으로 회귀분석 수행
# 1. 회귀모델 생성
# 1-1 변수 모델링
formula = Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width
# 1-2 회귀모델 생성
model <- lm(formula = formula, data = iris)
model
# 2. 잔차(오차)분석
# 2-1 독립성 검정 - 더빈 왓슨 값으로 확인
install.packages('lmtest')
library(lmtest)
dwtest(model)
# 2-2 등분산성 검정 - 잔차와 적합값의 분포
plot(model, which = 1)
# 2-3 잔차의 정규성 검정
attributes(model)
res <- residuals(model)
shapiro.test(res)
par(mfrow = c(1,2))
hist(res, freq = F)
qqnorm(res)
# shapiro.test()함수 이용하여 정규성 검정
# hist()함수로 히스토그램과 qqnorm()함수를 통해 Normal Q-Q plot으로 정규성 확인 가능

# 3. 다중 공선성 검사
library(car)
sqrt(vif(model)) > 2

# 4. 회귀모델 생성과 평가
formula = Sepal.Length ~ Sepal.Width + Petal.Length
model <- lm(formula = formula, data = iris)
summary(model)

# 자기 상관 함수/ 부분 자기 상관 함수 시각화
# 1. 시계열 요소 분해 시각화
# 1-1 시계열 자료 생성
input <- c(3180, 3000, 3200, 3100, 3300, 3200,
           3400, 3550, 3200, 3400, 3300, 3700)
length(input)
tsdata <- ts(input, start = c(2015, 2), frequency = 12)
tsdata

# 1-2 자기 상관 함수 시각화
windows()
acf(na.omit(tsdata), main = '자기상관함수', col = 'red')

# 1-3 부분 자기 상관 함수 시각화
windows()
pacf(na.omit(tsdata), main = '부분자기상관함수', col = 'red')
# 파란선 안쪽으로 위치하면 괜찮음.

# 2. 추세 패턴 찾기 시각화
# 2-1 시계열 자료의 추세 패턴 찾기 시각화 / 자료생성
input <- c(3180, 3000, 3200, 3100, 3300, 3200,
           3400, 3550, 3200, 3400, 3300, 3700)

# 2-2 추세선 시각화
windows()
plot(tsdata, type = 'l', col = 'red')

# 2-3 자기상관함수 시각화
windows()
acf(na.omit(tsdata), main = '자기 상관함수', col = 'red')

# 2-4 차분시각화
windows()
plot(diff(tsdata, differences = 1))

# 3. 평활법
# 3-1 이동평균법을 이용한 평활하기
data <- c(45, 56, 45, 43, 69, 75, 58, 59, 66, 64, 62, 65,
          55, 49, 67, 55, 71, 78, 71, 65, 69, 43, 70, 75,
          56, 56, 65, 55, 82, 85, 75, 77, 77, 69, 79, 89)
length(data)
tsdata <- ts(data, start = c(2016, 1), frequency = 12)
tsdata

# 3-2 평활관련 패키지 설치
install.packages('TTR')
library(TTR)

# 3-3 이동평균법으로 평활 및 시각화
windows()
par(mfrow = c(2,2))
plot(tsdata, main = '원 시계열 자료')
plot(SMA(tsdata, n = 1), main = ' 1개월 단위 이동평균법으로 평활')
plot(SMA(tsdata, n = 2), main = ' 2개월 단위 이동평균법으로 평활')
plot(SMA(tsdata, n = 3), main = ' 3개월 단위 이동평균법으로 평활')
par(mfrow = c(1,1))

# ARIMA모형
# 4. 계절성이 없는 정상성 시계열 분석
# 4-1 시계열 자료 특성 분석
input <- c(3180, 3000, 3200, 3100, 3300, 3200,
           3400, 3550, 3200, 3400, 3300, 3700)

# 4-2 시계열 객체 생성(12개월)
tsdata <- ts(input, start = c(2015, 2), frequency = 12)
tsdata

# 4-3 추세선 시각화
windows()
plot(tsdata, type = 'l', col = 'red')

# 4-4 정상성 시계열 변환
windows()
par(mfrow = c(1,2))
ts.plot(tsdata)
diff <- diff(tsdata)
plot(diff)

# 4-5 모델 식별과 추정
install.packages('forecast')
library(forecast)
arima <- auto.arima(tsdata)
arima

# 4-6 모형생성
model <- arima(tsdata, order = c(1,1,0))
model

# 4-7 모형진단(모형의 타당성 검정)
windows()
tsdiag(model)
# Box-Ljung검정에 의한 잔차항 모형 진단
Box.test(model$residuals, lag = 1, type = 'Ljung')
# Box-Ljung검정: 모형의 잔차를 이용하는 카이제곱 검정방법. 시계열 모형이 통계적으로 적절한지
# 검정. P-value가 0.05이상이면 모형이 통계적으로 적절

# 4-8 미래 예측(업무적용)
fore <- forecast(model)
fore
windows()
par(mfrow = c(1,2))
plot(fore)
model2 <- forecast(model, h = 6)
plot(model2)

# 5. 계절성을 갖는 정상성 시계열 분석
# 5-1 시계열 자료 특성분석 및 데이터 준비
data <- c(55, 56, 45, 43, 69, 75, 58, 59, 66, 64, 62, 65, 
          55, 49, 67, 55, 71, 78, 61, 65, 69, 53, 70, 75, 
          56, 56, 65, 55, 68, 80, 65, 67, 77, 69, 79, 82,
          57, 55, 63, 60, 68, 70, 58, 65, 70, 55, 65, 70)
length(data)

# 5-2 시계열 자료 생성
tsdata <- ts(data, start = c(2020, 1), frequency = 12)
tsdata

# 5-3 시계열 요소 분해 시각화
ts_feature <- stl(tsdata, s.window = 'periodic')
windows()
plot(ts_feature)

# 5-4 정상성 시계열 변환
windows()
par(mfrow = c(1,2))
ts.plot(tsdata)
diff <- diff(tsdata)
plot(diff)

# 5-5 모형 식별과 추정
library(forecast)
ts_model2 <- auto.arima(tsdata)
ts_model2
# ARIMA(0, 1, 1)(1, 1, 0)[12]
# (0, 1, 1): 차분차수 1, MA모형 차수1. 한번 차분한 결과가 정상성 시계열의 ARMA(0, 1)모형으로 식별
# (1, 1, 0): 계절성을 갖는 자기 회귀(AR)모형 차수가 1. 계절성을 갖는 시계열 [12]: 계절의 차수 12개월

# 5-6 모형 생성
model <- arima(tsdata, c(0,0,1), seasonal = list(order = c(1,1,0)))
model

# 5-7 모형 진단
windows()
tsdiag(model)
# Box-Ljung에 의한 잔차항 모형 진단
Box.test(model$residuals, lag = 1, type = 'Ljung')

# 5-8 미래 예측
windows()
par(mfrow = c(1,2))
fore <- forecast(model, h = 24)
plot(fore)
fore2 <- forecast(model, h = 6)
plot(fore2)
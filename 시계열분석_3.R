data('AirPassengers')
AirPassengers
plot(AirPassengers)
plot(stl(AirPassengers, s.window = 'periodic')) # 계절성,추세,불확실성 요소로 분해해서 그래프 확인

install.packages('tseries')
library(tseries)
difflogAirPassengers <- diff(log(AirPassengers))
plot(difflogAirPassengers)

adf.test(difflogAirPassengers, alternative = 'stationary', k = 0) # 안정적 시계열 데이터인지 확인

library(forecast)
auto.arima(difflogAirPassengers)

fitted <- arima(log(AirPassengers), c(1,0,1), seasonal = list(order = c(0,1,1), period = 12))
fitted

predicted <- predict(fitted, n.ahead = 120)
ts.plot(AirPassengers, exp(predicted$pred), lty = c(1,2))
# 향후 10년(120개월)간의 데이터 예측 후 기존데이터와 예측치를 이어 그래프 표현

getwd()
setwd('E:/BigDate/R programing/dataset2/dataset2')
library(forecast)
ex_data <- read.csv('example.csv', header = T)
plot(ex_data)

training_data = ex_data[1:293, 2] # 총 318개데이터중 처음 293개의 데이터를 훈련 데이터 세트로 설정
testing_data = ex_data[294:318, 2] # 나머지 25개의 데이터를 검증 데이터세트로 설정
training_timeseries = ts(training_data, frequency = 1)
testing_timeseries = ts(testing_data, frequency = 1)
plot.ts(training_timeseries)

# ARIMA 모델생성
max_p = 5 # 최대 arima p 값
max_q = 5 # 최대 arima q 값

AIC_set = matrix(0, nrow = (max_p+1), ncol = (max_q+1))
  for(p in 0:max_p)
  {
    for(q in 0:max_q)
    {
      model = arima(training_timeseries, order = c(p,1,q), method = 'ML')
      AIC_set[(p+1), (q+1)] = model$aic
    }
  }

which(AIC_set == min(AIC_set), arr.ind = T)

model = arima(training_timeseries, order = c(6,1,4))
forecasted = forecast.Arima(model, h=25)

MAE_result = mean(abs(testing_data - forecasted$mean))

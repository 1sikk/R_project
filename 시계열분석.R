# 시계열 분석
getwd()
setwd()

# 비정상성 시계열을 정상성 시계열로 변경
# 1. 데이터셋 가져오기
data("AirPassengers")
AirPassengers

# 1-2 차분적용 - 평균정상화
par(mfrow = c(1,2))
ts.plot(AirPassengers)
diff <- diff(AirPassengers)
plot(diff)

# 1-3 로그적용 - 분산정상화
par(mfrow = c(1,2))
plot(AirPassengers)
log <- diff(AirPassengers)
plot(log)

# 시계열 추세선 시각화
# 2. 단일 시계열 자료 시각화
data('WWWusage')
str(WWWusage)
WWWusage

# 2-1 시계열 자료 추세선 시각화
X11() # windows() 함수랑 동일
ts.plot(WWWusage, type = 'l', col = 'red')
X11()
plot.ts(WWWusage, type = 'l', col = 'red') # ts.plot랑 동일

# 3. 다중 시계열 자료 시각화
# 3-1 데이터셋 가져오기
data(EuStockMarkets)
head(EuStockMarkets)

# 3-2 데이터프레임으로 변환
EuStock <- data.frame(EuStockMarkets)
head(EuStock)

# 3-3 단일 시계열자료 추세선 시각화(1,000개 데이터 대상)
X11()
plot(EuStock$DAX[1:1000], type = 'l', col = 'red')

# 3-4 다중 시계열자료 추세선 시각화(1,000개 데이터 대상)
X11()
plot.ts(cbind(EuStock$DAX[1:1000], EuStock$SMI[1:1000]), main = '주가지수 추세선')

# 4. 시계열 요소 분해 시각화
# 4-1 시계열 자료준비
data <- c(45, 56, 45, 43, 69, 75, 58, 59, 66, 64, 62, 65,
          55, 49, 67, 55, 71, 78, 71, 65, 69, 43, 70, 75,
          56, 56, 65, 55, 82, 85, 75, 77, 77, 69, 79, 89)
length(data)

# 4-2 시계열 자료생성 - 시계열 자료형식으로 객체 생성
tsdata <- ts(data, start = c(2016, 1), frequency = 12)
tsdata

# 4-3 추세선 확인 - 각 요인(추세, 순환, 계절, 불규칙)을 시각적 확인
X11()
ts.plot(tsdata)

# 4-4 시계열 분해
X11()
plot(stl(tsdata, 'periodic'))
# stl()함수: 하나의 시계열 자료를 대상으로 시계열 변동요인인 계절요소(seasonal), 추세(trend), 
# 잔차(remainder)를 모두 제공

# 4-5 시계열 분해와 변동요인 제거
m <- decompose(tsdata)
attributes(m)

plot(m)

par(mfrow = c(1,1))
plot(tsdata - m$seasonal)

# 4-6 추세요인과 불규칙요인 제거
plot(tsdata - m$trend)
plot(tsdata - m$seasonal - m$trend)
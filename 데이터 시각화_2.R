# 격자형 기법 시각화
# lattice 패키지 활용
install.packages('lattice')
library(lattice)

# 1. 데이터 가져오기
install.packages('mlmRev', type = 'binary')
library(mlmRev)
data('Chem97')
str(Chem97)
head(Chem97, 30)
Chem97

# 2. 히스토그램
histogram(~gcsescore, data = Chem97)

# 2-1 score 변수를 조건변수로 지정 데이터 시각화
histogram(~gcsescore | score, data = Chem97)
histogram(~gcsescore | factor(score), data = Chem97)

# 2-2 밀도그래프
densityplot(~gcsescore | factor(score), data = Chem97,
            groups = gender, plot.Points = T,
            auto.ley = T)

# 2-3 막대그래프
data(VADeaths)
VADeaths
str(VADeaths) # 데이터 셋 구조보기
class(VADeaths) # 데이터 셋 구조보기
mode(VADeaths) # 데이터 셋 구조보기

# 2-3-1 데이터 형식 변경(matrix형식을 table형식으로)
dft <- as.data.frame.table(VADeaths)
str(dft)
dft

# 2-3-2 막대그래프 그리기
barchart(Var1 ~ Freq | Var2, data = dft, layout = c(4,1))

# 2-3-3 origin 속성을 사용 막대 그래프 그리기
barchart(Var1 ~ Freq | Var2, data = dft, layout = c(4,1), origin = 0)

# 2-4 점 그래프
# layout 속성이 없는 경우
dotplot(Var1 ~ Freq | Var2, dft)
# layout 속성을 적용한 후
dotplot(Var1 ~ Freq | Var2, dft, layout = c(4,1))

# 2-4-1 점을 선으로 연결하여 시각화 하기
dotplot(Var1 ~ Freq, data = dft,
        groups = Var2, type = 'o',
        auto.key = list(space = 'right', points = T, lines = T))

# 2-5 산점도그래프
library(datasets)
str(airquality)

# xyplot()함수 이용 산점도 그리기
xyplot(Ozone ~ Wind, data = airquality, col = 'red')

# 조건변수를 사용하는 xyplot()함수로 산점도 그리기
xyplot(Ozone ~ Wind | Month, data = airquality)

# 조건변수와 layout속성을 사용하는 xyplot()함수로 산점도 그리기
xyplot(Ozone ~ Wind | Month, data = airquality, layout = c(5,1))

# month변수를 factor타입으로 변환하여 산점도 그리기(패널 제목 factor값 표시)
convert <- transform(airquality, Month = factor(Month))
str(convert)
xyplot(Ozone ~ Wind | Month, data = convert)
xyplot(Ozone ~ Wind | Month, data = convert, layout = c(5,1)) # layout 이용

# 2-5-1 quakes 데이터 셋 이용 산점도 그리기
head(quakes)
str(quakes)

# 지진 발생 진앙지(위도와 경도) 산점도 그리기
xyplot(lat ~ long, data = quakes, pch = '★', col = 'purple')

# 산점도 그래프를 변수에 저장, 제목 문자열 추가
tplot <- xyplot(lat ~ long, data = quakes, pch = '☆')
tplot <- update(tplot, main = '1964년 이후 태평양에서 발생한 지진 위치')
print(tplot)

# 2-5-2 이산형 변수를 조건으로 지정 산점도 그리기
# 수심별로 진앙지 파악 위해 depth변수를 이상현 변수조건으로 지정.
range(quakes$depth)

# depth 변수 리코딩 : 6개의 범주(100단위)로 코딩 변경
quakes$depth2[quakes$depth >= 40 & quakes$depth <= 150] <- 1
quakes$depth2[quakes$depth >= 151 & quakes$depth <= 250] <- 2
quakes$depth2[quakes$depth >= 251 & quakes$depth <= 350] <- 3
quakes$depth2[quakes$depth >= 351 & quakes$depth <= 450] <- 4
quakes$depth2[quakes$depth >= 451 & quakes$depth <= 550] <- 5
quakes$depth2[quakes$depth >= 551 & quakes$depth <= 680] <- 6

# 리코딩 변수(depth2)를 조건으로 산점도 그리기
convert <- transform(quakes, depth2 = factor(depth2))
xyplot(lat ~ long | depth2, data = convert)
xyplot(lat ~ long | depth2, data = convert, layout = c(6,1))
# 동일 패널에 두개의 변수값 표현
xyplot(Ozone + Solar.R ~ Wind | factor(Month),
       data = airquality,
       col = c('skyblue', 'red'),
       layout = c(5,1))

# 2-6 데이터 범주화
# lattice 패키지의 equal.count()함수 이용
# 1~150 대상으로 겹치지 않게 4개 영역으로 범주화
numgroup <- equal.count(1:150, number = 4, overlap = 0)
numgroup
# 지진의 깊이를 5개 영역으로 범주화
depthgroup <- equal.count(quakes$depth, number = 5, overlap = 0)
depthgroup
# 범주화된 변수(depthgroup)조건으로 산점도 그리기
xyplot(lat ~ long | depthgroup, data = quakes,
       main = 'Fiji Earthquakes(depthgroup)',
       ylab = 'latitude', xlab = 'longitube',
       pch = '★', col = 'red')

# 2-6-1 수심과 리히터 규모 변수를 동시에 적용 산점도 그리기
# 리히터 규모를 2개 영역으로 구분
magnitudegroup <- equal.count(quakes$mag, number = 2, overlap = 0)
magnitudegroup
# magnitudegroup 변수를 기준 산점도 그리기
xyplot(lat ~ long | magnitudegroup, data = quakes,
       main = 'Fiji Earthquakes(magnitudegroup)',
       ylab = 'latitude', xlab = 'longitude',
       pch = '@', col = 'skyblue')

# 2-6-2 이산형변수를 리코딩한 뒤 factor로 변환 후 산점도 그리기
# depth 변수 리코딩
quakes$depth3[quakes$depth >= 39.5 & quakes$depth <= 80.5] <- 'd1'
quakes$depth3[quakes$depth >= 80.6 & quakes$depth <= 186.5] <- 'd2'
quakes$depth3[quakes$depth >= 186.6 & quakes$depth <= 397.5] <- 'd3'
quakes$depth3[quakes$depth >= 397.6 & quakes$depth <= 562.5] <- 'd4'
quakes$depth3[quakes$depth >= 562.6 & quakes$depth <= 680.5] <- 'd5'
# mag 변수 리코딩
quakes$mag3[quakes$mag >= 3.95 & quakes$mag <= 4.65] <- 'm1'
quakes$mag3[quakes$mag >= 4.66 & quakes$mag <= 6.65] <- 'm2'
# factor변환
convert <- transform(quakes,
                     depth3 = factor(depth3),
                     mag3 = factor(mag3))
# 산점도 그래프 그리기
xyplot(lat ~ long | depth3*mag3, data = convert,
       main = 'Fiji Earthquakes',
       ylab = 'latitude', xlab = 'longitude',
       pch = 'o', col = c('red', 'skyblue'))

# 2-7 조건 그래프
# depth조건에 의해 위도와 경도의 조건 그래프 그리기
coplot(lat ~ long | depth, data = quakes)

# 조건의 구간 크기, 겹침, 간격 적용 후 조건 그래프 그리기
# 조건의 구간 막대기가 0, 1 단위로 겹쳐 범주화
coplot(lat ~ long | depth, data = quakes,
       overlap = 0.1)
# 조건 구간을 5개로 지정 후 1행 5열의 패널로 조건 그래프 작성
coplot(lat ~ long | depth, data = quakes,
       number = 5, row = 1)

# 2-7-1 패널과 조건 막대에 색을 적용 후 조건 그래프 그리기
# 패널 영역에 부드러운 곡선 추가
coplot(lat ~ long | depth, data = quakes,
       number = 5, row = 1,
       panel = panel.smooth)
# 패널 영역과 조건 막대에 색상 적용
coplot(lat ~ long | depth, data = quakes,
       number = 5, row = 1,
       panel = panel.smooth,
       col = 'skyblue',
       bar.bg = c(num = 'green'))

# 2-8 3차원 산점도 그래프
# 위도, 경도, 깊이를 이용 3차원 산점도 그리기
cloud(depth ~ lat * long, data = quakes,
      xlab = '경도', ylab = '위도', zlab = '깊이')

# 2-8-1 테두리와 회전 속성을 추가 후 3차원 산점도 그래프 그리기
cloud(depth ~ lat * long, data = quakes,
      zlim = rev(range(quakes$depth)),
      panel.aspect = 0.9,
      screen = list(z = 45, x = -25),
      xlab = '경도', ylab = '위도', zlab = '깊이')


# 기하학적 기법 시각화
# qplot()함수

# 1. ggplot2 패키지 이용
library(ggplot2)
data(mpg)
str(mpg)
head(mpg)
summary(mpg)
table(mpg$drv)

# qplot함수의 fill, binwidth속성 적용
# 도수 분포를 세로 막대 그래프로 표현
qplot(hwy, data = mpg)
# fill 속성 적용
qplot(hwy, data = mpg, fill = drv)
# binwidth 속성 적용
qplot(hwy, data = mpg, fill = drv, binwidth = 2)

# factor속성 사용 drv변수값으로 행/열 단위로 패널 생성
# 열단위 패널 생성
qplot(hwy, data = mpg, fill = drv, facets = . ~ drv, binwidth = 2)
# 행단위 패널 생성
qplot(hwy, data = mpg, fill = drv, facets = drv ~ ., binwidth = 2)

# 두개 변수 대상으로 qplot적용 / qplot함수에서 color속성 사용 후 두 변수 구분하기
# 두 변수로 displ과 hwy 사용
qplot(displ, hwy, data = mpg)
# 두 변수에 색상 적용
qplot(displ, hwy, data = mpg, color = drv)
# 두 변수 관계를 driv변수로 구분하기
qplot(displ, hwy, data = mpg, color = drv, facets = . ~ drv)

# 2. 미적요소 맵핑
# mtcars데이터 셋 이용
head(mtcars)
# 색상 적용
qplot(wt, mpg, data = mtcars, color = factor(carb))
# 크기 적용
qplot(wt, mpg, data = mtcars,
      size = qsec, color = factor(carb))
# 모양 적용
qplot(wt, mpg, data = mtcars,
      size = qsec, color = factor(carb), shape = factor(cyl))

# 3. 기하학적 객체 적용
# geom 속성 이용
head(diamonds)
# geom속성과 fill속성 사용
qplot(clarity, data = diamonds, fill = cut, geom = 'bar')
# 테두리 색 사용
qplot(clarity, data = diamonds, colour = cut, geom = 'bar')
# geom = 'point' 속성으로 산점도 그래프 그리기
qplot(wt, mpg, data = mtcars, size = qsec, geom = 'point')
# 산점도 그래프에 cyl변수 요인으로 포인트 크기 적용 및 carb변수 요인으로 포인트 색 적용
qplot(wt, mpg, data = mtcars, size = factor(cyl),
      color = factor(carb), geom = 'point')
# 산점도 그래프에 qsec변수 요인으로 포인트 크기 적용 및 cyl변수 요인으로 포인트 모양 적용
qplot(wt, mpg, data = mtcars, size = qsec,
      color = factor(carb),
      shape = factor(cyl), geom = 'point')
# geom='smooth'속성으로 산점도 그래프에 평활 그리기
qplot(wt, mpg, data = mtcars,
      geom = c('point', 'smooth'))
# 산점도 그래프 평활에 cyl변수 요인으로 색상 적용
qplot(wt, mpg, data = mtcars, color = factor(cyl),
      geom = c('point', 'smooth'))
# geom=line 속성으로 그래프 그리기
qplot(mpg, wt, data = mtcars,
      color = factor(cyl), geom = 'line')
# geom=c('point', 'line') 속성으로 그래프 그리기
qplot(mpg, wt, data = mtcars,
      color = factor(cyl), geom = c('point', 'line'))

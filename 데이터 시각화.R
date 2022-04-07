# 데이터 시각화
library(ggplot2)
# 1. 이산 변수 시각화
# barplot() 이용

# 1-1 차트 작성 위한 자료
chart_data <- c(305, 450, 320, 460, 330, 480, 380, 520)
names(chart_data) <- c('2018 1분기', '2019 1분기',
                       '2018 2분기', '2019 2분기',
                       '2018 3분기', '2019 3분기',
                       '2018 4분기', '2019 4분기')
str(chart_data)
chart_data

# 1-2 세로 막대 차트 그리기
barplot(chart_data, ylim = c(0, 600),
        col = rainbow(8),
        main = '2018년도 vs 2019년도 매출현황 비교')

# help("barplot") -> 함수 도움말 보기

# 1-3 막대 차트의 가로축과 세로축에 레이블 추가
barplot(chart_data, ylim = c(0, 600),
        ylab = '매출액(단위:만원)',
        xlab = '년도별 분기 현황',
        col = rainbow(8),
        main = '2018년도 vs 2019년도 매출현황 비교')

# 2. 가로막대 차트
# 2-1 가로 막대 그리기
barplot(chart_data, xlim = c(0, 600), horiz = T,
        ylab = '매출액(단위:만원)',
        xlab = '년도별 분기 현황',
        col = rainbow(8),
        main = '2018년도 vs 2019년도 매출현황 비교')

# 2-2 막대차트에서 막대사이 간격조정하기
barplot(chart_data, xlim = c(0, 600), horiz = T,
        ylab = '매출액(단위:만원)',
        xlab = '년도별 분기 현황',
        col = rainbow(8), space = 1, cex.names = 0.8,
        main = '2018년도 vs 2019년도 매출현황 비교')
# space속성 - 막대의 굵기와 간격을 지정 / 값이 클수록 막대 굵기는 작아지고, 막대 사이의 간격은 넓어짐.
# cex.names속성 - 축 이름의 크기 지정

# 2-3 막대차트에서 막대의 색상 지정
barplot(chart_data, xlim = c(0, 600), horiz = T,
        ylab = '매출액(단위:만원)',
        xlab = '년도별 분기 현황',
        space = 1, cex.names = 0.8,
        main = '2018년도 vs 2019년도 매출현황 비교',
        col = rep(c(2,4),4))
# 1 - black / 2 - red / 3 - green / 4 - blue / 5 - skyblue / 6 - purple / 7 - yellow
# col = rep(c(2,4), 4) <- 2번과 4번색상을 4번 반복

# 2-4 색상이름을 사용하여 막대의 색상 지정하기
barplot(chart_data, xlim = c(0, 600), horiz = T,
        ylab = '매출액(단위:만원)',
        xlab = '년도별 분기 현황',
        space = 1, cex.names = 0.8,
        main = '2018년도 vs 2019년도 매출현황 비교',
        col = rep(c('red','green'), 4))

# 3. 누적막대 차트
# 3-1 메모리에 데이터 가져오기
data('VADeaths')
VADeaths

# 3-2 데이터셋 구조보기
str(VADeaths)
class(VADeaths)
mode(VADeaths)

# 3-3 개별차트와 누적차트 그리기
windows()
par(mfrow = c(1,2))
barplot(VADeaths, beside = T, col = rainbow(5),
        main = '미국 버지니아주 하위계층 사망비율')
legend(19, 71, c('50-54', '55-59', '60-64', '65-69', '70-74'),
       cex = 0.8, fill = rainbow(5)) # 개별차트
barplot(VADeaths, beside = F, col = rainbow(5))
title(main = '미국 버지나아주 하위계층 사망비율', font.main = 4)
legend(3.8, 200, c('50-54', '55-59', '60-64', '65-69', '70-74'),
       cex = 0.8, fill = rainbow(5)) # 누적차트

# 4. 점 차트 시각화
# help(dotchart)

# 4-1 점 차트 사용하기
par(mfrow = c(1,1))
dotchart(chart_data, color = c('blue', 'red'),
         lcolor = 'black', pch = 1:2,
         labels = names(chart_data),
         xlab = '매출액',
         main = '분기별 판매현황 : 점차트 시각화',
         cex = 1.2)

# 5. 원형 차트 시각화
# help(pie)

# 5-1 분기별 매출현황을 파이 차트로 시각화
par(mfrow = c(1,1))
pie(chart_data, labels = names(chart_data), col = rainbow(8), cex = 1.2)
title('2018~2019년도 분기별 매출현황')

# 6. 연속변수 시각화
# 6-1 상자그래프 시각화
# VADeaths 데이터를 상자그래프로 시각화
# 1단계 notch = FALSE 일때
boxplot(VADeaths, range = 0)
# 2단계 notch = TRUE 일때
boxplot(VADeaths, range = 0, notch = T)
abline(h = 37, lty = 3, col = 'red')
# 데이터셋 요약통계량 보기
summary(VADeaths)

# 6-2 히스토그램 시각화
# iris데이터 이용
data(iris)
names(iris)
str(iris)
head(iris)

# 꽃받침 길이(Sepal.Length)컬럼으로 히스토그램 시각화
summary(iris$Sepal.Length)
hist(iris$Sepal.Length, xlab = iris$Sepal.Length, col = 'magenta',
     main = 'iris 꽃 받침 길이 Histogram', xlim = c(4.3, 7.9))

# 꽃받침 너비(Speal.Width)컬럼으로 히스토그램 시각화
summary(iris$Sepal.Width)
hist(iris$Sepal.Width, xlab = iris$Sepal.Width, col = 'mistyrose',
     main = 'iris 꽃받침 너비 Histogram', xlim = c(2.0, 4.5))

# 6-2-1 히스토그램에서 빈도와 밀도 표현
# 빈도수에 의해 히스토그램 그리기
par(mfrow = c(1,2))
hist(iris$Sepal.Width, xlab = 'iris$Sepal.Width',
     col = 'green',
     main = 'iris 꽃받침 너비 Histogram: 빈도수', xlim = c(2.0, 4.5))
# 확률 밀도에 의해 히스토그램 그리기
hist(iris$Sepal.Width, xlab = 'iris$Sepal.Width',
     col = 'mistyrose', freq = F,
     main = 'iris 꽃받침 너비 Histogram: 확률 밀도', xlim = c(2.0, 4.5))
# 밀도 기준 line추가하기
lines(density(iris$Sepal.Width, col = 'red'))

# 7. 정규분포 추정 곡선 나타내기
# 계급을 밀도로 표현한 히스토그램 시각화
par(mfrow = c(1,1))
hist(iris$Sepal.Width, xlab = 'iris$Sepal.Width', col = 'mistyrose',
     freq = F, main = 'iris 꽃받침 너비 Histogram', xlim = c(2.0, 4.5))
# 히스토그램에 밀도를 기준으로 분포곡선 추가
lines(density(iris$Sepal.Width), col = 'red')
# 히스토그램에 정규분포 추정 곡선 추가
x <- seq(2.0, 4.5, 0.1)
curve(dnorm(x, mean = mean(iris$Sepal.Width),
            sd = sd(iris$Sepal.Width)),
      col = 'blue', add = T)

# 8. 산점도 시각화
# 산점도 그래프에 대각선과 텍스트 추가하기
# 기본 산점도 시각화
price <- runif(10, min = 1, max = 100)
plot(price, col  = 'red')
# 대각선 추가
par(new = T)
line_chart = 1:100
plot(line_chart, type = 'l', col = 'red', axes = F, ann = F)
# 텍스트 추가
text(70, 80, '대각선 추가', col = 'skyblue')

# 8-1 type속성으로 산점도 그리기
par(mfrow = c(2,2))
plot(price, type = 'l')
plot(price, type = 'o')
plot(price, type = 'h')
plot(price, type = 's')

# pch속성으로 산점도 그리기
# pch속성과 col, ces속성 사용
par(mfrow = c(2,2))
plot(price, type = 'o', pch = 5)
plot(price, type = 'o', pch = 15)
plot(price, type = 'o', pch = 20, col = 'blue')
plot(price, type = 'o', pch = 20, col = 'orange', cex = 1.5)
plot(price, type = 'o', pch = 20, col = 'green', cex = 2.0, lwd = 3)

# lwd속성 추가 사용
par(mfrow = c(1,1))
plot(price, type = 'o', pch = 20,
     col = 'green', cex = 2.0, lwd = 3)

# methods('plot') / plot()시각화 도구 목록

# 9. plot()함수에서 시계열 객체 사용하여 추세선 그리기
data('WWWusage')
str(WWWusage)
plot(WWWusage)

# 10. 중첩자료 시각화
# 10-1 두개의 벡터 객체 준비
x <- c(1, 2, 3, 4, 2, 4)
y <- rep(2, 6)
x; y

# 10-2 교차테이블 작성
table(x,y)

# 10-3 산점도 시각화
plot(x,y)

# 10-4 교차테이블 데이터 프레임 생성
xy.df <- as.data.frame(table(x,y))
xy.df

# 10-5 좌표에 중복된 수 만큼 점을 확대
plot(x,y,
     pch = '@', col = 'blue', cex = 0.5*xy.df$Freq,
     xlab = 'x 벡터의 원소', ylab = 'y 벡터 원소')

# 11. galton 데이터 이용 중복된 자료 시각화하기
# 11-1 데이터 가져오기
install.packages('UsingR')
library(UsingR)
data(galton)

# 11-2 교차테이블 작성 후, 데이터 프레임 변환
galtonData <- as.data.frame(table(galton$child, galton$parent))
head(galtonData)

# 11-3 컬럼단위 추출
names(galtonData) = c('child', 'parent', 'freq')
head(galtonData)
parent <- as.numeric(galton$parent)
child <- as.numeric(galton$child)

# 11-4 점의 크기 확대
par(mfrow = c(1,1))
plot(parent, child,
     pch = 21, col = 'red', bg = 'black',
     cex = 0.2*galtonData$freq,
     xlab = 'parent', ylab = 'child')

# 12. 변수간의 비교 시각화
# iris 데이터 4개 변수를 상호비교
attributes(iris)
pairs(iris[iris$Species == 'virginica', 1:4])
pairs(iris[iris$Species == 'setosa', 1:4])

# 12-1 3차원으로 산점도 시각화
install.packages('scatterplot3d')
library(scatterplot3d)

# 꽃의 종류별 분류
iris_setosa = iris[iris$Species == 'setosa',]
iris_versicolor = iris[iris$Species == 'versicolor',]
iris_virginica = iris[iris$Species == 'virginica',]

# 3차원 틀(frame)생성
d3 <- scatterplot3d(iris$Petal.Length,
                    iris$Sepal.Length,
                    iris$Sepal.Width,
                    type = 'n')

# 3차원 산점도 시각화
d3$points3d(iris_setosa$Petal.Length,
            iris_setosa$Sepal.Length,
            iris_setosa$Sepal.Width,
            bg = 'orange', pch = 21)
d3$points3d(iris_versicolor$Petal.Length,
            iris_versicolor$Sepal.Length,
            iris_versicolor$Sepal.Width,
            bg = 'skyblue', pch = 23)
d3$points3d(iris_virginica$Petal.Length,
            iris_virginica$Sepal.Length,
            iris_virginica$Sepal.Width,
            bg = 'green', pch = 25)

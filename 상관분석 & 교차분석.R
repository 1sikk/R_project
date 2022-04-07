getwd()
setwd('/Users/sik/Desktop/dataset2')
product <- read.csv('product.csv', header = TRUE, encoding = 'UTF-8')
head(product)

# 기술 통계량
summary(product)
sd(product$제품_친밀도); sd(product$제품_적절성); sd(product$제품_만족도)

# 상관계수 보기
# 변수간의 상관계수는 stats패키지에서 제공하는 cor()함수 사용
# 형식: cor(x, y=NULL, use=”everything”, method=c(“pearson”, “kendall”, “spearman”))
# method를 생략하면 pearson이 사용된다(default)

# 1. 변수 간의 상관계수 보기
cor(product$제품_친밀도, product$제품_적절성) # pearson
cor(product$제품_친밀도, product$제품_적절성, method = 'spearman')
cor(product$제품_친밀도, product$제품_적절성, method = 'kendall')
cor(product$제품_친밀도, product$제품_만족도)

# 2. 제품_적절성과 제품_만족도의 상관계수 보기
cor(product$제품_적절성, product$제품_만족도) # pearson
cor(product$제품_적절성, product$제품_만족도, method = 'spearman')
cor(product$제품_적절성, product$제품_만족도, method = 'kendall')

# 3. (제품_적절성+제품_친밀도)와 제품_만족도의 상관계수 보기
cor(product$제품_적절성 + product$제품_친밀도, product$제품_만족도) # pearson
cor(product$제품_적절성 + product$제품_친밀도, product$제품_만족도, method = 'spearman')
cor(product$제품_적절성 + product$제품_친밀도, product$제품_만족도, method = 'kendall')

# 전체 변수 간의 상관계수 보기
cor(product, method = 'pearson')
cor(product, method = 'kendall')

# 방향성 있는 색상으로 표현
install.packages('corrgram')
library(corrgram)
windows() # 별도의 창으로 그래프 불러오기.
corrgram(product)
corrgram(product, upper.panel = panel.conf)
corrgram(product, lower.panel = panel.conf)

# 차트에 밀도곡선, 상관성, 유의확률(별표) 추가
install.packages('PerformanceAnalytics')
library(PerformanceAnalytics)

# 1. 상관성, p값(*), 정규분포(모수 검정 조건) 시각화
chart.Correlation(product, histogram = , pch = '+')
chart.Correlation(product[ ,1:3], histogram = , pch = '+')
# 서열척도 대상 상관계수
cor(product, method = 'spearman')

# 피어슨(Pearson)상관계수: 대상변수가 등간척도 또는 비율척도일 때
# 스피어만(Spearman) 상관계수: 대상변수가 서열척도일 때

# 교차분석
getwd()
setwd()
data <- read.csv('cleanDescriptive.csv', header = TRUE)
head(data)

# 1. 변수 리코딩
x <- data$level2
y <- data$pass2

# 교차 분할표 작성을 위해 데이터프레임 생성 방법
# data.frame(컬럼명 = x, 컬럼명 = y)
# x, y: 명목척도 변수

# 2. 데이터 프레임 생성
result <- data.frame(Level = x, Pass = y)
result
dim(result)

# 교차 분할표 작성
table(result)


# 교차분할표 작성 위한 패키지 설치
install.packages('gmodels')
library(gmodels)
install.packages('ggplot2')
library(ggplot2)

CrossTable(x = diamonds$color, y = diamonds$cut)

# 부모의 학력수준과 자녀 대학 진학여부
# 변수모델 : 학력수준(독립변수) -> 진학여부(종속변수)
x <- data$level2
y <- data$pass2

CrossTable(x, y)

# 광역시, 특별시, 시구군에 남,여
a <- data$resident2
b <- data$gender2

CrossTable(a, b)

# 남,여의 나이
c <- data$gender2
d <- data$age2

CrossTable(c, d)

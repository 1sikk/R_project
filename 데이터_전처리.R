# 데이터 셋 가져오기
setwd('C:/Temp/Rwork/dataset2/dataset2')
setwd('E:/BigDate/R programing/dataset2/dataset2')
data <- read.csv("descriptive.csv",header = TRUE)
head(data)
# 데이터 특성 보기
dim(data)
length(data)
length(data$survey) # $ -> data셋에 컬럼명을 찍어라
str(data)
# 데이터 특성(최소값, 최대값, 평균, 분위수, 결측치) 제공
summary(data)

# 명목척도 기술통계량
# 성별 변수의 기수통계량과 빈도수 summary(), table()함수

length(data$gender)
summary(data$gender)
table(data$gender)

# 이상치(outlier)제거
# Alt + - : <- 생성단축키
lee <- subset(data, gender == 1 | gender == 2)
x <- table(lee$gender)
x
print(x)
barplot(x)

# 구성 비율 계산
prop.table(x)
y <- prop.table(x)
round(y * 100, 2)
# prop.table(x) 함수
# 형식 : prop.table(x, margin = NULL)
# Where margin = 1(행), 2(열) 계산기준
# 문자열과 같이 쓸려면 data.frame을 씀

m <- matrix(1:4, 2)
m
print(m)
prop.table(m, 1)
prop.table(m, 2)
prop.table(m)
n <- prop.table(m)
round(n * 100, 2)

# round()함수
# 형식 : round(x, digits=0)
# Where digits : 표시할 소수점 이하 자리
# 오사오입 반올림 : 앞자리가 홀수이면 반올림
# 짝수이면 반올림 안함 1.5 -> 2 / 4.5 -> 4
prop.table(x)
o <- prop.table(x)
round(o * 10, 1)
prop.table(m)
p <- prop.table(m)
round(m * 1000, 1)

# 서열척도 기술통계량
# 학력수준 변수를 대상으로 구성 비율 산출
data
head(data)
length(data$level)
summary(data$level)
table(data$level)

x1 <- table(data$level)
barplot(x1)

# 등간척도 기술통계량
# 만족도 변수를 대상으로 요약 통계량
# 1단계 : 등간척도 변수 추출
data
head(data)
survey <- data$survey
survey
# 2단계 : 등간척도 요약 통계량
summary(survey)
# 등간척도 빈도분석
data1 <- table(survey)
data1
# 등간척도 시각화
hist(survey)
pie(data1)

# 비율척도 기술통계량
# 생활비 변수 대상 요약통계량
data
head(data)
length(data$cost)
summary(data$cost)
# 데이타 정제(결측치 제거)
plot(data$cost)
soon <- subset(data, data$cost >= 2 & data$cost <= 10)
t <- soon$cost
mean(t)
# plot()함수로 결측치 발견
# subset()함수로 결측치 제거된 subset 산출
# 1) 대표값 구하기
# 평균과 중위수
mean(t) # 평균
median(t) # 중위수
sort(t)
sort(t, decreasing = T) # 내림차순정렬
sort(t, decreasing = F) # 오름차순정렬

# 사분위수
quantile(t, 1/4)
quantile(t, 2/4)
quantile(t, 3/4)
quantile(t, 4/4)

# 생활비 변수의 최빈수
length(t)
x.t <- table(t)
max(x.t)
# 2개 행을 묶어 matrix생성
x.m <- rbind(x.t)
class(x.m)
str(x.m)
which(x.m[1,]==18) # 비교
which(x.m == 18) # 비교
# 데이터 프레임 변경
x.df <- as.data.frame(x.m)
which(x.df[1,]==18) #비교
which(x.df == 18) # 비교
# 최빈수와 변량 확인
x.df[1, 19] # 행 기준 19인게 무엇이냐?
attributes(x.df)
names(x.df[19])
# attributes() 특정 객체의 속성 정보를 확인하는 함수
# 산포도 구하기
var(t)
sd(t)
sqrt(var(soon$cost, na.rm = T))
# 생활비 변수의 빈도분석과 시각화
table(soon$cost) # 연속형 변수의 빈도분석
hist(soon$cost) # 연속형 변수의 히스토그램 시각화
plot(soon$cost) # 연속형 변수의 산점도 시각화
# 연속형 변수 범주화
soon$cost2[soon$cost >= 1 & soon$cost <= 3] <- 1
soon$cost2[soon$cost >= 4 & soon$cost <= 6] <- 2
soon$cost2[soon$cost >= 7] <- 3
# 범주형 데이터 시각화
table(soon$cost2)
par(mfrow = c(1,2)) # 1행2열의 크기로 그래프 생성
barplot(table(soon$cost2))
pie(table(soon$cost2))

# 패키지를 이용한 비대칭도 구하기
# 왜도와 첨도 사용을 위한 패키지 설치
install.packages('moments')
library(moments)
cst <- soon$cost
cst
skewness(cst) # 왜도 구하기
kurtosis(cst) # 첨도 구하기
# 히스토그램으로 왜도 첨도 확인
hist(cst)
par(mfrow = c(1,1))

# 히스토그램과 정규분포 곡선 그리기
hist(cst, freq = F)
lines(density(cst), col = 'blue')
x <- seq(0, 8, 0.1)
curve(dnorm(x, mean(cst),sd(cst)), col = 'red', add = T)

attach(data)
length(cost)
summary(cost)
mean(cost)
min(cost)
max(cost)
range(cost)
sd <- sd(cost, na.rm = T)
sqrt(var(cost, na.rm = T))
sd(cost, na.rm = T)
detach(data)

test <- c(1:5, NA, 10:20)
test
min(test)
max(test)
range(test)
mean(test)

min(test, na.rm = T)
max(test, na.rm = T)
range(test, na.rm = T)
mean(test, na.rm = T)

# 기술통계량 보고서 작성
data$resident2[data$resident==1] <- '특별시'
data$resident2[data$resident>=2 & data$resident<=4] <- '광역시'
data$resident2[data$resident==5] <- '시구군'

x <- table(data$resident2)
x
prop.table(x)
y <- prop.table(x)
round(y*100, 2)

data$gender2[data$gender==1] <- '남자'
data$gender2[data$gender==2] <- '여자'

x <- table(data$gender2)
x
prop.table(x)
y <- prop.table(x)
round(y*100, 2)

data$age2[data$age <= 45] <- '중년층'
data$age2[data$age >= 46 & data$age <= 59] <- '장년층'
data$age2[data$age >= 60] <- '노년층'

x <- table(data$age2)
x
prop.table(x)
y <- prop.table(x)
round(y*100, 2)

data$level2[data$level==1] <- '고졸'
data$level2[data$level==2] <- '대졸'
data$level2[data$level==3] <- '대학원졸'

x <- table(data$level2)
x
prop.table(x)
y <- prop.table(x)
round(y*100, 2)

data$pass2[data$pass==1] <- '합격'
data$pass2[data$pass==2] <- '실패'

x <- table(data$pass2)
x
prop.table(x)
y <- prop.table(x)
round(y*100, 2)

# 데이터 가져오기 및 전체 데이터 보기

getwd()
setwd('C:/Temp/Rwork/dataset3/dataset3')
setwd('E:/BigDate/R programing/dataset3/dataset3')
getwd()

das <- read.csv('dataset.csv', header = T)
das
head(das) # 데이터 앞부분 보기
tail(das) # 데이터 뒷부분 보기

print(das)
View(das)

# 데이터 셋 구조 보기

names(das)
attributes(das)
str(das)

# 데이터 셋 특정 변수 조회

das$age
das$resident
das$price
length(das$age)
length(das$price)

x <- das$gender # 특정 변수 조회결과를 변수에 저장
y <- das$price
x
y

plot(das$price) # 산점도 그래프로 변수 조회

das['gender'] # 컬럼명사용 특정 변수 조회
das['price']

das[2] # index사용 특정 변수 조회
das[6]
das[3, ]
das[, 3]

das[c('job', 'price')] # 두개이상 컬럼 조회
das[c(2, 6)]
das[c(1, 2, 3)]
das[c(2, 4:6, 3, 1)]

das[, c(2:4)] # 2~4열의 모든 행 조회
das[c(2:4), ] # 2~4행의 모든 열 조회
das[-c(1:100),] # 1~100행을 제외한 나머지 행의 모든열 조회

# 결측치 확인
summary(das$price)
sum(das$price) # NA값으로 인한 NA도출
sum(das$price, na.rm = T) # NA값 제외 합산결과

price2 <- na.omit(das$price) # NA값 제거 함수
sum(price2)
length(price2)
# na.omit() 함수는 특정 칼럼의 결측치를 제거한다.
# 결측치를 0으로 대체
x <- das$price
x[1:30]
das$price2 = ifelse(!is.na(x), x, 0)
das$price2[1:30]
# 결측치 평균으로 대체
y <- das$price
y[1:30]
das$price3 = ifelse(!is.na(x), x, round(mean(x, na.rm = T), 2))
das$price3[1:30]
das[c('price', 'price2', 'price3')]

# 극단치 처리
# 극단치 = 이상치(outlier)
table(das$gender)
pie(table(das$gender))
# subset()을 이용한 데이터 정제
das1 <- subset(das, gender == 1 | gender == 2)
das1
length(das1)
pie(table(das1$gender))
pie(table(das1$gender), col = c('red', 'blue'))
# 연속형 변수의 극단치 처리
daset <- read.csv('dataset.csv', header = T)
daset$price
length(daset$price)
plot(daset$price)
summary(daset$price)
# price 변수의 데이터 정제 및 시각화
daset1 <- subset(daset, price >= 2 & price <= 8)
length(daset1$price)
stem(daset1$price) # 나무줄기형태
# age 변수의 데이터 정제 및 시각화
# age 변수의 NA발견
summary(daset1$age)
length(daset1$age)
# age 변수 정제
daset2 <- subset(daset1, age >= 20 & age <= 69)
length(daset2)
# box플로팅으로 평균 연령 분석
boxplot(daset2$age)
# 극단치를 찾기 어려운 경우
# boxplot 통계를 이용한 극단치 처리
# boxplot으로 price 극단치 시각화
boxplot(daset$price)
# 극단치 통계 확인
boxplot(daset$price)$stats
# 극단치 제거한 서브 셋 만들기
daset_sub <- subset(daset, price >= 2 & price <= 7.9)
summary(daset_sub$price)

# 코딩변경
das$resident1[das$resident == 1] <- '1. 서울특별시'
das$resident1[das$resident == 2] <- '2. 인천광역시'
das$resident1[das$resident == 3] <- '3. 대전광역시'
das$resident1[das$resident == 4] <- '4. 대구광역시'
das$resident1[das$resident == 5] <- '5. 시구군'
das[c('resident', 'resident1')]

das$job1[das$job == 1] <- '공무원'
das$job1[das$job == 2] <- '회사원'
das$job1[das$job == 3] <- '개인사업'
das[c('job', 'job1')]
# 척도 변경을 위한 코딩 변경
das$age1[das$age <= 30] <- '쳥년층'
das$age1[das$age > 30 & das$age <= 55] <- '중년층'
das$age1[das$age > 55] <- '장년층'
head(das)
# 역코딩을 위한 코딩 변경
survey <- das$survey
csurvey <- 6 - survey
csurvey

das$survey <- csurvey
head(das)

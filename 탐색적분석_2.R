# 변수간의 관계 분석

# 범주형 VS 범주형 데이터 분포 시각화
getwd()
setwd('C:/Temp/Rwork/dataset3/dataset3')
setwd('E:/BigDate/R programing/dataset3/dataset3')
soongyu <- read.csv('new_data.csv', header = T)
str(soongyu)
# 변경된 거주지역과 성별 대상으로 빈도수 구하기
resident_gender <- table(soongyu$resident2, soongyu$gender2)
resident_gender
gender_resident <- table(soongyu$gender2, soongyu$resident2)
gender_resident
# 성별에 따른 거주지역 분포 현황 시각화
barplot(resident_gender, beside = T, horiz = T,
        col = rainbow(5),
        legend = row.names(resident_gender),
        main = '성별에 따른 거주지역 분포 현황')
# 거주지역에 따른 성별 분포 현황 시각화
barplot(gender_resident, beside = T,
        col = rep(c(2, 4), 5), horiz = T,
        legend = c('남자', '여자'),
        main = '거주지역별 성별 분포 현황')

# 연속형 VS 범주형 데이터의 시각화
install.packages('lattice')
library(lattice)
# 직업유형에 따른 나이 분포 현황
densityplot(~ age, data = soongyu,
            groups = job2,
            # plot.points = T : 밀도, auto.key = T : 범례
            plot.points = T, auto.key = T)

# 연속형 VS 범주형 VS 범주형 데이터 분포 시각화
# 성별에 따른 직급별 구매비용
densityplot(~ price | factor(gender),
            data = soongyu,
            groups = position2,
            plot.points = T, auto.key = T)
# 직급에 따른 성별 구매비용
densityplot(~ price | factor(position2),
            data = soongyu,
            groups = gender2,
            plot.points = T, auto.key = T)
# 연속형(2개) VS 범주형(1개) 데이터 분포 시각화
xyplot(price ~ age | factor(gender2),
       data = soongyu)

# 파생변수
getwd()
userdata <- read.csv('user_data.csv', header = T)
table(userdata$house_type)
# 더미변수 생성
# 단독주택,다세대주택 = 0, 아파트,오피스텔 = 1
house_type2 <- ifelse(userdata$house_type == 1 |
                        userdata$house_type == 2, 0, 1)
house_type2[1:10]
# 파생변수 추가
userdata$house_type2 <- house_type2
head(userdata)

# 1:N 관계를 1:1관계 파생변수 생성
paydata <- read.csv('pay_data.csv', header = T)
head(paydata, 10)
table(paydata$product_type)
# 고객별 상품 유형에 따른 구매금액과 합계를 나타내는 파생변수 생성
library(reshape2)
product_price <- dcast(paydata, user_id ~ product_type,
                       sum, na.rm = T)
head(product_price, 3)
# 컬럼명 수정
names(product_price) <- c('user_id', '식료품(1)', '생필품(2)',
                          '의류(3)', '잡화(4)', '기타(5)')
head(product_price)

# 고객식별번호에 대한 지불유형 파생변수 생성
pay_price <- dcast(paydata, user_id ~ pay_method, length)
head(pay_price, 3)
# 컬럼명 변경
names(pay_price) <- c('user_id', '현금(1)', '직불카드(2)',
                      '신용카드(3)', '상품권(4)')
head(pay_price, 3)

# 고객정보 테이블에 파생변수 추가
# 고객정보 테이블과 고객별 상품 유형에 따른 구매금액 합계 병합
library(plyr)
user_pay_data <- join(userdata, product_price, by = 'user_id')
head(user_pay_data, 10)

# 고객별 지불유형에 따른 구매상품 갯수 병합
user_pay_data <- join(user_pay_data, pay_price, by = 'user_id')
user_pay_data[c(1:10), c(1,7:15)]
# 사칙연산으로 총 구매금액 파생변수 생성
# 고객별 구매금액의 합계
user_pay_data$총구매금액 <- user_pay_data$`식료품(1)`+
  user_pay_data$`생필품(2)`+
  user_pay_data$`의류(3)`+
  user_pay_data$`잡화(4)`+
  user_pay_data$`기타(5)`
user_pay_data$총구매금액
head(user_pay_data$총구매금액)
# 고객별 상품 구매 총금액 컬럼 확인
user_pay_data[c(1:10), c(1,7:11,16)]

# 표본추출
# 정제된 데이터 저장
print(user_pay_data)
setwd('C:/rwork/')
write.csv(user_pay_data, 'cleanData.csv', quote = F, row.names = F)

data <- read.csv('cleanData.csv', header = TRUE)
data

# 표본 샘플링
# 표본 추출하기
nrow(data)
choice1 <- sample(nrow(data), 30)
choice1
# 50~(data길이) 사이에서 30개 행을 무작위 추출
choice2 <- sample(50:nrow(data), 30)
choice2
# 50~100 사이에서 30개 행을 무작위 추출
choice3 <- sample(c(50:100), 30)
choice3
# 다양한 범위를 지정하여 무작위 샘플링
choice4 <- sample(c(10:50, 80:150, 160:190), 30)
choice4
# 샘플링 데이터로 표본 추출
data[choice1, ]

# iris 데이터 셋의 관측치와 컬럼수 확인
data('iris')
dim(iris)
# 학습데이터(70%), 검정데이터(30%) 비율로 데이터셋 구성 <- 홀드아웃
idx <- sample(1:nrow(iris), nrow(iris) * 0.7)
training <- iris[idx, ]
testing <- iris[-idx, ]
dim(training)
dim(testing)

# 데이터셋을 대상으로 K겹 교차 검정 데이터셋 생성
# 데이터 프레임 생성
name <- c('a', 'b', 'c', 'd', 'e', 'f')
score <- c(90, 85, 99, 75, 65, 88)
df <- data.frame(Name = name, Score = score)
df
# 교차 검증 위한 패키지 설치
install.packages('cvTools')
library(cvTools)
# cvTools() 함수 : K교차 검증 데이터 셋을 생성
cross <- cvFolds(n = 6, K = 3, R = 1, type = 'random')
cross
# K겹 교차 검증 데이터 셋 구조 보기
str(cross)
cross$which
# subsets 데이터 참조
cross$subsets[cross$which == 1, 1]
cross$subsets[cross$which == 2, 1]
cross$subsets[cross$which == 3, 1]
# 데이터프레임 관측치 적용
r = 1
K = 1:3
for(i in K){
  datas_idx <- cross$subsets[cross$which == i,r]
  cat('K = ', i, '검정데이터 \n')
  print(df[datas_idx, ])
  
  cat('K = ', i, '훈련데이터 \n')
  print(df[-datas_idx, ])
}

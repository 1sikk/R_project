# reshape2 패키지활용
library(reshape2)
# 긴 형식의 데이터를 넓은 형식의 데이터로 변경
# dcast() 함수 활용
# 형식 dcast(데이터셋, 앞변수 ~뒤변수, 적용함수)
getwd()
setwd('E:/BigDate/R programing/dataset3/dataset3')
data <- read.csv('data.csv')
data
wide <- dcast(data, Customer_ID ~ Date, sum)
wide
# 실습
write.csv(wide, 'wide.csv', row.names = FALSE)
wide <- read.csv('wide.csv')
colnames(wide) <- c('Customer_ID', 'day1', 'day2', 'day3',
                    'day4', 'day5', 'day6', 'day7')
wide

# 넓은 형식의 데이터를 긴 형식의 데이터로 변경
# melt() 함수 활용
# 형식: melt(데이터셋, id=”컬럼명”)
long <- melt(wide, id = 'Customer_ID')
long
# 컬럼명 변경
name <- c('Customer_ID', 'Date', 'Buy')
colnames(long) <- name
head(long)

# 실습
data('smiths')
smiths
# 넓은 형식 smiths데이터를 긴 형식으로 변경
long <- melt(id = 1:2, smiths)
long
# 긴 형식을 넓은 형식으로 변경
# subject 와 time 컬럼 기준
dcast(long, subject + time ~ ...)

# 3차원 배열형식으로 변경
# dcast()함수: 데이터프레임 형식으로 구조를 변경
# acast()함수: 3차원 구조를 갖는 배열형태로 변경
# 실습
data('airquality')
str(airquality)
airquality
# 컬럼제목 대문자로 변경
names(airquality) <- toupper(names(airquality))
head(airquality)
# melt() 함수 이용 넓은 형식 긴 형식으로 변경
air_melt <- melt(airquality, id = c('MONTH', 'DAY'), na.rm = TRUE)
head(air_melt)
# acast() 함수 이용 3차원 구조 변경
names(air_melt) <- tolower(names(air_melt)) # 소문자 변경
head(air_melt)
acast <- acast(air_melt, day ~ month ~ variable)
acast
class(acast)
# 집합함수 적용하기
acast(air_melt, month ~ variable, sum, margins = TRUE)
# acast()함수: month 컬럼을 기준으로 측정변수의 합계 계산
# margin=TRUE 속성: 각 행과 열의 합계(all) 컬럼 추가
# 3차 구조인 air_melt데이터셋에 dcast()함수 적용 시 데이터프레임 형태로 구조가 변경
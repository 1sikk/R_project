##211105 
library(dplyr)
getwd()
setwd("c:/Rwork/Data")
csvgrade <- read.csv("grade_csv.csv")
csvgrade %>% group_by(class) %>% summarise(mean_math = mean(math))

#컬럼의 집단별 다중요약 통계량
csvgrade %>% group_by(class) %>% summarise(mean_math = mean(math),
                                           sum_math  = sum(math),
                                           median_math = median(math))

#컬럼그룹화해서 변수에 담기

speicies <- group_by(iris,Species)
str(speicies)
speicies

## 데이터 프레임 병합 ##

a <- data.frame(id = c(1,2,3,4,5),score = c(60,80,70,90,85))
b <- data.frame(id = c(3,4,5,6,7),weight= c(80,90,85,60,85))

# id 값이 일치하는 열만 결합

merge(a,b ,by = "id")
inner_join(a,b,by ="id")

###실습
df1 <- data.frame(x = 1:5, y = rnorm(5))
df2 <- data.frame(x = 2:6, z = rnorm(5))

#inner join하기

inner_join(df1,df2,by = "x")

# left join 하기
df1;df2
left_join(df1,df2,by = "x")
#right join 하기
right_join(df1,df2,by = "x")
#full join하기
full_join(df1,df2,by = "x")

##데이터 프레임 두개를 행단위로 합치기

df1 <- data.frame(x = 1:5 , y = rnorm(5))
df2 <- data.frame(x = 6:10, y = rnorm(5))

df_rows <-bind_rows(df1,df2)
df_rows

## resahpe2 패키지 설치 

install.packages("reshape2")
data <- read.csv("data.csv")
data
library(reshape2)

#와이드 포맷으로 변경
wide <- dcast(data,Customer_ID ~Date,sum)
wide

#번호없이  저장
write.csv(wide, "wide.csv", row.names = FALSE)

wide <-  read.csv("wide.csv")
# 컬럼네임 설정
colnames(wide) <- c('Customer_ID', 'day1', 'day2', 'day3',
'day4', 'day5', 'day6', 'day7')
wide

#넓은데이터 긴형식으로 변경

long <- melt(wide,id = "Customer_ID")
long
# 컬럼 네임 설정
name = c("Customer_ID","Date","Buy")
colnames(long) <- name
head(long)

# 와이드 포맷인 smiths 데이터를 긴형식으로 변경하기

long <- melt(id = 1:2,smiths)
long

# 데이터 셋의 구조변경

data('airquality')
str(airquality)
airquality

# 컬럼 네임을 대문자로 변경하기
names(airquality) <- toupper(names(airquality))
head(airquality)

# melt함수로 와이드 폼으로 변경
air_melt <- melt(airquality,id = c("MONTH","DAY"), na.rm= TRUE)
air_melt

#소문자로 변경
names(air_melt) <- tolower(names(air_melt))
#3차원배열
acast <-  acast(air_melt, day ~ month ~ variable)
acast
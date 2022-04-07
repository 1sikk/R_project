install.packages('dplyr')
library(dplyr)

# %>% 파이프 연산자를 이용한 함수 적용
# 형식 : dataframe %>% 함수1() %>% 함수2()
# %>% 연산자는 인수를 함수에 편하게 적용할 수 있다.
# %>% 연산자의 >(라이트 앵글 브래킷) 기호는 방향의 의미로 왼쪽에 있는 인자를
# 오른쪽에 있는 함수에 집어넣는 것이 파이프라인의 기능이다.
# %>% 연산자를 사용하면 여러 가지 함수를 한 번에 사용할 수 있다.
# %>% 함수의 큰 장점은 한 번에 한 줄로 코드를 사용할 수 있으므로 함수를 사용할
# 때마다 따로 저장하는 과정이 없이 여러 함수를 실행할 수 있는 편리성에 있다.
# %>% 연산자에서 왼쪽 %(퍼센트)를 LHS(Left Hand Side) 변수라고 하며 오른쪽 %(퍼센트)를
# RHS(Right Hand Side) 변수라고 한다.

getwd()
setwd('E:/BigDate/R programing/dataset3/dataset3')
csvgrade <- read.csv('grade_csv.csv')
csvgrade %>% head() %>% summary()

iris %>% head()
iris %>% head() %>% subset(Sepal.Length >= 5.0)

# %>% 함수의 도트(.) 연산자
# 도트(.) 연산자를 사용하여 LHS가 들어갈 위치를 정할 수 있다.
# 도트(.) 연산자는 저장한 인자의 이름을 다시 쓰지 않고 데이터를 사용할 수 있다.
# 1:5의 인자를 다시 쓰지 않고 사용할 수 있으며 sum 함수로 호출한다.
1:5 %>% sum(.)
# length 함수에 의해서 길이인 5까지 더해져서 20을 반환한다.
1:5 %>% sum(length(.))
# sum(length(1:5))와 같이 수열을 지정하여 반환하므로 수열의 길이인 5까지 더해서
# 20을 반환한다.
5 %>% sum(1:.)
# { }(브레이스)를 적용하면 함수를 한 번씩만 실행한다.
5 %>% {sum(1:.)}
# csvgrade <- read.csv("grade_csv.csv")
# 1부터 행의 수만큼 수열로 출력하고 나눈 나머지가 0인 행의 부분집합을 추출한다.
csvgrade %>% subset(1:nrow(.) %% 2 == 0)

install.packages('hflights')
library(hflights)
str(hflights)

hflights_df <- tbl_df(hflights)
hflights_df

# tbl_df()함수: 현재 R의 console창 크기에서 볼수 있는 만큼 결과를 나타내고, 나머지는
# 아래에 생략된 행 수와 컬럼명 표시

hflights <- hflights %>% tbl_df()
hflights

# 조건에 맞는 데이터 필터링
# filter 함수는 조건에 해당하는 데이터의 모든 컬럼을 추출한다.
# filter(dataframe, 조건1, 조건2)
csvgrade <- read.csv('grade_csv.csv')
csvgrade %>% filter(class == 1) # == 연산자 이용 객체의 특정값 호출
csvgrade %>% filter(class != 1) # != 연산자 이용 객체의 특정값이 아닌 나머지 값 호출
csvgrade %>% filter(math > 50) # > 연산자 이용 객체의 특정값 초과인 값 호출
csvgrade %>% filter(math < 50) # < 연산자 이용 객체의 특정값 미만인 값 호출
csvgrade %>% filter(eng >= 80) # >= 연산자 이용 객체의 특정값 이상인 값 호출
csvgrade %>% filter(eng <= 80) # <= 연산자 이용 객체의 특정값 이하인 값 호출
# & 연산자는 객체를 평가하고 객체 모두 TRUE일 때 TRUE로 평가하고 그렇지 않으면
# FALSE로 평가한다.
# & 연산자를 적용하여 여러 조건을 충족하는 행을 추출한다.
csvgrade %>% filter(class == 1 & math >= 50)
csvgrade %>% filter(eng < 90 | sci < 50) # 영어점수 90점미만 or 과학점수 50점미만 학생 호출
csvgrade %>% filter(class == 1 | class == 3 | class == 5)
# %in% 연산자
# %in% 연산자는 값의 포함 여부를 확인하고 반환한다.
# %in% 연산자는 여러 객체에서 여러 조건을 줄 때는 사용할 수 없다.
csvgrade %>% filter(class %in% c(1,3,5))
# 객체 생성
class1 <- csvgrade %>% filter(class == 1)
mean(class1$math)
mean(class1$eng)
mean(class1$sci)
# 특정일 데이터 추출하기
filter(hflights_df, Month == 1 & DayofMonth == 2) # 1월2일 데이터 추출
hflights_df %>% filter(Month == 1 & DayofMonth == 1)
filter(hflights_df, Month == 1 | Month == 2) # 1월 또는 2월 데이터 추출
# 오름차순 정렬, 낮은수 부터 높은수로
csvgrade %>% arrange(math)
# 내림차순 정렬, 높은수 부터 낮은수로
csvgrade %>% arrange(desc(math))
# 다중 객체의 오름차순 정렬
csvgrade %>% arrange(class, math)
arrange(hflights_df, Year, Month, DepTime, ArrTime)
hflights_df %>% arrange(Year, Month, DepTime, AirTime)

# 컬럼으로 데이터 검색
# 데이터 셋의 특정 컬럼을 기준으로 데이터 검색 시 select()함수 사용
# 형식: select(dataframe, 컬럼1, 컬럼2, …)
# select 함수는 추출하고자 하는 객체를 할당하면 해당 객체만 추출한다.
csvgrade %>% select(math)
# 다중객체 추출
csvgrade %>% select(class, math)
# 객체 제외
csvgrade %>% select(-math)
# 특정 객체의 값 추출
csvgrade %>% filter(class == 1) %>% select(eng)
# 특정 객체의 값 일부를 추출
csvgrade %>% select(id, math) %>% head(3)
# hflights_df를 대상으로 지정된 컬럼 데이터 검색
select(hflights_df, Year, Month, DepTime, ArrTime)
hflights_df %>% select(hflights_df, Year, Month, DepTime, AirTime)
# 실습 (hflights_df 대상 컬럼의 범위로 검색)
select(hflights_df, Year:ArrTime)

# 데이터셋 컬럼 추가
# 데이터 셋에 특정 컬럼을 추가하는 mutate()함수
# 형식: mutate(dataframe, 컬럼명1=수식1, 컬럼명2=수식2, …)
# hflights_df에서 출발 지연시간과 도착 지연시간의 차이를 계산한 컬럼 추가
mutate(hflights_df, gain = ArrTime - DepTime,
       gain_per_hour = gain / (AirTime/60))
hflights_df %>% mutate(gain = ArrDelay - DepDelay, gain_per_hour = gain / (AirTime/60))
# 추가된 컬럼 보기
select(mutate(hflights_df, gain = ArrDelay - DepDelay,
              gain_per_hour = gain / (AirTime / 60)),
       Year, Month, ArrDelay, DepDelay, gain, gain_per_hour)
# 요약통계 summary()함수와 혼동하지말것
# 데이터 셋에 특정 컬럼을 대상으로 기술통계량을 계산하는 summarise() 함수
# 형식: summarise(dataframe, 추가할 컬럼명 = 함수(컬럼명), …)
csvgrade %>% summarise(mean_math = mean(math))
# hflights_df에서 비행시간의 평균 구하기
summarise(hflights_df, angAirTime = mean(AirTime, na.rm = TRUE))
# hflights_df %>% summarise(avgAirTime = mean(AirTime, na.rm = TRUE))
# mean()함수 사용 평균 계산 후 avgAirTime 변수에 저장
hflights_df %>% summarise(avgAirTime = mean(AirTime, na.rm = TRUE))
# hflights_df 의 관측치 길이 구하기
summarise(hflights_df, cnt = n(), 
          delay = mean(AirTime, na.rm = TRUE))
# 도착시간(AirTime)의 표준편차와 분산 계산
summarise(hflights_df, arrTimeSd = sd(ArrTime, na.rm = TRUE),
          arrTimeVar = var(ArrTime, na.rm = T))

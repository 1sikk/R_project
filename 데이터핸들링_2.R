getwd()
setwd('C:/Temp/Rwork/dataset3/dataset3')
# 데이터프레임 병합
# inner_join()함수
# 형식: inner_join(dataframe1, dataframe2, 공통변수)
# inner_join(A, B, by = key) 
# A 매개변수: 객체를 설정한다.
# B 매개변수: 객체를 설정한다.
# by 옵션: 결합할 기준이 되는 칼럼을 설정한다.
library(dplyr)
a <- data.frame(id = c(1,2,3,4,5), score = c(60,80,70,90,85))
b <- data.frame(id = c(3,4,5,6,7), weight = c(80,90,85,60,85))
inner_join(a,b,by = 'id')
merge(a,b,by = 'id')

# 실습
# 데이터생성
df1 <- data.frame(x = 1:5, y = rnorm(5))
df2 <- data.frame(x = 2:6, z = rnorm(5))
df1
df2
# inner_join하기
inner_join(df1, df2, by = 'x')
# left_join() -> 키를 기준 왼쪽 열을 결합
# 형식: left_join(dataframe1, dataframe2, 공통변수)
# left_join(A, B, by = key) 
left_join(a,b,by = 'id')
merge(a,b,by = 'id',all.x = T)
# 실습
left_join(df1, df2, by = 'x')
# right_join() -> 키를 기준 오른쪽 열을 결합
# 형식: right_join(dataframe1, dataframe2, 공통변수)
# right_join(A, B, by = key) 
right_join(a,b,by = 'id')
merge(a,b,by = 'id', all.y = T)
# 실습
right_join(df1, df2, by = 'x')
# full_join() -> 키를 기준으로 모든 열을 결합
# 형식: full_join(dataframe1, dataframe2, 공통변수)
# full_join(A, B, by = key)
full_join(a,b,by = 'id')
merge(a,b,by = 'id', all = T)
# 실습
full_join(df1, df2, by = 'x')

# 데이터 프레임 합치기
# 세로결합 bind_rows()
c <- data.frame(id = c(1,2,3,4,5), score = c(60,80,70,90,85))
d <- data.frame(id = c(3,4,5,6,7), weight = c(80,90,85,60,85))
bind_rows(a,b)
# 실습
df3 <- data.frame(x= 1:5, y=rnorm(5))
df4 <- data.frame(x=6:10, y=rnorm(5))
df3
df4
df_rows <- bind_rows(df3, df4)
df_rows
# 가로 결합 bind_cols()
bind_cols(a,b)
df_cols <- bind_cols(df3, df4)
df_cols

# rbind()함수
lee <- data.frame(id = c(1, 2, 3, 4, 5), score = c(60, 80, 70, 90, 85))
lee
gyu <- data.frame(id = c(6, 7 , 8), score = c(80, 90, 85))
gyu
rbind(lee, gyu)
# cbind()함수
lee1 <- data.frame(id = c(1, 2, 3, 4, 5), score = c(60, 80, 70, 90, 85))
lee1
gyu1 <- data.frame(age = c(20, 19 , 20, 19, 21), weight = c(80, 90, 85, 60, 85))
gyu1
cbind(lee1, gyu1)
lee2 <- data.frame(id = c(1, 2, 3, 4, 5), score = c(60, 80, 70, 90, 85))
lee2
gyu2 <- data.frame(id = c(3, 4 , 5, 6, 7), weight = c(80, 90, 85, 60, 85))
gyu2
cbind(lee2, gyu2)

# merge() -> 키를 기준으로 열을 결합하여 반환
# 형식: merge(A, B, by = key, all = FALSE, all.x = all, all.y = all) 함수
# A 매개변수: 객체를 설정한다.
# B 매개변수: 객체를 설정한다.
# by 옵션: 결합할 기준이 되는 칼럼을 설정한다.
# all 옵션: 모든 열을 결합하며 기본값은 FALSE다.
# all.x 옵션: A 객체를 기준으로 열을 결합한다.
# all.y 옵션: B 객체를 기준으로 열을 결합한다.

# 컬럼명 수정하기
# rename() 함수
df5 <- data.frame(one = c(4,3,8))
df5
df5 <- rename(df5, '원' = one)
df5
# 실습
df_rename <- rename(df_cols, x2 = x...1)
df_rename <- rename(df_rename, y2 = y...2)
df_rename

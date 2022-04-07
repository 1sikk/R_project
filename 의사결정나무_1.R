# 의사결정나무(Decision Tree)
# 1. 고속도로 주행거리에 미치는 영향변수
# 1-1 패키지 설치 및 로딩
library(ggplot2)
data(mpg)

# 1-2 학습데이터와 검정데이터 생성
t <- sample(1:nrow(mpg), 120)
train <- mpg[-t, ]
test <- mpg[t, ]
dim(train)
dim(test)

# 1-3 formula 작성과 분류모델 생성
test$drv <- factor(test$drv)
formula <- hwy ~ displ+ cyl + drv
tree_model <- ctree(formula, data = test)
windows()
plot(tree_model)

# 2. Adultuci 데이터셋을 이용한 분류분석
# 2-1 패키지 설치 및 데이터 셋 구조보기
install.packages('arules')
library(arules)
data(AdultUCI)
str(AdultUCI)
names(AdultUCI)

# 2-2 데이터 샘플링
set.seed(1234)
choice <- sample(1:nrow(AdultUCI), 10000)
choice

adult.df <- AdultUCI[choice, ]
str(adult.df)

# 2-3 변수 추출 및 데이터 프레임 생성
# 2-3-1 변수 추출
capital <- adult.df$`capital-gain`
hours <- adult.df$`hours-per-week`
education <- adult.df$`education-num`
race <- adult.df$race
age <- adult.df$age
income <- adult.df$income

# 2-3-2 데이터 프레임 생성
adult.df <- data.frame(capital = capital, age = age, race = race,
                       hours = hours, education = education, income = income)
str(adult.df)

# 2-4 formula생성 - 자본이득(capital)에 영향을 미치는 변수
formula <- capital ~ income + education + hours + race + age

# 2-5 분류모델 생성 및 예측
adult_ctree <- ctree(formula, data = adult.df)
adult_ctree

# 2-6 분류모델 플로팅
windows()
plot(adult_ctree)

# 2-7 자본이득 요약 통계량 보기
adultResult <- subset(adult.df,
                      adult.df$income == 'large'&
                        adult.df$education > 14)
length(adultResult$education)
summary(adultResult$capital)

windows()
boxplot(adultResult$capital)

# 3. 조건부추론나무
install.packages('party')
library(party)

# 3-1 샘플링
str(iris)
set.seed(1000)
sampnum <- sample(2, nrow(iris), replace = TRUE, prob = c(0.7,0.3))
sampnum

# 3-2 training & testing data 구분
trData <- iris[sampnum == 1, ]
head(trData)

teData <- iris[sampnum == 2, ]
head(teData)

shortvar <- Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width

# 학습
citreeResult <- ctree(shortvar, data = trData)

# 예측값과 실제값 비교
table(predict(citreeResult), trData$Species)
citreeResult2 <- ctree(shortvar, data = teData)

# 테스트 데이터를 이용하여 분류
forcasted <- predict(citreeResult2, data = teData)

forcasted
teData$Species

# 예측결과와 실제값 비교
table(forcasted, teData$Species)

windows()
plot(citreeResult2)

# 결과해석
# 종(Species) 판단
# Petal.Length <= 1.9 : setosa로 판단
# Petal.Length > 1.9 & Petal.Width <= 1.6 : versicolor로 판단
# 나머지: virginica 로 판단

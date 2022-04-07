# 앙상블 기법
# 1. 배깅(Bagging)
library(party)
library(caret)

# 1-1 데이터 샘플링
data1 <- iris[sample(1:nrow(iris), replace = T),]
data2 <- iris[sample(1:nrow(iris), replace = T),]
data3 <- iris[sample(1:nrow(iris), replace = T),]
data4 <- iris[sample(1:nrow(iris), replace = T),]
data5 <- iris[sample(1:nrow(iris), replace = T),]

# 1-2 예측모형 생성
citree1 <- ctree(Species ~ ., data1)
citree2 <- ctree(Species ~ ., data2)
citree3 <- ctree(Species ~ ., data3)
citree4 <- ctree(Species ~ ., data4)
citree5 <- ctree(Species ~ ., data5)

# 1-3 예측수행
predicted1 <- predict(citree1, iris)
predicted2 <- predict(citree2, iris)
predicted3 <- predict(citree3, iris)
predicted4 <- predict(citree4, iris)
predicted5 <- predict(citree5, iris)

# 1-4 예측모형과 결합하여 새로운 예측모형 생성
newmodel <- data.frame(Species = iris$Species, predicted1, predicted2, predicted3, predicted4, predicted5)
head(newmodel)
newmodel

# 1-5 최종모형으로 통합
funcValue <- function(x){
  result <- NULL
  for(i in 1:nrow(x)){
    xtab <- table(t(x[i, ]))
    rvalue <- names(sort(xtab, decreasing = T)[1])
    result <- c(result, rvalue)
  }
  return(result)
}

# 1-6 최종모형의 2번째에서 6번째 통합하여 최종 결과 생성
newmodel$result <- funcValue(newmodel[, 2:6])
newmodel$result

# 1-7 최종결과 비교
table(newmodel$result, newmodel$Species)

# 2. adabag 이용한 배깅
install.packages('adabag')
library(adabag)

data(iris)
iris.bagging <- bagging(Species ~ ., data = iris, mfinal = 10) # mfinal = 트리 반복 생성 횟수
iris.bagging$importance # 당연하지만 매번 수치는 달라질수 있음

windows()
iris.bagging$trees
# 10번째의 값을 시각화
plot(iris.bagging$trees[[10]])
text(iris.bagging$trees[[10]])
pred <- predict(iris.bagging, newdata = iris)
table(pred$class, iris[,5])

# 3. 부스팅
library(adabag)
data(iris)
boostadabag <- boosting(Species ~ ., data = iris, boos = TRUE, mfinal = 10)
boostadabag$importance # 돌릴때 마다 값이 다름.


windows()
plot(boo.adabag$trees[[10]])
text(boo.adabag$trees[[10]])
pred <- predict(boostadabag, newdata = iris)
pred$class <- as.factor(pred$class)
confusionMatrix(pred$class, iris[,5])


error.rpart <- 1-(sum(diag(tb))/sum(tb)) # 오분류율 계산
error.rpart

# 4. 신경망 모형 대입
install.packages('ada')
library(ada)
data(iris)
iris[iris$Species!='setosa',] -> iris # stsosa 50개 자료 제외
iris
n <- dim(iris)[1]

trind <- sample(1:n, floor(.6*n), FALSE)
teind <- setdiff(1:n, trind) # set difference(차집합)
iris[,5] <- as.factor((levels(iris[, 5])[2:3])[as.numeric(iris[,5])-1])
# as.numeric 부터 0,1,2가 차례대로 50개(총 150개)

gdis <- ada(Species ~., data = iris[trind,], iter = 20, nu = 1, type = 'discrete')
# nu = 1은 부스팅을 위한 축소 모수 / type = 'discrete'는 부스팅 알고리즘 지정 'real', 'gentle' 부스팅
gdis <- addtest(gdis, iris[teind, -5], iris[teind, 5])
gdis # 값은 돌릴때마다 변함

windows()
plot(gdis , TRUE, TRUE)

windows()
varplot(gdis)

windows()
pairs(gdis, iris[trind, -5], maxvar = 4)
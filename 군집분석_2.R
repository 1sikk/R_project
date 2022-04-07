# 유클리디안 거리
# 1. 유클리디안 거리 계산법
# 1-1 matrix 객체 생성
rm(list = ls())
x <- matrix(1:9, nrow = 3, by = T)
x

# 1-2 유클리디안 거리 생성
dist <- dist(x, method = 'euclidean')
dist

# 1-3 1행과 2행 변량의 유클리디안 거리 구하기
s <- sum((x[1, ] - x[2, ])^ 2)
sqrt(s)

# 1-4 1행과 3행 변량의 유클리디안 거리 구하기
s <- sum((x[1, ] - x[3, ])^ 2)
sqrt(s)

# 2. 유클리디안 거리를 이용한 군집화
install.packages('cluster')
library(cluster)

# 2-1 데이터 생성
x <- matrix(1:9, nrow = 3, by = T)
x

# 2-2 matrix 객체 대상 유클리디안 거리 생성
dist <- dist(x, method = 'euclidean')

# 2-3 유클리디안 거리 matrix를 이용한 군집화
hc <- hclust(dist)

# 2-4 클러스트 시각화
windows()
plot(hc)

# 3. 신입사원의 면접시험 결과를 군집 분석
getwd()
setwd('E:/BigDate/R programing/dataset4/dataset4')
install.packages('RDX3')
interview <- read.csv('interview.csv', header = TRUE)
names(interview)
head(interview)
data(wine)
# 3-1 유클리디안 거리계산
interview_df <- interview[c(2:7)]
idist <- dist(interview_df)
head(interview_df)

# 3-2 계층적 군집 분석
hc <- hclust(idist)
hc

# 3-3 군집분석 시각화
plot(hc, hang = -1)
# hang = -1 속성을 이용 덴드로그램에서 음수값 제거

# 3-4 군집단위 테두리 생성
rect.hclust(hc, k = 3, border = 'red')

# 4. 군집별 특징 보기
# 4-1 군집별 서브 셋 만들기
g1 <- subset(interview, no == 108 | no == 110 | no == 107 | no == 112 | no == 115)
g2 <- subset(interview, no == 102 | no == 101 | no == 104 | no == 106 | no == 113)
g3 <- subset(interview, no == 105 | no == 114 | no == 109 | no == 103 | no == 111)

# 4-2 각 서브 셋의 요약통계량 보기
summary(g1)
summary(g2)
summary(g3)

# 5. 군집 수 자르기
# 5) iris 데이터셋을 대상으로 군집 수 자르기
# 5-1 유클리디안 거리 계산
idist <- dist(iris[1:4])
hc <- hclust(idist)
plot(hc, hang = -1)

# 5-2 군집 수 자르기
ghc <- cutree(hc, k = 3) # 그룹 수를 자르는 cutree()함수
ghc

# 5-3 iris데이터 셋에 ghc 컬럼 추가
iris$ghc <- ghc
table(iris$ghc)
head(iris)

# 5-4 요약통계량 구하기
g1 <- subset(iris, ghc == 1)
summary(g1[1:4])
g2 <- subset(iris, ghc == 2)
summary(g2[1:4])
g3 <- subset(iris, ghc == 3)
summary(g3[1:4])

# 6. 비게층적 군집분석
# k-means알고리즘에 군집 수를 적용하여 군집별로 시각화
# 6-1 군집 분석에 사용할 변수 추출
library(ggplot2)
data(diamonds)
t <- sample(1:nrow(diamonds), 1000)
test <- diamonds[t, ]
dim(test)
head(test)

mydia <- test[c('price', 'carat', 'depth', 'table')]
head(mydia)

# 6-2 계층적 군집 분석(탐색적 분석)
result <- hclust(dist(mydia), method = 'average')
result
plot(result, hang = -1)

# 6-3 비계층적 군집 분석
result2 <- kmeans(mydia, 3)
names(result2)
result2$cluster

mydia$cluster <- result2$cluster
head(mydia)

# 6-4 변수 간의 상관계수 보기
cor(mydia[, -5], method = 'pearson')
plot(mydia[, -5])

# 6-5 상관계수를 색상으로 시각화
install.packages('mclust')
install.packages('corrgram')
library(mclust)
library(corrgram)
corrgram(mydia[, -5], upper.panel = panel.conf)
corrgram(mydia[, -5], lower.panel = panel.conf)

# 6-6 비계층적 군집 시각화
plot(mydia$carat, mydia$price, col = mydia$cluster)
points(result2$centers[, c('carat', 'price')], col = c(3, 1, 2), pch = 8, cex = 5)

# 7. 계층적 군집법
# 7-1 iris 데이터셋 로딩(50개)
rm(list = ls())
idx <- sample(1:dim(iris)[1], 50)
idx
irisSample <- iris[idx, ]
head(irisSample)

# 7-2 species 데이터 제거
irisSample$Species <- NULL
head(irisSample)

# 7-3 계층적 군집법 실행
hc_result <- hclust(dist(irisSample), method = 'ave')
hc_result

# 7-4 군집 결과 시각화
plot(hc_result, hang = -1, labels = iris$Species[idx])
rect.hclust(hc_result, k = 4)

# 8. k 평균 군집법
# 8-1 iris 데이터셋 로딩
rm(list = ls())
data(iris)
iris
iris2 <- iris

# 8-2 species 데이터 제거
iris2$Species <- NULL
head(iris2)

# 8-3 k-means clustering 실행
kmeans_result <- kmeans(iris2, 6)
kmeans_result
str(kmeans_result)

# 8-4 군집 결과 시각화
plot(iris2[c("Sepal.Length", "Sepal.Width")], col=kmeans_result$cluster)
points(kmeans_result$centers[, c("Sepal.Length", "Sepal.Width")], col=1:4, pch=8, cex=2)
plot(iris2[c("Petal.Length", "Petal.Width")], col=kmeans_result$cluster)
points(kmeans_result$centers[, c("Petal.Length", "Petal.Width")], col=1:4, pch=8, cex=2)

# 8-5 군집의 수 결정
kmeans_result <- kmeans(iris2, 7)
plot(iris2[c("Sepal.Length", "Sepal.Width")], col=kmeans_result$cluster)
points(kmeans_result$centers[, c("Sepal.Length", "Sepal.Width")], col=1:4, pch=8, cex=2)

# 9. k 중심 군집법
# 9-1 패키지 설치
install.packages("fpc")
library(fpc) 

# 9-2 군집의 수 설정. 임의로 6개
pamk_result <- pamk(iris2, 5) 
pamk_result 

# 9-3 군집의 수 확인
pamk_result$nc 
table(pamk_result$pamobject$clustering, iris$Species) 

# 9-4 한 윈도우에서 그림을 여러개 그림
layout(matrix(c(1,2),1,2))
plot(pamk_result$pamobject)

# 10. 밀도기반 군집법
# 10-1 패키지 로딩
library(fpc)

# 10-2 species 컬럼 제거
iris2 <- iris[-5]
head(iris2)

# 10-3 밀도기반 군집
db_result <- dbscan(iris2, eps=0.42, MinPts=5)
db_result

# 10-4 시각화
plot(db_result, iris2)

# 10-5 자세히 보기
plot(db_result, iris2[c(1,4)])

# 10-6 군집 결과 보기
plotcluster(iris2, db_result$cluster)
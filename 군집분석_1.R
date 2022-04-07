# 군집분석
# 1. USArrets를 이용한 군집분석 (계층적 군집 수행)
data(USArrests)
str(USArrests)
d <- dist(USArrests, method = 'euclidean')
fit <- hclust(d, method = 'ave')

windows()
par(mfrow = c(1,2))
plot(fit)
plot(fit, hang = -1) # hang - 길이의 조절

par(mfrow = c(1,1))
groups <- cutree(fit, k = 6)
groups
windows()
plot(fit)
rect.hclust(fit, k = 6, border = 'red')

# 2. USArrets를 이용한 군집분석 (agnes()함수를 이용한 계층적 군집 수행)
install.packages('cluster')
library(cluster)
agn1 <- agnes(USArrests, metric = 'manhattan', stand = TRUE)
agn1
windows()
par(mfrow = c(1,2))
plot(agn1)
agn2 <- agnes(daisy(USArrests), diss = TRUE, method = 'complete')
windows()
par(mfrow = c(1,2))
plot(agn2)
agn3 <- agnes(USArrests, method = 'flexible', par.method = 0.6)
windows()
par(mfrow = c(1,2))
plot(agn3)

# K-평균 군집 kmeans()함수이용
# 1. rattle패키지 wine데이터 이용
getwd()
setwd('/Users/sik/Desktop/')
wine <- read.csv("wine.csv")
wine
install.packages('NbClust')
library(NbClust)
head(wine)
df <- scale(wine[-1])
wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")} 
windows()
wssplot(df) # 사용자 정의 함수
df
set.seed(1234)
nc <- NbClust(df, min.nc = 2, max.nc = 14, method = 'kmeans')
windows()
barplot(table(nc$Best.nc[1, ]),
        xlab = 'Number of Clusters', ylab = 'Number of Criteria',
        main = 'Number of Clusters Chosen by 26 Criteria')


set.seed(1234)
fit.km <- kmeans(df, 4, nstart = 25)
fit.km$size
fit.km$centers
windows()
plot(df, col = fit.km$cluster)
points(fit.km$centers, col = 1:3, pch = 8, cex = 1.5)
aggregate(wine[-1], by = list(cluster = fit.km$cluster), mean)

ct.km <- table(wine$Type, fit.km$cluster)
ct.km


install.packages('flexclust')
library(flexclust)
randIndex(ct.km)

# 2. flexclust패키지의 kcca()함수를 이용한 k-평균 군집화
data('Nclus')
windows()
plot(Nclus)

cl <- kcca(Nclus, k = 4, family = kccaFamily('kmeans'))
windows()
image(cl)
points(Nclus)

windows()
barplot(cl)

windows()
stripes(cl)

# 3. cclust패키지의 cclust()함수 이용한 k-평균 군집화
install.packages('cclust')
library(cclust)
cl.1 <- cclust(Nclus, 4, 20, method = 'kmeans')
windows()
plot(Nclus, col = cl.1$cluster)
points(cl.1$centers, col = 1:4, pch = 10, cex = 1.5)

library(cluster)
windows()
clusplot(Nclus, cl.1$cluster)


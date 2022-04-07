#### 20211123~24 텍스트마이닝  ####
rm(list = ls())
library(KoNLP)
library(tm)
library(wordcloud)
library(Sejong)
library(arules)
library(httr)
library(XML)

setwd("c:/Rwork/Data")
facebook <- file("facebook_bigdata.txt",encoding = "UTF-8")
facebook_data <- readLines(facebook)
# 세종 사전에 단어 추가하기
user_dic <- data.frame(term=c("R 프로그래밍","페이스북","홍길동","소셜네트워크"),
                       tag = 'ncn')
buildDictionary(ext_dic = "sejong",user_dic = user_dic)
#R 제공 함수로 단어 추출하기
paste(extractNoun('홍길동은 많은 사람과 소통을 위해서 소셜네트워크에 가입하셨습니다.'),
      collapse = " ")
# 단어추출을 사용자함수로 정의해서 편리하게 사용가능하다
exNouns <- function(x){paste(extractNoun(as.character(x)),
                             collapse = " ")}
# exNouns()를 이용하여 단어를 추출하기
facebook_nouns <- sapply(facebook_data, exNouns)
facebook_nouns[1]

#### 추출된 단어 대상으로 전처리하기####
# 1. 추출된 단어를 이용해 말뭉치 생성
myCorpus <- Corpus(VectorSource(facebook_nouns))
# 2. 문장부호 제거
myCorpusPrepro <-  tm_map(myCorpus,removePunctuation)
# 3. 수치제거
myCorpusPrepro <-  tm_map(myCorpusPrepro,removeNumbers)
# 4. 소문자 변경
myCorpusPrepro <- tm_map(myCorpusPrepro,tolower)
# 5. 불용어 제거
myCorpusPrepro <- tm_map(myCorpusPrepro,removeWords,stopwords('english'))
# 6. 전처리한 결과를 확인하기
inspect(myCorpusPrepro[1:5])

####전처리 완료된 자료에서 단어 선별하기####
myCorpusPrepro_term <- 
  TermDocumentMatrix(myCorpusPrepro,control = list(wordLength = c(4,16)))
myCorpusPrepro_term
myTerms_df <- as.data.frame(as.matrix(myCorpusPrepro_term))
dim(myTerms_df)
# 단어 출현 빈도수 구하기
wordResult <- sort(rowSums(myTerms_df),decreasing = TRUE)
wordResult[1:10]
# 불용어 제거
# 1. 문장부호 제거
myCorpusPrepro <-  tm_map(myCorpus,removePunctuation)
# 2. 수치제거
myCorpusPrepro <-  tm_map(myCorpusPrepro,removeNumbers)
# 3. 소문자 변경
myCorpusPrepro <- tm_map(myCorpusPrepro,tolower)
# 4. 제거할 단어를 지정해주기
myStopwords = c(stopwords('english'),"사용","하기")
# 변수를 이용한 불용어 제거
myCorpusPrepro <- tm_map(myCorpusPrepro,removeWords,myStopwords)
# 단어 선별과 평서문 전환
myCorpusPrepro_term <- 
  TermDocumentMatrix(myCorpusPrepro,
                     control = list(wordLengths = c(4,16)))
myTerms_df <- as.data.frame(as.matrix(myCorpusPrepro_term))
# 단어 빈도수
wordResult <- sort(rowSums(myTerms_df),decreasing = TRUE)
wordResult[1:10]

####워드클라우드 ####
# 단어 이름과 빈도수로 데이터 프레임 생성
myName <- names(wordResult)
word.df <- data.frame(word = myName,freq = wordResult)
str(word.df) # 구조 데이터프레임
# 단어 색상과 글꼴지정
pal <- brewer.pal(12,"Paired")
# 단어구름 시각화
wordcloud(word.df$word,word.df$freq,scale = c(5,1),
          min.freq = 3,random.order = F,
          rot.per = .1,colors = pal)
#family = 글꼴인데 malgun이라는 글꼴이 없는듯 ? 


#### 연관어 분석 ####
install.packages("backports")
setwd("C:/Rwork/data")
marketing <- file("marketing.txt",encoding = "UTF-8")
# readLines 줄단위로 끊음.
marketing2 <- readLines(marketing)
close(marketing)
head(marketing2)
# 줄단위 단어 추출
lword <- Map(extractNoun,marketing2)
length(lword)
lword <- unique(lword) #중복제거
length(lword)
# 중복단어 제거와 추출단어 확인하기
lword <- sapply(lword, unique)
length(lword)
lword
# 연관어 분석을 위한 전처리
# 단어 필터링 함수 정의
# 4글자보다작고 2글자 보다 크게 
filter1 <- function(x){
  nchar(x) <= 4&& nchar(x) >= 2&&is.hangul(x)
}
filter2 <- function(x){Filter(filter1,x)}

# 줄 단위로된 단어 전처리
lword <- sapply(lword, filter2)
lword
# 트랜잭션 생성하기 
#install.packages("arules")
#library(arules)

wordtran <- as(lword,"transactions")
wordtran
# 단어간 연관 규칙 발견하기
tranrules <- apriori(wordtran,
                     parameter = list(supp = 0.25,conf = 0.05))
# 연관 규칙 생성 결과보기
detach(package:tm,unload=TRUE)
inspect(tranrules)
# 연관어 시각화 하기
# 시각화위해 자료구조 변경하기
rules <- labels(tranrules,ruleSep = " ")
rules
# 문자열로 묶인 연관 단어를 행렬로 변경
rules <- sapply(rules,strsplit," ",USE.NAMES = F)
rules
# 행 단위로 묶어서 matrix로 변환
rulemat <- do.call("rbind",rules)
class(rulemat)

#시각화를 위한 패키지 설치
install.packages("igraph")
library(igraph)
# edgelist 보기
ruleg <- graph.edgelist(rulemat[c(12:59),],directed = F)
ruleg
# edgelist 플로팅
plot.igraph(ruleg,vertex.label = V(ruleg)$name,
            vertex.label.cex = 1.2, vertext.label.color = "black",
            vertex.size = 20,vertext.color = "green",
            vertex.frame.co.or = "blue")

#### 크롤링,스크래핑,파싱 ####
#install.packages("httr")
#library(httr)
#install.packages("XML")
#library(XML)

url <- "http://media.daum.net"
web <- GET(url)
web
# HTML 파싱하기
html <- htmlTreeParse(web,useInternalNodes = T,trim=T,encoding = "UTF-8")
rootNode <- xmlRoot(html)
# 태그자료 수집하기
news <- xpathSApply(rootNode,"//a[@class ='link_txt']",xmlValue)
news
# 수집한 자료 전처리하기
news_pre <- gsub("[\r\n\t]",' ',news)
news_pre <- gsub('[[:punct:]],news_pre')
news_pre <- gsub('[[:cntrl:]]', ' ', news_pre)
news_pre <- gsub('\\d+', ' ', news_pre) #숫자제거
news_pre <- gsub('[a-z]+', ' ', news_pre) # 소영문자제거
news_pre <- gsub('[A-Z]+', ' ', news_pre) # 대영문자제거
news_pre <- gsub('\\s+', ' ', news_pre) # 빈칸이사라짐
# 기사와 관련이없는 내용은 제거하기
news_data <- news_pre[1:46]
news_data
# 수집한 자료를 파일로 읽고 저장하기
setwd("c:/Rwork/data")
write.csv(news_data,"news_data.csv") #quoto를 없애니까 제대로 출력 ?;;
# 불러오기
news_data1 <- read.csv("news_data.csv",header = T,stringsAsFactors = F)
str(news_data1)
names(news_data1) <- c("번호","기사")
head(news_data1)
# 세종사전에 단어 추가하기
user_dic <- data.frame(term = c("삼성","코로나19","재판"),tag = 'ncn')
buildDictionary(ext_dic = 'sejong',user_dic = user_dic)
# 사용자 정의 함수
exNouns <- function(x){paste(extractNoun(x), collapse = "")}
# 함수이용하여 단어추출
news_nouns <- sapply(news_data,exNouns)
# 말뭉치 생성 집계행렬
newsCorpus <- Corpus(VectorSource(news_nouns))
inspect(newsCorpus) #데이터 확인
# 단어, 문서 집계행렬만들기
TDM <- TermDocumentMatrix(newsCorpus,control = list(wordLength = c(4,16)))
TDM
# 데이터 프레임으로 변경
tdm.df <- as.data.frame(as.matrix(TDM))
dim(tdm.df)
# 단어 출현 빈도수 구하기
wordResult <- sort(rowSums(tdm.df), decreasing = TRUE)
wordResult
#### 워크클라우드 2 ####
library(devtools)
install.packages('bslib')
.libPaths()
.libPaths("C:/Program Files/R/R-3.6.3/library") 
devtools::install_github("lchiffon/wordcloud2")
install_github("lchiffon/wordcloud2")
library(wordcloud2)

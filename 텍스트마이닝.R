# 텍스트 마이닝(교재 실습)
rm(list = ls())

# 텍스트 마이닝(PDF실습)
install.packages("https://cran.rstudio.com/bin/windows/contrib/3.4/KoNLP_0.80.1.zip", repos = NULL)

install.packages("wordcloud")
install.packages("tm")

install.packages(c("stringr", "hash", "tau", "Sejong", "RSQLite", "devtools"),
                 type = "binary")
install.packages("rJava")


library(KoNLP)
library(tm)
library(wordcloud)

# 1. facebook_bidata.txt 파일 실습
rm(list = ls())
getwd()
setwd('E:/BigDate/R programing/dataset3/dataset3')
facebook <- file('facebook_bigdata.txt', encoding = 'UTF-8')
facebook_data <- readLines(facebook)
head(facebook_data)

# 1-1 세종사전에 단어 추가하기
user_dic <- data.frame(term =  c("R 프로그래밍", "페이스북", "홍길동", "소셜네트워크"), tag = 'ncn')
buildDictionary(ext_dic = "sejong", user_dic = user_dic)

# 1-2 r제공 함수로 단어 추출하기
paste(extractNoun('홍길동은 많은 사람과 소통을 위해서 소셜네트워크에 가입하였습니다.'),
      collapse = " ")

# 1-3 단어 추출을 위한 사용자 함수 정의하기
# 단계 1: 사용자 정의 함수 작성
exNouns <- function(x) { paste(extractNoun(as.character(x)), collapse = " ") }
# 단계 2: exNouns() 함수를 이용하여 단어 추출
facebook_nouns <- sapply(facebook_data, exNouns)
facebook_nouns[1]

# 1-4 추출된 단어를 대상으로 전처리하기
# 단계 1: 추출된 단어를 이용하여 말뭉치(Corpus) 생성
myCorpus <- Corpus(VectorSource(facebook_nouns))
# 단계 2: 데이터 전처리
# 단계 2-1: 문장부호 제거
myCorpusPrepro <- tm_map(myCorpus, removePunctuation)
# 단계 2-2: 수치 제거
myCorpusPrepro <- tm_map(myCorpusPrepro, removeNumbers)
# 단계 2-3: 소문자 변경
myCorpusPrepro <- tm_map(myCorpusPrepro, tolower)
# 단계 2-4: 불용어 제거
myCorpusPrepro <- tm_map(myCorpusPrepro, removeWords, stopwords('english'))
# 단계 2-5: 전처리 결과 확인
inspect(myCorpusPrepro[1:5])

# 1-5 단어선별(2~8음절사이 단어 선택)
# 1-5-1 전처리된 단어집에서 2~8음절 단어 대상 선정
# 단계 1: 전처리된 단어집에서 2 ~ 8 음절 단어 대상 선정
myCorpusPrepro_term <-
  TermDocumentMatrix(myCorpusPrepro, 
                     control = list(wordLengths = c(4, 16)))
myCorpusPrepro_term
# 단계 2: matrix 자료구조를 data.frame 자료구조로 변경
myTerm_df <- as.data.frame(as.matrix(myCorpusPrepro_term))
dim(myTerm_df)

# 1-6 단어 출현 빈도수 구하기
wordResult <- sort(rowSums(myTerm_df), decreasing = TRUE)
wordResult[1:10]

# 1-7 불용어 제거하기
# 단계 1: 데이터 전처리
# 단계 1-1: 문장부호 제거
myCorpusPrepro <- tm_map(myCorpus, removePunctuation)
# 단계 1-2: 수치 제거
myCorpusPrepro <- tm_map(myCorpusPrepro, removeNumbers)
# 단계 1-3: 소문자 변경
myCorpusPrepro <- tm_map(myCorpusPrepro, tolower)
# 단계 1-4: 제거할 단어 지정
myStopwords = c(stopwords('english'), "사용", "하기")
# 단계 1-5: 불용어 제거
myCorpusPrepro <- tm_map(myCorpusPrepro, removeWords, myStopwords)

#단계 2: 단어 선별과 평서문 변환
myCorpusPrepro_term <-
  TermDocumentMatrix(myCorpusPrepro,
                     control = list(wordLengths = c(4, 16)))
myTerm_df <- as.data.frame(as.matrix(myCorpusPrepro_term))

# 단계 3: 단어 출현 빈도수 구하기
wordResult <- sort(rowSums(myTerm_df), decreasing = TRUE)
wordResult[1:10]

# 1-8 단어 구름에 디자인(빈도수, 색상, 위치, 회전) 적용하기
# 1-8-1 단어 이름과 빈도수로 data.frame 생성
# 단계 1: 단어 이름과 빈도수로 data.frame 생성
myName <- names(wordResult)
word.df <- data.frame(word = myName, freq = wordResult)
str(word.df )
# 단계 2: 단어 색상과 글꼴 지정
pal <- brewer.pal(12, "Paired")
# 단계 3: 단어 구름 시각화
wordcloud(word.df$word, word.df$freq, scale = c(5, 1), 
          min.freq = 3, random.order = F, 
          rot.per = .1, colors = pal, family = "malgun")
# 2. 연관어 분석
rm(list = ls())

setwd('E:/BigDate/R programing/dataset3/dataset3')

# 단계 1: 텍스트 파일 가져오기
marketing <- file("marketing.txt", encoding = "UTF-8")
marketing2 <- readLines(marketing)
close(marketing)
head(marketing2)

# 단계 2: 줄 단위 단어 추출
lword <- Map(extractNoun, marketing2)
length(lword)
lword <- unique(lword)
length(lword)

# 단계 3: 중복 단어 제거와 추출 단어 확인
lword <- sapply(lword, unique)
length(lword)
lword

# 실습: 연관어 분석을 위한 전처리하기

# 단계 1: 단어 필터링 함수 정의
filter1 <- function(x) {
  nchar(x) <= 4 && nchar(x) >= 2 && is.hangul(x)
}
filter2 <- function(x) { Filter(filter1, x) }

# 단계 2: 줄 단위로 추출된 단어 전처리
lword <- sapply(lword, filter2)
lword

# 실습: 필터링 간단 예문 살펴보기

# 단계 1: vector 이용 list 객체 생성
word <- list(c("홍길동", "이순", "만기", "김"),
             c("대한민국", "우리나라대한민구", "한국", "resu"))
class(word)

# 단계 2: 단어 필터링 함수 정의(길이 2 ~ 4 사이 한글 단어 추출) 
filter1 <- function(x) {
  nchar(x) <= 4 && nchar(x) >= 2 && is.hangul(x)
}
filter2 <- function(x) {
  Filter(filter1, x)
}

# 단계 3: 함수 적용 list 객체 필터링
filterword <- sapply(word, filter2)
filterword

# 실습: 트랜잭션 생성하기

# 단계 1: 연관분석을 위한 패키지 설치와 로딩
install.packages("arules")
library(arules)

# 단계 2: 트랜잭션 생성
wordtran <- as(lword, "transactions")
wordtran

# 실습: 단어 간 연관규칙 발견하기
install.packages('backports')
library(backports)

# 단계 1: 연관규칙 발견
tranrules <- apriori(wordtran, 
                     parameter = list(supp = 0.25, conf = 0.05))

# 단계 2: 연관규칙 생성 결과보기
detach(package:tm, unload = TRUE)
inspect(tranrules)

# 실습: 연관규칙을 생성하는 간단한 예문 살펴보기

# 단계 1: Adult 데이터 셋 메모리 로딩
data("Adult")
Adult
str(Adult)
dim(Adult)
inspect(Adult)

# 단계 2: 특정 항목의 내용을 제외한 itermsets 수 발견
apr1 <- apriori(Adult,
                parameter = list(support = 0.1, target = "frequent"),
                appearance = list(none = 
                                    c("income=small", "income=large"),
                                  default = "both"))
apr1
inspect(apr1)

# 단계 3: 특정 항목의 내용을 제외한 rules 수 발견
apr2 <- apriori(Adult, 
                parameter = list(support = 0.1, target = "rules"), 
                appearance = list(none = 
                                    c("income=small", "income=large"),
                                  default = "both"))
apr2

# 단계 4: 지지도와 신뢰도 비율을 높일 경우
apr3 <- apriori(Adult, 
                parameter = list(supp = 0.5, conf = 0.9, target = "rules"),
                appearance = list(none =
                                    c("income=small", "income=large"),
                                  default = "both"))
apr3

# 실습: inspect() 함수를 사용하는 간단 예문 보기
data(Adult)
rules <- apriori(Adult)
inspect(rules[10])

# 연관어 시각화하기

# 단계 1: 연관단어 시각화를 위해서 자료구조 변경
rules <- labels(tranrules, ruleSep = " ")
rules

# 단계 2: 문자열로 묶인 연관 단어를 행렬구조로 변경
rules <- sapply(rules, strsplit, " ", USE.NAMES = F)
rules

# 단계 3: 행 단위로 묶어서 matrix로 변환
rulemat <- do.call("rbind", rules)
class(rulemat)

# 단계 4: 연관어 시각화를 위한 igraph 패키지 설치와 로딩
install.packages("igraph")
library(igraph)

# 단계 5: edgelist 보기
ruleg <- graph.edgelist(rulemat[c(12:59), ], directed = F)
ruleg

# 단계 6: edgelist 시각화
plot.igraph(ruleg, vertex.label = V(ruleg)$name, 
            vertex.label.cex = 1.2, vertext.label.color = 'black',
            vertex.size = 20, vertext.color = 'green',
            vertex.frame.co.or = 'blue')
install.packages("httr")
library(httr)
install.packages("XML")
library(XML)

# 1. url 요청

# 실습: 웹 문서 요청
url <- "http://media.daum.net"
web <- GET(url)
web

# 2. html 파싱

# 실습: HTML 파싱하기
html <- htmlTreeParse(web, useInternalNodes = T, trim = T, encoding = "utf-8")
rootNode <- xmlRoot(html)

# 3. 태그 자료 수집

# 실습: 태그 자료 수집하기
news <- xpathSApply(rootNode, "//a[@class = 'link_txt']", xmlValue)
news

# 4. 수집한 자료 전처리

# 실습: 자료 전처리하기

# 단계 1: 자료 전처리 - 수집한 문서를 대상으로 불용어 제거
news_pre <- gsub("[\r\n\t]", ' ', news)
news_pre <- gsub('[[:punct:]]', ' ', news_pre)
news_pre <- gsub('[[:cntrl:]]', ' ', news_pre)

news_pre <- gsub('\\d+', ' ', news_pre) # corona19(covid19) 때문에 숫자 제거 생략

news_pre <- gsub('[a-z]+', ' ', news_pre)
news_pre <- gsub('[A-Z]+', ' ', news_pre)
news_pre <- gsub('\\s+', ' ', news_pre)

news_pre

# 단계 2: 기사와 관계 없는 'TODAY', '검색어 순위' 등의 내용은 제거
news_data <- news_pre[1:59]
news_data

# 6. 파일 저장 및 읽기

# 실습: 수집한 자료를 파일로 저장하고 읽기
setwd('C:/rwork')
write.csv(news_data, "news_data.csv", quote = F)
news_data <- read.csv("news_data.csv", header = T, stringsAsFactors = F)
str(news_data)
names(news_data) <- c("no", "news_text")
head(news_data)
news_text <- news_data$news_text
news_text

# 7. 토픽 분석

# 실습: 세종 사전에 단어 추가
user_dic <- data.frame(term = c("펜데믹", "코로나19", "타다"), tag = 'ncn')
buildDictionary(ext_dic = 'sejong', user_dic = user_dic)

# 실습: 단어 추출 사용자 함수 정의하기

# 단계 1: 사용자 정의 함수 작성
exNouns <- function(x) { paste(extractNoun(x), collapse = " ")}

# 단계 2: exNouns() 함수를 이용하어 단어 추출
news_nouns <- sapply(news_text, exNouns)
news_nouns

# 단계 3: 추출 결과 확인
str(news_nouns)

# 실습: 말뭉치 생성과 집계 행렬 만들기

# 단계 1: 추출된 단어를 이용한 말뭉치(corpus) 생성
newsCorpus <- Corpus(VectorSource(news_nouns))
newsCorpus
inspect(newsCorpus[1:5])

# 단계 2: 단어 vs 문서 집계 행렬 만들기
TDM <- TermDocumentMatrix(newsCorpus, control = list(wordLengths = c(4, 16)))
TDM

# 단계 3: matrix 자료구조를 data.frame 자료구조로 변경
tdm.df <- as.data.frame(as.matrix(TDM))
dim(tdm.df )

# 실습: 단어 출현 빈도수 구하기
wordResult <- sort(rowSums(tdm.df), decreasing = TRUE)
wordResult[1:10]

# 단계 1: 패키지 로딩과 단어 이름 추출
library(wordcloud)
myNames <- names(wordResult)
myNames

# 단계 2: 단어와 단어 빈도수 구하기
df <- data.frame(word = myNames, freq = wordResult)
head(df )

# 단계 3: 단어 구름 생성
pal <- brewer.pal(12, "Paired")
wordcloud(df$word, df$freq, min.freq = 2,
          random.order = F, scale = c(4, 0.7),
          rot.per = .1, colors = pal, family = "malgun")

library(devtools)
devtools::install_github("lchiffon/wordcloud2")
# install_github("lchiffon/wordcloud2")
library(wordcloud2)
wordcloud2(data = demoFreq)
# 사용법
# https://cran.r-project.org/web/packages/wordcloud2/vignettes/wordcloud.html
# https://www.r-graph-gallery.com/196-the-wordcloud2-library.html
wc2data <- data.frame(df$word, df$freq)
wc2data
wordcloud2(data=wc2data, size=0.4, color='random-dark')
wordcloud2(data=wc2data, size=1.2, color='random-light', backgroundColor = "black")

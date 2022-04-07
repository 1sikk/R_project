# 감정분석
# 필요 패키지 불러오기

# 감정 사전 활용하기
install.packages("tidyverse")
library(tidyverse)
install.packages('knitr')
library(knitr)
install.packages('dplyr', type = 'binary') # 감정사전 불러오기 패키지
library(dplyr)
install.packages('readr') # 감정사전 불러오기 패키지
library(readr)
install.packages('stringr')
library(stringr)
install.packages('tidytext')
library(tidytext)
install.packages('textclean')
library(textclean)

# 사전 파일 불러오기
getwd()
setwd('/Users/sik/Desktop/dataset5')
dic <- read_csv("knu_sentiment_lexicon.csv")
dic
str(dic)
# 긍정단어
dic %>% filter(polarity == 2) %>% arrange(word)

# 부정단어
dic %>% filter(polarity == -2) %>% arrange(word)

# 감정 단어 종류 살펴보기
dic %>% filter(word %in% c('좋은', '나쁜'))
dic %>% filter(word %in% c('기쁜', '슬픈'))

# 이모티콘
dic %>% filter(!str_detect(word, '[가-힣]')) %>% arrange(word)

# 총 단어 갯수
dic %>% mutate(sentiment = ifelse(polarity >= 1, 'pos',
                           ifelse(polarity <= -1, 'neg', 'neu'))) %>% count(sentiment)

str(dic)
# 1. 문장의 감정 점수 구하기
# 1-1 단어 기준 토큰화
df <- tibble(sentence = c('디자인 예쁘고 마감도 좋아서 만족스럽다.',
                          '디자인은 괜찮다. 그런데 마감이 나쁘고 가격도 비싸다.'))
df


df <- df %>% unnest_tokens(input = sentence, output = word, token = 'words', drop = F)
df

# 1-2 단어에 감정 점수 부여하기
df <- df %>% left_join(dic, by = 'word') %>% mutate(polarity = ifelse(is.na(polarity), 0, polarity))
df

# 1-3 문장별로 감정 점수 합산하기
score_df <- df %>% group_by(sentence) %>% summarise(score = sum(polarity))
score_df

# 2. 댓글 감정 분석하기
# 데이터 불러오기
raw_news_comment <- read_csv('news_comment_parasite.csv')

# 기본적 전처리
news_comment <- raw_news_comment %>% mutate(id = row_number(), 
                                            reply = str_squish(replace_html(reply)))

# 데이터 구조 확인
glimpse(news_comment)

# 2-1 단어 기준 토큰화 및 감정 점수 부여
word_comment <- news_comment %>% unnest_tokens(input = reply, output = word, token = 'words', drop = F)
word_comment %>% select(word, reply) # 토큰화

word_comment <- word_comment %>% left_join(dic, by = 'word') %>% mutate(polarity = ifelse(is.na(polarity), 0, polarity))
word_comment %>% select(word, polarity) # 감정 점수 부여

# 2-2 자주 사용된 감정 단어 살펴보기
word_comment <- word_comment %>% mutate(sentiment = ifelse(polarity == 2, 'pos',
                                                    ifelse(polarity == -2, 'neg', 'neu')))
word_comment %>% count(sentiment)

# 2-3 막대 그래프 만들기
top10_sentiment <- word_comment %>% filter(sentiment != 'neu') %>%
  count(sentiment, word) %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10)
top10_sentiment

library(ggplot2)
ggplot(top10_sentiment, aes(x = reorder(word, n),
                            y = n,
                            fill = sentiment)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = n), hjust = -0.3) +
  facet_wrap(~ sentiment, scales = 'free') +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
  labs(x = NULL) +
  theme(text = element_text(family = 'nanumgothic'))

# 2-4 댓글별 감정 점수 구하기
score_comment <- word_comment %>% group_by(id, reply) %>% summarise(score = sum(polarity)) %>% ungroup()
score_comment %>% select(score, reply) %>% head()

# 2-4-1 긍정댓글
score_comment %>% select(score, reply) %>% arrange(-score) 
# 2-4-2 부정댓글
score_comment %>% select(score, reply) %>% arrange(score) 

# 2-4-3 감정 점수 빈도 구하기
score_comment %>% count(score)

# 2-5 감정 분류 후 막대 그래프 만들기
# 감정 분류
score_comment <- score_comment %>% mutate(sentiment = ifelse(score >= 1, 'pos',
                                                      ifelse(score <= -1, 'neg', 'neu')))

# 빈도 비율 구하기
frequency_score <- score_comment %>% count(sentiment) %>% mutate(ratio = n/sum(n)*100)
frequency_score

# 그래프 생성
ggplot(frequency_score, aes(x = sentiment, y = n, fill = sentiment)) +
  geom_col() +
  geom_text(aes(label = n), vjust = -0.3) +
  scale_x_discrete(limits = c('pos', 'neu', 'neg'))

# 3. 샘플 데이터로 비율 누적 막대 그래프 생성
df <- tibble(contry = c('korea', 'korea', 'japan', 'japan'), #축
             sex = c('m', 'f', 'm', 'f'), # 누적막대
             ratio = c(60, 40, 30, 70)) # 값
df

ggplot(df, aes(x = contry, y = ratio, fill = sex)) + geom_col()
ggplot(df, aes(x = contry, y = ratio, fill = sex)) +
  geom_col() +
  geom_text(aes(label = paste0(ratio, '%')), # %표시
            position = position_stack(vjust = 0.5)) # 가운데 표시시

# 4. 댓글의 감정 비율로 누적 그래프 생성
# 4-1 더미 변수 생성
frequency_score$dummy <- 0
frequency_score

ggplot(frequency_score, aes(x = dummy, y = ratio, fill = sentiment)) +
  geom_col() +
  geom_text(aes(label = paste0(round(ratio, 1), '%')),
            position = position_stack(vjust = 0.5)) +
  theme(axis.title.x = element_blank(), # x축 이름 삭제
        axis.text.x = element_blank(), # x축 값 삭제
        axis.ticks.x = element_blank()) # x축 눈금 삭제제


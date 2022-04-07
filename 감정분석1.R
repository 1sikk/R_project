# 감정 범주별 주요 단어 실습
install.packages('knitr')
library(knitr)
# include_graphics('Image/etc/04_1_dic.png') # 이미지 파일 없음
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
library(ggplot2)

# 감정 범주별 단어 빈도 구하기
# 토큰화하고 두 글자 이상 한글 단어만 남기기
comment <- score_comment %>% unnest_tokens(input = reply, # 단어기준 토큰화
                                           output = word,
                                           token = 'words',
                                           drop = F) %>%
  filter(str_detect(word, '[가-힣]') & # 한글 추출
         str_count(word) >= 2) # 두 글자 이상 추출

#감정 및 단어별 빈도 구하기
frequency_word <- comment %>%
  filter(str_count(word) >= 2) %>%
  count(sentiment, word, sort = T)
frequency_word

# 긍정댓글 고빈도 단어
frequency_word %>% filter(sentiment == 'pos')
#부정댓글 고빈도 단어
frequency_word %>% filter(sentiment == 'neg')

# 1-4 로그 오즈비 구하기
library(tidyr)
comment_wide <- frequency_word %>%
  filter(sentiment != 'neu') %>%
  pivot_wider(names_from = sentiment,
              values_from = n,
              values_fill = list(n=0))
comment_wide

comment_wide <- comment_wide %>%
  mutate(log_odds_ratio = log(((pos + 1) / (sum(pos + 1))) /
                              ((neg + 1) / (sum(neg + 1)))))  
comment_wide 
# dbl값이 높을 수록 pos 긍정쪽이 많다.

# 1-5 로그 오즈비 가장 큰단어 10개 추출
top10 <- comment_wide %>% 
  group_by(sentiment = ifelse(log_odds_ratio > 0, 'pos', 'neg')) %>%
  slice_max(abs(log_odds_ratio), n = 10, with_ties = F)
top10

# 1-6 막대 그래프 만들기
ggplot(top10, aes(x= reorder(word, log_odds_ratio),
                  y = log_odds_ratio,
                  fill = sentiment)) +
  geom_col() +
  coord_flip() +
  labs(x = NULL) +
  theme(text = element_text(family = 'nanumgothic'))

# 감정 사전 수정하기
# 소름, 미친 : 긍정적 감정을 극적으로 표현한 단어

# 소름이 사용된 댓글
score_comment %>% filter(str_detect(reply, '소름')) %>% select(reply)
# 미친이 사용된 댓글
score_comment %>% filter(str_detect(reply, '미친')) %>% select(reply)

# 소름, 미친이 부정적 단어로 분류 -> 감정 사전 수정 필요
dic %>% filter(word %in% c('소름', '소름이', '미친'))

new_dic <- dic %>% mutate(polarity = ifelse(word %in% c('소름', '소름이', '미친'), 2, polarity))
new_dic %>% filter(word %in% c('소름', '소름이', '미친'))

# 2-2 수정한 사전으로 검정 점수 부여하기
new_word_comment <- word_comment %>%
  select(-polarity) %>%
  left_join(new_dic, by = 'word') %>%
  mutate(polarity = ifelse(is.na(polarity), 0, polarity))

new_score_comment <- new_word_comment %>%
  group_by(id, reply) %>%
  summarise(score = sum(polarity)) %>%
  ungroup()

new_score_comment %>%
  select(score, reply) %>%
  arrange(-score)

# 2-3 1점 기준으로 긍정, 중립, 부정 분류
new_score_comment <- new_score_comment %>%
  mutate(sentiment = ifelse(score >= 1, 'pos',
                     ifelse(score <= -1, 'neg', 'neu')))

# 원본 감정 사전
score_comment %>% count(sentiment) %>% mutate(ratio = n/sum(n)*100)
# 수정 감정 사전
new_score_comment %>% count(sentiment) %>% mutate(ratio = n/sum(n)*100)

# 2-4 분석결과 비교하기
word <- '소름|소름이|미친'

# 원본 감정 사전
score_comment %>% filter(str_detect(reply, word)) %>% count(sentiment)
# 수정 감정 사전
new_score_comment %>% filter(str_detect(reply, word)) %>% count(sentiment)

# 2-5 두글자 이상 한글 단어만 남기고 단어 빈도 구하기
# 토큰화 및 전처리
new_comment <- new_score_comment %>%
  unnest_tokens(input = reply,
                output = word,
                token = 'words',
                drop = F) %>%
  filter(str_detect(word, '[가-힣]') &
           str_count(word) >= 2)

# 감정 및 단어별 빈도 구하기
new_frequency_word <- new_comment %>% count(sentiment, word, sort = T)

# wide form으로 변환
new_comment_wide <- new_frequency_word %>%
  filter(sentiment != 'neu') %>%
  pivot_wider(names_from = sentiment,
              values_from = n,
              values_fill = list(n = 0))
# 로그 오즈비 구하기
new_comment_wide <- new_comment_wide %>%
  mutate(log_odds_ratio = log(((pos + 1) / (sum(pos + 1))) /
                              ((neg + 1) / (sum(neg + 1)))))

# 그래프 생성
new_top10 <- new_comment_wide %>%
  group_by(sentiment = ifelse(log_odds_ratio > 0, 'pos', 'neg')) %>%
  slice_max(abs(log_odds_ratio), n = 10, with_ties = F)
ggplot(new_top10, aes(x = reorder(word, log_odds_ratio),
                      y = log_odds_ratio,
                      fill = sentiment)) +
  geom_col() +
  coord_flip() +
  labs(x = NULL) +
  theme(text = element_text(family = 'nanumgothic'))

# 2-6 주요 단어 사용 댓글 살펴보기
# 긍정 댓글 원문
new_score_comment %>%
  filter(sentiment == 'pos' & str_detect(reply, '축하')) %>%
  select(reply)
new_score_comment %>%
  filter(sentiment == "pos" & str_detect(reply, "소름")) %>%
  select(reply)

# 부정 댓글 원문
new_score_comment %>%
  filter(sentiment == 'neg' & str_detect(reply, '좌빨')) %>%
  select(reply)
new_score_comment %>%
  filter(sentiment == 'neg' & str_detect(reply, '못한')) %>%
  select(reply)

# 2-7 분석결과 비교
# 수정한 감정 사전
new_top10 %>% select(-pos, -neg) %>% arrange(-log_odds_ratio)
# 원본 감정 사전
top10 %>% select(-pos, -neg) %>% arrange(-log_odds_ratio)

new_comment_wide %>% filter(word == '미친')


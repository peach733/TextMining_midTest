#맥북 ggplot2 출력시, 한글 깨짐 방지 코드
theme_set(theme_gray(base_family='NanumGothic'))

#필요한 패키지 설치
install.packages("stringr")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("tidytext")
install.packages("readr")
install.packages("KoNLP")
install.packages("textclean")
library(stringr)
library(ggplot2)
library(dplyr)
library(tidytext)
library(readr)
library(KoNLP)
library(textclean)

#2번
#단어 추출 코드
#txt 파일 불러오기
raw_gpt <- readLines("/Users/jihyekim/4학년/1학기/텍스트마이닝/mid_test.txt", encoding = "UTF-8")

#txt 파일을 tibble 형태로 변환 -> 토큰 추출하기 위함
new <- str_squish(raw_gpt) %>% as_tibble()

#토큰 추출
text <- new %>% unnest_tokens(input = value, output = word, token = "words")

#단어 빈도
text <- text %>% count(word, sort = T)

#1개만 언급된 단어 제외 후, 상단 20개만 추출
answer <- text %>% filter(str_count(word) > 1)
answer <- answer %>% head(20)

#그래프 그리기
ggplot(answer, aes(x = reorder(word, n), y = n)) + geom_col() + coord_flip()

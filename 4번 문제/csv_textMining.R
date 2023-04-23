#4번
#감정 사전을 만들기 위해서 knu_sentiment_lexicon CSV 파일 불러오기
dic <- read_csv("/Users/jihyekim/4학년/1학기/텍스트마이닝/knu_sentiment_lexicon.csv")

#긍정, 부정, 중립 단어 구분
dic %>% filter(!str_detect(word, "[^가-힣]")) %>% arrange(word)
dic %>% mutate(sentiment = ifelse(polarity >= 1, "pos", ifelse(polarity <= -1, "neg", "neu"))) %>%
  count(sentiment)

#국정원 트위터 CSV 파일을 감정 사전을 통하여 긍정, 부정, 중립 단어 빈도 확인하기 위해 파일 불러오기
raw_twitter <- read_csv("/Users/jihyekim/4학년/1학기/텍스트마이닝/mid_csv.csv")

#국정원 트위터 CSV 파일을 토큰화하기
twitter_txt <- raw_twitter %>% unnest_tokens(input = value, output = word, token = "words", drop = F)

#국정원 트위터 CSV 파일과 감정 점수 부여하기
twitter_txt <- twitter_txt %>% left_join(dic, by = "word") %>% 
  mutate(polarity = ifelse(is.na(polarity), 0, polarity))

#감정 분류하기 1 -> 긍정, -1 -> 부정, 그외는 중립
twitter_txt <- twitter_txt %>% 
  mutate(sentiment = ifelse(polarity == 1, "pos", ifelse(polarity == -1, "neg", "neu")))

#국정원 트위터 CSV 파일을 감정 사전을 통한 긍정, 부정, 중립 값을 확인 후 그래프화
freq_twitter <- twitter_txt %>% count(sentiment) %>% mutate(ratio = n/sum(n) * 100)
ggplot(freq_twitter, aes(x = sentiment, y = n, fill = sentiment)) + 
  geom_col() + geom_text(aes(label = n, vjust = -0.3)) + scale_x_discrete(limits = c("pos", "neu", "neg"))

#3번
#TF-IDF로 분석할 국정원 트위터 CSV 파일 불러오기
raw_twitter <- read_csv("/Users/jihyekim/4학년/1학기/텍스트마이닝/mid_csv.csv")

#CSV 파일에서 중복값은 제외하기 -> 데이터가 너무 많아서 시간 단축을 위함
raw_twitter <- unique(raw_twitter)

#단어의 빈도를 구하기
#전처리
twitter <- raw_twitter %>% mutate(value = str_replace_all(value, "[^가-힣]", " "), value = str_squish(value))

#토큰화
twitter <- twitter %>% unnest_tokens(input = value, output = word, token = extractNoun)

#단어 빈도 구하기
freq <- twitter %>% count(type, word) %>% filter(str_count(word) > 1)

#단어의 빈도를 내림차순 정렬하여 전체 단어 수의 비율이 높은 순으로 정렬
freq <- freq %>% bind_tf_idf(term = word, document = type, n = n) %>% arrange(-tf_idf)

#막대 그래프를 이용하여 비교하기
top_10 <- freq %>% group_by(type) %>% slice_max(tf_idf, n = 20, with_ties = F)

#범주형으로 만들면서 그래프의 순서 정함
top_10$type <- factor(top_10$type, levels = c("리트윗", "직접 작성"))
ggplot(top_10, aes(x = reorder_within(word, tf_idf, type), y = tf_idf, fill = type)) + 
  geom_col(show.legend = F) + coord_flip() + 
  facet_wrap(~type, scales = "free", ncol = 2) + 
  scale_x_reordered() + labs(x = NULL)

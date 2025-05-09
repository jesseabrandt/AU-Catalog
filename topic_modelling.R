pacman::p_load(pacman, tidyverse, tidytext, data.table, topicmodels, tidyr, stringr, reshape2)
#
#basing some stuff on www.tidytextmining.com (LDA Chapter)

#load data from 2024-2025 catalog
df <- fread("all_catalogs2.csv")

#preprocessing

  
df$index = 1:nrow(df)
# df <- df %>%
#   select(!(link_1:link_15))
course_words <- df %>%
  select(title, index, description) %>%
  unnest_tokens(word, description)
#remove stop words
#remove words about logistics and stufF? could just include more topics in model
special_words <- data.frame(word = c("offered", "spring", "summer", "fall", "students", "topics",
                                     "credit", "grading", "prerequisite", "corequisite", "concurrent",
                                     "section", "topic", "permission", "crosslist", "study",
                                     "instructor", "vary", "pass", "fail", "repeatable",
                                     "gpa", "department", "chair", "selected", "recurring",
                                     "i", "ii", "introduction", "advanced", "emphasis"))

depts <- unique(df$dept) %>% tolower()
depts <- tibble(word = depts)
data("stop_words")
course_words <- course_words %>%
  anti_join((stop_words)) %>%
  anti_join(special_words) %>%
  anti_join(depts) %>%
  count(title, index, word, sort = TRUE)
course_words <- course_words %>%
  filter(!grepl("\\d", word))

#Convert to Document Term Matrix
course_dtm <- course_words %>%
  cast_dtm(index, word, n)

#Run LDA
course_lda <- LDA(course_dtm, k = 20, control = list(seed = 12))
course_lda
load("LDA.RData")


course_topics <- tidy(course_lda, matrix = "beta")
course_topics
top_terms <- course_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 15) %>% 
  ungroup() %>%
  arrange(topic, -beta)
top_terms

top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  scale_fill_viridis_d() +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered() +
  theme_bw() +
  # xlim(0,0.20) +
  # theme(axis.ticks.length.y  = unit(0.5, "cm"))
  scale_x_continuous(labels = scales::label_number(accuracy = 0.01), limits = c(0,0.12), 
                     breaks = c(0,0.05,0.1))

ggsave("topics2.png", units = "in", width = 10, height = 7)


save(course_lda, file = "LDA2.RData")

topic_names <- tribble(
  ~topic, ~topic_name,
  1, "Management",
  2, "Gender/Race/Identity",
  3, "Information Technology",
  4, "Finance",
  5, "International Issues",
  6, "Media",
  7, "Arts",
  8, "Quantitative Analysis",
  9, "Social Issues",
  10, "Washington",
  11, "Research",
  12, "Politics and Policy",
  13, "History",
  14, "Education",
  15, "Capstone",
  16, "Repeatable",
  17, "Psychology",
  18, "Languages",
  19, "Community Health",
  20, "Business"
)
course_gamma <- tidy(course_lda, matrix = "gamma")
course_gamma <- course_gamma %>%
  left_join(topic_names, by = "topic") %>%
  data.table()

df2 <- dcast(course_gamma, topic_name + document ~ gamma, value.var = "gamma")




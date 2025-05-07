pacman::p_load(pacman, tidyverse, tidytext, data.table, topicmodels, tidyr, stringr, reshape2)

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
  16, "Repeatable Topics",
  17, "Psychology",
  18, "Languages",
  19, "Community Health",
  20, "Business"
)
load("LDA.RData")
course_gamma <- tidy(course_lda, matrix = "gamma")

course_gamma <- course_gamma %>%
  left_join(topic_names, by = "topic") 

df2 <- dcast(course_gamma, document ~ topic_name, value.var = "gamma", sep = "_")

df2 <- df2 %>%
  mutate(document = as.integer(document))
df <- fread("all_catalogs3.csv")


df$index = 1:nrow(df)

df3 <- df2 %>%
  right_join(df, by = join_by(document == index))

fwrite(df2, "topics_data.csv")
#top_topics$document %>% unique() %>% length()
top_topics <- course_gamma %>%
  group_by(document) %>%
  mutate(max_gamma = max(gamma), document = as.integer(document)) %>%
  filter(gamma == max_gamma) %>%
  select(document, top_topic = topic_name, top_topic_gamma = max_gamma)
df4 <- df3 %>%
  left_join(top_topics, by = "document")
fwrite(df4, file = "top_topics.csv")

dept_topics <- df4 %>%
  group_by(dept, top_topic) %>%
  summarize(n = n())


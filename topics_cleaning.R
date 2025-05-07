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

df2 <- df2 %>%
  right_join(df, by = join_by(document == index))

fwrite(df2, "topics_data.csv")


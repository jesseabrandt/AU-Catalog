pacman::p_load(data.table, tidyverse)

topics <- fread("topics_data.csv")
topics <- topics %>%
  select(!(V1.y:school.y))
topics <- topics %>%
  select(Arts:Washington, dept = dept.x, dept_name = dept_name.x,min_credits = min_credits.x, academic_year = academic_year.x,
         fall = fall.x, top = top_topic.x, school = school.x) %>%
  rename(quant = "Quantitative Analysis") %>%
  mutate(school = ifelse(school == "soe", "School of Education", school))
fwrite(file = "topics_clean.csv", topics)




df <- fread("data_placeholder3.csv")
df <- fread("data_placeholder.csv")

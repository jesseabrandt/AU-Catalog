pacman::p_load(pacman, tidyverse, data.table)

#Load scraped past catalogs
df <- fread("past_catalogs.csv")

#separate into department, number, and name
title_info <- str_match(df$title, "([A-Z]*)-(\\d*\\.*\\d*) (.*) opens a") %>% 
  data.frame() %>%
  select(dept = X2, course_num = X3, course_name = X4)
df <- bind_cols(df, title_info)

df <- df %>% #keep relevant columns (drop misc html attributes)
  select(dept, course_num, course_name, title, fall, spring, href)

credits_all <- str_match(df$course_name, "\\((\\d*\\.*\\d*)-*(\\d*\\.*\\d*)\\)") |>
  data.frame()
names(credits_all) = c("credits", "min_credits", "max_credits")
credits_all <- credits_all %>%
  mutate(max_credits = ifelse(max_credits == "", min_credits, max_credits))
credits_all <- credits_all %>%
  mutate(across(min_credits:max_credits, as.numeric))

df <- bind_cols(df, credits_all)

fwrite(df, "past_catalogs_clean.csv")


pacman::p_load(pacman, tidyverse, tidytext, data.table)

#df <- read_csv("catalog_scrape.csv")
#something wrong w csv parsing, I would recommend using fread instead
df <- fread("catalog_scrape.csv")


#Start by getting info from the titles
title_info <- str_match(df$title, "([A-Z]*)-(\\d*) (.*)(\\(\\d-*\\d*\\))") %>% data.frame()

names(title_info) <- c("title", "dept", "course_num", "course_name", "credits")
credits_all <- str_match(title_info$credits, "\\((\\d*\\.*\\d*)-*(\\d*\\.*\\d*)\\)")
credits_all <- credits_all %>%
  data.frame()
names(credits_all) = c("credits", "min_credits", "max_credits")
credits_all <- credits_all %>%
  mutate(max_credits = ifelse(max_credits == "", min_credits, max_credits))
credits_all <- credits_all %>%
  mutate(across(min_credits:max_credits, as.numeric)) %>%
  select(!credits)

title_info <- bind_cols(title_info, credits_all) %>%
  select(!title)
df <- bind_cols(df, title_info)


#Now get basic info from the description
#Get rid of uniform extraneous text
df$description <- df$description %>% gsub(x = ., pattern = "HELPAmerican University Catalog 2024-2025 Print-Friendly Page \\(opens a new window\\)",
                          replacement = "") %>%
  gsub(x = ., pattern = "  ", replacement = "") %>%
  gsub(x = ., pattern = "Back to Top | Print-Friendly Page \\(opens a new window\\)", replacement = "")


to_match <- c("Usually Offered: (.*?)\\.", "Crosslist: (.*?)\\.", "Grading: (.*?)\\.","Restriction: (.*?)\\.",
              "Note: (.*?)\\.", "Permission: (.*?)\\.", "Prerequisite: (.*?)\\.", "Requirement: (.*?)\\.",
              "Concurrent: (.*?)\\.", "Corequisite: (.*?)\\.")

match_names <- c("offered", "crosslist", "grading", "restriction", "note", "permission",
                 "prerequisite", "requirement", "concurrent", "corequisite")

get_capture_group <- function(pattern, x){#return only the first capture group from str_match
  matches = str_match(x, pattern = pattern)
  if(ncol(matches)>1){
    return(matches[,2])
  }else{
    return (NA)
  }

}
result = map(to_match, \(x) get_capture_group(pattern = x ,x = df$description))
names(result) = match_names
result = data.frame(result)

df2 = bind_cols(df, result)
#NOTE corequisite and concurrent may be the same thing, redundant label


#values <- df$description %>% str_match("(\\w*): (.*?)\\.") # to examine what words go with :


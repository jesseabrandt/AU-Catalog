pacman::p_load(pacman, tidyverse, data.table)

#Load scraped past catalogs
df <- fread("all_catalogs.csv")

#separate into department, number, and name
title_info <- str_match(df$title, "([A-Z]*)-(\\d*\\.*\\d*) (.*) opens a") %>% 
  data.frame() %>%
  select(dept = X2, course_num = X3, course_name = X4)
df <- bind_cols(df, title_info)

#extract credit info
credits_all <- str_match(df$description, "\\((\\d*\\.*\\d*)-*(\\d*\\.*\\d*)\\)") |>
  data.frame()
names(credits_all) = c("credits", "min_credits", "max_credits")
credits_all <- credits_all %>%
  mutate(max_credits = ifelse(max_credits == "", min_credits, max_credits))
credits_all <- credits_all %>%
  mutate(across(min_credits:max_credits, as.numeric))

df <- bind_cols(df, credits_all)
df <- df %>% 
  mutate(description = sub(".*?\\)", "", description),#Take the title out of description column text
         title = sub(" opens a new window", "", title),
         description = gsub("\\n", ";", description),
         description = gsub("\\t", ";", description),
         description = sub("Back to Top | Print-Friendly Page (opens a new window)", "", description))
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

df = bind_cols(df, result)


#Add Eagle Service URL
es_url <- "https://eagleservice.american.edu/Student/Student/Courses/Search?keyword="
formatC(1, format = "d", width = 3, flag = "0")
df <- df %>%
  mutate(eagle_service_url = paste0(es_url, dept, "+", formatC(course_num, format = "d", width = 3, flag = "0")))
#New column - update subsequent files
df <- df %>%
  mutate(academic_year = paste(fall,spring, sep = "-"))
fwrite(df, "all_catalogs2.csv")


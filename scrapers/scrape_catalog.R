# This file scrapes the course catalog for 2024-2025.
#Todo: scrape other years' catalogs

# Load packages with pacman
if (!require("pacman")) install.packages("pacman")
pacman::p_load(pacman, tidyverse, rvest)


# Get page one of catalog
url <- "https://catalog.american.edu/content.php?catoid=22&catoid=22&navoid=4006&filter%5Bitem_type%5D=3&filter%5Bonly_active%5D=1&filter%5B3%5D=1&filter%5Bcpage%5D=1#acalog_template_course_filter"
page <- read_html(url)
num_pages <- page %>%
  html_elements("a:nth-child(14)") %>%
  html_text()


#Get some attributes of ?
# course_text <- page %>%
#  html_elements(".width a") %>%
# html_text()
# course_href <- page %>%
# html_elements(".width a") %>%
# html_attrs("")

#Writing code for the coming for loop
course_attributes <- page %>%
  html_elements(".width a") %>%
  html_attrs()
tbl <- tibble(attr = course_attributes) %>%
  unnest_wider(attr)#unnest data

# Scrapes every page of classes in catalog



for(i in 2:num_pages){#
  # 
  url = paste("https://catalog.american.edu/content.php?catoid=22&catoid=22&navoid=4006&filter%5Bitem_type%5D=3&filter%5Bonly_active%5D=1&filter%5B3%5D=1",
              "&filter%5Bcpage%5D=",
              i, sep = "")
  #"#acalog_template_course_filter" is usually in url
  page = read_html(url)
  
  
  course_attributes <- page %>%
    html_elements(".width a") %>%
    html_attrs()
  tbl2 <- tibble(attr = course_attributes) %>%
    unnest_wider(attr)#unnest data
  tbl <- bind_rows(tbl, tbl2)
  Sys.sleep(1)
}

#practice code for code below
# url = paste("https://catalog.american.edu/content.php?catoid=22&catoid=22&navoid=4006&filter%5Bitem_type%5D=3&filter%5Bonly_active%5D=1&filter%5B3%5D=1&filter%5Bcpage%5D=",
#             i, "#acalog_template_course_filter", sep = "")
# page = read_html(url)
# 
# 
# tbl = tbl %>%
#   mutate(url = paste("https://catalog.american.edu/",
#                      href, sep = ""))


# Get course descriptions
for(j in 1:nrow(table)){
  page <- read_html(tbl$url[[j]])
  tbl$description[j] <- page %>%
    html_elements(css = ".block_content") %>%
    html_text() 
  tbl$links[j] <- page %>%
    html_elements(".block_content a") %>%
    html_attrs() %>%
    list()
print(paste("Course ",j, ": ",tbl$title[j], sep = ""))
}
tbl_raw <- tbl

tbl$description <- gsub("\n", "", tbl$description)
tbl$description <- gsub("\t", "", tbl$description)


tbl <- tibble(tbl)
tbl$links[[1]] %>% View()

tbl_long <- tbl %>%
  unnest_longer(links) %>%
  unnest_wider(links, names_sep = "_")
tbl_long <- tbl_long %>%
  filter(is.na(links_alt), links_href !="help.php?catoid=22", links_href != "javascript:void(0);")
#tbl_long = tbl_long %>%
  #select(1:3,6:9, links_id)
tbl_long <- tbl_long %>%
  select(!links_onclick)
tbl_long <- tbl_long %>%
  select(!index)
tbl_long <- tbl_long %>%
  select(!links_id)
tbl_long = tbl_long %>%
  group_by(title) %>%
  mutate(counter = row_number(title))
links_wide <- tbl_long %>%
  pivot_wider(values_from = links_href, names_from = counter, names_prefix = "link_")
links_wide <- links_wide %>%
  select(title, link_1:link_15)

tbl_full <- left_join(tbl, links_wide, by = "title") %>%
  select(href, title, url, description, link_1:link_15)

write_csv(tbl_full, "catalog_scrape.csv")
#unique(tbl_long$links_href) %>% head(10)

#Now you have some scraped data. If you want more from the pages
#time for regex

# "([A-Z]*)-(\d*) (.*)(\(\d-*\d*\))"
# needs extra backslashes
#after main str_match for title you can make a min and max credit column

#page_urls <- page %>%
#  html_elements(".block_content td:nth-child(1) a") %>%
#  html_attr("href")


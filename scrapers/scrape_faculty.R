pacman::p_load(tidyverse, rvest)

#Crawl-delay: 10

#Retrieve each school's directory link
url = "https://www.american.edu/provost/academicaffairs/faculty-profiles.cfm"
page <- read_html(url)
school_links <- page %>%
  html_elements("#txt-4597127 a") %>%
  html_attr(name = "href")
Sys.sleep(10)


#Find all school faculty directory links -some multi-page and some single page

############CAS
#Retrieve CAS page links
url = paste0("https://www.american.edu",school_links[1])
page <- read_html(url)
cas_page_hrefs <- page %>%
  html_elements("#txt-4612711 p:nth-child(1) a") %>%
  html_attr("href")
names <- page %>%
  html_elements(".profile-name span") %>%
  html_text()
cas_page_links <- paste0("https://www.american.edu", cas_page_hrefs)
Sys.sleep(10)


##########Kogod
#full-time, also get links for adjuncts
url = paste0("https://www.american.edu", school_links[2])
page <- read_html(url)
Sys.sleep(10)
kogod_page_hrefs <- page %>%
  html_elements("#cs_control_4596274 a") %>%
  html_attr(name = "href")
kogod_page_links <- (paste0("https://www.american.edu", kogod_page_hrefs[2:5]))
kogod_page_links <- c(kogod_page_hrefs[1], kogod_page_links)

###SOC
soc_page_link <- "https://www.american.edu/soc/faculty/"

###SOE
soe_page_link <- "https://www.american.edu/soe/faculty/"

###SIS
sis_page_links = c("https://www.american.edu/sis/faculty/", "https://www.american.edu/sis/faculty/adjunct.cfm",
                   "https://www.american.edu/sis/faculty/emeriti.cfm")


#####SPA
url = "https://www.american.edu/spa/faculty/"
page <- read_html(url)
spa_page_hrefs <- page %>%
  html_elements("#meet-the-spa-faculty a") %>%
  html_attr("href")
spa_page_links <- paste0("https://www.american.edu/", spa_page_hrefs)
spa_page_links <- c(url, spa_page_links)
Sys.sleep(10)

###WCL
wcl_page_link <- "https://www.american.edu/wcl/faculty/full-time-faculty.cfm"



all_page_links <- c(cas_page_links, kogod_page_links, spa_page_links, soc_page_link,
                    sis_page_links, soe_page_link, wcl_page_link)

full_table <- tibble(name = character(0), title = character(0), dept_info = character(0),
                     profile_link = character(0), email = character(0), phone = character(0))


for(i in 1:length(all_page_links)){
  url = all_page_links[i]
  page <- read_html(url)
  
  profs <- page %>%
    html_elements(".flex-3")
  name <- profs %>%
    html_element(".profile-name span") %>%
    html_text2()
  title <- profs %>%
    html_element("small") %>%
    html_text2()
  dept_info <- profs %>%
    html_element(".profile-office") %>%
    html_text2()
  profile_link <- profs %>%
    html_element(".btn-sm") %>%
    html_attr("href") %>%
    paste0("https://www.american.edu", .)
  email <- profs %>%
    html_element(".profile-email span") %>%
    html_text2()
  phone <- profs %>%
    html_element(".profile-phone span") %>%
    html_text2()
  temp_tbl <- tibble(name, title, dept_info, profile_link, email, phone)
  #add results to full table
  full_table <- bind_rows(full_table, temp_tbl)
  message("Page ", i, " retrieved")
  message("Starting with ", name[1])
  Sys.sleep(10)
}

write_csv(full_table, "faculty_basic.csv")
full_table <- full_table %>%
  mutate(teaching = NA, degrees = NA, bio = NA, see_also = NA)

#Get faculty profile info
for(i in 432:nrow(full_table)){
  url = full_table$profile_link[i]
  page <- read_html(url)
  Sys.sleep(10)
  teaching <- page %>%
    html_elements("#onetrust-accept-btn-handler , #profile-teaching") %>%
    html_text2() %>%
    gsub(x = ., "\\n", ";")
  bio <- page %>%
    html_elements(".bio-text") %>%
    html_text2()
  see_also <- page %>%
    html_elements(".profile-see-also") %>%
    #html_elements("li") %>%
    html_elements(".bullet") %>%
    html_elements("a") %>%
    html_attr("href")
  degrees <- page %>%
    html_elements(".profile-info-bio") %>%
    html_text2() %>%
    gsub(x = ., "\\n", ";") %>%
    str_match(., "Degrees.*?;;")
  teaching = ifelse(length(teaching) == 0, NA, teaching)
  degrees = ifelse(length(degrees) == 0, NA, degrees)
  bio = ifelse(length(bio) == 0, NA, bio)
  see_also = ifelse(length(see_also) == 0, NA, list(see_also, sep = ";"))
  full_table$teaching[i] = teaching
  full_table$degrees[i] = degrees
  full_table$bio[i] = bio
  full_table$see_also[i] = see_also
  #add message
  message(i, ". Profile for ", full_table$name[i], " retrieved.")
}
# url = "https://www.american.edu/cas/faculty/dabraham.cfm"
# url = "https://www.american.edu/cas/faculty/lu.cfm"
# url = "https://www.american.edu/kogod/faculty/bedford.cfm"
# page <- read_html(url)
# teaching <- page %>%
#   html_elements("#onetrust-accept-btn-handler , #profile-teaching") %>%
#   html_text2() %>%
#   gsub(x = ., "\\n", ";")
# # fall_classes <- teaching %>%
# #   gsub(x = ., "\\n", ";") %>%
# #   str_match(., "Fall.*Spring")
# # spring_classes <- teaching %>%
# #   gsub(x = ., "\\n", ";") %>%
# #   str_match(., "Spring.*$")
# # summer_classes <- teaching %>%
# #   gsub(x = ., "\\n", ";") %>%
# #   str_match(., "Summer.*Fall")
# # Fix but can do in cleaning stage - formats may vary
# bio <- page %>%
#   html_elements(".bio-text") %>%
#   html_text2()
# see_also <- page %>%
#   html_elements(".profile-see-also") %>%
#   #html_elements("li") %>%
#   html_elements(".bullet") %>%
#   html_elements("a") %>%
#   html_attr("href")
# degrees <- page %>%
#   html_elements(".profile-info-bio") %>%
#   html_text2() %>%
#   gsub(x = ., "\\n", ";") %>%
#   str_match(., "Degrees.*?;;")

write_csv(full_table, "faculty_profile_scrape.csv")


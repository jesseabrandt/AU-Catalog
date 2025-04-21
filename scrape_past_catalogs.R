if (!require("pacman")) install.packages("pacman")
pacman::p_load(pacman, tidyverse, rvest, logger)

# todo: implement error handling
# NOTE this file will take a long time (~19 hours?) to run 
# (it complies with 120 second crawl-delay as requested in robots.txt)

#Catalog urls for each academic year
year_urls <- c("https://catalog.american.edu/content.php?filter%5B27%5D=-1&filter%5B29%5D=&filter%5Bkeyword%5D=&filter%5B32%5D=1&cur_cat_oid=3&expand=&navoid=643&search_database=Filter",
               "https://catalog.american.edu/content.php?catoid=5&navoid=1375",
               "https://catalog.american.edu/content.php?catoid=6&navoid=1484",
               "https://catalog.american.edu/content.php?catoid=9&navoid=1864",
               "https://catalog.american.edu/content.php?catoid=10&navoid=1991",
               "https://catalog.american.edu/content.php?catoid=12&navoid=2203",
               "https://catalog.american.edu/content.php?catoid=14&navoid=2339",
               "https://catalog.american.edu/content.php?catoid=16&navoid=2589",
               "https://catalog.american.edu/content.php?catoid=17&navoid=2815",
               "https://catalog.american.edu/content.php?catoid=18&navoid=3072"
               )
fall <- c(2014:2023)
spring <- c(2015:2024)


#find a way to note which pages were null. (perfect this process before you do the much longer one.)

#function not useful, use get_catalog_page() instead
#get_catalog_list <- function(url){
  page <- read_html(url)
  num_pages <- page %>%
    html_elements("a:nth-child(14)") %>%
    html_text()
  #get page 1 attributes
  course_attributes <- page %>%
    html_elements(".width a") %>%
    html_attrs()
  tbl <- tibble(attr = course_attributes) %>%
    unnest_wider(attr)#unnest data
  Sys.sleep(120)
  for(i in 2:num_pages){#
    # 
    url = paste(url,"&filter%5Bcpage%5D=",
                i, sep = "")
    #"#acalog_template_course_filter" is usually in url
    page = read_html(url)
    
    
    course_attributes <- page %>%
      html_elements(".width a") %>%
      html_attrs()
    tbl2 <- tibble(attr = course_attributes) %>%
      unnest_wider(attr)#unnest data
    tbl <- bind_rows(tbl, tbl2)
    print(tbl2$href[[1]])
    #print('test1')
    #Sys.sleep(rnorm(1, mean = 5, sd = 2))#sleep a random amount of time to not hit rate limit
  }
  return(tbl)
}

page <- read_html(year_urls[[1]])
course_attributes <- page %>%
  html_elements(".width a") %>%
  html_attrs()
tbl <- tibble(attr = course_attributes) %>%
  unnest_wider(attr)
tbl$fall <- fall[[j]]
tbl$spring <- spring[[j]]
#did i accidentally duplicate the first page? make sure to dedupe and make sure it's only the first page

get_catalog_page <- function(url, page_num){
  url = paste(url,"&filter%5Bcpage%5D=",
              page_num, sep = "")
  page <- read_html(url)
  # num_pages <- page %>%
  #   html_elements("a:nth-child(14)") %>%
  #   html_text()
  #get page 1 attributes
  course_attributes <- page %>%
    html_elements(".width a") %>%
    html_attrs()
  course_attributes <- tibble(attr = course_attributes) %>%
    unnest_wider(attr)#unnest data
  return(course_attributes)
  
}

#Gets each historical catalog in year_urls  
for(j in 1:length(year_urls)){
  # catalog_list <- get_catalog_page(year_urls[[j]], 1)
  # Sys.sleep(120)
 
  #Get First page of catalog j (for one academic year)
  page <- read_html(year_urls[[j]])
  course_attributes <- page %>%
    html_elements(".width a") %>%
    html_attrs()
  temp_tbl <- tibble(attr = course_attributes) %>%
    unnest_wider(attr)#unnest data
  temp_tbl$fall <- fall[[j]]
  temp_tbl$spring <- spring[[j]]
  num_pages <- page %>%
    html_elements("a:nth-child(14)") %>%
    html_text()
  log_info("Scraped catalog page 1 for year ", fall[[j]], "-", spring[[j]], sep = "")
  log_info(paste("First course title on page:", temp_tbl[[3]][[1]]))
  Sys.sleep(120)
  
  for(i in 2:num_pages){
    temp_tbl <- get_catalog_page(year_urls[[j]], page = i)
    temp_tbl$fall <- fall[[j]]
    temp_tbl$spring <- spring[[j]]    
    tbl = bind_rows(tbl, temp_tbl)
    log_info("Catalog page ", i," for year ", fall[[j]], "-", spring[[j]], sep = "")
    log_info(paste("First course title on page:", temp_tbl[[3]][[1]]))

    Sys.sleep(120)
  }
  
  
}
#24 pages missed? I should have saved the numbers of those pages!!!!
#But I can find which pages from console
save(file = "more_scrapes.RData", catalog_list)



# catalog_list <- catalog_list %>%
#   filter(!is.na(fall))
# catalog_list3 <- catalog_list
# pages_missed = list(c(13,20,25), c(10,28), c(9,25,27), c(18,24), c(12,25,34), c(25,28), c(21,23,25,26,27), c(36), c(1,11,29),c(5,6,30))
# #page_nums of years in order (only need to run this once probably, but if reused implement better error handling)
# for(j in 1:10){
#   for(i in 1:length(pages_missed[[j]])){#not right yet
#     catalog_page <- get_catalog_page(year_urls[j], page = pages_missed[[j]][[i]])
#     catalog_list2$fall <- fall[j]
#     catalog_list2$spring <- spring[j]
#     catalog_list3 <- bind_rows(catalog_list3, catalog_list2)
#     print(paste("catalog page for ", fall[j],"-",spring[j], " downloaded.", sep = ""))
#     Sys.sleep(120)
#     #print(paste("j:", j, "i:",i))
#     print(pages_missed[[j]][[i]])
#     #print(year_urls[j])
#   }
#   
# }
# 
# catalog_list3 %>%
#   distinct()
write_csv(tbl, "past_catalogs.csv")
save(tbl, file = "past_catalogs2.RData")
test <- tbl %>%
  filter(href == "preview_course_nopop.php?catoid=3&coid=7992")

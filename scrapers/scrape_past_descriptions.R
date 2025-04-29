if (!require("pacman")) install.packages("pacman")
pacman::p_load(pacman, tidyverse, rvest, logger, data.table, httr)

df <- fread("past_catalogs.csv")
df <- df %>%
  select(href, title, fall, spring)
df <- df %>%
  mutate(url = paste0("https://catalog.american.edu/", href))
ua <- user_agent("occ-crawler")
occ_session <- session(df$url[[1]], ua)
df$description <- NA

get_description <- function(url){
  occ_session = session_jump_to(occ_session, url)
  page = read_html(occ_session)
  description <- page %>%
    html_elements(css = ".block_content") %>%
    html_text()
  return(description)
}
i=1
for(i in 9126:nrow(df)){
  description = get_description(df$url[[i]])
  message("Scraped page ",i," ", df$url[[i]], " Successful: ", as.logical(length(description)))
  df$description[[i]] = ifelse(length(description)>0, description, NA)
  #Sys.sleep(15)
  Sys.sleep(1)#SORRYYYYY (pls don't lock me out)
  
}

fwrite(df, "past_descriptions1.csv")

#checking it worked
#df2 <- fread("past_descriptions1.csv")

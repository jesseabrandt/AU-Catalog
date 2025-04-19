pacman::p_load(tidyverse, data.table)

df <- fread("catalog_basic_info.csv") #read data
all_cols <- names(df)#save colnames as reminder of what is available

nodes <- df[,c("title", "description", "dept", "course_num", "course_name", "min_credits",
               "max_credits", "offered", "grading", "note")]
nodes <- nodes[,course_abbrev := paste(dept, formatC(course_num, width=3, flag = 0), sep = "-")]
#unique(df$requirement)

df <- df %>%
  select(title, dept, course_num, course_name, crosslist, prerequisite, concurrent, corequisite)
df <- df %>%
  mutate(course_abbrev = paste(dept, formatC(course_num, width=3, flag = 0), sep = "-")) 


#crosslists$crosslist = gsub(" ", "", crosslists$crosslist) #remove spaces - but it doesn't work???

crosslists <- tibble(crosslist = str_match_all(df$crosslist, "[A-Z]*-\\d\\d\\d")) %>%
  unnest_wider(crosslist, names_sep = "_")
prerequisites <- tibble(prerequisite = str_match_all(df$prerequisite, "[A-Z]*-\\d\\d\\d")) %>%
  unnest_wider(prerequisite, names_sep = "_")
concurrents <- tibble(concurrent = str_match_all(df$concurrent, "[A-Z]*-\\d\\d\\d")) %>%
  unnest_wider(concurrent, names_sep = "_")
corequisites <- tibble(corequisite = str_match_all(df$corequisite, "[A-Z]*-\\d\\d\\d")) %>%
  unnest_wider(corequisite, names_sep = "_")

df2 <- bind_cols(df, crosslists, prerequisites, corequisites, concurrents)
#names(df2) = gsub("\\[,1\\]", "", names(df2)) # not sure why the names look weird in View()

ties <- df2 %>%
  melt(variable.name = "type", measure.vars = 10:37, value.name = "to")
ties <- ties[!is.na(to)]
ties$type <- gsub("_\\d*","",ties$type)
ties <- ties[,c("course_abbrev", "type", "to")]
ties <- ties[,.(from=course_abbrev, type, to)]
fwrite(nodes, "AU_Course_Nodes.csv")
fwrite(ties, "AU_Course_Edges.csv")




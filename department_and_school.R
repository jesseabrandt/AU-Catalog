library(tidyverse)
library(dplyr)
library(plyr)

all_catalogs3 <- read_csv("all_catalogs2.csv") ## reading csv

## selecting dept
department <- all_catalogs3 |>
  select(dept) 

## adding number column
department <- department |>
  count()

## School of Arts and Sciences
cas <- tribble(
  ~dept, ~dept_name,
  "CAS", "School of Arts and Sciences",
  "PUBH", "Public Health Program",
  "AMST", "American Studies Program",
  "ANTH", "Department of Anthropology",
  "AWST", "Arab World Studies",
  "ARTS", "Department of Art",
  "GDES", "Department of Art",
  "ARTH", "Department of Art",
  "PSM", "Department of Biology",
  "BIO", "Department of Biology",
  "BEHS", "Department of Biology and Chemistry", ## both
  "CHEM", "Department of Chemistry",
  "CSC", "Department of Computer Science",
  "GAME", "Department of Computer Science",
  "CRGC", "Department of Critical Race, Gender & Culture Studies",
  "AFAM", "Department of Critical Race, Gender & Culture Studies",
  "ASIA", "Department of Critical Race, Gender & Culture Studies",
  "LTST", "Department of Critical Race, Gender & Culture Studies",
  "APDS", "Department of Critical Race, Gender & Culture Studies",
  "ECON", "Department of Economics", 
  "ENVS", "Department of Environmental Science",
  "HLTH", "Health Studies",
  "HPRM", "Health Studies", 
  "HIST", "Department of History",
  "JWST", "Jewish Studies Program", 
  "ISR", "Jewish Studies Program", 
  "LIT", "Department of Literature",
  "WRT", "Department of Literature",
  "WRTG", "Department of Literature",
  "STAT", "Department of Mathematics and Statistics",
  "DATA", "Department of Mathematics and Statistics",
  "MATH", "Department of Mathematics and Statistics", ## Administered by same dept
  "NEUR", "Department of Neuroscience",
  "ATEC", "Department of Performing Arts", ## Administered by same dept
  "DNCE", "Department of Performing Arts",
  "AMGT", "Department of Performing Arts",
  "MUS", "Department of Performing Arts",
  "THTR", "Department of Performing Arts",
  "PERF", "Department of Performing Arts",
  "PHIL", "Department of Philosophy and Religion",
  "RELG", "Department of Philosophy and Religion",
  "PHYS", "Department of Physics",
  "PSYC", "Department of Psychology",
  "SOCY", "Department of Sociology",
  "WGSS", "Women's, Gender and Sexuality Studies Program", ## https://www.american.edu/directory/index.cfm?d=departments&id=1137 doesnt link properly to correct site
  "WLC", "World Languages & Cultures",
  "ARAB", "World Languages & Cultures",
  "CHIN", "World Languages & Cultures",
  "LING", "World Languages & Cultures",
  "TESL", "World Languages & Cultures", ## Could be considered misc
  "FREN", "World Languages & Cultures",
  "GERM", "World Languages & Cultures",
  "JAPN", "World Languages & Cultures",
  "RUSS", "World Languages & Cultures",
  "SPAN", "World Languages & Cultures",
  "KOR", "World Languages & Cultures",
  "PORT", "World Languages & Cultures",
  "HEBR", "World Languages & Cultures",
  "ITAL", "World Languages & Cultures",
  "PERS", "World Languages & Cultures",
  "SWAH", "World Languages & Cultures",
  "TEFL", "World Languages & Cultures",
) |>
  mutate(school = "cas")

## School of Education
soe <- tribble(
  ~dept, ~dept_name,
  "EDU", "School of Education"
  ) |>
  mutate(school = "soe")

## Kogod School of Business
kogod <- tribble(
  ~dept, ~dept_name,
  "KSB", "Kogod School of Business",
  "ACCT", "Department of Accounting",
  "FIN", "Department of Finance and Real Estate",
  "REAL", "Department of Finance and Real Estate",
  "IBUS", "Department of International Business",
  "ITEC", "Department of Information Technology and Analytics",
  "MGMT", "Department of Management",
  "MKTG", "Department of Marketing",
) |>
  mutate(school = "kogod")

## school of international service
sis <- tribble(
  ~dept, ~dept_name,
  "SISU", "School of International Service",
  "SIS", "School of International Service",
  "SISA", "School of International Service",
  "SISG", "School of International Service",
) |>
  mutate(school = "sis")

## school of communications
comm <- tribble(
  ~dept, ~dept_name,
  "COMM", "School of Communication"
) |>
  mutate(school = "comm")

## school of public affairs
spa <- tribble(
  ~dept, ~dept_name,
  "GOVT", "Department of Government",
  "SPA", "Department of Public Administration and Policy",
  "PUAD", "Department of Public Administration and Policy",
  "JLC", "Department of Justice, Law and Criminology",
) |>
  mutate(school = "spa")

## misc
misc <- tribble(
  ~dept, ~dept_name,
  "CORE", "Various Departments",
  "GNED", "Various Departments",
  "APM", "Office of Graduate & Professional Studies",
  "ELTA", 'Provost',
  "HCS", "Office of Graduate & Professional Studies",
  "HRAM", "Office of Graduate & Professional Studies",
  "WSEM", "Washington Semester Program",
  "HFIT", "Recreational Sports & Fitness",
  "HNRS", "Honors Program",
  "IDIS", "Interdisciplinary Studies",
  "ISCI", "Interdisciplinary Studies",
  "SABD", "Study Abroad",
  "SPEX", "Various Departments", 
  "UGST", "Various Departments",
  "UCOL", "Various Departments",
  "SAM", "Office of Graduate & Professional Studies",
  "PME", "Office of Graduate & Professional Studies",
  "PROF", "Office of Graduate & Professional Studies",
  "IDLA", "Office of Graduate & Professional Studies",
  "IGP", "International Gateway Program",
  "OGIS", "Various Departments",
)|>
  mutate(school = "misc")

schools <- bind_rows(cas, soe, kogod, comm, sis, spa, misc)

department_with_schools_count <- department |>
  left_join(schools, by = "dept")

all_catalogs3 <- all_catalogs3 |>
  left_join(schools, by = "dept")

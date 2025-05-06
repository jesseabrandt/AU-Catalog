library(tidyverse)


# source

school_budget <- tribble(
  ~year, ~budget, ~school,
  2014, 45319, "School of Arts and Sciences", # combined with SOE until 2019
  2014, 22367, "Kogod School of Business",
  2014, 13675, "School of International Service",
  2014, 11305, "School of Communications",
  2014, 15439, "School of Public Affairs",
  
  2015, 45319, "School of Arts and Sciences", 
  2015, 23864, "Kogod School of Business",
  2015, 13675, "School of International Service",
  2015, 11305, "School of Communications",
  2015, 15439, "School of Public Affairs",
  # https://www.american.edu/finance/budget/upload/budget-report-fy2014-and-fy2015.pdf
  
  2016, 49263, "School of Arts and Sciences", 
  2016, 23533, "Kogod School of Business",
  2016, 16622, "School of International Service",
  2016, 11840, "School of Communications",
  2016, 16053, "School of Public Affairs",
  
  2017, 48774, "School of Arts and Sciences", 
  2017, 25626, "Kogod School of Business",
  2017, 16466, "School of International Service",
  2017, 11720, "School of Communications",
  2017, 15891, "School of Public Affairs",
  
  # https://www.american.edu/finance/budget/upload/budget-report-fy2016-and-fy2017.pdf
  
  2018, 50578, "School of Arts and Sciences", 
  2018, 23441, "Kogod School of Business",
  2018, 17435, "School of International Service",
  2018, 12009, "School of Communications",
  2018, 16424, "School of Public Affairs",
  
  2019, 50578, "School of Arts and Sciences", 
  2019, 24918, "Kogod School of Business",
  2019, 17435, "School of International Service",
  2019, 12009, "School of Communications",
  2019, 16424, "School of Public Affairs",
  
  # https://www.american.edu/finance/budget/upload/budget-report-fy2018-and-fy2019.pdf
  
  2020, 51516, "School of Arts and Sciences", 
  2020, 2209, "School of Education",
  2020, 25757, "Kogod School of Business",
  2020, 17991, "School of International Service",
  2020, 12339, "School of Communications",
  2020, 16831, "School of Public Affairs",
  
  2021, 51175, "School of Arts and Sciences", 
  2021, 2198, "School of Education",
  2021, 26839, "Kogod School of Business",
  2021, 18033, "School of International Service",
  2021, 12237, "School of Communications",
  2021, 16831, "School of Public Affairs",
  
  # cant fucking find the actual budget allocation for 2022.
  # https://www.american.edu/finance/budget/upload/budget-report-fy2022.pdf
  
  # https://www.american.edu/finance/budget/upload/budgetreport_fys2023-2024.pdf
  
  2023, 55305, "School of Arts and Sciences", 
  2023, 3554, "School of Education",
  2023, 28439, "Kogod School of Business",
  2023, 19316, "School of International Service",
  2023, 13548, "School of Communications",
  2023, 22756, "School of Public Affairs",
  
  2024, 55315, "School of Arts and Sciences", 
  2024, 3554, "School of Education",
  2024, 30769, "Kogod School of Business",
  2024, 19316, "School of International Service",
  2024, 13548, "School of Communications",
  2024, 22603, "School of Public Affairs",
)

library(tidyverse)
library(ggplot2)
library(dplyr)


budget_allocation <- read_csv("budget_allocation.csv")
budget_allocation <- budget_allocation[ , -1]
all_catalogs3 <- read_csv("all_catalogs3.csv")
all_catalogs3 <- all_catalogs3[ , -1]

budget_allocation_by_school <- ggplot(budget_allocation, aes(x = year, y = budget, color = school)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Budget Allocation by School (2014–2024)",
    x = "Year",
    y = "Budget (in thousands)",
    color = "School"
  ) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    axis.title = element_text(size = 12),
    legend.position = "bottom"
  )

budget_pct_change <- budget_allocation |>
  filter(year %in% c(2014, 2024)) |> # modify these to use in shiny app
  pivot_wider(names_from = year, values_from = budget, names_prefix = "year_") |>
  mutate(
    pct_increase = (year_2024 - year_2014) / year_2014 * 100) |>
  select(school, year_2014, year_2024, pct_increase) |>
  arrange(desc(pct_increase))

print(budget_pct_change)


school_department_count <- all_catalogs3 |>
  filter(fall == 2014) |> # change this year value for shiny app customization
  group_by(school, dept_name) |> 
  tally()



ggplot(school_department_count, aes(x = reorder(dept_name, n), y = n, fill = school)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Number of Classes by Department and School",
    x = "Department",
    y = "Number of Classes",
    fill = "School"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 4),  
    axis.text.y = element_text(size = 8)  
  ) # way to many departments to be readable, sorry. Maybe somehow make the interactable filter by school too idk. 

ggplot(school_department_count, aes(x = reorder(dept_name, n), y = n, fill = school)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ school, scales = "free_x") +
  labs(
    title = "Number of Classes by Department and School",
    x = "Department",
    y = "Number of Classes"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 6)
  ) # basically what the interactable would look like




school_count <- all_catalogs3 |>
  filter(fall == 2014) |> # change this year value for shiny app customization
  group_by(school) |> # you can also use (acedmic_year if you want, just using fall bc im lazy)
  tally()

ggplot(school_count, aes(x = reorder(school, n), y = n, fill = school)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Number of Classes by School",
    x = "School",
    y = "Number of Classes",
    fill = "School"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y = element_text(size = 8)
  )


school_count <- all_catalogs3 |>
  filter(fall == 2014 & school != "misc") |> # change this year value for shiny app customization
  group_by(school, fall) |> 
  tally() |>
  mutate(year = fall) |>
  select(-fall)

school_count_budget <- budget_allocation |>
  filter(year == 2014 & school != "misc") # change this year value for shiny app customization

classes_by_school <- left_join(school_count, school_count_budget, by = "school")

classes_by_school <- classes_by_school |>
  select(-year.y) |> # during 2014-2014, school of education classes should count as college of arts and science
  mutate(budget_divided_by_classes = budget/n)






school_count_2 <- all_catalogs3 |>
  filter(school != "misc") |>
  group_by(school, fall) |> 
  tally() |>
  mutate(year = fall) |>
  select(-fall)

school_count_budget_2 <- budget_allocation |>
  filter( school != "misc")

classes_by_school_2 <- left_join(school_count_2, school_count_budget_2, by = c("school", "year"))

classes_by_school_2 <- classes_by_school_2 |>
  mutate(budget_divided_by_classes = budget/n)

ggplot(classes_by_school_2, aes(x = year, y = budget_divided_by_classes, color = school, fill = school)) +  
  geom_line() +
  geom_point(shape = 21, size = 1) +  
  labs(
    title = "Budget Divided by Classes Over Time",
    x = "Year",
    y = "Budget per Class"
  ) +
  scale_color_brewer(palette = "Set2") + 
  scale_fill_brewer(palette = "Set2") +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  

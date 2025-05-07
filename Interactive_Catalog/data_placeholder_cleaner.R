library(tidyverse)
library(ggplot2)

data_placeholder3 <- read.csv("data_placeholder3.csv")

data_placeholder3$description <- sub(";", "", data_placeholder3$description)
data_placeholder3$description <- sub(";;;;;", "", data_placeholder3$description)
data_placeholder3$description <- sub(". |", ".", data_placeholder3$description)

head(data_placeholder3)

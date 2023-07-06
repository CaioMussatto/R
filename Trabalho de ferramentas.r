library(ggplot2)
library(dplyr)

covid2020 <- read.csv("cases-brazil-cities-time_2020.csv")
covid2021 <- read.csv("cases-brazil-cities-time_2021.csv")

colnames(covid2020)

covid2020 <- covid2020 %>%
    group_by("city") %>%
    select(c("date", "city", "deaths"))

covid2020 <- covid2020 %>% filter(date > "2020-05-1" & date < "2020-05-31")

covid2020

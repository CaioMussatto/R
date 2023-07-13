library(ggplot2)
library(dplyr)
library(patchwork)
library(tidyverse)
library(xlsx)
library(mapdata)
library(maps)
library(lubridate)


covid2020 <- read.csv("cases-brazil-cities-time_2020.csv")
covid2021 <- read.csv("cases-brazil-cities-time_2021.csv")

colnames(covid2020)

covid2020 <- covid2020 %>%
    select(c("date", "state", "city", "deaths"))

covid2020 <- covid2020 %>% filter(date >= "2020-05-1" & date <= "2020-05-31")

covid2020$date <- ymd(covid2020$date)

Norte <- c("AM", "RR", "AP", "PA", "TO", "RO", "AC")
Nordeste <- c("MA", "PI", "CE", "RN", "PE", "PB", "SE", "AL", "BA")
Centro <- c("MT", "MS", "GO")
Sudeste <- c("SP", "RJ", "ES", "MG")
Sul <- c("PR", "RS", "SC")

covid2020$regiao <- NA

regiao_nordeste <- covid2020$state %in% Nordeste
regiao_norte <- covid2020$state %in% Norte
regiao_centro <- covid2020$state %in% Centro
regiao_sudeste <- covid2020$state %in% Sudeste
regiao_sul <- covid2020$state %in% Sul

covid2020$regiao[regiao_nordeste] <- "Nordeste"
covid2020$regiao[regiao_norte] <- "Norte"
covid2020$regiao[regiao_centro] <- "Centro-Oeste"
covid2020$regiao[regiao_sudeste] <- "Sudeste"
covid2020$regiao[regiao_sul] <- "Sul"

grafico <- function(filtro, tipo) {
    filtro <- str_to_title(filtro)
    covid2020 <- subset(covid2020, regiao == filtro)
    if (tipo == "coluna") {
        imagem <- ggplot(covid2020, aes(state, deaths, fill = state)) +
            geom_col() +
            ylim(NA, 29400)
    } else if (tipo == "linha") {
        imagem <- ggplot(covid2020, aes(date, deaths, color = state)) +
            geom_line() +
            facet_wrap(~state) +
            scale_x_date(date_labels = "%d") +
            ylim(NA, 2000)
    } else {
        print("Os tipos aceitos são 'coluna' e 'linha'")
    }
    return(imagem)
}

grafico("nordeste", "linha")

grafico("nordeste", "coluna")

grafico("norte", "linha")

grafico("norte", "coluna")

grafico("centro-oeste", "linha")

grafico("centro-oeste", "coluna")

grafico("sudeste", "linha")

grafico("sudeste", "coluna")

grafico("sul", "linha")

grafico("sul", "coluna")

worldmap <- map_data("world")

Brazil <- worldmap[which(worldmap$region == "Brazil"), ]

Brazil_map <- ggplot() +
    geom_polygon(
        data = Brazil,
        aes(x = long, y = lat, group = group),
        fill = "white", color = "black"
    ) +
    coord_fixed(ratio = 1.3, xlim = c(-74, -34), ylim = c(-33, 4)) +
    theme_minimal()
Brazil_map

cordenadas_estados <- read.csv("estados.csv")

teste <- covid2020 %>% full_join(cordenadas_estados, by = c("regiao"), multiple = "all")

teste2 <- teste %>%
    group_by(state) %>%
    mutate(media_morte = mean(deaths))

head(teste2$media_morte)

teste$discrete <- cut(teste$media_morte, breaks = 3, labels = c(0, 50, 1000))

a <- Brazil_map +
    geom_point(
        data = teste2,
        aes(
            x = as.numeric(longitude),
            y = as.numeric(latitude), size = media_morte, color = media_morte
        )
    ) + theme(legend.position = "right") + theme(title = element_text(size = 5))


# 2021

colnames(covid2021)

covid2021 <- covid2021 %>%
    select(c("date", "state", "city", "deaths"))

covid2021 <- covid2021 %>% filter(date >= "2021-05-1" & date <= "2021-05-31")

covid2021$date <- ymd(covid2021$date)

Norte <- c("AM", "RR", "AP", "PA", "TO", "RO", "AC")
Nordeste <- c("MA", "PI", "CE", "RN", "PE", "PB", "SE", "AL", "BA")
Centro <- c("MT", "MS", "GO")
Sudeste <- c("SP", "RJ", "ES", "MG")
Sul <- c("PR", "RS", "SC")

covid2021$regiao <- NA

regiao_nordeste <- covid2021$state %in% Nordeste
regiao_norte <- covid2021$state %in% Norte
regiao_centro <- covid2021$state %in% Centro
regiao_sudeste <- covid2021$state %in% Sudeste
regiao_sul <- covid2021$state %in% Sul

covid2021$regiao[regiao_nordeste] <- "Nordeste"
covid2021$regiao[regiao_norte] <- "Norte"
covid2021$regiao[regiao_centro] <- "Centro-Oeste"
covid2021$regiao[regiao_sudeste] <- "Sudeste"
covid2021$regiao[regiao_sul] <- "Sul"

grafico <- function(filtro, tipo) {
    filtro <- str_to_title(filtro)
    covid2021 <- subset(covid2021, regiao == filtro)
    if (tipo == "coluna") {
        imagem <- ggplot(covid2021, aes(state, deaths, fill = state)) +
            geom_col() +
            ylim(NA, 29400)
    } else if (tipo == "linha") {
        imagem <- ggplot(covid2021, aes(date, deaths, color = state)) +
            geom_line() +
            facet_wrap(~state) +
            scale_x_date(date_labels = "%d") +
            ylim(NA, 2000)
    } else {
        print("Os tipos aceitos são 'coluna' e 'linha'")
    }
    return(imagem)
}

grafico("nordeste", "linha")

grafico("nordeste", "coluna")

grafico("norte", "linha")

grafico("norte", "coluna")

grafico("centro-oeste", "linha")

grafico("centro-oeste", "coluna")

grafico("sudeste", "linha")

grafico("sudeste", "coluna")

grafico("sul", "linha")

grafico("sul", "coluna")

worldmap <- map_data("world")

Brazil <- worldmap[which(worldmap$region == "Brazil"), ]

Brazil_map <- ggplot() +
    geom_polygon(
        data = Brazil,
        aes(x = long, y = lat, group = group),
        fill = "white", color = "black"
    ) +
    coord_fixed(ratio = 1.3, xlim = c(-74, -34), ylim = c(-33, 4)) +
    theme_minimal()
Brazil_map

cordenadas_estados <- read.csv("estados.csv")

teste <- covid2021 %>% full_join(cordenadas_estados, by = c("regiao"), multiple = "all")

teste2 <- teste %>%
    group_by(state) %>%
    mutate(media_morte = mean(deaths))

head(teste2$media_morte)

a <- Brazil_map +
    geom_point(
        data = teste2,
        aes(
            x = as.numeric(longitude),
            y = as.numeric(latitude), size = media_morte, color = media_morte
        )
    ) + theme(legend.position = "right") + theme(title = element_text(size = 5))


ggsave("a.png", a, width = 10, height = 10, dpi = 300)

head(teste2)

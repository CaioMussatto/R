library(dplyr)
library(stringr)
dados <- read.csv("Pokemon_full.csv")
df_grass <- filter(dados, type == "grass")
df_grass


dados %>% filter(type == "grass")

df_fogo_e_agua <- dados %>% filter(type == "fire" | type == "water")

dados %>% filter(grepl("fly", name))

dados %>% filter(grepl("bee", name) | grepl("saur", name))

dados[str_detect(dados$name, "bee|saur"), ]

dados %>% head()

# Pull para retornar um vetor.
dados %>%
    filter(type == "fire") %>%
    pull(secundary.type) %>%
    unique()

# ou dados2 <- dados[dados$type == 'fire',]
# unique(dados2$secundary.tipe)

# ou unique(dados[dados$type == 'fire',]$secundary.tipe)

# Select para selecionar colunas - retorna um dataframe, e não um vetor
dados %>%
    select(type, secundary.type) %>%
    unique()

dados %>% select(c(1, 2, 3)) # Numero da coluna

dados %>% select(name, type, height) # Nome da coluna

# Mutate cria ou modifica uma coluna

dados %>%
    mutate(height2 = 2 * height, bee = grepl("bee", name)) %>%
    head() # Criando uma nova

dados %>%
    mutate(speed = 2 * speed) %>%
    head() # Modificando uma coluna

# Arrange coloca o data frame em ordem

dados %>%
    arrange(name) %>%
    head() # se adicionar o desc ele ordena de forma decrescente

dados %>%
    arrange(desc(name), height) %>%
    head()

# Sumarise agrega as colunas e coloca dentro da função

dados %>% summarise(media_altura = mean(height), media_peso = mean(weight))

dados %>%
    group_by(type) %>%
    summarise(media_altura = mean(height), media_peso = mean(weight), N = n()) %>%
    arrange(media_altura) # o N retorna quantas ocorrencias tem

# Filtrando os pokemons que tem o peso acima da media do seu type

dados %>%
    group_by(type) %>%
    mutate(
        media_altura = mean(height)
    ) %>%
    filter(height > media_altura) %>%
    select(-media_altura) <- df

write.csv(df, file = "df.csv")


dados %>%
    group_by(type) %>%
    mutate(
        media_altura = mean(height),
        media_peso = mean(weight)
    ) %>%
    filter(height > media_altura, weight > media_peso) %>%
    select(-media_altura) %>%
    ungroup() %>%
    rowwise() %>%
    mutate(imc = weight / height * 2, mm = sum(weight)) -> df

write.csv(df, file = "df.csv")

# Adicionado um 'sobrenome'
dados %>%
    mutate((name <- paste(name, "- NOVO"))) %>%
    head()

# Criando uma funçã com if e else

f <- function(x) {
    if (x <= 15) {
        return(("Executei essa ação"))
    } else {
        return(("Executei Aquela ação "))
    }
}

f(20)

# Rowwise serve para passar linha por linha como valor
dados %>%
    rowwise() %>%
    mutate(nova_var = f(height)) %>%
    select(height, nova_var) %>%
    head(30)

# Lição Z-score

df <- dados %>%
    rowwise() %>%
    group_by(type) %>%
    mutate(Z_score = (height - mean(height)) / sd(height))

library(ggplot2)

dados %>%
    pull(type) %>%
    unique()

ggplot(df) +
    geom_density(aes(x = Z_score, color = type))

# Renomear colunas e movendo elas - rename e relocate

dados %>%
    group_by(type) %>%
    summarise(
        media_altura = mean(height),
        media_peso = mean(weight), N = n()
    ) %>%
    arrange(media_altura) %>%
    rename("Número de pokemons" = N) %>%
    relocate("Número de pokemons")

# Ifelse

dados %>%
    mutate(tamanho = ifelse(height < 15, "baixinho", "altão")) %>%
    head()

# Posso fazer uma função para isso

ff <- function(y) {
    resposta <- c()
    for (i in 1:length(y)) {
        if (y[i] <= 15) {
            resposta[i] <- "baixinho"
        } else {
            resposta[i] <- "altão"
        }
    }
    return(resposta)
}

dados %>%
    mutate(tamanho = ff(height)) %>%
    head()

# case_when

dados %>%
    mutate(tamanho = case_when(
        height < 5 ~ "baixinho",
        height < 10 ~ "pequeno",
        height < 15 ~ "médio",
        TRUE ~ "altão"
    )) %>%
    head()

# rbind e cbind
df_A <- data.frame(A = c(1, 2, 3, 4), B = c(5, 6, 3, 2))
df_B <- data.frame(A = c(12, 22, 32, 42), B = c(7, 5, 3, 2))

rbind(df_A, df_B)

df_A <- data.frame(A = c(1, 2, 3, 4), B = c(5, 6, 3, 2))
df_B <- data.frame(A = c(12, 22, 32, 42), C = c(7, 5, 3, 2))

bind_rows(df_A, df_B)

df_A <- data.frame(A = c(1, 2, 3, 4))
df_B <- data.frame(B = c(12, 22, 32, 42))

cbind(df_A, df_B)

# Join - juntar dois dataframe

df_means <- dados %>%
    group_by(type) %>%
    summarise(media_h = mean(height), media_w = mean(weight))

df_means %>% filter(!grepl("^g", type)) # Excluindo quem começa com g (o ^ indica isso) na coluna type

novo_grupo <- data.frame(
    type = "bug",
    media_h = 10,
    media_w = 800
)

df_means <- rbind(df_means, novo_grupo)

df_means

df <- full_join(dados, df_means, by = "type")
df

# Full junta todo mundo, insere NA onde não da 'match'
# Inner em comum
# Left - mantem o dataframe da esquerda, se não tem 'match' com o dataframe da direita, completa com NA


df_means <- dados %>%
    group_by(type, secundary.type) %>%
    summarise(media_h = mean(height), media_w = mean(weight))

df <- right_join(dados, df_means, by = c("type", "secundary.type"))

df

library(tidyr)

dados <- readr::read_rds("ferramentasdemodelagem/R/Dados/imdb.rds")

df <- dados %>% select(titulo, orcamento, receita, receita_eua)

df_long <- df %>%
    slice(1:10) %>%
    tidyr::pivot_longer(2:4, values_to = "valor", names_to = "Tipo de valor")

df_long <- df %>%
    slice(1:10) %>%
    tidyr::pivot_longer(2:4, values_to = "valor", names_to = "Tipo de Valor")


library(ggplot2)

ggplot() +
    geom_col(
        data = df_long, aes(x = titulo, y = valor, fill = `Tipo de Valor`),
        position = position_dodge2()
    ) +
    theme_bw() +
    theme(
        axis.text.x = element_text(angle = 45, hjust = 1.0)
    )

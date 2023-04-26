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

dados %>%
    rowwise() %>%
    group_by(type) %>%
    mutate(Z_score = (height - mean(height)) / sd(height)) %>%
    head()

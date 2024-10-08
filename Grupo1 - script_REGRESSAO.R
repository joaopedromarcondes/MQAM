# Importar bibliotecas e dependências
library(readxl)
library(ggplot2)
library(dplyr)

# Definir funções importantes
moda <- function(x) {
  modal <- unique(x)
  modal[which.max(tabulate(match(x, modal)))]
}

# Importar banco de dados
BRAZIL_CITIES_REV2022_CSV <- read_excel("BRAZIL_CITIES_REV2022.CSV.xlsx")

# Selecionar colunas que serão usadas na análise
dados <- BRAZIL_CITIES_REV2022_CSV[, c("IDHM", "IBGE_RES_POP", "ALT", "AREA", "TAXES", "STATE", "Motorcycles", "Cars")]


# Tratar dados
# Remover os valores 0 e os valores menores que 0
# menos para altitude e estado, já que não faz sentido tirar desses
dados_negativos <- (dados[, c("IDHM", "IBGE_RES_POP", "AREA", "TAXES", "Motorcycles", "Cars")] <= 0)
dados <- dados[-which(dados_negativos) %% nrow(dados), ]

# IDH
# Remover outliers de IDH
outliers_idh <- boxplot.stats(dados$IDHM)$out
dados <- dados[-which(dados$IDHM %in% outliers_idh),]
boxplot(dados$IDHM)

# Altitude
boxplot(dados$ALT)

# População
dados$IBGE_RES_POP <- log(dados$IBGE_RES_POP)
# Remover dados infinitos
#dados <- dados[-which(is.infinite(dados$IBGE_RES_POP)), ]
boxplot(dados$IBGE_RES_POP)


# Área
# Amplitude muito alta dos dados
dados$AREA <- log(dados$AREA)
boxplot(dados$AREA)


# Impostos
# Amplitude muito alta dos dados
#which(dados$TAXES <= 0)
#dados <- dados[-which(dados$TAXES <= 0),]
dados$TAXES <- log(dados$TAXES)
boxplot(dados$TAXES)

# Motos
dados$Motorcycles <- log(dados$Motorcycles)
boxplot(dados$Motorcycles)




# Importar bibliotecas e dependências
# Verifica e instala o pacote pacman, se necessário
if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}

# Carrega o pacote pacman
library(pacman)

# Carrega os pacotes necessários, instalando-os se não estiverem presentes
pacman::p_load(dplyr, ggplot2, readxl, DT)


# Definir funções importantes
moda <- function(x) {
  modal <- unique(x)
  modal[which.max(tabulate(match(x, modal)))]
}

descrever_coluna <- function(x) {
  data.frame(
    Média = mean(x),
    Moda = moda(x),
    Mediana = median(x),
    "Variância" = var(x),
    "Desvio-Padrão" = sd(x)
  )
}


# Importar banco de dados
BRAZIL_CITIES_REV2022_CSV <- read_excel("BRAZIL_CITIES_REV2022.CSV.xlsx")

# Selecionar colunas que serão usadas na análise
dados <- BRAZIL_CITIES_REV2022_CSV[, c("IDHM", "IBGE_RES_POP", "ALT", "AREA", "TAXES", "STATE", "Motorcycles", "Cars")]



# Dados antes de tratar
datatable(sapply(select(dados, -STATE), descrever_coluna))
boxplot(select(dados, -STATE))
pie(sort(table(dados$STATE)))

# Tratar dados
# Remover os valores 0 e os valores menores que 0
# menos para altitude e estado, já que não faz sentido tirar desses
dados_negativos <- (dados[, c("IDHM", "IBGE_RES_POP", "AREA", "TAXES", "Motorcycles", "Cars")] <= 0)
dados <- dados[!apply(dados_negativos, 1, any), ]


# IDH


# População
dados$IBGE_RES_POP <- log(dados$IBGE_RES_POP)


# Área
# Amplitude muito alta dos dados
dados$AREA <- log(dados$AREA)


# Impostos
# Amplitude muito alta dos dados
dados$TAXES <- log(dados$TAXES)


# Motos
dados$Motorcycles <- log(dados$Motorcycles)


# Carros
dados$Cars <- log(dados$Cars)

# Visualização dos dados após o tratamento
datatable(sapply(select(dados, -STATE), descrever_coluna))
boxplot(select(dados, -STATE))
pie(sort(table(dados$STATE)))
sort(table(dados$STATE))
length(dados)
datatable(dados)

plot(dados$IDHM, dados$IBGE_RES_POP)
plot(dados$IDHM, dados$ALT)
plot(dados$IDHM, dados$AREA)
plot(dados$IDHM, dados$TAXES)
plot(dados$IDHM, dados$STATE)
plot(dados$IDHM, dados$Motorcycles)
plot(dados$IDHM, dados$Cars)


heatmap((cor(dados[, -6])))
heatmap(abs(cor(dados[, -6])))

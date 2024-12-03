# Importar bibliotecas e dependências
# Verifica e instala o pacote pacman, se necessário
if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}

library(pacman)

# Carrega os pacotes necessários, instalando-os se não estiverem presentes
pacman::p_load(dplyr, ggplot2, readxl, readr, DT, fastDummies, lmtest, tidyverse, psych, GPArotation, corrplot)

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

# Função para criar gráfico de frequência com intervalos personalizados
criar_grafico_frequencia <- function(dados, variavel, intervalo, titulo) {
  ggplot(dados, aes(x = .data[[variavel]])) +  # Usando .data[[variavel]] para acessar a coluna com nome variável
    geom_histogram(binwidth = intervalo, fill = "skyblue", color = "black", boundary = 0) +
    labs(title = titulo, x = variavel, y = "Frequência") +
    theme_minimal()
}

# Função para criar a tabela de distribuição de frequência com frequências absolutas e relativas acumuladas
criar_tabela_frequencia <- function(dados, variavel, intervalo) {
  # Criar os intervalos
  intervalos <- cut(dados[[variavel]], breaks = seq(min(dados[[variavel]], na.rm = TRUE),
                                                    max(dados[[variavel]], na.rm = TRUE),
                                                    by = intervalo),
                    include.lowest = TRUE, right = FALSE)

  # Frequências absolutas
  frequencias_absolutas <- table(intervalos)

  # Frequências relativas
  frequencias_relativas <- prop.table(frequencias_absolutas)
  # Criar a tabela final combinando frequências absolutas
  # e relativas com soma acumulada das frequências relativas
  tabela <- data.frame(
    Intervalo = names(frequencias_absolutas),
    Frequencia_Absoluta = as.vector(frequencias_absolutas),
    Frequencia_Relativa = round(as.vector(frequencias_relativas), 4),  # Frequência relativa com 4 casas decimais
    Soma_Frequencia_Relativa = round(cumsum(as.vector(frequencias_relativas)), 4)  # Soma acumulada das frequências relativas
  )
  return(tabela)
}

#Preparando o Dataset
tabela <- read_csv2("MQA2024–grupo01–dataset_ analisefatorial.csv")
dados <- tabela[, c("CITY", "IBGE_RES_POP", "IBGE_DU", "IBGE_PLANTED_AREA", "IBGE_CROP_PRODUCTION_$", "IDHM", "AREA", "RURAL_URBAN", "TAXES", "Cars", "Motorcycles")]
dados_numericos <- tabela[, c("IBGE_RES_POP", "IBGE_DU", "IBGE_PLANTED_AREA", "IBGE_CROP_PRODUCTION_$", "IDHM", "AREA", "TAXES", "Cars", "Motorcycles")]


# Contar quantas linhas estão sendo consideradas 
tamanho_dataset <- nrow(dados_numericos)

# Exibir o número de linhas restantes
cat("Número de linhas:", tamanho_dataset, "\n")

# Tratar dados
# Remover os valores 0 e os valores menores que 0

dados_negativos_ou_na <- (dados_numericos <= 0) | (is.na(dados_numericos))
length(which(apply(dados_negativos_ou_na, 1, any)))
dados <- dados[!apply(dados_negativos_ou_na, 1, any), ]
dados_numericos <- dados_numericos[!apply(dados_negativos_ou_na, 1, any), ]

# Contar quantas linhas sobraram após a remoção
tamanho_dataset_apos_remover_negativos <- nrow(dados)

# Exibir o número de linhas restantes
cat("Número de linhas restantes:", tamanho_dataset_apos_remover_negativos, "\n")

boxplot(dados[["IBGE_RES_POP"]])
boxplot(dados[["IBGE_DU"]])
boxplot(dados[["IBGE_PLANTED_AREA"]])
boxplot(dados[["IBGE_CROP_PRODUCTION_$"]])
boxplot(dados[["IDHM"]])
boxplot(dados[["AREA"]])
boxplot(dados[["TAXES"]])
boxplot(dados[["Cars"]])
boxplot(dados[["Motorcycles"]])

# Transformação Logarítmica
dados_numericos[, "IBGE_RES_POP"] <- log(dados_numericos[, "IBGE_RES_POP"])
dados_numericos[, "IBGE_DU"] <- log(dados_numericos[, "IBGE_DU"])
dados_numericos[, "IBGE_PLANTED_AREA"] <- log(dados_numericos[, "IBGE_PLANTED_AREA"])
dados_numericos[, "IBGE_CROP_PRODUCTION_$"] <- log(dados_numericos[, "IBGE_CROP_PRODUCTION_$"])
dados_numericos[, "AREA"] <- log(dados_numericos[, "AREA"])
dados_numericos[, "TAXES"] <- log(dados_numericos[, "TAXES"])
dados_numericos[, "Cars"] <- log(dados_numericos[, "Cars"])
dados_numericos[, "Motorcycles"] <- log(dados_numericos[, "Motorcycles"])

### BOXPLOTS DOS DADOS APÓS TRANSFORMACAO LOGARITMICA
boxplot(dados_numericos[["IBGE_RES_POP"]])
boxplot(dados_numericos[["IBGE_DU"]])
boxplot(dados_numericos[["IBGE_PLANTED_AREA"]])
boxplot(dados_numericos[["IBGE_CROP_PRODUCTION_$"]])
boxplot(dados_numericos[["AREA"]])
boxplot(dados_numericos[["TAXES"]])
boxplot(dados_numericos[["Cars"]])
boxplot(dados_numericos[["Motorcycles"]])

# Criar gráficos para cada variável
grafico1 <- criar_grafico_frequencia(dados_numericos, "IBGE_RES_POP", 2, "Frequência de IBGE_RES_POP")
grafico2 <- criar_grafico_frequencia(dados_numericos, "IBGE_DU", 2, "Frequência de IBGE_DU")
grafico3 <- criar_grafico_frequencia(dados_numericos, "IBGE_PLANTED_AREA", 2, "Frequência de IBGE_PLANTED_AREA")
grafico4 <- criar_grafico_frequencia(dados_numericos, "IBGE_CROP_PRODUCTION_$", 2, "Frequência de IBGE_CROP_PRODUCTION_$")
grafico5 <- criar_grafico_frequencia(dados_numericos, "AREA", 0.1, "Frequência de AREA")
grafico6 <- criar_grafico_frequencia(dados_numericos, "TAXES", 2, "Frequência de TAXES")
grafico7 <- criar_grafico_frequencia(dados_numericos, "Cars", 2, "Frequência de Cars")
grafico8 <- criar_grafico_frequencia(dados_numericos, "Motorcycles", 0.1, "Frequência de Motorcycles")

grid.arrange(grafico1, grafico2, grafico3, grafico4, grafico5, grafico6, grafico7, grafico8, ncol = 2)

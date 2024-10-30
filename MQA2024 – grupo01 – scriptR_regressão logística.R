####Instalacao das dependencias
if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}

library(pacman)
pacman::p_load(dplyr, ggplot2, readxl, readr, DT, fastDummies, lmtest, tidyverse)

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



#Preparando o Dataset
tabela <- read_csv("BRAZIL_CITIES.csv")
dados <- tabela[, c("IDHM", "TAXES", "IBGE_CROP_PRODUCTION_$", "AREA", "RURAL_URBAN")] # falta colocar as colunas usadas para esse teste...
dados_numericos <- tabela[, c("IDHM", "TAXES", "IBGE_CROP_PRODUCTION_$", "AREA")] # removendo rural_urban por ser qualitativa

# Dados antes de tratar
#datatable(sapply(select(dados, -RURAL_URBAN), descrever_coluna))
boxplot(select(dados, -RURAL_URBAN))
pie(sort(table(dados$RURAL_URBAN)))

# Tratar dados
# Remover os valores 0 e os valores menores que 0
dados_negativos <- dados[, c("IDHM", "IBGE_CROP_PRODUCTION_$", "AREA", "TAXES")] <= 0
dados <- dados[!rowSums(dados_negativos) > 0, ]

# Contar quantas linhas sobraram após a remoção
tamanho_dataset_apos_remover_negativos <- nrow(dados)

# Exibir o número de linhas restantes
cat("Número de linhas restantes:", tamanho_dataset_apos_remover_negativos, "\n")

boxplot(dados$IDHM)
boxplot(dados$TAXES)
boxplot(dados[["IBGE_CROP_PRODUCTION_$"]])
boxplot(dados$AREA)

# transformação logarítmica em TAXES
dados$TAXES <- log(dados$TAXES)

# transformação logarítmica em IBGE_CROP_PRODUCTION
dados[["IBGE_CROP_PRODUCTION_$"]] <- log(dados[["IBGE_CROP_PRODUCTION_$"]])

# transformação logarítmica em AREA
dados$AREA <- log(dados$AREA)

# Tratar dados novamente
# Remover os valores 0 e os valores menores que 0
dados_negativos <- dados[, c("IDHM", "IBGE_CROP_PRODUCTION_$", "AREA", "TAXES")] <= 0
dados <- dados[!rowSums(dados_negativos) > 0, ]

# Contar quantas linhas sobraram após a remoção
tamanho_dataset_apos_remover_negativos <- nrow(dados)

# Exibir o número de linhas restantes
cat("Número de linhas restantes:", tamanho_dataset_apos_remover_negativos, "\n")

#calculando as estatísticas descritivas das varíaveis
media <- sapply(dados, mean, na.rm = TRUE)
mediana <- sapply(dados, median, na.rm = TRUE)
variancia <- sapply(dados, var, na.rm = TRUE)
desvio_padrao <- sapply(dados, sd, na.rm = TRUE)
coef_var <- (desvio_padrao / media) * 100 

# Exibir os resultados em um data frame
estatisticas_descritivas <- data.frame(
  Media = media,
  Mediana = mediana,
  Variancia = variancia,
  DesvioPadrao = desvio_padrao,
  CoeficienteDeVariacao = coef_var
)

print(estatisticas_descritivas)

boxplot(dados$IDHM)
boxplot(dados$TAXES)
boxplot(dados[["IBGE_CROP_PRODUCTION_$"]])
boxplot(dados$AREA)

descrever_coluna(tabela$IDHM)
tabela$IDHM



###Tratando as variáveis

#Pensamos em tratar algumas variáveis (em relação ao tamanho da população)
#com isso deixando a visualização mais palpável dos nossos gráficos

#######################################################################

#Calculo do log-likelihood
log_likelihood <- function(x) {
  print("Hello")
}
log_likelihood

#Calculo do pseudo-R²
pseudo_r_quadrado <- function(x) {
  print("Hello")
}

#Tabela de Confusão, sensibilidade, especifidade e acurácia
tabela_confusao() <- function(x) {
  print("Hello")
}

#Tendencia de Recursos
grafico_tendencia_recursos <- function(x) {
  print("Hello")
}

#Autocorrelação dos resíduos e das variáveis independentes
grafico_autocorrelacao <- function(x) {

}

#Multicolinearidade das variáveis preditoras
multi <- function(x) {

}

#Coeficientes B0, B1, B2, ...

#######################################################################

#Saida da Regressão Logística


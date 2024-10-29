####Instalacao das dependencias
if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}

library(pacman)
pacman::p_load(dplyr, ggplot2, readxl, readr, DT, fastDummies, lmtest, tidyverse)


#Preparando o Dataset
tabela <- read_csv("MQA2024–grupo01–dataset_regressão logística.csv")
dados <- tabela[, c("")] # falta colocar as colunas usadas para esse teste...


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
  printf("Hello")
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
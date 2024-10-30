####Instalacao das dependencias
if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}

library(pacman)
pacman::p_load(dplyr, ggplot2, readxl, readr, DT, fastDummies, lmtest, tidyverse, car)

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
dados <- tabela[, c("IDHM", "TAXES", "IBGE_CROP_PRODUCTION_$", "AREA", "RURAL_URBAN")]
dados_numericos <- tabela[, c("IDHM", "TAXES", "IBGE_CROP_PRODUCTION_$", "AREA")] # removendo rural_urban por ser qualitativa

# Dados antes de tratar
#datatable(sapply(select(dados, -RURAL_URBAN), descrever_coluna))
boxplot(select(dados, -RURAL_URBAN))
pie(sort(table(dados$RURAL_URBAN)))

# Tratar dados
# Remover os valores 0 e os valores menores que 0

dados_negativos_ou_na <- (dados_numericos <= 0) | (is.na(dados_numericos))
length(which(dados_negativos_ou_na))
dados <- dados[!apply(dados_negativos_ou_na, 1, any), ]
summary(dados)
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

datatable(sapply(select(dados, -RURAL_URBAN), descrever_coluna))

# #calculando as estatísticas descritivas das varíaveis
# media <- sapply(dados, mean, na.rm = TRUE)
# mediana <- sapply(dados, median, na.rm = TRUE)
# variancia <- sapply(dados, var, na.rm = TRUE)
# desvio_padrao <- sapply(dados, sd, na.rm = TRUE)
# coef_var <- (desvio_padrao / media) * 100

# # Exibir os resultados em um data frame
# estatisticas_descritivas <- data.frame(
#   Media = media,
#   Mediana = mediana,
#   Variancia = variancia,
#   DesvioPadrao = desvio_padrao,
#   CoeficienteDeVariacao = coef_var
# )

#print(estatisticas_descritivas)

# Variáveis Quantitativas
boxplot(dados$IDHM)
boxplot(dados$TAXES)
boxplot(dados[["IBGE_CROP_PRODUCTION_$"]])
boxplot(dados$AREA)

# Variáveis Qualitativas
pie(sort(table(dados$RURAL_URBAN)))
sort(table(dados$RURAL_URBAN))



# Transformar qualitativas em DUMMY
# Fixa-se Intermediário Adjacente.
dados <- dummy_cols(dados, select_columns = "RURAL_URBAN", remove_first_dummy = TRUE)
dados <- select(dados, -RURAL_URBAN)


# Transformar IDH em Qualitativa Binária
dados$IDHM <- as.numeric(dados$IDHM > 0.7)



### MODELO REGRESSÃO ###
# Ajustando um modelo inicial com todas as variáveis
modelo_full <- glm(IDHM ~ ., data = dados, family = binomial)

# Resumo do modelo completo
cat("\nResumo do modelo completo:\n")
summary(modelo_full)

# Realizando a seleção stepwise
modelo_stepwise <- step(modelo_full, direction = "both", trace = TRUE)

# Resumo do modelo final após stepwise
cat("\nResumo do modelo após seleção stepwise:\n")
summary(modelo_stepwise)

# Verificando multicolinearidade no modelo final
vif_results <- vif(modelo_stepwise)
cat("\nResultados do VIF para o modelo final:\n")
print(vif_results)

# Identificando valores influentes
influence_measures <- influence.measures(modelo_stepwise)
cat("\nMedidas de influência para o modelo final:\n")
print(influence_measures)

# Calculando Log Likehood para o modelo
modelo_log_likelihood <- logLik(modelo_stepwise)
cat("\nLog Likelihood:\n")
print(modelo_log_likelihood)

# Ajustar o modelo nulo (sem preditores)
null_model <- glm(IDHM ~ 1, data = dados, family = binomial)

# Log-likelihood do modelo nulo
log_likelihood_null <- logLik(null_model)
cat("\nLog Likelihood:\n")
print(log_likelihood_null)
1-logLik(modelo_stepwise)/logLik(null_model)


# Calcular o pseudo R^2 de Cox-Snell
n <- nrow(dados)  # Número de observações
pseudo_R2_cox_snell <- 1 - ((log_likelihood_null / modelo_log_likelihood)^(2 / n))
pseudo_R2_cox_snell

# Calcular o pseudo R^2 de Nagelkerke
pseudo_R2_nagelkerke <- pseudo_R2_cox_snell / (1 - exp((2 / n) * log_likelihood_null))

# Exibir o valor do pseudo R^2 de Nagelkerke
cat("Pseudo R^2 de Nagelkerke:", pseudo_R2_nagelkerke, "\n")
cat("Log-Likelihood do modelo ajustado:", modelo_log_likelihood, "\n")
cat("Log-Likelihood do modelo nulo:", log_likelihood_null, "\n")



# Gerar previsões com probabilidade
probabilidades <- predict(modelo_stepwise, type = "response")
plot(sort(probabilidades))
# Definir o limiar para classificação binária (0.5, por exemplo)
previsoes <- ifelse(probabilidades > 0.6, 1, 0)

# Criar a matriz de confusão
matriz_confusao <- table(Predito = previsoes, Real = dados$IDHM)

# Calcular Acurácia
acuracia <- sum(diag(matriz_confusao)) / sum(matriz_confusao)
cat("Acurácia:", acuracia, "\n")

# Calcular Sensibilidade (Recall)
sensibilidade <- matriz_confusao[2, 2] / sum(matriz_confusao[, 2])
cat("Sensibilidade:", sensibilidade, "\n")

# Calcular Especificidade
especificidade <- matriz_confusao[1, 1] / sum(matriz_confusao[, 1])
cat("Especificidade:", especificidade, "\n")
matriz_confusao


# Obter resíduos e valores ajustados
residuos <- residuals(modelo_stepwise, type = "deviance")
ajustados <- fitted(modelo_stepwise)

# Gráfico de Resíduos vs. Valores Ajustados
plot(ajustados, residuos,
     xlab = "Valores Ajustados",
     ylab = "Resíduos",
     main = "Resíduos vs. Valores Ajustados")
abline(h = 0, col = "red", lwd = 2)

# Autocorrelação dos Resíduos
acf(residuos, main = "Autocorrelação dos Resíduos")

# Autocorrelação das Variáveis Independentes
par(mfrow = c(1, 2))  # Dividir a tela para múltiplos gráficos
acf(dados$TAXES, main = "Autocorrelação de predictor1")
acf(dados$AREA, main = "Autocorrelação de predictor2")
par(mfrow = c(1, 1))  # Resetar a tela para um único gráfico

# Calcular o VIF para cada variável preditora
vif_valores <- vif(modelo_stepwise)
print(vif_valores)


# Gráfico de Cook's Distance para detectar influências
plot(modelo_stepwise, which = 4, main = "Gráfico de Cook's Distance")

# Verificando a linearidade do logit
# Criando gráficos de dispersão
logit_values <- predict(modelo_stepwise, type = "link")  # Logit estimado

# Gráfico para x1
ggplot(dados, aes(x = TAXES, y = logit_values)) + 
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  labs(title = "Verificação da Linearidade do Logit para Taxes",
       x = "x1",
       y = "Logit estimado") +
  theme_minimal()

# Gráfico para x2
ggplot(dados, aes(x = AREA, y = logit_values)) + 
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  labs(title = "Verificação da Linearidade do Logit para x2",
       x = "x2",
       y = "Logit estimado") +
  theme_minimal()














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


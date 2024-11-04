####Instalacao das dependencias
if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}

library(pacman)
pacman::p_load(dplyr, ggplot2, readxl, readr, DT, fastDummies, lmtest, tidyverse, car, psych, MASS, lmtest, trend)

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

# Instale o pacote se ainda não estiver instalado
install.packages("lmtest")

# Carregue o pacote
library(lmtest)



#Preparando o Dataset
tabela <- read_csv("BRAZIL_CITIES.csv")
dados <- tabela[, c("IDHM", "TAXES", "IBGE_CROP_PRODUCTION_$", "AREA", "RURAL_URBAN")]
dados_numericos <- tabela[, c("IDHM", "TAXES", "IBGE_CROP_PRODUCTION_$", "AREA")] # removendo rural_urban por ser qualitativa

# Dados antes de tratar
#datatable(sapply(select(dados, -RURAL_URBAN), descrever_coluna))
boxplot(dplyr::select(dados, -RURAL_URBAN))
pie(sort(table(dados$RURAL_URBAN)))

# Tratar dados
# Remover os valores 0 e os valores menores que 0

dados_negativos_ou_na <- (dados_numericos <= 0) | (is.na(dados_numericos))
length(which(dados_negativos_ou_na))
dados <- dados[!apply(dados_negativos_ou_na, 1, any), ]

# Contar quantas linhas sobraram após a remoção
tamanho_dataset_apos_remover_negativos <- nrow(dados)

# Exibir o número de linhas restantes
cat("Número de linhas restantes:", tamanho_dataset_apos_remover_negativos, "\n")

boxplot(dados$IDHM)
boxplot(dados$TAXES)
boxplot(dados[["IBGE_CROP_PRODUCTION_$"]])
boxplot(dados$AREA)


### Tratamento dos dados ###
# transformação logarítmica em TAXES
dados$TAXES <- log(dados$TAXES)

# transformação logarítmica em IBGE_CROP_PRODUCTION
dados[["IBGE_CROP_PRODUCTION_$"]] <- log(dados[["IBGE_CROP_PRODUCTION_$"]])

# transformação logarítmica em AREA
dados$AREA <- log(dados$AREA)

datatable(sapply(dplyr::select(dados, -RURAL_URBAN), descrever_coluna))

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
dados <- dplyr::select(dados, -RURAL_URBAN)


# Transformar IDH em Qualitativa Binária
dados$IDHM <- as.numeric(dados$IDHM > 0.7)



### MODELO REGRESSÃO ###
# Ajustando um modelo inicial com todas as variáveis
modelo_full <- glm(IDHM ~ ., data = dados, family = binomial(link = "logit"))

plot(modelo_full, 5)
summary(stdres(modelo_full))

heatmap(abs(cor(dados)))
vif(modelo_full)




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

pseudo_r2 <- 1 - logLik(modelo_stepwise) / logLik(null_model)
cat("\nPseudo R2: ", pseudo_r2, "\n")


# Obter os coeficientes do modelo
coeficientes <- summary(modelo_full)$coefficients

# Calcular o Odds Ratio
odds_ratios <- exp(coeficientes[, "Estimate"])
print(odds_ratios)

# Calcular o intervalo de confiança
alpha <- 0.05 # nível de significância
ci_lower <- exp(coeficientes[, "Estimate"] - qnorm(1 - alpha / 2) * coeficientes[, "Std. Error"])
ci_upper <- exp(coeficientes[, "Estimate"] + qnorm(1 - alpha / 2) * coeficientes[, "Std. Error"])

# Combinar Odds Ratios e intervalos de confiança em um data frame
resultados <- data.frame(
  Odds_Ratio = odds_ratios,
  CI_Lower = ci_lower,
  CI_Upper = ci_upper
)

print(resultados)

# Gerar previsões com probabilidade
probabilidades <- predict(modelo_stepwise, type = "response")
probabilidades
# Definir o limiar para classificação binária (0.5, por exemplo)
previsoes <- ifelse(probabilidades > 0.5, 1, 0)

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


# Autocorrelação dos Resíduos
acf(residuos, main = "Autocorrelação dos Resíduos")

#Teste de Durbin-Watson para autocorrelação dos resíduos
dw_test <- dwtest(modelo_stepwise, alternative = "two.sided")
print(dw_test)

# Teste dos resíduos
# Aplicar o Teste de Mann-Kendall para tendência nos resíduos
teste_tendencia <- mk.test(residuos)
print(teste_tendencia)


# Gráfico de Resíduos vs. Valores Ajustados
plot(ajustados, residuos,
     xlab = "Valores Ajustados",
     ylab = "Resíduos",
     main = "Resíduos vs. Valores Ajustados")
abline(h = 0, col = "red", lwd = 2)

# Ajustar o modelo
modelo_full <- glm(IDHM ~ TAXES + `IBGE_CROP_PRODUCTION_$` + AREA, data = dados, family = binomial(link = "logit"))

# Resumo do modelo
summary(modelo_full)

# Coeficientes
coeficientes <- coef(modelo_full)
print(coeficientes)

# Odds Ratios
odds_ratios <- exp(coeficientes)
print(odds_ratios)

# Intervalos de Confiança
ic <- confint(modelo_full)
odds_ratios_ic <- exp(ic)
print(odds_ratios_ic)



# Calcular o VIF para cada variável preditora
vif_valores <- vif(modelo_stepwise)
print(vif_valores)



# Verificando a linearidade do logit
# Criando gráficos de dispersão
logit_values <- predict(modelo_stepwise, type = "link")  # Logit estimado



# Gráfico para Taxes
ggplot(dados, aes(x = TAXES, y = logit_values)) +
  geom_point(size = 0.7, alpha = 1) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(title = "Verificação da Linearidade do Logit para Taxes",
       x = "Taxes",
       y = "Logit estimado") +
  theme_minimal()

# Gráfico para x2
ggplot(dados, aes(x = AREA, y = logit_values)) +
  geom_point(size = 0.5) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(title = "Verificação da Linearidade do Logit para x2",
       x = "x2",
       y = "Logit estimado") +
  theme_minimal()

# Gráfico para x2
ggplot(dados, aes(x = "IBGE_CROP_PRODUCTION_$", y = logit_values)) +
  geom_point(size = 0.5) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(title = "Verificação da Linearidade do Logit para x2",
       x = "x2",
       y = "Logit estimado") +
  theme_minimal()

# Gráfico para x2
ggplot(dados, aes(x = dados["IBGE_CROP_PRODUCTION_$"], y = logit_values)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  labs(title = "Verificação da Linearidade do Logit para x2",
       x = "x2",
       y = "Logit estimado") +
  theme_minimal()

# Ajustando o modelo (exemplo)
modelo_full <- glm(IDHM ~ ., data = dados, family = binomial(link = "logit"))

# Calculando os resíduos
residuos <- residuals(modelo_full)

# Calculando os valores ajustados
valores_ajustados <- fitted(modelo_full)

# Criando o gráfico
plot(valores_ajustados, residuos, 
     xlab = "Valores Ajustados", 
     ylab = "Resíduos", 
     main = "Gráfico de Resíduos vs. Valores Ajustados")

# Adicionando uma linha horizontal em zero
abline(h = 0, col = "red", lty = 2)
# RODANDO COM DADOS RELATIVIZADOS (DIVIDINDO A POPULAÇÃO POR IMPOSTOS, PRODUÇÃO AGRÍCOLA E ÁREA PARA ACHAR ESSES DADOS RELATIVOS)
#Preparando o Dataset

#Preparando o Dataset
tabela <- read_csv("BRAZIL_CITIES.csv")
dados <- tabela[, c("IDHM", "TAXES", "IBGE_PLANTED_AREA", "IBGE_DU", "RURAL_URBAN")]
dados_numericos <- tabela[, c("IDHM", "TAXES", "IBGE_PLANTED_AREA", "IBGE_DU")]

# Dados antes de tratar
#datatable(sapply(select(dados, -RURAL_URBAN), descrever_coluna))
boxplot(dplyr::select(dados, -RURAL_URBAN))
pie(sort(table(dados$RURAL_URBAN)))

# Tratar dados
# Remover os valores 0 e os valores menores que 0

dados_negativos_ou_na <- (dados_numericos <= 0) | (is.na(dados_numericos))
length(which(dados_negativos_ou_na))
dados <- dados[!apply(dados_negativos_ou_na, 1, any), ]

# Contar quantas linhas sobraram após a remoção
tamanho_dataset_apos_remover_negativos <- nrow(dados)

# Exibir o número de linhas restantes
cat("Número de linhas restantes:", tamanho_dataset_apos_remover_negativos, "\n")

boxplot(dados$IDHM)
boxplot(dados$TAXES)
boxplot(dados$IBGE_PLANTED_AREA)
boxplot(dados$IBGE_DU)


### Tratamento dos dados ###
# transformação logarítmica em TAXES
dados$TAXES <- log(dados$TAXES)

# transformação logarítmica em IBGE_CROP_PRODUCTION
dados$IBGE_PLANTED_AREA <- log(dados$IBGE_PLANTED_AREA)

# transformação logarítmica em AREA
dados$IBGE_DU <- log(dados$IBGE_DU)

datatable(sapply((dados), descrever_coluna))

# Variáveis Quantitativas
boxplot(dados$IDHM)
boxplot(dados$TAXES)
boxplot(dados$IBGE_PLANTED_AREA)
boxplot(dados$IBGE_DU)

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
modelo_full <- glm(IDHM ~ ., data = dados, family = binomial(link = "logit"))

plot(modelo_full, 5)
summary(stdres(modelo_full))

heatmap(abs(cor(dados)))
vif(modelo_full)




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

pseudo_r2 <- 1 - logLik(modelo_stepwise) / logLik(null_model)
cat("\nPseudo R2: ", pseudo_r2, "\n")


# Gerar previsões com probabilidade
probabilidades <- predict(modelo_stepwise, type = "response")
probabilidades
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




# Calcular o VIF para cada variável preditora
vif_valores <- vif(modelo_stepwise)
print(vif_valores)



# Verificando a linearidade do logit
# Criando gráficos de dispersão
logit_values <- predict(modelo_stepwise, type = "link")  # Logit estimado



# Gráfico para Taxes
ggplot(dados, aes(x = TAXES, y = logit_values)) +
  geom_point(size = 0.7, alpha = 1) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(title = "Verificação da Linearidade do Logit para Taxes",
       x = "Taxes",
       y = "Logit estimado") +
  theme_minimal()

# Gráfico para x2
ggplot(dados, aes(x = AREA, y = logit_values)) +
  geom_point(size = 0.5) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(title = "Verificação da Linearidade do Logit para x2",
       x = "x2",
       y = "Logit estimado") +
  theme_minimal()

# Gráfico para x2
ggplot(dados, aes(x = "IBGE_CROP_PRODUCTION_$", y = logit_values)) +
  geom_point(size = 0.5) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(title = "Verificação da Linearidade do Logit para x2",
       x = "x2",
       y = "Logit estimado") +
  theme_minimal()

# Gráfico para x2
ggplot(dados, aes(x = dados["IBGE_CROP_PRODUCTION_$"], y = logit_values)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  labs(title = "Verificação da Linearidade do Logit para x2",
       x = "x2",
       y = "Logit estimado") +
  theme_minimal()
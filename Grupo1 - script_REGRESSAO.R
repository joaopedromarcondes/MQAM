# Importar bibliotecas e dependências
# Verifica e instala o pacote pacman, se necessário
if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}

# Carrega o pacote pacman
library(pacman)



# Carrega os pacotes necessários, instalando-os se não estiverem presentes
pacman::p_load(dplyr, ggplot2, readxl, DT, fastDummies, lmtest, tidyverse)


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


# Função para realizar regressão e gerar gráficos
regressoes_idhm_com_graficos <- function(data, idhm_col = "IDHM") {
  # Selecionar variáveis (excluindo o IDHM)
  variaveis <- data %>%
    select(-all_of(idhm_col)) %>%
    colnames()

  # Criar uma lista para armazenar os resultados e os gráficos
  resultados <- list()
  graficos <- list()

  # Loop pelas variáveis, ajustando o modelo e criando gráficos
  for (variavel in variaveis) {
    # Formula da regressão
    formula <- as.formula(paste(idhm_col, "~", variavel))
    modelo <- lm(formula, data = data)

    # Resumo dos coeficientes da regressão
    print("========================================")
    print(formula)
    print(summary(modelo))
    coeficientes <- summary(modelo)$coefficients
    resultados[[variavel]] <- coeficientes

    # Criar gráfico com a linha de regressão
    grafico <- ggplot(data, aes_string(x = variavel, y = idhm_col)) +
      geom_point() +  # Adiciona os pontos
      geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Linha de regressão
      labs(title = paste("Regressão Linear entre", idhm_col, "e", variavel),
           x = variavel,
           y = idhm_col) +
      theme_minimal()

    # Armazenar o gráfico
    graficos[[variavel]] <- grafico
  }

  return(list("coeficientes" = resultados, "graficos" = graficos))
}




# Importar banco de dados
BRAZIL_CITIES_REV2022_CSV <- read_excel("BRAZIL_CITIES_REV2022.CSV.xlsx")

# Selecionar colunas que serão usadas na análise
dados <- BRAZIL_CITIES_REV2022_CSV[, c("IDHM", "IBGE_RES_POP", "ALT", "AREA", "TAXES", "RURAL_URBAN", "Motorcycles", "Cars")]


# Dados antes de tratar
datatable(sapply(select(dados, -RURAL_URBAN), descrever_coluna))
boxplot(select(dados, -RURAL_URBAN))
pie(sort(table(dados$RURAL_URBAN)))

# Tratar dados
# Remover os valores 0 e os valores menores que 0
# menos para altitude e estado, já que não faz sentido tirar desses
dados_negativos <- (dados[, c("IDHM", "IBGE_RES_POP", "AREA", "TAXES", "Motorcycles", "Cars")] <= 0)
dados <- dados[!apply(dados_negativos, 1, any), ]
length(which(dados_negativos))

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

dados$TAXES <- dados$TAXES / dados$IBGE_RES_POP
dados$Motorcycles <- dados$Motorcycles / dados$IBGE_RES_POP
dados$Cars <- dados$Cars / dados$IBGE_RES_POP


# Visualização dos dados após o tratamento
datatable(sapply(dados, descrever_coluna))
sort(table(dados$STATE))
length(dados)
datatable(dados)


resultados_graficos <- regressoes_idhm_com_graficos(select(dados, -RURAL_URBAN))

# IBGE_RES_POP
print(resultados_graficos$coeficientes$IBGE_RES_POP)
print(resultados_graficos$graficos$IBGE_RES_POP)

# ALT
print(resultados_graficos$coeficientes$ALT)
print(resultados_graficos$graficos$ALT)

# AREA
print(resultados_graficos$coeficientes$AREA)
print(resultados_graficos$graficos$AREA)

# TAXES
print(resultados_graficos$coeficientes$TAXES)
print(resultados_graficos$graficos$TAXES)

# Motorcycles
print(resultados_graficos$coeficientes$Motorcycles)
print(resultados_graficos$graficos$Motorcycles)

# Cars
print(resultados_graficos$coeficientes$Cars)
print(resultados_graficos$graficos$Cars)




# Transformar qualitativas em DUMMY
dados <- dummy_cols(dados, select_columns = "RURAL_URBAN")
dados <- select(dados, -RURAL_URBAN)


# Ajustar um modelo completo (com todas as variáveis independentes)
modelo_completo <- lm(IDHM ~ ., data = dados)

# Aplicar o método stepwise
modelo_stepwise <- step(modelo_completo, direction = "both")

# Verificar o resumo do modelo stepwise
summary(modelo_stepwise)
AIC(modelo_stepwise)
formula(modelo_stepwise)

residuos <- residuals(modelo_stepwise)

# Gráfico de Resíduos vs. Valores Ajustados
plot(modelo_stepwise$fitted.values, residuos,
     xlab = "Valores Ajustados",
     ylab = "Resíduos",
     main = "Resíduos vs. Valores Ajustados")
abline(h = 0, col = "red")


# Q-Q Plot
qqnorm(residuos)
qqline(residuos, col = "red")  # Linha de referência


# Gráfico de Dispersão dos Resíduos
plot(modelo_stepwise$fitted.values, abs(residuos),
     xlab = "Valores Ajustados",
     ylab = "Resíduos Absolutos",
     main = "Dispersão dos Resíduos")
abline(h = 0, col = "red")  # Linha horizontal em y = 0

# Teste de normalidade dos resíduos
ks.test(residuos, "pnorm", mean = mean(residuos), sd = sd(residuos))

# Teste de Breusch-Pagan
gqtest(modelo_stepwise)

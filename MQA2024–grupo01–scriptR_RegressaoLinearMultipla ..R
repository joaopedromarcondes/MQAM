# Importar bibliotecas e dependências
# Verifica e instala o pacote pacman, se necessário
if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}

# Carrega o pacote pacman
library(pacman)


# Carrega os pacotes necessários, instalando-os se não estiverem presentes
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

#Funcao para calcular todos os p-value e r quadrado
calcular_p_values_r_square <- function(data, idhm_col = "IDHM") {
  # Lista de variáveis para análise
  variaveis <- c("IBGE_RES_POP", "AREA", "ALT", "TAXES", "Motorcycles", "Cars")
  
  # Lista para armazenar os resultados
  resultados <- list()
  
  # Loop pelas variáveis para calcular p-values
  for (variavel in variaveis) {
    # Para variáveis contínuas, usar regressão linear
     modelo <- lm(as.formula(paste(idhm_col, "~", variavel)), data = data)
    summary_model <- summary(modelo)
    p_value <- summary_model$coefficients[2,4]  # p-value para o coeficiente da variável
    r_squared <- summary_model$r.squared
  
    resultados[[variavel]] <- list(p_value = p_value, r_squared = r_squared)
  }
  return(resultados)
}


# Função para formatar p-values e r-quadrado
format_value <- function(p_value, digits = 3) {
  if (is.na(p_value)) {
    return("NA")
  } else if (p_value < 0.001) {
    return("< 0.001")
  } else {
    return(format(round(p_value, digits), nsmall = digits))
  }
}

#BOXPLOOOOOOOOOOOOOOOOOOOOOOOOOOOTS
gerar_boxplots <- function(data, idhm_col = "IDHM") {
  # Lista de variáveis para criar boxplots
  variaveis <- c("IBGE_RES_POP")
  
  # Lista para armazenar os gráficos
  boxplots <- list()
  
  # Loop pelas variáveis para criar os boxplots
  for (variavel in variaveis) {
# Para variáveis contínuas, criar boxplot com grupos
    # Dividir a variável em quartis
    data$grupo <- cut(data[[variavel]], breaks = quantile(data[[variavel]], probs = seq(0, 1, 0.25)), 
                      labels = c("Q1", "Q2", "Q3", "Q4"), include.lowest = TRUE)
    
    plot <- ggplot(data, aes_string(x = "grupo", y = idhm_col)) +
      geom_boxplot() +
      labs(title = paste("Boxplot de", idhm_col, "por quartis de", variavel),
           x = paste("Quartis de", variavel),
           y = idhm_col) +
      theme_minimal()
    
    boxplots[[variavel]] <- plot
  }
  
  return(boxplots)
}

#######################################################################################################################################################

# Importar banco de dados
BRAZIL_CITIES_REV2022_CSV <- read_csv("BRAZIL_CITIES.csv")
getwd()
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

#Normalização
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

#imprime os p-valores e r-quadrado
resultados <- calcular_p_values_r_square(dados)

# Exibir os resultados
for (variavel in names(resultados)) {
  cat(paste0(variavel, ":\n"))
  cat(paste0("  p-value = ", format_value(resultados[[variavel]]$p_value), "\n"))
  cat(paste0("  R-squared = ", format_value(resultados[[variavel]]$r_squared), "\n\n"))
}
#imprime a regressão linear de cada um dos 



# Gerar os boxplots
boxplots <- gerar_boxplots(dados)

# Visualizar os boxplots
for (variavel in names(boxplots)) {
  print(paste("Boxplot para", variavel))
  print(boxplots[[variavel]])
}


################################################################################################################################

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

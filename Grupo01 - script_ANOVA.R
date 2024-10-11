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
BRAZIL_CITIES_REV2022_CSV <- read_excel("MQA2024 - Grupo01 - dataset.csv")

# Selecionar colunas que serão usadas na análise - IDH e População
idh_e_pop <- BRAZIL_CITIES_REV2022_CSV[, c("IDHM", "IBGE_RES_POP")]

# Tratamento do IDH
# Remover outliers de IDH
outliers_idh <- boxplot.stats(idh_e_pop$IDHM)$out
idh_e_pop <- idh_e_pop[-which(idh_e_pop$IDHM %in% outliers_idh),]

# Tratamento da População
# Transformação logarítmica
idh_e_pop$IBGE_RES_POP <- log(idh_e_pop$IBGE_RES_POP)
idh_e_pop <- idh_e_pop[-which(idh_e_pop$IBGE_RES_POP == -Inf),]

# Função para categorizar IDHM
categorizar_idhm <- function(idhm) {
  case_when(
    idhm < 5000 ~ "Muito Baixo",
    idhm >= 5000 & idhm < 6000 ~ "Baixo",
    idhm >= 6000 & idhm < 7000 ~ "Médio",
    idhm >= 7000 & idhm < 8000 ~ "Alto",
    idhm >= 8000 ~ "Muito Alto",
    TRUE ~ "Não classificado"
  )
}

# Aplicar a função à coluna IDHM
idh_e_pop$IDHM_Categoria <- categorizar_idhm(idh_e_pop$IDHM)

# Visualizações

# 1. Boxplot
boxplot_idhm <- ggplot(idh_e_pop, aes(x = IDHM_Categoria, y = IDHM)) +
  geom_boxplot(fill = "lightblue", color = "blue") +
  labs(title = "Boxplot do IDHM por Categoria",
       x = "Categoria do IDHM",
       y = "IDHM") +
  theme_minimal()

#boxplot_pop <- ggplot(idh_e_pop, aes(x, y = IDH))

# 2. Gráfico de Dispersão
dispersao_idhm <- ggplot(idh_e_pop, aes(x = IBGE_RES_POP, y = IDHM, color = IDHM_Categoria)) +
  geom_point() +
  labs(title = "Gráfico de Dispersão: IDHM vs Log(População)",
       x = "Log(População)",
       y = "IDHM") +
  theme_minimal() +
  theme(legend.title = element_blank())

# 3. Histograma
histograma_idhm <- ggplot(idh_e_pop, aes(x = IDHM)) +
  geom_histogram(binwidth = 0.01, fill = "lightgreen", color = "darkgreen") +
  labs(title = "Histograma do IDHM",
       x = "IDHM",
       y = "Frequência") +
  theme_minimal()

histograma_pop <- ggplot(pop, aes(x = POP)) +
 geom_histogram(binwidth = 0.01, fill = "ligthpurple", color = "darkpurple")
 labs(title = "Histograma da População",
     x = "População",
     y = "Frequência") +
 theme_minimal()

# Exibir os gráficos
print(boxplot_idhm)
print(dispersao_idhm)
print(histograma_idhm) #está demorando muito para ser executado, não se precupem professora e monitor.


# Análises adicionais
summary(idh_e_pop)
cat("Moda do IDHM:", moda(idh_e_pop$IDHM), "\n")
cat("Variância do IDHM:", var(idh_e_pop$IDHM), "\n")
cat("Desvio padrão do IDHM:", sd(idh_e_pop$IDHM), "\n")

# Tabela de frequências do IDHM categorizado
table(idh_e_pop$IDHM_Categoria)

#########################################################################################

# Realizar ANOVA
modelo_anova <- aov(IDHM ~ IDHM_Categoria, data = idh_e_pop)

# Sumário da ANOVA
cat("\nResultados da ANOVA:\n")
print(summary(modelo_anova))

# Teste post-hoc de Tukey
cat("\nTeste post-hoc de Tukey:\n")
tukey_result <- TukeyHSD(modelo_anova)
print(tukey_result)

# Visualização dos resultados da ANOVA
plot_anova <- ggplot(idh_e_pop, aes(x = IDHM_Categoria, y = IDHM)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.2) +
  labs(title = "ANOVA: IDHM por Categoria",
       x = "Categoria do IDHM",
       y = "IDHM") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(plot_anova)

# Verificar pressupostos da ANOVA

# 1. Normalidade dos resíduos
residuos <- residuals(modelo_anova)
qqnorm(residuos)
qqline(residuos)

# 2. Homogeneidade de variâncias
plot(modelo_anova, which = 1)

#A seguinte biblioteca não foi reconhecida pelo nosso R studio, porém no nosso material de referencia (UFRGS R WIKI)
#ela foi usada.
# Teste de Levene para homogeneidade de variâncias
library(car)
levene_test <- leveneTest(IDHM ~ IDHM_Categoria, data = idh_e_pop)
print(levene_test)

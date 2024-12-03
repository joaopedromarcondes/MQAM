# Importar bibliotecas e dependências
# Verifica e instala o pacote pacman, se necessário
if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}

library(pacman)

# Carrega os pacotes necessários, instalando-os se não estiverem presentes
pacman::p_load(dplyr, ggplot2, readxl, readr, DT, fastDummies, lmtest, tidyverse, psych, GPArotation, corrplot, gridExtra)

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
grafico5 <- criar_grafico_frequencia(dados_numericos, "IDHM", 2, "Frequência de IDHM")
grafico6 <- criar_grafico_frequencia(dados_numericos, "AREA", 0.1, "Frequência de AREA")
grafico7 <- criar_grafico_frequencia(dados_numericos, "TAXES", 2, "Frequência de TAXES")
grafico8 <- criar_grafico_frequencia(dados_numericos, "Cars", 2, "Frequência de Cars")
grafico9 <- criar_grafico_frequencia(dados_numericos, "Motorcycles", 2, "Frequência de Motorcycles")

grid.arrange(grafico1, grafico2, grafico3, grafico4, grafico5, grafico6, grafico7, grafico8, grafico9, ncol = 2)


# Definindo intervalos personalizados
intervalos_personalizados <- list(
  "IBGE_RES_POP" = 2,
  "IBGE_DU" = 2,
  "IBGE_PLANTED_AREA" = 2,
  "IBGE_CROP_PRODUCTION_$" = 2,
  "IDHM" = 0.1,
  "AREA" = 2,
  "TAXES" = 2,
  "Cars" = 2,
  "Motorcycles" = 2
)

# Exibindo cada tabela individualmente com datatable()
# Para visualizar, execute cada linha separadamente
datatable(criar_tabela_frequencia(dados_numericos, "IBGE_RES_POP", intervalos_personalizados[["IBGE_RES_POP"]]), options = list(pageLength = 5, autoWidth = TRUE))
datatable(criar_tabela_frequencia(dados_numericos, "IBGE_DU", intervalos_personalizados[["IBGE_DU"]]), options = list(pageLength = 5, autoWidth = TRUE))
datatable(criar_tabela_frequencia(dados_numericos, "IBGE_PLANTED_AREA", intervalos_personalizados[["IBGE_PLANTED_AREA"]]), options = list(pageLength = 5, autoWidth = TRUE))
datatable(criar_tabela_frequencia(dados_numericos, "IBGE_CROP_PRODUCTION_$", intervalos_personalizados[["IBGE_CROP_PRODUCTION_$"]]), options = list(pageLength = 5, autoWidth = TRUE))
datatable(criar_tabela_frequencia(dados_numericos, "IDHM", intervalos_personalizados[["IDHM"]]), options = list(pageLength = 5, autoWidth = TRUE))
datatable(criar_tabela_frequencia(dados_numericos, "AREA", intervalos_personalizados[["AREA"]]), options = list(pageLength = 5, autoWidth = TRUE))
datatable(criar_tabela_frequencia(dados_numericos, "TAXES", intervalos_personalizados[["TAXES"]]), options = list(pageLength = 5, autoWidth = TRUE))
datatable(criar_tabela_frequencia(dados_numericos, "Cars", intervalos_personalizados[["Cars"]]), options = list(pageLength = 5, autoWidth = TRUE))
datatable(criar_tabela_frequencia(dados_numericos, "Motorcycles", intervalos_personalizados[["Motorcycles"]]), options = list(pageLength = 5, autoWidth = TRUE))


datatable(sapply(dados_numericos, descrever_coluna))

# A: matriz de correlação entre as variáveis envolvidas
# Calcular a matriz de correlação
cor_matrix <- cor(dados_numericos)

# Visualizar a matriz de correlação
corrplot(cor_matrix, method = "circle", type = "upper", order = "hclust")

# B: cálculo do método KMO das variáveis envolvidas
# Calcular o índice KMO
kmo <- KMO(dados_numericos)
print(kmo)

# C: teste de bartlett da matriz de correlação 
# Teste de Bartlett
bartlett <- cortest.bartlett(cor_matrix, n = nrow(dados_numericos))
print(bartlett)

# D: Cálculo do MSA (Measure of Sampling Adequacy) para cada variável
# Exibir MSA de cada variável
print(kmo$MSAi)

nfactors(dados_numericos, 8, rotate="none")

#Método do cotovelo 
# Calcular autovalores
eigen_values <- eigen(cor(dados_numericos))$values
print(eigen_values)

# Gerar Scree Plot
plot(eigen_values, type = "b", main = "Scree Plot (Método do Cotovelo)",
     xlab = "Número de Fatores", ylab = "Autovalor", pch = 19, col = "blue")

# Adicionar linha horizontal no autovalor 1 (Critério de Kaiser)
abline(h = 1, col = "red", lty = 2)

# F: rotação dos fatores
# Extração de fatores (Ex.: 3 fatores)
fa_unrotated <- fa(dados_numericos, nfactors = 3, rotate = "none")
print(fa_unrotated$loadings)

# Rotação Varimax (ortogonal)
fa_rotated <- fa(dados_numericos, nfactors = 3, rotate = "varimax")
print(fa_rotated$loadings)

#G: interpretação dos fatores obtidos
# Exibir apenas os loadings significativos
print(fa_rotated$loadings, cutoff = 0.4)

# Gerar escores fatoriais
factor_scores <- fa_rotated$scores

# Adicionar ao dataset
dados_numericos$fator1 <- factor_scores[, 1]
dados_numericos$fator2 <- factor_scores[, 2]

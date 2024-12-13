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
dados <- tabela[, c("CITY", "IBGE_RES_POP", "IBGE_DU", "IBGE_PLANTED_AREA", "IBGE_CROP_PRODUCTION_$", "IDHM", "AREA", "RURAL_URBAN", "TAXES", "Cars", "Motorcycles", "IBGE_1-4", "IBGE_5-9", "IBGE_10-14", "IBGE_15-59", "IBGE_1", "IBGE_60+", "IDHM_Renda", "IDHM_Educacao")]
dados_numericos <- tabela[, c("IBGE_RES_POP", "IBGE_DU", "IBGE_PLANTED_AREA", "IBGE_CROP_PRODUCTION_$", "IDHM", "AREA", "TAXES", "Cars", "Motorcycles", "IBGE_1-4", "IBGE_5-9", "IBGE_10-14", "IBGE_15-59", "IBGE_1", "IBGE_60+", "IDHM_Renda", "IDHM_Educacao")]


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
boxplot(dados[["IBGE_1-4"]])
boxplot(dados[["IBGE_5-9"]])
boxplot(dados[["IBGE_10-14"]])
boxplot(dados[["IBGE_15-59"]])
boxplot(dados[["IBGE_1"]])
boxplot(dados[["IBGE_60+"]])
boxplot(dados[["IDHM_Renda"]])
boxplot(dados[["IDHM_Educacao"]])

# Transformação Logarítmica
dados_numericos[, "IBGE_RES_POP"] <- log(dados_numericos[, "IBGE_RES_POP"])
dados_numericos[, "IBGE_DU"] <- log(dados_numericos[, "IBGE_DU"])
dados_numericos[, "IBGE_PLANTED_AREA"] <- log(dados_numericos[, "IBGE_PLANTED_AREA"])
dados_numericos[, "IBGE_CROP_PRODUCTION_$"] <- log(dados_numericos[, "IBGE_CROP_PRODUCTION_$"])
dados_numericos[, "AREA"] <- log(dados_numericos[, "AREA"])
dados_numericos[, "TAXES"] <- log(dados_numericos[, "TAXES"])
dados_numericos[, "Cars"] <- log(dados_numericos[, "Cars"])
dados_numericos[, "Motorcycles"] <- log(dados_numericos[, "Motorcycles"])
dados_numericos[, "IBGE_1-4"] <- log(dados_numericos[, "IBGE_1-4"])
dados_numericos[, "IBGE_5-9"] <- log(dados_numericos[, "IBGE_5-9"])
dados_numericos[, "IBGE_10-14"] <- log(dados_numericos[, "IBGE_10-14"])
dados_numericos[, "IBGE_15-59"] <- log(dados_numericos[, "IBGE_15-59"])
dados_numericos[, "IBGE_1"] <- log(dados_numericos[, "IBGE_1"])
dados_numericos[, "IBGE_60+"] <- log(dados_numericos[, "IBGE_60+"])

### BOXPLOTS DOS DADOS APÓS TRANSFORMACAO LOGARITMICA
boxplot(dados_numericos[["IBGE_RES_POP"]])
boxplot(dados_numericos[["IBGE_DU"]])
boxplot(dados_numericos[["IBGE_PLANTED_AREA"]])
boxplot(dados_numericos[["IBGE_CROP_PRODUCTION_$"]])
boxplot(dados_numericos[["AREA"]])
boxplot(dados_numericos[["TAXES"]])
boxplot(dados_numericos[["Cars"]])
boxplot(dados_numericos[["Motorcycles"]])
boxplot(dados_numericos[["IBGE_1-4"]])
boxplot(dados_numericos[["IBGE_5-9"]])
boxplot(dados_numericos[["IBGE_10-14"]])
boxplot(dados_numericos[["IBGE_15-59"]])
boxplot(dados_numericos[["IBGE_1"]])
boxplot(dados_numericos[["IBGE_60+"]])

# Criar gráficos para cada variável
grafico1 <- criar_grafico_frequencia(dados_numericos, "IBGE_RES_POP", 2, "Frequência de IBGE_RES_POP")
grafico2 <- criar_grafico_frequencia(dados_numericos, "IBGE_DU", 2, "Frequência de IBGE_DU")
grafico3 <- criar_grafico_frequencia(dados_numericos, "IBGE_PLANTED_AREA", 2, "Frequência de IBGE_PLANTED_AREA")
grafico4 <- criar_grafico_frequencia(dados_numericos, "IBGE_CROP_PRODUCTION_$", 2, "Frequência de IBGE_CROP_PRODUCTION_$")
grafico5 <- criar_grafico_frequencia(dados_numericos, "IDHM", 0.1, "Frequência de IDHM")
grafico6 <- criar_grafico_frequencia(dados_numericos, "AREA", 2, "Frequência de AREA")
grafico7 <- criar_grafico_frequencia(dados_numericos, "TAXES", 2, "Frequência de TAXES")
grafico8 <- criar_grafico_frequencia(dados_numericos, "Cars", 2, "Frequência de Cars")
grafico9 <- criar_grafico_frequencia(dados_numericos, "Motorcycles", 2, "Frequência de Motorcycles")

grid.arrange(grafico1, grafico2, grafico3, grafico4, grafico5, grafico6, grafico7, grafico8, grafico9, ncol = 3)

grafico1 <- criar_grafico_frequencia(dados_numericos, "IBGE_1-4", 2, "Frequência de IBGE_1-4")
grafico2 <- criar_grafico_frequencia(dados_numericos, "IBGE_5-9", 2, "Frequência de IBGE_5-9")
grafico3 <- criar_grafico_frequencia(dados_numericos, "IBGE_10-14", 2, "Frequência de IBGE_10-14")
grafico4 <- criar_grafico_frequencia(dados_numericos, "IBGE_15-59", 2, "Frequência de IBGE_15-59")
grafico5 <- criar_grafico_frequencia(dados_numericos, "IBGE_1", 2, "Frequência de IBGE_1")
grafico6 <- criar_grafico_frequencia(dados_numericos, "IBGE_60+", 2, "Frequência de IBGE_60+")
grafico7 <- criar_grafico_frequencia(dados_numericos, "IDHM_Renda", 0.1, "Frequência de IDHM_Renda")
grafico8 <- criar_grafico_frequencia(dados_numericos, "IDHM_Educacao", 0.1, "Frequência de IDHM_Educacao")

grid.arrange(grafico1, grafico2, grafico3, grafico4, grafico5, grafico6, grafico7, grafico8, ncol = 3)

datatable(sapply(dados_numericos, descrever_coluna))

# A: matriz de correlação entre as variáveis envolvidas
# Calcular a matriz de correlação
cor_matrix <- cor(dados_numericos)

# Visualizar a matriz de correlação
corrplot(cor_matrix, method = "circle", type = "upper", order = "hclust")

# Criando uma matriz de correlação numérica
cor_matrix <- cor(dados_numericos)
cor_matrix_rounded <- round(cor_matrix, 2)

# Criando um grid com a matriz de correlação como uma tabela numérica
png("matriz_correlacao_numerica.png", width = 3000, height = 3000, res = 150)
grid.table(cor_matrix_rounded)
dev.off()


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

#E: Determinação do número de fatores (
# Calcular autovalores
eigen_values <- eigen(cor(dados_numericos))$values
print(eigen_values)


variancia_explicada <- eigen_values / sum(eigen_values) * 100
print(variancia_explicada)


# Gerar Scree Plot
plot(eigen_values, type = "b", main = "Scree Plot (Método do Cotovelo)",
     xlab = "Número de Fatores", ylab = "Autovalor", pch = 19, col = "blue")

# Adicionar linha horizontal no autovalor 1 (Critério de Kaiser)
abline(h = 1, col = "red", lty = 2)

# F: rotação dos fatores
# Extração de fatores (Ex.: 3 fatores)
fa_unrotated <- fa(dados_numericos, nfactors = 3, rotate = "none")
print(fa_unrotated$loadings)

print(fa_unrotated$loadings, cutoff = 0.5)

# Rotação Varimax (ortogonal)
fa_rotated <- fa(dados_numericos, nfactors = 3, rotate = "varimax")
print(fa_rotated$loadings)

#G: interpretação dos fatores obtidos
# Exibir apenas os loadings significativos
print(fa_rotated$loadings, cutoff = 0.5)
x
# Gerar escores fatoriais
factor_scores <- fa_rotated$scores

# Adicionar ao dataset
dados_numericos$fator1 <- factor_scores[, 1]
dados_numericos$fator2 <- factor_scores[, 2]

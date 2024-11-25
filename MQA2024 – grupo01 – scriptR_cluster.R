# Instalação das dependencias
if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}

library(pacman)
pacman::p_load(dplyr, ggplot2, readxl, readr, DT, factoextra, gridExtra)

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
tabela <- read_csv2("BRAZIL_CITIES.csv")
dados <- tabela[, c("CITY", "IBGE_PLANTED_AREA", "IBGE_CROP_PRODUCTION_$", "IBGE_RES_POP", "TAXES", "IDHM")]
dados_numericos <- tabela[, c("IBGE_PLANTED_AREA", "IBGE_CROP_PRODUCTION_$", "IBGE_RES_POP", "TAXES", "IDHM")]

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

boxplot(dados$IBGE_PLANTED_AREA)
boxplot(dados[["IBGE_CROP_PRODUCTION_$"]])
boxplot(dados[["IBGE_RES_POP"]])
boxplot(dados[["TAXES"]])
boxplot(dados[["IDHM"]])

# Transformação Logarítmica
dados_numericos[, "IBGE_PLANTED_AREA"] <- log(dados_numericos[, "IBGE_PLANTED_AREA"])
dados_numericos[, "IBGE_CROP_PRODUCTION_$"] <- log(dados_numericos[, "IBGE_CROP_PRODUCTION_$"])
dados_numericos[, "IBGE_RES_POP"] <- log(dados_numericos[, "IBGE_RES_POP"])
dados_numericos[, "TAXES"] <- log(dados_numericos[, "TAXES"])


### BOXPLOTS DOS DADOS APÓS TRANSFORMACAO LOGARITMICA
boxplot(dados_numericos[, "IBGE_PLANTED_AREA"])
boxplot(dados_numericos[, "IBGE_CROP_PRODUCTION_$"])
boxplot(dados_numericos[, "IBGE_RES_POP"])
boxplot(dados_numericos[, "TAXES"])
boxplot(dados_numericos[, "IDHM"])

#TABELA DE DISTRIBUICAO DE FREQUENCIAS


# Criar gráficos para cada variável
grafico1 <- criar_grafico_frequencia(dados_numericos, "IBGE_PLANTED_AREA", 2, "Frequência de IBGE_PLANTED_AREA")
grafico2 <- criar_grafico_frequencia(dados_numericos, "IBGE_CROP_PRODUCTION_$", 2, "Frequência de IBGE_CROP_PRODUCTION_$")
grafico3 <- criar_grafico_frequencia(dados_numericos, "IBGE_RES_POP", 2, "Frequência de IBGE_RES_POP")
grafico4 <- criar_grafico_frequencia(dados_numericos, "TAXES", 2, "Frequência de TAXES")
grafico5 <- criar_grafico_frequencia(dados_numericos, "IDHM", 0.1, "Frequência de IDHM")

# Exibir os gráficos
grid.arrange(grafico1, grafico2, grafico3, grafico4, grafico5, ncol = 2)



# Definindo intervalos personalizados
intervalos_personalizados <- list(
  "IBGE_PLANTED_AREA" = 2,
  "IBGE_CROP_PRODUCTION_$" = 2,
  "IBGE_RES_POP" = 2,
  "TAXES" = 2,
  "IDHM" = 0.1
)

# Exibindo cada tabela individualmente com datatable()
# Para visualizar, execute cada linha separadamente

datatable(criar_tabela_frequencia(dados_numericos, "IBGE_PLANTED_AREA", intervalos_personalizados[["IBGE_PLANTED_AREA"]]), options = list(pageLength = 5, autoWidth = TRUE))
datatable(criar_tabela_frequencia(dados_numericos, "IBGE_CROP_PRODUCTION_$", intervalos_personalizados[["IBGE_CROP_PRODUCTION_$"]]), options = list(pageLength = 5, autoWidth = TRUE))
datatable(criar_tabela_frequencia(dados_numericos, "IBGE_RES_POP", intervalos_personalizados[["IBGE_RES_POP"]]), options = list(pageLength = 5, autoWidth = TRUE))
datatable(criar_tabela_frequencia(dados_numericos, "TAXES", intervalos_personalizados[["TAXES"]]), options = list(pageLength = 5, autoWidth = TRUE))
datatable(criar_tabela_frequencia(dados_numericos, "IDHM", intervalos_personalizados[["IDHM"]]), options = list(pageLength = 5, autoWidth = TRUE))

datatable(sapply(dados_numericos, descrever_coluna))


### Tratamento dos dados ###
dados_padronizados <- scale(dados_numericos)
boxplot(dados_padronizados)

# Calcular a matriz de distâncias
distancia <- dist(dados_padronizados, method = "euclidian")

# Aplicar o método de clusterização hierárquica (método de ligação completa)
modelo_hclust <- hclust(distancia, method = "ward.D2")

# Plotar o dendrograma
plot(modelo_hclust, labels = rownames(dados),
     main = "Dendrograma de Clusterização Hierárquica")


# Cortar em 4 clusters
grupos <- cutree(modelo_hclust, k = 4)
dados$grupo <- grupos

# Análise dos clusters
table(grupos)
aggregate(dplyr::select(dados, -CITY, -grupo), list(dados$grupo), mean)
datatable(aggregate(dplyr::select(dados, -CITY, -grupo), list(dados$grupo), mean))


### Aplicando K-means com k clusters
set.seed(123)  # Para resultados reprodutíveis

# Método para escolher o número de Clusters
fviz_nbclust(dados_numericos, kmeans, method = "wss")

kmeans_result <- kmeans(dados_padronizados, centers = 8, nstart = 10)

# Adicionando os clusters aos dados originais
dados$cluster <- as.factor(kmeans_result$cluster)


pca_result <- prcomp(dados_padronizados)
dados_pca <- as.data.frame(pca_result$x)
dados_pca$cluster <- as.factor(kmeans_result$cluster)
dados_pca$grupo <- as.factor(dados$grupo)

ggplot(dados_pca, aes(x = PC1, y = PC4, color = cluster)) +
  geom_point(size = 2) +
  labs(title = "Clusters com PCA") +
  theme_minimal()

ggplot(dados_pca, aes(x = PC1, y = PC4, color = grupo)) +
  geom_point(size = 2) +
  labs(title = "Clusters com PCA") +
  theme_minimal()


# Visualização dos dados
table(dados$cluster)

# Selecionar apenas as colunas numéricas
dados_num <- dados[, sapply(dados, is.numeric)]

# Aplicar o aggregate apenas nas colunas numéricas
medias_por_cluster <- aggregate(. ~ cluster, data = cbind(dados_num, cluster = dados$cluster), FUN = mean)

# Visualizando o resultado
print(medias_por_cluster)

datatable(medias_por_cluster)

aggregate(dplyr::select(dados, -CITY, -grupo, -cluster), list(dados$cluster), mean)

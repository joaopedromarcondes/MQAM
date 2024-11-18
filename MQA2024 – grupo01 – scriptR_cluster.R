####ao das dependencias
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

#Preparando o Dataset
tabela <- read_csv2("BRAZIL_CITIES.csv")
dados <- tabela[, c("CITY", "IBGE_PLANTED_AREA", "IBGE_CROP_PRODUCTION_$", "IBGE_RES_POP", "TAXES", "IDHM", "RURAL_URBAN")]
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

#IMPOSTO PERCAPTA
#dados_numericos["TAXES"] <- dados_numericos["TAXES"] / dados_numericos["IBGE_RES_POP"]

#BOXPLOT IMPOSTO PER CAPTA
#boxplot(dados_numericos[, "TAXES"])


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
# Criação das tabelas de frequência com intervalos personalizados para cada variável
frequencias <- list(
  IBGE_PLANTED_AREA = table(cut(dados_numericos$IBGE_PLANTED_AREA, 
                                breaks = seq(min(dados_numericos$IBGE_PLANTED_AREA, na.rm = TRUE), 
                                             max(dados_numericos$IBGE_PLANTED_AREA, na.rm = TRUE), 
                                             by = 2))),
  
  IBGE_CROP_PRODUCTION = table(cut(dados_numericos$"IBGE_CROP_PRODUCTION_$", 
                                   breaks = seq(min(dados_numericos$"IBGE_CROP_PRODUCTION_$", na.rm = TRUE), 
                                                max(dados_numericos$"IBGE_CROP_PRODUCTION_$", na.rm = TRUE), 
                                                by = 2))),
  
  IBGE_RES_POP = table(cut(dados_numericos$IBGE_RES_POP, 
                           breaks = seq(min(dados_numericos$IBGE_RES_POP, na.rm = TRUE), 
                                        max(dados_numericos$IBGE_RES_POP, na.rm = TRUE), 
                                        by = 2))),
  
  TAXES = table(cut(dados_numericos$TAXES, 
                    breaks = seq(min(dados_numericos$TAXES, na.rm = TRUE), 
                                 max(dados_numericos$TAXES, na.rm = TRUE), 
                                 by = 2))),
  
  IDHM = table(cut(dados_numericos$IDHM, breaks = seq(0, 1, by = 0.1)))  # Intervalos de 0,1 para IDHM
)

# Exibir as tabelas de frequência
plot(frequencias$IBGE_PLANTED_AREA)
plot(frequencias$IBGE_CROP_PRODUCTION)
plot(frequencias$IBGE_RES_POP)
plot(frequencias$TAXES)
plot(frequencias$IDHM)

# Função para criar gráfico de frequência com intervalos personalizados
criar_grafico_frequencia <- function(dados, variavel, intervalo, titulo) {
  ggplot(dados, aes(x = .data[[variavel]])) +  # Usando .data[[variavel]] para acessar a coluna com nome variável
    geom_histogram(binwidth = intervalo, fill = "skyblue", color = "black", boundary = 0) +
    labs(title = titulo, x = variavel, y = "Frequência") +
    theme_minimal()
}

# Criar gráficos para cada variável
grafico1 <- criar_grafico_frequencia(dados_numericos, "IBGE_PLANTED_AREA", 2, "Frequência de IBGE_PLANTED_AREA")
grafico2 <- criar_grafico_frequencia(dados_numericos, "IBGE_CROP_PRODUCTION_$", 2, "Frequência de IBGE_CROP_PRODUCTION_$")
grafico3 <- criar_grafico_frequencia(dados_numericos, "IBGE_RES_POP", 2, "Frequência de IBGE_RES_POP")
grafico4 <- criar_grafico_frequencia(dados_numericos, "TAXES", 2, "Frequência de TAXES")
grafico5 <- criar_grafico_frequencia(dados_numericos, "IDHM", 0.1, "Frequência de IDHM")

# Exibir os gráficos
library(gridExtra)
grid.arrange(grafico1, grafico2, grafico3, grafico4, grafico5, ncol = 2)


library(DT)
  
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
  
  # Criar a tabela final combinando frequências absolutas e relativas com soma acumulada das frequências relativas
  tabela <- data.frame(
    Intervalo = names(frequencias_absolutas),
    Frequencia_Absoluta = as.vector(frequencias_absolutas),
    Frequencia_Relativa = round(as.vector(frequencias_relativas), 4),  # Frequência relativa com 4 casas decimais
    Soma_Frequencia_Relativa = round(cumsum(as.vector(frequencias_relativas)), 4)  # Soma acumulada das frequências relativas
  )
  
  return(tabela)
}

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
# Visualizando os dados padronizados
datatable(dados_padronizados)

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
dados[which(dados$grupo == 4), ]
table(grupos)
aggregate(dplyr::select(dados, -CITY, -grupo), list(dados$grupo), mean)
datatable(aggregate(dplyr::select(dados, -CITY, -grupo), list(dados$grupo), mean)
)
#library(cluster)

# Aplicar o método PAM com 3 clusters
#modelo_pam <- pam(dados_padronizados, k = 50)

# Visualizar clusters com um gráfico em silhueta
#plot(modelo_pam)


fviz_nbclust(dados_numericos, kmeans, method = "wss")


# Aplicando K-means com k clusters
set.seed(123)  # Para resultados reprodutíveis
kmeans_result <- kmeans(dados_padronizados, centers = 8, nstart = 10)

# Adicionando os clusters aos dados originais
dados$cluster <- as.factor(kmeans_result$cluster)


pca_result <- prcomp(dados_padronizados)
dados_pca <- as.data.frame(pca_result$x)
dados_pca$cluster <- as.factor(kmeans_result$cluster)

ggplot(dados_pca, aes(x = PC1, y = PC4, color = cluster)) +
  geom_point(size = 2) +
  labs(title = "Clusters com PCA") +
  theme_minimal()

observacoes_por_cluster <- kmeans_result$size

# Exibindo o resultado
print(observacoes_por_cluster)


# Selecionar apenas as colunas numéricas
dados_num <- dados[, sapply(dados, is.numeric)]

# Aplicar o aggregate apenas nas colunas numéricas
medias_por_cluster <- aggregate(. ~ cluster, data = cbind(dados_num, cluster = dados$cluster), FUN = mean)

# Visualizando o resultado
print(medias_por_cluster)

datatable(medias_por_cluster)

# Visualizar clusters em 2D
fviz_cluster(kmeans_result, data = dados_padronizados, geom = "point", ellipse = TRUE) +
  labs(title = "Clusters com K-Means")

dados

table(dados$cluster)
aggregate(dplyr::select(dados, -CITY, -grupo, -cluster), list(dados$cluster), mean)
dados[which(dados$cluster == 1), ]

table(dados[(dados$RURAL_URBAN == "Intermediário Remoto"), ]$grupo)
table(dados$RURAL_URBAN)

#kmeans outro codigo

#determinar numero ótimo de clusters
library(factoextra)

# Calcular soma dos quadrados intra-cluster (total within-cluster sum of squares)
fviz_nbclust(dados_padronizados, kmeans, method = "wss") +
  labs(title = "Método do Cotovelo")

#aplicar kmeans
set.seed(123)  # Para resultados reprodutíveis

# Escolha o número de clusters (substitua `k` pelo valor escolhido)
k <-8  # Número de clusters
kmeans_resultados <- kmeans(dados_padronizados, centers = k, nstart = 25)

# Visualizar os resultados
print(kmeans_resultados)

#visualizar os clusters
# Visualizar clusters em 2D
fviz_cluster(kmeans_resultados, data = dados_padronizados, geom = "point", ellipse = TRUE) +
  labs(title = "Clusters com K-Means")

# Número de observações em cada cluster
observacoes_por_cluster <- kmeans_resultados$size

# Exibindo o resultado
print(observacoes_por_cluster)


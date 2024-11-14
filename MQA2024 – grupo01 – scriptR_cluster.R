####ao das dependencias
if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}
install.packages("ggplot2")
install.packages("gridExtra")

library(pacman)
pacman::p_load(dplyr, ggplot2, readxl, readr, DT, factoextra)

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

# Tratar dados
# Remover os valores 0 e os valores menores que 0

dados_negativos_ou_na <- (dados_numericos <= 0) | (is.na(dados_numericos))
length(which(dados_negativos_ou_na))
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

#estatisticas descritivas das variaveis quantitativas
datatable(sapply(dados_numericos, descrever_coluna))

library(ggplot2)
library(gridExtra)

# Função para converter a tabela de frequência em um gráfico de tabela
salvar_tabela_como_imagem <- function(nome, tabela) {
  tabela_df <- as.data.frame(tabela)
  colnames(tabela_df) <- c("Intervalo", "Frequência")  # Nome das colunas

  # Criar a tabela com ggplot2
  p <- ggplot(tabela_df, aes(x = "", y = Intervalo, label = Frequência)) +
    geom_tile(fill = "white", color = "black") +
    geom_text(aes(label = Frequência), size = 5, color = "black") +
    labs(title = nome) +
    theme_minimal() +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.y = element_text(size = 10),
          axis.title = element_blank(),
          panel.grid = element_blank())

  return(p)
}

# Salvar as tabelas como imagens individuais ou combinadas
tabelas <- lapply(names(frequencias), function(nome) salvar_tabela_como_imagem(nome, frequencias[[nome]]))

# Combina as tabelas em uma imagem
ggsave("tabelas_frequencia.png", grid.arrange(grobs = tabelas, ncol = 2), width = 10, height = 8)



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


# Cortar em 3 clusters
grupos <- cutree(modelo_hclust, k = 4)
#print(grupos)
dados$grupo <- grupos
dados[which(dados$grupo == 4), ]
table(grupos)
aggregate(dplyr::select(dados, -CITY, -grupo), list(dados$grupo), mean)

#library(cluster)

# Aplicar o método PAM com 3 clusters
#modelo_pam <- pam(dados_padronizados, k = 50)

# Visualizar clusters com um gráfico em silhueta
#plot(modelo_pam)


fviz_nbclust(dados_numericos, kmeans, method = "wss")


# Aplicando K-means com k clusters (por exemplo, k = 3)
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

dados

table(dados$cluster)
aggregate(dplyr::select(dados, -CITY, -grupo, -cluster), list(dados$cluster), mean)
dados[which(dados$cluster == 1), ]

table(dados[(dados$RURAL_URBAN == "Intermediário Remoto"), ]$grupo)
table(dados$RURAL_URBAN)

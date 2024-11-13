####Instalacao das dependencias
if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}

library(pacman)
pacman::p_load(dplyr, ggplot2, readxl, readr, DT)

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
dados <- tabela[, c("CITY", "IBGE_PLANTED_AREA", "IBGE_CROP_PRODUCTION_$")]
dados_numericos <- tabela[, c("IBGE_PLANTED_AREA", "IBGE_CROP_PRODUCTION_$")]

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

dados_numericos[, "IBGE_PLANTED_AREA"] <- log(dados_numericos[, "IBGE_PLANTED_AREA"])
dados_numericos[, "IBGE_CROP_PRODUCTION_$"] <- log(dados_numericos[, "IBGE_CROP_PRODUCTION_$"])

### Tratamento dos dados ###
dados_padronizados <- scale(dados_numericos)
class(dados_padronizados)

# Calcular a matriz de distâncias
distancia <- dist(dados_padronizados, method = "euclidean")

# Aplicar o método de clusterização hierárquica (método de ligação completa)
modelo_hclust <- hclust(distancia, method = "complete")

# Plotar o dendrograma
plot(modelo_hclust, labels = rownames(dados),
     main = "Dendrograma de Clusterização Hierárquica")


# Cortar em 3 clusters
grupos <- cutree(modelo_hclust, k = 8)
print(grupos)
dados$grupo <- grupos
dados[which(dados$grupo == 5), ]
table(grupos)


library(cluster)

# Aplicar o método PAM com 3 clusters
modelo_pam <- pam(dados_padronizados, k = 50)

# Visualizar clusters com um gráfico em silhueta
plot(modelo_pam)


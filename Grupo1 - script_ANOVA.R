# importar bibliotecas e dependências
library(readxl)

# definir funções importantes
moda <- function(x) {
    modal <- unique(x)
    modal[which.max(tabulate(match(x, modal)))]
}

# importar banco de dados
BRAZIL_CITIES_REV2022_CSV <- read_excel("BRAZIL_CITIES_REV2022.CSV.xlsx")

# visualização dos dados brutos - teste para ver se funcionou as importações
head(BRAZIL_CITIES_REV2022_CSV)

# seleciona colunas que serão usadas na análise - IDH e População
idh_e_pop <- BRAZIL_CITIES_REV2022_CSV[, c("IDHM", "IBGE_RES_POP")]


# Tratamento do IDH
# a partir da análise descritiva do IDH, percebemos alguns outliers em IDH
boxplot(idh_e_pop$IDHM)
summary(idh_e_pop)
moda(idh_e_pop$IDHM)
var(idh_e_pop$IDHM)
sd(idh_e_pop$IDHM)

# seleciona e remove os outliers de IDH
outliers_idh <- boxplot.stats(idh_e_pop$IDHM)$out
idh_e_pop <- idh_e_pop[-which(idh_e_pop$IDHM %in% outliers_idh),]
outliers_idh

# visualização dos novos dados de IDH
boxplot(idh_e_pop$IDHM)
summary(idh_e_pop)
moda(idh_e_pop$IDHM)
var(idh_e_pop$IDHM)
sd(idh_e_pop$IDHM)

# Tratamento da População
# a partir do boxplot, percebe-se que a população em cada estado tem uma amplitude muito grande de dados
boxplot(idh_e_pop$IBGE_RES_POP)
summary(idh_e_pop)
moda(idh_e_pop$IBGE_RES_POP)
var(idh_e_pop$IBGE_RES_POP)
sd(idh_e_pop$IBGE_RES_POP)

# é feito uma transformação logarítmica
idh_e_pop$IBGE_RES_POP <- log(idh_e_pop$IBGE_RES_POP)
idh_e_pop <- idh_e_pop[-which(idh_e_pop$IBGE_RES_POP == -Inf),]

# percebe-se que melhorou a amplitude dos dados
boxplot(idh_e_pop$IBGE_RES_POP)
summary(idh_e_pop)
moda(idh_e_pop$IBGE_RES_POP)
var(idh_e_pop$IBGE_RES_POP)
sd(idh_e_pop$IBGE_RES_POP)



# Daqui pra frente foi testes
divisoes_idh <- c("0 - 499", "500 - 599", "600 - 699", "700 - 799", "800 - 1000")
breaks_idh <- c(0, 5000, 6000, 7000, 8000, 10000)

# tabela de frequencias
table(cut(x=idh_e_pop$IDHM, breaks=breaks_idh, labels=divisoes_idh))
idh_e_pop

# plota um grafico
plot(idh_e_pop)
# detecta valores nulos
sum(is.na(idh_e_pop$IBGE_RES_POP))
# ordena os dados pela coluna pop
idh_e_pop[order(idh_e_pop$IBGE_RES_POP), decresing=true]
# plota um gráfico esquisito
plot(as.factor(idh_e_pop$IDHM))


# deve dar pra filtrar os dados assim
# nao consigo testar agora
# o resto deve ser a mesma ideia
pop_em_intervalos[, "Muito Baixo"] <- idh_e_pop[idh_e_pop$IDHM < 5000]


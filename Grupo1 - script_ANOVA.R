library(readxl)

BRAZIL_CITIES_REV2022_CSV <- read_excel("BRAZIL_CITIES_REV2022.CSV.xlsx")

head(BRAZIL_CITIES_REV2022_CSV)

idh <- (BRAZIL_CITIES_REV2022_CSV$"IDHM")

boxplot(idh)

outliers_idh <- boxplot.stats(idh)$out

idh_no_outlier <- idh[-which(idh %in% outliers_idh)] 
boxplot(idh_no_outlier)


ibge_res_pop <- BRAZIL_CITIES_REV2022_CSV$IBGE_RES_POP

boxplot(log(ibge_res_pop))

outliers_pop <- boxplot.stats(ibge_res_pop)$out
outliers_pop
pop_no_outlier <- ibge_res_pop[-which(ibge_res_pop %in% outliers_pop)] 

boxplot(pop_no_outlier)

log_pop = log(ibge_res_pop)
plot(idh, log_pop, type="p")
plot(as.factor(idh))
plot(as.factor(log_pop))

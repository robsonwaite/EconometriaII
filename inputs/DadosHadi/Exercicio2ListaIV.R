### Questão 2 Lista IV de Econometria

setwd("/Users/diogobmbraga/Documents/UFF/EconometriaI/All_Data")

datP83 = read.table("P083.txt", header = T, sep = "", na.strings = "EMPTY", strip.white = T)

## Organização das Variáveis
X = vector("list", ncol(datP83)-1)
for(i in 1:length(X)){
  X[[i]] = datP83[,i+1]
}

Y = datP83[,1]

## Regredindo Modelo 1
fit1 = lm(Y ~ X[[1]], data = datP83)

## Regredindo Modelo 2
fit2 = lm(Y ~ 1, data = datP83)

## Regredindo Modelo 3
fit3 = lm(Y ~ X[[1]] + X[[2]], data = datP83)


## Analisar e Comparar os modelos

summary(fit1)
summary(fit2)
summary(fit3)

anova(fit1,fit2)
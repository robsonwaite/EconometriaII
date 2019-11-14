## Goldfeld-Quandt Test

#  Ordene as obs de acordo com os valores de Xi (de ordem crescente)
#  omita 'c' observação centrais, em que 'c' é especificada a priori. Divida as (n's) obs, ordenados em 2 grupos
#  cada um c/ n-c/2 obs
#  Regrida OLS em cada grupo e obtenha SQE1 e SQE2 
#  Compute a razão Lambda = (SQE2/g.l2) / (SQE1/g.l1)        SQE soma dos quadrados dos residuos
#  se lambda > F critico, rejeitamos a homocedasticidade.

#  definir o C ? 


Table113 <- read.table("Table 11.3.txt", header = T, sep = "", na.strings = "EMPTY", strip.white = T, dec = ".")

# Seguindo a recomendacao, sugerimos c = 4, ou seja, removeremos as 4 observacoes centrais de X e Y.
# Na tabela, as colunas 3 e 4 sao as colunas 1 e 2 ordenadas do menor para o maior, como manda o teste.
c <- 4
GQTest <- Table113[-(((nrow(Table113) - c)/2 + 1):(((nrow(Table113) - c)/2) + 4)),]
GQTest1 <- GQTest[1:13,]
GQTest2 <- GQTest[14:26,]

FReg <- lm(GQTest1[,3] ~ GQTest1[,4])
summary(FReg)

SReg <- lm(GQTest2[,3] ~ GQTest2[,4])
summary(SReg)
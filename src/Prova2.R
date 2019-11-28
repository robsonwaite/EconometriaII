#<============================================================================>#                                                                            #   
#     Autor:Robson Waite                                                       #
#     Data:26/11/2019                                                          #
#     Titulo: Prova Antiga - Econometria II                                    #
#     Descrição: Exercicios                                                    #           
#<============================================================================>#
## Preparando Ambiente #########################################################
# Diretorio
setwd("D:\\Download\\EconometriaII (1)\\TabelasProva2EconometriaII\\") 
# Pacotes
library(tseries)
library(lmtest)
# Questão 01 ###################################################################

tabela1 = read.table("Tabela1.txt", header = T, stringsAsFactors = F)
# (A) faça o grafico de Y vs X
plot(tabela1$X,tabela1$Y)
# (B) Regrida Y em X e examine os residuos dessa regressão
fit1 = lm(formula = Y ~ X, data = tabela1)
summary(fit1)
# Y preço das ações e X preços ao consumidor

# Os resultados demostram que o coeficiente angular estimado é significante 
# ao nível de 5%. A equação mostra que, quando preços ao consumidor aumentam, 
# uma unidade o preço das ações aumentam em média 0.7574.

# Exame dos residuos

plot(tabela1$X, resid(fit1))
#  É perceptivel que que existe um outliner na analise residual. O chile predomina
# sobre os outros.

# (C) Remove chile é repita o processo.

# Removendo Chile
tabela1 = tabela1[-5,]

plot(tabela1$X,tabela1$Y)

fit1 = lm(formula = Y ~ X, data = tabela1)
summary(fit1)
# Y preço das ações e X preços ao consumidor

# Os resultados demostram que o coeficiente angular estimado não é significante 
# ao nível de 5%. Os dados não tem qualquer relação economica.

# Exame dos residuos
plot(tabela1$X, resid(fit1))
# Os dados agora se espalham mais sobre o grafico, não havendo predominancia
# visivel de nenhum dos paises restantes sobre os demais.

# (Uma conclusão)
# Como vemos em (b) o coeficiente angular era muito significativo, mas não 
# o é nesta regressão. Podemos observar como um outlier pode deturpar os resultados
# da regressão.

# (D) Ao comparar os graficos residuas (Antes de depois de remover o CHILE), vemos
# que a relação entre Y e X ficou pequena (insifnificante estatisticamente)
# Portanto, qualquer indicio de heterocedasticidade é espúrio.


# Questão 02 ###################################################################

tabela2 = read.table("Tabela2.txt", header = T, stringsAsFactors = F)

# (A) Rode o Modelo P&D(y) ~ X(Vendas)
fit2 = lm(formula = RD ~ SALES, data = tabela2)
summaryfit2 = summary(fit2)

# Os resultados demostram que o coeficiente angular estimado é estatisticamente 
# significante ao nível de 5%. A equação mostra que um aumento de, por exmeplo, 
# uma unidade de vendas, causa um aumento de aproximadamente 0.032 em P&D.

# (B) Faça o grafico dos residuos padronizados vs Vendas

plot(tabela2$SALES,rstandard(fit2))

# A analise do grafico permite identificar uma forma conica, sugerindo um
# a existencia de heterocedasticidade nos dados.

# (C) Faça o teste de Park e White. Com base neles aceite ou rejeite a heteroce..

# T.Park

res2 = resid(fit2)^2
x = tabela2$SALES
testepark = lm(formula = log(res2) ~ log(x))
summary(testepark)

# Com um p-valor igual a 0.262 obviamente não existe relação estatistica entre 
# as duas variaveis. Segundo o Teste Park podemos concluir que não há heteroce-
# dasticidade na variancia dos erros.

# A hipotese nula H0: B2 = 0, é estatisticamente signigicativa, logo não existe
# relação estatistica entre as variaveis.

# T.White

fitwhite = lm(formula = res2 ~ x + I(x*x))
n = nrow(tabela2)
r2 = summary(fitwhite)$r.squared
nr2 = n*r2

vcrit = qchisq(0.95,2)

# nr2(5.21) < vcrit(5.99) Como o valor do qui-quadrado não excede o valor critico
# do qui-quadrado, se conclui que segundo o teste white, não existe heterocedasticidade
# nos dados. (Que alfa1 e alfa2 são iguais a zero.)

pvalue = 1 - pchisq(nr2,2)

# O p-valor é igual a 0.0738, apesar de pequeno o valor é superior aos 5%
# aceitando-se a hipotese nula de homocedasticidade.

# (Resposta) Uma vez que o teste de white não requer a hipótese da normalidade,
# sendo considerado um teste geral, damos mais peso aos seus resultados, assim,
# Aceitamos a hipotese de homocedasticidade.

# Poderiamos ter rejeitado com base que o teste de park não aceita, e que o 
# p valor do teste de white resulta em um valor pequeno, proximo dos 5%.

# (D)

library(lmtest)
library(sandwich)

coeftest(fit2, vcov = vcovHC(fit2, type="HC1"))

# EP de white:
# B1: 533.93
# B2: 0.010

# EP comum
# B1: 991
# B2: 0.008329

# Não houve variação significativa dos coeficientes estimados, podemos supor
# que não vale a pena estimarmos por Erros Padrões Robustos.

# Ao mesmo tempo o EP do intercepto cai quase pela metade, e o EP do coeficiente
# angular fica mais que 10 vezes maior. Dessa forma, sem maior aprofundamento
# sobre os dados fica complicado supor se vale ou não a pena uma estimação com 
# correção de erros.

# Questão 03 ###################################################################

# (A) Estime o modelo, os resultados fazem sentido do ponto de vista economico ?

tabela3 = read.table("Tabela3.txt", header = T, stringsAsFactors = F)

fit3 = lm(formula = Expenditure ~ Stock, data = tabela3)
summary(fit3)

# Pode ser observado pelo p-valor (8.99e-14) que o coeficiente angular estimado  
# possui significancia estatistica para o nivel de 5%. A equação demostra que para
# cada um bilhão adicionado ao dinheiro circulante, o consumo aumenta em 2.3 bilhões
# de dólares. Do ponto de vista economico o resultado condiz com uma politica 
# economica expansiva, que aumenta a oferta de moeda impactando na demanda, ou no caso
# no consumo.

# (B) Verifique a presença de correlação por:

# Teste Grafico

plot(tabela3$Year, resid(fit3), type = "o")
abline(h=0)

# O Analise grafica indica ausência de padrão sistematico, sugerindo ausência de
# autocorrelação no modelo.

# Durbin-Watson

lmtest::dwtest(fit3)

# Pelo teste de Durbin-Watson temos um valor de d = 0.328, com uma estatistica
# de teste p-valor = 2.303e-08. Com um p-valor tão baixo não restam duvidas 
# de que a hipotese nula, de ausência de autocorrelação, será rejeitada indicando 
# a existencia de autocorrelação no modelo.

# Run Test

resR <- as.vector(residuals(fit3))
sinaisR <- sign(resR)
R <- length(rle(sinaisR)$lengths)

RunsR <- rle(sinaisR)
N <- length(sinaisR)
N1 <- sum(RunsR$lengths[RunsR$value==1])
N2 <- sum(RunsR$lengths[RunsR$value==-1])
ER <- 2*N1*N2/N + 1
VR <- (4*(N1^2)*(N2^2) - 2*N1*N2*N)/((N^2)*(N-1))
sdR <- sqrt(VR)

# ER +- 1.96*(sdR)
superior = ER + 1.96*(sdR)
Inferior = ER - 1.96*(sdR)

# Como R = 5, e o intervalo critico sendo, 6.51 <  R* < 14.68, o R não pertence 
# ao intervalo critico, sendo assim, temos indicios da existencia de autocorrelação
# pelo Run test.

# (Resposta) Apesar do grafico não apresentar nenhuma indicativo de autocorrelação
# ambos os testes nos levaram a mesma conclusão, de que existe autocorrelação
# no modelo escolhido.

# (C) Use o metodo de Cochrane-Orcutt para chegar a rho. (Não cai)
# (D) ...

# Questão 04 ####################################################################

tabela4 = read.table("Tabela4.txt", header = T, stringsAsFactors = F)

# (A)  Sim, uma vez que o aumento do crescimento populacional iria impactar 
# no setor da construção civil, com o aumento do numero de habitações.

# (B) Faça o modelo H ~ P

fit4 = lm(formula = H ~ P, data = tabela4)
summary(fit4)

# Os dados demostram que o coeficiente angular estimado é estatisticamente signi
# -ficativo ao nível de 5%. A equação demotrou que para o aumento de uma unidade
# no crescimento populacional, a construção de casas aumenta em 0.07, o que condiz
# com a ideia inicial que se tinha sobre a relação entre as variaveis. Podemos confirmar
# então que existe significado economico.

# (C) Faça o teste Durbin Watson e o Run test

# Durbin Watson test

lmtest::dwtest(fit4)

# O teste de Durbin Watson apresentou valor d = 0.62, com um p-valor = 6.645e-06,
# baseando-se em um valor p tão baixo, fica claro que o modelo apresenta 
# autocorrelação.

# Run test 

resR <- as.vector(residuals(fit4))
sinaisR <- sign(resR)
R <- length(rle(sinaisR)$lengths)

RunsR <- rle(sinaisR)
N <- length(sinaisR)
N1 <- sum(RunsR$lengths[RunsR$value==1])
N2 <- sum(RunsR$lengths[RunsR$value==-1])
ER <- 2*N1*N2/N + 1
VR <- (4*(N1^2)*(N2^2) - 2*N1*N2*N)/((N^2)*(N-1))
sdR <- sqrt(VR)

superior = ER + 1.96*sdR
inferior = ER - 1.96*sdR

# Analisando os valores temos o R = 6, com um intervalo critico de [8.59,18.04], 
# logo o R não pertence ao intervalor critico, indicando a existencia de autocorrelação
# no modelo.


# (Resposta) Com base em ambos os testes chegamos a conclusão que o modelo possui
# autocorrelação.

# (D) Faça o modelo H ~ P + D

fit4 = lm(formula = H ~ P + D, data = tabela4)
summary(fit4)

# Os dados demostram que ambos os coeficientes angulares estimados possuem 
# significancia estatistica ao nível de 5%. A equação demostra que para cada
# que a variação de 1 und de crescimento populacional, a construção de habitações
# aumenta em 0,03, e que uma variação de 1 und de disponibilidade de credito imob
# a construção aumenta em 0.76. Ambos os valores possuem sentido economico, logo que se
# espera que o aumento do credito facilite o financiamento de novas habitações.

# (E) Refaça os testes.

# Durbin Watson test

lmtest::dwtest(fit4)

# O teste de Durbin Watson apresentou valor d = 1,85, com uma estatistica de teste
# p-valor = 0.2316, com um p valor tão alto, não restam duvida de que a hipotese nula
# de ausencia de correlação não será rejeitada.

# Run test 

resR <- as.vector(residuals(fit4))
sinaisR <- sign(resR)
R <- length(rle(sinaisR)$lengths)

RunsR <- rle(sinaisR)
N <- length(sinaisR)
N1 <- sum(RunsR$lengths[RunsR$value==1])
N2 <- sum(RunsR$lengths[RunsR$value==-1])
ER <- 2*N1*N2/N + 1
VR <- (4*(N1^2)*(N2^2) - 2*N1*N2*N)/((N^2)*(N-1))
sdR <- sqrt(VR)

superior = ER + 1.96*sdR
inferior = ER - 1.96*sdR

# Analisando os valores temos o R = 13, com um intervalo critico de [8.40,17.59], 
# como o R pertence ao intervalor critico temos um indicativo de ausencia de autocorrelação.

# (Resposta) Diferente dos resultados do modelo anterior, o modelo utilizado agora
# possui ausência de autocorrelação, fato analisando em ambos os testes.

# (F) O modelo anterior estava com erro de especificação, a falta variavel de disponibilidade
# de credito imobiliario, estava causando autocorrelação. Uma vez incluida o modelo
# não apresenta mais autocorrelação.

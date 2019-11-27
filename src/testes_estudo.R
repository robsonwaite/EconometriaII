#<============================================================================>#                                                                            #   
#     Autor:Robson Waite                                                       #
#     Data:26/11/2019                                                          #
#     Titulo: Testes -- Econometria II                                         #
#     Descrição: Exercicios                                                    #           
#<============================================================================>#
## Preparando Ambiente #########################################################
# Diretorio
setwd("./inputs/DadosGujarati/") 
# Pacotes
library(tseries)
library(lmtest)
# Teste Park ###################################################################
tabela11.1 = read.table(file = "Table 11.1.txt", header = T,stringsAsFactors = F)
Y = tabela11.1[11,-1]
X = tabela11.1[13,-1]
X = unlist(X)
Y = unlist(Y)

fit_park = lm(formula = Y ~ X)
summary(fit_park)

# Os resultados revelam que o coeficiente angular estimado é significante no nível de
# 5%, com base no p-valor = 0,0524. A equação mostra que, quando a produtividade
# no trabalho aumenta, em por exemplo um dolar, a remuneração da mão de obra aumenta em 
# média 23 centavos.

# Teste Park

res2 = resid(fit_park)^2

teste_park = lm(formula = log(res2) ~ log(X))
summary(teste_park)

# Com um p-valor igual a 0.526 obviamente não existe relação estatistica entre 
# as duas variaveis. Segundo o teste park podemos concluir que não há heteroce-
# dasticidade na variancia dos erros.

absres2 = abs(res2)
teste_glejser = lm(formula = absres2 ~ X)
summary(teste_glejser)

# Com um p-valor igual a 0.940 obviamente não existe relação estatistica entre
# as duas variaveis. Segundo o teste Glejser podemode concluir que não há hetero
# cedasticidade na variancia dos erros.

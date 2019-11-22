#<============================================================================>#                                                                            #   
#     Autor:Robson Waite                                                       #
#     Data:21/11/2019                                                          #
#     Titulo: Lista 4 - Econometria II                                         #
#     Descrição: Exercicios                                                    #           
#<============================================================================>#
## Preparando Ambiente #########################################################
  
  setwd("./inputs/DadosGujarati/") 


# Questão 01 ###################################################################
  # Lendo dados ---
    tabela12.7 = read.table("Table 12.7.txt", skip = 12, header = T, stringsAsFactors = F) 
  # A) Interpretando Modelo - logCt = B1 + B2logIt + B3logLt + B4logHt + B5logAt + ut
    fit01 = lm(formula = log(C) ~ log(I)+log(L)+log(H)+log(A), data = tabela12.7)
    summary(fit01)
    # As variaveis I, L e A possuem significancia em 5%. (descrever o restante)
  # B) Obtenha os residuos e os residuos padronizados e faça um grafico 
      #Residuos 
      plot(tabela12.7$YEAR ,residuals(fit01),
             ylab = "Residuos", # Legenda de y
             xlab = "Tempo", # Legenda de Y
             type = "l", # tipo de grafico - pontos
             main = "Análise de Resíduos", # Titulo do grafico
             col = "green", # Cor dos Plots
             pch= 20, # desenho dos "plots"
             cex= 1  # tamanho dos "pontos" plotados
        )
    # Pelo Grafico percebe-se 'ausência de padrão sistemático'
      
    
    
    
    
    
    
    
    
    
    
    
    
# Questão 02 ###################################################################
# Questão 03 ###################################################################
# Questão 04 ###################################################################
 
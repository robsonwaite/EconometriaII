#<============================================================================>#                                                                            #   
#     Autor:Robson Waite                                                       #
#     Data:21/11/2019                                                          #
#     Titulo: Lista 4 - Econometria II                                         #
#     Descrição: Exercicios                                                    #           
#<============================================================================>#
## Preparando Ambiente #########################################################
  # Diretorio
  setwd("./inputs/DadosGujarati/") 
  # Pacotes
  library(tseries)
  library(lmtest)

# Questão 01 ###################################################################
  # Lendo dados ---
    tabela12.7 = read.table("Table 12.7.txt", skip = 12, header = T, stringsAsFactors = F) 
  # A) Interpretando Modelo - logCt = B1 + B2logIt + B3logLt + B4logHt + B5logAt + ut
    fit01 = lm(formula = log(C) ~ log(I)+log(L)+log(H)+log(A), data = tabela12.7)
    summary(fit01)
    # As variaveis I, L e A possuem significancia em 5%. (descrever o restante)
  # B) Obtenha os residuos e os residuos padronizados e faça um grafico 
      par(mfrow=c(2,1))
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
      
      # Residuos Padronizados
      plot(tabela12.7$YEAR ,rstandard(fit01),
           ylab = "Residuos", # Legenda de y
           xlab = "Tempo", # Legenda de Y
           type = "l", # tipo de grafico - pontos
           main = "Análise de Resíduos Padronizados", # Titulo do grafico
           col = "red", # Cor dos Plots
           pch= 20, # desenho dos "plots"
           cex= 1  # tamanho dos "pontos" plotados
      )   
    # Pelo Grafico percebe-se 'ausência de padrão sistemático'
  
  # C) Estatistica Durbin Watson (d) - Fale sobre a natureza da autocorrelação
    # Calculo do Durbin
      d1 = lmtest::dwtest(fit01)
      # >> Com d = 0.95 temos uma evidencia indicando a correlação serial positiva.
    
  # D) Execute o Run Test
      
      resR = as.vector(residuals(fit01))
      sinaisR = sign(resR)
      R = length(rle(sinaisR)$lengths)
      RunsR = rle(sinaisR)
      N = length(sinaisR)
      N1 = sum(RunsR$lengths[RunsR$value==1])
      N2 = sum(RunsR$lengths[RunsR$value==-1])
      ER = 2*N1*N2/N + 1
      VR = (4*(N1^2)*(N2^2) - 2*N1*N2*N)/((N^2)*(N-1))
      sdR = sqrt(VR)
      
      du = ER + 1.96*sdR
      dl = ER - 1.96*sdR
    
      #  R(9) < dl(10.55) < R() < du(20.91) -> Como R não pertence ao Intervalo Critivo
      #  Logo estamos diante de Autocorrelação ou correlação serial positiva Confirmando
      #  o que estava sendo indicado pelo Durbin-Watson .
    
  # E) Teorica
      
# Questão 02 ###################################################################
  # Leitura dos dados
    tabela12.8 = read.table("Table 12.8.txt", skip = 6, header = T, stringsAsFactors = F)
  # A) Verifique que o d de DW é igual 0.4148
    d2 = lmtest::dwtest(formula = Y ~ X, data = tabela12.8)
     # Sim, DW é igual a 0.4148
  # B) Existe Evidencia de Autocorrelação Positiva ?
      
        
# Questão 03 ###################################################################
# Questão 04 ###################################################################
 
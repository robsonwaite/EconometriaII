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
     # Com um valor de d tão proximo de 0, temos evidência de correlação serial positiva.
  # C) Estime rho pelo metodo DW e pelo Cochrane-Orcutt
      # Rho pelo metodo DW - pela formula d ~ 2( 1 - rho)   
      rho = 1 - ((d2$statistic)/2)
      # rho = 0,793
  
      # rho pelo metodo CO >> Não entendi Bulhufas
      fit02 = lm(formula = Y ~ X, data = tabela12.8)
      
      exog = model.matrix(fit02)
      endo = model.frame(fit02)[,1]
      
      n = length(endo)
      res = resid(fit02)
      
      rest = res[-1]
      res_1 = res[-n]
      
      mod1 = lm(rest ~ 0 + res_1)
      rho = summary(mod1)$coeff[1]
      # rho = 0,66102
      
      # Proximo valor de rho
      
      y <- endo[-1]-rho*endo[-n]
      X <- exog[-1,]-rho*exog[-n,]
      
      
      mod2 <- lm(y ~ 0 + X)
      
      res2 <- endo - coef(mod2)[1] - as.matrix(exog[, -1])%*%coef(mod2)[2]
      res2t <- res2[-1]
      res2_1 <- res2[-n]
      
      mod3 <- lm(res2t ~ 0 + res2_1)
      rho2 <- summary(mod3)$coeff[1]
      
      # rho2 = 0.664057 
      
        
# Questão 03 ###################################################################
    # Gerando os dados ---
      Et = c(0.464, 2.026, 2.455, -0.323, -0.068, 0.296,-0.288, 1.298, 0.241, -0.957)
      Xt = seq(1:10)
      ut = c()
      ut[1] = 0.9*10 + Et[1] # u0 = 10
      for(i in 2:10){
        ut[i] = 0.9*ut[i - 1] + Et[i]
      } 
      Yt = 3 + 0.5*Xt + ut
    # A) Estime o Modelo  
      
      fit03 = lm(formula = Yt ~ Xt)
      summary(fit03)
      
      # O B2 não possui significancia estatistica a qualquer nivel, não sendo possivel
      # dar a ele sentido economico.
      
      d3 = lmtest::dwtest(fit03)
      # A estatistica d = 1.296
      
    # B)  
      
      ut = c()
      ut[1] = 0.9*17 + Et[1] # u0 = 17
      for(i in 2:10){
        ut[i] = 0.9*ut[i - 1] + Et[i]
      } 
      Yt = 3 + 0.5*Xt + ut
      # Modelo 2
      fit03 = lm(formula = Yt ~ Xt)
      summary(fit03)
      
      # O B2 possui significancia estatistica para um n° de 95% de confiança,  sendo possivel
      # dar a ele sentido economico.
      
      d3 = lmtest::dwtest(fit03)
      # A estatistica d = 1.4148
      
      # Monte Carlo
      
      coeficienteX <- c()
      coeficienteC <- c()
      r2 <- c()
      dw <- c()
      
      for(j in 1:10){
        for(i in 1:10){
          if(i == 1){ 
            ut[i] <- 0.9*17 + rnorm(1)
            } else{
              ut[i] <- 0.9*ut[i - 1] + rnorm(1)
                  }
                }
            Yt <- 3 + 0.5*Xt + ut
            mod <- lm(Yt ~ Xt)
            coeficienteC[j] <- coef(mod)[["(Intercept)"]]
            coeficienteX[j] <- coef(mod)[["Xt"]]
            r2[j] <- summary(mod)$r.squared
            dw[j] <- lmtest::dwtest(mod)$statistic
        }
      
        par(mfrow = c(4,1))
        plot(coeficienteC,type = "p", main = "Coeficiente Constante")
        plot(coeficienteX,type = "p", main = "Coeficiente X")
        plot(r2,type = "p", main = "Coef de Ajuste")
        plot(dw,type = "p", main = "Estatistica de Teste")
      
    # C) 
        # Monte Carlo
        
        coeficienteX <- c()
        coeficienteC <- c()
        r2 <- c()
        dw <- c()
        
        for(j in 1:10){
          for(i in 1:10){
            if(i == 1){ 
              ut[i] <- 0.3*17 + rnorm(1)
            } else{
              ut[i] <- 0.3*ut[i - 1] + rnorm(1)
            }
          }
          Yt <- 3 + 0.5*Xt + ut
          mod <- lm(Yt ~ Xt)
          coeficienteC[j] <- coef(mod)[["(Intercept)"]]
          coeficienteX[j] <- coef(mod)[["Xt"]]
          r2[j] <- summary(mod)$r.squared
          dw[j] <- lmtest::dwtest(mod)$statistic
        }
        
        par(mfrow = c(4,1))
        plot(coeficienteC,type = "p", main = "Coeficiente Constante")
        plot(coeficienteX,type = "p", main = "Coeficiente X")
        plot(r2,type = "p", main = "Coef de Ajuste")
        plot(dw,type = "p", main = "Estatistica de Teste")
        
            
      
# Questão 04 ###################################################################
    # Leitura dos dados
      tabela12.9 = read.table("Table 12.9.txt", skip = 8, header = T, stringsAsFactors = F)
    # A) Estime a Regressão  Yt = B1 + B2Xt + ut
      fit04 = lm(formula = INVENTORIES ~ SALES, data = tabela12.9)
    # B) Faça o Teste DW
      d4 = lmtest::dwtest(fit04)
      # d = 1.37 e p-value = 0.01179
      
    # C) Faça o teste LM - Como você escolheria a ordem p?
      
      # Escolha da Ordem p 
        teste = arima(resid(fit04), order = c(9,0,0), xreg = tabela12.9$SALES)
        coeftest(teste)
        # tentando variar as defasagem de 1 para 9 de forma que aponte quais 
        # possiveis ordens de AR podem ser significativas, verificamos que 
        # o AR 3 é o mais significativo.
      
      # Teste BG ou LM
      lm4 = lmtest::bgtest(fit04,order = 3)
      # LM test = 8.5078, df = 3, p-value = 0.0366
 
      # Região de Rejeição 
      ZC = qchisq(0.95, 3)
      # Zona critica =  7.8147
      
      # Resposta.
      # Pelo p-value = 0.0366 se afirmarmos que existe autocorrelação estaremos 
      # sujeitos a um erro de 3.66 % segundo o teste de LM.
      
      
    # D) Como você transformaria o modelo para remover a autocorrelação tomando como base o teste da letra (d)
      
      # Poderiamos corrigir a autocorrelação serial por meio do método dos Mínimos
      # Quadrados Generalizados - utilizando o metodo da diferença generalizada quando
      # rho é conhecido, ou empregando o método da primeira diferença quando rho
      # é desconhecido (maioria dos casos)
      
      
    # E) Repita as etapas anteriores com a forma funcional loglinear LogYt = B1 + LogB2Xt + ut
      
      tabela12.9 = log(tabela12.9) # transformando a tabela.
      
      # A2) Estime a Regressão  Yt = B1 + B2Xt + ut
      fit04 = lm(formula = INVENTORIES ~ SALES, data = tabela12.9)
      # B2) Faça o Teste DW
      d4 = lmtest::dwtest(fit04)
      # DW = 1.2078, p-value = 0.002051
      
      # C) Faça o teste LM - Como você escolheria a ordem p?
      
      # Escolha da Ordem p 
      teste = arima(resid(fit04), order = c(9,0,0), xreg = tabela12.9$SALES)
      coeftest(teste)
      # tentando variar as defasagem de 1 para 9 de forma que aponte quais 
      # possiveis ordens de AR podem ser significativas, verificamos que 
      # o AR 3 é o mais significativo.
      
      # Teste BG ou LM
      lm4 = lmtest::bgtest(fit04,order = 1)
      # LM test = 7.7105, df = 3, p-value = 0.05239 => Ordem 3
      # LM test = 6.3398, df = 1, p-value = 0.01181 => Ordem 1
      
      # Região de Rejeição 
      ZC = qchisq(0.95, 1)
      # Zona critica =  7.8147 => Ordem 3
      # Zona critica =  3.8414 => Ordem 1
      
      # Resposta.
      # Pelo p-value = 0.05239 se afirmarmos que existe autocorrelação estaremos 
      # sujeitos a um erro de 5.24 % segundo o teste de LM.
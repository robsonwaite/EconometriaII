#<============================================================================>#                                                                            #   
#     Autor:Robson Waite                                                       #
#     Data:05/11/2019                                                          #
#     Titulo: Lista 3 - Econometria II                                         #
#     Descrição: Exercicios                                                    #           
#<============================================================================>#                                                                            #   
## Configurações Gerais - Diretorio - Pasta de dados do Gujarati ###############
    pastaGujarati = "./inputs/DadosGujarati"
  # Carregando testes ----------------------------------------------------------
    source("./src/Park.R")    # teste de Park - função
    source("./src/Glejser.R") # teste de Glejser - função
    source("./src/Goldfeld-Quandt.R") # teste de Goldfeld-Quandt com k = 2
    source("./src/White.R") # teste de white com k = 2
  # Instalando e carregando pacotes --------------------------------------------
    if(!"sandwich" %in% installed.packages()[,1]) install.packages("sandwich")
    library(sandwich, quietly = T,verbose = F)
    if(!"lmtest" %in% installed.packages()[,1]) install.packages("lmtest")
    library(lmtest, quietly = T, verbose = F)
    if(!"ggplot2" %in% installed.packages()[,1]) install.packages("ggplot2")
    library(ggplot2, quietly = T, verbose = F)
    if(!"gridExtra" %in% installed.packages()[,1]) install.packages("gridExtra")
    library(gridExtra, quietly = T, verbose = F)
## Questão 1 ###################################################################
  ## 1.1.0 Configurações Iniciais ##############################################
    # 1.1.1 Carregando Base de dados ###########################################
      # Carregando Dados
        tabela2.8 = read.table(file = paste(pastaGujarati,"Table 2.8.txt", sep = "/"),header = T, stringsAsFactors = F)
  ## 1.2.0 Analise - Total Gasto x Total Gasto com Alimentação  ################
    # 1.2.1 Regressão ----------------------------------------------------------
      fit = lm(formula = FOODEXP ~ TOTALEXP, data = tabela2.8)
      # Grafico dos Resíduos
      plot(x = (summaryFit$residuals), # Valores de y (no grafico)
           xlab = "Resíduos", # Legenda de y
           type = "p", # tipo de grafico - pontos
           main = "Análise de Resíduos", # Titulo do grafico
           col = "dark red", # Cor dos Plots
           pch=20, # desenho dos "plots"
           cex=1  # tamanho dos "pontos" plotados
      )
      abline(h=0, lty=2)
    # 1.2.2 Sumario ------------------------------------------------------------
      summaryFit = summary(fit)
    # 1.2.3 Grafico residuos x total gasto - Analise Informal ------------------
      plot(y = (summaryFit$residuals), # Valores de y (no grafico)
           ylab = "Resíduos", # Legenda de y
           x = tabela2.8$TOTALEXP, # Valores de x
           xlab = "Gasto Total", # Legenda de x
           type = "p", # tipo de grafico - pontos
           main = "Análise de Resíduos - Gastos Total x Resíduos", # Titulo do grafico
           col = "dark red", # Cor dos Plots
           pch=20, # desenho dos "plots"
           cex=1  # tamanho dos "pontos" plotados
           )
      abline(h=0, lty=2)
  ## 1.3.0 Analise Formal ------------------------------------------------------
    # 1.3.1 Teste de Park ------------------------------------------------------
        park = testePark(fit, tabela2.8$TOTALEXP)
        summary(park)
      #
      # Segundo o teste park, se B for est .significativo é indicativo 
      # de heterocedasticidade
      # H0: B = 0 -> Insignificante -> Homocedasticidade.
      # H1: B != 0 -> Significante -> Heterocedasticidade.
      #
      # Sendo p-value(0.0206) < 0.025. A H0 é estatisticamente insignificativa
      # Assim, Pelo t. Park podemos rejeitar a Hipotese de Homocedasticidade
      #
    # 1.3.2 Teste de Glejser ---------------------------------------------------
        glejser = testeGlejser(fit, tabela2.8$TOTALEXP)
        summary(glejser)
      #
      # O teste Glejser segue o mesmo espirito do Park,
      # se B for est. significativo é indicativo de heterocedasticidade
      # H0: B = 0 -> Insignificante -> Homocedasticidade.
      # H1: B != 0 -> Significante -> Heterocedasticidade.
      #
      # Sendo p-value(0.00576) < 0.025. A H0 é estatisticamente significativa
      # Assim, Pelo t. Glejser podemos rejeitar a Hipotese de Homocedasticidade
      #
    # 1.3.3 Teste de White -----------------------------------------------------
        whiteTest = testeWhite(fit,tabela2.8,"TOTALEXP","FOODEXP")
      # Se o valor do Qui-quadado obtido por meio do valor do teste de white for
      # maior que o valor critico do qui-quadrado ao nivel escolhido de signi-
      # - ficancia, a conclusão é de que há heterocedasticidade
        Vcrit = qchisq(0.95,2)  
      # Logo que o valor do qui-quadrado obtido(7.37) >> ao valor critico do
      # qui-quadrado(5.99), existe prova estatistica para concluir que há
      # heterocedasticidade.
        whiteTest > qchisq(0.95,2) 
      # O p-value(0.02504) do teste é 'muito pequeno', corroborando ao resultado acima.
        pvalueWhite <- 1-pchisq(whiteTest,2) 
      #
    # 1.3.4 EP consistente de White --------------------------------------------
        
      coeftest(fit, vcov = vcovHC(fit, type="HC1"))
      # OLS EP = 0.07832
      # White Const EP = 0.074254  
      # Variação = 0.004066
      
      # Com uma variação tão pequena, não valerá a pena corrigir o EP
          
## Questão 2 ###################################################################
  ## 2.1.0 Configurações Iniciais ##############################################
    # 2.1.1 Carregando Base de dados -------------------------------------------
        tabela11.7 = read.table(file = paste(pastaGujarati,"Table 11.7.txt", sep = "/"),header = T, stringsAsFactors = F)
  ## 2.2.0 Analise do Modelo Proposto ##########################################
    # 2.2.1 Regressão ----------------------------------------------------------
        fit2 = lm(formula = MPG ~ SP + HP + WT, data = tabela11.7)
    # 2.2.2 Analisando a regressão --------------------------------------------- 
        summaryFit2 = summary(fit2)
      # Considerando que na Hipotese Nula, os parametros 
      # são estatisticamente insignificantes, isto é, H0: SP = HP = WT = 0.
      # Então, para:
      # - SP temos um p-value(5.72e-07) << 0.025,logo a H0 é est. insignificante
      # - HP temos um p-value(2.19e-06) << 0.025,logo a H0 é est. insignificante 
      # - WT temos um p-value(4.64e-16) << 0.025,logo a H0 é est. insignificante
      #
      # Portanto, pode-se dizer com base nos dados, que os parametros são signi-
      # - ficativos. 
      # 
      # Sentido Economico 
      # SP(Velocidade) -> Quanto maior, maior o gasto -> Negativo - OK
      # HP(Potencia)  -> Quanto maior, menor o gasto -> Positivo - OK
      # WT(PESO) -> Quanto maior, maior o gasto -> Negativo - OK
      #  
  ## 2.3.0 Heterocedasticidade #################################################
    # Em dados de corte transversal envolvendo unidades heterogêneas,
    # a heterocedasticidade pode ser a regra e não a exceção.
    # 2.3.1 Grafico de Resíduos x MPG - Analise Informal -----------------------
      plot(y = (summaryFit2$residuals), # Valores de y
           ylab = "Resíduos", # Legenda de y
           x = tabela11.7$MPG, # Valores de x
           xlab = "MPG", # Legenda de x
           type = "p", # tipo de grafico - pontos
           main = "Análise de Resíduos - MPG x Resíduos", # Titulo do grafico
           col = "blue", # Cor dos Plots
           pch=20, # desenho dos "plots"
           cex=1  # tamanho dos "pontos" plotados
      )
      abline(h=0, lty=2)
    # A analise grafica 'pode' indicar uma linearidade  
    # 2.3.2 Teste de White - Analise Formal ------------------------------------
      # Construindo colunas das variaveis ao cubo.
        tabela11.7$SP2 = tabela11.7$SP^2
        tabela11.7$HP2 = tabela11.7$HP^2
        tabela11.7$WT2 = tabela11.7$WT^2
      # Regressão de White
        WhiteTest2 <- lm(formula = (resid(fit2)^2) ~ SP + HP + WT + 
                           SP2 + HP2 + WT2 + 
                           (SP*HP) + (SP*WT) + (HP*WT),
                            data = tabela11.7
                         )
        NRsquare2 = nrow(tabela11.7)*(summary(WhiteTest2)$r.squared)
      # Se o valor do Qui-quadado obtido por meio do valor do teste de white for
      # maior que o valor critico do qui-quadrado ao nivel escolhido de signi-
      # -ficancia, a conclusão é de que há heterocedasticidade
        Vcrit2 = qchisq(0.95,9) # Logo que existem 9 regressores, o grau de liber-
      # -dade será igual a 9.
      # Logo que o valor do qui-quadrado obtido(37.65) >> ao valor critico do
      # qui-quadrado(16.91), existe prova estatistica para concluir que há
      # heterocedasticidade.
        NRsquare2 > qchisq(0.95,9) 
      # O p-value do teste é muito pequeno, corroborando ao resultado acima.
       pvalueWhite2 <- 1-pchisq(NRsquare2,9) 
  ## 2.4.0 Transformação dos dados #############################################
    # 2.4.1 Busca pela forma funcional da Variancia ----------------------------
         par(mfrow=c(3, 2)) # Gerar os graficos em um mesmo 'painel' 3x2
         plot(tabela11.7$SP, resid(fit2))
         abline(h=0, lty=2)
         plot(tabela11.7$HP, resid(fit2))
         abline(h=0, lty=2)
         plot(tabela11.7$WT, resid(fit2))
         abline(h=0, lty=2)
         plot(tabela11.7$SP, resid(fit2)^2)
         abline(h=0, lty=2)
         plot(tabela11.7$HP, resid(fit2)^2)
         abline(h=0, lty=2)
         plot(tabela11.7$WT, resid(fit2)^2)
       # Não se pode perceber nenhuma forma funcional no graficos.
         par(mfrow=c(1,1)) #Voltando a apresentação normal de dados.
         plot(tabela11.7$SP, resid(fit2)^2)
         abline(h=0, lty=2)
       # Adicionando a coluna res² na tabela.
         res = resid(fit2)^2
         tabela11.7$res = res
         
       # Melhorando a analise com ggplot2
       
         gridExtra::grid.arrange(nrow = 2, ncol = 1,
          ggplot2::ggplot(tabela11.7 , aes(SP , res)) + 
           geom_point() + 
           geom_smooth(method=lm) +
            theme(axis.title.x=element_blank(),
                  axis.text.x=element_blank(),
                  axis.ticks.x=element_blank()),
         
         ggplot2::ggplot(tabela11.7 , aes(SP , res)) + 
           geom_point() + 
           geom_smooth()
         )
    # 2.4.2 Transformação Logaritmica ------------------------------------------
      # Adicionando novas colunas de dados 
        tabela11.7$logMPG = log(tabela11.7$MPG)
        tabela11.7$logSP = log(tabela11.7$SP)
        tabela11.7$logHP = log(tabela11.7$HP)
        tabela11.7$logWT = log(tabela11.7$WT)
      # Nova regressão
        fit2.1 = lm(formula = logMPG ~ logSP + logHP + logWT, data = tabela11.7)
    # 2.4.3 Analisando a regressão --------------------------------------------- 
        summaryFit2.1 = summary(fit2.1)
    # 2.4.4 Teste de White depois da transformação logaritmitca ---------------- 
      # Adicionando novas colunas de dados
        tabela11.7$logres2 = resid(fit2.1)^2 
        tabela11.7$logSP2 = tabela11.7$logSP^2
        tabela11.7$logHP2 = tabela11.7$logHP^2
        tabela11.7$logWT2 = tabela11.7$logWT^2
      # Regressão de white
        WhiteTest2.1 <- lm(formula = logres2 ~ logSP + logHP + logWT + 
                           logSP2 + logHP2 + logWT2 + 
                           (logSP*logHP) + (logSP*logWT) + (logHP*logWT),
                         data = tabela11.7
        )
        NRsquare2.1 = nrow(tabela11.7)*(summary(WhiteTest2.1)$r.squared)
      # Novamente se o valor do Qui-quadado obtido por meio do valor do teste 
      # de white for maior que o valor critico do qui-quadrado ao nivel 
      # escolhido de significancia, a conclusão é de que há heterocedasticidade
        Vcrit2 = qchisq(0.95,9) # Logo que existem 9 regressores, o grau de liber-
      # -dade será igual a 9.
      # Como o valor do qui-quadrado obtido(21.62) >> ao valor critico do
      # qui-quadrado(16.91), existe prova estatistica para concluir que há
      # heterocedasticidade, a transformação Logaritmica não a removeu.
        NRsquare2.1 > qchisq(0.95,9) 
      # O p-value do teste pode ser considerado pequeno (0.01), 
      # corroborando ao resultado acima.
        pvalueWhite2.1 <- 1-pchisq(NRsquare2.1,9)
    # 2.4.5 Considerando outra transformação -----------------------------------
      # Supondo que exista alguma relação de liner entre SP e os residuos.
        # Regressão com valores dividos por SP (O HP talvez tb fosse um cand.)
          fit2.2 = lm(formula = MPG ~ SP + HP + WT, data = tabela11.7, weights = 1/SP)
        # Analisando a regressão
          summaryFit2.2 = summary(fit2.2) 
        # Regressão de White
          resfi2.3 = resid(fit2.2)^2
          WhiteTest2.2 <- lm(formula = resfi2.3 ~ SP + HP + WT + 
                             SP2 + HP2 + WT2 + 
                             (SP*HP) + (SP*WT) + (HP*WT),
                           data = tabela11.7
          )
          NRsquare2.2 = nrow(tabela11.7)*(summary(WhiteTest2.2)$r.squared)
        # Se o valor do Qui-quadado obtido por meio do valor do teste de white for
        # maior que o valor critico do qui-quadrado ao nivel escolhido de signi-
        # -ficancia, a conclusão é de que há heterocedasticidade
         Vcrit2 = qchisq(0.95,9) # Logo que existem 9 regressores, o grau de liber-
        # -dade será igual a 9.
        # Logo que o valor do qui-quadrado obtido(37.64) >> ao valor critico do
        # qui-quadrado(16.91), existe prova estatistica para concluir que há
        # heterocedasticidade - novamente a tranf não surtiu efeito.
          NRsquare2.2 > qchisq(0.95,9) 
        # O p-value do teste é muito pequeno, corroborando ao resultado acima.
          pvalueWhite2.2 <- 1-pchisq(NRsquare2.2,9)
        #
## Questão 3 ###################################################################
  ## 3.1.0 Configurações Iniciais ##############################################
    # 3.1.1 Carregando Base de dados -------------------------------------------
      tabela11.8 = read.table(file = paste(pastaGujarati,"Table 11.8.txt", sep = "/"),header = T, stringsAsFactors = F, skip = 4)    
    # 3.1.2 Tratando dados -----------------------------------------------------
      colnames(tabela11.8) = c("years","medianSalary")
      tabela11.8["medianSalary"] = tabela11.8["medianSalary"]/1000
  ## 3.2.0 Geração e Comparação de Modelos #####################################
    # Gerando Grafico Mediana dos Salários vs. Anos de Experiência
      ggplot2::ggplot(tabela11.8,(aes(years, medianSalary)))+
        geom_point()  + 
        geom_smooth(method=lm)
    # 3.2.1 - Modelo 1 - MedianSalary = B1 + B2*Years + Ui ---------------------
        fit3.1 = lm(formula = medianSalary ~ years, data = tabela11.8)
    # 3.2.2 - Modelo 2 - MedianSalary = B1 + B2*Years + B3*Years² + Ui ---------
        tabela11.8["years2"] = (tabela11.8$years)^2 #Adicionando Coluna de anos² 
        fit3.2 = lm(formula = medianSalary ~ years + years2 , data = tabela11.8)
    # 3.2.3 - Analise e Comparação dos Modelos 1 e 2 ---------------------------
        summaryFit3.1 = summary(fit3.1)
        #
        # Considerando a H0: B2 = 0, dado o p-value(0.000777) < 0.025
        # pode-se considerar a H0 estatisticamente insignificante, logo
        # a quantidade de anos de experiencia possui efeito na mediana salarial
        #
        summaryFit3.2 = summary(fit3.2) 
        #
        # Considerando a H0: B2 = 0, dado o p-value(0.00691) < 0.025
        # pode-se considerar a H0 estatisticamente insignificante, logo
        # a os anos de experiencia possuem efeito na mediana salarial
        #
        # Considerando a H0: B3 = 0, dado o p-value(0.07091) > 0.025
        # pode-se considerar a H0 estatisticamente significante, logo
        # a quantidade de (anos de experiencia)² não possui efeito 
        # na mediana salarial.
        #
        # Você poderia argumentar do porquê da escolha do modelo 02 em 
        # detrimento do modelo 01? Não, o modelo dois possui uma variavel 
        # sem significancia estatistica para um nivel de 95% de confiabilidade
        #
        # > Comparação de modelos.
        #
  ## 3.3.0 Teste de Heterocedasticidade ########################################
    # 3.3.1 Existencia de Heterocedasticidade no Modelo 01 ---------------------
      # Analise Grafica Informal
        plot(y = (summaryFit3.1$residuals^2), # Valores de X
             ylab = "Resíduos²", # Legenda de X
             x = tabela11.8$medianSalary, # Valores de Y
             xlab = "Mediana do Salario", # Legenda de Y
             type = "p", # tipo de grafico - pontos
             main = "Análise de Resíduos - Mediana do Salario x Resíduos²", # Titulo do grafico
             col = "Red", # Cor dos Plots
             pch=20, # desenho dos "plots"
             cex=1  # tamanho dos "pontos" plotados
        )
      # Analise grafica não indica heterocedasticidade
      #
      # Analise Formal - Teste Park
        park3.1 = testePark(fit3.1, tabela11.8$medianSalary)
        summary(park3.1)
        # Considerando H0: B = 0, temos um p-value(0.669) > 0.025, a hipotese
        # nula então é estatisticamente significativa, a hipotese de homocedasti
        # cidade pode ser mantida.
      # Analise Formal - Teste White
        testeWhite3 = testeWhite(fit3.1,tabela11.8,"years","medianSalary")
        # Se o valor do Qui-quadado obtido por meio do valor do teste de white for
        # maior que o valor critico do qui-quadrado ao nivel escolhido de signi-
        # -ficancia, a conclusão é de que há heterocedasticidade
        Vcrit3 = qchisq(0.95,2) # Logo que existem 9 regressores, o grau de liber-
        # -dade será igual a 9.
        # Logo que o valor do qui-quadrado obtido(11.41) > ao valor critico do
        # qui-quadrado(5.99), existe prova estatistica para concluir que há
        # heterocedasticidade.
        testeWhite3 > qchisq(0.95,2)
    # 3.3.2 Existencia de Heterocedasticidade no Modelo 02 ---------------------
      # Analise Grafica Informal
        plot(y = (summaryFit3.2$residuals^2), # Valores de X
             ylab = "Resíduos²", # Legenda de X
             x = tabela11.8$medianSalary, # Valores de Y
             xlab = "Mediana do Salario", # Legenda de Y
             type = "p", # tipo de grafico - pontos
             main = "Análise de Resíduos 2 - Mediana do Salario x Resíduos²", # Titulo do grafico
             col = "green", # Cor dos Plots
             pch=20, # desenho dos "plots"
             cex=1  # tamanho dos "pontos" plotados
        )
      # Analise grafica não indica heterocedasticidade
      #
      # Analise Formal
        park3.2 = testePark(fit3.2, tabela11.8$medianSalary)
        summary(park3.2)
        # Considerando H0: B = 0, temos um p-value(0.531) > 0.025, a hipotese
        # nula então é estatisticamente significativa, a hipotese de homocedasti
        # cidade pode ser mantida. (B é estatisticamente insignificativo)
        #
        # Analise Formal - Teste White
          WhiteTest3.2 <- lm(formula = (resid(fit3.2)^2) ~ years + years2+ I(years^2)+I(years2^2)+I(years*years2),
                             data = tabela11.8
          )
          NRsquare3.2 = nrow(tabela11.8)*(summary(WhiteTest3.2)$r.squared)
        # Se o valor do Qui-quadado obtido por meio do valor do teste de white for
        # maior que o valor critico do qui-quadrado ao nivel escolhido de signi-
        # -ficancia, a conclusão é de que há heterocedasticidade
          Vcrit3.2 = qchisq(0.95,5) # Logo que existem 5 regressores, o grau de liber-
        # -dade será igual a 5.
        # Logo que o valor do qui-quadrado obtido(8.51) < ao valor critico do
        # qui-quadrado(11.07), existe prova estatistica para concluir que 'não' 
        # há heterocedasticidade.
          NRsquare3.2 > qchisq(0.95,5) #(FALSE)
## Questão 4 ###################################################################
  ## 4.1.0 Sabendo que lambda é igual (SQR2/gl)/(SQR1/gl)
    # E que os seguintes valores são dados:
      SQR1 = 55
      SQR2 = 140
      gl = 25
    # Calculando Lambda
      lambda = SQR2 / SQR1  # Os gl se cortam - por isso foram omitidos 
    # Valor Critico de F 
      qf(.95, df1=25, df2=25)
    #
    # Sendo valor de lambda(2.5454) > F(1.9554), podemos rejeitar a hipotese de 
    # homocedasticidade, é dizer que a heterocedasticidade é muito provavel.
## Questão 5 ###################################################################
  ## 5.1.0 Configurações Iniciais ##############################################
    # 5.1.1 Carregando Base de dados -------------------------------------------
      tabela11.9 = read.table(file = paste(pastaGujarati,"Table 11.9.9.txt", sep = "/"),header = T, stringsAsFactors = F)          
      # Para leitura correta do arquivo foi necessario juntar o nome "New Zeland" 
        
        
        
        
        
        
        
        
        
        
      
#<============================================================================>#                                                                            #   
#     Autor:Robson Waite                                                       #
#     Data:05/11/2019                                                          #
#     Titulo: Lista 3 - Econometria II                                         #
#     Descrição: Exercicios                                                    #           
#<============================================================================>#                                                                            #   
## Configurações Gerais - Diretorio - Pasta de dados do Gujarati ###############
  pastaGujarati = "./inputs/DadosGujarati"
## Questão 1 ###################################################################
  ## 1.1.0 Configurações Iniciais ##############################################
    # 1.1.1 Carregando testes --------------------------------------------------
      source("./src/Park.R")    # teste de Park - função
      source("./src/Glejser.R") # teste de Glejser - função
    # 1.1.2 Carregando Base de dados ###########################################
      # Carregando Dados
        tabela2.8 = read.table(file = paste(pastaGujarati,"Table 2.8.txt", sep = "/"),header = T, stringsAsFactors = F)
  ## 1.2.0 Analise - Total Gasto x Total Gasto com Alimentação  ################
    # 1.2.1 Regressão ----------------------------------------------------------
      fit = lm(formula = TOTALEXP ~ FOODEXP, data = tabela2.8)
    # 1.2.2 Sumario ------------------------------------------------------------
      summaryFit = summary(fit)
    # 1.2.3 Grafico residuos x total gasto - Analise Informal ------------------
      plot(y = (summaryFit$residuals^2), # Valores de X
           ylab = "Resíduos²", # Legenda de X
           x = tabela2.8$TOTALEXP, # Valores de Y
           xlab = "Gasto Total", # Legenda de Y
           type = "p", # tipo de grafico - pontos
           main = "Análise de Resíduos - Gastos Total x Resíduos²", # Titulo do grafico
           col = "dark red", # Cor dos Plots
           pch=20, # desenho dos "plots"
           cex=1  # tamanho dos "pontos" plotados
           )
  ## 1.3.0 Analise Formal ------------------------------------------------------
    ## 1.3.1 Teste de Park -----------------------------------------------------
        park = testePark(fit, tabela2.8$TOTALEXP)
        summary(park)
        #
        # Segundo o teste park, se B for est .significativo é indicativo de heterocedasticidade
        # H0: B = 0 -> Insignificante -> Homocedasticidade.
        # H1: B != 0 -> Significante -> Heterocedasticidade.
        #
        # Sendo p-value(0.1871) > 0.025. A H0 é estatisticamente significativa
        # Assim, Pelo teste Park podemos aceitar a Hipotese de Homocedasticidade.
        #
    ## 1.3.2 Teste de Glejser --------------------------------------------------
        glejser = testeGlejser(fit, tabela2.8$TOTALEXP)
        summary(glejser)
        #
        # O teste Glejser segue o mesmo espirito do Park,
        # se B for est. significativo é indicativo de heterocedasticidade
        # H0: B = 0 -> Insignificante -> Homocedasticidade.
        # H1: B != 0 -> Significante -> Heterocedasticidade.
        #
        # Sendo p-value(0.3223) > 0.025. A H0 é estatisticamente significativa
        # Assim, Pelo teste Glejser podemos aceitar a Hipotese de Homocedasticidade.
        #
    ## 1.3.3 Teste de White ----------------------------------------------------
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
      # Portanto, pode-se dizer com base nos dados, que existe sentido economico
      # nas variaveis acima, e na regressão como um todo - 
      # p-value(2.2e-16) << 0.025.
  ## 2.3.0 Heterocedasticidade #################################################
    # Em dados de corte transversal envolvendo unidades heterogêneas,
    # a heterocedasticidade pode ser a regra e não a exceção.
    # 2.3.1 Grafico de Resíduos x MPG - Analise Informal -----------------------
      plot(y = (summaryFit2$residuals^2), # Valores de X
           ylab = "Resíduos²", # Legenda de X
           x = tabela11.7$MPG, # Valores de Y
           xlab = "MPG", # Legenda de Y
           type = "p", # tipo de grafico - pontos
           main = "Análise de Resíduos - MPG x Resíduos²", # Titulo do grafico
           col = "blue", # Cor dos Plots
           pch=20, # desenho dos "plots"
           cex=1  # tamanho dos "pontos" plotados
      )
      # A analise grafica 'pode' indicar uma linearidade  
    # 2.3.2 Teste de White - Analise Formal ------------------------------------
  ## 2.4.0 Transformação dos dados #############################################
## Questão 3 ###################################################################
  ## 3.1.0 Configurações Iniciais ##############################################
    # 3.1.1 Carregando Base de dados -------------------------------------------
      tabela11.8 = read.table(file = paste(pastaGujarati,"Table 11.8.txt", sep = "/"),header = T, stringsAsFactors = F, skip = 4)    
    # 3.1.2 Tratando dados -----------------------------------------------------
      colnames(tabela11.8) = c("years","medianSalary")
      tabela11.8["medianSalary"] = tabela11.8["medianSalary"]/1000
  ## 3.2.0 Geração e Comparação de Modelos #####################################
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
      # Analise Formal
        park3.1 = testePark(fit3.1, tabela11.8$medianSalary)
        summary(park3.1)
        # Considerando H0: B = 0, temos um p-value(0.669) > 0.025, a hipotese
        # nula então é estatisticamente significativa, a hipotese de homocedasti
        # cidade pode ser mantida.
        #
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
## Questão 4 ###################################################################
  ## 4.1.0       
## Questão 5 ###################################################################
  ## 5.1.0 Configurações Iniciais ##############################################
    # 5.1.1 Carregando Base de dados -------------------------------------------
      tabela11.9 = read.table(file = paste(pastaGujarati,"Table 11.9.9.txt", sep = "/"),header = T, stringsAsFactors = F)          
      # Para leitura correta do arquivo foi necessario juntar o nome "New Zeland" 
        
        
        
        
        
        
        
        
        
        
      
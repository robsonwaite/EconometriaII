#<============================================================================>#                                                                            #   
#     Autor:Robson Waite                                                       #
#     Data:11/10/2019                                                          #
#     Titulo:                                                                  #
#     Descrição:                                                               #           
#<============================================================================>#                                                                            #   
# 0.0 Configurações Iniciais ###################################################

# 1.0 Pacotes ##################################################################
## 1.1 Instalando Pacotes 

# if(!"" %in% installed.packages()[,1]) install.packages("")

## 1.2 Carregando Pacotes



## 1.3 Carregando Funções

source("./src/Primeiro_dia_util_do_mes_entre.R")

## 2.0 Carregando Base de dados ################################################

ipca = read.csv2(file = "./Outros/VarIPCA.csv", encoding = "UTF-8", stringsAsFactors = F)

IBOV = read.csv2(file = "./Outros/IBOV.csv", encoding = "UTF-8", sep = ",", stringsAsFactors = F)

## 3.0 Tratamento dos dados ####################################################

# Indice bovespa 2008-2018 

IBOV = IBOV[,(1:2)] #Removendo dados desnecessarios 
colnames(IBOV) = c("Data", "Fechamento") #Renomeando colunas
IBOV$Data = as.Date(IBOV$Data, "%d.%m.%Y") # convertendo string em data
IBOV$Fechamento = gsub("\\.","",IBOV$Fechamento) #removendo ponto
IBOV$Fechamento = gsub(",",".",IBOV$Fechamento) #trocando a virgula por ponto - padrão R
IBOV$Fechamento = as.numeric(IBOV$Fechamento) #convertendo string em numero
datas = P_dia_util_do_mes_entre(2008, 2018) # vetor de datas do primeiro dia util 
IBOV_Corte = merge(datas, IBOV, by = "Data") #Corte de dados - fechamento do IBOV no primeiro dia util de cada mes
IBOV_Corte$Data = format(as.Date(IBOV_Corte$Data), "%Y-%m") #Removendo 'dia' da data

# IPCA MENSAL 2008-2018

ipca$Data = as.Date(ipca$Data, "%d/%m/%Y") #convertendo string para data
colnames(ipca) = c("Data", "IPCA") #Renomeando colunas 
ipca$Data = format(as.Date(ipca$Data), "%Y-%m") #Removendo 'dia' da data
ipca$IPCA = as.numeric(ipca$IPCA)


# Consolidação - Dados 

Dados_Consolidados = merge(IBOV_Corte, ipca, by = "Data") # Consolidando dados pelo mes/ano

## 4.0 Regressão - Ibovespa x IPCA #############################################

fit = lm(Fechamento ~ IPCA, data = Dados_Consolidados)
summary(fit)

plot(Dados_Consolidados$IPCA, rstandard(fit))



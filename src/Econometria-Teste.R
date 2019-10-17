#<============================================================================>#                                                                            #   
#     Autor:Robson Waite                                                       #
#     Data:11/10/2019                                                          #
#     Titulo:                                                                  #
#     Descrição:                                                               #           
#<============================================================================>#                                                                            #   
# 0.0 Configurações Iniciais ###################################################

# 1.0 Pacotes ##################################################################
## 1.1 Instalando Pacotes 

if(!"timeDate" %in% installed.packages()[,1]) install.packages("timeDate")

## 1.2 Carregando Pacotes

library("timeDate")

## 1.3 Carregando Funções

source("./src/Primeiro_dia_util_do_mes_entre.R")

## 2.0 Carregando Base de dados ################################################

ipca = read.csv2(file = "./Outros/VarIPCA.csv", encoding = "UTF-8")

IBOV = read.csv2(file = "./Outros/IBOV.csv", encoding = "UTF-8", sep = ",", stringsAsFactors = F)

## 3.0 Tratamento dos dados ####################################################

# Indice bovespa
IBOV$DATA = IBOV$X.U.FEFF.Data #Alterando Nome da Coluna
IBOV = IBOV[,-1] #Removendo duplicata 
IBOV = IBOV[,-(2:6)] #Removendo dados desnecessarios 
IBOV$DATA = as.Date(IBOV$DATA, "%d.%m.%Y") # convertendo string em data

IBOV$Último = gsub("\\.","",IBOV$Último) #removendo ponto
IBOV$Último = gsub(",",".",IBOV$Último) #trocando a virgula por ponto - padrão R
IBOV$Último = as.numeric(IBOV$Último) #convertendo string em numero

datas = P_dia_util_do_mes_entre(2008, 2018)

# subset(expr, cell_type %in% c("bj fibroblast", "hesc"))

IBOV_Corte = subset(IBOV, DATA %in% datas)





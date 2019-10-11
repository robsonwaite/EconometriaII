#<============================================================================>#                                                                            #   
#     Autor:Robson Waite                                                       #
#     Data:11/10/2019                                                          #
#     Titulo: Convertendo dados                                                #
#     Descrição:                                                               #           
#<============================================================================>#                                                                            #   
# 0.0 Configurações Iniciais ###################################################

# 1.0 Pacotes ##################################################################
## 1.1 Instalando Pacotes 

if(!"" %in% installed.packages()[,1]) install.packages("")

## 1.2 Carregando Pacotes 

## Carregando Base de dados

cotahist = read.table(file = "./Outros/COTAHIST_M092019.TXT", header = T, sep = "",row.names = F)

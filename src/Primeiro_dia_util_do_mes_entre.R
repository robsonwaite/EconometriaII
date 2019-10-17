#<============================================================================>#                                                                               
#     Autor:Robson Waite                                                       #
#     Data:16/10/2019                                                          #
#     Titulo: Primeiro Dia Util                                                #
#     Descrição: Função que informa o primeiro dia util de cada mes do ano     #
#     dentro de um intervalo dado de anos                                      #
#<============================================================================>#                                                                               
# 0.0 Função ##################################################################
P_dia_util_do_mes_entre = function(inicio, fim){
  library(bizdays)
  create.calendar("Brazil/ANBIMA", holidaysANBIMA, weekdays=c("saturday", "sunday"))
  bizdays.options$set(default.calendar="Brazil/ANBIMA")
  dias = c()
  for (Y in inicio:fim){
    for(m in 1:12){
      for(d in 1:10){
        dia = as.Date(paste(d,m,Y,sep = "."),"%d.%m.%Y")
        if(bizdays::is.bizday(dia)){
          dias = append(dias,dia)
          break
        }
      }
    }
  }
  return(dias)
}
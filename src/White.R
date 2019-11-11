## Teste White
## ver 0.0

testeWhite = function(dados,x,y){
WhiteTest <- lm(dados[,y] ~ dados[,x] + ((dados[,x])^2))
NRsquare = nrow(dados)*(summary(WhiteTest)$r.squared)
return(NRsquare)
}





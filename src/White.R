## Teste White
## ver 0.0

testeWhite = function(fit,dados,x,y){
WhiteTest <- lm((resid(fit)^2) ~ dados[,x] + I((dados[,x])^2))
NRsquare = nrow(dados)*(summary(WhiteTest)$r.squared)
return(NRsquare)
}




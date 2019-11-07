## Teste de Glejser
## ver 0.0

testeGlejser = function(fit,varY){
  glejser <- lm(abs(residuals(fit)) ~ varY)
  return(glejser)
}

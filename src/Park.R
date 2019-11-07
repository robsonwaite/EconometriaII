## Teste de Park
## ver 0.0

testePark = function(fit,varY) {
park = lm(log((residuals(fit)^2)) ~ log(varY)) 
return(park)
}

## Teste Goldfeld-Quandt
## V 0.0

testeGoldFedlQuandtk2 = function(c,dados,X,Y){

GQTest <- dados[-(((nrow(dados) - c)/2 + 1):(((nrow(dados) - c)/2) + 4)),]
if((nrow(dados) %% 2) == 0){
  GQTest1 = GQTest[1:(nrow(GQTest)/2),]
  GQTest2 = GQTest[(nrow(GQTest1)+1):nrow(GQTest),]
} else {
  GQTest1 = GQTest[1:((nrow(GQTest)+1)/2),]
  GQTest2 = GQTest[nrow(GQTest1)+1:nrow(GQTest),]
}
Reg01 <- lm(GQTest1[,Y] ~ GQTest1[,X])
summary(Reg01)
Reg02 <- lm(GQTest2[,Y] ~ GQTest2[,X])
summary(Reg02)

## Lambda
lambda = (sum(residuals(Reg02)^2)/(nrow(GQTest2)-2))/(sum(residuals(Reg01)^2)/(nrow(GQTest1)-2))
return(lambda)

}

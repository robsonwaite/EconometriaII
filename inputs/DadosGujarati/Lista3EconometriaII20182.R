## White test Exercício 1 Lista 3

Table28 <- read.table("Table 2.8.txt", header = T, sep = "", na.strings = "EMPTY",
                      strip.white = T)[,-1]

fitIndia <- lm(FOODEXP ~ TOTALEXP, data = Table28)
res2fIndia <- residuals(fitIndia)^2

DataWhite <- data.frame(Table28,res2fIndia,(Table28[,2])^2)
colnames(DataWhite) <- c(colnames(Table28),"Residuos","TOTALEXP2")
WhiteTestIndia <- lm(Residuos ~ TOTALEXP + TOTALEXP2, data = DataWhite)

## White test Exercício 2 Lista 3

Table117 <- read.table("Table 11.7.txt", header = T, sep = "", na.strings = "EMPTY",
                       strip.white = T)

fitCars <- lm(MPG ~ SP + HP + WT, data = Table117)
res2fCars <- residuals(fitCars)^2

DataWhite <- data.frame(Table117,res2fCars,Table117^2,Table117[,1]*Table117[,5],Table117[,1]*Table117[,6],Table117[,5]*Table117[,6])
colnames(DataWhite) <- c(colnames(DataWhite[1:6]),"Residuos",colnames(DataWhite[8:13]),"HPWT","HPSP","WTSP")
WhiteTestCars <- lm(Residuos ~ SP + HP + WT, data = DataWhite)

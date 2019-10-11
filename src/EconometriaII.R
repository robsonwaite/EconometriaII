##### Econometria II #####
setwd("D:\\R-codes\\EconometriaII\\DadosGujarati\\DadosGujarati")
#### Leitura dos dados (Gujarati, Cap 11)
## Tabela 11.1
ProdComp = as.matrix(read.table("Table 11.1.txt", header = T, sep = "", na.strings = "EMPTY", strip.white = T, row.names = 1))

AvProdtivity <- ProdComp[13,]
Sd <- ProdComp[12,]
AvCompens <- ProdComp[11,]
ProdComp <- ProdComp[-(11:13),]

ProdCompFinal <- matrix(NA, ncol = ncol(ProdComp), nrow = 2)
for(i in 1:ncol(ProdComp)){
  ProdCompFinal[1,i] <- mean(ProdComp[,i])
  ProdCompFinal[2,i] <- sd(ProdComp[,i])
}
colnames(ProdCompFinal) <- colnames(ProdComp)

### Regressao Produtividade x Salario
options(scipen = 999) #desabilita notação cientifica 
fit = lm(AvCompens ~ AvProdtivity)
summary(fit)

plot(AvProdtivity, rstandard(fit))

qt(0.975) # t valor que limita a região de aceitação, > que ele ñ aceita. 


## Park Test

res2 <- log(residuals(fit)^2)
lnAvP <- log(AvProdtivity)

Park <- lm(res2 ~  lnAvP)
summary(Park)

## Glejser Test

absres <- abs(residuals(fit))

Glejser <- lm(absres ~ AvProdtivity)
summary(Glejser)

## Spearman's rank correlation test
RankCor = read.table("Table 11.2.txt", header = F, sep = "", na.strings = "EMPTY", strip.white = T, dec = ".")
RankCor <- matrix(as.numeric(unlist(RankCor[,-1])), ncol = 8, byrow = F)

## Spearman coefficient
SpCoef <- cor(RankCor[,5:6], method = "spearman")[2] ## cor e uma matriz de correlacao, 2 e o elemento a12 ou a21 

# ttest
ttestSpCor <- function(x){
  n <- nrow(x);
  y <- cor(x, method = "spearman")[2];
  t <- y*sqrt(n-2)/sqrt(1-(y^2));
  t
} ## Aqui basta prover a matrix com os ranks

ttest<- ttest(RankCor[,5:6]) ## Como gl = 8, t nao e signitivativo nem a 10%
pvalue <- 1 - pt(ttest, nrow(RankCor) - 2)

## Goldfeld-Quandt Test
Table113 <- read.table("Table 11.3.txt", header = T, sep = "", na.strings = "EMPTY", strip.white = T, dec = ".")

# Seguindo a recomendacao, sugerimos c = 4, ou seja, removeremos as 4 observacoes centrais de X e Y.
# Na tabela, as colunas 3 e 4 sao as colunas 1 e 2 ordenadas do menor para o maior, como manda o teste.
c <- 4
GQTest <- Table113[-(((nrow(Table113) - c)/2 + 1):(((nrow(Table113) - c)/2) + 4)),]
GQTest1 <- GQTest[1:13,]
GQTest2 <- GQTest[14:26,]

FReg <- lm(GQTest1[,3] ~ GQTest1[,4])
summary(FReg)

SReg <- lm(GQTest2[,3] ~ GQTest2[,4])
summary(SReg)

## Lambda

lambdaNum <- sum(residuals(SReg)^2)/(nrow(GQTest2)-2) 
lambdaDen <- sum(residuals(FReg)^2)/(nrow(GQTest1)-2)
lambda <- lambdaNum/lambdaDen ## Aqui estamos falando de uma F 11,11. A 5%, podemos fazer
pvalueF <- 1 - pf(lambda,11,11) ## com esse valor rejeita-se a hipotese de homocedasticidade (mas nao a 1%)

### Breusch-Pagan-Godfrey Test
BPGY <- Table113[,1]
BPGX <- Table113[,2]

# step 1
BPGReg <- lm(BPGY ~ BPGX)
# step 2
sigmatilde <- sum(residuals(BPGReg)^2)/nrow(Table113)
# step 3
BPGp <- residuals(BPGReg)^2/sigmatilde
# step 4 - Zi será descrito como Xi no exemplo
BPGpReg <- lm(BPGp ~ BPGX)
# step 5 - SQT = SQR + SQE
BPGpSQT <- sum((BPGp - mean(BPGp))^2)
BPGpSQE <- sum(residuals(BPGpReg)^2)
BigThetaBPGp <- (BPGpSQT - BPGpSQE)/2 ## Aqui estamoa com uma dist chi-squared com 1 grau de liberdade. A 5%, podemos fazer
criticalChiBGPp <- pchisq(BigThetaBPGp,1) ## o valor associado a 5% na chi-squared 1 é 3.841459 e a 1% 6.634897
# O detalhe do teste é que estamos falando de uma teste assintótico. Logo, é bom ter cuidado com pequenas amostras, como é o caso.
# As conclusões são similares ao teste GQ.

### White Test

# Exemplo 11.6 Gujarati. Não há dados disponíveis.

### Weighted Least Squares (Table 11.1)
## Tabela 11.1
ProdComp = as.matrix(read.table("Table 11.1.txt", header = T, sep = "", na.strings = "EMPTY",
                                strip.white = T, row.names = 1))
AvProdtivity <- ProdComp[13, ]
Sd <- ProdComp[12, ]
AvCompens <- ProdComp[11, ]
ProdComp <- ProdComp[-(11:13), ]
EmplSize <- seq(1,9,1)

WLSReg <- lm(AvCompens ~ 0 + rep(1,9) + EmplSize, weights = 1/Sd) ## 0 retira o intercepto do modelo, como é o caso aqui.
summary(WLSReg)

## Alternativa ao método anterior
Table114 <- read.table("Table 11.4.txt", header = T, sep = "", na.strings = "EMPTY",
                       strip.white = T)
Sdt114 <- 1/Table114[,3]
YWLS <- Table114[,4]
XWLS <- Table114[,5]

WLSReg1 <- lm(YWLS ~ 0 + Sdt114 + XWLS)
summary(WLSReg1)

### R&D Expenditure, Sales and Profits in 18 industry groupings in the US, 1988 (Gujarati, pg 423, 4th edition)
Table115 <- read.table("Table 11.5.txt", header = T, sep = "", na.strings = "EMPTY",
                       strip.white = T)
RD <- Table115[,2]
Sales <- Table115[,1]

RegSales <- lm(RD ~ Sales)
summary(RegSales)

## Park Test
resSales <- residuals(RegSales)^2
lnSales <- log(Sales)

ParkTestSales <- lm(resSales ~ Sales)
summary(ParkTestSales) ## Veja que existe uma relação positiva e estatisticamente significativa (a 5%) entre o quadrado dos resíduos e Sales.

## Glejser Test
resSalesabs <- abs(residuals(RegSales))

GlejserTestSales <- lm(resSalesabs ~ Sales)
summary(GlejserTestSales)  ### observe que, pelo teste de Glejser, ainda há algum indício de heterocedasticidade, apesar da estatística
                        ## de teste estar um pouco acima de 5%.

## White Test
# Aqui usaremos tanto Sales como Sales^2 na regressão auxiliar no quadrado dos resíduos.

Salesaux <- Sales^2

WhiteTestSales <- lm(resSales ~ Sales + Salesaux)
summary(WhiteTestSales)

nR2Sales <- nrow(Table115)*(summary(WhiteTestSales)$r.squared) ## Como são 2 regressores na regressão auxiliar, g.l = 2
chiWhite <- qchisq(0.95,2)
nR2Sales > chiWhite # Se falso, rejeita-se heterocedasticidade. Mas é bom olhar para o p-valor associado a nR^2
pvalueWhite <- 1-pchisq(nR2Sales,2) # O valor é um pouco maior do que 7%. Você pode achá-lo suficientemente pequeno para rejeitar
                                    # a hipótese de homocedasticidade


# Gráfico de resíduos

par(mfrow = c(2,1))
plot(Sales,residuals(RegSales), xlab = "Sales", ylab = "Resíduos") ## Resíduos = hat(u)#
abline(0,0)

plot(Sales, resSales, xlab = "Sales", ylab = expression(hat(u)^2)) ## Note que parece haver uma relação linear
                                                                      ## entre hat(u)^2 e Sales

# Correção de heterocedasticidade
RDcor <- RD/sqrt(Sales)  ### A correção de heterocedasticidade se dá por sqrt(Sales). 
Salescor <- sqrt(Sales)
weigSales <- 1/sqrt(Sales)
NewRegressor <- rep(1, nrow(Table115))/sqrt(Sales)

HetCorSales <- lm(RDcor ~ 0 + NewRegressor + Salescor)
summary(HetCorSales) ## Diferença mais significativa se dá no e.p.

## Procedimento de White (correção da matriz de covariância/erros-padrão robustos)
library(lmtest)
library(sandwich)

coeftest(RegSales, vcov = vcovHC(RegSales, type="HC1")) ## Perceba a mudança dos erros-padrão e t test usando o procedimento de White
                                                        ## A limitação que temos aqui é o tamanho amostral (n=18). Usa-se
                                                        ## o procedimento, em geral, para grandes amostras.



### Exercícios sobre Autocorrelação ###

# Experimento de Monte Carlo

# Termos de erro autorrelacionados
## Equação: u_t = 0.7u_{t-1} + v_t e v_t ~ N(0,1)

MCAutcRes <- function(n,u0,rho){
  vt <- rnorm(n);
  VecAux <- vector("numeric",n+1);
  for(i in 2:(n+1)){
    VecAux[1] = u0;
    VecAux[i] <- rho*VecAux[i-1] + vt[i-1]
  }
  ResAutF <- VecAux[2:(n+1)];
  ResAutF; 
}

## Considerando que o vetor de X = {1,2,...,10}, teríamos a seguinte regressão

n = 10
rho = 0.7
u0 = 5
ResU <- MCAutcRes(n,u0,rho)

beta1 = 1
beta2 = 0.8

Xvec <- seq(1,n,1) ## Aqui é o vetor X
Yvec <- vector("numeric", length = n)
for(i in 1:n){
  Yvec[i] <- 1 + 0.8*Xvec[i] + ResU[i]
}
AutReg <- lm(Yvec ~ Xvec)
summary(AutReg)
Yest <- AutReg$coefficients[1] + AutReg$coefficients[2]*Xvec

# Comparando Y_hat com Y_real

Yr <- beta1 + beta2*Xvec

plot(Xvec, Yvec, pch = 20, ylim =c(0,10))
abline(AutReg, col = "green")
lines(Xvec, Yr, col = "red", type = "l", ylim = c(0,10))

##### O experimento acima dá uma noção do quanto "erramos" ao usar OLS na presença autocorrelação dos termos de erro; Veja
#### que a linha verde é bastante diferente da linha vermelha, aqui entendida como a verdadeira regressão da população.
#### Algumas coisas são facilmente perceptiveis: 1) no modelo "verde" o intercepto é superestimado, enquanto o coeficiente
#### angular é subestimado. 2) Além disso, a variância dos termos de erro é subestimada na regressão "verde" (basta notar
#### a distância dos pontos da linha verde, a regressão de mínimos quadrados, e da linha vermelha, a regressão verdadeira.

## Exemplo prático

Table124 <- read.table("Table 12.4.txt", header = T, sep = "", na.strings = "EMPTY", strip.white = T)
library(lmtest)
# Gráfico de salário por hora (Y) vs produção por hora - ou produtividade (X)
with(Table124, plot(X,Y))
## Pelo gráfico é possível ver que a relação entre salário e produtividade é praticamente linear, com o final da amostra
## sendo ligeiramente não-linear. Rodaremos tanto a forma funcional linear como a loglinear.

FFlinear <- lm(Y ~ X, data = Table124)
summary(FFlinear)
dwtest(Y ~ X, data = Table124)

FFloglinear<- lm(log(Y) ~ log(X), data = Table124)
summary(FFloglinear)
dwtest(log(Y) ~ log(X), data = Table124)

## Detectando autocorrelação pelo gráfico de resíduos (e também resíduos padronizados) - modelo linear

Anos <- 1959:1998
dataTable124 <- data.frame(Anos,residuals(FFlinear),rstandard(FFlinear))
colnames(dataTable124) <- c("Anos", "Residuos", "Res.Padronizado")

# Gráficos de resíduo e resíduo padronizado

par(mar = c(5,5,2,5))
with(dataTable124, plot(Anos, Residuos, ylim = c(-6,4), ylab = "Residuos", type = "l"))
abline(0,0)
par(new = T)
with(dataTable124, plot(Anos, Res.Padronizado, axes = F, ylab = NA, xlab = NA, type = "l", lty = "dashed", ylim = c(-2,2)))
axis(side = 4)
mtext(side = 4, line = 3, "Res Padronizado")
### Com o gráfico é possível perceber um formato específico para os resíduos (U invertido). Isso sugere possível autocorrelação
### dos termos de erro. Outra possibilidade é avaliarmos o gráfico de u_hat_t com u_hat_(t-1).

# Gráfico de u_hat_t vs u_hat_(t-1)

u.hat.t <- residuals(FFlinear)[-1]
u.hat.t1 <- residuals(FFlinear)[-length(residuals(FFlinear))]

plot(u.hat.t1, u.hat.t, xlim = c(-6,4), ylim = c(-6,4))
abline(h = 0) ## h para horizontal
abline(v = 0) ## v para vertical
text(-4,3,labels = "I", font = 2) # font = 2 significa negrito
text(1.5,3,labels = "II", font = 2)
text(1.5,-1,labels = "III", font = 2)
text(-4,-1,labels = "IV", font = 2)
## a divisão em quadrantes ajuda a perceber que os pontos estão concentrados nos quadrantes II e III, o que sugere que há
## forte correlação entre os resíduos em t e os resíduos em t-1

## Runs test
# A proposta desse teste é usar a sequência de sinais positivos ou negativos dos resíduos como indicação de autocorrelação.
# Em tese, se há poucas mudanças de sinal maior a chance de termos autocorrelação positiva. Se as mudanças de sinal forem
# muitas, há chance de autocorrelação negativa. Criaremos um intervalo de confiança para o número de mudanças de sinal.
# Se R = número de mudanças de sinal estiver no IC, temos indícios de não haver autocorrelação serial.

install.packages("tseries") ## responda "n" quando surgir uma pergunta.
library(tseries)

resR <- as.vector(residuals(FFlinear))
sinaisR <- sign(resR)
R <- length(rle(sinaisR)$lengths)

RunsR <- rle(sinaisR)
N <- length(sinaisR)
N1 <- sum(RunsR$lengths[RunsR$value==1])
N2 <- sum(RunsR$lengths[RunsR$value==-1])
ER <- 2*N1*N2/N + 1
VR <- (4*(N1^2)*(N2^2) - 2*N1*N2*N)/((N^2)*(N-1))
sdR <- sqrt(VR)

### IC para R será ER +- 1.96*(sdR); se R estiver no IC, rejeitamos Autocorrelação. Neste caso, R não pertence ao IC. Logo
# estamos diante de autocorrelação, usando o Runs Test. 

## Teste de Durbin-Watson

# O teste de DW é usado para detectar autocorrelação dos termos de erro. Pode ser definido como 
                      # d = sum(u_hat_t - u_hat_t-1)^2/sum(u_hat_t)^2
# A estatística d é usada para testar se rho = 0. Note que rho é o coeficiente que "atesta" a presença de erros autocorrelacionados.

### Pontos importantes:
## (1) Um dos aspectos mais importantes da validade de DW é que no modelo não pode haver, como variável explicativa, a própria
# variável dependente defasada. 
## (2) Os termos de erro são gerados por um esquema autorregressivo de primeira ordem (AR(1)). DW não pode ser usado para
# detectar estruturas de correlação mais complexas.
## (3) u_t ~ N(.,.).

### Fazendo algumas contas, pode-se chegar à relação d ~ 2*(1-rho_hat). Como -1 <= rho <= 1, podemos dizer que o
# valor de d se encontra no intervalo [0,4], ou seja, 0 <= d <= 4. Se rho_hat = 0, d = 2. Logo, se não há autocorrelação
# dos termos de erro, esperamos que d seja aproximadamente 2. 
# Se rho se aproxima de 1, d está perto de 0. Logo, há indicação de autocorrelação positiva se d estiver próximo de zero. 
# Por outro lado, se rho se aproxima de -1, d está perto de 4. Teremos indicação de autocorrelação negativa se d estiver
# próximo de 4. 

## Calcularemos a estatística d e depois avaliaremos se ela obedece as seguintes possibilidades;
    # (i) d < d_L, rejeita-se H_0 (H_0: rho = 0 vs. H_1: rho > 0); d_L é limite inferior do valor crítico.
    # (ii) d_L < d < d_U, o teste é inconclusivo (H_0: rho = 0 vs. H_1: rho > 0); d_U é limite superior do valor crítico.
    # (iii) 4 - d_L < d < 4, rejeita-se H_0 (H_0: rho = 0 vs. H_1: rho < 0); 
    # (iv) 4 - d_U < d < 4 - d_L, o teste é inconclusivo (H_0: rho = 0 vs. H_1: rho < 0);
    # (v) d_U < d < 4 - d_U, não se rejeita H_0, seja a alternativa com rho < 0 ou rho > 0.

## Voltando para os dados da Tabela 12.4

FFlinear <- lm(Y ~ X, data = Table124)
summary(FFlinear)
dwtest(Y ~ X, data = Table124)   
# Com o teste DW rejeitamos H_0: rho = 0 em detrimento de H_1: rho > 0. d_L = 1.44 e d_U = 1.54, ao nível de 5%.

## Teste LM (Lagrange multiplier)

# Muito mais geral que o teste DW. Aqui a estrutura de correlação dos termos de erro pode ser mais abrangente, envolvendo,
# por exemplo, processo MA(p). 
# u_t = rho_1*u_t-1 + rho_2*u_t-2 + rho_3*u_t-3 + ... + rho_p*u_t-p + v_t
# H_0: rho_1 = rho_2 = rho_3 = ... = rho_p = 0.

# Passos do teste:
  # (1) Estime o modelo por OLS e obtenha os resíduos (u_hat).
  # (2) Regrida u_hat em X_t (e se houver mais de uma variável explicativa, incluindo defasagens da variável dependente, inclua)
    # e em u_hat_t-1, u_hat_t-2, u_hat_t-3, ..., u_hat_t-p. *** O maior problema do teste é determinar p - isso é estudado
    # em Séries Temporais. O modelo terá o seguinte formato:
        # u_hat = alpha_1 + alpha_2X_t + rho_hat_1*u_hat_t-1 + rho_hat_2*u_hat_t-2 + ... +  rho_hat_p*u_hat_t-p + v_t
        # Obtenha R^2 do modelo.
  # (3) Admitindo que a amostra é grande, Breusch e Godfrey mostraram que (n-p)R^2 ~ chi^2_p
  # (4) Se p = 1, o teste LM é chamado de teste M de Durbin.

## O maior problema do teste LM é que o valor p não é determinado a priori. Mas é possível fazer alguns experimentos para
# obtê-lo.

# No exemplo, vamos admitir que p = 6, ou seja, que estamos diante de um processo AR(6).
Lmtest <- bgtest(FFlinear, order = 6, data = Table124)
# Veja que o valor do teste LM se encontra na região de rejeição da distribuição chi^2. 
pchisq(Lmtest$statistic, as.numeric(Lmtest$parameter)) ## ou
qchisq(0.99, as.numeric(Lmtest$parameter)) # Veja que Lmtest$statistic >>> qchisq(0.99,6), sugerindo rejeição de H_0(rho = 0);

## Construindo o teste manualmente para AR(6) - abordagem simplificada
DataLm <- data.frame(residuals(FFlinear)[-(1:6)], Table124$X[-(1:6)], residuals(FFlinear)[-c(1:5,40)], residuals(FFlinear)[-c(1:4,39:40)],
                     residuals(FFlinear)[-c(1:3,38:40)], residuals(FFlinear)[-c(1:2,37:40)], residuals(FFlinear)[-c(1,36:40)],
                     residuals(FFlinear)[-c(35:40)])
colnames(DataLm) <- c("ut", "Xt", "ut1", "ut2", "ut3", "ut4", "ut5", "ut6")

LMreg <- lm(ut ~ Xt + ut1 + ut2 + ut3 + ut4 + ut5 + ut6, data = DataLm)
pchisq(nrow(DataLm)*summary(LMreg)$r.squared, nrow(Table124) - nrow(DataLm)) ## Veja, novamente, que (n-p)*R^2 se encontra
                                                                              # na região de rejeição
# O resultado do bgtest e do teste manual são ligeiramente distintos, mas a indicação é idêntica: rejeição de H_0 (rho = 0);
# Isso quer dizer que há pelo menos um coeficiente associado às defasagens que é diferente de zero. É possível agora olhar
# para os resultados de LMreg. 
summary(LMreg)
# Eles sugerem que apenas a primeira defasagem é estatisticamente significativa. Logo, não haveria, em tese, necessidade
# de mais de uma defasagem na equação de autocorrelação dos termos de erro.

## Estratégias para corrigir a autocorrelação pura

# rho é conhecido

## Em geral dispomos do modelo 
      ## Y_t = beta_1 + beta_2 X_t + u_t, com u_t = rho u_t-1 + epsilon_t
## Como a equação acima vale para todo t, vale também para t-1. Daí poderíamos escrever
      ## Y_t-1 = beta_1 + beta_2 X_t-1 + u_t-1; 
      ## Passos
      ## 1) Multiplique todos os termos por rho
            ## rho Y_t-1 = rho beta_1 + rho beta_2 X_t-1 + rho u_t-1; 
      ## 2) Agora substitua na equação de u_t
            ## u_t = rho Y_t-1 - rho beta_1 - rho beta_2 X_t-1 + epsilon_t;
      ## 3) Finalmente, substitua na equação principal
            ## Y_t = beta_1 + beta_2 X_t + rho Y_t-1 - rho beta_1 - rho beta_2 X_t-1 + epsilon_t;
            ## Y_t - rho Y_t-1 = beta_1 (1 - rho) + beta_2 (X_t - rho X_t-1) + espilon_t;
            ## Y*_t = beta*_1 + beta*_2 X*_t + epsilon_t;
      ## 4) Rodamos OLS na regressão final e obtemos estimadores eficientes. Esse é o método GLS.

# rho desconhecido
    ## Em geral rho é desconhecido. Precisamos encontrar métodos interessantes para estimá-lo. Boa parte dos métodos
    ## são iterativos.

## Método da Primeira Diferença
    ## Regra: Se d (DW) < R^2, temos indícios para usarmos a hipótese de que rho = 1. 
    ## Passos
    ## 1) Rode o modelo OLS. Obtenha d a partir deste. Se d < R^2, use a hipótese de primeira diferença.
          ## Y_t - Y_t-1 = beta_2 (X_t - X_t-1) + (u_t - u_t-1); Veja que é só usar a equação de (3) do método anterior, com rho = 1.
    ## 2) Agora rode o modelo
          ## Delta Y_t = beta_2 Delta X_t + epsilon_t
    ## 3) Você pode rodar o Runs Test para verificar se ainda há resquícios de autocorrelação no modelo em diferenças;
        ## Pode também usar o d DW, mas que é de difícil interpretação.
    ## Note que se admitirmos a hipótese de que rho = 1 estamos falando que u_t NÃO é estacionária. No entanto, 
        # quando tomamos a primeira diferença, vemos que
            # u_t = u_t-1 + epsilon_t --->  Delta u_t = epsilon_t; Ou seja, a primeira diferença se torna estacionária,
                                                                # pois epsilon_t é ruído branco (estacionário).

FFlinear <- lm(Y ~ X, data = Table124)
summary(FFlinear)
DW <- dwtest(Y ~ X, data = Table124) 

Yt <- Table124[-1,1]
Xt <- Table124[-1,2]

Yt1 <- Table124[-nrow(Table124),1]
Xt1 <-  Table124[-nrow(Table124),2]

DeltaYt <- Yt - Yt1
DeltaXt <- Xt - Xt1

FirstDiff <- lm(DeltaYt ~ 0 + DeltaXt) ## Inclui-se o 0 para eliminar o intercepto.
summary(FirstDiff)


## rho baseado na estatística d DW
    ## Vimos que d ~ 2(1-rho); logo, hat_rho ~ 1 - d/2. Basta aplicar essa ideia ao modelo para o qual rho é conhecido.

rhoDW <- 1 - (DW$statistic)/2
DeltaYtrho <- Yt - rhoDW*Yt1
DeltaXtrho <- Xt - rhoDW*Xt1

## rho a partir dos resíduos
    ## Passos
    ## 1) Estime o modelo por OLS e obtenha os resíduos;
    ## 2) Rode o modelo
          ## hat_u_t = rho hat_u_t-1 + v_t; Com esse modelo você obterá hat_rho.
    ## 3) Substitua no modelo para o qual rho é conhecido.
    ## Note que os métodos de DW e este geram rho bastante parecidos.

## Métodos Iterativos
    ## Cochrane-Orcutt (iterativo e em dois estágios);
    ## Durbin (dois estágios);

    ## Vantagem de CO é que ele pode ser usado para AR de ordem maior do que 1. 
      

## Prova 2 Econometria II
options(scipen=999)
P153 <- read.table("P153.txt", header = T, sep = "", na.strings = "EMPTY", strip.white = T)

Modelo1 <- lm(Y ~ X1 + X2 + X3, data = P153[-49,])
summary(Modelo1)
Yhat <- Modelo1$fitted.values
ResMod1 <- rstandard(Modelo1)
plot(Yhat, ResMod1, type = "p", pch = 20, ylab = "Residuals", xlab = "Fitted Values")

P153Region <- split(P153[-49,],P153[-49,]$Region)
ResidualsData <- data.frame(P153[-49,ncol(P153)],ResMod1)
colnames(ResidualsData) = c("Region","Residuals")
aa <-split(ResidualsData,ResidualsData$Region)

plot(ResidualsData, type = "p", pch = 20, xaxt = "n")
axis(1, at = 1:4)

ResRegion <- data.frame(P153[-49,ncol(P153)],residuals(Modelo1))
colnames(ResRegion) = c("Region","Residuals")
ResRegion <- split(ResRegion,ResRegion$Region)

sigma2Hat <- vector("list",4)
for(i in 1:4){
  sigma2Hat[[i]] <- sum((ResRegion[[i]]$Residuals)^2)/(nrow(ResRegion[[i]])-1)
}
CjNum <- unlist(sigma2Hat)
CjDen <- sum((residuals(Modelo1))^2)/(nrow(P153[-49,]))

Cj <- sqrt(CjNum/CjDen)

CjCol <- c(rep(Cj[1],nrow(P153Region[[1]])),rep(Cj[2],nrow(P153Region[[2]])),
           rep(Cj[3],nrow(P153Region[[3]])),rep(Cj[4],nrow(P153Region[[4]])))
varMod <- colnames(P153[-c(1,6)])
P153mod <- data.frame(P153[varMod][-49,])
P153fin <-  data.frame(Y = rep(NA,nrow(P153mod)), X1 = rep(NA,nrow(P153mod)), X2 = rep(NA,nrow(P153mod)),
                       X3 = rep(NA,nrow(P153mod)))
for(i in 1:nrow(P153mod)){
  P153fin[i,] <- P153mod[i,]/CjCol[i]
}

P153mod <- data.frame(P153[varMod][-49,],1/CjCol)
colnames(P153mod) <- c(colnames(P153[-c(1,6)]),"Cj")

modfinal <- lm(Y ~ 0+Cj + X1 + X2 + X3, data = P153mod)
summary(modfinal)

weightS <- c(rep(1.177,9),rep(1.503,12),rep(0.475,16),rep(0.938,12))
modfinalS <- rlm(Y ~ X1 + X2 + X3, data = P153mod, weights = 1/CjCol)
summary(modfinalS)



## Prova

Tab2 = read.table("Tabela2.txt", header = T, sep = "")

mod2 <- lm(RD ~ SALES, data = Tab2)
summary(mod2)

## Park Test

res2 <- log(residuals(mod2)^2)
lnSales <- log(Tab2$SALES)

Park <- lm(res2 ~  lnSales)
summary(Park)

## White

resWhite <- (residuals(mod2))^2
Salesaux <- (Tab2$SALES)^2

WhiteTestSales <- lm(resWhite ~ Tab2$SALES + Salesaux)
summary(WhiteTestSales)

nR2Sales <- nrow(Tab2)*(summary(WhiteTestSales)$r.squared)
chiWhite <- qchisq(0.95,2)
nR2Sales > chiWhite # Se falso, rejeita-se heterocedasticidade. Mas é bom olhar para o p-valor associado a nR^2
pvalueWhite <- 1-pchisq(nR2Sales,2) # O valor é um pouco maior do que 7%. Você pode achá-lo suficientemente pequeno para rejeitar
# a hipótese de homocedasticidade

## Correção

library(lmtest)
library(sandwich)

coeftest(mod2, vcov = vcovHC(mod2, type="HC1"))


## Runs
N1 <- 13
N2 <- 7
N <- 20
ER <- 2*N1*N2/N + 1
VR <- (4*(N1^2)*(N2^2) - 2*N1*N2*N)/((N^2)*(N-1))
sdR <- sqrt(VR)

Tab3 = read.table("Tabela3.txt", header = T, sep = "")

mod3 <- lm(Expenditure ~ Stock, data = Tab3)
# DW
DW <-dwtest(Expenditure ~ Stock, data = Tab3)

# Runs
resR <- as.vector(residuals(mod3))
sinaisR <- sign(resR)
R <- length(rle(sinaisR)$lengths)

RunsR <- rle(sinaisR)
N <- length(sinaisR)
N1 <- sum(RunsR$lengths[RunsR$value==1])
N2 <- sum(RunsR$lengths[RunsR$value==-1])
ER <- 2*N1*N2/N + 1
VR <- (4*(N1^2)*(N2^2) - 2*N1*N2*N)/((N^2)*(N-1))
sdR <- sqrt(VR)

# C-O
uHat <- residuals(mod3)[-1]
uHat1 <- residuals(mod3)[-nrow(Tab3)]

modAux <- lm(uHat ~ 0+uHat1)
rho <- modAux$coefficients

rho <- 0.751
# 2 passo
ExpendNew <- (Tab3$Expenditure[-1]) - rho*(Tab3$Expenditure[-nrow(Tab3)])
StockNew <- (Tab3$Stock[-1]) - rho*(Tab3$Stock[-nrow(Tab3)])

mod3New <- lm(ExpendNew ~ StockNew)
summary(mod3New)

DWNew <-dwtest(ExpendNew ~ StockNew)

## Q4
Tab4 = read.table("Tabela4.txt", header = T, sep = "")

## (b) e (c)
mod4 <- lm(H ~ P, data = Tab4)
summary(mod4)
dwtest(H ~ P, data = Tab4)

# Runs
resR <- as.vector(residuals(mod4))
sinaisR <- sign(resR)
R <- length(rle(sinaisR)$lengths)

RunsR <- rle(sinaisR)
N <- length(sinaisR)
N1 <- sum(RunsR$lengths[RunsR$value==1])
N2 <- sum(RunsR$lengths[RunsR$value==-1])
ER <- 2*N1*N2/N + 1
VR <- (4*(N1^2)*(N2^2) - 2*N1*N2*N)/((N^2)*(N-1))
sdR <- sqrt(VR)

## (d)
mod4New <- lm(H ~ P + D, data = Tab4)
summary(mod4New)
dwtest(H ~ P + D, data = Tab4)

# Runs
resR <- as.vector(residuals(mod4New))
sinaisR <- sign(resR)
R <- length(rle(sinaisR)$lengths)

RunsR <- rle(sinaisR)
N <- length(sinaisR)
N1 <- sum(RunsR$lengths[RunsR$value==1])
N2 <- sum(RunsR$lengths[RunsR$value==-1])
ER <- 2*N1*N2/N + 1
VR <- (4*(N1^2)*(N2^2) - 2*N1*N2*N)/((N^2)*(N-1))
sdR <- sqrt(VR)






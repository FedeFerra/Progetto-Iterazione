dati <- read.table("Scrivania/Progetto/derma.dat", he = TRUE)
colnames(dati) <- c("Gravità", colnames(dati)[-1])
colnames(dati)

dati.prova <- apply(dati, 2, function(x) rep(x, dati$n))[, -6]
dati.prova <- as.data.frame(dati.prova)
dati.table <- table(dati.prova)

head(dati.prova)
str(dati.table)

for(i in 1:5){
  print(margin.table(dati.table, i))
}


for(i in 1:4){
  for(j in (i + 1):5){
    print(margin.table(dati.table, c(i, j)))
  }
}

obj <- NULL
for(i in 1:4){
  for(j in (i + 1):5){
    ftest <- fisher.test(margin.table(dati.table, c(i, j)))
    obj <- rbind(obj, c(i,j, round(ftest$estimate, 3), round(ftest$p.value, 3)))
  }
}

obj <- as.data.frame(obj)
colnames(obj)[3] <- "odds"
obj$odds.sem <- ifelse(obj$V4<0.12,obj$odds,1)

V1 <- c()
V2 <- c()
for(i in 1:nrow(obj)){
  V1[i] <- names(dati)[obj$V1[i]]
  V2[i] <- names(dati)[obj$V2[i]]
}

obj$V1 <- V1
obj$V2 <- V2
obj


dati.new <- dati #32 righe
dati.new$Trattamento <- (as.numeric(dati$Trattamento) - 2)*(-1)
dati.new$Gravità <- (as.numeric(dati$Gravità) - 2)*(-1)
dati.new$X1sett <- (as.numeric(dati$X1sett) - 2)*(-1)
dati.new$X2sett <- (as.numeric(dati$X2sett) - 2)*(-1)
dati.new$X4sett <- (as.numeric(dati$X4sett) - 2)*(-1)
colnames(dati.new)[3:5] <- c("S1", "S3", "S4")

dati.prova <- apply(dati.new, 2, function(x) rep(x, dati.new$n))[, -6] #340 righe
dati.prova <- as.data.frame(dati.prova)
dati.table <- table(dati.prova)

#Proviamo modello logit
#Codifica delle variabili

# mod1 <- glm(S1 ~ Trattamento + Gravità, data = dati.prova, family = "binomial")
# summary(mod1)
# 
# mod3 <- glm(S3 ~ Trattamento + Gravità + S1, data = dati.prova, family = "binomial")
# summary(mod3)
# 
# mod4 <- glm(S4 ~ Trattamento + Gravità + S1 + S3, data = dati.prova, family = "binomial")
# summary(mod4)
# 
# mod4.2 <- glm(S4 ~ Trattamento + Gravità + S3, data = dati.prova, family = "binomial")
# summary(mod4.2)
# 
# #Interazioni
# mod1.1 <- glm(S1 ~ Trattamento*Gravità, data = dati.prova, family = "binomial")
# summary(mod1.1)
# 
# mod3.1 <- glm(S3 ~ Trattamento*Gravità + S1, data = dati.prova, family = "binomial")
# summary(mod3.1)
# 
# mod4.1 <- glm(S4 ~ Trattamento*Gravità + S1 + S3, data = dati.prova, family = "binomial")
# summary(mod4.1)
# 
# #Stima di S1
# mod1.2 <- glm(S1 ~ Gravità, data = dati.prova, family = "binomial")
# summary(mod1.2)
# 
# #Tabella di contingenza
# table(dati.prova$Gravità, dati.prova$S1)/matrix(c(rep(150, 2), rep(190, 2)), nrow = 2, byrow = TRUE)
# 
# b0 <- as.numeric(mod1.2$coefficients[1]) 
# b1 <- as.numeric(mod1.2$coefficients[2])
# 
# gamma1 <- exp(b0 + b1)/(1 + exp(b0 + b1))
# gamma1
# 1 - gamma1
# gamma0 <- exp(b0)/(1 + exp(b0))
# gamma0
# 1 - gamma0
# ##mi sembrano tutti troppo uguali come risultati!! ?????????????
# 
# 
# #Stima di P
# alpha <- 0.5*(mean(I(dati.prova$S1==0 & dati.prova$S3==0)))+0.5*(mean(I(dati.prova$S3==0 & dati.prova$S4==0)))
# 
# alpha1 <- 0.5*(mean(I(dati.prova$S1[dati.prova$Trattamento == 1]==0 & dati.prova$S3[dati.prova$Trattamento == 1]==0)))+
#   0.5*(mean(I(dati.prova$S3[dati.prova$Trattamento == 1]==0 & dati.prova$S4[dati.prova$Trattamento == 1]==0)))
# alpha0 <- 0.5*(mean(I(dati.prova$S1[dati.prova$Trattamento == 0]==0 & dati.prova$S3[dati.prova$Trattamento == 0]==0)))+
#   0.5*(mean(I(dati.prova$S3[dati.prova$Trattamento == 0]==0 & dati.prova$S4[dati.prova$Trattamento == 0]==0)))
# ##attenzione! credo che ipotiazzare che alpha1 sia cost da s1->s3 e s3->s4 sia sbagliato. valori molto diversi (0.25,0.56)
# ##mentre per alpha0 sembra cost (0.18,0.23)
# 
# 
# beta <- 0.5*(mean(I(dati.prova$S1==1 & dati.prova$S3==1)))+0.5*(mean(I(dati.prova$S3==1 & dati.prova$S4==1)))
# 
# beta1 <- 0.5*(mean(I(dati.prova$S1[dati.prova$Trattamento == 1]==1 & dati.prova$S3[dati.prova$Trattamento == 1]==1)))+
#   0.5*(mean(I(dati.prova$S3[dati.prova$Trattamento == 1]==1 & dati.prova$S4[dati.prova$Trattamento == 1]==1)))
# beta0 <- 0.5*(mean(I(dati.prova$S1[dati.prova$Trattamento == 0]==1 & dati.prova$S3[dati.prova$Trattamento == 0]==1)))+
#   0.5*(mean(I(dati.prova$S3[dati.prova$Trattamento == 0]==1 & dati.prova$S4[dati.prova$Trattamento == 0]==1)))
# ##attenzione! credo che ipotiazzare che beta1 sia cost da s1->s3 e s3->s4 sia sbagliato. valori molto diversi (0.29,0.05)
# ##anche beta0 abbastanza diverso (0.42,0.25)
# 
# 
# P1 <- matrix(c(beta1, 1 - beta1, 1 - alpha1, alpha1), nrow = 2, byrow = TRUE)
# P0 <- matrix(c(beta0, 1 - beta0, 1 - alpha0, alpha0), nrow = 2, byrow = TRUE)
# #dist invariante di P1=(0.41,0.58) e P0=(0.55,0.45)
# #cio? se trat==1, allora ho prob 0.58 di avere sintomi anormali alla lunga
# #cio? se trat==0, allora ho prob 0.45 di avere sintomi anormali alla lunga
# #questo ? molto sbagliato dato che la dist marginale di s4 ? 0.71,0.29
# 
# 
# 
# 
# #verifica dei valori predetti
# 
# 
# #S1
# c(gamma0,1-gamma0,gamma1,1-gamma1)*c(rep(150, 2), rep(190, 2))
# #stima di margin.table(dati.table, c(1,3))
# 
# 
# #S3
# p3.11 <- (c(gamma1, 1 - gamma1)%*%P1)[1]
# 
# p3.10 <- (c(gamma0, 1 - gamma0)%*%P1)[1]
# 
# p3.01 <- (c(gamma1, 1 - gamma1)%*%P0)[1]
# 
# p3.00 <- (c(gamma0, 1 - gamma0)%*%P0)[1]
# 
# round(c(p3.00, p3.01, p3.10, p3.11)*c(80, 100, 70, 90),1)
# #stima di  margin.table(dati.table,c(2,1,4)) per S3=1 :33,72,15,45
# round(c(1-p3.00, 1-p3.01, 1-p3.10, 1-p3.11)*c(80, 100, 70, 90),1)
# #stima di  margin.table(dati.table,c(2,1,4)) per S3=0 :47,72,15,45
# 
# 
# 
# #S4
# p4.11 <- (c(gamma1, 1 - gamma1)%*%P1%*%P1)[1]
# 
# p4.10 <- (c(gamma0, 1 - gamma0)%*%P1%*%P1)[1]
# 
# p4.01 <- (c(gamma1, 1 - gamma1)%*%P0%*%P0)[1]
# 
# p4.00 <- (c(gamma0, 1 - gamma0)%*%P0%*%P0)[1]
# 
# round(c(p4.00, p4.01, p4.10, p4.11)*c(80, 100, 70, 90),1)
# #stima di  margin.table(dati.table,c(2,1,5)) per S4=1 :26,54,2,15 
# round(c(1-p4.00, 1-p4.01, 1-p4.10, 1-p4.11)*c(80, 100, 70, 90),1)
# #stima di  margin.table(dati.table,c(2,1,5)) per S4=0 :54,46,68,75 






#Verosimiglianza parziale
# llik <- function(delta){
#   74*log(gamma(delta, 1, 1)) + 33*log(gamma(delta, 1, 0)) + 79*log(gamma(delta, 0, 1)) + 39*log(gamma(delta, 0, 0)) +
#     16*log(1 - gamma(delta, 1, 1)) + 37*log(1 - gamma(delta, 1, 0)) + 21*log(1 - gamma(delta, 0, 1)) + 41*log(1 - gamma(delta, 0, 0))
# }
# 
# mle <- nlminb(rep(2, 3), function(x) -llik(x))
# mle
# 
# library(numDeriv)
# hess <- optimHess(mle$par, llik)
# sqrt(diag(solve(-hess)))
# 
# alpha.hat <- 2*56/(2*56 - 76 - 17 + 26 + 14 + 28 + 17)
# beta.hat <- 2*40/(2*40 - 83 - 14 + 83 + 26 + 76 + 28)
# alpha.hat
# beta.hat

#Aggiungiamo le nuove variabili
dati.prova
dati.prova.due <- cbind(rbind(dati.prova[, 1:2], dati.prova[, 1:2]), rbind(as.matrix(dati.prova[, 3:4]), as.matrix(dati.prova[, 4:5])))
dati.prova.due$S1 <- rep(dati.prova$S1, 2)
dati.prova.due$Z <- ifelse(rowSums(dati.prova.due) == 0, 1, 0)
dati.prova.due$W <- ifelse(rowSums(dati.prova.due) == 2, 1, 0)
dati.prova.due$S3 <- NULL

table(dati.prova.due$Z, dati.prova.due$W)
head(dati.prova.due)


#MODELLO MARKOV (9 param)
#Aggiungiamo la parte di dipendenza da Gravità e trattamento
parametri <- c("delta0","delta1","delta3","eta0","eta1","eta2","zeta0","zeta1","zeta2")

gamma <- function(delta, t, g){
  exp(sum(delta*c(1, t, g)))/(1 + exp(sum(delta*c(1, t, g))))
}

alpha <- function(eta, t, g){
  exp(sum(eta*c(1, t, g)))/(1 + exp(sum(eta*c(1, t, g))))
}

beta <- function(zeta, t, g){
  exp(sum(zeta*c(1, t, g)))/(1 + exp(sum(zeta*c(1, t, g))))
}

GT <- matrix(c(0, 0, 0, 1, 1, 0, 1, 1), ncol = 2, byrow = TRUE)

llik <- function(param){
  delta <- param[1:3]
  eta <- param[4:6]
  zeta <- param[7:9]
  out <- 0
  for(i in 1:nrow(GT)){
    out <- out + sum(dati$n[(8*i - 7):(8*i)]*log(c((1-gamma(delta, GT[i, 2], GT[i, 1]))*alpha(eta, GT[i, 2], GT[i, 1])^2, 
                                     (1-gamma(delta, GT[i, 2], GT[i, 1]))*alpha(eta, GT[i, 2], GT[i, 1])*(1-alpha(eta, GT[i, 2], GT[i, 1])),
                                      (1-gamma(delta, GT[i, 2], GT[i, 1]))*(1 - beta(zeta, GT[i, 2], GT[i, 1]))*(1-alpha(eta, GT[i, 2], GT[i, 1])),
                                    (1-gamma(delta, GT[i, 2], GT[i, 1]))*beta(zeta, GT[i, 2], GT[i, 1])*(1-alpha(eta, GT[i, 2], GT[i, 1])),
                                    gamma(delta, GT[i, 2], GT[i, 1])*(1 - beta(zeta, GT[i, 2], GT[i, 1]))*alpha(eta, GT[i, 2], GT[i, 1]),
                                    gamma(delta, GT[i, 2], GT[i, 1])*(1 - beta(zeta, GT[i, 2], GT[i, 1]))*(1 - alpha(eta, GT[i, 2], GT[i, 1])),
                                    gamma(delta, GT[i, 2], GT[i, 1])*(1 - beta(zeta, GT[i, 2], GT[i, 1]))*beta(zeta, GT[i, 2], GT[i, 1]),
                                    gamma(delta, GT[i, 2], GT[i, 1])*beta(zeta, GT[i, 2], GT[i, 1])^2
                                    )))
  }
  return(out)
}

mle <- nlminb(rep(0, 9), function(x) -llik(x))
mle$par

library(numDeriv)
hess <- optimHess(mle$par, llik)
tval <- mle$par/sqrt(diag(solve(-hess)))
pval <- 2*(1-pnorm(abs(tval)))
parametri;round(mle$par,3); round(pval,3)
#Tutti significativi tranne delta1 delta0 e zeta0

#verifica stabilt? nuerica delle stime
mle.start <- matrix(rnorm(180, 0, 5), nrow = 20, ncol = 9)
mle.boot <- t(apply(mle.start, 1, function(y) nlminb(y, function(x) -llik(x))$par))
apply(mle.boot, 2, var)

#MODELLO MARKOV RISTRETTO  (6 param)
#Proviamo ad eliminare i parametri non significativi
llik.r <- function(param){
  delta <- c(0, 0, param[3])
  eta <- param[4:6]
  zeta <- c(0, param[8:9])
  out <- 0
  for(i in 1:nrow(GT)){
    out <- out + sum(dati$n[(8*i - 7):(8*i)]*log(c((1-gamma(delta, GT[i, 2], GT[i, 1]))*alpha(eta, GT[i, 2], GT[i, 1])^2, 
                                                   (1-gamma(delta, GT[i, 2], GT[i, 1]))*alpha(eta, GT[i, 2], GT[i, 1])*(1-alpha(eta, GT[i, 2], GT[i, 1])),
                                                   (1-gamma(delta, GT[i, 2], GT[i, 1]))*(1 - beta(zeta, GT[i, 2], GT[i, 1]))*(1-alpha(eta, GT[i, 2], GT[i, 1])),
                                                   (1-gamma(delta, GT[i, 2], GT[i, 1]))*beta(zeta, GT[i, 2], GT[i, 1])*(1-alpha(eta, GT[i, 2], GT[i, 1])),
                                                   gamma(delta, GT[i, 2], GT[i, 1])*(1 - beta(zeta, GT[i, 2], GT[i, 1]))*alpha(eta, GT[i, 2], GT[i, 1]),
                                                   gamma(delta, GT[i, 2], GT[i, 1])*(1 - beta(zeta, GT[i, 2], GT[i, 1]))*(1 - alpha(eta, GT[i, 2], GT[i, 1])),
                                                   gamma(delta, GT[i, 2], GT[i, 1])*(1 - beta(zeta, GT[i, 2], GT[i, 1]))*beta(zeta, GT[i, 2], GT[i, 1]),
                                                   gamma(delta, GT[i, 2], GT[i, 1])*beta(zeta, GT[i, 2], GT[i, 1])^2
    )))
  }
  return(out)
}

mle.r <- nlminb(rep(0, 9), function(x) -llik.r(x))
mle.r

mle$par
mle.r$par

hess.r <- optimHess(mle.r$par, llik.r)
hess.r <- hess.r[-c(1,2,7),-c(1,2,7)] #6x6
tval.r <- mle.r$par[-c(1,2,7)]/sqrt(diag(-solve(hess.r))) 
pval.r <- 2*(1-pnorm(abs(tval.r)))
parametri[-c(1,2,7)];round(mle.r$par,3); round(pval.r,3)
#i sei param rimanenti sono singif

#stabilit? numerica del modello ristretto
mle.start <- matrix(rnorm(180, 0, 5), nrow = 20, ncol = 9)
mle.start[,c(1,2,7)] <- 0
mle.boot.r <- t(apply(mle.start, 1, function(y) nlminb(y, function(x) -llik.r(x))$par))
apply(mle.boot.r, 2, var)

#scelta fra modello ristretto (6 param) e libero (9 param)
1 - pchisq(2*(598.16 - 596.28), 3)
#Il test non rifiuta l'ipotesi nulla di uguaglianza dei due modelli, quindi prendiamo il modello ristretto

#Predizioni con modello ristretto (6 param)
pred.r <- c()
for(i in 1:4){
  pred.r <- c(pred.r, c((1-gamma(mle.r$par[1:3], GT[i, 2], GT[i, 1]))*alpha(mle.r$par[4:6], GT[i, 2], GT[i, 1])^2, 
  (1-gamma(mle.r$par[1:3], GT[i, 2], GT[i, 1]))*alpha(mle.r$par[4:6], GT[i, 2], GT[i, 1])*(1-alpha(mle.r$par[4:6], GT[i, 2], GT[i, 1])),
  (1-gamma(mle.r$par[1:3], GT[i, 2], GT[i, 1]))*(1 - beta(mle.r$par[7:9], GT[i, 2], GT[i, 1]))*(1-alpha(mle.r$par[4:6], GT[i, 2], GT[i, 1])),
  (1-gamma(mle.r$par[1:3], GT[i, 2], GT[i, 1]))*beta(mle.r$par[7:9], GT[i, 2], GT[i, 1])*(1-alpha(mle.r$par[4:6], GT[i, 2], GT[i, 1])),
  gamma(mle.r$par[1:3], GT[i, 2], GT[i, 1])*(1 - beta(mle.r$par[7:9], GT[i, 2], GT[i, 1]))*alpha(mle.r$par[4:6], GT[i, 2], GT[i, 1]),
  gamma(mle.r$par[1:3], GT[i, 2], GT[i, 1])*(1 - beta(mle.r$par[7:9], GT[i, 2], GT[i, 1]))*(1 - alpha(mle.r$par[4:6], GT[i, 2], GT[i, 1])),
  gamma(mle.r$par[1:3], GT[i, 2], GT[i, 1])*(1 - beta(mle.r$par[7:9], GT[i, 2], GT[i, 1]))*beta(mle.r$par[7:9], GT[i, 2], GT[i, 1]),
  gamma(mle.r$par[1:3], GT[i, 2], GT[i, 1])*beta(mle.r$par[7:9], GT[i, 2], GT[i, 1])^2
))
}

pred.r <- pred.r*rep(c(80, 70, 100, 90), c(8, 8, 8, 8))
dati.new$pred.r <- round(pred.r,2)
dati.new

C.r <- sum((dati.new$n - dati.new$pred.r)^2/dati.new$pred.r)
1 - pchisq(C.r, (32 - 6))
curve(dchisq(x,32-6),xlim=c(0,100), ylab="Test Chi Quadro per bont? del modello")
abline(v=C.r,col=2)
#modello rifiutato

plot(1:32, dati.new$n, type = "h", ylim = c(0, max(c(dati.new$n, dati.new$pred.r))),ann = F,xaxt="n")
abline(v=c(8.6,16.6,24.6),lty="dotted")
arrows(1:32+0.2,rep(0,32),1:32+0.2,dati.new$pred.r, col=2,angle = 0, code = 0)
axis(1,1:32,rep(rev(letters[1:8]),4))



#MODELLO MARKOV RISTRETTO con S2 (6 param)
llik.r.s2 <- function(param){
  delta <- c(0,0,param[3])
  eta <- c(0,param[5:6])
  zeta <- param[7:9]
  out <- 0 
  for(i in 1:nrow(GT)){
    gg <- gamma(delta,GT[i,2],GT[i,1])
    aa <- alpha(eta,  GT[i,2],GT[i,1])
    bb <- beta(zeta,  GT[i,2],GT[i,1])
    out <- out + sum(dati$n[(8*i - 7):(8*i)]*log(c( (1-gg) * (aa^2 + (1-aa)*(1-bb))  * aa, 
                                                    (1-gg) * (aa^2 + (1-aa)*(1-bb))  * (1-aa),
                                                    (1-gg) * (aa*(1-aa) + (1-aa)*bb) * (1-bb),
                                                    (1-gg) * (aa*(1-aa) + (1-aa)*bb) * bb,
                                                    gg     * (bb*(1-bb) + (1-bb)*aa) * aa,
                                                    gg     * (bb*(1-bb) + (1-bb)*aa) * (1-aa),
                                                    gg     * (bb^2+(1-bb)*(1-aa))    * (1-bb),
                                                    gg     * (bb^2+(1-bb)*(1-aa))    * bb
    )))
  }
  return(out)
}

mle.r.s2 <- nlminb(rep(0, 9), function(x) -llik.r.s2(x))
mle.r.s2$par
#delta0 delta1 eta0  == 0

mle$par
mle.r$par
mle.r.s2$par

hess.r.s2 <- optimHess(mle.r.s2$par, llik.r.s2)
hess.r.s2 <- hess.r.s2[-c(1,2,4),-c(1,2,4)]
tval.r.s2 <- mle.r.s2$par[-c(1,2,4)]/sqrt(diag(-solve(hess.r.s2)))
pval.r.s2 <- 2*(1-pnorm(abs(tval.r.s2)))
parametri[-c(1,2,4)];round(mle.r.s2$par,3); round(pval.r.s2,3)
#erano non signif sono delta0 delta1 eta0 (non zeta0!) e sono stati imposti a zero
#i rimanenti sono signif

#stabilit? numerica del modello ristretto con S2
mle.start <- matrix(rnorm(180, 0, 5), nrow = 20, ncol = 9)
mle.start[,c(1,2,4)] <- 0
mle.boot.r.s2 <- t(apply(mle.start, 1, function(y) nlminb(y, function(x) -llik.r.s2(x))$par))
apply(mle.boot.r.s2, 2, var)

#scelta fra modello ristretto (6 param) e libero (9 param)
#verosim modello s2 libero = 598.164
1 - pchisq(-2*(598.164 - 598.515), 3)
#Il test non rifiuta l'ipotesi nulla di uguaglianza dei due modelli, quindi prendiamo il modello ristretto con s2


#Predizioni con modello ristretto con S2 (6 param)
pred.r.s2 <- c()
for(i in 1:4){
  gg <- gamma(mle.r.s2$par[1:3], GT[i,2],GT[i,1])
  aa <- alpha(mle.r.s2$par[4:6], GT[i,2],GT[i,1])
  bb <-  beta(mle.r.s2$par[7:9], GT[i,2],GT[i,1])
  pred.r.s2 <- c(pred.r.s2, c((1-gg) * (aa^2 + (1-aa)*(1-bb))  * aa, 
                        (1-gg) * (aa^2 + (1-aa)*(1-bb))  * (1-aa),
                        (1-gg) * (aa*(1-aa) + (1-aa)*bb) * (1-bb),
                        (1-gg) * (aa*(1-aa) + (1-aa)*bb) * bb,
                        gg     * (bb*(1-bb) + (1-bb)*aa) * aa,
                        gg     * (bb*(1-bb) + (1-bb)*aa) * (1-aa),
                        gg     * (bb^2+(1-bb)*(1-aa))    * (1-bb),
                        gg     * (bb^2+(1-bb)*(1-aa))    * bb

  ))
}

pred.r.s2 <- pred.r.s2*rep(c(80, 70, 100, 90), each=8)
dati.new$pred.r.s2 <- round(pred.r.s2,2)
dati.new

C.r.s2 <- sum((dati.new$n - dati.new$pred.r.s2)^2/dati.new$pred.r.s2)
1 - pchisq(C.r.s2, (32 - 6))
curve(dchisq(x,32-6),xlim=c(0,100), ylab="Test Chi Quadro per bont? del modello")
abline(v=C.r,col=2) #.r
abline(v=C.r.s2,col=4) #.r.s2
#modello rifiutato

plot(1:32, dati.new$n, type = "h", ylim = c(0, max(c(dati.new$n, dati.new$pred.r, dati.new$pred.r.s2))),ann = F,xaxt="n")
abline(v=c(8.65,16.65,24.65),lty="dotted")
arrows(1:32+0.2, rep(0,32),1:32+0.2, dati.new$pred.r,    col=2,angle = 0, code = 0)
arrows(1:32+0.3, rep(0,32),1:32+0.3, dati.new$pred.r.s2, col=4,angle = 0, code = 0)
axis(1,1:32,rep(rev(letters[1:8]),4))






#Proviamo con una frailty
dati.prova
dati.frailty <- as.data.frame(apply(dati.prova, 2, function(x) rep(x, rep(3, 340))))
Y <- unlist(sapply(1:32, function(x) rep(dati.new[x, 3:5], dati.new[x, 6])))
dati.frailty$Y <- as.numeric(Y)
dati.frailty$S1 <- NULL
dati.frailty$S3 <- NULL
dati.frailty$S4 <- NULL
dati.frailty$ID <- rep(1:340, rep(3, 340))
dati.frailty$S <- as.factor(names(Y))

m2 <- glmer(Y ~ Gravità + Trattamento + S + (1|ID), data = dati.frailty, family = binomial)
summary(m2)


library(geepack)
mf <- formula(Y ~ Gravità + Trattamento)
gee1 <- geeglm(mf, data = dati.frailty, family = binomial(link = "logit"), id = ID, corstr = "unstr",  std.err = "san.se")
summary(gee1)


#Markov frailty
#Funzione per la verosimiglianza di ogni osservazione
gamma.f <- function(delta, t, g, eps){
  exp(sum(delta*c(1, t, g), eps))/(1 + exp(sum(delta*c(1, t, g), eps)))
}

alpha.f <- function(eta, t, g, eps){
  exp(sum(eta*c(1, t, g), eps))/(1 + exp(sum(eta*c(1, t, g), eps)))
}

beta.f <- function(zeta, t, g, eps){
  exp(sum(zeta*c(1, t, g), eps))/(1 + exp(sum(zeta*c(1, t, g), eps)))
}

#In data come input vuole la riga Grav - Tratt - S1 - S3 - S4
#In param come input come prima aggiungendo eps (e forse sigma)
pattern <- dati.new[1:8, 3:5] 
llik.frailty.obs <- function(param, data){
  #delta <- param[1:3]
  delta <- c(0, 0, param[3])
  #eta <- param[4:6]
  eta <- c(0, param[5:6])
  zeta <- param[7:9]
  eps <- param[10]
  out <- NULL
  GT <- data[1:2]
  gg <- gamma.f(delta, GT[2], GT[1], eps)
  aa <- alpha.f(eta, GT[2], GT[1], eps)
  bb <- beta.f(zeta, GT[2], GT[1], eps)
  #Funziona ma ritorna il vettore con tutti 0 tranne quello giusto
  out <- sum(as.numeric(apply(pattern, 1, function(x) all(x == data[3:5])))*log(c( (1-gg) * (aa^2 + (1-aa)*(1-bb))  * aa, 
                                                    (1-gg) * (aa^2 + (1-aa)*(1-bb))  * (1-aa),
                                                    (1-gg) * (aa*(1-aa) + (1-aa)*bb) * (1-bb),
                                                    (1-gg) * (aa*(1-aa) + (1-aa)*bb) * bb,
                                                    gg     * (bb*(1-bb) + (1-bb)*aa) * aa,
                                                    gg     * (bb*(1-bb) + (1-bb)*aa) * (1-aa),
                                                    gg     * (bb^2+(1-bb)*(1-aa))    * (1-bb),
                                                    gg     * (bb^2+(1-bb)*(1-aa))    * bb)))
  return(out)
}

llik.frailty <- function(param, data){
  sum(as.vector(apply(data, 1, function(y) gauss.hermite(function(x) llik.frailty.obs(c(param, x), y), mu = 0, sd = 0.5, order = 9))))
}

mle.frailty <- nlminb(rep(0, 9), function(x) -llik.frailty(x, data = dati.prova))
mle.frailty$par


hess.frailty <- optimHess(mle.frailty$par, llik.frailty, data = dati.prova)
mle.frailty$par/sqrt(diag(-solve(hess.frailty))) 
#Proviamo a vedere come funziona


pred.frailty <- rep(0, nrow(dati.prova))
for(i in 1:nrow(dati.prova)){
  gg <- gamma.f(mle.frailty$par[1:3], dati.prova[i, 2], dati.prova[i, 1], eps[i])
  aa <- alpha.f(mle.frailty$par[4:6], dati.prova[i, 2], dati.prova[i, 1], eps[i])
  bb <- beta.f(mle.frailty$par[7:9], dati.prova[i, 2], dati.prova[i, 1], eps[i])
  #Funziona ma ritorna il vettore con tutti 0 tranne quello giusto
  pred.frailty[i] <- sum(as.numeric(apply(pattern, 1, function(x) all(x == dati.prova[i, 3:5])))*c( (1-gg) * (aa^2 + (1-aa)*(1-bb))  * aa, 
                                                                                   (1-gg) * (aa^2 + (1-aa)*(1-bb))  * (1-aa),
                                                                                   (1-gg) * (aa*(1-aa) + (1-aa)*bb) * (1-bb),
                                                                                   (1-gg) * (aa*(1-aa) + (1-aa)*bb) * bb,
                                                                                   gg     * (bb*(1-bb) + (1-bb)*aa) * aa,
                                                                                   gg     * (bb*(1-bb) + (1-bb)*aa) * (1-aa),
                                                                                   gg     * (bb^2+(1-bb)*(1-aa))    * (1-bb),
                                                                                   gg     * (bb^2+(1-bb)*(1-aa))    * bb))
}

pred.frailty <- pred.frailty*rep(c(80, 70, 100, 90), each = 8)
prob.frailty <- prob*rep(c(80, 70, 100, 90), each = 8)
dati.prova$pred.frailty <- pred.frailty
prob <- pred.frailty[cumsum(dati$n)]
pred <- pred.frailty*340
dati.new

C.frailty <- sum((dati.new$n - dati.new$pred.frailty)^2/dati.new$pred.frailty)
1 - pchisq(C.frailty, (32 - 6))
curve(dchisq(x,32-6),xlim=c(0,100), ylab="Test Chi Quadro per bont? del modello")
abline(v=C.r.s2,col=2) #.r
abline(v=C.frailty,col=4) #.r.s2


plot(1:32, dati.new$n, type = "h", ylim = c(0, max(c(dati.new$n, dati.new$pred.frailty, dati.new$pred.r.s2))),ann = F,xaxt="n")
abline(v=c(8.65,16.65,24.65),lty="dotted")
arrows(1:32+0.2, rep(0,32),1:32+0.2, dati.new$pred.frailty,    col=2,angle = 0, code = 0)
arrows(1:32+0.4, rep(0,32),1:32+0.4, dati.new$pred.r.s2, col=4,angle = 0, code = 0)
axis(1,1:32,rep(rev(letters[1:8]),4))

as.vector(apply(dati.prova, 1, function(y) gauss.hermite(function(x) llik.frailty.obs(c(mle.frailty$par, x), y), mu = 0, sd = 1, order = 9)))

#EM-ish
llik.frailty.sigma <- function(param, data, sigma){
  sum(as.vector(apply(data, 1, function(y) gauss.hermite(function(x) llik.frailty.obs(c(param, x), y), mu = 0, sd = sigma, order = 9))))
}

sigma.hat <- 1
theta.hat <- mle.frailty$par

for(i in 1:2){
  sigma.hat <- nlminb(sigma.hat, function(x) -llik.frailty.sigma(theta.hat, data = dati.prova, x))$par
  theta.hat <- nlminb(theta.hat, function(x) -llik.frailty.sigma(x, data = dati.prova, sigma.hat))$par
  cat(i, "\n")
}


#Proviamo con l'idea di bruno
#Ho riutilizzato frailty perchè non serviva ma faceva comodo come partenza
head(dati.frailty)
dati.frailty <- as.data.frame(apply(dati.prova, 2, function(x) rep(x, rep(3, 340))))
Y <- unlist(sapply(1:32, function(x) rep(dati.new[x, 3:5], dati.new[x, 6])))
dati.frailty$Y <- as.numeric(Y)
dati.frailty$pred.frailty <- NULL
dati.frailty$S <- as.factor(names(Y))

dati.frailty$SET <- ifelse(dati.frailty$S == "S1", "Inizio", 0)
dati.frailty$SET[dati.frailty$S == "S3"] <- ifelse(dati.frailty$S1[dati.frailty$S == "S3"] == 0, "I-normale", "I-anormale")
dati.frailty$SET[dati.frailty$S == "S4"] <- paste(dati.frailty$S1[dati.frailty$S == "S4"], dati.frailty$S3[dati.frailty$S == "S4"], sep = "")

mod1 <- glm(Y ~ Gravità*Trattamento*SET, data = dati.frailty, family = "binomial")
summary(mod1)
length(mod1$coefficients)

library(MASS)
search.opt <- stepAIC(mod1, direction = "both")
bothways <- step(mod1, direction = "both")

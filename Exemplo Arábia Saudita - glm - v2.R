### Conjunto de dados Saudi Arabia Real Estate (AQAR)
#
setwd('C:\\Gustavo\\Consultorias\\Previdência\\Aulas\\Aula 4')
library(tidyverse)
dados_orig<-read_csv('Saudi Arabia real estate.csv',locale = locale(encoding = "UTF-8"))

# الرياض    - Al Riyadh
# جدة       - Jeddah
# الدمام    - Dammam
# الخبر     - Al-Khobar

dados_orig$city[which(dados_orig$city=='الرياض')]<-'Al Riyadh'
dados_orig$city[which(dados_orig$city=='جدة')]<-'Jeddah'
dados_orig$city[which(dados_orig$city=='الدمام')]<-'Dammam'
dados_orig$city[which(dados_orig$city=='الخبر')]<-'Al Khobar'
table(dados_orig$city)

## Vamos estudar como o preço do aluguel de cada imóvel ('price') se relaciona com as variáveis 'city' e  'size'.

dados<-subset(dados_orig,select = c('price','city','size'))
attach(dados)
library(pastecs)
dados_num<-dados[,-which(names(dados) %in% c('city'))]
descritivas<-data.frame(round(stat.desc(dados_num),4))
plot(size,price)

# Duas observações caóticas: tamanho mínimo (1m^2) e tamanho máximo (95000m^2, mesmo valor de price: suspeito), ambas em Jeddah.
# Geralmente, vamos atrás para saber se essas observações estão corretas ou não.
# Se estiverem, ajustamos o modelo com elas e vemos suas influências no ajuste.
# Se for necessário, as removemos e nos lembramos sempre que o modelo foi ajustado sem tais outliers.

dados_limpos<-dados[-c(which.min(dados$size),which.max(dados$size)),]
dados_limpos$city<-factor(dados_limpos$city)

###################################
## Modelo normal da aula passada ##
###################################

modelo_norm<-lm(price~city+size+city:size,data = dados_limpos)

summary(modelo_norm)
anova(modelo_norm)

matriz_modelo_norm<-model.matrix(modelo_norm)
View(matriz_modelo_norm)

modelo_norm2<-update(modelo_norm,.~. -city:size)
summary(modelo_norm2)
matriz_modelo_norm2<-model.matrix(modelo_norm2)
View(matriz_modelo_norm2)

AIC(modelo_norm)
AIC(modelo_norm2)
AIC(lm(price~1,data = dados_limpos))

?step  # utiliza AIC para seleção de modelos
step(modelo_norm)
step(modelo_norm2,scope = list(upper=formula(modelo_norm),lower=formula(price~1) ))

## step e extractAIC diferem da função AIC, pois as duas primeiras ignoram algumas constantes que a última considera.

BIC(modelo_norm)
BIC(modelo_norm2)


dados_Riyadh<-subset(dados_limpos,city=='Al Riyadh')
dados_Jeddah<-subset(dados_limpos,city=='Jeddah')
dados_Dammam<-subset(dados_limpos,city=='Dammam')
dados_Khobar<-subset(dados_limpos,city=='Al Khobar')

dados_Khobar$predito<-predict(modelo_norm2,dados_Khobar)
dados_Dammam$predito<-predict(modelo_norm2,dados_Dammam)
dados_Jeddah$predito<-predict(modelo_norm2,dados_Jeddah)
dados_Riyadh$predito<-predict(modelo_norm2,dados_Riyadh)

par(mfrow=c(1,1))
plot(dados_limpos$size,dados_limpos$price,
     xlab='size',ylab='price',cex.lab=1.8,cex.axis=1.5)

names(modelo_norm2$coefficients)
size_graf<-c(0,5000)
lines(x=size_graf,y=modelo_norm2$coefficients['(Intercept)'] + modelo_norm2$coefficients['size']*size_graf,col='red',lwd=2) # Al Khobar
lines(x=size_graf,y=(modelo_norm2$coefficients['(Intercept)'] + modelo_norm2$coefficients['cityAl Riyadh']) + modelo_norm2$coefficients['size']*size_graf,col='green',lwd=2) # Al Riyadh
lines(x=size_graf,y=(modelo_norm2$coefficients['(Intercept)'] + modelo_norm2$coefficients['cityDammam']) + modelo_norm2$coefficients['size']*size_graf,col='blue',lwd=2) # Dammam
lines(x=size_graf,y=(modelo_norm2$coefficients['(Intercept)'] + modelo_norm2$coefficients['cityJeddah']) + modelo_norm2$coefficients['size']*size_graf,col='cyan',lwd=2) # Jeddah

plot(dados_Khobar$size,dados_Khobar$price,
     xlab='size',ylab='price',cex.lab=1.8,cex.axis=1.5,ylim=c(0,200000))
lines(dados_Khobar$size,dados_Khobar$predito,col=2,lwd=2)
dados_Khobar$IC_lwr<-data.frame(predict(modelo_norm2,dados_Khobar,interval = 'confidence'))$lwr
dados_Khobar$IC_upr<-data.frame(predict(modelo_norm2,dados_Khobar,interval = 'confidence'))$upr
dados_Khobar<-dados_Khobar[order(dados_Khobar$size),]
lines(dados_Khobar$size,dados_Khobar$IC_lwr,col=4,lwd=2,lty=3)
lines(dados_Khobar$size,dados_Khobar$IC_upr,col=4,lwd=2,lty=3)

dados_Khobar$IP_lwr<-data.frame(predict(modelo_norm2,dados_Khobar,interval = 'prediction'))$lwr
dados_Khobar$IP_upr<-data.frame(predict(modelo_norm2,dados_Khobar,interval = 'prediction'))$upr
dados_Khobar<-dados_Khobar[order(dados_Khobar$size),]
lines(dados_Khobar$size,dados_Khobar$IP_lwr,col=5,lwd=2,lty=3)
lines(dados_Khobar$size,dados_Khobar$IP_upr,col=5,lwd=2,lty=3)

plot(modelo_norm2)



###################################################################
## Modelo glm - dist. gama, link: \theta = -1/(\mu) é o canônico ##
###################################################################
?glm
modelo_gamma<-glm(price~city+size+city:size,data = dados_limpos,family = Gamma(link = inverse)) # Não consegue convergir.
modelo_gamma<-glm(price~city+size+city:size,data = dados_limpos,family = Gamma(link = inverse),start=rep(5,times=8)) # Não consegue convergir.
modelo_gamma<-glm(price~city+size+city:size,data = dados_limpos,family = Gamma(link = inverse),mustart=1/dados_limpos$price) # Não consegue convergir.
modelo_gamma<-glm(price~city+size+city:size,data = dados_limpos,family = Gamma(link = identity))
modelo_gamma<-glm(price~city+size+city:size,data = dados_limpos,family = Gamma(link = identity),start=rep(5,times=8))
modelo_gamma

summary(modelo_gamma)
summary_modelo_gamma<-summary(modelo_gamma)
matriz_modelo_gamma<-model.matrix(modelo_gamma)
View(matriz_modelo_gamma)

modelo_gamma2<-glm(price~city+size,data = dados_limpos,family = Gamma(link = identity),start=rep(5,times=5)) #
modelo_gamma2
summary_modelo_gamma2<-summary(modelo_gamma2)
summary_modelo_gamma2

AIC(modelo_gamma)
AIC(modelo_gamma2)

# Pseudo-R^2 - Likelihood ratio (Cohen) é usado para regressão logística. Aqui é apenas para ilustração.
(modelo_gamma2$null.deviance-modelo_gamma2$deviance)/modelo_gamma2$null.deviance

step(glm(price~city+size+city:size,data = dados_limpos,family = Gamma(link = identity),start=rep(5,times=8))) # utiliza AIC para seleção de modelos

BIC(modelo_gamma)
BIC(modelo_gamma2)

disper<-summary(modelo_gamma2)$dispersion
preditos_gamma2<-predict.glm(modelo_gamma2,dados_limpos)
resid_pearson_gamma2<-rstandard(modelo_gamma2,type = 'pearson')
plot(preditos_gamma2,resid_pearson_gamma2,xlab=expression(hat(mu)[i]),ylab=expression(r[i]))

dados_Khobar$predito_gamma<-predict.glm(modelo_gamma2,dados_Khobar)
dados_Dammam$predito_gamma<-predict.glm(modelo_gamma2,dados_Dammam)
dados_Jeddah$predito_gamma<-predict.glm(modelo_gamma2,dados_Jeddah)
dados_Riyadh$predito_gamma<-predict.glm(modelo_gamma2,dados_Riyadh)

#plot(modelo_gamma2)

par(mfrow=c(2,1),mar=c(4,4,2,2))
plot(preditos_gamma2,resid_pearson_gamma2,xlab=expression(hat(mu)[i]),ylab=expression(r[i]))
plot(fitted(modelo_norm2), rstandard(modelo_norm2),xlab=expression(hat(mu)[i]),ylab=expression(r[i]))


par(mfrow=c(2,1),mar=c(4,4,2,2))
resid_reduz<-data.frame(ajustado=preditos_gamma2,residuo=resid_pearson_gamma2)
resid_reduz<-subset(resid_reduz,residuo>-5 & residuo<5 & (ajustado<3*10^5) )
plot(resid_reduz$ajustado, resid_reduz$residuo,xlab=expression(hat(mu)[i]),ylab=expression(r[i]))

resid_reduz<-data.frame(ajustado=fitted(modelo_norm2),residuo=rstandard(modelo_norm2))
resid_reduz<-subset(resid_reduz,residuo>-5 & residuo<5 & (ajustado<3*10^5) )
plot(resid_reduz$ajustado, resid_reduz$residuo,xlab=expression(hat(mu)[i]),ylab=expression(r[i]))


## Breve visualização das observações com maior influência
dist_cook_modelo_gamma2<-data.frame(Dist_Cook=cooks.distance(modelo_gamma2),
                              size=dados_limpos$size,
                              price=dados_limpos$price)

dist_cook_modelo_gamma2_ord<-dist_cook_modelo_gamma2[order(dist_cook_modelo_gamma2$Dist_Cook,decreasing = T),]

par(mfrow=c(1,1),mar=c(4,4,2,2))
plot(dados_limpos$size,dados_limpos$price,
     xlab='size',ylab='price',cex.lab=1.8,cex.axis=1.5)

points(dist_cook_modelo_gamma2_ord$size[1:6],
       dist_cook_modelo_gamma2_ord$price[1:6],col='green',lwd=3)

plot(modelo_gamma2$fitted.values,residuals.glm(modelo_gamma2,type = 'pearson'),xlab=expression(hat(mu)),ylab = expression(e))
points(modelo_gamma2$fitted.values[as.numeric(rownames(dist_cook_modelo_gamma2_ord[1:6,]))],
       residuals.glm(modelo_gamma2,type = 'pearson')[as.numeric(rownames(dist_cook_modelo_gamma2_ord[1:6,]))],
       col='green',lwd=3)



## Valores ajustados - modelo normal
par(mfrow=c(1,1))
plot(dados_Khobar$size,dados_Khobar$price,
     xlab='size',ylab='price',cex.lab=1.8,cex.axis=1.5,ylim=c(0,200000))
lines(c(0,max(dados_Khobar$size)),modelo_norm2$coefficients['(Intercept)']+
        modelo_norm2$coefficients['size']*c(0,max(dados_Khobar$size)),col='red',lwd=2)
dados_Khobar$IC_lwr<-data.frame(predict(modelo_norm2,dados_Khobar,interval = 'confidence'))$lwr
dados_Khobar$IC_upr<-data.frame(predict(modelo_norm2,dados_Khobar,interval = 'confidence'))$upr
dados_Khobar<-dados_Khobar[order(dados_Khobar$size),]
lines(dados_Khobar$size,dados_Khobar$IC_lwr,col='blue',lwd=2,lty=3)
lines(dados_Khobar$size,dados_Khobar$IC_upr,col='blue',lwd=2,lty=3)

dados_Khobar$IP_lwr<-data.frame(predict(modelo_norm2,dados_Khobar,interval = 'prediction'))$lwr
dados_Khobar$IP_upr<-data.frame(predict(modelo_norm2,dados_Khobar,interval = 'prediction'))$upr
dados_Khobar<-dados_Khobar[order(dados_Khobar$size),]
lines(dados_Khobar$size,dados_Khobar$IP_lwr,col='green',lwd=2,lty=3)
lines(dados_Khobar$size,dados_Khobar$IP_upr,col='green',lwd=2,lty=3)


## Valores ajustados - modelo gamma
dados_Khobar$predito_gamma<-predict.glm(modelo_gamma2,dados_Khobar,type = 'response')
lines(c(0,max(dados_Khobar$size)),modelo_gamma2$coefficients['(Intercept)']+
        modelo_gamma2$coefficients['size']*c(0,max(dados_Khobar$size)),col='purple',lwd=2)


resid_bruto_norm2<-(resid(modelo_norm2))
resid_bruto_gamma2<-dados_limpos$price - predict.glm(modelo_gamma2,dados_limpos,type = 'response')
par(mfrow=c(2,1),mar=c(4,4,2,2))
hist(resid_bruto_norm2,xlab = 'Resíduo bruto',main="Normal",breaks = seq(min(c(min(resid_bruto_norm2,resid_bruto_gamma2))),
                                                                         max(c(max(resid_bruto_norm2,resid_bruto_gamma2))),
                                                                         length.out=30) )
text(x=10^6,y=1500,labels = paste0('DP(Resíduo) = ',round(sd(resid_bruto_norm2),2) ),cex=1.2)
hist(resid_bruto_gamma2,xlab = 'Resíduo bruto',main="Gama",breaks = seq(min(c(min(resid_bruto_norm2,resid_bruto_gamma2))),
                                                                         max(c(max(resid_bruto_norm2,resid_bruto_gamma2))),
                                                                         length.out=30) )
text(x=10^6,y=1500,labels = paste0('DP(Resíduo) = ',round(sd(resid_bruto_gamma2),2) ),cex=1.2)

#######################
# Ajuste para link: log
modelo_gamma3<-glm(price~city+size,data = dados_limpos,family = Gamma(link = log)) #
modelo_gamma3
summary(modelo_gamma3)

AIC(glm(price~city+size+city:size,data = dados_limpos,family = Gamma(link = log)))
AIC(glm(price~city+size,data = dados_limpos,family = Gamma(link = log)))
AIC(glm(price~city,data = dados_limpos,family = Gamma(link = log)))
AIC(glm(price~size,data = dados_limpos,family = Gamma(link = log)))
AIC(glm(price~1,data = dados_limpos,family = Gamma(link = log)))

modelo_gamma3<-glm(price~(city+size)^2,data = dados_limpos,family = Gamma(link = log))
summary(modelo_gamma3)

par(mfrow=c(2,2),mar=c(4,2,2,2))
resid_reduz<-data.frame(ajustado=preditos_gamma2,residuo=resid_pearson_gamma2)
resid_reduz<-subset(resid_reduz,residuo>-5 & residuo<5 & (ajustado<3*10^5) )
plot(resid_reduz$ajustado, resid_reduz$residuo,xlab=expression(hat(mu)[i]),ylab=expression(r[i]),main='Gama - ident')

resid_reduz<-data.frame(ajustado=fitted(modelo_norm2),residuo=rstandard(modelo_norm2))
resid_reduz<-subset(resid_reduz,residuo>-5 & residuo<5 & (ajustado<3*10^5) )
plot(resid_reduz$ajustado, resid_reduz$residuo,xlab=expression(hat(mu)[i]),ylab=expression(r[i]),main='Normal')

preditos_gamma3<-predict.glm(modelo_gamma3,dados_limpos,type = 'response')
resid_pearson_gamma3<-rstandard(modelo_gamma3,type = 'pearson')
resid_reduz<-data.frame(ajustado=preditos_gamma3,residuo=resid_pearson_gamma3)
resid_reduz<-subset(resid_reduz,residuo>-5 & residuo<5 & (ajustado<3*10^5) )
plot(resid_reduz$ajustado, resid_reduz$residuo,xlab=expression(hat(mu)[i]),ylab=expression(r[i]),main='Gama - log')


dados_Khobar$predito_gamma_log<-predict.glm(modelo_gamma3,dados_Khobar,type = 'response')
# Voltar e graficar os ajustes para Al-Khobar

par(mfrow=c(1,1))
plot(dados_Khobar$size,dados_Khobar$price,
     xlab='size',ylab='price',cex.lab=1.8,cex.axis=1.5,ylim=c(0,200000))
lines(c(0,max(dados_Khobar$size)),modelo_norm2$coefficients['(Intercept)']+
        modelo_norm2$coefficients['size']*c(0,max(dados_Khobar$size)),col='red',lwd=2)

dados_Khobar$predito_gamma<-predict.glm(modelo_gamma2,dados_Khobar,type = 'response')
lines(c(0,max(dados_Khobar$size)),modelo_gamma2$coefficients['(Intercept)']+
        modelo_gamma2$coefficients['size']*c(0,max(dados_Khobar$size)),col='purple',lwd=2)

lines(seq(0,max(dados_Khobar$size,length.out=500)),exp(modelo_gamma3$coefficients['(Intercept)']+
                                                         modelo_gamma3$coefficients['size']*seq(0,max(dados_Khobar$size,length.out=500))),col='cyan',lwd=2, lty=3)


resid_bruto_gamma3<-dados_limpos$price - predict.glm(modelo_gamma3,dados_limpos,type = 'response')
par(mfrow=c(2,2))
hist(resid_bruto_norm2,xlab = 'Resíduo bruto',main="Normal",breaks = seq(min(c(min(resid_bruto_norm2,resid_bruto_gamma2,resid_bruto_gamma3))),
                                                                         max(c(max(resid_bruto_norm2,resid_bruto_gamma2,resid_bruto_gamma3))),
                                                                         length.out=30) )
text(x=10^6,y=1500,labels = paste0('DP(Resíduo) = ',round(sd(resid_bruto_norm2),2) ),cex=1.2)
hist(resid_bruto_gamma2,xlab = 'Resíduo bruto',main="Gama",breaks = seq(min(c(min(resid_bruto_norm2,resid_bruto_gamma2,resid_bruto_gamma3))),
                                                                        max(c(max(resid_bruto_norm2,resid_bruto_gamma2,resid_bruto_gamma3))),
                                                                        length.out=30) )
text(x=10^6,y=1500,labels = paste0('DP(Resíduo) = ',round(sd(resid_bruto_gamma2),2) ),cex=1.2)

hist(resid_bruto_gamma3,xlab = 'Resíduo bruto',main="Gama - log",breaks = seq(min(c(min(resid_bruto_norm2,resid_bruto_gamma2,resid_bruto_gamma3))),
                                                                        max(c(max(resid_bruto_norm2,resid_bruto_gamma2,resid_bruto_gamma3))),
                                                                        length.out=30) )
text(x=10^6,y=1500,labels = paste0('DP(Resíduo) = ',round(sd(resid_bruto_gamma3),2) ),cex=1.2)

summary(resid_bruto_norm2)
summary(resid_bruto_gamma2)
summary(resid_bruto_gamma3)

# Exercício:
# 1. Com estas mesmas variáveis, procurar outras distribuições e funções de ligação que possam promover um ajuste melhor;
# 2. Explorar as demais variáveis do conjunto de dados para propor novos modelos para estimar Price (guiar-se pelos p-valores, AICs e BICs)








######### Heterocedasticidade

# Limpar memoria
rm(list=ls())

# Selecionar diretorio
setwd("/Users/nicholaslepetit/Documents/ECONOMIA/01.2019/Econometria I/Parte 3")
dir()

phones <- read.csv("phone.csv")

# Extraindo os numeros referentes a capacidade da bateria espalhados pela coluna "battery" das observacoes
mAh <- gsub("[^0-9.]", "",  phones$battery)
head(mAh,1000)

# Refinando os resultados ignorando os numeros 
mAh <- as.numeric((substring(mAh,1,4)))

mAh[mAh < 100] <- NA
head(mAh,1000)
summary(mAh)


#install.packages("anytime")
library(anytime)
rdates <- anydate(phones$announced)
head(rdates,1000)
summary(rdates)  


modeldb <- data.frame(rdates, mAh)
summary(modeldb)

# Limpando observacoes que tem algum NA, informacao faltando, em uma variavel e nas duas.
#modeldb_clean <- na.omit(modeldb)
#summary(modeldb_clean$rdates)

#mod1 <- lm(log(modeldb_clean$mAh)~modeldb_clean$rdates)
#summary(mod1)


# b2 <- coef(mod1)[[2]]
# g <- b2*100
# g

# Crescimento de 2,67% por dia/mês???

# Indexando para ano
library(lubridate)
years <- year(rdates)
head(years,1000)
summary(years)

mod_db2 <- data.frame(years, mAh)
mod_db2c <- na.omit(mod_db2)
head(mod_db2c)

mod2 <- lm(log(mAh)~years, data = mod_db2c)
summary(mod2)

b1_mod2 <- coef(mod2)[[1]]
b2_mod2 <- coef(mod2)[[2]]         
g2 <- b2_mod2*100
g2

# Taxa de crescimento de 9,96% ao ano.

#g2/g
# 373.. devia estar em dias
sig2 <- sqrt(deviance(mod2)/df.residual(mod2))
df <- df.residual(mod2)
# nao funcionou rse <- sum(resid(mod2)^2)/df


############# PREVISOES ##############
yhat18 <- exp(b1_mod2 + b2_mod2 * 2018 + (sig2/2))
yhat18

yhat19 <- exp(b1_mod2 + b2_mod2 * 2019 + (sig2/2))
yhat19

# Previsao 2020
yhat20 <- exp(b1_mod2 + b2_mod2 * 2020 + (sig2/2))
yhat20
# Media prevista para 2020 de 4850 mAh

# Previsao 2021
yhat21 <- exp(b1_mod2 + b2_mod2 * 2021 + (sig2/2))
yhat21
# Media prevista para 2021 de 5358 mAh

# Previsao 2022
yhat22 <- exp(b1_mod2 + b2_mod2 * 2022 + (sig2/2))
yhat22
# Media prevista para 2022 de 5919 mAh

# Previsao 2023
yhat23 <- exp(b1_mod2 + b2_mod2 * 2023 + (sig2/2))
yhat23
# Media prevista para 2023 de 6539 mAh

# Previsao 2024
yhat24 <- exp(b1_mod2 + b2_mod2 * 2024 + (sig2/2))
yhat24
# Media prevista para 2024 de 7223 mAh

# Previsao 2025
yhat25 <- exp(b1_mod2 + b2_mod2 * 2025 + (sig2/2))
yhat25
# Media prevista para 2025 de 7979 mAh


############### GRAFICOS E TESTE BP ##################

plot(mod_db2c$years, mod_db2c$mAh, col="green",
     xlab = "Anos",
     ylab = "Capacidade (em mAh")

library(visreg)
visreg(mod2, "years")

library(lmtest)
bptest(mod2)

#### PLOTAR PREVISOES

hats <- c(3224.68, yhat18, yhat19, yhat20, yhat21, yhat22, yhat23, yhat24, yhat25)
hats

# install.packages("tidyverse")
library(tidyverse)
years_avg <- mod_db2c %>% 
  group_by(years) %>% 
  summarise(year_avg = mean(mAh))
hatsyears <- c(2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024, 2025)
hatsdframe <- data.frame(hatsyears, hats)


mainplot_y <- as.numeric(substring(years_avg$year_avg,1,4))
plot(years_avg$years, years_avg$year_avg,
     type = "b", lty = 1, lwd = 3,
     xlab = "Anos",
     ylab = "Média Anual (em mAh)", col = "dodgerblue3",
     main = "Evolução da Capacidade de Baterias de Celulares",
     xlim = range(c(1996:2025)), ylim = range(c(0:8200)), pch=19)
text(years_avg$years, mainplot_y+230, labels=as.character(mainplot_y), cex = 0.55)

lines(hatsyears, hats, type = "b", lty = 3, lwd = 3, pch=19, col = "orangered2")
text(hatsyears, hats+230, label=as.character(substring(hats, 1, 4)), cex = 0.55)

legend("topleft", legend=c("Médias Anuais", "Previsões"),
       col=c("dodgerblue3", "orangered2"), lty = 1:2, cex=0.8)


plot(hatsyears, hats,
     type = "l", lty = 2, lwd = 3,
     xlab = "Anos",
     ylab = "Média Anual (em mAh)", col = "orangered",
     main = "Evolução da Capacidade de Baterias de Celulares",
     xlim = range(hatsyears), ylim = range(hats), pch=19)

# plot(x, f_x, xlim=range(x), ylim=range(f_x), xlab="x", ylab="y", 
# main = "noise-less data",pch=16)
# lines(x[order(x)], f_x[order(x)], xlim=range(x), ylim=range(f_x), pch=16)


library(lmtest)
bptest(mod2)
# Nao podemos rejeitar a homocedasticidade do modelo, pois nao há evidencias estatisticas para tal com um p-valor=0.1245

library(faraway)
vif(mod2)
# Nao há multicolinearidade

freqyear <- table(mod_db2c$years)
freqyear






########## AR
library(tidyverse)
years_avg <- mod_db2c %>% 
  group_by(years) %>% 
  summarise(year_avg = mean(mAh))
years_avg
view(years_avg)

yavg.ts <- ts(years_avg$year_avg, start = c(1996, 1), end = c(2017,1), frequency = 1)
acf(yavg.ts, main="", xlab="Defasagens na variável: médias anuais (em mAh)", ylab="Coeficiente de Correlação")

yavg.ts

library(dynlm)
ar2 <- dynlm(yavg.ts ~ yavg.ts + L(yavg.ts, 1) + L(yavg.ts, 2), data = yavg.ts)
summary(ar2)

# testes
library(lmtest)
bptest(ar2)
# Nao ha evidencias estatisticas para rejeitarmos a hipotese H0 da homocedasticidade.
# Portanto nao podemos afirmar que o modelo é heterocedastico.


armod <- ar(yavg.ts, aic=TRUE, method="ols", order.max = 2)
armod
library(forecast)
fc10 <- data.frame(forecast(armod, 10))
fc10


plot(forecast(armod, 10), main = "", xlab = "Anos", ylab = "Capacidade em mAh")
legend("topleft", legend=c("Estimativas Pontuais", "Intervalo de Confiança de 80%", "Intervalo de Confiança de 95%"),
       col=c("blue3", "skyblue4", "gray80"), lty = 1, cex=0.8)
#### 

plotForecastErrors <- function(forecasterrors)
{
  # make a histogram of the forecast errors:
  mybinsize <- IQR(forecasterrors)/4
  mysd   <- sd(forecasterrors)
  mymin  <- min(forecasterrors) - mysd*5
  mymax  <- max(forecasterrors) + mysd*3
  # generate normally distributed data with mean 0 and standard deviation mysd
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  # make a red histogram of the forecast errors, with the normally distributed data overlaid:
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
  # freq=FALSE ensures the area under the histogram = 1
  # generate normally distributed data with mean 0 and standard deviation mysd
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  # plot the normal curve as a blue line on top of the histogram of forecast errors:
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
  
}

plotForecastErrors(ar2$residuals)


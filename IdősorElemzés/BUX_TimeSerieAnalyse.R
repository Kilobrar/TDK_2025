setwd("C:/Kornel/Egyetem/TDK/IdősorElemzés")


library(ggplot2)
library(tseries)
library(rugarch)
library(dplyr)
library(tidyr)


bux = read.csv("bux_d.csv", sep=",")
str(bux)
bux$Date <- as.Date(bux$Date)


ggplot(bux, aes(x=Date, y=Close)) + geom_line()

bux$lhozam <- c(NA, diff(log(bux$Close)))

ggplot(bux, aes(x=Date, y=lhozam)) + geom_line()

adf.test(bux$Close[-1])
# p-value = 0.9456
adf.test(bux$lhozam[-1])
# p-value = 0.01

acf(bux$lhozam[-1])
pacf(bux$lhozam[-1])

lmtest::bgtest(bux$lhozam[-1] ~ 1,
               order = 30)
# p-value = 3.514e-07

en_armam <- arima(bux$lhozam[-1],
                  order = c(2,0,3))
lmtest::coeftest(en_armam)

AIC(en_armam)

# ARMA(3,6) --> AIC: -10073.95
# ARMA(3,3) --> AIC: -10074.32
# ARMA(2,3) --> AIC: -10075.04 WINNER!!!!
# ARMA(2,2) --> AIC: -10060.75
# ARMA(1,1) --> AIC: -10059.59

lmtest::bgtest(en_armam$residuals ~ 1,
               order = 30)
# p-value = 0.004807


garch_speci <- ugarchspec(
  mean.model = list(armaOrder=c(0,2)),
  variance.model = list(model="sGARCH",
                        garchOrder = c(1,1)),
  distribution.model = "sstd")

garch_fit <- ugarchfit(spec = garch_speci,
                       data = bux$lhozam[-1])

garch_fit

# AIC - ARMA(2,3) + GARCH(1,1) -6.2270
# BIC - ARMA(2,3) + GARCH(1,1) -6.1921

# AIC - ARMA(2,2) + GARCH(1,1) -6.2287
# BIC - ARMA(2,2) + GARCH(1,1) -6.1970

# AIC - ARMA(0,2) + GARCH(1,1) -6.2295 WINNER
# BIC - ARMA(0,2) + GARCH(1,1) -6.2041


################################################################# Gépi tanulás alapú
# Hangulatindex beolvasás
sentFin = read.csv("agg_sentiment_score_finbert.csv", sep="|")
sentFin$date <- as.Date(sentFin$date)

### Nem kereskedési napok kiszűrése
sent_filtered1 <- sentFin %>% filter(date %in% bux$Date)


### Nem kereskedési napok aggregálása a legutóbbi kereskedési naphoz
# Az eddigi kereskedési napok azonosítása
sent_filtered2 <- sentFin %>%
  mutate(TradingDate = ifelse(date %in% bux$Date, date, NA)) %>%
  fill(TradingDate, .direction = "down")  # Legutóbbi elérhető kereskedési naphoz társítjuk
sent_filtered2$TradingDate <- as.Date(sent_filtered2$TradingDate)

# Aggregálás az új TradingDate szerint (pl. átlagolás vagy összeadás)
sent_filtered2 <- sent_filtered2 %>%
  group_by(TradingDate) %>%
  summarise(AggSentiment = mean(mean, na.rm = TRUE),
            SentimentSD = mean(std, na.rm = TRUE))

ggplot(sent_filtered2, aes(x=TradingDate, y=AggSentiment)) + geom_line()
adf.test(sent_filtered2$AggSentiment)
# p-value smaller than printed p-value

sent_filtered2$sentChange <- c(NA, diff(sent_filtered2$AggSentiment))
sent_filtered2$TradingDate <- as.Date(sent_filtered2$TradingDate)

sent_filtered1$sentChange <- c(NA, diff(sent_filtered1$mean))

  
################ FinBERT, nem kereskedési napok is
garch_speci_1 <- ugarchspec(
  mean.model = list(armaOrder=c(0,2), external.regressors = as.matrix(sent_filtered2[2:(nrow(sent_filtered2)-1), c('SentimentSD')])),
  variance.model = list(model="sGARCH",
                        garchOrder = c(1,2), external.regressors = as.matrix(sent_filtered2[2:(nrow(sent_filtered2)-1), c('AggSentiment')])),
  distribution.model = "sstd")

garch_fit_1 <- ugarchfit(spec = garch_speci_1,
                       data = bux$lhozam[c(-1, -2)])

garch_fit_1
colnames(sent_filtered2)


garch_speci_2 <- ugarchspec(
  mean.model = list(armaOrder=c(0,2)),
  variance.model = list(model="sGARCH",
                        garchOrder = c(1,1), external.regressors = as.matrix(sent_filtered2[2:(nrow(sent_filtered2)-1), c('AggSentiment')])),
  distribution.model = "sstd")

garch_fit_2 <- ugarchfit(spec = garch_speci_2,
                       data = bux$lhozam[c(-1, -2)])

garch_fit_2





ugarch_fit_rollos = ugarchroll(spec = garch_speci_2,data = bux$lhozam[c(-1,-2)], refit.every = 250, refit.window = 'moving',
                        n.start = 800)

ugarch_fit_rollos = as.data.frame(ugarch_fit_rollos)

#hiba az átlagban

sqrt(mean((ugarch_fit_rollos$Mu-ugarch_fit_rollos$Realized)^2))

# hiba a szórásban
ugarch_fit_rollos$valos_volat <- sqrt(mean((ugarch_fit_rollos$Realized-mean(ugarch_fit_rollos$Realized))^2))

sqrt(mean((ugarch_fit_rollos$Sigma-ugarch_fit_rollos$valos_volat)^2))




############## FinBERT, nem kereskedési napok off
garch_speci <- ugarchspec(
  mean.model = list(armaOrder=c(0,2)),
  variance.model = list(model="sGARCH",
                        garchOrder = c(1,1), external.regressors = as.matrix(sent_filtered1[2:(nrow(sent_filtered1)-1), c('std')])),
  distribution.model = "sstd")

garch_fit <- ugarchfit(spec = garch_speci,
                       data = bux$lhozam[c(-1, -2)])
garch_fit



garch_speci <- ugarchspec(
  mean.model = list(armaOrder=c(0,2)),
  variance.model = list(model="sGARCH",
                        garchOrder = c(1,1), external.regressors = as.matrix(sent_filtered1[2:(nrow(sent_filtered1)-1), c('mean')])),
  distribution.model = "sstd")

garch_fit <- ugarchfit(spec = garch_speci,
                       data = bux$lhozam[c(-1, -2)])
garch_fit

ugarch_fit_rollos = ugarchroll(spec = garch_speci,data = bux$lhozam[c(-1,-2)], refit.every = 250, refit.window = 'moving',
                               n.start = 800)

ugarch_fit_rollos = as.data.frame(ugarch_fit_rollos)

#hiba az átlagban

sqrt(mean((ugarch_fit_rollos$Mu-ugarch_fit_rollos$Realized)^2))

# hiba a szórásban
ugarch_fit_rollos$valos_volat <- sqrt(mean((ugarch_fit_rollos$Realized-mean(ugarch_fit_rollos$Realized))^2))

sqrt(mean((ugarch_fit_rollos$Sigma-ugarch_fit_rollos$valos_volat)^2))

#--------------------------------------------------------------------------------------------------

################################ Szótári alapú

# Hangulatindex beolvasás
sentLMD = read.csv("agg_sentiment_score_LMD.csv", sep="|")
sentLMD$date <- as.Date(sentLMD$date)

### Nem kereskedési napok kiszűrése
sent_LMD_filtered1 <- sentLMD %>% filter(date %in% bux$Date)


### Nem kereskedési napok aggregálása a legutóbbi kereskedési naphoz
# Az eddigi kereskedési napok azonosítása
sent_LMD_filtered2 <- sentLMD %>%
  mutate(TradingDate = ifelse(date %in% bux$Date, date, NA)) %>%
  fill(TradingDate, .direction = "down")  # Legutóbbi elérhető kereskedési naphoz társítjuk
sent_LMD_filtered2$TradingDate <- as.Date(sent_LMD_filtered2$TradingDate)

# Aggregálás az új TradingDate szerint (pl. átlagolás vagy összeadás)
sent_LMD_filtered2 <- sent_LMD_filtered2 %>%
  group_by(TradingDate) %>%
  summarise(AggSentiment = mean(mean, na.rm = TRUE),
            SentimentSD = mean(std, na.rm = TRUE))

ggplot(sent_LMD_filtered2, aes(x=TradingDate, y=AggSentiment)) + geom_line()
adf.test(sent_LMD_filtered2$AggSentiment)
# p-value smaller than printed p-value

sent_LMD_filtered2$sentChange <- c(NA, diff(sent_LMD_filtered2$AggSentiment))
sent_LMD_filtered2$TradingDate <- as.Date(sent_LMD_filtered2$TradingDate)

sent_LMD_filtered1$sentChange <- c(NA, diff(sent_LMD_filtered1$mean))

########Szótári, nem kereskedési napok is

garch_speci_1 <- ugarchspec(
  mean.model = list(armaOrder=c(0,1), external.regressors = as.matrix(sent_LMD_filtered2[2:(nrow(sent_LMD_filtered2)-1), c('AggSentiment', 'SentimentSD', 'sentChange')])),
  variance.model = list(model="sGARCH",
                        garchOrder = c(1,1), external.regressors = as.matrix(sent_LMD_filtered2[2:(nrow(sent_LMD_filtered2)-1), c('AggSentiment', 'SentimentSD', 'sentChange')])),
  distribution.model = "sstd")

garch_speci_1 <- ugarchspec(
  mean.model = list(armaOrder=c(0,2)),
  variance.model = list(model="sGARCH",
                        garchOrder = c(1,1), external.regressors = as.matrix(sent_LMD_filtered2[2:(nrow(sent_LMD_filtered2)-1), c('AggSentiment')])),
  distribution.model = "sstd")

garch_fit <- ugarchfit(spec = garch_speci_1,
                       data = bux$lhozam[c(-1, -2)])
garch_fit

ugarch_fit_rollos = ugarchroll(spec = garch_speci_1,data = bux$lhozam[c(-1,-2)], refit.every = 250, refit.window = 'moving',
                               n.start = 800)

ugarch_fit_rollos = as.data.frame(ugarch_fit_rollos)

#hiba az átlagban

sqrt(mean((ugarch_fit_rollos$Mu-ugarch_fit_rollos$Realized)^2))

# hiba a szórásban
ugarch_fit_rollos$valos_volat <- sqrt(mean((ugarch_fit_rollos$Realized-mean(ugarch_fit_rollos$Realized))^2))

sqrt(mean((ugarch_fit_rollos$Sigma-ugarch_fit_rollos$valos_volat)^2))



########Szótári, nem kereskedési napok off

garch_speci_1 <- ugarchspec(
  mean.model = list(armaOrder=c(0,2), external.regressors = as.matrix(sent_LMD_filtered1[2:(nrow(sent_LMD_filtered1)-1), c('mean', 'std', 'sentChange')])),
  variance.model = list(model="sGARCH",
                        garchOrder = c(1,1), external.regressors = as.matrix(sent_LMD_filtered1[2:(nrow(sent_LMD_filtered1)-1), c('mean', 'std', 'sentChange')])),
  distribution.model = "sstd")


garch_speci_1 <- ugarchspec(
  mean.model = list(armaOrder=c(0,2)),
  variance.model = list(model="sGARCH",
                        garchOrder = c(1,1), external.regressors = as.matrix(sent_LMD_filtered1[2:(nrow(sent_LMD_filtered1)-1), c('mean')])),
  distribution.model = "sstd")



garch_fit <- ugarchfit(spec = garch_speci_1,
                       data = bux$lhozam[c(-1, -2)])
garch_fit

ugarch_fit_rollos = ugarchroll(spec = garch_speci_1,data = bux$lhozam[c(-1,-2)], refit.every = 250, refit.window = 'moving',
                               n.start = 800)

ugarch_fit_rollos = as.data.frame(ugarch_fit_rollos)

#hiba az átlagban

sqrt(mean((ugarch_fit_rollos$Mu-ugarch_fit_rollos$Realized)^2))

# hiba a szórásban
ugarch_fit_rollos$valos_volat <- sqrt(mean((ugarch_fit_rollos$Realized-mean(ugarch_fit_rollos$Realized))^2))

sqrt(mean((ugarch_fit_rollos$Sigma-ugarch_fit_rollos$valos_volat)^2))

#--------------------------------------------------------------------------------------------------

############################ Natúr, nincs hangulatindex
garch_speci <- ugarchspec(
  mean.model = list(armaOrder=c(0,2)),
  variance.model = list(model="sGARCH",
                        garchOrder = c(1,1)),
  distribution.model = "sstd")

garch_fit <- ugarchfit(spec = garch_speci,
                       data = bux$lhozam[c(-1, -2)])
garch_fit

ugarch_fit_rollos = ugarchroll(spec = garch_speci,data = bux$lhozam[c(-1,-2)], refit.every = 250, refit.window = 'moving',
                               n.start = 800)

ugarch_fit_rollos = as.data.frame(ugarch_fit_rollos)

#hiba az átlagban

sqrt(mean((ugarch_fit_rollos$Mu-ugarch_fit_rollos$Realized)^2))

# hiba a szórásban
ugarch_fit_rollos$valos_volat <- sqrt(mean((ugarch_fit_rollos$Realized-mean(ugarch_fit_rollos$Realized))^2))

sqrt(mean((ugarch_fit_rollos$Sigma-ugarch_fit_rollos$valos_volat)^2))



library(dplyr)

setwd("C:/Users/123/Desktop/Kwang")
read.csv('2019_data.csv') -> data2019
read.csv('2020_data.csv') -> data2020


#결측치 제거
data2019$가구주_종사상지위코드 <- ifelse(is.na(data2019$가구주_종사상지위코드), 7, data2019$가구주_종사상지위코드)
table(is.na(data2019))

data2020$가구주_종사상지위코드 <- ifelse(is.na(data2020$가구주_종사상지위코드), 7, data2020$가구주_종사상지위코드)
table(is.na(data2020))

#처분가능소득 파생변수 추가
attach(data2019)

data2019 <- data2019 %>%
  mutate(surplus = 소득 - (소비지출2 + 소비지출3 + 소비지출4 + 소비지출5 + 소비지출6 +
                               소비지출7 + 소비지출8 + 소비지출9 + 소비지출10 + 소비지출11 + 소비지출12 +
                               소비지출13 + 
                               비소비지출1 + 비소비지출2 + 비소비지출3 + 비소비지출4 + 
                               비소비지출5 + 비소비지출6 + 비소비지출7))
data2019 <- data2019 %>%
  mutate(surplus0 = ifelse(surplus >= 0, 1, 0))

attach(data2020)

data2020 <- data2020 %>%
  mutate(surplus = 소득 - (소비지출2 + 소비지출3 + 소비지출4 + 소비지출5 + 소비지출6 +
                               소비지출7 + 소비지출8 + 소비지출9 + 소비지출10 + 소비지출11 + 소비지출12 +
                               소비지출13 + 
                               비소비지출1 + 비소비지출2 + 비소비지출3 + 비소비지출4 + 
                               비소비지출5 + 비소비지출6 + 비소비지출7))
data2020 <- data2020 %>%
  mutate(surplus0 = ifelse(surplus >= 0, 1, 0))
detach(data2020)

#상관계수 확인
install.packages('corrplot')
library(corrplot)
corrplot(cor(data2019[19:43]))
corrplot(cor(data2020[19:43]))



#1차 분석 회귀모형
model2019 <- lm(surplus ~ factor(가구주_성별코드)+
                  가구주_연령+factor(가구주_학력코드)+factor(가구주_취업여부)+
                  factor(가구주_10차산업분류코드)+factor(가구주_종사상지위코드)+
                  factor(입주형태코드)+factor(주택소유유무)+
                  소득_경상1+소득_경상2+소득_경상3+소득_경상4+
                  소득_비경상1+
                  소비지출2+소비지출3+소비지출4+소비지출5+소비지출6+소비지출7+
                  소비지출8+소비지출9+소비지출10+소비지출11+소비지출12+소비지출13+
                  비소비지출2+
                  비소비지출5+비소비지출6+비소비지출7+factor(도시여부),
                data = data2019)
summary(model2019)
par(mfrow = c(2,2))
plot(model2019)

model2019_adj <- lm(surplus ~ 가구주_연령+factor(가구주_학력코드)+
                      factor(가구주_10차산업분류코드)+factor(가구주_종사상지위코드)+
                      소득_경상1+소득_경상2+소득_경상3+소득_경상4+
                      소득_비경상1+
                      소비지출2+소비지출3+소비지출4+소비지출5+소비지출6+소비지출7+
                      소비지출8+소비지출9+소비지출10+소비지출11+소비지출12+소비지출13+
                      비소비지출2+
                      비소비지출5+비소비지출6+비소비지출7+factor(도시여부),
                         data = data2019)
summary(model2019_adj)
par(mfrow = c(2,2))
plot(model2019_adjusted)

residuals2019 <- resid(model2019_adj)
yhat2019 <- predict(model2019_adj, interval = 'none')
plot(x=yhat2019, y = residuals2019)
plot(resid(model2019_adj))


step(lm(surplus~1,data2019), 
     scope = list(lower=~1, upper=~가구주_연령+factor(가구주_학력코드)+
                    factor(가구주_10차산업분류코드)+factor(가구주_종사상지위코드)+
                    소득_경상1+소득_경상2+소득_경상3+소득_경상4+
                    소득_비경상1+
                    소비지출2+소비지출3+소비지출4+소비지출5+소비지출6+소비지출7+
                    소비지출8+소비지출9+소비지출10+소비지출11+소비지출12+소비지출13+
                    비소비지출2+
                    비소비지출5+비소비지출6+비소비지출7+factor(도시여부)), 
     direction="forward")


model2020 <- lm(surplus ~ factor(가구주_학력코드)+factor(가구주_취업여부)+
                  factor(가구주_10차산업분류코드)+
                  소득_경상1+소득_경상2+소득_경상3+소득_경상4+
                  소득_비경상1+
                  소비지출2+소비지출3+소비지출4+소비지출5+소비지출6+소비지출7+
                  소비지출8+소비지출9+소비지출10+소비지출11+소비지출13+
                  비소비지출1+비소비지출2+비소비지출1+
                  비소비지출5+비소비지출6+비소비지출7+factor(도시여부),
                data = data2020)

summary(model2020)
par(mfrow = c(2,2))
plot(model2020)

model2020_adj <- lm(surplus ~ factor(가구주_학력코드)+factor(가구주_취업여부)+
                  factor(가구주_10차산업분류코드)+
                  소득_경상1+소득_경상2+소득_경상3+소득_경상4+
                  소득_비경상1+
                  소비지출2+소비지출3+소비지출4+소비지출5+소비지출6+소비지출7+
                  소비지출8+소비지출9+소비지출10+소비지출11+소비지출13+
                  비소비지출1+비소비지출2+비소비지출1+
                  비소비지출5+비소비지출6+비소비지출7+factor(도시여부),
                data = data2020)
summary(model2020_adj)
par(mfrow = c(2,2))
plot(model2020_adj)

par(mfrow = c(1,1))
residuals <- resid(model2020_adj)
yhat <- predict(model2020_adj, interval = 'none')
plot(x=yhat, y = residuals)
plot(resid(model2020_adj))

library(car)
AIC(model2019)


#resettest-------------------------------------------
install.packages('lmtest')
library(lmtest)
resettest(model2019_adj)
resettest(model2020_adj)
hist(resid(model2019_adj))
hist(resid(model2020_adj))
#---------자크베라 검정
install.packages('tseries')
library(tseries)
jarque.bera.test(resid(model2019_adj))
#-> 오차항이 정규분포를 따르지 않음
jarque.bera.test(resid(model2020_adj))

#VIF검정
install.packages('HH')
library(HH)


#----->다중공선성

#이분산
bptest(model2019_adj)
bptest(model2020_adj)

#더빈왓슨
library(car)

dwt(model2019_adj)
dwt(model2020_adj)

#로지스틱 회귀모형
log_model2019 <- glm(surplus0 ~ 0 + factor(가구주_성별코드)+
                       가구주_연령+factor(가구주_학력코드)+factor(가구주_취업여부)+
                       factor(가구주_10차산업분류코드)+factor(가구주_종사상지위코드)+
                       factor(입주형태코드)+factor(주택소유유무)+
                       소득_경상1+소득_경상2+소득_경상3+소득_경상4+
                       소득_비경상1+
                       소비지출2+소비지출3+소비지출4+소비지출5+소비지출6+소비지출7+
                       소비지출8+소비지출9+소비지출10+소비지출11+소비지출12+소비지출13+
                       비소비지출1+비소비지출2+비소비지출3+비소비지출4+
                       비소비지출5+비소비지출6+비소비지출7+factor(도시여부),
                     data = data2019)
summary(log_model2019)

log_model2019_adj <- glm(surplus0 ~ 0 + 
                       가구주_연령+
                       factor(가구주_10차산업분류코드)+factor(가구주_종사상지위코드)+
                       factor(입주형태코드)+factor(주택소유유무)+
                       소득_경상1+소득_경상2+소득_경상3+소득_경상4+
                       소득_비경상1+
                       소비지출2+소비지출3+소비지출4+소비지출5+소비지출6+소비지출7+
                       소비지출8+소비지출9+소비지출10+소비지출11+소비지출12+소비지출13+
                       비소비지출1+비소비지출2+비소비지출3+비소비지출4+
                       비소비지출5+비소비지출6+비소비지출7,
                     data = data2019)
summary(log_model2019_adj)



log_model2020 <- glm(surplus0 ~ 0 + factor(가구주_성별코드)+
                       가구주_연령+factor(가구주_학력코드)+factor(가구주_취업여부)+
                       factor(가구주_10차산업분류코드)+factor(가구주_종사상지위코드)+
                       factor(입주형태코드)+factor(주택소유유무)+
                       소득_경상1+소득_경상2+소득_경상3+소득_경상4+
                       소득_비경상1+
                       소비지출2+소비지출3+소비지출4+소비지출5+소비지출6+소비지출7+
                       소비지출8+소비지출9+소비지출10+소비지출11+소비지출12+소비지출13+
                       비소비지출1+비소비지출2+비소비지출3+비소비지출4+
                       비소비지출5+비소비지출6+비소비지출7+factor(도시여부),
                     data = data2020)
summary(log_model2020)
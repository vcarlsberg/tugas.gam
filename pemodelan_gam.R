library(tidyverse)
library(gam)
library(mgcv)
library(plyr)
library(lmtest)
library(Metrics)

#load data
mobil<-read.csv("file.csv")
summary(mobil)
attach(mobil)

#plotting sebaran data
modelmobil<-plyr::count(mobil,'model')
modelmobil<-modelmobil[order(-modelmobil$freq),]
barplot(head(modelmobil$freq,n = 3),
        names.arg = head(modelmobil$model,n=3),
        horiz=TRUE,
        xlab="Model",
        cex.names = 1.5,
        title="cl",
        main="3 Model Mobil dengan Jumlah Iklan Terbanyak di mobil123.com"
        )
detach(mobil)

#filtering dataset yang variannya Avanza, Jazz & Innova dan filter outlier yang mileage nya <10000000
mobil<-mobil %>% filter(model %in% 
                          c("Avanza","Jazz","Kijang Innova")
                        )
mobil<-mobil %>% filter(mileage < 1000000 )

attach(mobil)

#Uji linearitas
resettest(harga ~ mileage , power=3, type="regressor")
resettest(harga ~ tahun , power=3, type="regressor")
resettest(harga ~ mesin , power=3, type="regressor")

#plotting x,y
plot(mileage,harga)
plot(tahun,harga)
plot(mesin,harga)

#pemodelan GAM
model.gam<-gam(harga~s(tahun)+(mileage)+mesin+variant)
summary(model.gam)

#pemodelan LM
model.lm<-lm(harga~mesin+(mileage)+(tahun)
             +(variant))
summary(model.lm)
summary(model.lm$residuals)

#residual analysis
summary(model.gam$residuals)
hist(model.gam$residuals) 
qqnorm(model.gam$residuals) 
qqline(model.gam$residuals)

plot(model.gam$fitted.values,model.gam$residuals) 
plot(c(1:length(mesin)),model.gam$residuals)

#predict on Lamudi Dataset
data.prediksi<-data.frame(mesin=1500,mileage=57000,tahun=2015,variant="Veloz")
hasil.prediksi<-predict(model.gam,data.prediksi)
hasil.prediksi

mape(c(96000000,235000000,153000000,215000000),c(87079934,242554062,146106625,218184780))

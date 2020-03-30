library(tidyverse)
library(gam)
library(mgcv)
library(plyr)
library(lmtest)

mobil<-read.csv("file.csv")
summary(mobil)
attach(mobil)

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

mobil<-mobil %>% filter(
  model %in% c("Jazz","Avanza","Kijang Innova")
)
attach(mobil)

resettest(harga ~ mileage , power=3, type="regressor")
resettest(harga ~ tahun , power=3, type="regressor")
resettest(harga ~ mesin , power=3, type="regressor")

plot(mileage,harga)
plot(tahun,harga)
plot(mesin,harga)

model.gam<-gam(harga~mesin+s(mileage)+s(tahun)
               +factor(variant)
               )
summary(model.gam)

mean(model.gam$residuals)
summary(model.gam$residuals)

hist(model.gam$residuals) 
qqnorm(model.gam$residuals) 
qqline(model.gam$residuals)

plot(model.gam$fitted.values,model.gam$residuals) 
plot(c(1:length(mesin)),model.gam$residuals)

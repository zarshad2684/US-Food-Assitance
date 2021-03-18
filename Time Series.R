setwd("C:/Users/robyn/Documents/Krannert/Module 3/Advanced BA")

library(dplyr)
library(readxl)
library(forecast)
library(ggplot2)
library(DescTools)
library(scales)
library(zoo)

df<-read_xlsx("SalesData.xlsx",sheet = "Orders",
             col_types = c("numeric","guess","date",
                           "date",rep("text",13),
                           rep("numeric",4),"text"))
names(df)<-make.names(names(df),unique = TRUE)
df$Order.Date.MY<-format(df$Order.Date,"%Y-%m")
############################################################
Sales<-df%>%
  group_by(Order.Date.MY)%>%
  summarise(Sales.Per.Month = sum(Sales))
Sales.ts<-ts(Sales$Sales.Per.Month,start=c(2014,01),end = c(2017,12),
              frequency = 12)


month<-c("Jan 2014","Feb",'Mar','Apr',
         'May','Jun','Jul','Aug',
         "Sep",'Oct','Nov','Dec',
         "Jan 2015","Feb",'Mar','Apr',
         'May','Jun','Jul','Aug',
         "Sep",'Oct','Nov','Dec',
         "Jan 2016","Feb",'Mar','Apr',
         'May','Jun','Jul','Aug',
         "Sep",'Oct','Nov','Dec',
         "Jan 2017","Feb",'Mar','Apr',
         'May','Jun','Jul','Aug',
         "Sep",'Oct','Nov','Dec')
ggplot()+
  geom_line(data=Sales[1:42,],aes(Order.Date.MY,Sales.Per.Month,
                            group=1),size=2)+
  labs(title = "Total Monthly Sales",x="Month",
       y="Sales")+
  scale_x_discrete(labels=month[1:42])+
  scale_y_continuous(labels=scales::dollar_format())+
  theme(axis.text.x = element_text(angle = 90,size=15,
                                   vjust = 0.5,hjust=0.95),
        text=element_text(size = 25),
        plot.title=element_text(hjust = 0.5))

train.ts<-window(Sales.ts,end = c(2017,6))
valid.ts<-window(Sales.ts, start = c(2017,7))

plot(train.ts)

Acf(train.ts,lag.max=12)
# 12 month seasonality
diff(train.ts,lag = 12)%>%Acf(lag.max = 12)
diff(train.ts,lag = 12)%>%plot()

ts.lm<-tslm(train.ts~trend+season)
summary(ts.lm)


plot(ts.lm$residuals)
Acf(ts.lm$residuals,lag.max = 12)


fit <- data.frame(Fit=as.numeric(ts.lm$fitted.values), 
                   time=as.numeric(time(train.ts)),
                  Original = as.numeric(train.ts))
pred<-data.frame(Pred = forecast(ts.lm,h=6)$mean,
                 time = as.numeric(time(valid.ts)),
                 lower = forecast(ts.lm,h=6)$lower,
                 upper = forecast(ts.lm,h=6)$upper,
                 Original = as.numeric(valid.ts))

#Exponential Smoothening 
nValid <- 6

#Holt winters exponential smoothing
hwin <- ets(train.ts,model = "MAA")

#create predictions
hwin.pred<- forecast(hwin,h=nValid,level=0)

#plot the series
plot(hwin.pred,bty="l",xaxt="n",flty=2,ylim=c(7000,120000),ylab = "Sales",
     xlab = "Time",xlim=c(2014,2018),main = "Exponential Smoothing")
axis(1,at=seq(2014,2017,1),labels = format(seq(2014,2017,1)))
lines(hwin.pred$fitted,lwd=2,col="blue")
lines(valid.ts)

MAPE(hwin.pred$fitted,train.ts)

MAPE(hwin.pred$mean,valid.ts)
#######################################

fit <- data.frame(Fit=as.numeric(hwin$fitted), 
                  time=as.numeric(time(train.ts)),
                  Original = as.numeric(train.ts))
pred<-data.frame(Pred = forecast(hwin,h=6)$mean,
                 time = as.numeric(time(valid.ts)),
                 lower = forecast(hwin,h=6)$lower,
                 upper = forecast(hwin,h=6)$upper,
                 Original = as.numeric(valid.ts))
hwinF<-ets(Sales.ts,model = "MAA")
future<-data.frame(time = c(2018.000,2018.083,2018.167),
                   future = forecast())

month<-c("Jan 2014","Feb",'Mar','Apr',
         'May','Jun','Jul','Aug',
         "Sep",'Oct','Nov','Dec',
         "Jan 2015","Feb",'Mar','Apr',
         'May','Jun','Jul','Aug',
         "Sep",'Oct','Nov','Dec',
         "Jan 2016","Feb",'Mar','Apr',
         'May','Jun','Jul','Aug',
         "Sep",'Oct','Nov','Dec',
         "Jan 2017","Feb",'Mar','Apr',
         'May','Jun','Jul','Aug',
         "Sep",'Oct','Nov','Dec',
         "Jan 2018",'Feb','Mar')
pp<-"darkorchid1"
cc<-c("Actual"="black","Predicted"=pp)
ggplot()+
  geom_line(aes(x=time, y=Original,color="Actual"), 
            data=fit,size=2,alpha=0.6)+
  geom_line(data = pred,aes(x=time,y=Original,
                            color="Actual"),size=2,
            alpha=0.6)+
  geom_ribbon(data=pred,aes(x=time,ymin=lower.80.,
                            ymax=upper.80.),
              fill=pp,alpha=0.35)+
  geom_ribbon(data=pred,aes(x=time,ymin=lower.95.,
                            ymax=upper.95.),
              fill=pp,alpha=0.2)+
  geom_line(data = fit, aes(x=time,y=Fit,
                            color="Predicted"),
            size=2)+
  geom_line(data=pred,aes(x=time,y=Pred,
                          color="Predicted"),size=2)+
  labs(title = "Total Monthly Sales",x="Month",
       y="Sales")+
  scale_x_continuous(breaks=seq(from=2014,to=2017.917,length.out=48), 
                  labels=month)+
  scale_y_continuous(labels=scales::dollar_format())+
  scale_color_manual(values = cc)+
  theme(axis.text.x = element_text(angle = 90,size=15,
                                   vjust = 0.5,hjust=0.95),
        text=element_text(size = 25),
        plot.title=element_text(hjust = 0.5))


# What can we say?
# Trend 468.6
# Seasonality yearly

pred<-forecast(ts.lm,h=6)


MAPE(ts.lm$fitted.values,train.ts)
MAPE(pred$mean,valid.ts)

plot(train.ts, ylab='Sales',bty='l',xlim=c(2014,2018),
     ylim=c(7000,120000),main="Monthly Sales")
lines(ts.lm$fitted.values,col = 'blue')
lines(pred$mean, col = 'red')
lines(valid.ts)
lines(c(2017.44,2017.44),c(0,120000))
axis(1,at = seq(2014,2018,.25))

Sales$Fitted<-c(ts.lm$fitted.values,pred$mean)

ggplot(Sales)+
  geom_line(aes(Order.Date.MY,Sales.Per.Month,group=1))+
  geom_line(aes(Order.Date.MY,Fitted,group=1),color="red")+
  theme(axis.text.x = element_text(angle = 90))

ts.arima<-Arima(train.ts,order=c(0,1,0),seasonal=c(0,1,0))
summary(ts.arima)

plot(ts.arima$residuals)
Acf(ts.arima$residuals,lag.max = 12)

################# My own auto.arima

PDQ<-data.frame()
range<-0:2
for(p in range){
  for(d in range){
    for(q in range){
      for(P in range){
        for(D in range){
          for(Q in range){
            try({
            ts.arima<-Arima(train.ts,order = c(p,d,q),
                            seasonal = c(P,D,Q))
            pred<-forecast(ts.arima,h=6)
            m1<-MAPE(ts.arima$fitted,train.ts)
            m2<-MAPE(pred$mean,valid.ts)
            PDQ<-rbind(PDQ,cbind(p,d,q,P,D,Q,m1,m2))
            })
            print(c(as.character(p),as.character(d),as.character(q),
                  as.character(P),as.character(D),as.character(Q)))
          }
        }
      }
    }
  }
}
names(PDQ)<-c("p",'d','q','P','D','Q','MAPE_TR','MAPE_TE')
PDQ<-PDQ%>%arrange(MAPE_TR)
ts.arima<-auto.arima(train.ts)
summary(ts.arima)
Acf(ts.arima$residuals,lag.max = 12)

pred<-forecast(ts.arima,h=6)

plot(train.ts, ylab='Sales',bty='l',xlim=c(2014,2018),
     ylim=c(7000,120000))
lines(ts.arima$fitted,col = 'blue')
lines(pred$mean, col = 'blue')
lines(valid.ts)
lines(c(2016.96,2016.96),c(0,120000))

library(DescTools)
MAPE(ts.arima$fitted,train.ts)
MAPE(pred$mean,valid.ts)


#######################################
#######################################
#Category
Category<-df%>%
  group_by(Order.Date.MY,Category)%>%
  summarise(Sales.Per.Month = sum(Sales))

ggplot(Category[1:126,], aes(Order.Date.MY,Sales.Per.Month,
                     group = Category,color=Category))+
  geom_line(size=2)+
  scale_x_discrete(labels=month[1:42])+
  labs(title = "Monthly Sales by Category",
       x = "Month",y="Sales")+
  scale_y_continuous(labels=scales::dollar_format())+
  theme(axis.text.x = element_text(angle = 90,size=15,
                                   vjust = 0.5,hjust=0.95),
        text=element_text(size = 25),
        plot.title=element_text(hjust = 0.5))

p<-ggplot(Category[1:126,], aes(Order.Date.MY,Sales.Per.Month,
                     group = Category,color=Category))+
  geom_line(size=2)+
  facet_grid(Category~.)+
  scale_x_discrete(labels=month[1:42])+
  labs(title = "Monthly Sales by Category",
       x = "Month",y="Sales")+
  scale_y_continuous(labels=scales::dollar_format())+
  theme(axis.text.x = element_text(angle = 90,size=15,
                                   vjust = 0.5,hjust=0.95),
        text=element_text(size = 25),
        plot.title=element_text(hjust = 0.5))
ggplot_build(p)$data
Furniture <- subset(Category, Category == 'Furniture')
Furniture.ts<-ts(Furniture$Sales.Per.Month,start=c(2014,01),end = c(2017,12),
                 frequency = 12)

furn_train.ts <- window(Furniture.ts, start = c(2014, 1), end = c(2017, 6))
furn_valid.ts <- window(Furniture.ts, start = c(2017, 7), end = c(2017,12))
Acf(furn_train.ts,lag.max = 12)

fit_1<-furn_train.ts %>% Arima(order=c(2,1,1), seasonal=c(1,1,0))
Acf(fit_1$residuals)
Pacf(fit_1$residuals)

fit_12 <- forecast(fit_1,h=6)
summary(fit_1)

fit_2<-tslm(furn_train.ts~trend+season)
fit_22 <- forecast(fit_2,h=6)
MAPE(fit_1$fitted,furn_train.ts)
MAPE(fit_2$fitted.values,furn_train.ts)
MAPE(fit_12$mean,furn_valid.ts)
MAPE(fit_22$mean,furn_valid.ts)

fit_furn <- data.frame(Fit=as.numeric(fit_1$fitted), 
                  time=as.numeric(time(furn_train.ts)),
                  Original = as.numeric(furn_train.ts))
pred_furn<-data.frame(Pred = fit_12$mean,
                 time = as.numeric(time(furn_valid.ts)),
                 lower = fit_12$lower,
                 upper = fit_12$upper,
                 Original = as.numeric(furn_valid.ts))
month<-c("Jan 2014","Feb",'Mar','Apr',
         'May','Jun','Jul','Aug',
         "Sep",'Oct','Nov','Dec',
         "Jan 2015","Feb",'Mar','Apr',
         'May','Jun','Jul','Aug',
         "Sep",'Oct','Nov','Dec',
         "Jan 2016","Feb",'Mar','Apr',
         'May','Jun','Jul','Aug',
         "Sep",'Oct','Nov','Dec',
         "Jan 2017","Feb",'Mar','Apr',
         'May','Jun','Jul','Aug',
         "Sep",'Oct','Nov','Dec')
pred_color<-"red"
colors<-c("Actual"="black","Predicted"=pred_color)
ggplot()+
  geom_line(aes(x=time, y=Original,color="Actual"), 
            data=fit_furn,size=2,alpha=0.6)+
  geom_line(data = pred_furn,aes(x=time,y=Original,color="Actual"),
            size=2,alpha=0.6)+
  geom_ribbon(data=pred_furn,aes(x=time,ymin=lower.80.,
                                 ymax=upper.80.),
              fill=pred_color,alpha=0.3)+
  geom_ribbon(data=pred_furn,aes(x=time,ymin=lower.95.,
                                 ymax=upper.95.),
              fill=pred_color,alpha=0.1)+
  geom_line(data = fit_furn, aes(x=time,y=Fit,color="Predicted"),
            size=2,alpha=0.75)+
  geom_line(data=pred_furn,aes(x=time,y=Pred,
                               color="Predicted"),size=2,
            alpha=0.8)+
  labs(title = "Monthly Sales of Furniture",x="Month",y="Sales")+
  scale_x_continuous(breaks=seq(from=2014,to=2017.917,length.out=48), 
                     labels=month)+
  scale_y_continuous(labels=scales::dollar_format())+
  scale_color_manual(values = colors)+
  theme(axis.text.x = element_text(angle = 90,size=15,
                                   vjust = 0.5,hjust=0.95),
        text=element_text(size = 25),
        plot.title=element_text(hjust = 0.5))

###############################
OS <- subset(Category, Category == 'Office Supplies')
OS.ts<-ts(OS$Sales.Per.Month,start=c(2014,01),end = c(2017,12),
                 frequency = 12)

os_train.ts <- window(OS.ts, start = c(2014, 1), end = c(2017, 6))
os_valid.ts <- window(OS.ts, start = c(2017, 7), end = c(2017,12))
Acf(os_train.ts,lag.max = 12)

fit_3<-os_train.ts %>% Arima(order=c(0,1,1), seasonal=c(0,1,1))
Acf(fit_3$residuals)
Pacf(fit_3$residuals)

fit_32 <- forecast(fit_3,h=6)
summary(fit_3)

fit_4<-os.ts %>% Arima(order=c(0,1,1), seasonal=c(0,1,1))
future<-forecast(fit_4,h)



fit_os <- data.frame(Fit=as.numeric(fit_3$fitted), 
                       time=as.numeric(time(os_train.ts)),
                       Original = as.numeric(os_train.ts))
pred_os<-data.frame(Pred = fit_32$mean,
                      time = as.numeric(time(os_valid.ts)),
                      lower = fit_32$lower,
                      upper = fit_32$upper,
                      Original = as.numeric(os_valid.ts))
month<-c("Jan 2014","Feb",'Mar','Apr',
         'May','Jun','Jul','Aug',
         "Sep",'Oct','Nov','Dec',
         "Jan 2015","Feb",'Mar','Apr',
         'May','Jun','Jul','Aug',
         "Sep",'Oct','Nov','Dec',
         "Jan 2016","Feb",'Mar','Apr',
         'May','Jun','Jul','Aug',
         "Sep",'Oct','Nov','Dec',
         "Jan 2017","Feb",'Mar','Apr',
         'May','Jun','Jul','Aug',
         "Sep",'Oct','Nov','Dec')
pred_color1<-"green"
colors<-c("Actual"="black","Predicted"=pred_color1)
ggplot()+
  geom_line(aes(x=time, y=Original,color="Actual"), 
            data=fit_os,size=2,alpha=0.6)+
  geom_line(data = pred_os,aes(x=time,y=Original,color="Actual"),
            size=2,alpha=0.6)+
  geom_ribbon(data=pred_os,aes(x=time,ymin=lower.80.,
                                 ymax=upper.80.),
              fill=pred_color1,alpha=0.3)+
  geom_ribbon(data=pred_os,aes(x=time,ymin=lower.95.,
                                 ymax=upper.95.),
              fill=pred_color1,alpha=0.1)+
  geom_line(data = fit_os, aes(x=time,y=Fit,color="Predicted"),
            size=2,alpha=0.75)+
  geom_line(data=pred_os,aes(x=time,y=Pred,
                               color="Predicted"),size=2,
            alpha=0.8)+
  labs(title = "Monthly Sales of Office Supplies",x="Month",y="Sales")+
  scale_x_continuous(breaks=seq(from=2014,to=2017.917,length.out=48), 
                     labels=month)+
  scale_y_continuous(labels=scales::dollar_format())+
  scale_color_manual(values = colors)+
  theme(axis.text.x = element_text(angle = 90,size=15,
                                   vjust = 0.5,hjust=0.95),
        text=element_text(size = 25),
        plot.title=element_text(hjust = 0.5))

###############333
Tech <- subset(Category, Category == 'Technology')
Tech.ts<-ts(Tech$Sales.Per.Month,start=c(2014,01),end = c(2017,12),
                 frequency = 12)

tech_train.ts <- window(Tech.ts, start = c(2014, 1), end = c(2017, 6))
tech_valid.ts <- window(Tech.ts, start = c(2017, 7), end = c(2017,12))
Acf(tech_train.ts,lag.max = 12)

fit_5<-tech_train.ts %>% Arima(order=c(0,0,0), seasonal=c(1,1,0))
Acf(fit_5$residuals)
Pacf(fit_5$residuals)

fit_52 <- forecast(fit_5,h=6)
summary(fit_5)

fit_6<-tslm(tech_train.ts~trend+season)
fit_62 <- forecast(fit_6,h=6)
MAPE(fit_5$fitted,tech_train.ts)
MAPE(fit_6$fitted.values,tech_train.ts)
MAPE(fit_52$mean,tech_valid.ts)
MAPE(fit_62$mean,tech_valid.ts)

fit_tech <- data.frame(Fit=as.numeric(fit_5$fitted), 
                       time=as.numeric(time(tech_train.ts)),
                       Original = as.numeric(tech_train.ts))
pred_tech<-data.frame(Pred = fit_52$mean,
                      time = as.numeric(time(tech_valid.ts)),
                      lower = fit_52$lower,
                      upper = fit_52$upper,
                      Original = as.numeric(tech_valid.ts))
month<-c("Jan 2014","Feb",'Mar','Apr',
         'May','Jun','Jul','Aug',
         "Sep",'Oct','Nov','Dec',
         "Jan 2015","Feb",'Mar','Apr',
         'May','Jun','Jul','Aug',
         "Sep",'Oct','Nov','Dec',
         "Jan 2016","Feb",'Mar','Apr',
         'May','Jun','Jul','Aug',
         "Sep",'Oct','Nov','Dec',
         "Jan 2017","Feb",'Mar','Apr',
         'May','Jun','Jul','Aug',
         "Sep",'Oct','Nov','Dec')
pred_color2<-"#619CFF"
colors<-c("Actual"="black","Predicted"=pred_color2)
ggplot()+
  geom_line(aes(x=time, y=Original,color="Actual"), 
            data=fit_tech,size=2,alpha=0.6)+
  geom_line(data = pred_tech,aes(x=time,y=Original,color="Actual"),
            size=2,alpha=0.6)+
  geom_ribbon(data=pred_tech,aes(x=time,ymin=lower.80.,
                                 ymax=upper.80.),
              fill=pred_color2,alpha=0.3)+
  geom_ribbon(data=pred_tech,aes(x=time,ymin=lower.95.,
                                 ymax=upper.95.),
              fill=pred_color2,alpha=0.1)+
  geom_line(data = fit_tech, aes(x=time,y=Fit,color="Predicted"),
            size=2,alpha=0.75)+
  geom_line(data=pred_tech,aes(x=time,y=Pred,
                               color="Predicted"),size=2,
            alpha=0.8)+
  labs(title = "Monthly Sales of Technology",x="Month",y="Sales")+
  scale_x_continuous(breaks=seq(from=2014,to=2017.917,length.out=48), 
                     labels=month)+
  scale_y_continuous(labels=scales::dollar_format())+
  scale_color_manual(values = colors)+
  theme(axis.text.x = element_text(angle = 90,size=15,
                                   vjust = 0.5,hjust=0.95),
        text=element_text(size = 25),
        plot.title=element_text(hjust = 0.5))

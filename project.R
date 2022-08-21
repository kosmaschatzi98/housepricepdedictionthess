library(readxl)
library(tidyr)
library(dplyr)
library(ggplot2)
library(broom)
library(ggExtra)
library(scales)
houses <- read_xlsx("/home/kosmas/Documents/houses_agia_triada.xlsx")
str(houses)
head(houses)
attach(houses)

#year_built fixing NULL values
houses$year_built <- ifelse(is.na(houses$year_built), mean(houses$year_built, na.rm = TRUE), houses$year_built)
head(houses)

### Rental price per sq_meter
houses$price_per_sq <- price/sq_meters
houses$price_per_sq
head(houses)
#####energy class
sum(is.na(houses$energy_class))
houses$energy_class <- ifelse(is.na(houses$energy_class),6, houses$energy_class)
houses$energy_class <- ifelse(houses$energy_class=="A",1, houses$energy_class)
houses$energy_class <- ifelse(houses$energy_class=="B",2, houses$energy_class)
houses$energy_class <- ifelse(houses$energy_class=="C",3, houses$energy_class)
houses$energy_class <- ifelse(houses$energy_class=="D",4, houses$energy_class)
houses$energy_class <- ifelse(houses$energy_class=="E",5, houses$energy_class)
houses$energy_class <- ifelse(houses$energy_class=="F",6, houses$energy_class)
houses$energy_class <- ifelse(houses$energy_class=="G",6, houses$energy_class)
houses$energy_class <- ifelse(houses$energy_class=="H",6, houses$energy_class)
houses$energy_class <- ifelse(houses$energy_class=="Z",6, houses$energy_class)
houses$energy_class <- as.numeric(houses$energy_class)
head(houses)


##level
unique(houses$level)
houses$level <- ifelse(houses$level=="h",0, houses$level)
houses$level <- as.numeric(houses$level)

##Variable types

houses$id <- as.character(houses$id)


##stats
summary(price)
sd(price)
summary(sq_meters)
sd(sq_meters)
summary(houses$price_per_sq)
sd(houses$price_per_sq)
summary(bedrooms)
sd(bedrooms)

###Visualizations
##discrete
relbeds <-as.data.frame(table(houses$bedrooms)/length(houses$bedrooms))
ggplot(houses, aes(as.factor(bedrooms)))+geom_bar( fill = "blue")+xlab("Number of Bedrooms")+geom_text(aes(label= ..count..), stat = 'count', position=position_dodge(0.9),vjust=-0.2)
ggplot(relbeds,aes(x="",y=Freq, fill= Var1))+geom_bar(stat = "identity", width = 1)+coord_polar("y", start = 0)+
  geom_text(aes(label = paste0(percent(Freq/sum(Freq), accuracy = 0.1), "%")), position = position_stack(vjust=0.5)) +
  theme_void()+labs(x = NULL, y = NULL, fill = NULL)

##groups
df1<- group_by(houses, heat_type)
summarise(df1, mymean= mean(price))

##
p<-ggplot(data = houses, aes(sq_meters,price))+
  geom_point(color= "blue")+xlab("Apartment Square Meters") +ylab("Apartment Rent Price")+theme_linedraw()
p1<-ggMarginal(p, type="histogram",fill = "blue", size=10)

p1
ggplot(data = houses, aes(sq_meters,price, color= heat_type))+
  geom_point()+
  xlab("Apartment Square Meters") +ylab("Apartment Rent Price")+
  theme_linedraw()

ggplot(houses, aes(year_built))+geom_histogram(binwidth = 2, fill= "blue", color= "black")+xlab("Year Built distribution")
ggplot(houses, aes(sq_meters))+geom_histogram(binwidth = 2, fill= "blue", color= "black")+xlab("Apartment Area in Square Meters distribution")
ggplot(houses, aes(price_per_sq))+geom_histogram(binwidth = 1, fill= "blue")+xlab("Apartment Price per Square Meter distribution")

ggplot(houses, aes(price))+geom_boxplot(fill= "yellow")+scale_x_continuous(breaks = c(200,300,400,500,600,700,800))
ggplot(houses, aes(price))+geom_histogram(binwidth = 20, fill= "blue", color= "black")
ggplot(houses[heat_type="gas"], aes(price))+geom_histogram(binwidth = 20, fill= "red", color= "black")+xlab("Apartment Price for houses with gas distribution")

houses$renov_fact <- ifelse(houses$rennovated==1, "Rennovated", "Not rennovated")
ggplot(houses, aes(price, fill= renov_fact))+geom_histogram(binwidth = 20, color= "black")+ theme(legend.title = element_blank())
ggplot(houses, aes(year_built, fill= heat_type))+geom_histogram(binwidth = 2, color= "black")

ggplot(data= houses, aes(heat_type))+geom_bar(fill= "blue")+xlab("Heat Type")+ylab("")


ggplot(houses,aes(as.factor(rennovated)))+geom_bar()+scale_x_discrete(labels = c("0"="No", "1"="Yes"))+xlab("Rennoavated")
ggplot(houses,aes(as.factor(elevator)))+geom_bar(fill = "#8B2323")+scale_x_discrete(labels = c("0"="No Elevator","1"="Elevator"))+xlab("")

ggplot(data = houses, aes(sq_meters,price, color= as.factor(dual_aspect_apartment)))+
  geom_point()+
  xlab("Apartment Square Meters") +ylab("Apartment Rent Price")+
  scale_color_manual(labels = c("Not Dual Aspect", "Dual Aspect"),
                      values = c("black", "red"))+ theme(legend.title = element_blank())

ggplot(data = houses, aes(sq_meters,price, color= as.factor(rennovated)))+
  geom_point()+
  xlab("Apartment Square Meters") +ylab("Apartment Rent Price")+scale_color_manual(labels = c("Not Rennovated", "Rennovated"),
                                                                                  values = c("red", "blue"))+ theme(legend.title = element_blank())
##
houses$Elevator= as.factor(elevator)
ggplot(data = houses, aes(sq_meters,price, color= Elevator))+
  geom_point()+
  xlab("Apartment Square Meters") +ylab("Apartment Rent Price")+scale_color_manual(labels = c("No", "Υes"),
                                                                                   values = c("red", "blue"))

ggplot(houses,aes(as.factor(furniture)))+geom_bar(fill = "#458B00")+scale_x_discrete(labels = c("0"="Empty","1"="Furniture"))+xlab("")
h_furn<-group_by(houses, furniture)
tab_f<-summarise(h_furn, mean = mean(price))
ggplot(tab_f, aes(x=as.factor(furniture), y= mean))+geom_bar(stat = "identity")+scale_x_discrete(labels = c("0"="Empty","1"="Furniture"))+xlab("")

ggplot(data = houses, aes(sq_meters,price, color= as.factor(sea_nearby)))+
  geom_point()+
  xlab("Apartment Square Meters") +ylab("Apartment Rent Price")+scale_color_manual(labels = c("Not Close to the Sea", "Close to the Sea"),
                                                                                   values = c("red", "blue"))+ theme(legend.title = element_blank())
ggplot(data = houses, aes(sq_meters,price, color= as.factor(view)))+
  geom_point()+
  xlab("Apartment Square Meters") +ylab("Apartment Rent Price")+scale_color_manual(labels = c("No view", "View"),
                                                                                   values = c("red", "blue"))+ theme(legend.title = element_blank())
h_view<-group_by(houses, view)
tab_view<-summarise(h_view, mean = mean(price))
ggplot(tab_view, aes(x=as.factor(view), y= mean))+geom_bar(stat = "identity")+scale_x_discrete(labels = c("0"="No view","1"="View"))+xlab("")


cor(sq_meters,price)
cor(elevator, price)
cor( bedrooms, price)
cor(dual_aspect_apartment, price)

##Transform heat type variables
houses$has_gas <- ifelse(houses$heat_type=="gas",1,0)
houses$age<-2022 - houses$year_built
head(houses)
corrsub <- subset(houses,select= c(price, sq_meters,level, bedrooms,age,elevator, energy_class,dual_aspect_apartment,rennovated,pets_allowed,furniture,sea_nearby,view, has_gas))  
for_corheatmap<- round(cor(corrsub),2)
library(reshape2)
meltedcormap <- melt(for_corheatmap)
head(meltedcormap)
ggplot(meltedcormap,aes( Var1, Var2, fill= value))+geom_tile()+
  scale_fill_gradient2(low = "green", high = "yellow", mid = "white", 
                        midpoint = 0, limit = c(-1,1), space = "Lab", 
                                            name="Pearson\nCorrelation")+geom_text(aes(label=value))+xlab("") +ylab(" ")+theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))





##### Linear Model
#exclude outliers
houses1 <- subset(houses,price<700)
boxplot(houses1$price)





model<-lm(price~sq_meters, data = houses1)
summary(model)
tidy(model, conf.int = TRUE, conf.level = 0.95)
ggplot(data = houses1, aes(sq_meters,price))+
  geom_point(color= "blue")+geom_smooth(method = "lm")

mod<- lm(price~sq_meters+bedrooms+has_gas+dual_aspect_apartment+elevator+view, data = houses1)
summary(mod)

mod2<- lm(price~sq_meters+has_gas+elevator+view, data = houses1)
summary(mod2)
tidy(mod2, conf.int = TRUE, conf.level = 0.95)

library(Metrics)

##Quadratic linear regression
houses1$sq_m2 <- houses1$sq_meters^2
qmodel<-lm(price~sq_meters+ sq_m2, data = houses1)
summary(qmodel)

ggplot(data = houses1, aes(sq_meters,price))+
  geom_point(color= "blue")+stat_smooth(se=F, method='lm', formula=y~poly(x,2), color= "red")


##Quantile Regression
library(quantreg)

t= c(.25, .5, .75)
qmodel <- rq(price~sq_meters+has_gas+elevator+view, data = houses1, tau = t)
summary(qmodel) 
tidy(qmodel)
## elevator and view are statistically insignificant in 2 and 3rd percentile
qmodel1 <- rq(price~sq_meters+has_gas, data = houses1, tau = t)
summary(qmodel1)
plot(summary(qmodel1))
# !! the House area and gas coeff doesnt vary signifinactly from the linear regression model


# Lets see how square meters affect though the prices distribution

qant_mod_2<-rq(price~sq_meters, data = houses1, tau = t)
summary(qant_mod_2)
plot(summary(qant_mod_2))

##Ploting Qant_mod_2

ggplot(data= houses1,  aes(sq_meters,price))+
  geom_point(color= "blue")+geom_quantile(quantiles= t, colour= 'red',size = 1, alpha = 0.5)+xlim(0,125)

 
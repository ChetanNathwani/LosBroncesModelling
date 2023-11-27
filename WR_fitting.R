
#load the libraries
library(praise)
library(cowplot)
praise()
library(tidyverse)
library(naniar)
library(dplyr)
library(IsoplotR) 
library(forcats)
library(ggpubr)
library(provenance)


# read in the data
data_WR <- read.csv("WR_LB_new_R.csv") 

glimpse(data_WR)
tail(data_WR)



# Specify columns that should be turned into numerics (based on compile LA compile sheet)
i <- c(15:109) 

# turn factors into numerics. < and text turned into NA, no idea what 2 means -> it means columns
data_WR[ , i] <- apply(data_WR[, i], 2, 
                       function(x) as.numeric(as.character(x))) 


#Basic calculations
data_WR <- data_WR %>%
  mutate(SrY = Sr/Y) %>%
  mutate(BaTh = Ba/Th)%>%
  mutate(LaYb = La_LA/Yb_LA)%>%
  mutate(DyYb = Dy_LA/Yb_LA)%>%
  mutate(ThLa = Th/La)%>%
  mutate(VSc = V/Sc)%>%
  mutate(Date_Exp = Date - 4)
 

#Determine the different populations and sort them



Man_data <- 
  data_WR%>%
  filter(!Sample_Classification %in% c("Pre-porphyry_volcanics", "Dike","Intrusive_Complex_(distal)" ))%>%
  filter(!Alteration_level %in% c(5,4 ))%>%
  filter(LOI<2)
  

Man_data$Alteration_level <- factor(Man_data$Alteration_level)

Man_data_filter <- Man_data%>%
  filter(!FAMOS_UID %in% c("SLLBMI08"))

  

#Time Th


#linear model
model_Th <- lm(Th ~ Date, data = Man_data_filter)#lm = every line model. X~y, data= data file
summary(model_Th)

par(mfrow=c(2,2)) #test whether one can lineary fit something. The top and bottom line put the models into one diagram
plot (model_Th)
par(mfrow = c(1,1))



ggplot(Man_data, aes(x= Date, y = Th))+ #add regression
  geom_point( colour = "red", fill = "blue", size = 6, alpha = 0.8)+
  geom_smooth(method = lm, colour = "red", fill = "springgreen")+#nothing in brackets means baseline layout
  coord_cartesian(xlim = c(4,19), ylim = c(0, 25), expand = FALSE)+
  theme(aspect.ratio=1,
        panel.grid.major = element_line(colour = "NA", size=1.5),
        panel.grid.minor = element_line(colour = "NA", size=.25,linetype = "dashed"))+
  labs(y="Th_ppm")+
  labs(x="Date")+
  theme_bw()



newDate_Th <- expand.grid(Date = seq(from = 4, to = 18, length = 57)) #create new lne with uncertainty envelopes

head(newDate_Th)


newTh <- predict(model_Th, newdata = newDate_Th, interval = 'confidence') #Lets get the prediction line and the confidence bands using predict()

head(newTh)


Th_model <- data.frame(newDate_Th, newTh) #housekeeping (brush two data frames together)
Th_model <- rename(Th_model, Th = fit)

Th_plot <- ggplot(Man_data, aes(x= Date, y = Th))+ 
  geom_point( colour = "red", fill = "blue", size = 6, alpha = 0.8)+
  geom_smooth(data = Th_model, aes(ymin = lwr, ymax = upr),
              stat = 'identity')+
  coord_cartesian(xlim = c(4,19), ylim = c(0, 25), expand = FALSE)+
  theme_bw()


Th_model_AFC <- Th_model[,-1]
rownames(Th_model_AFC) <- Th_model[,1]



#ThLa model

#linear model
model_ThLa <- lm(ThLa ~ Date, data = Man_data_filter)#lm = every line model. X~y, data= data file
summary(model_ThLa)

par(mfrow=c(2,2)) #test whether one can lineary fit something. The top and bottom line put the models into one diagram
plot (model_ThLa)
par(mfrow = c(1,1))



ggplot(Man_data, aes(x= Date, y = ThLa))+ #add regression
  geom_point( colour = "red", fill = "blue", size = 6, alpha = 0.8)+
  geom_smooth(method = lm, colour = "red", fill = "springgreen")+#nothing in brackets means baseline layout
  coord_cartesian(xlim = c(4,19), ylim = c(0, 1.25), expand = FALSE)+
  theme(aspect.ratio=1,
        panel.grid.major = element_line(colour = "NA", size=1.5),
        panel.grid.minor = element_line(colour = "NA", size=.25,linetype = "dashed"))+
  labs(y="Th/La")+
  labs(x="Date")+
  theme_bw()



newDate_ThLa <- expand.grid(Date = seq(from = 4, to = 18, length = 57)) #create new lne with uncertainty envelopes
head(newDate_ThLa)


newThLa <- predict(model_ThLa, newdata = newDate_ThLa, interval = 'confidence') #Lets get the prediction line and the confidence bands using predict()

head(newTh)


ThLa_model <- data.frame(newDate_ThLa, newThLa) #housekeeping (brush two data frames together)
ThLa_model <- rename(ThLa_model, ThLa = fit)

ThLa_plot <- ggplot(Man_data, aes(x= Date, y = ThLa))+ 
  geom_point( colour = "red", fill = "blue", size = 6, alpha = 0.8)+
  geom_smooth(data = ThLa_model, aes(ymin = lwr, ymax = upr),
              stat = 'identity')+
  coord_cartesian(xlim = c(4,19), ylim = c(0, 1.25), expand = FALSE)+
  theme_bw()

ThLa_model_AFC <- ThLa_model[,-1]
rownames(ThLa_model_AFC) <- ThLa_model[,1]

#VSc model

#linear model
model_VSc <- lm(VSc ~ Date, data = Man_data_filter)#lm = every line model. X~y, data= data file
summary(model_VSc)

par(mfrow=c(2,2)) #test whether one can lineary fit something. The top and bottom line put the models into one diagram
plot (model_VSc)
par(mfrow = c(1,1))



ggplot(Man_data, aes(x= Date, y = VSc))+ #add regression
  geom_point( colour = "red", fill = "blue", size = 6, alpha = 0.8)+
  geom_smooth(method = lm, colour = "red", fill = "springgreen")+#nothing in brackets means baseline layout
  coord_cartesian(xlim = c(4,19), ylim = c(0, 1.25), expand = FALSE)+
  theme(aspect.ratio=1,
        panel.grid.major = element_line(colour = "NA", size=1.5),
        panel.grid.minor = element_line(colour = "NA", size=.25,linetype = "dashed"))+
  labs(y="VSc")+
  labs(x="Date")+
  theme_bw()



newDate_VSc <- expand.grid(Date = seq(from = 4, to = 18, length = 57)) #create new lne with uncertainty envelopes
head(newDate_VSc)


newVSc <- predict(model_VSc, newdata = newDate_VSc, interval = 'confidence') #Lets get the prediction line and the confidence bands using predict()

head(newTh)


VSc_model <- data.frame(newDate_VSc, newVSc) #housekeeping (brush two data frames together)
VSc_model <- rename(VSc_model, VSc = fit)

VSc_plot <- ggplot(Man_data, aes(x= Date, y = VSc))+ 
  geom_point( colour = "red", fill = "blue", size = 6, alpha = 0.8)+
  geom_smooth(data = VSc_model, aes(ymin = lwr, ymax = upr),
              stat = 'identity')+
  coord_cartesian(xlim = c(4,19), ylim = c(0, 25), expand = FALSE)+
  theme_bw()

VSc_model_AFC <- VSc_model[,-1]
rownames(VSc_model_AFC) <- VSc_model[,1]


#Ba/Th - exponential
#instead of fitting to y = a*e^(r*t) we use the log
# Log(y) = log(a) + b*x



model_BaTh <- lm(log(BaTh)~ Date, data = Man_data_filter)
summary(model_BaTh) 
coef(model_BaTh)

#log(y) = log(a) + b*x
#Intercept is log(a)
# "Date" is b

#exp(7.0150534+(-0.2293197*x))


newDate_BaTh <- expand.grid(Date = seq(from = 4, to = 18, length = 57)) #create new lne with uncertainty envelopes
head(newDate_BaTh)

newBaTh <- exp(predict(model_BaTh, newdata = newDate_BaTh, interval = 'confidence')) #Lets get the prediction line and the confidence bands using predict()

head(newBaTh)

BaTh_model <- data.frame(newDate_BaTh, newBaTh) #housekeeping (brush two data frames together)
BaTh_model <- rename(BaTh_model, BaTh = fit)

BaTh_plot <- ggplot(Man_data, aes(x= Date, y = BaTh))+ 
  geom_point( colour = "red", fill = "blue", size = 6, alpha = 0.8)+
  geom_smooth(data = BaTh_model, aes(ymin = lwr, ymax = upr),
              stat = 'identity')+
  coord_cartesian(xlim = c(4,19), ylim = c(0, 500), expand = FALSE)+
  theme(aspect.ratio=1,
        panel.grid.major = element_line(colour = "NA", size=1.5),
        panel.grid.minor = element_line(colour = "NA", size=.25,linetype = "dashed"))+
  labs(y="Ba/Th")+
  labs(x="Date")+
  theme_bw()

BaTh_model_AFC <- BaTh_model[,-1]
rownames(BaTh_model_AFC) <- BaTh_model[,1]

# Sr/Y model
#exp # exclude Don Luis and Agustina?

model_SrY <- lm(log(SrY)~ Date, data = Man_data_filter)
summary(model_BaTh) 
coef(model_BaTh)

newDate_SrY <- expand.grid(Date = seq(from = 4, to = 18, length = 57)) #create new lne with uncertainty envelopes
head(newDate_SrY)

newSrY <- exp(predict(model_SrY, newdata = newDate_SrY, interval = 'confidence')) #Lets get the prediction line and the confidence bands using predict()

head(newSrY)

SrY_model <- data.frame(newDate_SrY, newSrY) #housekeeping (brush two data frames together)
SrY_model <- rename(SrY_model, SrY = fit)

SrY_plot <- ggplot(Man_data, aes(x= Date, y = SrY))+ 
  geom_point( colour = "red", fill = "blue", size = 6, alpha = 0.8)+
  geom_smooth(data = SrY_model, aes(ymin = lwr, ymax = upr),
              stat = 'identity')+
  coord_cartesian(xlim = c(4,19), ylim = c(0, 200), expand = FALSE)+
  theme(aspect.ratio=1,
        panel.grid.major = element_line(colour = "NA", size=1.5),
        panel.grid.minor = element_line(colour = "NA", size=.25,linetype = "dashed"))+
  labs(y="Sr/Y")+
  labs(x="Date")+
  theme_bw()

SrY_model_AFC <- SrY_model[,-1]
rownames(SrY_model_AFC) <- SrY_model[,1]

# Dy/Ybmodel
#exp 


model_DyYb <- lm(log(DyYb)~ Date, data = Man_data_filter)
summary(model_DyYb) 
coef(model_DyYb)

newDate_DyYb <- expand.grid(Date = seq(from = 4, to = 18, length = 57)) #create new lne with uncertainty envelopes
head(newDate_DyYb)

newDyYb <- exp(predict(model_DyYb, newdata = newDate_DyYb, interval = 'confidence')) #Lets get the prediction line and the confidence bands using predict()

head(newDyYb)

DyYb_model <- data.frame(newDate_DyYb, newDyYb) #housekeeping (brush two data frames together)
DyYb_model <- rename(DyYb_model, DyYb = fit)

DyYb_plot <- ggplot(Man_data, aes(x= Date, y = DyYb))+ 
  geom_point( colour = "red", fill = "blue", size = 6, alpha = 0.8)+
  geom_smooth(data = DyYb_model, aes(ymin = lwr, ymax = upr),
              stat = 'identity')+
  coord_cartesian(xlim = c(4,19), ylim = c(1, 3.5), expand = FALSE)+
  theme(aspect.ratio=1,
        panel.grid.major = element_line(colour = "NA", size=1.5),
        panel.grid.minor = element_line(colour = "NA", size=.25,linetype = "dashed"))+
  labs(y="Dy/Yb")+
  labs(x="Date")+
  theme_bw()

DyYb_model_AFC <- DyYb_model[,-1]
rownames(DyYb_model_AFC) <- DyYb_model[,1]

# La/Yb model
#exp # excluded  Don Luis and La Copa
#not sure whether to include


model_LaYb <- lm(log(LaYb)~ Date, data = Man_data_filter)
summary(model_LaYb) 
coef(model_LaYb)

newDate_LaYb <- expand.grid(Date = seq(from = 4, to = 18, length =57)) #create new lne with uncertainty envelopes
head(newDate_LaYb)

newLaYb <- exp(predict(model_LaYb, newdata = newDate_LaYb, interval = 'confidence')) #Lets get the prediction line and the confidence bands using predict()

head(newLaYb)

LaYb_model <- data.frame(newDate_LaYb, newLaYb) #housekeeping (brush two data frames together)
LaYb_model <- rename(LaYb_model, LaYb = fit)

LaYb_plot <- ggplot(Man_data, aes(x= Date, y = LaYb))+ 
  geom_point( colour = "red", fill = "blue", size = 6, alpha = 0.8)+
  geom_smooth(data = LaYb_model, aes(ymin = lwr, ymax = upr),
              stat = 'identity')+
  coord_cartesian(xlim = c(4,19), ylim = c(0, 60), expand = FALSE)+
  theme(aspect.ratio=1,
        panel.grid.major = element_line(colour = "NA", size=1.5),
        panel.grid.minor = element_line(colour = "NA", size=.25,linetype = "dashed"))+
  labs(y="La/Yb")+
  labs(x="Date")+
  theme_bw()

LaYb_model_AFC <- LaYb_model[,-1] #make first column the row names
rownames(LaYb_model_AFC) <- LaYb_model[,1]







Fits<-ggarrange(Th_plot, ThLa_plot, BaTh_plot, SrY_plot, DyYb_plot, LaYb_plot, VSc_plot, #name the plots you want to combine
                      labels = c("A", "B", "C","D","F","G"), #Labels of the diagram
                      ncol = 2, nrow = 4,
                      common.legend = TRUE, legend = "bottom")
Fits

rm(list = ls()[grepl("model$", ls())])#deletes everything ending with MC
rm(list = ls()[grepl("plot$", ls())])#deletes everything ending with MC
rm(list = ls()[grepl("^new", ls())]) #deletes everything starting with D
rm(list = ls()[grepl("^model", ls())]) #deletes everything starting with D

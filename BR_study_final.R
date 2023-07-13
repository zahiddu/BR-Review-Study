install.packages("readxl")
library(readxl)
library(ggplot2)
library(ggpubr)

getwd()
setwd("C:/users/zahid/onedrive/desktop/dengue/biting rate/plosNTD/revision/2nd round revision")
df=read_excel("S4_Table.xlsx")


#############################################################################
############### Chompoosri et al, 2012 [30]: Hourly bitning across seasons #################
#############################################################################

chompoosri_hourly3<-df[which(df$`Temporality(military_time)`!="" & df$Study=="Chompoosri et al., 2012 [30]"),]
chompoosri_hourly_summer<-chompoosri_hourly3[which(chompoosri_hourly3$Season=="Summer"),]
chompoosri_hourly_rainy<-chompoosri_hourly3[which(chompoosri_hourly3$Season=="Rainy"),]
chompoosri_hourly_winter<-chompoosri_hourly3[which(chompoosri_hourly3$Season=="Winter"),]

p1=ggplot()+
  geom_line(aes(x=chompoosri_hourly_summer$`Temporality(military_time)`,y=chompoosri_hourly_summer$Mean_biting, color="a", group=1),linetype="dashed")+
  geom_point(aes(x=chompoosri_hourly_summer$`Temporality(military_time)`,y=chompoosri_hourly_summer$Mean_biting, color="a"),size=1)+
  geom_line(aes(x=chompoosri_hourly_rainy$`Temporality(military_time)`,y=chompoosri_hourly_rainy$Mean_biting, color="b", group=1),linetype="solid")+
  geom_point(aes(x=chompoosri_hourly_rainy$`Temporality(military_time)`,y=chompoosri_hourly_rainy$Mean_biting, color="b"),size=1)+
  geom_line(aes(x=chompoosri_hourly_winter$`Temporality(military_time)`,y=chompoosri_hourly_winter$Mean_biting,color="c", group=1),linetype="dotted")+
  geom_point(aes(x=chompoosri_hourly_winter$`Temporality(military_time)`,y=chompoosri_hourly_winter$Mean_biting, color="c"),size=1)+
  labs(x="hours of day",y="female Ae. aegypti/person-hour")+
  scale_color_manual(guide= guide_legend(""), labels = c("Summer", "Rainy","Winter"),values=c("bisque3","coral3","black"))+
  theme_classic()+
  theme(axis.text=element_text(angle=90, size=8))+
  theme(axis.title=element_text(size=9)) +
  theme(panel.border=element_rect(fill=NA), aspect.ratio = .75)+
  theme(legend.direction = "vertical") + 
  theme(legend.position=c(0.81,0.766))+
  theme(legend.text=element_text(size=7.65))+
  theme(legend.title = element_blank()) # To remove legend title
  
##########################################################################
################ Captain Esoah, 2020 [21]: hourly biting (2015 vs 2016) ################
##########################################################################

captain_hourly_2015<-df[which(df$`Temporality(military_time)`!="-" & df$Study=="Captain-Esoah et al., 2020 [21]" & df$Year == 2015),]
captain_hourly_2016<-df[which(df$`Temporality(military_time)`!="-" & df$Study=="Captain-Esoah et al., 2020 [21]" & df$Year == 2016),]
captain_both<-rbind(captain_hourly_2015, captain_hourly_2016)

p2=ggplot(captain_both)+
  geom_bar(aes(x=`Temporality(military_time)`, y=Mean_biting, fill=Year), width=.7, position="dodge", stat="identity") +
  xlab("hours of day") + ylab("female Ae. aegypti/person-hour") +
  scale_fill_manual(labels = c("2015", "2016"), values = c("bisque3", "coral3"))+
  theme_classic()+
  theme(axis.text=element_text(angle=90, size = 8)) +
  theme(axis.title=element_text(size=9)) +
  theme(panel.border=element_rect(fill=NA), aspect.ratio = .75)+
  theme(legend.direction = "vertical")+
  theme(legend.position = c(0.2,0.8))+
  theme(legend.title = element_blank()) # To remove legend title +

ggarrange(p1, p2, nrow=1, ncol=2, labels="auto")
ggsave(file="Fig2_hourly_bitings.tiff", width = 6.5, height = 3.2)

##############################################################################
################# Karch et al., 1995 [23]: Indoor VS outdoor ############################
##############################################################################

karch_indoor<-df[which(df$Study=="Karch et al., 1995 [23]" & df$Location=="Indoor"),]
karch_outdoor<-df[which(df$Study=="Karch et al., 1995 [23]" & df$Location=="Outdoor"),]
karch_indoor$Month_in_order<-factor(karch_indoor$Month, levels=unique(karch_indoor$Month)) #to keep the default order of values along the x-axis
karch_outdoor$Month_in_order<-factor(karch_outdoor$Month, levels=unique(karch_outdoor$Month)) #to keep the default order of values along the x-axis

ggplot() +
  geom_point(aes(x=karch_indoor$Month_in_order,y=karch_indoor$Mean_biting, color = "a"), shape = 16) +
  geom_point(aes(x=karch_outdoor$Month_in_order,y=karch_outdoor$Mean_biting, color = "b"), shape = 16) +
  xlab("month") + ylab("female Ae. aegypti/person-hour") + 
  ylim(c(0,1.25)) +
  scale_color_manual(guide= guide_legend(c("","")), labels = c("Indoor","Outdoor"),values=c("bisque3","coral3"))+
  theme_classic()+
  theme(axis.text = element_text(angle=90, size = 7.5))+
  theme(panel.border=element_rect(fill=NA), aspect.ratio = .75)+
  theme(legend.position = c(0.5,0.89))+
  theme(legend.title = element_blank())+
  theme(legend.direction = "horizontal")+
  theme(legend.text = element_text(size=9))

ggsave(file="Fig3_Karch_indoor_outdoor.tiff", width =3.2 , height = 3.2)   

#############################################################
############### Effect of Temperature & Rainfall ############
#############################################################

########### Karch et al., 1995 [23]: Temperature and Rainfall #################

karch_rainfall_temperature_indoor<-df[which(df$Study=="Karch et al., 1995 [23]" & df$Location=="Indoor"),c(1,2,9,11)]
karch_rainfall_temperature_indoor$Temperature <- as.numeric(karch_rainfall_temperature_indoor$Temperature)
karch_rainfall_temperature_indoor$`Rainfall(mm/day)` <- as.numeric(karch_rainfall_temperature_indoor$`Rainfall(mm/day)`)

p3=ggplot(karch_rainfall_temperature_indoor, aes(`Rainfall(mm/day)`, Temperature,  color=Mean_biting)) + 
  geom_point() + 
  scale_color_gradient(name = "Mean biting", low="bisque3", high="coral3") + 
  labs(x="Rainfall (mm/day)",y="Temperature (C)")+
  xlim(0,16.1)+ ylim(18,30)

karch_rainfall_temperature_outdoor<-df[which(df$Study=="Karch et al., 1995 [23]" & df$Location=="Outdoor"),c(1,2,9,11)]
karch_rainfall_temperature_outdoor$Temperature <- as.numeric(karch_rainfall_temperature_outdoor$Temperature)
karch_rainfall_temperature_outdoor$`Rainfall(mm/day)` <- as.numeric(karch_rainfall_temperature_outdoor$`Rainfall(mm/day)`)

p4=ggplot(karch_rainfall_temperature_outdoor, aes(`Rainfall(mm/day)`, Temperature,  color=Mean_biting)) + 
  geom_point() + 
  scale_color_gradient(name = "Mean biting", low="bisque3", high="coral3") + 
  labs(x="Rainfall (mm/day)",y="Temperature (C)")+
  xlim(0,16.1)+ ylim(18,30)

p3_and_p4=ggarrange(p3, p4, nrow=1, ncol=2, labels="auto")
annotate_figure(p3_and_p4, top = text_grob("Karch et al. (a) Indoor and (b) Outdoor", face = "bold"))
ggsave(file="Fig_S1_rain_temp_Karch.tiff", width = 6.5, height = 5)

########### Captain-Esoah et al., 2020 [21]: Temperature and Rainfall ####################

captain_rainfall_temp_2015<-df[which(df$Study=="Captain-Esoah et al., 2020 [21]" & df$Year == 2015 & df$Rainfall_source == "S1 Table"), c(1,2,8,9,11)]
captain_rainfall_temp_2015$Temperature <- as.numeric(captain_rainfall_temp_2015$Temperature)
captain_rainfall_temp_2015$`Rainfall(mm/day)` <- as.numeric(captain_rainfall_temp_2015$`Rainfall(mm/day)`)

p5=ggplot(captain_rainfall_temp_2015, aes(`Rainfall(mm/day)`, Temperature,  color=Mean_biting)) + 
  geom_point() + 
  scale_color_gradient(name = "Mean biting", low="bisque3", high="coral3") + 
  scale_x_continuous(breaks=c(0,3,6,9))+
  ylim(22.5,32.2) +
  labs(x="Rainfall (mm/day)",y="Temperature (C)")

captain_rainfall_temp_2016<-df[which(df$Study=="Captain-Esoah et al., 2020 [21]" & df$Year == 2016 & df$Rainfall_source == "S1 Table"), c(1,2,8,9,11)]
captain_rainfall_temp_2016$Temperature <- as.numeric(captain_rainfall_temp_2016$Temperature)
captain_rainfall_temp_2016$`Rainfall(mm/day)` <- as.numeric(captain_rainfall_temp_2016$`Rainfall(mm/day)`)

p6=ggplot(captain_rainfall_temp_2016, aes(`Rainfall(mm/day)`, Temperature,  color=Mean_biting)) + 
  geom_point() + 
  scale_color_gradient(name = "Mean biting", low="bisque3", high="coral3") + 
  scale_x_continuous(breaks=c(0,2,4,6,8,9))+
  ylim(22.5,32.2) +
  labs(x="Rainfall (mm/day)",y="Temperature (C)")

p5_and_p6=ggarrange(p5, p6, nrow=1, ncol=2, labels="auto")
annotate_figure(p5_and_p6, top = text_grob("Captain-Esoah et al. (a) 2015 and (b) 2016", face = "bold"))
ggsave(file="Fig_S2_rain_temp_Captain_Esoah.tiff", width = 6.5, height = 5)

########### Salas et al.,1994 [29] : Temperature and Rainfall #####################

salas_rainfall_temperature<-df[which(df$Study=="Salas-Luévano & Reyes-Villanueva, 1994 [29]"),c(1,2,9,11)]
salas_rainfall_temperature$Temperature <- as.numeric(salas_rainfall_temperature$Temperature)
salas_rainfall_temperature$`Rainfall(mm/day)` <- as.numeric(salas_rainfall_temperature$`Rainfall(mm/day)`)

ggplot(salas_rainfall_temperature, aes(`Rainfall(mm/day)`, Temperature,  color=Mean_biting)) + 
  geom_point() + 
  scale_color_gradient(name = "Mean biting", low="bisque3", high="coral3") + 
  labs(x="Rainfall (mm/day)",y="Temperature (C)")+
  xlim(0,16.1)+ ylim(18,30)

ggsave(file="Fig_S3_temp_rain_Salas.png", width = 2.5, height = 2.5) 


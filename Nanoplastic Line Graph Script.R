# Nanoplastics Visualization#

library(tidyverse)
library(here)
library(RColorBrewer)

## Glyphosate ##

glyphosate <- read.csv(here("Glyphosate.csv"))

# Control #

glyphosate1 <- subset(glyphosate, Dose == "2")
glyphosate2 <- subset(glyphosate, Dose == "1")

Dose2_meanTD <- glyphosate1 %>% 
  group_by(Minute = as.numeric(as.character(Minute))) %>% 
  summarise(mean_TD = mean(TD), stdev_TD = sd(TD))i
Dose1_meanTD <- glyphosate2 %>% 
  group_by(Minute = as.numeric(as.character(Minute))) %>% 
  summarise(mean_TD = mean(TD), stdev_TD = sd(TD))

Control_meanTD <- rbind(Dose1_meanTD, Dose2_meanTD)
Dose = c(rep(1,25), rep(2,25))
Control_meanTD <- cbind(Control_meanTD, Dose)

ggplot(data=Control_meanTD, aes(x=Minute, y=mean_TD, group=Dose, colour=Dose)) +
   geom_line() +
   geom_point()+
   geom_errorbar(aes(ymin=mean_TD-stdev_TD, ymax=mean_TD+stdev_TD), width=.2,
              position=position_dodge(0.05))
ggsave(here("Control_meanTD.png"))

# Low Dose #       

glyphosate3 <- subset(glyphosate, Dose == "3")
glyphosate4 <- subset(glyphosate, Dose == "4")

Dose3_meanTD <- glyphosate3 %>% 
  group_by(Minute = as.numeric(as.character(Minute))) %>% 
  summarise(mean_TD = mean(TD), stdev_TD = sd(TD))
Dose4_meanTD <- glyphosate4 %>% 
  group_by(Minute = as.numeric(as.character(Minute))) %>% 
  summarise(mean_TD = mean(TD), stdev_TD = sd(TD))

Low_meanTD <- rbind(Dose3_meanTD, Dose4_meanTD)
Dose = c(rep(3,25), rep(4,25))
Low_meanTD <- cbind(Low_meanTD, Dose)

ggplot(data=Low_meanTD, aes(x=Minute, y=mean_TD, group=Dose, colour=Dose)) +
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=mean_TD-stdev_TD, ymax=mean_TD+stdev_TD), width=.2,
                position=position_dodge(0.05))
ggsave(here("Low_meanTD.png"))

# Medium Dose #

glyphosate5 <- subset(glyphosate, Dose == "5")
glyphosate6 <- subset(glyphosate, Dose == "6")

Dose5_meanTD <- glyphosate5 %>% 
  group_by(Minute = as.numeric(as.character(Minute))) %>% 
  summarise(mean_TD = mean(TD), stdev_TD = sd(TD))
Dose6_meanTD <- glyphosate6 %>% 
  group_by(Minute = as.numeric(as.character(Minute))) %>% 
  summarise(mean_TD = mean(TD), stdev_TD = sd(TD))

Med_meanTD <- rbind(Dose5_meanTD, Dose6_meanTD)
Dose = c(rep(5,25), rep(6,25))
Med_meanTD <- cbind(Med_meanTD, Dose)

ggplot(data=Med_meanTD, aes(x=Minute, y=mean_TD, group=Dose, colour=Dose)) +
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=mean_TD-stdev_TD, ymax=mean_TD+stdev_TD), width=.2,
                position=position_dodge(0.05))
ggsave(here("Med_meanTD.png"))

# High Dose #

glyphosate7 <- subset(glyphosate, Dose == "7")
glyphosate8 <- subset(glyphosate, Dose == "8")

Dose7_meanTD <- glyphosate7 %>% 
  group_by(Minute = as.numeric(as.character(Minute))) %>% 
  summarise(mean_TD = mean(TD), stdev_TD = sd(TD))
Dose8_meanTD <- glyphosate8 %>% 
  group_by(Minute = as.numeric(as.character(Minute))) %>% 
  summarise(mean_TD = mean(TD), stdev_TD = sd(TD))

High_meanTD <- rbind(Dose7_meanTD, Dose8_meanTD)
Dose = c(rep(7,25), rep(8,25))
High_meanTD <- cbind(High_meanTD, Dose)

ggplot(data=High_meanTD, aes(x=Minute, y=mean_TD, group=Dose, colour=Dose)) +
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=mean_TD-stdev_TD, ymax=mean_TD+stdev_TD), width=.2,
                position=position_dodge(0.05))
ggsave(here("High_meanTD.png"))

# Everything #

Everything_TD <- rbind(Control_meanTD, Low_meanTD, Med_meanTD, High_meanTD)
Everything_TD <- cbind(Everything_TD, Dose)

ggplot(data=Everything_TD, aes(x=Minute, y=mean_TD, group=as.factor(Dose), colour=as.factor(Dose))) +
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=mean_TD-stdev_TD, ymax=mean_TD+stdev_TD), width=.2,
                position=position_dodge(0.05))+
  scale_color_brewer(palette  = "Dark2")

#Repeated Measures ANOVA#

#normality test was failing due to minute 26, so those data points are removed
#tested each dose by grouping them and plotting a histogram, outliers removed as noted

glyphosate1 <- subset(glyphosate, Dose == "2")
glyphosate2 <- subset(glyphosate, Dose == "1")
glyphosate3 <- subset(glyphosate, Dose == "3")
glyphosate4 <- subset(glyphosate, Dose == "4")
glyphosate5 <- subset(glyphosate, Dose == "5")
glyphosate6 <- subset(glyphosate, Dose == "6")
glyphosate7 <- subset(glyphosate, Dose == "7")
glyphosate8 <- subset(glyphosate, Dose == "8")

ggplot()+ 
  geom_histogram(data=glyphosate1, aes(x=TD), binwidth = 10, fill = "blue")+ 
  theme_classic(base_size=18) 

ggplot()+ 
  geom_histogram(data=glyphosate2, aes(x=TD), binwidth = 10, fill = "blue")+ 
  theme_classic(base_size=18) 

ggplot()+ 
  geom_histogram(data=glyphosate3, aes(x=TD), binwidth = 10, fill = "blue")+ 
  theme_classic(base_size=18) 

ggplot()+ 
  geom_histogram(data=glyphosate4, aes(x=TD), binwidth = 10, fill = "blue")+ 
  theme_classic(base_size=18) 

ggplot()+ 
  geom_histogram(data=glyphosate5, aes(x=TD), binwidth = 10, fill = "blue")+ 
  theme_classic(base_size=18) 

ggplot()+ 
  geom_histogram(data=glyphosate6, aes(x=TD), binwidth = 10, fill = "blue")+ 
  theme_classic(base_size=18) 

ggplot()+ 
  geom_histogram(data=glyphosate7, aes(x=TD), binwidth = 10, fill = "blue")+ 
  theme_classic(base_size=18) 

ggplot()+ 
  geom_histogram(data=glyphosate8, aes(x=TD), binwidth = 10, fill = "blue")+ 
  theme_classic(base_size=18) 

shapiro.test(glyphosate1$TD)
shapiro.test(glyphosate2$TD)
shapiro.test(glyphosate3$TD)
shapiro.test(glyphosate4$TD)
shapiro.test(glyphosate5$TD)
shapiro.test(glyphosate6$TD)
shapiro.test(glyphosate7$TD)
shapiro.test(glyphosate8$TD)

#
#
# data transform that is no longer necessary, keeping for reference #

log10TD <- mutate(glyphosate, log10TD = log10(TD))

ggplot()+ 
geom_boxplot(data=log10TD, aes(group = Dose, x=Dose, y=log10TD))+ 
theme_classic(base_size=18) 

dose1 <- subset(log10TD, Dose == "1")
#check removing infinite values#

ggplot()+ 
  geom_histogram(data=dose1, aes(x=log10TD), binwidth = 10, fill = "blue")+ 
  theme_classic(base_size=18) 

# end not necessary
#
#

###########################################################################

  
## Rotenone ##

rotenone <- read.csv(here("Rotenone.csv"))

# Control #

rotenone1 <- subset(rotenone, Dose == "2")
rotenone2 <- subset(rotenone, Dose == "1")

Dose2_meanTD <- rotenone1 %>% 
  group_by(Minute = as.numeric(as.character(Minute))) %>% 
  summarise(mean_TD = mean(TD), stdev_TD = sd(TD))
Dose1_meanTD <- rotenone2 %>% 
  group_by(Minute = as.numeric(as.character(Minute))) %>% 
  summarise(mean_TD = mean(TD), stdev_TD = sd(TD))

Control_meanTD <- rbind(Dose1_meanTD, Dose2_meanTD)
Dose = c(rep(1,25), rep(2,25))
Control_meanTD <- cbind(Control_meanTD, Dose)

ggplot(data=Control_meanTD, aes(x=Minute, y=mean_TD, group=Dose, colour=Dose)) +
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=mean_TD-stdev_TD, ymax=mean_TD+stdev_TD), width=.2,
                position=position_dodge(0.05))
ggsave(here("Control_meanTD.png"))

# Low Dose #       

rotenone3 <- subset(rotenone, Dose == "3")
rotenone4 <- subset(rotenone, Dose == "4")

Dose3_meanTD <- rotenone3 %>% 
  group_by(Minute = as.numeric(as.character(Minute))) %>% 
  summarise(mean_TD = mean(TD), stdev_TD = sd(TD))
Dose4_meanTD <- rotenone4 %>% 
  group_by(Minute = as.numeric(as.character(Minute))) %>% 
  summarise(mean_TD = mean(TD), stdev_TD = sd(TD))

Low_meanTD <- rbind(Dose3_meanTD, Dose4_meanTD)
Dose = c(rep(3,25), rep(4,25))
Low_meanTD <- cbind(Low_meanTD, Dose)

ggplot(data=Low_meanTD, aes(x=Minute, y=mean_TD, group=Dose, colour=Dose)) +
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=mean_TD-stdev_TD, ymax=mean_TD+stdev_TD), width=.2,
                position=position_dodge(0.05))
ggsave(here("Low_meanTD.png"))

# Medium Dose #

rotenone5 <- subset(rotenone, Dose == "5")
rotenone6 <- subset(rotenone, Dose == "6")

Dose5_meanTD <- rotenone5 %>% 
  group_by(Minute = as.numeric(as.character(Minute))) %>% 
  summarise(mean_TD = mean(TD), stdev_TD = sd(TD))
Dose6_meanTD <- rotenone6 %>% 
  group_by(Minute = as.numeric(as.character(Minute))) %>% 
  summarise(mean_TD = mean(TD), stdev_TD = sd(TD))

Med_meanTD <- rbind(Dose5_meanTD, Dose6_meanTD)
Dose = c(rep(5,25), rep(6,25))
Med_meanTD <- cbind(Med_meanTD, Dose)

ggplot(data=Med_meanTD, aes(x=Minute, y=mean_TD, group=Dose, colour=Dose)) +
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=mean_TD-stdev_TD, ymax=mean_TD+stdev_TD), width=.2,
                position=position_dodge(0.05))
ggsave(here("Med_meanTD.png"))

# High Dose #

rotenone7 <- subset(rotenone, Dose == "7")
rotenone8 <- subset(rotenone, Dose == "8")

Dose7_meanTD <- rotenone7 %>% 
  group_by(Minute = as.numeric(as.character(Minute))) %>% 
  summarise(mean_TD = mean(TD), stdev_TD = sd(TD))
Dose8_meanTD <- rotenone8 %>% 
  group_by(Minute = as.numeric(as.character(Minute))) %>% 
  summarise(mean_TD = mean(TD), stdev_TD = sd(TD))

High_meanTD <- rbind(Dose7_meanTD, Dose8_meanTD)
Dose = c(rep(7,25), rep(8,25))
High_meanTD <- cbind(High_meanTD, Dose)

ggplot(data=High_meanTD, aes(x=Minute, y=mean_TD, group=Dose, colour=Dose)) +
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=mean_TD-stdev_TD, ymax=mean_TD+stdev_TD), width=.2,
                position=position_dodge(0.05))
ggsave(here("High_meanTD.png"))

#Everything#

Everything_TD <- rbind(Control_meanTD, Low_meanTD, Med_meanTD, High_meanTD)

ggplot(data=Everything_TD, aes(x=Minute, y=mean_TD, group=as.factor(Dose), colour=as.factor(Dose))) +
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=mean_TD-stdev_TD, ymax=mean_TD+stdev_TD), width=.2,
                position=position_dodge(0.05))+
  scale_color_brewer(palette  = "Dark2")
ggsave(here("Everything_TD.png"))

#histos#

rotenone1 <- subset(rotenone, Dose == "2")
rotenone2 <- subset(rotenone, Dose == "1")
rotenone3 <- subset(rotenone, Dose == "3")
rotenone4 <- subset(rotenone, Dose == "4")
rotenone5 <- subset(rotenone, Dose == "5")
rotenone6 <- subset(rotenone, Dose == "6")
rotenone7 <- subset(rotenone, Dose == "7")
rotenone8 <- subset(rotenone, Dose == "8")

ggplot()+ 
  geom_histogram(data=rotenone1, aes(x=TD), binwidth = 10, fill = "blue")+ 
  theme_classic(base_size=18) 

ggplot()+ 
  geom_histogram(data=rotenone2, aes(x=TD), binwidth = 10, fill = "blue")+ 
  theme_classic(base_size=18) 

ggplot()+ 
  geom_histogram(data=rotenone3, aes(x=TD), binwidth = 10, fill = "blue")+ 
  theme_classic(base_size=18) 

ggplot()+ 
  geom_histogram(data=rotenone4, aes(x=TD), binwidth = 10, fill = "blue")+ 
  theme_classic(base_size=18) 

ggplot()+ 
  geom_histogram(data=rotenone5, aes(x=TD), binwidth = 10, fill = "blue")+ 
  theme_classic(base_size=18) 

ggplot()+ 
  geom_histogram(data=rotenone6, aes(x=TD), binwidth = 10, fill = "blue")+ 
  theme_classic(base_size=18) 

ggplot()+ 
  geom_histogram(data=rotenone7, aes(x=TD), binwidth = 10, fill = "blue")+ 
  theme_classic(base_size=18) 

ggplot()+ 
  geom_histogram(data=rotenone8, aes(x=TD), binwidth = 10, fill = "blue")+ 
  theme_classic(base_size=18) 

#########################################################################

## FCCP ##

FCCP <- read.csv(here("FCCP.csv"))

# Control #

FCCP1 <- subset(FCCP, Dose == "2")
FCCP2 <- subset(FCCP, Dose == "1")

Dose2_meanTD <- FCCP1 %>% 
  group_by(Minute = as.numeric(as.character(Minute))) %>% 
  summarise(mean_TD = mean(TD), stdev_TD = sd(TD))
Dose1_meanTD <- FCCP2 %>% 
  group_by(Minute = as.numeric(as.character(Minute))) %>% 
  summarise(mean_TD = mean(TD), stdev_TD = sd(TD))

Control_meanTD <- rbind(Dose1_meanTD, Dose2_meanTD)
Dose = c(rep(1,25), rep(2,25))
Control_meanTD <- cbind(Control_meanTD, Dose)

ggplot(data=Control_meanTD, aes(x=Minute, y=mean_TD, group=Dose, colour=Dose)) +
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=mean_TD-stdev_TD, ymax=mean_TD+stdev_TD), width=.2,
                position=position_dodge(0.05))
ggsave(here("Control_meanTD.png"))

# Low Dose #       

FCCP3 <- subset(FCCP, Dose == "3")
FCCP4 <- subset(FCCP, Dose == "4")

Dose3_meanTD <- FCCP3 %>% 
  group_by(Minute = as.numeric(as.character(Minute))) %>% 
  summarise(mean_TD = mean(TD), stdev_TD = sd(TD))
Dose4_meanTD <- FCCP4 %>% 
  group_by(Minute = as.numeric(as.character(Minute))) %>% 
  summarise(mean_TD = mean(TD), stdev_TD = sd(TD))

Low_meanTD <- rbind(Dose3_meanTD, Dose4_meanTD)
Dose = c(rep(3,25), rep(4,25))
Low_meanTD <- cbind(Low_meanTD, Dose)

ggplot(data=Low_meanTD, aes(x=Minute, y=mean_TD, group=Dose, colour=Dose)) +
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=mean_TD-stdev_TD, ymax=mean_TD+stdev_TD), width=.2,
                position=position_dodge(0.05))
ggsave(here("Low_meanTD.png"))

# Medium Dose #

FCCP5 <- subset(FCCP, Dose == "5")
FCCP6 <- subset(FCCP, Dose == "6")

Dose5_meanTD <- FCCP5 %>% 
  group_by(Minute = as.numeric(as.character(Minute))) %>% 
  summarise(mean_TD = mean(TD), stdev_TD = sd(TD))
Dose6_meanTD <- FCCP6 %>% 
  group_by(Minute = as.numeric(as.character(Minute))) %>% 
  summarise(mean_TD = mean(TD), stdev_TD = sd(TD))

Med_meanTD <- rbind(Dose5_meanTD, Dose6_meanTD)
Dose = c(rep(5,25), rep(6,25))
Med_meanTD <- cbind(Med_meanTD, Dose)

ggplot(data=Med_meanTD, aes(x=Minute, y=mean_TD, group=Dose, colour=Dose)) +
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=mean_TD-stdev_TD, ymax=mean_TD+stdev_TD), width=.2,
                position=position_dodge(0.05))
ggsave(here("Med_meanTD.png"))

# High Dose #

FCCP7 <- subset(FCCP, Dose == "7")
FCCP8 <- subset(FCCP, Dose == "8")

Dose7_meanTD <- FCCP7 %>% 
  group_by(Minute = as.numeric(as.character(Minute))) %>% 
  summarise(mean_TD = mean(TD), stdev_TD = sd(TD))
Dose8_meanTD <- FCCP8 %>% 
  group_by(Minute = as.numeric(as.character(Minute))) %>% 
  summarise(mean_TD = mean(TD), stdev_TD = sd(TD))

High_meanTD <- rbind(Dose7_meanTD, Dose8_meanTD)
Dose = c(rep(7,25), rep(8,25))
High_meanTD <- cbind(High_meanTD, Dose)

ggplot(data=High_meanTD, aes(x=Minute, y=mean_TD, group=Dose, colour=Dose)) +
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=mean_TD-stdev_TD, ymax=mean_TD+stdev_TD), width=.2,
                position=position_dodge(0.05))
ggsave(here("High_meanTD.png"))

#Everything#

Everything_TD <- rbind(Control_meanTD, Low_meanTD, Med_meanTD, High_meanTD)

ggplot(data=Everything_TD, aes(x=Minute, y=mean_TD, group=as.factor(Dose), colour=as.factor(Dose))) +
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=mean_TD-stdev_TD, ymax=mean_TD+stdev_TD), width=.2,
                position=position_dodge(0.05))+
  scale_color_brewer(palette  = "Dark2")
ggsave(here("Everything_TD.png"))

#########################################################################

## TDCPP ##

TDCPP <- read.csv(here("TDCPP.csv"))

# Control #

TDCPP1 <- subset(TDCPP, Dose == "2")
TDCPP2 <- subset(TDCPP, Dose == "1")

Dose2_meanTD <- TDCPP1 %>% 
  group_by(Minute = as.numeric(as.character(Minute))) %>% 
  summarise(mean_TD = mean(TD), stdev_TD = sd(TD))
Dose1_meanTD <- TDCPP2 %>% 
  group_by(Minute = as.numeric(as.character(Minute))) %>% 
  summarise(mean_TD = mean(TD), stdev_TD = sd(TD))

Control_meanTD <- rbind(Dose1_meanTD, Dose2_meanTD)
Dose = c(rep(1,25), rep(2,25))
Control_meanTD <- cbind(Control_meanTD, Dose)

ggplot(data=Control_meanTD, aes(x=Minute, y=mean_TD, group=Dose, colour=Dose)) +
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=mean_TD-stdev_TD, ymax=mean_TD+stdev_TD), width=.2,
                position=position_dodge(0.05))
ggsave(here("Control_meanTD.png"))

# Low Dose #       

TDCPP3 <- subset(TDCPP, Dose == "3")
TDCPP4 <- subset(TDCPP, Dose == "4")

Dose3_meanTD <- TDCPP3 %>% 
  group_by(Minute = as.numeric(as.character(Minute))) %>% 
  summarise(mean_TD = mean(TD), stdev_TD = sd(TD))
Dose4_meanTD <- TDCPP4 %>% 
  group_by(Minute = as.numeric(as.character(Minute))) %>% 
  summarise(mean_TD = mean(TD), stdev_TD = sd(TD))

Low_meanTD <- rbind(Dose3_meanTD, Dose4_meanTD)
Dose = c(rep(3,26), rep(4,26))
Low_meanTD <- cbind(Low_meanTD, Dose)

ggplot(data=Low_meanTD, aes(x=Minute, y=mean_TD, group=Dose, colour=Dose)) +
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=mean_TD-stdev_TD, ymax=mean_TD+stdev_TD), width=.2,
                position=position_dodge(0.05))
ggsave(here("Low_meanTD.png"))

# Medium Dose #

TDCPP5 <- subset(TDCPP, Dose == "5")
TDCPP6 <- subset(TDCPP, Dose == "6")

Dose5_meanTD <- TDCPP5 %>% 
  group_by(Minute = as.numeric(as.character(Minute))) %>% 
  summarise(mean_TD = mean(TD), stdev_TD = sd(TD))
Dose6_meanTD <- TDCPP6 %>% 
  group_by(Minute = as.numeric(as.character(Minute))) %>% 
  summarise(mean_TD = mean(TD), stdev_TD = sd(TD))

Med_meanTD <- rbind(Dose5_meanTD, Dose6_meanTD)
Dose = c(rep(5,26), rep(6,26))
Med_meanTD <- cbind(Med_meanTD, Dose)

ggplot(data=Med_meanTD, aes(x=Minute, y=mean_TD, group=Dose, colour=Dose)) +
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=mean_TD-stdev_TD, ymax=mean_TD+stdev_TD), width=.2,
                position=position_dodge(0.05))
ggsave(here("Med_meanTD.png"))

# High Dose #

TDCPP7 <- subset(TDCPP, Dose == "7")
TDCPP8 <- subset(TDCPP, Dose == "8")

Dose7_meanTD <- TDCPP7 %>% 
  group_by(Minute = as.numeric(as.character(Minute))) %>% 
  summarise(mean_TD = mean(TD), stdev_TD = sd(TD))
Dose8_meanTD <- TDCPP8 %>% 
  group_by(Minute = as.numeric(as.character(Minute))) %>% 
  summarise(mean_TD = mean(TD), stdev_TD = sd(TD))

High_meanTD <- rbind(Dose7_meanTD, Dose8_meanTD)
Dose = c(rep(7,26), rep(8,26))
High_meanTD <- cbind(High_meanTD, Dose)

ggplot(data=High_meanTD, aes(x=Minute, y=mean_TD, group=Dose, colour=Dose)) +
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=mean_TD-stdev_TD, ymax=mean_TD+stdev_TD), width=.2,
                position=position_dodge(0.05))
ggsave(here("High_meanTD.png"))

#Everything#

Everything_TD <- rbind(Control_meanTD, Low_meanTD, Med_meanTD, High_meanTD)

ggplot(data=Everything_TD, aes(x=Minute, y=mean_TD, group=as.factor(Dose), colour=as.factor(Dose))) +
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=mean_TD-stdev_TD, ymax=mean_TD+stdev_TD), width=.2,
                position=position_dodge(0.05))+
  scale_color_brewer(palette  = "Dark2")
ggsave(here("Everything_TD.png"))

###########################################################################################

## Roundup ##

Roundup <- read.csv(here("Roundup.csv"))

# Control #

Roundup1 <- subset(Roundup, Dose == "2")
Roundup2 <- subset(Roundup, Dose == "1")

Dose2_meanTD <- Roundup1 %>% 
  group_by(Minute = as.numeric(as.character(Minute))) %>% 
  summarise(mean_TD = mean(TD), stdev_TD = sd(TD))
Dose1_meanTD <- Roundup2 %>% 
  group_by(Minute = as.numeric(as.character(Minute))) %>% 
  summarise(mean_TD = mean(TD), stdev_TD = sd(TD))

Control_meanTD <- rbind(Dose1_meanTD, Dose2_meanTD)
Dose = c(rep(1,25), rep(2,25))
Control_meanTD <- cbind(Control_meanTD, Dose)

ggplot(data=Control_meanTD, aes(x=Minute, y=mean_TD, group=Dose, colour=Dose)) +
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=mean_TD-stdev_TD, ymax=mean_TD+stdev_TD), width=.2,
                position=position_dodge(0.05))
ggsave(here("Control_meanTD.png"))

# Low Dose #       

Roundup3 <- subset(Roundup, Dose == "3")
Roundup4 <- subset(Roundup, Dose == "4")

Dose3_meanTD <- Roundup3 %>% 
  group_by(Minute = as.numeric(as.character(Minute))) %>% 
  summarise(mean_TD = mean(TD), stdev_TD = sd(TD))
Dose4_meanTD <- Roundup4 %>% 
  group_by(Minute = as.numeric(as.character(Minute))) %>% 
  summarise(mean_TD = mean(TD), stdev_TD = sd(TD))

Low_meanTD <- rbind(Dose3_meanTD, Dose4_meanTD)
Dose = c(rep(3,25), rep(4,25))
Low_meanTD <- cbind(Low_meanTD, Dose)

ggplot(data=Low_meanTD, aes(x=Minute, y=mean_TD, group=Dose, colour=Dose)) +
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=mean_TD-stdev_TD, ymax=mean_TD+stdev_TD), width=.2,
                position=position_dodge(0.05))
ggsave(here("Low_meanTD.png"))

# Medium Dose #

Roundup5 <- subset(Roundup, Dose == "5")
Roundup6 <- subset(Roundup, Dose == "6")

Dose5_meanTD <- Roundup5 %>% 
  group_by(Minute = as.numeric(as.character(Minute))) %>% 
  summarise(mean_TD = mean(TD), stdev_TD = sd(TD))
Dose6_meanTD <- Roundup6 %>% 
  group_by(Minute = as.numeric(as.character(Minute))) %>% 
  summarise(mean_TD = mean(TD), stdev_TD = sd(TD))

Med_meanTD <- rbind(Dose5_meanTD, Dose6_meanTD)
Dose = c(rep(5,25), rep(6,25))
Med_meanTD <- cbind(Med_meanTD, Dose)

ggplot(data=Med_meanTD, aes(x=Minute, y=mean_TD, group=Dose, colour=Dose)) +
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=mean_TD-stdev_TD, ymax=mean_TD+stdev_TD), width=.2,
                position=position_dodge(0.05))
ggsave(here("Med_meanTD.png"))

# High Dose #

Roundup7 <- subset(Roundup, Dose == "7")
Roundup8 <- subset(Roundup, Dose == "8")

Dose7_meanTD <- Roundup7 %>% 
  group_by(Minute = as.numeric(as.character(Minute))) %>% 
  summarise(mean_TD = mean(TD), stdev_TD = sd(TD))
Dose8_meanTD <- Roundup8 %>% 
  group_by(Minute = as.numeric(as.character(Minute))) %>% 
  summarise(mean_TD = mean(TD), stdev_TD = sd(TD))

High_meanTD <- rbind(Dose7_meanTD, Dose8_meanTD)
Dose = c(rep(7,25), rep(8,25))
High_meanTD <- cbind(High_meanTD, Dose)

ggplot(data=High_meanTD, aes(x=Minute, y=mean_TD, group=Dose, colour=Dose)) +
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=mean_TD-stdev_TD, ymax=mean_TD+stdev_TD), width=.2,
                position=position_dodge(0.05))
ggsave(here("High_meanTD.png"))

#Everything#

Everything_TD <- rbind(Control_meanTD, Low_meanTD, Med_meanTD, High_meanTD)

ggplot(data=Everything_TD, aes(x=Minute, y=mean_TD, group=as.factor(Dose), colour=as.factor(Dose))) +
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=mean_TD-stdev_TD, ymax=mean_TD+stdev_TD), width=.2,
                position=position_dodge(0.05))+
  scale_color_brewer(palette  = "Dark2")
ggsave(here("Everything_TD.png"))

ggplot()+ 
  geom_histogram(data=Roundup1, aes(x=TD), binwidth = 10, fill = "blue")+ 
  theme_classic(base_size=18) 

ggplot()+ 
  geom_histogram(data=Roundup2, aes(x=TD), binwidth = 10, fill = "blue")+ 
  theme_classic(base_size=18) 

ggplot()+ 
  geom_histogram(data=Roundup3, aes(x=TD), binwidth = 10, fill = "blue")+ 
  theme_classic(base_size=18) 

ggplot()+ 
  geom_histogram(data=Roundup4, aes(x=TD), binwidth = 10, fill = "blue")+ 
  theme_classic(base_size=18) 

ggplot()+ 
  geom_histogram(data=Roundup5, aes(x=TD), binwidth = 10, fill = "blue")+ 
  theme_classic(base_size=18) 

ggplot()+ 
  geom_histogram(data=Roundup6, aes(x=TD), binwidth = 10, fill = "blue")+ 
  theme_classic(base_size=18) 

ggplot()+ 
  geom_histogram(data=Roundup7, aes(x=TD), binwidth = 10, fill = "blue")+ 
  theme_classic(base_size=18) 

ggplot()+ 
  geom_histogram(data=Roundup8, aes(x=TD), binwidth = 10, fill = "blue")+ 
  theme_classic(base_size=18) 






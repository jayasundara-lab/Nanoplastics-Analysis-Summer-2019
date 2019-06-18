#Nanoplastics Analysis: Data Visualization

#Packages required
library(tidyverse)
library(here)

#Importing Data: Change 'FCCP.csv' to the sheet you're importing

Chem <- read.csv(here("FCCP.csv"))

#And now for the rest of the script

#
#

##Group 1: NP Control##

group1 <- subset(Chem, Dose == "1")

#NP Control: Average Distance

group1.MD <- group1 %>% 
  group_by(Minute) %>% 
  summarise(average = mean(MD))

group1.MD[["Minute"]] <- as.numeric(group1.MD[["Minute"]])
ndx = order(group1.MD$Minute, decreasing=F)
group1.MD.O = group1.MD[ndx,]

ggplot(group1.MD.O, aes(x=Minute, y=average)) +
   geom_boxplot(fill="skyblue", aes(group = cut_width(Minute, 5)))

ggsave(here("group1MD.png"))

#NP Control: Mean Velocity

group1.MV <- group1 %>% 
  group_by(Minute) %>% 
  summarise(average = mean(MV))

group1.MV[["Minute"]] <- as.numeric(group1.MV[["Minute"]])
ndx = order(group1.MV$Minute, decreasing=F)
group1.MV.O = group1.MV[ndx,]

ggplot(group1.MV.O, aes(x=Minute, y=average)) +
  geom_boxplot(fill="skyblue", aes(group = cut_width(Minute, 5)))

ggsave(here("group1MV.png"))

#Total Distance

group1.TD <- group1 %>% 
  group_by(Minute) %>% 
  summarise(average = mean(TD))

group1.TD[["Minute"]] <- as.numeric(group1.TD[["Minute"]])
ndx = order(group1.TD$Minute, decreasing=F)
group1.TD.O = group1.TD[ndx,]

ggplot(group1.TD.O, aes(x=Minute, y=average)) +
  geom_boxplot(fill="skyblue", aes(group = cut_width(Minute, 5)))

ggsave(here("group1TD.png"))

#
#

##Group 2: Control###

group2 <- subset(Chem, Dose == "2")

#NP Control: Average Distance

group2.MD <- group2 %>% 
  group_by(Minute) %>% 
  summarise(average = mean(MD))

group2.MD[["Minute"]] <- as.numeric(group2.MD[["Minute"]])
ndx = order(group2.MD$Minute, decreasing=F)
group2.MD.O = group2.MD[ndx,]

ggplot(group2.MD.O, aes(x=Minute, y=average)) +
  geom_boxplot(fill="skyblue", aes(group = cut_width(Minute, 5)))

ggsave(here("group2MD.png"))

#NP Control: Mean Velocity

group2.MV <- group2 %>% 
  group_by(Minute) %>% 
  summarise(average = mean(MV))

group2.MV[["Minute"]] <- as.numeric(group2.MV[["Minute"]])
ndx = order(group2.MV$Minute, decreasing=F)
group2.MV.O = group2.MV[ndx,]

ggplot(group2.MV.O, aes(x=Minute, y=average)) +
  geom_boxplot(fill="skyblue", aes(group = cut_width(Minute, 5)))

ggsave(here("group2MV.png"))

#Total Distance

group2.TD <- group2 %>% 
  group_by(Minute) %>% 
  summarise(average = mean(TD))

group2.TD[["Minute"]] <- as.numeric(group2.TD[["Minute"]])
ndx = order(group2.TD$Minute, decreasing=F)
group2.TD.O = group2.TD[ndx,]

ggplot(group2.TD.O, aes(x=Minute, y=average)) +
  geom_boxplot(fill="skyblue", aes(group = cut_width(Minute, 5)))

ggsave(here("group2TD.png"))

#
#

##Group 3: Low Dose##

group3 <- subset(Chem, Dose == "3")

#NP Control: Average Distance

group3.MD <- group3 %>% 
  group_by(Minute) %>% 
  summarise(average = mean(MD))

group3.MD[["Minute"]] <- as.numeric(group3.MD[["Minute"]])
ndx = order(group3.MD$Minute, decreasing=F)
group3.MD.O = group3.MD[ndx,]

ggplot(group3.MD.O, aes(x=Minute, y=average)) +
  geom_boxplot(fill="skyblue", aes(group = cut_width(Minute, 5)))

ggsave(here("group3MD.png"))

#NP Control: Mean Velocity

group3.MV <- group3 %>% 
  group_by(Minute) %>% 
  summarise(average = mean(MV))

group3.MV[["Minute"]] <- as.numeric(group3.MV[["Minute"]])
ndx = order(group3.MV$Minute, decreasing=F)
group3.MV.O = group3.MV[ndx,]

ggplot(group3.MV.O, aes(x=Minute, y=average)) +
  geom_boxplot(fill="skyblue", aes(group = cut_width(Minute, 5)))

ggsave(here("group3MV.png"))

#Total Distance

group3.TD <- group3 %>% 
  group_by(Minute) %>% 
  summarise(average = mean(TD))

group3.TD[["Minute"]] <- as.numeric(group3.TD[["Minute"]])
ndx = order(group3.TD$Minute, decreasing=F)
group3.TD.O = group3.TD[ndx,]

ggplot(group3.TD.O, aes(x=Minute, y=average)) +
  geom_boxplot(fill="skyblue", aes(group = cut_width(Minute, 5)))

ggsave(here("group3TD.png"))

#
#

##Dose 4: Low Dose + NP##

group4 <- subset(Chem, Dose == "4")

#NP Control: Average Distance

group4.MD <- group4 %>% 
  group_by(Minute) %>% 
  summarise(average = mean(MD))

group4.MD[["Minute"]] <- as.numeric(group4.MD[["Minute"]])
ndx = order(group4.MD$Minute, decreasing=F)
group4.MD.O = group4.MD[ndx,]

ggplot(group4.MD.O, aes(x=Minute, y=average)) +
  geom_boxplot(fill="skyblue", aes(group = cut_width(Minute, 5)))

ggsave(here("group4MD.png"))

#NP Control: Mean Velocity

group4.MV <- group4 %>% 
  group_by(Minute) %>% 
  summarise(average = mean(MV))

group4.MV[["Minute"]] <- as.numeric(group4.MV[["Minute"]])
ndx = order(group4.MV$Minute, decreasing=F)
group4.MV.O = group4.MV[ndx,]

ggplot(group4.MV.O, aes(x=Minute, y=average)) +
  geom_boxplot(fill="skyblue", aes(group = cut_width(Minute, 5)))

ggsave(here("group4MV.png"))

#Total Distance

group4.TD <- group4 %>% 
  group_by(Minute) %>% 
  summarise(average = mean(TD))

group4.TD[["Minute"]] <- as.numeric(group4.TD[["Minute"]])
ndx = order(group4.TD$Minute, decreasing=F)
group4.TD.O = group4.TD[ndx,]

ggplot(group4.TD.O, aes(x=Minute, y=average)) +
  geom_boxplot(fill="skyblue", aes(group = cut_width(Minute, 5)))

ggsave(here("group4TD.png"))

#
#

##Dose 5: Med Dose##

group5 <- subset(Chem, Dose == "5")

#NP Control: Average Distance

group5.MD <- group5 %>% 
  group_by(Minute) %>% 
  summarise(average = mean(MD))

group5.MD[["Minute"]] <- as.numeric(group5.MD[["Minute"]])
ndx = order(group5.MD$Minute, decreasing=F)
group5.MD.O = group5.MD[ndx,]

ggplot(group5.MD.O, aes(x=Minute, y=average)) +
  geom_boxplot(fill="skyblue", aes(group = cut_width(Minute, 5)))

ggsave(here("group5MD.png"))

#NP Control: Mean Velocity

group5.MV <- group5 %>% 
  group_by(Minute) %>% 
  summarise(average = mean(MV))

group5.MV[["Minute"]] <- as.numeric(group5.MV[["Minute"]])
ndx = order(group5.MV$Minute, decreasing=F)
group5.MV.O = group5.MV[ndx,]

ggplot(group5.MV.O, aes(x=Minute, y=average)) +
  geom_boxplot(fill="skyblue", aes(group = cut_width(Minute, 5)))

ggsave(here("group5MV.png"))

#Total Distance

group5.TD <- group5 %>% 
  group_by(Minute) %>% 
  summarise(average = mean(TD))

group5.TD[["Minute"]] <- as.numeric(group5.TD[["Minute"]])
ndx = order(group5.TD$Minute, decreasing=F)
group5.TD.O = group5.TD[ndx,]

ggplot(group5.TD.O, aes(x=Minute, y=average)) +
  geom_boxplot(fill="skyblue", aes(group = cut_width(Minute, 5)))

ggsave(here("group5TD.png"))

#
#

##Dose 6: Med Dose + NP##

group6 <- subset(Chem, Dose == "6")

#NP Control: Average Distance

group6.MD <- group6 %>% 
  group_by(Minute) %>% 
  summarise(average = mean(MD))

group6.MD[["Minute"]] <- as.numeric(group6.MD[["Minute"]])
ndx = order(group6.MD$Minute, decreasing=F)
group6.MD.O = group6.MD[ndx,]

ggplot(group6.MD.O, aes(x=Minute, y=average)) +
  geom_boxplot(fill="skyblue", aes(group = cut_width(Minute, 5)))

ggsave(here("group6MD.png"))

#NP Control: Mean Velocity

group6.MV <- group6 %>% 
  group_by(Minute) %>% 
  summarise(average = mean(MV))

group6.MV[["Minute"]] <- as.numeric(group6.MV[["Minute"]])
ndx = order(group6.MV$Minute, decreasing=F)
group6.MV.O = group6.MV[ndx,]

ggplot(group6.MV.O, aes(x=Minute, y=average)) +
  geom_boxplot(fill="skyblue", aes(group = cut_width(Minute, 5)))

ggsave(here("group6MV.png"))

#Total Distance

group6.TD <- group6 %>% 
  group_by(Minute) %>% 
  summarise(average = mean(TD))

group6.TD[["Minute"]] <- as.numeric(group6.TD[["Minute"]])
ndx = order(group6.TD$Minute, decreasing=F)
group6.TD.O = group6.TD[ndx,]

ggplot(group6.TD.O, aes(x=Minute, y=average)) +
  geom_boxplot(fill="skyblue", aes(group = cut_width(Minute, 5)))

ggsave(here("group6TD.png"))

#
#

##Dose 7: High Dose##

group7 <- subset(Chem, Dose == "7")

#NP Control: Average Distance

group7.MD <- group7 %>% 
  group_by(Minute) %>% 
  summarise(average = mean(MD))

group7.MD[["Minute"]] <- as.numeric(group7.MD[["Minute"]])
ndx = order(group7.MD$Minute, decreasing=F)
group7.MD.O = group7.MD[ndx,]

ggplot(group7.MD.O, aes(x=Minute, y=average)) +
  geom_boxplot(fill="skyblue", aes(group = cut_width(Minute, 5)))

ggsave(here("group7MD.png"))

#NP Control: Mean Velocity

group7.MV <- group7 %>% 
  group_by(Minute) %>% 
  summarise(average = mean(MV))

group7.MV[["Minute"]] <- as.numeric(group7.MV[["Minute"]])
ndx = order(group7.MV$Minute, decreasing=F)
group7.MV.O = group7.MV[ndx,]

ggplot(group7.MV.O, aes(x=Minute, y=average)) +
  geom_boxplot(fill="skyblue", aes(group = cut_width(Minute, 5)))

ggsave(here("group7MV.png"))

#Total Distance

group7.TD <- group7 %>% 
  group_by(Minute) %>% 
  summarise(average = mean(TD))

group7.TD[["Minute"]] <- as.numeric(group7.TD[["Minute"]])
ndx = order(group7.TD$Minute, decreasing=F)
group7.TD.O = group7.TD[ndx,]

ggplot(group7.TD.O, aes(x=Minute, y=average)) +
  geom_boxplot(fill="skyblue", aes(group = cut_width(Minute, 5)))

ggsave(here("group7TD.png"))

#
#

##Dose 8: High Dose+NP##

group8 <- subset(Chem, Dose == "8")

#NP Control: Average Distance

group8.MD <- group8 %>% 
  group_by(Minute) %>% 
  summarise(average = mean(MD))

group8.MD[["Minute"]] <- as.numeric(group8.MD[["Minute"]])
ndx = order(group8.MD$Minute, decreasing=F)
group8.MD.O = group8.MD[ndx,]

ggplot(group8.MD.O, aes(x=Minute, y=average)) +
  geom_boxplot(fill="skyblue", aes(group = cut_width(Minute, 5)))

ggsave(here("group8MD.png"))

#NP Control: Mean Velocity

group8.MV <- group8 %>% 
  group_by(Minute) %>% 
  summarise(average = mean(MV))

group8.MV[["Minute"]] <- as.numeric(group8.MV[["Minute"]])
ndx = order(group8.MV$Minute, decreasing=F)
group8.MV.O = group8.MV[ndx,]

ggplot(group8.MV.O, aes(x=Minute, y=average)) +
  geom_boxplot(fill="skyblue", aes(group = cut_width(Minute, 5)))

ggsave(here("group8MV.png"))

#Total Distance

group8.TD <- group8 %>% 
  group_by(Minute) %>% 
  summarise(average = mean(TD))

group8.TD[["Minute"]] <- as.numeric(group8.TD[["Minute"]])
ndx = order(group8.TD$Minute, decreasing=F)
group8.TD.O = group8.TD[ndx,]

ggplot(group8.TD.O, aes(x=Minute, y=average)) +
  geom_boxplot(fill="skyblue", aes(group = cut_width(Minute, 5)))

ggsave(here("group8TD.png"))

#
#

#end script










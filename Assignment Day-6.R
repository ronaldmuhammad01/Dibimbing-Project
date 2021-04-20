library(dplyr)
library(tidyverse)


#Check str data
head(Insurance_Loss)
str(Insurance_Loss)
summary(Insurance_Loss)

#Check outlier in every coloumn
boxplot(Insurance_Loss$Years.of.Driving.Experience) #no outlier
boxplot(Insurance_Loss$Age) #no outlier
boxplot(Insurance_Loss$Number.of.Vehicles) #no outlier
boxplot(Insurance_Loss$Vehicle.Age) #no outlier

#check Missing data, if Mean = NA then got missing value
mean(Insurance_Loss$Age) #no Missing value
mean(Insurance_Loss$Years.of.Driving.Experience) #no missing value
mean(Insurance_Loss$Age) #no missing value
mean(Insurance_Loss$Number.of.Vehicles) #no missing value
mean(Insurance_Loss$Vehicle.Age) #no missing value
mean(Insurance_Loss$Losses) #no missing value

#Check dummies variables
unique(Insurance_Loss$Gender) #got dummy
unique(Insurance_Loss$Married) #got dummy
unique(Insurance_Loss$Fuel.Type) #got dummy

#Business Question-1
#Profiling customer yang mengalami kecelakaan/claim insurance

Total_loss <-sum(Insurance_Loss$Losses) #Total Loss 5960955
Total_loss

#Create Age Groups, According DepKES RI, Group age 0-11 "Kanak-Kanak", 12-25 "Remaja", 26-45 "Dewasa", 46-65 "Lansia"

Insurance_Loss$AgeGroup <- cut(Insurance_Loss$Age, breaks = c(0,12,26,46,Inf), labels = c("0-11", "12-25", "26-45", "46-65"),  right = FALSE)
unique (Insurance_Loss$AgeGroup)
head(Insurance_Loss)

#LOSS BY AGE
Loss_by_age <- Insurance_Loss %>%
  group_by(AgeGroup) %>%
  summarise(sum(Losses))

New_df1 <- Loss_by_age %>%
  mutate(percentage_loss_by_groupage = (Loss_by_age/Total_loss)*100)
New_df1


Age_vs_DrivingExperience <- Insurance_Loss %>%
  group_by(AgeGroup) %>%
  filter(Years.of.Driving.Experience <= 6) %>%
  summarise(sum(Losses))
Age_vs_DrivingExperience

New <- (Age_vs_DrivingExperience[1:2,2]/New_df1[1:2,2])*100
New

Insurance_Loss %>%
  group_by(AgeGroup, Driving_Experience) %>%
  summarise(Total_LOSS_NEW = sum(Losses))


Percentage_Loss_GroupAge <- c (40.2, 26.5, 33.3)
Warna <- c ("blue", "yellow", "green")
Loss_labels <- c(round(Percentage_Loss_GroupAge/sum(Percentage_Loss_GroupAge)*100,1))
Loss_labels <- paste(Loss_labels, "%", sep = " ")
pie (Percentage_Loss_GroupAge, main = "Total Loss by Group Age", col = Warna, labels = Loss_labels, cex=0.8)
legend (1.5, 0.5, c ("12-25","26-45","46-65"), cex = 0.8, fill = Warna)

#LOSS BY GENDER
Loss_by_gender <- Insurance_Loss %>%
  group_by(Gender) %>%
  summarise(sum(Losses))
Loss_by_gender

New_df2 <- Loss_by_gender %>%
  mutate(Percentage_Loss = (Loss_by_gender$`sum(Losses)`/Total_loss)*100) 
New_df2

Percentage_Loss_Gender <- c (44.7, 55.3)
Warna <- c ("blue", "green")
Loss_labels <- c(round(Percentage_Loss_Gender/sum(Percentage_Loss_Gender)*100,1))
Loss_labels <- paste(Loss_labels, "%", sep = " ")
pie (Percentage_Loss_Gender, main = "Total Loss by Gender", col = Warna, labels = Loss_labels, cex=0.8)
legend (1.5, 0.5, c ("Male","Female"), cex = 0.8, fill = Warna)

#LOSS BY Marital Status
Loss_by_Marital <- Insurance_Loss %>%
  group_by(Married) %>%
  summarise(sum(Losses))
Loss_by_Marital

New_df3 <- Loss_by_Marital %>%
  mutate(Percentage_Loss = (Loss_by_Marital$`sum(Losses)`/Total_loss)*100) 
New_df3

Percentage_Loss_Marital <- c (42.3, 57.7)
Warna <- c ("blue", "green")
Loss_labels <- c(round(Percentage_Loss_Marital/sum(Percentage_Loss_Marital)*100,1))
Loss_labels <- paste(Loss_labels, "%", sep = " ")
pie (Percentage_Loss_Marital, main = "Total Loss by Marital Status", col = Warna, labels = Loss_labels, cex=0.8)
legend (1.5, 0.5, c ("Married","Single"), cex = 0.8, fill = Warna)

#LOSS BY Fuel Type
Loss_by_Fuel <- Insurance_Loss %>%
  group_by(Fuel.Type) %>%
  summarise(sum(Losses))
Loss_by_Fuel

New_df4 <- Loss_by_Fuel %>%
  mutate(Percentage_Loss = (Loss_by_Fuel$`sum(Losses)`/Total_loss)*100) 
New_df4

Percentage_Loss_Fuel <- c (43.7, 56.3)
Warna <- c ("blue", "green")
Loss_labels <- c(round(Percentage_Loss_Fuel/sum(Percentage_Loss_Fuel)*100,1))
Loss_labels <- paste(Loss_labels, "%", sep = " ")
pie (Percentage_Loss_Marital, main = "Total Loss by Fuel Type", col = Warna, labels = Loss_labels, cex=0.8)
legend (1.5, 0.5, c ("D","P"), cex = 0.8, fill = Warna)


#LOSS BY Vehicle Age
unique(Insurance_Loss$Vehicle.Age)
Insurance_Loss$Vehicle.Age_group <- cut(Insurance_Loss$Vehicle.Age, breaks = c(0,6,11,Inf), labels = c("0-5", "6-10", "11-15"),  right = FALSE)
unique (Insurance_Loss$Vehicle.Age_group)
head(Insurance_Loss)


Loss_by_Vehicleage <- Insurance_Loss %>%
  group_by(Vehicle.Age_group) %>%
  summarise(sum(Losses))


New_df5 <- Loss_by_Vehicleage %>%
  mutate(percentage_loss_by_vehicleGroupAge = (Loss_by_Vehicleage/Total_loss)*100)
New_df5

  Percentage_Loss_VehicleGroupAge <- c (32.3, 34.4, 33.3)
  Warna <- c ("blue", "yellow", "green")
  Loss_labels <- c(round(Percentage_Loss_VehicleGroupAge/sum(Percentage_Loss_VehicleGroupAge)*100,1))
  Loss_labels <- paste(Loss_labels, "%", sep = " ")
  pie (Percentage_Loss_VehicleGroupAge, main = "Total Loss by Vehicle Group Age", col = Warna, labels = Loss_labels, cex=0.8)
  legend (1.5, 0.5, c ("0-5","6-10","11-15"), cex = 0.8, fill = Warna)


#LOSS BY number of vehicle
Loss_by_Number_ofvehicle <- Insurance_Loss %>%
  group_by(Number.of.Vehicles) %>%
  summarise(sum(Losses))
Loss_by_Number_ofvehicle

New_df6 <- Loss_by_Number_ofvehicle %>%
  mutate(Percentage_Loss = (Loss_by_Number_ofvehicle$`sum(Losses)`/Total_loss)*100) 
New_df6

Percentage_Loss_numberofvehicle <- c (16.9, 33.7, 33.1, 16.3)
Warna <- c ("blue", "green", "yellow", "red")
Loss_labels <- c(round(Percentage_Loss_numberofvehicle/sum(Percentage_Loss_numberofvehicle)*100,1))
Loss_labels <- paste(Loss_labels, "%", sep = " ")
pie (Percentage_Loss_numberofvehicle, main = "Total Loss by Number of Vehicle", col = Warna, labels = Loss_labels, cex=0.8)
legend (1.5, 0.5, c ("1","2","3","4"), cex = 0.8, fill = Warna)

#LOSS By Driving Experience
unique(Insurance_Loss$Years.of.Driving.Experience)
Insurance_Loss$Driving_Experience <- cut(Insurance_Loss$Years.of.Driving.Experience, breaks = c(0,6,11,16,21,26,31,36,41,46,51, Inf), labels = c("0-5", "6-10", "11-15", "16-20","21-25","26-30","31-35","36-40","41-45","46-50", ">51"),  right = FALSE)
unique (Insurance_Loss$Driving_Experience)
head(Insurance_Loss)


Loss_by_DrivingExperience <- Insurance_Loss %>%
  group_by(Driving_Experience) %>%
  summarise(sum(Losses))
Loss_by_DrivingExperience

New_df7 <- Loss_by_DrivingExperience %>%
  mutate(percentage_loss_by_DrivingExperience = (Loss_by_DrivingExperience/Total_loss)*100)
New_df7


Percentage_Loss_DrivingExperience <- c (32.1, 13.5, 6.67, 6.51, 6.43, 6.71, 6.48, 6.44, 15.1)
Warna <- c ("blue", "yellow", "green", "purple","orange", "red", "brown", "black", "pink")
Loss_labels <- c(round(Percentage_Loss_DrivingExperience/sum(Percentage_Loss_DrivingExperience)*100,1))
Loss_labels <- paste(Loss_labels, "%", sep = " ")
pie (Percentage_Loss_DrivingExperience, main = "Total Loss by Driving Experience", col = Warna, labels = Loss_labels, cex=0.8)
legend (1.5, 0.5, c ("0-5","6-10","11-15", "16-20", "21-25", "26-30", "31-35", "36-40",">41"), cex = 0.8, fill = Warna)

#LOSS by Driving Experience & Customer Age

Age_vs_Experience <- Insurance_Loss %>%
  group_by(AgeGroup, Driving_Experience) %>%
  summarise(Total_LOSS_NEW = sum(Losses))
Age_vs_Experience

Age_vs_Experience$Percentage_total_loss <- (Age_vs_Experience$Total_LOSS_NEW/Total_loss)*100
Age_vs_Experience

kk <- c (31.8, 8.43, 0.267, 5.12, 6.67, 6.51, 6.23, 1.70, 0.2, 5.01, 6.48, 6.44, 15.2)
Loss_labels <- c(round(kk/sum(kk)*100))
Loss_labels <- paste(Loss_labels, "%", sep = " ")
pie(kk, main = "Customer Age Group & Driving Experience VS Total Loss", col = colors(distinct = F),labels = Loss_labels, cex= 0.6)
legend(1.5, 0.5, c("Remaja [0-5]", "Remaja [6-10]", "Dewasa [0-5]", "Dewasa [6-10]", "Dewasa [11-15]", "Dewasa [16-20]", "Dewasa [21-25]", "Dewasa [26-30", "Lansia [21-25]", "Lansia [26-30]", "Lansia [31-35]", "Lansia [36-40]","Lansia [>41]"), cex = 0.6, fill = colors())

ggplot(data = Age_vs_Experience, aes(x=AgeGroup, y=Percentage_total_loss, fill=Driving_Experience)) + geom_bar(stat = "identity") + coord_polar("y")
#Average Loss
mean(Insurance_Loss$Losses)

Df_Claim_above_avg <- Insurance_Loss %>%
  filter(Losses > mean(Insurance_Loss$Losses))
Df_Claim_above_avg

#profile above average
Loss_by_Age_Avg <- Df_Claim_above_avg %>%
  group_by(AgeGroup) %>%
  summarise(sum(Losses))

Loss_by_Age_Avg

A1 <- Loss_by_Age_Avg %>%
  mutate(percentage_loss_by_groupage = (Loss_by_Age_Avg/sum(Loss_by_Age_Avg$`sum(Losses)`)*100))
A1


Percentage_Loss_GroupAge_avg <- c (49.5, 26.3, 24.3)
Warna <- c ("blue", "yellow", "green")
Loss_labels <- c(round(Percentage_Loss_GroupAge_avg/sum(Percentage_Loss_GroupAge_avg)*100,1))
Loss_labels <- paste(Loss_labels, "%", sep = " ")
pie (Percentage_Loss_GroupAge_avg, main = "Total Loss by Group Age Above Avg", col = Warna, labels = Loss_labels, cex=0.8)
legend (1.5, 0.5, c ("12-25","26-45","46-65"), cex = 0.8, fill = Warna)


Loss_by_Driving_Experience_Avg <- Df_Claim_above_avg %>%
  group_by(Driving_Experience) %>%
  summarise(sum(Losses))

Loss_by_Driving_Experience_Avg

A2 <- Loss_by_Driving_Experience_Avg %>%
  mutate(percentage_loss_by_DrivingExperience = (Loss_by_Driving_Experience_Avg/sum(Loss_by_Driving_Experience_Avg$`sum(Losses)``)*100)
A2


percentage_loss_by_DrivingExperience <- c (43.8, 30.1, 26.1)
Warna <- c ("blue", "yellow", "green")
Loss_labels <- c(round(percentage_loss_by_DrivingExperience/sum(percentage_loss_by_DrivingExperience)*100,1))
Loss_labels <- paste(Loss_labels, "%", sep = " ")
pie (percentage_loss_by_DrivingExperience, main = "Total Loss by Driving Experience", col = Warna, labels = Loss_labels, cex=0.8)
legend (1.5, 0.5, c ("0-5","6-10","11-15"), cex = 0.8, fill = Warna)



#LOSS by Driving Experience & Customer Age above Avg

Age_vs_Experience_Avg <- Df_Claim_above_avg %>%
  group_by(AgeGroup, Driving_Experience) %>%
  summarise(Total_LOSS_Avg = sum(Losses))
Age_vs_Experience_Avg

Age_vs_Experience_Avg$Percentage_total_loss_avg <- (Age_vs_Experience_Avg$Total_LOSS_Avg/ sum(Age_vs_Experience_Avg$Total_LOSS_Avg)*100)
Age_vs_Experience_Avg


kG <- c (39.1, 10.3, 0.33, 4.89, 6.59, 6.46, 6.21, 1.77, 0.22, 4.85, 6.46, 5.92, 6.80)
Loss_labels <- c(round(kG/sum(kG)*100))
Loss_labels <- paste(Loss_labels, "%", sep = " ")
pie(kG, main = "Customer Age Group & Driving Experience VS Total Loss Above Avg", col = colors(distinct = F),labels = Loss_labels, cex= 0.6)
legend(1.5, 0.5, c("Remaja [0-5]", "Remaja [6-10]", "Dewasa [0-5]", "Dewasa [6-10]", "Dewasa [11-15]", "Dewasa [16-20]", "Dewasa [21-25]", "Dewasa [26-30", "Lansia [21-25]", "Lansia [26-30]", "Lansia [31-35]", "Lansia [36-40]","Lansia [>41]"), cex = 0.6, fill = colors())

#LOSS by Vehicle age group
Loss_by_Vehicleage_Avg <- Df_Claim_above_avg %>%
  group_by(Vehicle.Age_group) %>%
  summarise(sum(Losses))
Loss_by_Vehicleage_Avg

A2 <- Loss_by_Vehicleage_Avg %>%
  mutate(percentage_loss_by_Vehicle_age_avg = (Loss_by_Vehicleage_Avg/sum(Loss_by_Vehicleage_Avg$`sum(Losses)`)*100))
A2

Percentage_Loss_VehicleGroupAge_Avg <- c (43.8, 30.1, 26.1)
Warna <- c ("blue", "yellow", "green")
Loss_labels <- c(round(Percentage_Loss_VehicleGroupAge_Avg/sum(Percentage_Loss_VehicleGroupAge_Avg)*100,1))
Loss_labels <- paste(Loss_labels, "%", sep = " ")
pie (Percentage_Loss_VehicleGroupAge_Avg, main = "Total Loss by Vehicle Group Age Above Avg", col = Warna, labels = Loss_labels, cex=0.8)
legend (1.5, 0.5, c ("0-5","6-10","11-15"), cex = 0.8, fill = Warna)

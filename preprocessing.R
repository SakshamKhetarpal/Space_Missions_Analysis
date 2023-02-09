#installation of required packages
install.packages("readxl")
install.packages("writexl")
install.packages("dplyr")

#set the desired working directory
getwd()
setwd("C:/Users/OM/OneDrive/Desktop/SpaceMissions")

#reading the data set
library("readxl")
Station_Info<-read_excel("Station_Info.xlsx")
Status<-read_excel("Status.xlsx")
Satellite_Info<-read_excel("Satellite_Info.xlsx")
Payload_Info<-read_excel("Payload_Info.xlsx")


#1)pre-processing the Station_Info table

Station_Info
class(Station_Info$`Launch Time`)

#extracting time from Launch Time
Station_Info$`LaunchTime`<-format(as.POSIXct(Station_Info$`LaunchTime`), format = "%H:%M:%S")

#extracting date from Launch Date
Station_Info$`LaunchDate`<-format(as.POSIXct(Station_Info$`LaunchDate`), format = "%Y-%m-%d")
Station_Info

#replacing null values according to launch site
library("dplyr")
df_site<- group_by(Station_Info,LaunchSite)

#replacing null temperature values with mean temperature of corresponding launch sites
df_mean<-summarise(df_site,Temp=mean(Temperature, na.rm = TRUE))
View(df_mean)
for(x in df_mean$LaunchSite)
{
  a<-is.na(Station_Info$Temperature)
  b<-Station_Info$LaunchSite==x 
  Station_Info$Temperature[a&b]=round(df_mean$Temp[df_mean$LaunchSite==x],digits=0)
}

#replacing null wind speed values
df_max<-summarise(df_site,wind=max(WindSpeed, na.rm = TRUE))
for(y in df_max$LaunchSite)
{
  a<-is.na(Station_Info$WindSpeed)
  b<-Station_Info$LaunchSite==y
  Station_Info$WindSpeed[a&b]=round(df_max$wind[df_max$LaunchSite==y],digits=0)
}

#replacing null humidity values
df_med<-summarise(df_site,hum=median(Humidity, na.rm = TRUE))
for(z in df_med$LaunchSite)
{
  a<-is.na(Station_Info$Humidity)
  b<-Station_Info$LaunchSite==z
  Station_Info$Humidity[a&b]=round(df_med$hum[df_med$LaunchSite==z],digits=0)
}

#2)pre-processing the Status table

View(Status)

#finding the entry where the mission failed but reason is unavailable
Status[is.na(Status$`Failure Reason`)& Status$`Mission Status`=='Failure',]

#manually replacing the correct reason
Status[Status$Space_Id==1030,'Failure Reason']<-"Collision During Launch"

#3)pre-processing the Payload_Info table

Payload_Info[is.na(Payload_Info$`Payload_Type`),]$`Payload_Name`
Payload_Info[Payload_Info$`Payload_Name`=="RatSat (DemoSat)",]
Payload_Info[Payload_Info$`Payload_Name`=="RatSat (DemoSat)",3]<-"Mass simulator"
Payload_Info[Payload_Info$`Payload_Name`=="Dragon Spacecraft Qualification Unit",]
Payload_Info[Payload_Info$`Payload_Name`=="Dragon Spacecraft Qualification Unit",3]<-"Boilerplate Satellite"


library(stringr)
Payload_Info$Payload_Type<-str_to_title(Payload_Info$Payload_Type)

df_type<- group_by(Payload_Info,Payload_Type)
#replacing null payload mass values
df_mean1<-summarise(df_type,mass=mean(Payload_Mass_kg, na.rm = TRUE))
for(p in df_mean1$Payload_Type)
{
  a<-is.na(Payload_Info$Payload_Mass_kg)
  b<-Payload_Info$Payload_Type==p
  Payload_Info$Payload_Mass_kg[a&b]=round(df_mean1$mass[df_mean1$Payload_Type==p],digits=0)
}
#satellites with classified information
Payload_Info[is.na(Payload_Info$Payload_Mass_kg),]

#4)pre-processing the Satellite_Info table

df_type<- group_by(Satellite_Info,Vehicle_Type)
#replacing null thrust values
df_mean2<-summarise(df_type,thrust=mean(Liftoff_Thrust_kN, na.rm = TRUE))
for(q in df_mean2$Vehicle_Type)
{
  a<-is.na(Satellite_Info$Liftoff_Thrust_kN)
  b<-Satellite_Info$Vehicle_Type==q
  Satellite_Info$Liftoff_Thrust_kN[a&b]=round(df_mean2$thrust[df_mean2$Vehicle_Type==q],digits=0)
}

#exporting the final data frame to excel file
library("writexl")

#Q1) What are the average temperature details of the launch sites?
avg_temp<-Station_Info[,c(5,6)]
write_xlsx(avg_temp,"C:\\Users\\OM\\OneDrive\\Desktop\\SpaceMissions\\avg_temp.xlsx")

#Q2) What are the average Wind Speeds details of the launch sites?
avg_wind<-Station_Info[,c(5,7)]
write_xlsx(avg_wind,"C:\\Users\\OM\\OneDrive\\Desktop\\SpaceMissions\\avg_wind.xlsx")

#Q3)What are the average Humidity details of the launch sites?
avg_hum<-Station_Info[,c(5,8)]
write_xlsx(avg_hum,"C:\\Users\\OM\\OneDrive\\Desktop\\SpaceMissions\\avg_hum.xlsx")

#Q4) What are the different varieties of launch vehicles used by different companies?
v1<-Station_Info[,2]
v2<-Satellite_Info[,2]
vehicle<-cbind(v1,v2)
write_xlsx(vehicle,"C:\\Users\\OM\\OneDrive\\Desktop\\SpaceMissions\\vehicle_details.xlsx")

#Q5) What is the Mission Status of the companies?

#Q6) Track Records of the Launch Sites.

#compilation of the given data into one data frame using inner join
df1<-merge(Station_Info,Satellite_Info,by="Space_Id")
df2<-merge(Payload_Info,Status,by="Space_Id")

SpaceMissions<-merge(df1,df2,by="Space_Id")

write_xlsx(Station_Info,"C:\\Users\\OM\\OneDrive\\Desktop\\SpaceMissions\\SpaceMissions.xlsx")
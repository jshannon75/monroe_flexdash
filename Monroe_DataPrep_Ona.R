library(ona) #See http://onaio.github.io/ona.R/ for more on this package
MonroeData<-onaDownload("Monroe_Hsurvey_v9","communitymapuga","godawgs!")
Monroe_data<-data.frame(MonroeData)
Monroe_data$ParcelID<-as.character(Monroe_data$Parcel_ID)

#Correct parcel ID (some are only 8 characters, missing last three 0s)
Monroe_data$parcel_len<-nchar(Monroe_data$ParcelID)
Monroe_data[Monroe_data$parcel_len==8,]$ParcelID<-paste(Monroe_data[Monroe_data$parcel_len==8,]$ParcelID,"000",sep="")

#Read in parcels


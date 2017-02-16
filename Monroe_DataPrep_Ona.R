library(ona) #See http://onaio.github.io/ona.R/ for more on this package
MonroeData_v9<-onaDownload("Monroe_Hsurvey_v9","communitymapuga","godawgs!")
Monroe_data_v9<-data.frame(MonroeData_v9)
Monroe_data_v9$ParcelID<-as.character(Monroe_data_v9$Parcel_ID)
Monroe_data<-Monroe_data_v9

#Will add in version 8 later

#Correct parcel ID (some are only 8 characters, missing last three 0s)
Monroe_data$parcel_len<-nchar(Monroe_data$ParcelID)
table(Monroe_data$parcel_len)
Monroe_data[Monroe_data$parcel_len==8,]$ParcelID<-paste(Monroe_data[Monroe_data$parcel_len==8,]$ParcelID,"000",sep="")

# #Pull in prior dataset--Will eventually match records based on Ona's ID and only add new ones.
# Monroe_data_old<-read.csv("Monroe_surveydata_2016_11_08.csv")
# parcels_old<-cbind(data.frame(Monroe_data_old$Parcel_ID),1)
# names(parcels_old)<-c("Parcel_ID","Dummy") #Create list of parcels
# parcels_old<-unique(parcels_old)
# 
# #Delete records already formatted
# Monroe_data_join<-merge(Monroe_data,parcels_old,by="Parcel_ID",all.x=TRUE)
# Monroe_data_join[is.na(Monroe_data_join$Dummy),]$Dummy<-0
# Monroe_data_new<-subset(Monroe_data_join,Dummy==0)
# Monroe_data_new$Dummy<-NULL

#Join in parcel data
parcel.points <- read.csv("Monroe_parcels.csv",stringsAsFactors=FALSE)
survey.data<-merge(Monroe_data,parcel.points,by.x="ParcelID",by.y="ID")
survey.data<-survey.data[,c(1:3,5:49,61:62)]

column.names<-read.csv("Monroe_collabels.csv",header=FALSE) #May have to edit this file to match existing data
names(survey.data)<-column.names[,2]

#Get rid of NAs
survey.data[survey.data=="n/a"]<-"NA"
survey.data[,1:42][is.na(survey.data[,1:42])]<-"NA"
survey.data[,11:42][survey.data[,11:42]=="NA"]<-"False"

#Convert true/false to yes/no
survey.data[survey.data=="True"]<-"Yes"
survey.data[survey.data=="False"]<-"No"
survey.data[survey.data=="TRUE"]<-"Yes"
survey.data[survey.data=="FALSE"]<-"No"

#Count issues
survey.data$MIN_TOTAL<-rowSums(survey.data[12:26]=="Yes",na.rm=TRUE)
survey.data$MAJ_TOTAL<-rowSums(survey.data[28:35]=="Yes",na.rm=TRUE)
survey.data$MnMj_total<-survey.data$MIN_TOTAL+survey.data$MAJ_TOTAL

survey.data$Classify<-"Standard"
survey.data[survey.data$MIN_TOTAL>1|survey.data$MAJ_TOTAL>0,]$Classify<-"Substandard"
survey.data[survey.data$MIN_TOTAL>3|survey.data$MAJ_TOTAL>1,]$Classify<-"Dilapidated"

survey.data$other_tot<-rowSums(survey.data[36:42]=="Yes",na.rm=TRUE)

#Change streetlights and property type
survey.data$proptype[survey.data$proptype=="APARTM"]<-"Apartment"
survey.data$proptype[survey.data$proptype=="OTHER"]<-"Other"
survey.data$proptype[survey.data$proptype=="DUPLEX"]<-"Duplex"
survey.data$proptype[survey.data$proptype=="SF_NOGAR"]<-"Single family (no garage)"
survey.data$proptype[survey.data$proptype=="SF_GAR"]<-"Single family (garage)"
survey.data$proptype[survey.data$proptype=="MOB_HOME"]<-"Mobile home"
survey.data$proptype[survey.data$proptype=="VACANT"]<-"Vacant"


#Fill in pictures
survey.data$Pic1[survey.data$Pic1=="NA"]<-"None"
survey.data$Pic2[survey.data$Pic2=="NA"]<-"None"
survey.data$Pic3[survey.data$Pic3=="NA"]<-"None"
survey.data$Pic4[survey.data$Pic4=="NA"]<-"None"
survey.data$Comments[survey.data$Comments=="NA"]<-"None"

#Flag duplicate records and adjust lat long
parcel_count<-data.frame(table(survey.data$Parcel_ID))
names(parcel_count)<-c("Parcel_ID","ParcCount")

survey.data<-merge(survey.data,parcel_count,by="Parcel_ID")

survey.data1<-survey.data[survey.data$ParcCount>1,]
survey.data2<-survey.data[survey.data$ParcCount==1,]
random.lat<-data.frame(runif(nrow(survey.data1),0.0001,0.0008))
random.long<-data.frame(runif(nrow(survey.data1),0.0001,0.0008))
random<-cbind(random.lat,random.long)
names(random)<-c("random.lat","random.long")
survey.data1<-cbind(survey.data1,random)
survey.data1$lat<-survey.data1$lat+survey.data1$random.lat
survey.data1$long<-survey.data1$long+survey.data1$random.long
survey.data1$random.lat<-NULL
survey.data1$random.long<-NULL

survey.data<-rbind(survey.data1,survey.data2)
survey.data$rec_id<-1:nrow(survey.data)
survey.data$rec_id<-paste(survey.data$Parcel_ID,survey.data$rec_id,sep="")

write.csv(survey.data,"Monroe_surveydata_2016_11_08.csv")



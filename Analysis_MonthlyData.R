#################################################################################
#                  National Green Infrastructure Facility                       #
#     Priming Laboratory EXperiments on infrastructure and Urban Systems        #
#                                PLEXUS                                         #
#                   written by Anil Yildiz, Dr. sc.                             #
#                         Created on 03.07.2020                                 #
#                       Last edited on 06.07.2020                               #
#################################################################################
#-------------------------------------------------------------------------------#
#     Data used in this script is obtained from Yildiz & Stirling (2020).       #
#       Please refer to https://doi.org/10.25405/data.ncl.12593768.v2           #
#-------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------#
#                    Time frame presented in the paper                          #
#-------------------------------------------------------------------------------#
startdate <- as.POSIXct("2019-07-01 00:00:00",tz="UTC")-1
enddate <- as.POSIXct("2019-12-01 00:00:00",tz="UTC")-1
#-------------------------------------------------------------------------------#
#                      Importing data file from the repository                  #
#-------------------------------------------------------------------------------#
link <- "https://s3-eu-west-1.amazonaws.com/pstorage-ncl-8160713447/23654957/meteo_daily_v2.csv"
meteo <- read.csv(link,stringsAsFactors=F)
#-------------------------------------------------------------------------------#
#Subsetting the date frame to include data only from the time frame investigated#
#-------------------------------------------------------------------------------#
meteo <- meteo[which(as.POSIXct(meteo$Time,"UTC")>startdate & as.POSIXct(meteo$Time,"UTC")<enddate),]
limit <- as.POSIXct(as.character(seq.Date(as.Date(startdate+1),as.Date(enddate+1),"month")),"UTC")

meteo_monthly <- as.data.frame(matrix(NA,nrow=length(limit)-1,ncol=7))
meteo_monthly[,1] <- month.abb[as.numeric(substr(limit[c(1:(length(limit)-1))],start=6,stop=7))]

for(i in 1:(length(limit)-1))
{
   data <- meteo[which(as.POSIXct(meteo$Time,"UTC")>limit[i]-1 & as.POSIXct(meteo$Time,"UTC")<limit[i+1]),]
   assign(paste0(month.abb[as.numeric(substr(limit[i],start=6,stop=7))],substr(limit[i],start=3,stop=4)),data)
   meteo_monthly[i,2:6] <- round(colMeans(data[,sapply(data,is.numeric)])[c(2,5,8,9,10)],2)
   meteo_monthly[i,7] <- sum(data$Rain,na.rm=T)
   rm(list="data")
}

colnames(meteo_monthly) <- c("Month","AirTemp","Humid","Baro","WindSpeed","NetRad","Rain")

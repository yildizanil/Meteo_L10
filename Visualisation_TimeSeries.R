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
#-------------------------------------------------------------------------------#
#                         RGB codes of NGIF colours                             #
#-------------------------------------------------------------------------------#
green <- rgb(157/256,175/256,33/256)
blue <- rgb(32/256,137/256,203/256)
#-------------------------------------------------------------------------------#
#                         Axis locations and labels                             #
#-------------------------------------------------------------------------------#
axis_seq <- as.character(seq.Date(as.Date(startdate+1),as.Date(enddate+1),"month"))
axis_names <- paste0("01-",substr(axis_seq,start=6,stop=7))
axis_months <- as.POSIXct(axis_seq,"UTC")
axis_days <- seq(startdate,enddate,60*60*24)
#-------------------------------------------------------------------------------#
#                Generating a 1000-dpi figure in tiff format                    #
#-------------------------------------------------------------------------------#
pdf("Visualisation_TimeSeries.pdf",width=(150/25.4),height=(110/25.4),pointsize=10)
#-------------------------------------------------------------------------------#
#       Generating a panel layout consisting of two subfigures and a legend     #
#-------------------------------------------------------------------------------#
layout(matrix(c(1,2,3),nrow=3,ncol=1),heights=c(1,5,5))
#-------------------------------------------------------------------------------#
#                           Legend of the figure                                #
#-------------------------------------------------------------------------------#
par(mar=c(0.25,0.25,0.25,0.25),mgp=c(0.1,0.1,0),family="serif",ps=10,cex=1,cex.main=1,las=1)
plot(0,0,xlab=NA,ylab=NA,axes=F,pch="")
legend("center",c("Rainfall","Air temperature","Relative Humidity","Net radiation"),bty="n",hor=T,
       pch=c(15,NA,NA,NA),lwd=c(NA,2,2,2),col=c(8,2,blue,1),x.intersp=1,xjust = 0.5)
#-------------------------------------------------------------------------------#
#                Daily rainfall and range of air temperature                    #
#-------------------------------------------------------------------------------#
par(mar=c(2,2.5,0.25,2.5),mgp=c(0.1,0.1,0),family="serif",ps=10,cex=1,cex.main=1,las=1)
plot(0,0,type="l",lwd=3,ylim=c(0,40),xlim=c(startdate,enddate),axes=F,xlab=NA,ylab=NA,pch="")
segments(x0=seq(startdate,enddate,60*60*24),y0=0,x1=seq(startdate,enddate,60*60*24),y1=40,col=rgb(0,0,0,0.1),lty=1)
segments(x0=startdate-60*60*48,y0=seq(0,40,10),x1=enddate,y1=seq(0,40,10),col=rgb(0,0,0,0.1),lty=1)
axis(1,tck=0.02,at=axis_days,labels=NA)
axis(1,tck=0.04,at=axis_months,labels=axis_names)
axis(2,tck=0.02)     
for(i in 1:nrow(meteo))
{
   rect(xleft=as.POSIXct(meteo$Time[i],"UTC"),ybottom=0,
        xright=as.POSIXct(meteo$Time[i+1],"UTC"),ytop=meteo$Rain[i],border=T,col=8)  
}
text(enddate,40,"(a)",adj=c(1,1))
par(new=T)
plot(0,0,type="l",lwd=3,ylim=c(-2,30),xlim=c(startdate,enddate),axes=F,xlab=NA,ylab=NA,pch="")
axis(4,tck=0.02,at=seq(-2,30,8),labels=seq(-2,30,8))
polygon(x=c(as.POSIXct(meteo$Time,"UTC"),rev(as.POSIXct(meteo$Time,"UTC"))),
        y=c(meteo$AirTemp_Max,rev(meteo$AirTemp_Min)),
        border=NA,col=rgb(1,0,0,0.2))
lines(meteo$AirTemp_Mean~as.POSIXct(meteo$Time,"UTC"),col=2,lwd=2)
par(las=0)
box()
mtext("Time [dd-mm]",side=1,line=1)
mtext("Daily rainfall [mm]",side=2,line=1.25)
mtext("Air temperature [°C]",side=4,line=1.5)
#-------------------------------------------------------------------------------#
#                Net radiation and range of relative humidity                   #
#-------------------------------------------------------------------------------#
par(mar=c(2,2.5,0.25,2.5),mgp=c(0.1,0.1,0),family="serif",ps=10,cex=1,cex.main=1,las=1)
plot(0,0,type="l",lwd=3,ylim=c(0,100),xlim=c(startdate,enddate),axes=F,xlab=NA,ylab=NA,pch="")
segments(x0=seq(startdate,enddate,60*60*24),y0=0,x1=seq(startdate,enddate,60*60*24),y1=100,col=rgb(0,0,0,0.1),lty=1)
segments(x0=startdate,y0=seq(0,100,20),x1=enddate,y1=seq(0,100,20),col=rgb(0,0,0,0.1),lty=1)
axis(1,tck=0.02,at=axis_days,labels=NA)
axis(1,tck=0.04,at=axis_months,labels=axis_names)
axis(2,tck=0.02)
polygon(x=c(as.POSIXct(meteo$Time,"UTC"),rev(as.POSIXct(meteo$Time,"UTC"))),
        y=c(meteo$Humid_Max,rev(meteo$Humid_Min)),
        border=NA,col=rgb(32/256,137/256,203/256,0.2))
lines(meteo$Humid_Mean~as.POSIXct(meteo$Time,"UTC"),col=blue,lwd=2)
text(enddate,100,"(b)",adj=c(1,1))
par(new=T)
plot(0,0,type="l",lwd=3,ylim=c(-50,200),xlim=c(startdate,enddate),axes=F,xlab=NA,ylab=NA,pch="")
axis(4,tck=0.02,at=seq(-50,200,50))
lines(meteo$NetRad~as.POSIXct(meteo$Time,"UTC"),col=1,lwd=2)
par(las=0)
par(las=0)
mtext("Time [dd-mm]",side=1,line=1)
mtext("Relative humidity [%]",side=2,line=1.25)
mtext(expression(paste("Net radiation [W/m"^2,"]")),side=4,line=1.5)
box()

dev.off()






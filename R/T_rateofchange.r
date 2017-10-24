#' A Function characterizes rate of change of stream temperature
#'
#' This function cacluate metrics that characterize rate of change of stream temperature
#' @param sitedata stream monitoring site data in SiteID, Date(in as.Date format), 
#' MaxT, MinT, MeanT.
#' @param TlengthPortion portion of length that required for calculating metric summaries,
#' defaulty=2/3
#' @param SeasonSp define spring season, default as March, April, and May, c(3,4,5)
#' @param SeasonSu define summer season, default as June, July, August, c(6,7,8)
#' @param SeasonFa define fall season, default as September, October, November, c(9,10,11)
#' @param SeasonWi define winter season, default as December, Januray, February, c(12,1,2)
#' @keywords rate of change
#' @export
#' @examples
#' install.packages("dataRetrieval")
#' library(dataRetrieval)
#' ExUSGSStreamTemp<-readNWISdv("01382310","00010","2011-01-01","2011-12-31",c("00001","00002","00003"))
#' sitedata<-subset(ExUSGSStreamTemp, select=c("site_no","Date","X_00010_00001","X_00010_00002","X_00010_00003"))
#' names(sitedata)<-c("siteID","Date","MaxT","MinT","MeanT")
#' T_rateofchange(sitedata)

T_rateofchange <- function(sitedata,TlengthPortion=2/3,
                           SeasonSp=c(3,4,5), SeasonSu=c(6,7,8),
                           SeasonFa=c(9,10,11),SeasonWi=c(12,1,2)){
  library(labdsv)
  library(stringr)
  library(zoo)

  SiteInfo<-subset(sitedata,select=-c(Date,MaxT,MinT,MeanT))
  SiteInfo<-SiteInfo[1]
  
# timing of maximum mean and maximum temperature
# take average if multiple years

#Monthly------------------------------------------------------------------------
RateOfChangeMonth<-function(sitedata,y,TlengthPortion){
  RC12month<-c()
  # find months
  mo<-as.numeric(format(sitedata$Date,"%m"))
  for(jj in 1:12){
    i_mo<-which(mo==jj)
    if (length(i_mo)>=30*TlengthPortion){
      maxDmean<-max(na.omit(sitedata$MeanT[i_mo]))
      minDmean<-min(na.omit(sitedata$MeanT[i_mo]))
      
      #there are times, more than one day has the maximum, we decide to pick the first day of them
      i_MaxDmean<-which(sitedata$MeanT[i_mo]==maxDmean)[1]
      i_MinDmean<-which(sitedata$MeanT[i_mo]==minDmean)[1]
      
      JDmaxMeanT<-julian(sitedata$Date[i_mo[i_MaxDmean]],origin = as.Date(paste(y,"-01-01",sep="")))
      JDminMeanT<-julian(sitedata$Date[i_mo[i_MinDmean]],origin = as.Date(paste(y,"-01-01",sep="")))
      
      RCmonth<-abs((maxDmean-minDmean)/(JDmaxMeanT-JDminMeanT))
      
    }else{
      RCmonth<-NA
    }

    RC12month<-c(RC12month,RCmonth)
  }
  SiteMonthlyMetrics<-c(RC12month)
  SiteMonthlyMetrics<-matrix(SiteMonthlyMetrics,nrow=1)
  return(SiteMonthlyMetrics)
}

#average multiple years
RC12month<-c()
m_years<-unique(format(sitedata$Date,"%Y"))
for (y in m_years){
  i_year<-which(format(sitedata$Date,"%Y")==y)
  sitedata_year<-sitedata[i_year,]
  
  RC12month_temp<-RateOfChangeMonth(sitedata_year,y,TlengthPortion)
  RC12month<-rbind(RC12month,RC12month_temp)
  
}

RC12month<-colMeans(RC12month,na.rm=TRUE)

SiteMonthlyMetrics<-c(RC12month)


#Seasonal-----------------------------------------------------------------------
RateOfChangeSeason<-function(sitedata,season,y,TlengthPortion){
  mo<-as.numeric(format(sitedata$Date,"%m"))
  i_season<-c()
  seasondays<-0
  for(ii in season){
    i_temp<-which(mo==ii)
    i_season<-c(i_season,i_temp)
    days<-monthDays(as.Date(paste("1990-",ii,"-01",sep="")))
    seasondays<-seasondays+days
  }
  
  if(length(i_season)>=seasondays*TlengthPortion){
    
    maxDmean<-max(na.omit(sitedata$MeanT[i_season]))
    minDmean<-min(na.omit(sitedata$MeanT[i_season]))
    
    #there are times, more than one day has the maximum, we decide to pick the first day of them
    i_MaxDmean<-which(sitedata$MeanT[i_season]==maxDmean)[1]
    i_MinDmean<-which(sitedata$MeanT[i_season]==minDmean)[1]
    
    JDmaxMeanT<-julian(sitedata$Date[i_season[i_MaxDmean]],origin = as.Date(paste(y,"-01-01",sep="")))
    JDminMeanT<-julian(sitedata$Date[i_season[i_MinDmean]],origin = as.Date(paste(y,"-01-01",sep="")))
    
    RCseason<-abs((maxDmean-minDmean)/(JDmaxMeanT-JDminMeanT))
    
  }else{
    RCseason<-NA
  }
  return(RCseason)
}

RC_sp<-c()
RC_su<-c()
RC_fa<-c()
RC_wi<-c()
m_years<-unique(format(sitedata$Date,"%Y"))
for (y in m_years){
  i_year<-which(format(sitedata$Date,"%Y")==y)
  sitedata_year<-sitedata[i_year,]
  #spring
  RC_sp_temp<-RateOfChangeSeason(sitedata_year,SeasonSp,y,TlengthPortion)
  RC_sp<-rbind(RC_sp,RC_sp_temp)
  #summer
  RC_su_temp<-RateOfChangeSeason(sitedata_year,SeasonSu,y,TlengthPortion)
  RC_su<-rbind(RC_su,RC_su_temp)
  #fall
  RC_fa_temp<-RateOfChangeSeason(sitedata_year,SeasonFa,y,TlengthPortion)
  RC_fa<-rbind(RC_fa,RC_fa_temp)
  #winter
  RC_wi_temp<-RateOfChangeSeason(sitedata_year,SeasonWi,y,TlengthPortion)
  RC_wi<-rbind(RC_wi,RC_wi_temp)
}

RCsp<-colMeans(RC_sp,na.rm=TRUE)
RCsu<-colMeans(RC_su,na.rm=TRUE)
RCfa<-colMeans(RC_fa,na.rm=TRUE)
RCwi<-colMeans(RC_wi,na.rm=TRUE)

SiteSeasonMetrics<-c(RCsp,
                     RCsu,
                     RCfa, 
                     RCwi)

# collect all the metrics-----------------------------------------------------

SiteMetrics<-matrix(c(SiteInfo[1],SiteMonthlyMetrics,SiteSeasonMetrics),nrow=1, ncol=17)
                           

colnames(SiteMetrics)<-c("siteinfo",
                         paste("RC",1:12,sep=""),
                         "RCsp",
                         "RCsu",
                         "RCfa",
                         "RCwi")
return(SiteMetrics)
	
}

#==============================================================================================================================================================


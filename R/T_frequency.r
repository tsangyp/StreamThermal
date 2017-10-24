#' A function characterizes frequency of stream thermal regime
#'
#' This function summarize metrics of stream thermal frequency.
#' @param sitedata stream monitoring site data in SiteID, Date(in as.Date format), 
#' MaxT, MinT, MeanT.
#' @param cT critical temperature of interest, default = 16 (Celsius)
#' @param TlengthPortion portion of length that required for calculating metric summaries,
#' defaulty=2/3
#' @param SeasonSp define spring season, default as March, April, and May, c(3,4,5)
#' @param SeasonSu define summer season, default as June, July, August, c(6,7,8)
#' @param SeasonFa define fall season, default as September, October, November, c(9,10,11)
#' @param SeasonWi define winter season, default as December, Januray, February, c(12,1,2)
#' @keywords timing
#' @export
#' @examples
#' install.packages("dataRetrieval")
#' library(dataRetrieval)
#' ExUSGSStreamTemp<-readNWISdv("01382310","00010","2011-01-01","2011-12-31",c("00001","00002","00003"))
#' sitedata<-subset(ExUSGSStreamTemp, select=c("site_no","Date","X_00010_00001","X_00010_00002","X_00010_00003"))
#' names(sitedata)<-c("siteID","Date","MaxT","MinT","MeanT")
#' T_frequency(sitedata)


T_frequency <- function(sitedata, cT=16, TlengthPortion=2/3,
                        SeasonSp=c(3,4,5), SeasonSu=c(6,7,8),
                        SeasonFa=c(9,10,11), SeasonWi=c(12,1,2)){

  library(zoo)

  SiteInfo<-subset(sitedata,select=-c(Date,MaxT,MinT,MeanT))
  SiteInfo<-SiteInfo[1]
#Monthly------------------------------------------------------------------------
  FrequencyMonth<-function(sitedata,cT,y,TlengthPortion){
    FmaxcT<-c()
    FmincT<-c()
    FmeancT<-c()
    mo<-as.numeric(format(sitedata$Date,"%m"))
    for(jj in 1:12){
      i_mo<-which(mo==jj)
      if (length(i_mo)>=30*TlengthPortion){
        
        i_MaxcT<-which(sitedata[i_mo,]$MaxT>cT)
        i_MincT<-which(sitedata[i_mo,]$MinT>cT)
        i_MeancT<-which(sitedata[i_mo,]$MeanT>cT)
        FmaxcT_mo<-length(i_MaxcT)
        FmincT_mo<-length(i_MincT)
        FmeancT_mo<-length(i_MeancT)
        
      }else{
        
        FmaxcT_mo<-NA
        FmincT_mo<-NA
        FmeancT_mo<-NA
        
      }
      FmaxcT<-c(FmaxcT, FmaxcT_mo)
      FmincT<-c(FmincT, FmincT_mo)
      FmeancT<-c(FmeancT, FmeancT_mo)

    }
    SiteMonthlyMetrics<-c(FmaxcT,FmincT,FmeancT)
    SiteMonthlyMetrics<-matrix(SiteMonthlyMetrics,nrow=1)
    return(SiteMonthlyMetrics)
  }
  
  #average multiple years
  FcT12month<-c()

  m_years<-unique(format(sitedata$Date,"%Y"))
  for (y in m_years){
    i_year<-which(format(sitedata$Date,"%Y")==y)
    sitedata_year<-sitedata[i_year,]
    
    FcT12month_temp<-FrequencyMonth(sitedata_year,cT,y,TlengthPortion)
    FcT12month<-rbind(FcT12month,FcT12month_temp)
    
  }
  
  FcT12month<-colMeans(FcT12month,na.rm=TRUE)
  
  SiteMonthlyMetrics<-c(FcT12month)
  
#Seasonal-----------------------------------------------------------------------
  monthdays<-function(month){
    if(month==2){
      MonthDays<-28
    }else if(month==1|month==3|month==5|month==7|month==8|month==10|month==12){
      MonthDays<-31
    }else{
      MonthDays<-30
    }
    return(MonthDays)
  }
  
 FrequencySeason<-function(sitedata,season,cT, y,TlengthPortion){
    mo<-as.numeric(format(sitedata$Date,"%m"))
    i_season<-c()
    seasondays<-0
    for(ii in season){
      i_temp<-which(mo==ii)
      i_season<-c(i_season,i_temp)
      days<-monthdays(as.Date(paste("1990-",ii,"-01",sep="")))
      seasondays<-seasondays+days
    }
    
    if(length(i_season)>=seasondays*TlengthPortion){
      
      i_MaxcT<-which(sitedata[i_season,]$MaxT>cT)
      i_MincT<-which(sitedata[i_season,]$MinT>cT)
      i_MeancT<-which(sitedata[i_season,]$MeanT>cT)
      FmaxcT_season<-length(i_MaxcT)
      FmincT_season<-length(i_MincT)
      FmeancT_season<-length(i_MeancT)
      
    }else{
      FmaxcT_season<-NA
      FmincT_season<-NA
      FmeancT_season<-NA
    }
    return(c(FmaxcT_season, FmincT_season, FmeancT_season))
  }
  
  FcT_sp<-c()
  FcT_su<-c()
  FcT_fa<-c()
  FcT_wi<-c()
  m_years<-unique(format(sitedata$Date,"%Y"))
  for (y in m_years){
    i_year<-which(format(sitedata$Date,"%Y")==y)
    sitedata_year<-sitedata[i_year,]
    #spring
    FcT_sp_temp<-FrequencySeason(sitedata_year,SeasonSp,cT,y,TlengthPortion)
    FcT_sp<-rbind(FcT_sp,FcT_sp_temp)
    #summer
    FcT_su_temp<-FrequencySeason(sitedata_year,SeasonSu,cT,y,TlengthPortion)
    FcT_su<-rbind(FcT_su,FcT_su_temp)
    #fall
    FcT_fa_temp<-FrequencySeason(sitedata_year,SeasonFa,cT,y,TlengthPortion)
    FcT_fa<-rbind(FcT_fa,FcT_fa_temp)
    #winter
    FcT_wi_temp<-FrequencySeason(sitedata_year,SeasonWi,cT,y,TlengthPortion)
    FcT_wi<-rbind(FcT_wi,FcT_wi_temp)
  }
  
  FcTsp<-colMeans(FcT_sp,na.rm=TRUE)
  FcTsu<-colMeans(FcT_su,na.rm=TRUE)
  FcTfa<-colMeans(FcT_fa,na.rm=TRUE)
  FcTwi<-colMeans(FcT_wi,na.rm=TRUE)
  
  SiteSeasonMetrics<-c(FcTsp,
                       FcTsu,
                       FcTfa, 
                       FcTwi)
  
  # collect all the metrics-----------------------------------------------------
  
  SiteMetrics<-matrix(c(SiteInfo[1],SiteMonthlyMetrics,SiteSeasonMetrics),nrow=1, ncol=49)
  
  
  colnames(SiteMetrics)<-c("siteinfo",
                           paste("FmaxcT",1:12,sep=""),
                           paste("FmincT",1:12,sep=""),
                           paste("FmeancT",1:12,sep=""),
                           "FmaxcTsp","FmincTsp","FmeancTsp",
                           "FmaxcTsu","FmincTsu","FmeancTsu",
                           "FmaxcTfa","FmincTfa","FmeancTfa",
                           "FmaxcTwi","FmincTwi","FmeancTwi")
  

  
  return(SiteMetrics)	
}
  
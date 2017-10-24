#' A function characterizes timing of stream temperature 
#'
#' This function summarize the timing of a variety metrics.
#' @param sitedata stream monitoring site data in SiteID, Date(in as.Date format), 
#' MaxT, MinT, MeanT.
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
#' T_timing(sitedata)


T_timing <- function(sitedata, TlengthPortion=2/3, 
                              SeasonSp=c(3,4,5), SeasonSu=c(6,7,8),
                              SeasonFa=c(9,10,11),SeasonWi=c(12,1,2)){

  library(zoo)

  SiteInfo<-sitedata[1,1]

# timing of maximum mean and maximum temperature
# take average if multiple years

#Monthly------------------------------------------------------------------------
TimingMonth<-function(sitedata,y,TlengthPortion){
  JDmaxMaxT_mo<-c()
  JDminMinT_mo<-c()
  JDmaxMeanT_mo<-c()
  # find months
  mo<-as.numeric(format(sitedata$Date,"%m"))
  for(jj in 1:12){
    i_mo<-which(mo==jj)

    if (length(i_mo)>=30*TlengthPortion){
      
      #there are times, more than one day has the maximum, we decide to pick the first day of them
      i_MaxDmax<-which(sitedata$MaxT[i_mo]==max(na.omit(sitedata$MaxT[i_mo])))[1]
      i_MinDmin<-which(sitedata$MinT[i_mo]==min(na.omit(sitedata$MinT[i_mo])))[1]
      i_MaxDmean<-which(sitedata$MeanT[i_mo]==max(na.omit(sitedata$MeanT[i_mo])))[1]
      
      JDmaxMaxT_temp<-julian(sitedata$Date[i_mo[i_MaxDmax]],origin = as.Date(paste(y,"-01-01",sep="")))
      JDminMinT_temp<-julian(sitedata$Date[i_mo[i_MinDmin]],origin = as.Date(paste(y,"-01-01",sep="")))
      JDmaxMeanT_temp<-julian(sitedata$Date[i_mo[i_MaxDmean]],origin = as.Date(paste(y,"-01-01",sep="")))
      
    }else{
      JDmaxMaxT_temp<-NA
      JDminMinT_temp<-NA
      JDmaxMeanT_temp<-NA
    }

    JDmaxMaxT_mo<-c(JDmaxMaxT_mo,JDmaxMaxT_temp)
    JDminMinT_mo<-c(JDminMinT_mo,JDminMinT_temp)
    JDmaxMeanT_mo<-c(JDmaxMeanT_mo,JDmaxMeanT_temp)  
  }
  
  SiteMonthlyMetrics<-c(JDmaxMaxT_mo,JDminMinT_mo,JDmaxMeanT_mo)
  SiteMonthlyMetrics<-matrix(SiteMonthlyMetrics,nrow=1)
  return(SiteMonthlyMetrics)
}

#average multiple years
JD<-c()
m_years<-unique(format(sitedata$Date,"%Y"))
for (y in m_years){
  i_year<-which(format(sitedata$Date,"%Y")==y)
  sitedata_year<-sitedata[i_year,]
  
  JD_temp<-TimingMonth(sitedata_year,y,TlengthPortion)
  JD<-rbind(JD,JD_temp)
}

JD<-colMeans(JD,na.rm=TRUE)

SiteMonthlyMetrics<-c(JD)

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

TimingSeason<-function(sitedata,season,y,TlengthPortion){
  mo<-as.numeric(format(sitedata$Date,"%m"))
  i_season<-c()
  seasondays<-0
  for(ii in season){
    i_temp<-which(mo==ii)
    i_season<-c(i_season,i_temp)
    days<-monthdays(ii)
    seasondays<-seasondays+days
  }
  
  if(length(i_season)>=seasondays*TlengthPortion){
    
    #there are times, more than one day has the maximum, we decide to pick the first day of them
    i_MaxDmax<-which(sitedata$MaxT[i_season]==max(na.omit(sitedata$MaxT[i_season])))[1]
    i_MinDmin<-which(sitedata$MinT[i_season]==min(na.omit(sitedata$MinT[i_season])))[1]
    i_MaxDmean<-which(sitedata$MeanT[i_season]==max(na.omit(sitedata$MeanT[i_season])))[1]
    
    JDmaxMaxT<-julian(sitedata$Date[i_season[i_MaxDmax]],origin = as.Date(paste(y,"-01-01",sep="")))
    JDminMinT<-julian(sitedata$Date[i_season[i_MinDmin]],origin = as.Date(paste(y,"-01-01",sep="")))
    JDmaxMeanT<-julian(sitedata$Date[i_season[i_MaxDmean]],origin = as.Date(paste(y,"-01-01",sep="")))
    
  }else{
    JDmaxMaxT<-NA
    JDminMinT<-NA
    JDmaxMeanT<-NA
  }
  return(c(JDmaxMaxT, JDminMinT, JDmaxMeanT))
}

#average multiple years
Jmaxminmean_sp<-c()
Jmaxminmean_su<-c()
Jmaxminmean_fa<-c()
Jmaxminmean_wi<-c()
m_years<-unique(format(sitedata$Date,"%Y"))
for (y in m_years){
  i_year<-which(format(sitedata$Date,"%Y")==y)
  sitedata_year<-sitedata[i_year,]
  #spring
  Jmaxminmean_sp_temp<-TimingSeason(sitedata_year,SeasonSp,y,TlengthPortion)
  Jmaxminmean_sp<-rbind(Jmaxminmean_sp,Jmaxminmean_sp_temp)
  #summer
  Jmaxminmean_su_temp<-TimingSeason(sitedata_year,SeasonSu,y,TlengthPortion)
  Jmaxminmean_su<-rbind(Jmaxminmean_su,Jmaxminmean_su_temp)
  #fall
  Jmaxminmean_fa_temp<-TimingSeason(sitedata_year,SeasonFa,y,TlengthPortion)
  Jmaxminmean_fa<-rbind(Jmaxminmean_fa,Jmaxminmean_fa_temp)
  #winter
  Jmaxminmean_wi_temp<-TimingSeason(sitedata_year,SeasonWi,y,TlengthPortion)
  Jmaxminmean_wi<-rbind(Jmaxminmean_wi,Jmaxminmean_wi_temp)
}

Jmaxminmean_sp<-colMeans(Jmaxminmean_sp,na.rm=TRUE)
Jmaxminmean_su<-colMeans(Jmaxminmean_su,na.rm=TRUE)
Jmaxminmean_fa<-colMeans(Jmaxminmean_fa,na.rm=TRUE)
Jmaxminmean_wi<-colMeans(Jmaxminmean_wi,na.rm=TRUE)

SiteSeasonMetrics<-c(Jmaxminmean_sp,
                     Jmaxminmean_su,
                     Jmaxminmean_fa,
                     Jmaxminmean_wi)

#Timing of moving average-------------------------------------------------------
  # ID the logest data within the range with missing days no more than 5 days
  # find the consecutive data
  notmissing<-sitedata[!is.na(sitedata$MeanT),]
  constart<-notmissing[c(1,diff(notmissing$Date))>5,]$Date		
  constart<-c(min(notmissing$Date),constart)
  conend<-notmissing[diff(notmissing$Date)>5,]$Date
  conend<-c(conend,max(notmissing$Date))
  conend<-conend[1:length(constart)]
  condays<-conend-constart
  i_longest<-which(condays==max(condays))
  
  ## timing of maximum moving average of daily mean(a), daily maximum (b) and daily range
  # 1-- maximum of 30 days moving average of daily a. mean b. maximum c.daily range
  # 2-- maximum of 21 days moving average of daily a. mean b. maximum c.daily range
  # 3-- maximum of 14 days moving average of daily a. mean b. maximum c.daily range
  # 4-- maximum of 7 days moving average of daily a. mean b. maximum c.daily range
  # 5-- maximum of 3 days moving average of daily a. mean b. maximum c.daily range
  # 6-- maximum of 1 days moving average of daily a. mean b. maximum c.daily range <- no need YPT 2012.7.14
  
  if(i_longest>=1){
    for(jj in i_longest){
      i_start<-which(sitedata$Date==constart[jj])
      i_end<-which(sitedata$Date==conend[jj])
      sitedata_consec5<-sitedata[i_start:i_end,]
      
      windowdays<-c(30, 21, 14, 7, 3)
      
      JDMMAMeanT<-vector("list", length(windowdays))
      JDMMAMaxT<-vector("list", length(windowdays))
      JDMMADRT<-vector("list", length(windowdays))
      
      SiteMovingMetrics<-data.frame(1)
      # maximum moving average of (a,b,c) in different moving windows
      for (dd in 1:length(windowdays)){			
        
        # each site might have multiple years
        m_years<-unique(format(sitedata$Date[i_start:i_end],"%Y"))
        for(y in m_years){
          i_year<-which(format(sitedata_consec5$Date,"%Y")==y)
          
          #YPT 2015.2.20--
          #moving metrics is used to characterizing thermal pattens in warmest time
          #therefore, make sure the moving window include at least one summer month
          mo<-as.numeric(format(sitedata_consec5$Date[i_year],"%m"))
          #--YPT 2015.2.24 decide to take out the limitation of checking summer months
          #if(sum(mo%in%SeasonSu)>=1){
            
            # a. daily mean
            if(length(na.omit(sitedata_consec5$MeanT[i_year]))>=windowdays[dd]){
              zoo_MeanT<-zoo(sitedata_consec5$MeanT[i_year],as.Date(sitedata_consec5$Date[i_year]))
              MovingMeanT<-rollmean(na.omit(zoo_MeanT),windowdays[dd],align="center")
              
              i_movingmeanT<-index(MovingMeanT)
              i_MMmeanT<-i_movingmeanT[which(MovingMeanT==max(MovingMeanT))]
              
              #MaxMovingAMeanT[[dd]]<-c(MaxMovingAMeanT[[dd]],max(MovingMeanT))
              JDMMAMeanT[[dd]]<-c(JDMMAMeanT[[dd]],julian(i_MMmeanT,origin = as.Date(paste(y,"-01-01",sep=""))))
            }
            # b. daily maximum
            if(length(na.omit(sitedata_consec5$MaxT[i_year]))>=windowdays[dd]){
              zoo_MaxT<-zoo(sitedata_consec5$MaxT[i_year],as.Date(sitedata_consec5$Date[i_year]))
              MovingMaxT<-rollmean(na.omit(zoo_MaxT),windowdays[dd],align="center")
              
              i_movingmaxT<-index(MovingMaxT)
              i_MMmaxT<-i_movingmaxT[which(MovingMaxT==max(MovingMaxT))]
              
              #MaxMovingAMaxT[[dd]]<-c(MaxMovingAMaxT[[dd]],max(MovingMaxT))
              JDMMAMaxT[[dd]]<-c(JDMMAMaxT[[dd]],julian(i_MMmaxT,origin = as.Date(paste(y,"-01-01",sep=""))))
            }
            
            # c. daily range
            DRT<-sitedata_consec5$MaxT[i_year]-sitedata_consec5$MinT[i_year]
            if(length(na.omit(DRT))>=windowdays[dd]){
              zoo_DRT<-zoo(DRT,as.Date(sitedata_consec5$Date[i_year]))
              MovingDRT<-rollmean(na.omit(zoo_DRT),windowdays[dd],align="center")
              
              i_movingDRT<-index(MovingDRT)
              i_MMDRT<-i_movingDRT[which(MovingDRT==max(MovingDRT))]
              
              #MaxMovingADRT[[dd]]<-c(MaxMovingADRT[[dd]],max(MovingDRT))
              JDMMADRT[[dd]]<-c(JDMMADRT[[dd]],julian(i_MMDRT,origin = as.Date(paste(y,"-01-01",sep=""))))
            }
          
          #}
        }
        JDMMAMeanT[[dd]]<-mean(JDMMAMeanT[[dd]])
        JDMMAMaxT[[dd]]<-mean(JDMMAMaxT[[dd]])
        JDMMADRT[[dd]]<-mean(JDMMADRT[[dd]])
      
        SiteMovingMetrics<-data.frame(SiteMovingMetrics,
                                      JDMMAMeanT[[dd]],
                                      JDMMAMaxT[[dd]],
                                      JDMMADRT[[dd]])	
      }
      SiteMovingMetrics<-data.frame(SiteMovingMetrics[,2:length(SiteMovingMetrics)])
      
    }
    
  }else{
    
    SiteMovingMetrics<-as.data.frame(matrix(rep(NA,15),ncol=15))
  }

  SiteMovingMetrics<-as.numeric(SiteMovingMetrics)
# collect all the metrics-----------------------------------------------------

  SiteMetrics<-matrix(c(SiteMonthlyMetrics,SiteSeasonMetrics,SiteMovingMetrics),nrow=1,ncol=63)
  
  colnames(SiteMetrics)<-c(
                           paste("JDmaxMaxT",1:12,sep=""),paste("JDminMinT",1:12,sep=""),paste("JDmaxMeanT",1:12,sep=""),
                           "JDmaxMaxTSp","JDminMinTSp","JDmaxMeanTSp",
                           "JDmaxMaxTSu","JDminMinTSu","JDmaxMeanTSu",
                           "JDmaxMaxTFa","JDminMinTFa","JDmaxMeanTFa",
                           "JDmaxMaxTWi","JDminMinTWi","JDmaxMeanTWi",
                           "JDM30MAMeanT","JDM30MAMaxT","JDM30MADRT",
                           "JDM21MAMeanT","JDM21MAMaxT","JDM21MADRT",
                           "JDM14MAMeanT","JDM14MAMaxT","JDM14MADRT",
                           "JDM7MAMeanT","JDM7MAMaxT","JDM7MADRT",
                           "JDM3MAMeanT","JDM3MAMaxT","JDM3MADRT")
  SiteMetrics<-data.frame(SiteInfo,SiteMetrics,stringsAsFactors=FALSE)
  return(SiteMetrics)

}

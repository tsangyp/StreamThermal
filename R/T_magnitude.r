#' A Function characterizes magnitude of stream temperature
#'
#' This function cacluate metrics that characterize magnitude of stream temperature
#' @param sitedata stream monitoring site data in SiteID, Date(in as.Date format), 
#' MaxT, MinT, MeanT.
#' @param TlengthPortion portion of length that required for calculating metric summaries,
#' defaulty=2/3
#' @param SeasonSp define spring season, default as March, April, and May, c(3,4,5)
#' @param SeasonSu define summer season, default as June, July, August, c(6,7,8)
#' @param SeasonFa define fall season, default as September, October, November, c(9,10,11)
#' @param SeasonWi define winter season, default as December, Januray, February, c(12,1,2)
#' @keywords magnitude
#' @export
#' @examples 
#' install.packages("dataRetrieval")
#' library(dataRetrieval)
#' ExUSGSStreamTemp<-readNWISdv("01382310","00010","2011-01-01","2011-12-31",c("00001","00002","00003"))
#' sitedata<-subset(ExUSGSStreamTemp, select=c("site_no","Date","X_00010_00001","X_00010_00002","X_00010_00003"))
#' names(sitedata)<-c("siteID","Date","MaxT","MinT","MeanT")
#' T_magnitude(sitedata)

T_magnitude <- function(sitedata, TlengthPortion=2/3, 
                        SeasonSp=c(3,4,5), SeasonSu=c(6,7,8),
                        SeasonFa=c(9,10,11),SeasonWi=c(12,1,2)){
library(zoo)

SiteInfo<-subset(sitedata,select=-c(Date,MaxT,MinT,MeanT))
SiteInfo<-SiteInfo[1]

#Monthly------------------------------------------------------------------------
MagnitudeMonth<-function(sitedata,y,TlengthPortion){
  ADmax<-c()
  ADmin<-c()
  ADmean<-c()
  # find months
  mo<-as.numeric(format(sitedata$Date,"%m"))
  for(jj in 1:12){
    i_mo<-which(mo==jj)
    if (length(i_mo)>=30*TlengthPortion){
      
      ADmax_mo<-mean(na.omit(sitedata$MaxT[i_mo]))
      ADmin_mo<-mean(na.omit(sitedata$MinT[i_mo]))
      ADmean_mo<-mean(na.omit(sitedata$MeanT[i_mo]))
      
    }else{
      ADmax_mo<-NA
      ADmin_mo<-NA
      ADmean_mo<-NA
    }
    
    ADmax<-c(ADmax,ADmax_mo)
    ADmin<-c(ADmin,ADmin_mo)
    ADmean<-c(ADmean,ADmean_mo)
  }
  SiteMonthlyMetrics<-c(ADmax,ADmin,ADmean)
  SiteMonthlyMetrics<-matrix(SiteMonthlyMetrics,nrow=1)
  return(SiteMonthlyMetrics)
}

#average multiple years
MA12month<-c()
m_years<-unique(format(sitedata$Date,"%Y"))
for (y in m_years){
  i_year<-which(format(sitedata$Date,"%Y")==y)
  sitedata_year<-sitedata[i_year,]
  
  MA12month_temp<-MagnitudeMonth(sitedata_year,y,TlengthPortion)
  MA12month<-rbind(MA12month,MA12month_temp)
  
}

MA12month<-colMeans(MA12month,na.rm=TRUE)

SiteMonthlyMetrics<-c(MA12month)



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

MagnitudeSeason<-function(sitedata,season,y,TlengthPortion){
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
    
    SeasonMaxDmean<-max(na.omit(sitedata$MeanT[i_season]))
    SeasonMinDmean<-min(na.omit(sitedata$MeanT[i_season]))
    SeasonAvgDmean<-mean(na.omit(sitedata$MeanT[i_season]))
    
  }else{
    SeasonMaxDmean<-NA
    SeasonMinDmean<-NA
    SeasonAvgDmean<-NA
    
  }
  #found no nonmissing values to max or min, returning -Inf
  SeasonMaxDmean<-ifelse(SeasonMaxDmean!=-Inf,SeasonMaxDmean,NA)
  
  return(c(SeasonMaxDmean,SeasonMinDmean,SeasonAvgDmean))
}

MA_sp<-c()
MA_su<-c()
MA_fa<-c()
MA_wi<-c()
m_years<-unique(format(sitedata$Date,"%Y"))
for (y in m_years){
  i_year<-which(format(sitedata$Date,"%Y")==y)
  sitedata_year<-sitedata[i_year,]
  #spring
  MA_sp_temp<-MagnitudeSeason(sitedata_year,SeasonSp,y,TlengthPortion)
  MA_sp<-rbind(MA_sp,MA_sp_temp)
  #summer
  MA_su_temp<-MagnitudeSeason(sitedata_year,SeasonSu,y,TlengthPortion)
  MA_su<-rbind(MA_su,MA_su_temp)
  #fall
  MA_fa_temp<-MagnitudeSeason(sitedata_year,SeasonFa,y,TlengthPortion)
  MA_fa<-rbind(MA_fa,MA_fa_temp)
  #winter
  MA_wi_temp<-MagnitudeSeason(sitedata_year,SeasonWi,y,TlengthPortion)
  MA_wi<-rbind(MA_wi,MA_wi_temp)
}

MAsp<-colMeans(MA_sp,na.rm=TRUE)
MAsu<-colMeans(MA_su,na.rm=TRUE)
MAfa<-colMeans(MA_fa,na.rm=TRUE)
MAwi<-colMeans(MA_wi,na.rm=TRUE)

SiteSeasonMetrics<-c(MAsp,
                     MAsu,
                     MAfa, 
                     MAwi)

#Moving average-----------------------------------------------------------------
# ID the longest data within the range with missing days no more than 5 days
# find the consecutive data
notmissing<-sitedata[!is.na(sitedata$MeanT),]
constart<-notmissing[c(1,diff(notmissing$Date))>5,]$Date		
constart<-c(min(notmissing$Date),constart)
conend<-notmissing[diff(notmissing$Date)>5,]$Date
conend<-c(conend,max(notmissing$Date))
conend<-conend[1:length(constart)]
condays<-conend-constart
i_longest<-which(condays==max(condays))

## magnitude (a.,b.) moving average
# 1-- maximum of 30 days moving average of daily a. mean b. maximum
# 2-- maximum of 21 days moving average of daily a. mean b. maximum
# 3-- maximum of 14 days moving average of daily a. mean b. maximum
# 4-- maximum of 7 days moving average of daily a. mean b. maximum
# 5-- maximum of 3 days moving average of daily a. mean b. maximum

if(i_longest>=1){
  for(jj in i_longest){
    i_start<-which(sitedata$Date==constart[jj])
    i_end<-which(sitedata$Date==conend[jj])
    sitedata_consec5<-sitedata[i_start:i_end,]
    
    windowdays<-c(30, 21, 14, 7, 3)
    
    MaxMovingAMeanT<-vector("list", length(windowdays))
    MaxMovingAMaxT<-vector("list", length(windowdays))
    
    SiteMovingMetrics<-data.frame(1)
    # maximum moving average of (a,b) in different moving windows
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
            
            MaxMovingAMeanT[[dd]]<-c(MaxMovingAMeanT[[dd]],max(MovingMeanT))
          }
          # b. daily maximum
          if(length(na.omit(sitedata_consec5$MaxT[i_year]))>=windowdays[dd]){
            zoo_MaxT<-zoo(sitedata_consec5$MaxT[i_year],as.Date(sitedata_consec5$Date[i_year]))
            MovingMaxT<-rollmean(na.omit(zoo_MaxT),windowdays[dd],align="center")
            
            i_movingmaxT<-index(MovingMaxT)
            i_MMmaxT<-i_movingmaxT[which(MovingMaxT==max(MovingMaxT))]
            
            MaxMovingAMaxT[[dd]]<-c(MaxMovingAMaxT[[dd]],max(MovingMaxT))
          }
                           
        #}
      }
      MaxMovingAMeanT[[dd]]<-mean(MaxMovingAMeanT[[dd]])
      MaxMovingAMaxT[[dd]]<-mean(MaxMovingAMaxT[[dd]])
    
      SiteMovingMetrics<-data.frame(SiteMovingMetrics,
                                    MaxMovingAMeanT[[dd]],
                                    MaxMovingAMaxT[[dd]])	
    }
    SiteMovingMetrics<-data.frame(SiteMovingMetrics[,2:length(SiteMovingMetrics)])
    
  }
  
}else{
  
  SiteMovingMetrics<-as.data.frame(matrix(rep(NA,10),ncol=10))
  
  
}

# collect all the metrics-------------------------------------------------------
	SiteMetrics<-matrix(c(SiteInfo[1],SiteMonthlyMetrics,SiteSeasonMetrics,SiteMovingMetrics),nrow=1,ncol=59)
	colnames(SiteMetrics)<-c("siteinfo",
					paste("ADmax",1:12,sep=""),paste("ADmin",1:12,sep=""),paste("ADmean",1:12,sep=""),              
					"MaxDmeanSp","MinDmeanSp","AvgDmeanSp",
					"MaxDmeanSu","MinDmeanSu","AvgDmeanSu",
					"MaxDmeanFa","MinDmeanFa","AvgDmeanFa",
					"MaxDmeanWi","MinDmeanWi","AvgDmeanWi",
					"Max30MovingAMeanT","Max30MovingAMaxT",
					"Max21MovingAMeanT","Max21MovingAMaxT",
					"Max14MovingAMeanT","Max14MovingAMaxT",
					"Max7MovingAMeanT","Max7MovingAMaxT",
					"Max3MovingAMeanT","Max3MovingAMaxT")
		
	return(SiteMetrics)		
}
#==============================================================================================================================================================

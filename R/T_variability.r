#' A Function characterizes variability of stream temperature
#'
#' This function cacluate metrics that characterize variability of stream temperature
#' @param sitedata stream monitoring site data in SiteID, Date(in as.Date format), 
#' MaxT, MinT, MeanT.
#' @param TlengthPortion portion of length that required for calculating metric summaries,
#' defaulty=2/3
#' @param SeasonSp define spring season, default as March, April, and May, c(3,4,5)
#' @param SeasonSu define summer season, default as June, July, August, c(6,7,8)
#' @param SeasonFa define fall season, default as September, October, November, c(9,10,11)
#' @param SeasonWi define winter season, default as December, Januray, February, c(12,1,2)
#' @keywords variability
#' @export
#' @examples
#' install.packages("dataRetrieval")
#' library(dataRetrieval)
#' ExUSGSStreamTemp<-readNWISdv("01382310","00010","2011-01-01","2011-12-31",c("00001","00002","00003"))
#' sitedata<-subset(ExUSGSStreamTemp, select=c("site_no","Date","X_00010_00001","X_00010_00002","X_00010_00003"))
#' names(sitedata)<-c("siteID","Date","MaxT","MinT","MeanT")
#' T_variability(sitedata)

T_variability<- function(sitedata, TlengthPortion=2/3,
                         SeasonSp=c(3,4,5), SeasonSu=c(6,7,8),
                         SeasonFa=c(9,10,11),SeasonWi=c(12,1,2)){
  library(zoo)

  SiteInfo<-sitedata[1,1]
  
#Monthly------------------------------------------------------------------------
VariabilityMonth<-function(sitedata,y,TlengthPortion){
  ADrange<-c()
  Rmean<-c()
  CVDmax<-c()
  CVDmin<-c()
  CVDmean<-c()
  # find months
  mo<-as.numeric(format(sitedata$Date,"%m"))
  for(jj in 1:12){
    i_mo<-which(mo==jj)
    if (length(i_mo)>=30*TlengthPortion){
      
      ADrange_mo<-mean(na.omit(sitedata$MaxT[i_mo]-sitedata$MinT[i_mo]))
      Rmean_mo<-max(na.omit(sitedata$MeanT[i_mo]))-min(na.omit(sitedata$MeanT[i_mo]))
      CVDmax_mo<-CV(na.omit(sitedata$MaxT[i_mo])+273)
      CVDmin_mo<-CV(na.omit(sitedata$MinT[i_mo])+273)
      CVDmean_mo<-CV(na.omit(sitedata$MeanT[i_mo])+273)
      
    }else{
      ADrange_mo<-NA
      Rmean_mo<-NA
      CVDmax_mo<-NA
      CVDmin_mo<-NA
      CVDmean_mo<-NA
    }
    
    #found no nonmissing values to max or min, returning -Inf
    Rmean_mo<-ifelse(Rmean_mo!=-Inf,Rmean_mo,NA)
    
    ADrange<-c(ADrange,ADrange_mo)
    Rmean<-c(Rmean,Rmean_mo)
    CVDmax<-c(CVDmax,CVDmax_mo)
    CVDmin<-c(CVDmin,CVDmin_mo)
    CVDmean<-c(CVDmean,CVDmean_mo)
  }
  
  SiteMonthlyMetrics<-c(ADrange,Rmean,CVDmax,CVDmin,CVDmean)
  SiteMonthlyMetrics<-matrix(SiteMonthlyMetrics,nrow=1)
  return(SiteMonthlyMetrics)
}

#average multiple years
VA12month<-c()
m_years<-unique(format(sitedata$Date,"%Y"))
for (y in m_years){
  i_year<-which(format(sitedata$Date,"%Y")==y)
  sitedata_year<-sitedata[i_year,]
  
  VA12month_temp<-VariabilityMonth(sitedata_year,y,TlengthPortion)
  VA12month<-rbind(VA12month,VA12month_temp)
  
}

VA12month<-colMeans(VA12month,na.rm=TRUE)

SiteMonthlyMetrics<-c(VA12month)

# ## monthly----------------------------------------------------------------------
# 	ADrange<-c()
# 	Rmean<-c()
#   CVDmax<-c()
#   CVDmin<-c()
#   CVDmean<-c()
# 	# find months
# 	mo<-as.numeric(format(sitedata$Date,"%m"))
# 	for(jj in 1:12){
# 		i_mo<-which(mo==jj)
#     if (length(i_mo)>=30*TlengthPortion){
#       ADrange_mo<-mean(na.omit(sitedata$MaxT[i_mo]-sitedata$MinT[i_mo]))
#       Rmean_mo<-max(na.omit(sitedata$MeanT[i_mo]))-min(na.omit(sitedata$MeanT[i_mo]))
#       CVDmax_mo<-CV(na.omit(sitedata$MaxT[i_mo])+273)
#       CVDmin_mo<-CV(na.omit(sitedata$MinT[i_mo])+273)
#       CVDmean_mo<-CV(na.omit(sitedata$MeanT[i_mo])+273)
#     }else{
#       ADrange_mo<-NA
#       Rmean_mo<-NA
#       CVDmax_mo<-NA
#       CVDmin_mo<-NA
#       CVDmean_mo<-NA
#     }
# 		#found no nonmissing values to max or min, returning -Inf
# 		Rmean_mo<-ifelse(Rmean_mo!=-Inf,Rmean_mo,NA)
# 		
# 		ADrange<-c(ADrange,ADrange_mo)
# 		Rmean<-c(Rmean,Rmean_mo)
# 		CVDmax<-c(CVDmax,CVDmax_mo)
# 		CVDmin<-c(CVDmin,CVDmin_mo)
# 		CVDmean<-c(CVDmean,CVDmean_mo)
# 	}
# 	SiteMonthlyMetrics<-c(ADrange,Rmean,CVDmax,CVDmin,CVDmean)
# 	SiteMonthlyMetrics<-matrix(SiteMonthlyMetrics,nrow=1)

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

VariabilitySeason<-function(sitedata,season,y,TlengthPortion){
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
    
    SeasonRmax<-max(na.omit(sitedata$MaxT[i_season]))-min(na.omit(sitedata$MaxT[i_season]))
    SeasonRmin<-max(na.omit(sitedata$MinT[i_season]))-min(na.omit(sitedata$MinT[i_season]))
    SeasonRmean<-max(na.omit(sitedata$MeanT[i_season]))-min(na.omit(sitedata$MeanT[i_season]))
    
    
  }else{
    SeasonRmax<-NA
    SeasonRmin<-NA
    SeasonRmean<-NA
    
  }
  
  return(c(SeasonRmax,SeasonRmin,SeasonRmean))
}

VA_sp<-c()
VA_su<-c()
VA_fa<-c()
VA_wi<-c()
m_years<-unique(format(sitedata$Date,"%Y"))
for (y in m_years){
  i_year<-which(format(sitedata$Date,"%Y")==y)
  sitedata_year<-sitedata[i_year,]
  #spring
  VA_sp_temp<-VariabilitySeason(sitedata_year,SeasonSp,y,TlengthPortion)
  VA_sp<-rbind(VA_sp,VA_sp_temp)
  #summer
  VA_su_temp<-VariabilitySeason(sitedata_year,SeasonSu,y,TlengthPortion)
  VA_su<-rbind(VA_su,VA_su_temp)
  #fall
  VA_fa_temp<-VariabilitySeason(sitedata_year,SeasonFa,y,TlengthPortion)
  VA_fa<-rbind(VA_fa,VA_fa_temp)
  #winter
  VA_wi_temp<-VariabilitySeason(sitedata_year,SeasonWi,y,TlengthPortion)
  VA_wi<-rbind(VA_wi,VA_wi_temp)
}

VAsp<-colMeans(VA_sp,na.rm=TRUE)
VAsu<-colMeans(VA_su,na.rm=TRUE)
VAfa<-colMeans(VA_fa,na.rm=TRUE)
VAwi<-colMeans(VA_wi,na.rm=TRUE)

SiteSeasonMetrics<-c(VAsp,
                     VAsu,
                     VAfa, 
                     VAwi)


#moving-------------------------------------------------------------------------
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

## variability, (daily range) (c.)
# 1-- maximum of 30 days moving average c.daily range
# 2-- maximum of 21 days moving average c.daily range
# 3-- maximum of 14 days moving average c.daily range
# 4-- maximum of 7 days moving average c.daily range
# 5-- maximum of 3 days moving average c.daily range

## characterize the thermal extreme fluctuation metrics
# 7-- extreme metrics 6-days consequtive avg. high - avg. low within the maximum 30 days mean window
# 8-- extreme metrics 5-days consequtive avg. high - avg. low within the maximum 21 days mean window
# 9-- extreme metrics 4-days consequtive avg. high - avg. low within the maximum 14 days mean window
# 10-- extreme metrics 2-days consequtive avg. high - avg. low within the maximum 7 days mean window
# 11-- extreme metrics 1-days consequtive avg. high - avg. low within the maximum 3 days mean window
# 12-- extreme metrics 1-days consequtive avg. high - avg. low within the maximum 1 days mean window <- no need YPT 2012.7.14

if(i_longest>=1){
  for(jj in i_longest){
    i_start<-which(sitedata$Date==constart[jj])
    i_end<-which(sitedata$Date==conend[jj])
    sitedata_consec5<-sitedata[i_start:i_end,]
    
    windowdays<-c(30, 21, 14, 7, 3)
    extreme<-c(6,5,4,2,1)
    
    MaxMovingAMeanT<-vector("list", length(windowdays))
    MaxMovingADRT<-vector("list", length(windowdays))
    DiffExtreme<-vector("list", length(windowdays))
    
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
            
            MaxMovingAMeanT[[dd]]<-c(MaxMovingAMeanT[[dd]],max(MovingMeanT))
            #JDMMAMeanT[[dd]]<-c(JDMMAMeanT[[dd]],julian(i_MMmeanT,origin = as.Date(paste(y,"-01-01",sep=""))))
          }
                 
          # c. daily range
          DRT<-sitedata_consec5$MaxT[i_year]-sitedata_consec5$MinT[i_year]
          if(length(na.omit(DRT))>=windowdays[dd]){
            zoo_DRT<-zoo(DRT,as.Date(sitedata_consec5$Date[i_year]))
            MovingDRT<-rollmean(na.omit(zoo_DRT),windowdays[dd],align="center")
            
            i_movingDRT<-index(MovingDRT)
            i_MMDRT<-i_movingDRT[which(MovingDRT==max(MovingDRT))]
            
            MaxMovingADRT[[dd]]<-c(MaxMovingADRT[[dd]],max(MovingDRT))
          }
          
          # extreme metrics
          if(length(na.omit(sitedata_consec5$MeanT[i_year]))>=windowdays[dd]){
            if(windowdays[dd]%%2==1){
              zoo_MeanT_extreme<-zoo_MeanT[as.Date(i_MMmeanT-(windowdays[dd]-1)/2:i_MMmeanT+(windowdays[dd]-1)/2,origin="1970-01-01")]
            }else{
              zoo_MeanT_extreme<-zoo_MeanT[as.Date((i_MMmeanT-(windowdays[dd]/2-1)):(i_MMmeanT+windowdays[dd]/2),origin="1970-01-01")]
            }
            if(length(na.omit(zoo_MeanT_extreme))>=extreme[dd]){
              MovingExtremeMeanT<-rollmean(na.omit(zoo_MeanT_extreme),extreme[dd],align="center")
              ConsecAvgHigh<-max(MovingExtremeMeanT)
              ConsecAvgLow<-min(MovingExtremeMeanT)
              DiffExtreme[[dd]]<-c(DiffExtreme[[dd]],(ConsecAvgHigh-ConsecAvgLow))
            }             
          }
          
        #}
      }

      MaxMovingADRT[[dd]]<-mean(MaxMovingADRT[[dd]])
      DiffExtreme[[dd]]<-mean(DiffExtreme[[dd]],na.rm=TRUE)
    
      SiteMovingMetrics<-data.frame(SiteMovingMetrics,
                                    MaxMovingADRT[[dd]],
                                    DiffExtreme[[dd]])	
    }
    
    SiteMovingMetrics<-data.frame(SiteMovingMetrics[,2:length(SiteMovingMetrics)])
        
  }
  
}else{
  
  SiteMovingMetrics<-as.data.frame(matrix(rep(NA,10),ncol=10))
  
}
SiteMovingMetrics<-as.numeric(SiteMovingMetrics)
# collect all the metrics-----------------------------------------------------
	SiteMetrics<-matrix(c(SiteMonthlyMetrics,SiteSeasonMetrics,SiteMovingMetrics),nrow=1,ncol=82)
	colnames(SiteMetrics)<-c(
					paste("ADrange",1:12,sep=""),paste("Rmean",1:12,sep=""),
          paste("CVDmax",1:12,sep=""),paste("CVDmin",1:12,sep=""),paste("CVDmean",1:12,sep=""),              
					"RmaxSp","RminSp","RmeanSp",
					"RmaxSu","RminSu","RmeanSu",
					"RmaxFa","RminFa","RmeanFa",
					"RmaxWi","RminWi","RmeanWi",
					"Max30MovingADRT","DiffExtreme6-30",
					"Max21MovingADRT","DiffExtreme5-21",
					"Max14MovingADRT", "DiffExtreme4-14",
					"Max7MovingADRT", "DiffExtreme2-7",
					"Max3MovingADRT", "DiffExtreme1-3")
	SiteMetrics<-data.frame(SiteInfo,SiteMetrics,stringsAsFactors=FALSE)
	return(SiteMetrics)		
}

#==============================================================================================================================================================
#coefficient of variation 
CV <- function(x){sd(x)/mean(x)} 

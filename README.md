#Note 2018/11/14--
update the installation line in the Readme.md.
~~ tsang

#Note 2017/10/23--
This release updated the output as data.frame.
~~ tsangyp

#Note 2016/10/27--
I have updated the StreamThermal (V1.1) with little package dependency. Please try to install again.
~~ tsangyp

Potential errors:

1. The "internal error -3" often happens when you use install_github to install a package that's currently loaded; try restarting R and running the app again.

# StreamThermal

For the full documentation of StreamThermal and an application on association between stream temperature and fish, please read

[StreamThermal: A Software Package for Calculating Thermal Metrics from Stream Temperature Data](http://www.tandfonline.com/doi/full/10.1080/03632415.2016.1210517)

StreamThermal synthesizes daily average, maximum, and/or minimum stream temperature records into 267 possible stream thermal metrics for a time period of interest, including months or seasons, which may be user-defined).  Metrics are listed in Table 1 and are grouped into five different categories describing different aspects of stream thermal regimes. To identify these categories as well as specific metrics in each category, we incorporated information from recent publications on stream thermal regimes (e.g., Chu, 2010, Olden and Naiman 2010, Arismendi 2013) and based on expert opinion.  Magnitude metrics characterize monthly and seasonal average, maximum, and minimum temperatures from daily temperatures, as well as 3, 7, 14, 21, and 30-day moving averages for mean and maximum daily temperatures.  Variability metrics summarize monthly and seasonal range in daily mean temperatures, as well as monthly coefficient of variation of daily mean, maximum, and minimum, temperatures.  Variability metrics also summarize moving averages for daily ranges and moving variability in extreme temperatures, which is calculated from differences in average high and low temperatures over various time periods.  Frequency metrics indicate number of days in months or seasons that key events exceed user-defined temperatures of interest.  Timing metrics indicate Julian days of key events including mean, maximum, and minimum temperatures; they also indicate Julian days of mean, maximum, and minimum values over moving windows.  The final category, rate of change includes just two metrics.  For months or seasons, rate of change indicates the difference in magnitude of maximum and minimum temperatures divided by the number of days between these events (following Chu et al. 2010). In all categories, the months that are included in the calculation of seasonal metrics can be defined by users.

To use StreamThermal to calculate stream thermal metrics, the date, maximum daily temperature, minimum daily temperature, and mean daily temperature must be assembled for at least one month of data from a single year.  Records must be assembled in the order listed above, formatted as four columns of data frame in R  If multiple years of data are available, metrics will be calculated based on averages across years.  Missing records are allowed within the dataset, and StreamThermal will calculate metrics for a time period of interest provided data are available for two-thirds of that time and provided that there is not a gap of records of more than 5 days in length. Metrics are calculated in groupings (Table 1), and R provides output in a data frame format.
The package is attached with this paper and can be downloaded from github, repository “StreamThermal” in account: tsangyp. 

###To install this package in R

install.packages("devtools")

library(devtools)

install_github('tsangyp/StreamThermal')

library(StreamThermal)

###List of functions
T_magnitude
  
T_variability
  
T_frequency
  
T_timing
  
T_rateofchange


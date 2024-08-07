#SANDI - File Processing Script
#Author: Angus Kennedy
#Last updated: April 4, 2024

#===Purpose===
#To take individual csv files from SANDI, plot and modify the plot of the concentrations of CO2 and CH4, calculate CO2 and CH4 flux rates, calculate average temperature, output the modified plots, and output the flux rates into separate csv files with the average temperature.

#The selection of individual files is required for input

#===LIBRARIES & PACKAGES===
#install.packages("tidyverse")
#install.packages("dplyr")
#install.packages("zoo")
#install.packages("readr")
#install.packages("lubridate")
#install.packages("hms")
#install.packages("gridExtra")
#install.packages("lutz")
#install.packages("sf")
#install.packages("stats")
#install.packages("broom")
library("tidyverse")
library("dplyr")
library("zoo")
library("readr")
library("lubridate")
library("hms")
library("gridExtra")
library("lutz")
library("sf")
library("stats")
library("broom")


#===CONSTANTS & VARIABLES===
#Files
benchmark_file <- read.csv(file.choose(), header = FALSE, sep = ",", quote = "\"", dec = ".", fill = TRUE, skip = 4)

sample_file <- read.csv(file.choose(), header = FALSE, sep = ",", quote = "\"", dec = ".", fill = TRUE, skip = 4)

#Check
#View(sample_file)

#output_loc <- "C:\Users\studentloaner\Documents\Important_stuff\StFX_Year_4\BIOL_491\Thesis\Garbarys_Lab\SANDI\Processed_data"

#survey variables
#date and time
OlsonNames(tzdir=NULL) #get list of R timezones
timezone = "America/Halifax" #timezone in which measurements were taken
surveydate = "2023-08-03"

#gas constants and variables
mwCO2 = 44.01 #molecular weight of CO2 (g/mol)
mwCH4 = 16.04 #molecular weight of CH4 (g/mol)

p_Pa = 101325 #assumed pressure at STP (1 atm)
R = 8.3145 #ideal gas constant (m^3*Pa/mol*K)

#chamber variables
#Small chambers
vol_Sma = 0.008439 #total volume of the flux chamber system (m^3) (chamber = 0.008340 m^3 + Vaisala = 0.000059 m^3 + Axetris = 0.000019 m^3 + tubing = 0.000021 m^3)
sa_Sma = 0.05726 # surface area of the flux chamber meeting the ground (m2)

#Large chambers
#Dark
vol_LaDa = 0.017599 #total volume of the flux chamber system (m^3) (chamber = 0.017500 m^3 + Vaisala = 0.000059 m^3 + Axetris = 0.000019 m^3 + tubing = 0.000021 m^3)
sa_soil_surface_LaDa = 0.05726 #The surface area of the ground that the chamber covers
sa_LaDa = 0.357 # surface area of the flux chamber (m2), loss of SA to handle region considered negligible for now give that it is just indentation, not a loss of SA all together 

#Light
vol_LaLi = 0.017349 #total volume of the flux chamber system (m^3) (chamber = 0.017250 m^3 + Vaisala = 0.000059 m^3 + Axetris = 0.000019 m^3 + tubing = 0.000021 m^3)
sa_soil_surface_LaLi = 0.05683 #The surface area of the ground that the chamber covers
sa_interior_LaLi = 0.352 # surface area of the flux chamber (m2), loss of SA to handle region considered negligible for now give that it is just indentation, not a loss of SA all together

#Select volume and surface area
vol = vol_Sma
#vol = vol_LaDa
#vol = vol_LaLi

sa = sa_Sma
#sa = sa_soil_surface_LaDa
#sa = sa_soil_surface_LaLi

#Constants for an alternate method of flux calculation
#assume STP
mvCO2 = 22.4 # molar volume of CO2 (l/mol)
mvCH4 = 22.4 #l/mol
vol_litres = vol*1000 # volume of the flux chamber (liters)
area = sa # surface area of the flux chamber (m2)

#Naming columns
#for sample file
time_stamp<-sample_file$V1
record_num<-sample_file$V2
batt_volt_min<-sample_file$V3
LoggerTemp_C<-sample_file$V4
methane_conc_ppm<-sample_file$V5
carbon_dioxide_conc_ppm<-sample_file$V7
therm_temp<-sample_file$V8
time_utc<-sample_file$V10
latitude_SANDI<-sample_file$V11
longitude_SANDI<-sample_file$V12

#for benchmarking file
TS_bench<-benchmark_file$V1
rec_num<-benchmark_file$V2
batt_volt_min_bench<-benchmark_file$V3
LogT_C_bench<-benchmark_file$V4
CH4_ppm_bench<-benchmark_file$V5
CO2_ppm_bench<-benchmark_file$V7
therm_temp_bench<-benchmark_file$V8
time_utc_bench<-benchmark_file$V10
lat_bench<-benchmark_file$V11
long_bench<-benchmark_file$V12

#===Creating data frames===
file2analyze<-data.frame(
  time_stamp,record_num,methane_conc_ppm,carbon_dioxide_conc_ppm,
  therm_temp,time_utc,latitude_SANDI,longitude_SANDI)
diagnostic_df<-data.frame(time_stamp,record_num,batt_volt_min,
                          LoggerTemp_C,therm_temp,time_utc)

Benchmarking<-data.frame(TS_bench,rec_num, CH4_ppm_bench,
                         CO2_ppm_bench, therm_temp_bench, time_utc_bench)

#===Step 0:Determine drift to see if it needs correction===
#Unclear if I would need to correct things because I am dealing with rate of change, not really a big deal of the concentrations themselves
Benchmarking$TS_bench <- ymd_hms(Benchmarking$TS_bench)

class(Benchmarking$TS_bench[1])
Benchmarking$TS_bench[1] <= Benchmarking$TS_bench[2]

Benchmarking<- subset(Benchmarking, 
                           Benchmarking$TS_bench>=ymd_hms("2023-08-23 16:21:10") 
                           & Benchmarking$TS_bench<=ymd_hms("2023-08-23 16:23:30"))


Benchmarking$CH4_ppm_bench <- as.numeric(as.character(Benchmarking$CH4_ppm_bench))
Benchmarking$CO2_ppm_bench <- as.numeric(as.character(Benchmarking$CO2_ppm_bench))

bench_CH4_mean<- as.numeric(mean(Benchmarking$CH4_ppm_bench))
#print(bench_CH4_mean)
#print(sd(Benchmarking$CH4_ppm_bench))
bench_CO2_mean<- as.numeric(mean(Benchmarking$CO2_ppm_bench))
#print(bench_CO2_mean)
#print(sd(Benchmarking$CO2_ppm_bench))

offset_CH4<-4.5254 - bench_CH4_mean #4.5254ppm is the concentration of CH4 in the benchmarking gas
offset_CO2<-516.54 - bench_CO2_mean #516.54ppm is the concentration of CO2 in the benchmarking gas
#print(offset_CH4)
#print(offset_CO2)

#===Step 1: Clean and correct file===
#Remove extraordinarily low or high measurements and fix time measurement 
#Fix time - turn TIMESTAMP into ymd_hms
file2analyze$time_stamp <- ymd_hms(file2analyze$time_stamp)

#print(file2analyze$time_stamp[1])
class(file2analyze$time_stamp[1])
file2analyze$time_stamp[1] <= file2analyze$time_stamp[2]

#Apply offsets
#print(file2analyze$methane_conc_ppm[1])
#print(file2analyze$carbon_dioxide_conc_ppm[3])
i<-0
while (i < length(file2analyze$carbon_dioxide_conc_ppm)) {
  file2analyze$methane_conc_ppm[i] <- file2analyze$methane_conc_ppm[i] + offset_CH4
  file2analyze$carbon_dioxide_conc_ppm[i] <- file2analyze$carbon_dioxide_conc_ppm[i] + offset_CO2
  i<- i+1
}

#print(file2analyze$methane_conc_ppm[1])
#print(file2analyze$carbon_dioxide_conc_ppm[3])


#===Step 2: Ensure values are usable===
#Turn things numeric
file2analyze$methane_conc_ppm <- as.numeric(as.character(file2analyze$methane_conc_ppm))
file2analyze$carbon_dioxide_conc_ppm <- as.numeric(as.character(file2analyze$carbon_dioxide_conc_ppm))
#class(file2analyze$methane_conc_ppm)
#class(file2analyze$carbon_dioxide_conc_ppm)


#===Step 3: Remove negatives===
#Remove extrodinarily low or high measurements
file2analyze_CH4 <- subset(file2analyze, file2analyze$methane_conc_ppm>=0
                           & file2analyze$methane_conc_ppm<=10) 
#I'm unsure if it is valid to not include negative concentrations but not wildly high concentrations


file2analyze_CO2 <- subset(file2analyze, file2analyze$carbon_dioxide_conc_ppm>=0
                           & file2analyze$carbon_dioxide_conc_ppm<=1000) 

#I'm unsure if it is valid to not include negative concentrations but not wildly high concentrations


#===Step 4: Plot to assess===
#Plot the CH4 and CO2 concentrations
Time_HHMMSS<-file2analyze_CH4$time_stamp
Concentration_CH4_ppm<-file2analyze_CH4$methane_conc_ppm
plot(Time_HHMMSS,Concentration_CH4_ppm)
#plot(x=file2analyze$time_stamp, y=file2analyze$methane_conc_ppm, 
      #$xlab("Time (hh:mm:ss)"), ylab("CH4 Concentration (ppm)"), xlim = NULL, 
      #ylim = NULL) # I am confused by the problems I keep experiencing here, but I have to move on

Time_HHMMSS<-file2analyze_CO2$time_stamp
Concentration_CO2_ppm<-file2analyze_CO2$carbon_dioxide_conc_ppm
plot(x=Time_HHMMSS, y=Concentration_CO2_ppm)
#Assess both plots to determine equation type (e.g. linear, logarithmic, cubic, etc)
#Assess both plots to remove artifacts

#===Step 5: Remove artifacts===
#Assess plot and decide the start and end times of the survey to remove artifacts
file2analyze_CH4 <- subset(file2analyze_CH4, 
                       file2analyze_CH4$time_stamp>=ymd_hms("2023-08-03 17:26:30") 
                       & file2analyze_CH4$time_stamp<=ymd_hms("2023-08-03 17:47:00"))

file2analyze_CO2 <- subset(file2analyze_CO2, 
                           file2analyze_CO2$time_stamp>=ymd_hms("2023-08-03 17:27:30") 
                           & file2analyze_CO2$time_stamp<=ymd_hms("2023-08-03 17:32:00"))


#You'll have to enter this manually

#===Step 6: Apply linear Regressions===
#Apply a linear regression to find slope
#time is the independent variable and gas concentration is the dependent variable
#The slope will be used to calculate the flux rates

#Methane first

return_slope<- function(times, concentrations) {
  
  conc_change <- vector(mode = "numeric")
  a<-1
  while (a < length(concentrations)) {
    new_value<- concentrations[a+1] - concentrations[a]
    new_value<- as.numeric(new_value)
    conc_change<-c(conc_change,new_value)
    a<- a+1
  }

  time_change<-vector()
  b<-1
  while (b < length(concentrations)) {
    new_value<- times[b+1] - times[b]
    new_value<- as.numeric(new_value)
    time_change<- c(time_change,new_value)
    b<- b+1
  }
  
  conc_over_time<-vector()
  d<-1
  while (d < length(concentrations)) {
    new_value <- conc_change[[d]]/time_change[[d]]
    new_value<- as.numeric(new_value)
    conc_over_time<- c(conc_over_time, new_value)
    d<- d+1
  }
  
  return(conc_over_time)
  #print(mean(conc_over_time))
  #print(sd(conc_over_time))
  
}

CH4_slope <- return_slope(file2analyze_CH4$time_stamp, file2analyze_CH4$methane_conc_ppm)

CO2_slope <- return_slope(file2analyze_CO2$time_stamp, file2analyze_CO2$carbon_dioxide_conc_ppm)

#===Step 7: Calc flux rates from slope===
#Multiply slope by the volume to surface area ratio to get flux rate

return_flux_rate<- function(slope, SA, volume){
  slope<- as.numeric(slope)
  SA<- as.numeric(SA)
  volume<- as.numeric(volume)
  ratio<- volume/SA
  flux_rate<- slope*ratio
  return(flux_rate)
}

CH4_slope_mean<- as.numeric(mean(CH4_slope))
CH4_slope_sd<- as.numeric(sd(CH4_slope))

CH4_flux_rate <- return_flux_rate(CH4_slope_mean,sa,vol)
CH4_flux_rate_sd <- return_flux_rate(CH4_slope_sd,sa,vol)
#print(CH4_flux_rate)
#print(CH4_flux_rate_sd)


CO2_slope_mean<- as.numeric(mean(CO2_slope))
CO2_slope_sd<- as.numeric(sd(CO2_slope))

CO2_flux_rate <- return_flux_rate(CO2_slope_mean,sa,vol)
CO2_flux_rate_sd <- return_flux_rate(CO2_slope_sd,sa,vol)
#print(CO2_flux_rate)
#print(CO2_flux_rate_sd)


#===Step 8:Get average soil temp===
#First plot
Time_HHMMSS<-file2analyze$time_stamp
SoilTemp_C<-file2analyze$therm_temp
plot(Time_HHMMSS,SoilTemp_C)

#Define time and temp parameters, this is manual
file2analyze_temp<-subset(file2analyze, 
                          file2analyze$time_stamp>=ymd_hms("2023-08-03 17:00:00") 
                          & file2analyze$time_stamp<=ymd_hms("2023-08-03 18:23:30"))

file2analyze_temp$therm_temp<-as.numeric(file2analyze_temp$therm_temp)

file2analyze_temp<-subset(file2analyze_temp, 
                         file2analyze_temp$therm_temp>=0
                         & file2analyze_temp$therm_temp<=100)

Time_HHMMSS<-file2analyze_temp$time_stamp
SoilTemp_C<-file2analyze_temp$therm_temp
plot(Time_HHMMSS,SoilTemp_C)

#Get average
temp_mean<-mean(SoilTemp_C)
temp_sd<-sd(SoilTemp_C)

#===Step 9: Spit it out===
#There is more to add, like I'm only getting ppm/s right now
#For now I'm just manually recording these, but this will not always be the case

column_and_row_names<-list(list("CH4","CO2","Temp"),list("mean","sd"))

results <- matrix(c(CH4_flux_rate,CH4_flux_rate_sd, CO2_flux_rate,CO2_flux_rate_sd, temp_mean, temp_sd),3,2,byrow=TRUE, dimnames=column_and_row_names)
print(results) #Column 1 is CH4, Column 2 is CH4, Row 1 is mean, Row 2 is standard deviation
  



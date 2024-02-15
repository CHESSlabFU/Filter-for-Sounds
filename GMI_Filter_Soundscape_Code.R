



## R-biophony V2.2
## Stuart H. Gage
## REAL-MSU
## gages@msu.edu
## 2016-03-24

## Acknowledgements: Anne C. Axel, Eric Kasten and Jerome Sueur
## Further edits made by J. Quinn and A. Schindler

## Note:
## Set the directory in R-Studio prior to running R-Biophony in R on the desktop.
## Set the number of characters for the Site (e.g. Site <- 4)
## Specify if there is a character after the Site code (e.g. Space <- 0).
## Specify the year for Day-of-Year computations. Now set at 2012 (e.g. JD1 <- as.character(20120101)).
## After setting directory in R-Studio, copy and paste this script to R-Studio
## SET DIRECTORY IN R STUDIO

## Add the following libraries: 

#install.packages("soundecology");install.packages("seewave"); install.packages("tuneR");install.packages("tidyverse")
library(soundecology);
library(tuneR)
library(seewave); 
library(tidyverse)

#set the directory where your files are found
setwd("")

## Change the NUMBER OF CHARACTERS for a Site Name and specify if there is a SPACE after the Site Name. e.g. TB0120150601-060000.wav (Site = 4 (TB01); Space = 0)
## Note that all Site Names in a biophony run should have the same length.
## I want to input these values using the keyboard as they are variable values. (SHG)

###############################################################################################
Site <- 8                       # CHANGE THIS TO MATCH FILENAME
Space <- 1                      # CHANGE THIS TO MATCH FILENAME
###############################################################################################
SS <- Site + Space  
SS
# Constant values for Start-End characters for Date, Time, etc. ## Comments are values for Site = 7 and Space = 1
SDate <- SS + 1    # 9  
EDate <- SS + 8    # 17
SYear <- SS + 1    # 9
EYear <- SS + 4    # 12 
SMonth <- SS + 5   # 14 
EMonth <- SS + 6   # 14
SDay <- SS + 7     # 15 
EDay <- SS + 8     # 16 
SHour <- SS + 10   # 18
EHour <- SS + 11   # 19 
SMinute <- SS + 12 # 20  
EMinute <- SS + 15 # 21
STime <- SS + 10   # 18
ETime <- SS + 15   # 21

## Start R-biophony computations
## get the names and the number of wav files
files <- dir(pattern='.wav')
n <- length(files) # where length is number of sound files in data set
n
## prepare variable headers in a data frame

## results1 contains soundscape energy and associated statistics
results1 <- data.frame(Date=numeric(n),Year=numeric(n),Month=numeric(n),Day=numeric(n),Hour=numeric(n),Minute=numeric(n),Time=numeric(n),DayofYear=numeric(n),
                       F12=numeric(n),F23=numeric(n),F34=numeric(n),F45=numeric(n),F56=numeric(n),F67=numeric(n),F78=numeric(n),F89=numeric(n),F910=numeric(n),F1011=numeric(n),
                       Sum=numeric(n),Mean=numeric(n),SD=numeric(n),CV100=numeric(n))


## results2 contains soundscape indices computed here as from seewave and soundecology R packages
results2 <- data.frame(Date=numeric(n),Year=numeric(n),Month=numeric(n),Day=numeric(n),Hour=numeric(n),Minute=numeric(n),Time=numeric(n),DayofYear=numeric(n),
                       BiophonyFreqCh1=numeric(n),BiophonyAmpCh1=numeric(n),
                       TechnophonyCh1=numeric(n),
                       NDSIshgFreqCh1=numeric(n),NDSIshgAmpCh1=numeric(n),NDSIseeCh1=numeric(n),NDSIsouCh1=numeric(n),NDSIsouCh2=numeric(n),
                       BiophonyCh1=numeric(n),
                       AnthrophonyCh1=numeric(n),BiophonyCh2=numeric(n),AnthrophonyCh2=numeric(n),
                       HseeCh1=numeric(n),HsouCh1=numeric(n),
                       ACIseeCh1=numeric(n),ACIsouCh1=numeric(n),ACIsouCh2=numeric(n),ACIsouByMinCh1=numeric(n),ACIsouByMinCh2=numeric(n),
                       ADIsouCh1=numeric(n),ADIsouCh2=numeric(n),
                       AEIsouCh1=numeric(n),AEIsouCh2=numeric(n),
                       BAIsouCh1=numeric(n),BAIsouCh2=numeric(n),
                       Filename=character(n),SiteID=character(n))


## Main loop through recordings
for(i in 1:length(files))
{
  ## read wav file i
  s <- readWave(files[i])
  #s <- readWave("CAW_20120201_160000.wav") ##for testing code with single file
  #s <- downsample(s, 16000) #Change to the sampling rate of the recordings, or can take out if all recordings sample at the same rate
  s <- ffilter(s, from = 2000, output = "Wave")
  
  ## Compute Day of Year from beginning of sample year 2015
  DD <- as.character(substr(files[i],SDate,EDate))
  YR <- as.character(substr(files[i],SYear,EYear))
  DD1 <- as.Date(DD,"%Y%m%d") 		  		## Convert to Date format
  
  #############################################################################################################################	
  JD1 <- as.character(20200101) 				## CHANGE THIS! to match beginning of year of recording date
  #############################################################################################################################
  
  JD1 <- as.Date(JD1,format="%Y%m%d") 		    ## Convert to Date format
  JD1 <- julian(as.Date(JD1,format="%Y%m%d")) ## Calculate Julian date for first of year
  JD2 <- julian(as.Date(DD1,format="%Y%m%d")) ## Calculate Julian date for current date 
  JD <- (JD2-JD1) + 1							            ## Compute Day-of-Year	(Add 1 to match with db DAYOFYEAR)
  
  ## Calculate Soundscape Power and associated statistics
  x <-soundscapespec(s, plot=FALSE) 	## function call to compute soundscape power in R-seewave(no plots)
  #x<-x[1:7,] ##### this constrains x to individual bands with bits are under 8 
  FSUM <- colSums(x) 				        	## soundscape power sum
  FMEAN <- colMeans(x) 			        	## soundscape power mean
  v <- as.vector(x[,2]) 			        ## soundscape power F 1-2 kHz 
  FSD <- sd(v)						            ## soundscape power Standard Deviation
  FCV <- (FSD/FMEAN)*100		         	## soundscape power Coefficient of variation
  
  ## Soundscape power to results1
  results1[i,1] <- substr(files[i],SDate,EDate) 		  ## numeric Date of sample
  results1[i,2] <- substr(files[i],SYear,EYear)   	  ## Year
  results1[i,3] <- substr(files[i],SMonth,EMonth) 	  ## Month
  results1[i,4] <- substr(files[i],SDay,EDay) 		    ## Day
  results1[i,5] <- substr(files[i],SHour,EHour) 		  ## Hour
  results1[i,6] <- substr(files[i],SMinute,EMinute) 	## Minute
  results1[i,7] <- substr(files[i],STime,ETime) 	  	## Time
  results1[i,8] <- JD								## Cumulative Day from first of year
  results1[i,9] <-  x[1,2] 				  ## 1-2 kHz
  results1[i,10] <- x[2,2] 				  ## 2-3 kHz
  results1[i,11] <- x[3,2] 				  ## 3-4 kHz
  results1[i,12] <- x[4,2] 				  ## 4-5 kHz
  results1[i,13] <- x[5,2] 				  ## 5-6 kHz
  results1[i,14] <- x[6,2] 				  ## 6-7 kHz
  results1[i,15] <- x[7,2] 				  ## 7-8 kHz
  results1[i,16] <- x[8,2] 	## 8-9 kHz, replace x[] with a number in lines 121-123 if recording only to 8000 Hz (meaning sampling rate of 1600 Hz)
  results1[i,17] <- x[9,2] 	## 9-10 kHz
  results1[i,18] <- x[10,2] ## 10-11 kHz
  results1[i,19] <- FSUM[2] 				## sum of frequency intervals
  results1[i,20] <- FMEAN[2] 				## mean of frequency intervals
  results1[i,21] <- FSD   				  ## standard deviation of frequency intervals
  results1[i,22] <- FCV[2]				  ## coefficient of variation of frequency intervals
  results1[i,23] <- as.character(paste(strsplit(files[i], ".wav"))) 	## File name
  results1[i,24] <- as.character(substr(files[i],1,Site)) 			      ## Location
  
  ## Compute Soundscape Indices
  Biophony <- colSums(x)-v[1]				                       ## Biophony
  Technophony <- v[1]						                           ## Technophony
  NDSIshg <- (Biophony-Technophony)/(Technophony+Biophony) ## Normalized Difference Soundscape Power (SHG Version)
  NDSIsee <- NDSI(x)				 ## NDSI from R-seewave, use NDSI(x,biophony = 2:7) to constrain biophony if sampling rate 16000 Hz
  NDSIsou <- ndsi(s) 	  ## NDSI from R-soundecology, use ndsi(s, bio_max = 8000) to constrain bio_max if sampling rate 16000 Hz
  ACIsee <- ACI(s)	                    ## Acoustic Complexity Index from R-seewave
  ACIsou <- acoustic_complexity(s)		  ## Acoustics Complexity Index from R-soundecology
  ADIsou <- acoustic_diversity (s)		  ## Acoustic Diversity Index from R-soundecology
  AEIsou <- acoustic_evenness(s)				## Acoustic Evenness Index from R-soundecology
  BAIsou <- bioacoustic_index(s)				## Bioacoustic Index from R-soundecology
  Hsee <- sh(meanspec(s, plot=FALSE))		## H Index from R-seewave
  Hsou <- H (s)							    ## H Index from R-seewave via R-soundecology
  ## Soundscape Indices to results2
  results2[i,1] <- substr(files[i],SDate,EDate) 		## numeric Date of sample
  results2[i,2] <- substr(files[i],SYear,EYear)   	## YearNDSI
  results2[i,3] <- substr(files[i],SMonth,EMonth) 	## Month
  results2[i,4] <- substr(files[i],SDay,EDay) 		  ## Day
  results2[i,5] <- substr(files[i],SHour,EHour) 		## Hour
  results2[i,6] <- substr(files[i],SMinute,EMinute) ## Minute
  results2[i,7] <- substr(files[i],STime,ETime) 		## Time
  results2[i,8] <- JD						  ## Cumulative Day from first of year
  results2[i,9] <- Biophony[1]		## Biophony frequency from left channel, BiophonyFreqCh1
  results2[i,10] <- Biophony[2]   ## Biophony amplitude from left channel, BiophonyAmpCh1
  results2[i,11] <- Technophony		## Technophony from left channel, TechnophonyCh1
  results2[i,12] <- NDSIshg[1]    ## Calculated above (Gage version), NDSIshg frequency from left channel, NDSIshgFreqCh1
  results2[i,13] <- NDSIshg[2]		## Calculated above (Gage version), NDSIshg amplitutde from left channel, NDSIshgAmpCh1
  results2[i,14] <- NDSIsee[1]		## NDSI (from R-seewave-New Version as of 27-Jan-2014) from left channel, NDSIseeCh1
  results2[i,15] <- NDSIsou[1]    ## NDSI (from R-soundecology) from left channel, NDSIsouCH1
  results2[i,16] <- NDSIsou[2]    ## NDSI (from R-soundecology) from right channel, NDSIsouCh2
  results2[i,17] <- NDSIsou[3]    ## NDSI (from R-soundecology) biophony from left channel, BiophonyCh1
  results2[i,18] <- NDSIsou[4]    ## NDSI (from R-soundecology) anthrophony from left channel, AnthrophonyCh1
  results2[i,19] <- NDSIsou[5]    ## NDSI (from R-soundecology) biophony from right channel, BiophonyCh2
  results2[i,20] <- NDSIsou[6]    ## NDSI (from R-soundecology) anthrophony from right channel, AnthrophonyCh2
  results2[i,21] <- Hsee[1]				## Jerome's entropy R-seewave from left channel, HseeCh1
  results2[i,22] <- Hsou[1]				## Jerome's entropy R-seewave via soundecology from left channel, HsouCh1
  results2[i,23] <- ACIsee[1] 		## Nadia's Acoustic Complexity Index ACISee from left channel, ACIseeCh1
  results2[i,24] <- ACIsou[1]			## Nadia's ACI R-soundecology total all from left channel, ACIsouCh1
  results2[i,25] <- ACIsou[2]     ## Nadia's ACI R-soundecology total all from right channel, ACIsouCh2
  results2[i,26] <- ACIsou[3]     ## Nadia's ACI R-soundecology total all by minute from left channel, ACIsouByMinCh1
  results2[i,27] <- ACIsou[4]     ## Nadia's ACI R-soundecology total all by minute from right channel, ACIsouByMinCh2
  results2[i,28] <- ADIsou[1]			## Acoustic diversity R-soundecology from left channel, ADIsouCh1
  results2[i,29] <- ADIsou[2]     ## Acoustic diversity R-soundecology from right channel, ADIsouCh2
  results2[i,30] <- AEIsou[1]			## Acoustic evenness R-soundecology from left channel, AEIsouCh1
  results2[i,31] <- AEIsou[2]     ## Acoustic evenness R-soundecology from right channel, AEIsouCh2
  results2[i,32] <- BAIsou[1]			## Bioacoustic index R-soundecology from left channel, BAIsouCh1
  results2[i,33] <- BAIsou[2]     ## Bioacoustic index R-soundecology from right channel, BAIsouCh2
  results2[i,34] <- as.character(paste(strsplit(files[i], ".wav"))) 	## Filename
  results2[i,35] <- as.character(substr(files[i],1,Site)) 			      ## Location
  
  ##channel 1=left, channel 2=right
  ## Output results into two tables (results1 and results2)
  ##	print(results1[i,1:24]) ## Soundscape power and statistics-debug
  ##	print(results2[i,1:22])	## Soundscape Indices-debug
  
  ## Output number of recordings remaining to complete computations
  print(i)
  print(n)
  
} ## End big loop
## export to results1 (power) and results2 (indices) csv file after computations complete
#setwd("H:/80filter")
setwd("H:/2k_filter")

write.csv(results1, file="SMA03738_2k_Power.csv")
write.csv(results2, file="SMA03738_2k_Indices.csv")  

## END OF BIOPHONY


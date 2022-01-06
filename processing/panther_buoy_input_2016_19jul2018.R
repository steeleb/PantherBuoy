#*********************************************************************
#*      Cary Institute of Ecosystem Studies (Millbrook, NY)          *
#*        for use by the Panther Pond Citizen Scientists             * 
#*                                                                   *
#* TITLE:   panther_buoy_input_2016_19jul2018.r                      *
#* AUTHOR:  B. Steele                                                *
#* SYSTEM:  Lenovo ThinkCentre, Windows 10, R 3.4.3                  *
#* DATE:    19Jul2018                                                *
#* PROJECT: Panther Pond datalogger data                             *
#* PURPOSE: collate raw data files downloaded from all data          *
#*          loggers. display data from downloads                     *
#* LAST DATA ADD: Oct 2017  <- this should be the last download date *
#* LAST UPDATED: 19Jul2018  <- this should match the date in the     * 
#*                             title of this file                    *
#* BY:      B. Steele       <- update this with your name            *
#* UPDATES: NA              <- update this field with any comments   *
#*                             pertinent to the update               *
#*********************************************************************
#*                     folder tree structure                         *
#*                                                                   *
#*                          Google Drive                             *
#*                               |                                   *
#*                          Panther Pond                             *
#*                        _______|_______                            *
#*            ___________/       |       \________                   *
#*           /                   |                \                  *
#*     r program              raw_data        summary_data           *
#*                               |                                   *
#*                   folders with download dates                     *
#*                               |                                   *
#*                           data files                              *
#*                                                                   *
#*********************************************************************

# set working directory - change if running from a different location
setwd("G:/My Drive/Panther Pond/raw data/2016/") #to 'run' a line in the console (below), just type 'Ctrl + Enter'

#the first time you use R, you will have to download the 'packages' you use by running the following prompts:
#install.packages('tidyverse', dep=T)
#install.packages('readxl', dep=T)
#install.packages('ggthemes', dep=T)

#then you have to load the packages into your session by running the following prompts:
library(tidyverse) #you will see some red text about objects being 'masked' that's okay. 
library(readxl)
library(ggthemes)

#save final theme for ggplot
final_theme=theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        plot.title=element_text(size=16, face='bold', hjust=0.5)) #save as a grom



#### VARIABLE LISTS ####
#**************************************************************
# HERE, WE SAVE THE COLUMN HEADERS FOR THE .CSV FILES IN LIST FORM. YOU WILL NEED TO MAKE SURE THAT YOU CHOOSE THE
#     PROPER VARIABLE LIST FOR YOUR .CSV, BECAUSE SOMETIMES THE ORDER OF VARIABLES CHANGES! THESE ARE JUST SHORTENED
#     COLUMN NAMES THAT ARE APPLIED TO THE .CSV FILE WHEN IT IS READ INTO R SO THAT YOU DON'T HAVE TO RE-NAME 
#     COLUMNS. YOU MAY NEED TO ADD ADDITIONAL LISTS HERE, IN WHICH CASE, JUST COPY A LINE AND EDIT IT AS NEEDED.
#     EACH COLUMN NAME NEEDS TO BE INSIDE ' ' OR " " AND SEPARATED BY A COLUMN. THE 'c' INDICATES THAT THERE ARE 
#     MULTIPLE ITEMS IN YOUR LIST. KEEP IN MIND THAT CAPITALIZATION IS IMPORTANT IN R!

# define vabiable names lists
varnames1=c("obsno","datetime", "temp_C", "intens_lux", "batt", "attach", "stop", "eof")
varnames2=c("obsno","datetime", "temp_C", "intens_lux", "batt", "attach", "detach", "stop", "eof")
varnames3=c("obsno","datetime", "temp_C", "intens_lux", "batt", "eof")
varnames4=c("obsno","datetime", "temp_C", "intens_lux", "attach", "connected", "detach", "stop", "eof")
varnames5=c("obsno","datetime", "temp_C", "intens_lux", "batt", "stop", "eof")
varnames6=c("obsno","datetime", "temp_C", "intens_lux", "attach", "stop", "eof")
varnames7=c("obsno","datetime", "temp_C", "intens_lux", "attach", "detach", "stop", "eof")
varnames8=c("obsno","datetime", "temp_C", "intens_lux", "batt", "attach", "detach", "connected", "eof")
varnames9=c("obsno","datetime", "temp_C", "intens_lux", "batt", "attach", "connected", "detach", "eof")
varnames10=c("obsno","datetime", "temp_C", "intens_lux", "batt", "attach", "connected", "eof")
varnames11=c("obsno","datetime", "temp_C", "intens_lux", "attach", "detach", "connected", "eof")
varnames12=c("obsno","datetime", "temp_C", "intens_lux", "attach", "connect", "eof")



#### 2016-06-05 ####
#**************************************************************
# 10616536
PAN_536_20160605 <- read_csv("2016-06-05/10616536_Jun16_1.csv", 
                             skip=2, 
                             col_names=varnames1) %>% 
  mutate(fileorig="10616536_Jun16_1.csv",  #orig file name
         loggernum = substr(fileorig, 1, 8),
         depth_m = 0.5, 
         datetime = as.POSIXct(datetime, tz='UTC', format ='%m/%d/%y %I:%M:%S %p')) %>% 
  select(-obsno, -attach, -stop, -eof)

# 10616537
PAN_537_20160605 <- read_csv("2016-06-05/10616537_Jun16_1.csv", 
                             skip=2, 
                             col_names=varnames1) %>% 
  mutate(fileorig="10616537_Jun16_1.csv",  #orig file name
         loggernum = substr(fileorig, 1, 8),
         depth_m = 1, 
         datetime = as.POSIXct(datetime, tz='UTC', format ='%m/%d/%y %I:%M:%S %p')) %>% 
  select(-obsno, -attach, -stop, -eof)


# 10616538
PAN_538_20160605 <- read_csv("2016-06-05/10616538_Jun16_1.csv", 
                             skip=2, 
                             col_names=varnames1) %>% 
  mutate(fileorig="10616538_Jun16_1.csv",  #orig file name
         loggernum = substr(fileorig, 1, 8),
         depth_m = 2, 
         datetime = as.POSIXct(datetime, tz='UTC', format ='%m/%d/%y %I:%M:%S %p')) %>% 
  select(-obsno, -attach, -stop, -eof)

# 10616540
PAN_540_20160605 <- read_csv("2016-06-05/10616540_Jun16_1.csv", 
                             skip=2, 
                             col_names=varnames1) %>% 
  mutate(fileorig="10616540_Jun16_1.csv",  #orig file name
         loggernum = substr(fileorig, 1, 8),
         depth_m = 4, 
         datetime = as.POSIXct(datetime, tz='UTC', format ='%m/%d/%y %I:%M:%S %p')) %>% 
  select(-obsno, -attach, -stop, -eof)

# 10616544
PAN_544_20160605 <- read_csv("2016-06-05/10616544_Jun16_1.csv", 
                             skip=2, 
                             col_names=varnames1) %>% 
  mutate(fileorig="10616544_Jun16_1.csv",  #orig file name
         loggernum = substr(fileorig, 1, 8),
         depth_m = 6, 
         datetime = as.POSIXct(datetime, tz='UTC', format ='%m/%d/%y %I:%M:%S %p')) %>% 
  select(-obsno, -attach, -stop, -eof)

# 10616545
PAN_545_20160605 <- read_csv("2016-06-05/10616545_Jun16_1.csv", 
                             skip=2, 
                             col_names=varnames1) %>% 
  mutate(fileorig="10616545_Jun16_1.csv",  #orig file name
         loggernum = substr(fileorig, 1, 8),
         depth_m = 8, 
         datetime = as.POSIXct(datetime, tz='UTC', format ='%m/%d/%y %I:%M:%S %p')) %>% 
  select(-obsno, -attach, -stop, -eof)

# 10796876
# no data at 2016-06-05 download

# 10796877
PAN_877_20160605 <- read_csv("2016-06-05/10796877_Jun16_1.csv", 
                             skip=2, 
                             col_names=varnames2) %>% 
  mutate(fileorig="10796877_Jun16_1.csv",  #orig file name
         loggernum = substr(fileorig, 1, 8),
         depth_m = 15, 
         datetime = as.POSIXct(datetime, tz='UTC', format ='%m/%d/%y %I:%M:%S %p')) %>% 
  select(-obsno, -attach, -stop, -eof, -detach)

# 10796878
PAN_878_20160605 <- read_csv("2016-06-05/10796878_Jun16_1.csv", 
                             skip=2, 
                             col_names=varnames1) %>% 
  mutate(fileorig="10796878_Jun16_1.csv",  #orig file name
         loggernum = substr(fileorig, 1, 8),
         depth_m  = 20, 
         datetime = as.POSIXct(datetime, tz='UTC', format ='%m/%d/%y %I:%M:%S %p')) %>% 
  select(-obsno, -attach, -stop, -eof)

#merge all 20160605 files
PAN_20160605 <- merge(PAN_536_20160605, PAN_537_20160605, all=T)
PAN_20160605 <- merge(PAN_20160605, PAN_538_20160605, all=T)
PAN_20160605 <- merge(PAN_20160605, PAN_540_20160605, all=T)
PAN_20160605 <- merge(PAN_20160605, PAN_544_20160605, all=T)
PAN_20160605 <- merge(PAN_20160605, PAN_545_20160605, all=T)
PAN_20160605 <- merge(PAN_20160605, PAN_877_20160605, all=T)
PAN_20160605 <- merge(PAN_20160605, PAN_878_20160605, all=T)

#add column for download date
PAN_20160605$download <- '2016-06-05'

#plot to check data
ggplot(PAN_20160605, aes(x=datetime, y=temp_C, col=depth_m)) +
  geom_point()

#clean up workspace
 rm(PAN_536_20160605, PAN_537_20160605, PAN_538_20160605, PAN_540_20160605, 
    PAN_544_20160605, PAN_545_20160605, PAN_877_20160605, PAN_878_20160605)

#### 2016-06-12 ####
#**************************************************************
# 10616536
PAN_536_20160612 <- read_csv("2016-06-12/10616536_Jun16_2.csv", 
                             skip=2, 
                             col_names=varnames1) %>% 
  mutate(fileorig="10616536_Jun16_2.csv",  #orig file name
         loggernum = substr(fileorig, 1, 8),
         depth_m = 0.5, 
         datetime = as.POSIXct(datetime, tz='UTC', format ='%m/%d/%y %I:%M:%S %p')) %>% 
  select(-obsno, -attach, -stop, -eof)

# 10616537
PAN_537_20160612 <- read_csv("2016-06-12/10616537_Jun16_2.csv", 
           skip=2, 
           col_names=varnames3) %>% 
  mutate(fileorig="10616537_Jun16_2.csv",  #orig file name
         loggernum = substr(fileorig, 1, 8),
         depth_m = 1, 
         datetime = as.POSIXct(datetime, tz='UTC', format ='%m/%d/%y %I:%M:%S %p')) %>% 
  select(-obsno, -eof)

# 10616538
PAN_538_20160612 <- read_csv("2016-06-12/10616538_Jun16_2.csv", 
                             skip=2, 
                             col_names=varnames2) %>% 
  mutate(fileorig="10616538_Jun16_2.csv",  #orig file name
         loggernum = substr(fileorig, 1, 8),
         depth_m = 2, 
         datetime = as.POSIXct(datetime, tz='UTC', format ='%m/%d/%y %I:%M:%S %p')) %>% 
  select(-obsno, -attach, -detach, -stop, -eof)


# 10616540
PAN_540_20160612 <- read_csv("2016-06-12/10616540_Jun16_2.csv", 
                             skip=2, 
                             col_names=varnames2) %>% 
  mutate(fileorig="10616540_Jun16_2.csv",  #orig file name
         loggernum = substr(fileorig, 1, 8),
         depth_m = 4, 
         datetime = as.POSIXct(datetime, tz='UTC', format ='%m/%d/%y %I:%M:%S %p')) %>% 
  select(-obsno, -attach, -detach, -stop, -eof)


# 10616544
PAN_544_20160612 <- read_csv("2016-06-12/10616544_Jun16_2.csv", 
                             skip=2, 
                             col_names=varnames4) %>% 
  mutate(fileorig="10616544_Jun16_2.csv",  #orig file name
         loggernum = substr(fileorig, 1, 8),
         depth_m = 6, 
         datetime = as.POSIXct(datetime, tz='UTC', format ='%m/%d/%y %I:%M:%S %p')) %>% 
  select(-obsno, -attach, -connected, -detach, -stop, -eof)


# 10616545
PAN_545_20160612 <- read_csv("2016-06-12/10616545_Jun16_2.csv", 
                             skip=2, 
                             col_names=varnames1) %>% 
  mutate(fileorig="10616545_Jun16_2.csv",  #orig file name
         loggernum = substr(fileorig, 1, 8),
         depth_m = 8, 
         datetime = as.POSIXct(datetime, tz='UTC', format ='%m/%d/%y %I:%M:%S %p')) %>% 
  select(-obsno, -attach, -stop, -eof)

# 10796876
PAN_876_20160612 <- read_csv("2016-06-12/10796876_Jun16_2.csv", 
                             skip=2, 
                             col_names=varnames3) %>% 
  mutate(fileorig="10796876_Jun16_2.csv",  #orig file name
         loggernum = substr(fileorig, 1, 8),
         depth_m = 10, 
         datetime = as.POSIXct(datetime, tz='UTC', format ='%m/%d/%y %I:%M:%S %p')) %>% 
  select(-obsno, -eof)

# 10796877
PAN_877_20160612 <- read_csv("2016-06-12/10796877_Jun16_2.csv", 
                             skip=2, 
                             col_names=varnames1) %>% 
  mutate(fileorig="10796877_Jun16_2.csv",  #orig file name
         loggernum = substr(fileorig, 1, 8),
         depth_m = 15, 
         datetime = as.POSIXct(datetime, tz='UTC', format ='%m/%d/%y %I:%M:%S %p')) %>% 
  select(-obsno, -attach, -stop, -eof)

# 10796878
PAN_878_20160612 <- read_csv("2016-06-12/10796878_Jun16_2.csv", 
                             skip=2, 
                             col_names=varnames1) %>% 
  mutate(fileorig="10796878_Jun16_2.csv",  #orig file name
         loggernum = substr(fileorig, 1, 8),
         depth_m = 20, 
         datetime = as.POSIXct(datetime, tz='UTC', format ='%m/%d/%y %I:%M:%S %p')) %>% 
  select(-obsno, -attach, -stop, -eof)

#merge all 20160612 files
PAN_20160612 <- merge(PAN_536_20160612, PAN_537_20160612, all=T)
PAN_20160612 <- merge(PAN_20160612, PAN_538_20160612, all=T)
PAN_20160612 <- merge(PAN_20160612, PAN_540_20160612, all=T)
PAN_20160612 <- merge(PAN_20160612, PAN_544_20160612, all=T)
PAN_20160612 <- merge(PAN_20160612, PAN_545_20160612, all=T)
PAN_20160612 <- merge(PAN_20160612, PAN_876_20160612, all=T)
PAN_20160612 <- merge(PAN_20160612, PAN_877_20160612, all=T)
PAN_20160612 <- merge(PAN_20160612, PAN_878_20160612, all=T)

#add column for download date
PAN_20160612$download <- '2016-06-12'

#plot to check data
ggplot(PAN_20160612, aes(x=datetime, y=temp_C, col=depth_m)) +
  geom_point()

#clean up workspace
rm(PAN_536_20160612, PAN_537_20160612, PAN_538_20160612,PAN_540_20160612, PAN_544_20160612, 
   PAN_545_20160612, PAN_876_20160612,PAN_877_20160612, PAN_878_20160612)


#### 2016-07-11 ####
#**************************************************************
# 10616536
PAN_536_20160711 <- read_csv("2016-07-11/10616536_Jul16.csv", 
                             skip=2, 
                             col_names=varnames5) %>% 
  mutate(fileorig="10616536_Jul16.csv",  #orig file name
         loggernum = substr(fileorig, 1, 8),
         depth_m = 0.5, 
         datetime = as.POSIXct(datetime, tz='UTC', format ='%m/%d/%y %I:%M:%S %p')) %>% 
  select(-obsno, -stop, -eof)

# 10616537
PAN_537_20160711 <- read_csv("2016-07-11/10616537_Jul16.csv", 
                             skip=2, 
                             col_names=varnames1) %>% 
  mutate(fileorig="10616537_Jul16.csv",  #orig file name
         loggernum = substr(fileorig, 1, 8),
         depth_m = 1, 
         datetime = as.POSIXct(datetime, tz='UTC', format ='%m/%d/%y %I:%M:%S %p')) %>% 
  select(-obsno, - attach, -stop, -eof)

# 10616538
PAN_538_20160711 <- read_csv("2016-07-11/10616538_Jul16.csv", 
                             skip=2, 
                             col_names=varnames1) %>% 
  mutate(fileorig="10616538_Jul16.csv",  #orig file name
         loggernum = substr(fileorig, 1, 8),
         depth_m = 2, 
         datetime = as.POSIXct(datetime, tz='UTC', format ='%m/%d/%y %I:%M:%S %p')) %>% 
  select(-obsno, - attach, -stop, -eof)

# 10616540
PAN_540_20160711 <- read_csv("2016-07-11/10616540_Jul16.csv", 
                             skip=2, 
                             col_names=varnames1) %>% 
  mutate(fileorig="10616540_Jul16.csv",  #orig file name
         loggernum = substr(fileorig, 1, 8),
         depth_m = 4, 
         datetime = as.POSIXct(datetime, tz='UTC', format ='%m/%d/%y %I:%M:%S %p')) %>% 
  select(-obsno, - attach, -stop, -eof)

# 10616544
PAN_544_20160711 <- read_csv("2016-07-11/10616544_Jul16.csv", 
                             skip=2, 
                             col_names=varnames7) %>% 
  mutate(fileorig="10616544_Jul16.csv",  #orig file name
         loggernum = substr(fileorig, 1, 8),
         depth_m = 6, 
         datetime = as.POSIXct(datetime, tz='UTC', format ='%m/%d/%y %I:%M:%S %p')) %>% 
  select(-obsno, - attach, -detach, -stop, -eof)

# 10616545
PAN_545_20160711 <- read_csv("2016-07-11/10616545_Jul16.csv", 
                             skip=2, 
                             col_names=varnames2) %>% 
  mutate(fileorig="10616545_Jul16.csv",  #orig file name
         loggernum = substr(fileorig, 1, 8),
         depth_m = 8, 
         datetime = as.POSIXct(datetime, tz='UTC', format ='%m/%d/%y %I:%M:%S %p')) %>% 
  select(-obsno, - attach, -detach, -stop, -eof)

# 10616541
PAN_541_20160711 <- read_csv("2016-07-11/10616541_Jul16.csv", 
                             skip=2, 
                             col_names=varnames6) %>% 
  mutate(fileorig="10616541_Jul16.csv",  #orig file name
         loggernum = substr(fileorig, 1, 8),
         depth_m = 10, 
         datetime = as.POSIXct(datetime, tz='UTC', format ='%m/%d/%y %I:%M:%S %p')) %>% 
  select(-obsno, - attach, -stop, -eof)

# 10796877
PAN_877_20160711 <- read_csv("2016-07-11/10796877_Jul16.csv", 
                             skip=2, 
                             col_names=varnames2) %>% 
  mutate(fileorig="10796877_Jul16.csv",  #orig file name
         loggernum = substr(fileorig, 1, 8),
         depth_m = 15, 
         datetime = as.POSIXct(datetime, tz='UTC', format ='%m/%d/%y %I:%M:%S %p')) %>% 
  select(-obsno, - attach, -detach, -stop, -eof)

# 10796878
PAN_878_20160711 <- read_csv("2016-07-11/10796878_Jul16.csv", 
                             skip=2, 
                             col_names=varnames1) %>% 
  mutate(fileorig="10796878_Jul16.csv",  #orig file name
         loggernum = substr(fileorig, 1, 8),
         depth_m = 20, 
         datetime = as.POSIXct(datetime, tz='UTC', format ='%m/%d/%y %I:%M:%S %p')) %>% 
  select(-obsno, - attach, -stop, -eof)



#merge all 20160711 files
PAN_20160711 <- merge(PAN_536_20160711, PAN_537_20160711, all=T)
PAN_20160711 <- merge(PAN_20160711, PAN_538_20160711, all=T)
PAN_20160711 <- merge(PAN_20160711, PAN_540_20160711, all=T)
PAN_20160711 <- merge(PAN_20160711, PAN_544_20160711, all=T)
PAN_20160711 <- merge(PAN_20160711, PAN_545_20160711, all=T)
PAN_20160711 <- merge(PAN_20160711, PAN_541_20160711, all=T)
PAN_20160711 <- merge(PAN_20160711, PAN_877_20160711, all=T)
PAN_20160711 <- merge(PAN_20160711, PAN_878_20160711, all=T)

#add column for download date
PAN_20160711$download <- '2016-07-11'

#plot to check data
ggplot(PAN_20160711, aes(x=datetime, y=temp_C, col=depth_m)) +
  geom_point()


#clean up workspace
rm(PAN_536_20160711, PAN_537_20160711, PAN_538_20160711, PAN_540_20160711, PAN_544_20160711, 
   PAN_545_20160711, PAN_541_20160711, PAN_877_20160711, PAN_878_20160711)

#### 2016-08-11 ####
#**************************************************************
# 10616536
PAN_536_20160811 <-read_csv("2016-08-11/10616536_Aug16.csv", 
                            skip=2, 
                            col_names=varnames1) %>% 
  mutate(fileorig="10616536_Aug16.csv",  #orig file name
         loggernum = substr(fileorig, 1, 8),
         depth_m = 0.5, 
         datetime = as.POSIXct(datetime, tz='UTC', format ='%m/%d/%y %I:%M:%S %p')) %>% 
  select(-obsno, - attach, -stop, -eof) 

# 10616537
PAN_537_20160811 <- read_csv("2016-08-11/10616537_Aug16.csv", 
                             skip=2, 
                             col_names=varnames2) %>% 
  mutate(fileorig="10616537_Aug16.csv",  #orig file name
         loggernum = substr(fileorig, 1, 8),
         depth_m = 1, 
         datetime = as.POSIXct(datetime, tz='UTC', format ='%m/%d/%y %I:%M:%S %p')) %>% 
  select(-obsno, - attach, -detach, -stop, -eof) 

# 10616538
PAN_538_20160811 <- read_csv("2016-08-11/10616538_Aug16.csv", 
                             skip=2, 
                             col_names=varnames1) %>% 
  mutate(fileorig="10616538_Aug16.csv",  #orig file name
         loggernum = substr(fileorig, 1, 8),
         depth_m = 2, 
         datetime = as.POSIXct(datetime, tz='UTC', format ='%m/%d/%y %I:%M:%S %p')) %>% 
  select(-obsno, - attach, -stop, -eof) 

# 10616540
PAN_540_20160811 <- read_csv("2016-08-11/10616540_Aug16.csv", 
                             skip=2, 
                             col_names=varnames1) %>% 
  mutate(fileorig="10616540_Aug16.csv",  #orig file name
         loggernum = substr(fileorig, 1, 8),
         depth_m = 4, 
         datetime = as.POSIXct(datetime, tz='UTC', format ='%m/%d/%y %I:%M:%S %p')) %>% 
  select(-obsno, - attach, -stop, -eof) 

# 10616544
#no download available

# 10616545
PAN_545_20160811 <- read_csv("2016-08-11/10616545_Aug16.csv", 
                             skip=2, 
                             col_names=varnames2) %>% 
  mutate(fileorig="10616545_Aug16.csv",  #orig file name
         loggernum = substr(fileorig, 1, 8),
         depth_m = 8, 
         datetime = as.POSIXct(datetime, tz='UTC', format ='%m/%d/%y %I:%M:%S %p')) %>% 
  select(-obsno, - attach, -detach, -stop, -eof) 

# 10616541
#no download available

# 10796877
PAN_877_20160811 <- read_csv("2016-08-11/10796877_Aug16.csv", 
                             skip=2, 
                             col_names=varnames1) %>% 
  mutate(fileorig="10796877_Aug16.csv",  #orig file name
         loggernum = substr(fileorig, 1, 8),
         depth_m = 15, 
         datetime = as.POSIXct(datetime, tz='UTC', format ='%m/%d/%y %I:%M:%S %p')) %>% 
  select(-obsno, - attach, -stop, -eof) 

# 10796878
PAN_878_20160811 <- read_csv("2016-08-11/10796878_Aug16.csv", 
                             skip=2, 
                             col_names=varnames2) %>% 
  mutate(fileorig="10796878_Aug16.csv",  #orig file name
         loggernum = substr(fileorig, 1, 8),
         depth_m = 20, 
         datetime = as.POSIXct(datetime, tz='UTC', format ='%m/%d/%y %I:%M:%S %p')) %>% 
  select(-obsno, - attach, -detach, -stop, -eof) 


#merge all 20160811 files
PAN_20160811 <- merge(PAN_536_20160811, PAN_537_20160811, all=T)
PAN_20160811 <- merge(PAN_20160811, PAN_538_20160811, all=T)
PAN_20160811 <- merge(PAN_20160811, PAN_540_20160811, all=T)
PAN_20160811 <- merge(PAN_20160811, PAN_545_20160811, all=T)
PAN_20160811 <- merge(PAN_20160811, PAN_877_20160811, all=T)
PAN_20160811 <- merge(PAN_20160811, PAN_878_20160811, all=T)

#add column for download date
PAN_20160811$download <- '2016-08-11'

#plot to check data
ggplot(PAN_20160811, aes(x=datetime, y=temp_C, col=depth_m)) +
  geom_point()

#clean up workspace
rm(PAN_536_20160811, PAN_537_20160811, PAN_538_20160811, PAN_540_20160811, 
   PAN_545_20160811, PAN_877_20160811, PAN_878_20160811)

#### 2016-09-12 ####
#**************************************************************
# 10616536
PAN_536_20160912 <- read_csv("2016-09-12/10616536_Sept16.csv", 
                             skip=2, 
                             col_names=varnames1) %>% 
  mutate(fileorig="10616536_Sept16.csv",  #orig file name
         loggernum = substr(fileorig, 1, 8),
         depth_m = 0.5, 
         datetime = as.POSIXct(datetime, tz='UTC', format ='%m/%d/%y %I:%M:%S %p')) %>% 
  select(-obsno, - attach, -stop, -eof) 

# 10616537
PAN_537_20160912 <- read_csv("2016-09-12/10616537_Sept16.csv", 
                             skip=2, 
                             col_names=varnames1) %>% 
  mutate(fileorig="10616537_Sept16.csv",  #orig file name
         loggernum = substr(fileorig, 1, 8),
         depth_m = 1, 
         datetime = as.POSIXct(datetime, tz='UTC', format ='%m/%d/%y %I:%M:%S %p')) %>% 
  select(-obsno, - attach, -stop, -eof) 

# 10616538
PAN_538_20160912 <- read_csv("2016-09-12/10616538_Sept16.csv", 
                             skip=2, 
                             col_names=varnames2) %>% 
  mutate(fileorig="10616538_Sept16.csv",  #orig file name
         loggernum = substr(fileorig, 1, 8),
         depth_m = 2, 
         datetime = as.POSIXct(datetime, tz='UTC', format ='%m/%d/%y %I:%M:%S %p')) %>% 
  select(-obsno, - attach, -detach, -stop, -eof) 

# 10616540
PAN_540_20160912 <- read_csv("2016-09-12/10616540_Sept16.csv", 
                             skip=2, 
                             col_names=varnames1) %>% 
  mutate(fileorig="10616540_Sept16.csv",  #orig file name
         loggernum = substr(fileorig, 1, 8),
         depth_m = 4, 
         datetime = as.POSIXct(datetime, tz='UTC', format ='%m/%d/%y %I:%M:%S %p')) %>% 
  select(-obsno, - attach, -stop, -eof) 

# 10616544
PAN_544_20160912 <- read_csv("2016-09-12/10616544_Sept16.csv", 
                             skip=2, 
                             col_names=varnames6) %>% 
  mutate(fileorig="10616544_Sept16.csv",  #orig file name
         loggernum = substr(fileorig, 1, 8),
         depth_m = 6, 
         datetime = as.POSIXct(datetime, tz='UTC', format ='%m/%d/%y %I:%M:%S %p')) %>% 
  select(-obsno, - attach, -stop, -eof) 

# 10616545
PAN_545_20160912 <- read_csv("2016-09-12/10616545_Sept16.csv", 
                             skip=2, 
                             col_names=varnames2) %>% 
  mutate(fileorig="10616545_Sept16.csv",  #orig file name
         loggernum = substr(fileorig, 1, 8),
         depth_m = 8, 
         datetime = as.POSIXct(datetime, tz='UTC', format ='%m/%d/%y %I:%M:%S %p')) %>% 
  select(-obsno, - attach, -detach, -stop, -eof) 

# 10616541
PAN_541_20160912 <- read_csv("2016-09-12/10616541_Sept16.csv", 
                             skip=2, 
                             col_names=varnames7) %>% 
  mutate(fileorig="10616541_Sept16.csv",  #orig file name
         loggernum = substr(fileorig, 1, 8),
         depth_m = 10, 
         datetime = as.POSIXct(datetime, tz='UTC', format ='%m/%d/%y %I:%M:%S %p')) %>% 
  select(-obsno, - attach, -detach, -stop, -eof) 

# 10796877
PAN_877_20160912 <- read_csv("2016-09-12/10796877_Sept16.csv", 
                             skip=2, 
                             col_names=varnames1) %>% 
  mutate(fileorig="10796877_Sept16.csv",  #orig file name
         loggernum = substr(fileorig, 1, 8),
         depth_m = 15, 
         datetime = as.POSIXct(datetime, tz='UTC', format ='%m/%d/%y %I:%M:%S %p')) %>% 
  select(-obsno, - attach, -stop, -eof) 

# 10796878
PAN_878_20160912 <- read_csv("2016-09-12/10796878_Sept16.csv", 
                             skip=2, 
                             col_names=varnames2) %>% 
  mutate(fileorig="10796878_Sept16.csv",  #orig file name
         loggernum = substr(fileorig, 1, 8),
         depth_m = 20, 
         datetime = as.POSIXct(datetime, tz='UTC', format ='%m/%d/%y %I:%M:%S %p')) %>% 
  select(-obsno, - attach, -detach, -stop, -eof) 

#merge all 20160912 files
PAN_20160912 <- merge(PAN_536_20160912, PAN_537_20160912, all=T)
PAN_20160912 <- merge(PAN_20160912, PAN_538_20160912, all=T)
PAN_20160912 <- merge(PAN_20160912, PAN_540_20160912, all=T)
PAN_20160912 <- merge(PAN_20160912, PAN_544_20160912, all=T)
PAN_20160912 <- merge(PAN_20160912, PAN_545_20160912, all=T)
PAN_20160912 <- merge(PAN_20160912, PAN_541_20160912, all=T)
PAN_20160912 <- merge(PAN_20160912, PAN_877_20160912, all=T)
PAN_20160912 <- merge(PAN_20160912, PAN_878_20160912, all=T)

#add column for download date
PAN_20160912$download <- '2016-09-12'

#plot to check data
ggplot(PAN_20160912, aes(x=datetime, y=temp_C, col=depth_m)) +
  geom_point()

#clean up workspace
rm(PAN_536_20160912, PAN_537_20160912, PAN_538_20160912, PAN_540_20160912, PAN_544_20160912, 
   PAN_545_20160912, PAN_541_20160912, PAN_877_20160912, PAN_878_20160912)

#### 2016-10-07 ####
#**************************************************************
# 10616536
PAN_536_20161007 <- read_csv("2016-10-07/10616536_Oct16.csv", 
                             skip=2, 
                             col_names=varnames9) %>% 
  mutate(fileorig="10616536_Oct16.csv",  #orig file name
         loggernum = substr(fileorig, 1, 8),
         depth_m = 0.5, 
         datetime = as.POSIXct(datetime, tz='UTC', format ='%m/%d/%y %I:%M:%S %p')) %>% 
  select(-obsno, - attach, -connected, -detach, -eof) 

# 10616537
PAN_537_20161007 <- read_csv("2016-10-07/10616537_Oct16.csv", 
                             skip=2, 
                             col_names=varnames8) %>% 
  mutate(fileorig="10616537_Oct16.csv",  #orig file name
         loggernum = substr(fileorig, 1, 8),
         depth_m = 1, 
         datetime = as.POSIXct(datetime, tz='UTC', format ='%m/%d/%y %I:%M:%S %p')) %>% 
  select(-obsno, - attach, -connected, -detach, -eof) 

# 10616538
PAN_538_20161007 <- read_csv("2016-10-07/10616538_Oct16.csv", 
                             skip=2, 
                             col_names=varnames9) %>% 
  mutate(fileorig="10616538_Oct16.csv",  #orig file name
         loggernum = substr(fileorig, 1, 8),
         depth_m = 2, 
         datetime = as.POSIXct(datetime, tz='UTC', format ='%m/%d/%y %I:%M:%S %p')) %>% 
  select(-obsno, - attach, -connected, -detach, -eof) 

# 10616540
PAN_540_20161007 <- read_csv("2016-10-07/10616540_Oct16.csv", 
                             skip=2, 
                             col_names=varnames10) %>% 
  mutate(fileorig="10616540_Oct16.csv",  #orig file name
         loggernum = substr(fileorig, 1, 8),
         depth_m = 4, 
         datetime = as.POSIXct(datetime, tz='UTC', format ='%m/%d/%y %I:%M:%S %p')) %>% 
  select(-obsno, - attach, -connected, -eof) 

# 10616544
PAN_544_20161007 <- read_csv("2016-10-07/10616544_Oct16.csv", 
                             skip=2, 
                             col_names=varnames11) %>% 
  mutate(fileorig="10616544_Oct16.csv",  #orig file name
         loggernum = substr(fileorig, 1, 8),
         depth_m = 6, 
         datetime = as.POSIXct(datetime, tz='UTC', format ='%m/%d/%y %I:%M:%S %p')) %>% 
  select(-obsno, - attach, -detach, -connected, -eof) 

# 10616545
PAN_545_20161007 <- read_csv("2016-10-07/10616545_Oct16.csv", 
                             skip=2, 
                             col_names=varnames9) %>% 
  mutate(fileorig="10616545_Oct16.csv",  #orig file name
         loggernum = substr(fileorig, 1, 8),
         depth_m = 8, 
         datetime = as.POSIXct(datetime, tz='UTC', format ='%m/%d/%y %I:%M:%S %p')) %>% 
  select(-obsno, - attach, -detach, -connected, -eof) 

# 10616541
PAN_541_20161007 <- read_csv("2016-10-07/10616541_Oct16.csv", 
                             skip=2, 
                             col_names=varnames12) %>% 
  mutate(fileorig="10616541_Oct16.csv",  #orig file name
         loggernum = substr(fileorig, 1, 8),
         depth_m = 10, 
         datetime = as.POSIXct(datetime, tz='UTC', format ='%m/%d/%y %I:%M:%S %p')) %>% 
  select(-obsno, - attach, -connect, -eof) 

# 10796877
PAN_877_20161007 <- read_csv("2016-10-07/10796877_Oct16.csv", 
                             skip=2, 
                             col_names=varnames10) %>% 
  mutate(fileorig="10796877_Oct16.csv",  #orig file name
         loggernum = substr(fileorig, 1, 8),
         depth_m = 15, 
         datetime = as.POSIXct(datetime, tz='UTC', format ='%m/%d/%y %I:%M:%S %p')) %>% 
  select(-obsno, - attach, -connected, -eof) 

# 10796878
PAN_878_20161007 <- read_csv("2016-10-07/10796878_Oct16.csv", 
                             skip=2, 
                             col_names=varnames9) %>% 
  mutate(fileorig="10796878_Oct16.csv",  #orig file name
         loggernum = substr(fileorig, 1, 8),
         depth_m = 20, 
         datetime = as.POSIXct(datetime, tz='UTC', format ='%m/%d/%y %I:%M:%S %p')) %>% 
  select(-obsno, - attach, -connected, -detach,-eof) 


#merge all 20161007 files
PAN_20161007 <- merge(PAN_536_20161007, PAN_537_20161007, all=T)
PAN_20161007 <- merge(PAN_20161007, PAN_538_20161007, all=T)
PAN_20161007 <- merge(PAN_20161007, PAN_540_20161007, all=T)
PAN_20161007 <- merge(PAN_20161007, PAN_544_20161007, all=T)
PAN_20161007 <- merge(PAN_20161007, PAN_545_20161007, all=T)
PAN_20161007 <- merge(PAN_20161007, PAN_541_20161007, all=T)
PAN_20161007 <- merge(PAN_20161007, PAN_877_20161007, all=T)
PAN_20161007 <- merge(PAN_20161007, PAN_878_20161007, all=T)


#add column for download date
PAN_20161007$download <- '2016-10-07'

#plot to check data
ggplot(PAN_20161007, aes(x=datetime, y=temp_C, col=depth_m)) +
  geom_point()

#clean up workspace
rm(PAN_536_20161007, PAN_537_20161007, PAN_538_20161007, PAN_540_20161007, PAN_544_20161007, 
   PAN_545_20161007, PAN_541_20161007, PAN_877_20161007, PAN_878_20161007)

#### Create L0 dataset ####
PAN_L0 <- merge(PAN_20160605, PAN_20160612, all=T)
PAN_L0 <- merge(PAN_L0, PAN_20160711, all=T)
PAN_L0 <- merge(PAN_L0, PAN_20160811, all=T)
PAN_L0 <- merge(PAN_L0, PAN_20160912, all=T)
PAN_L0 <- merge(PAN_L0, PAN_20161007, all=T)

ggplot(PAN_L0, aes(x=datetime, y=temp_C, col=as.factor(depth_m))) +
  geom_point() + 
  labs(title='Panther Pond Buoy Data, Level 0', 
       x='date', 
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                            "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  final_theme
# ggsave('G:/My Drive/Panther Pond/r export/figures/L0/2016/2016 L0.tiff', width=4.5, height = 4, dpi=200)

#clean up workspace
rm(PAN_20160605, PAN_20160612, PAN_20160711, PAN_20160811, PAN_20160912, PAN_20161007)

setwd('G:/My Drive/Panther Pond/r export/datasets/L0')
# write.csv(PAN_L0, 'PantherBuoy_L0_2016.csv', row.names = F)

#### view and clean data, creating L1 dataset ####

#copy L0 to L1 dataset
PAN_L1 <- PAN_L0
setwd('G:/My Drive/Panther Pond/r export/')

####May 2016####
#May 2016
ggplot(subset(PAN_L0, subset=(datetime>=as.POSIXct('2016-05-01', tz='GMT') &datetime < as.POSIXct('2016-06-01', tz='GMT'))), aes(x=datetime, y=temp_C, color=loggernum)) +
  geom_point() +
  labs(title='Panther Pond L0, May 2016', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_colorblind()+
  final_theme
ggsave('figures/L0/2016/201605 L0.tiff', width=4.5, height = 4, dpi=200)

ggplot(subset(PAN_L0, subset=(datetime>=as.POSIXct('2016-05-07', tz='GMT') & datetime < as.POSIXct('2016-05-07 15:00', tz='GMT'))), aes(x=datetime, y=temp_C, color=loggernum)) +
  geom_point() +
  labs(title='Panther Pond L0, May 2016 Deployment', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_colorblind()+   
  final_theme
#ggsave('figures/L0/2016/20160507 L0 deploy.tiff', width=4.5, height = 4, dpi=200)

ix=which(PAN_L1$datetime<as.POSIXct('2016-05-07 11:50', tz='GMT'))
for(i in c('temp_C', 'intens_lux')) {PAN_L1[ix,i]=NA}

ggplot(subset(PAN_L1, subset=(datetime>=as.POSIXct('2016-05-07', tz='GMT') & datetime < as.POSIXct('2016-05-07 15:00', tz='GMT'))), aes(x=datetime, y=temp_C, color=loggernum)) +
  geom_point() +
  labs(title='Panther Pond L1, May 2016 Deployment', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_colorblind()+   
  final_theme
#ggsave('figures/L1/2016/20160507 L1 deploy.tiff', width=4.5, height = 4, dpi=200)

ggplot(subset(PAN_L1, subset=(datetime>=as.POSIXct('2016-05-01', tz='GMT') &datetime < as.POSIXct('2016-06-01', tz='GMT'))), aes(x=datetime, y=temp_C, color=loggernum)) +
  geom_point() +
  labs(title='Panther Pond L1, May 2016', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_colorblind()+   
  final_theme
# ggsave('figures/L1/2016/201605 L1 a.tiff', width=4.5, height = 4, dpi=200)

#May 28
ggplot(subset(PAN_L0, subset=(datetime>=as.POSIXct('2016-05-28 12:00', tz='GMT') & datetime < as.POSIXct('2016-05-28 18:00', tz='GMT'))), aes(x=datetime, y=temp_C, color=loggernum)) +
  geom_point() +
  labs(title='Panther Pond L0, May 2016 Visit', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_colorblind()+   
  final_theme
#ggsave('figures/L0/2016/20160528 L0 visit.tiff', width=4.5, height = 4, dpi=200)

ix=which(PAN_L1$datetime>as.POSIXct('2016-05-28 14:40', tz='GMT') & PAN_L1$datetime<as.POSIXct('2016-05-28 15:10', tz='GMT'))
for(i in c('temp_C', 'intens_lux')) {PAN_L1[ix,i]=NA}

ggplot(subset(PAN_L1, subset=(datetime>=as.POSIXct('2016-05-28 12:00', tz='GMT') & datetime < as.POSIXct('2016-05-28 18:00', tz='GMT'))), aes(x=datetime, y=temp_C, color=loggernum)) +
  geom_point() +
  labs(title='Panther Pond L1, May 2016 Visit', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_colorblind()+   
  final_theme
# ggsave('figures/L1/2016/20160528 L1 visit.tiff', width=4.5, height = 4, dpi=200)

ggplot(subset(PAN_L1, subset=(datetime>=as.POSIXct('2016-05-01', tz='GMT') &datetime < as.POSIXct('2016-06-01', tz='GMT'))), aes(x=datetime, y=temp_C, color=loggernum)) +
  geom_point() +
  labs(title='Panther Pond L1, May 2016', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_colorblind()+   
  final_theme
# ggsave('figures/L1/2016/201605 L1 b.tiff', width=4.5, height = 4, dpi=200)

####June 2016####
#June 2016
ggplot(subset(PAN_L0, subset=(datetime>=as.POSIXct('2016-06-01', tz='GMT') &datetime < as.POSIXct('2016-07-01', tz='GMT'))), aes(x=datetime, y=temp_C, color=loggernum)) +
  geom_point() +
  labs(title='Panther Pond L0, June 2016', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
                              "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  final_theme
# ggsave('figures/L0/2016/201606 L0.tiff', width=4.5, height = 4, dpi=200)

#June 5
ggplot(subset(PAN_L0, subset=(datetime>=as.POSIXct('2016-06-05 9:00', tz='GMT') & datetime < as.POSIXct('2016-06-05 12:00', tz='GMT'))), aes(x=datetime, y=temp_C, color=loggernum)) +
  geom_point() +
  labs(title='Panther Pond L0, June 2016 download 1', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
                              "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  final_theme
#ggsave('figures/L0/2016/20160605 L0 download 1.tiff', width=4.5, height = 4, dpi=200)

ix=which(PAN_L1$datetime>=as.POSIXct('2016-06-05 10:20', tz='GMT') & PAN_L1$datetime<=as.POSIXct('2016-06-05 10:40', tz='GMT'))
for(i in c('temp_C', 'intens_lux')) {PAN_L1[ix,i]=NA}

ggplot(subset(PAN_L1, subset=(datetime>=as.POSIXct('2016-06-05 9:00', tz='GMT') & datetime < as.POSIXct('2016-06-05 12:00', tz='GMT'))), aes(x=datetime, y=temp_C, color=loggernum)) +
  geom_point() +
  labs(title='Panther Pond L1, June 2016 download 1', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
                              "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  final_theme
#ggsave('figures/L1/2016/201606 L1 download 1.tiff', width=4.5, height = 4, dpi=200)

#June 12
ggplot(subset(PAN_L0, subset=(datetime>=as.POSIXct('2016-06-12 8:00', tz='GMT') & datetime < as.POSIXct('2016-06-12 15:00', tz='GMT'))), aes(x=datetime, y=temp_C, color=loggernum)) +
  geom_point() +
  labs(title='Panther Pond L0, June 2016 download 2', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
                              "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  final_theme
#ggsave('figures/L0/2016/20160612 L0 download 2.tiff', width=4.5, height = 4, dpi=200)

#June 16
ggplot(subset(PAN_L0, subset=(datetime>=as.POSIXct('2016-06-16 8:00', tz='GMT') & datetime < as.POSIXct('2016-06-16 12:00', tz='GMT'))), aes(x=datetime, y=temp_C, color=loggernum)) +
  geom_point() +
  labs(title='Panther Pond L0, June 2016 redeploy', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
                              "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  final_theme
#ggsave('figures/L0/2016/20160616 L0 redeploy.tiff', width=4.5, height = 4, dpi=200)

ix=which(PAN_L1$datetime>=as.POSIXct('2016-06-12 09:10', tz='GMT') & PAN_L1$datetime<=as.POSIXct('2016-06-16 10:00', tz='GMT'))
for(i in c('temp_C', 'intens_lux')) {PAN_L1[ix,i]=NA}

#June 12
ggplot(subset(PAN_L1, subset=(datetime>=as.POSIXct('2016-06-12 8:00', tz='GMT') & datetime < as.POSIXct('2016-06-12 15:00', tz='GMT'))), aes(x=datetime, y=temp_C, color=loggernum)) +
  geom_point() +
  labs(title='Panther Pond L1, June 2016 download 2', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
                              "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  final_theme
#ggsave('figures/L1/2016/20160612 L1 download 2.tiff', width=4.5, height = 4, dpi=200)

#June 16
ggplot(subset(PAN_L1, subset=(datetime>=as.POSIXct('2016-06-16 8:00', tz='GMT') & datetime < as.POSIXct('2016-06-16 12:00', tz='GMT'))), aes(x=datetime, y=temp_C, color=loggernum)) +
  geom_point() +
  labs(title='Panther Pond L1, June 2016 redeploy', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
                              "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  final_theme
#ggsave('figures/L1/2016/20160616 L1 redeploy.tiff', width=4.5, height = 4, dpi=200)


#compare 6/5-12 to 6/16-20 for the top 3 loggers
ggplot(subset(PAN_L1, subset=(datetime>=as.POSIXct('2016-06-05', tz='GMT') & datetime < as.POSIXct('2016-06-12', tz='GMT'))), aes(x=datetime, y=temp_C, color=loggernum)) +
  geom_point() +
  labs(title='Panther Pond L1, June 2016 5-11', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
                              "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  coord_cartesian(ylim=c(16,24)) +
  final_theme
#ggsave('figures/L0/2016/20160605-20160612 questionable.tiff', width=4.5, height = 4, dpi=200)

ggplot(subset(PAN_L1, subset=(datetime>=as.POSIXct('2016-06-16', tz='GMT') & datetime < as.POSIXct('2016-06-22', tz='GMT'))), aes(x=datetime, y=temp_C, color=loggernum)) +
  geom_point() +
  labs(title='Panther Pond L1, June 2016 16-22', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
                              "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  coord_cartesian(ylim=c(16,24)) +
  final_theme
# ggsave('figures/L0/2016/20160616-20160621 questionable.tiff', width=4.5, height = 4, dpi=200)
#looks fine upon larger timeframe

#look at 0616 redeploy
ggplot(subset(PAN_L1, subset=(datetime>=as.POSIXct('2016-06-16', tz='GMT') & datetime < as.POSIXct('2016-06-23', tz='GMT'))), aes(x=datetime, y=temp_C, color=loggernum)) +
  geom_point() +
  labs(title='Panther Pond L1, June 2016 16-22', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
                              "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  final_theme
#ggsave('figures/L0/2016/20160616-20160622 questionable lower.tiff', width=4.5, height = 4, dpi=200)

#look at 0619 
ggplot(subset(PAN_L1, subset=(datetime>=as.POSIXct('2016-06-19', tz='GMT') & datetime < as.POSIXct('2016-06-20', tz='GMT'))), aes(x=datetime, y=temp_C, color=loggernum)) +
  geom_point() +
  labs(title='Panther Pond L1, June 2016 19', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
                              "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  final_theme
# ggsave('figures/L0/2016/20160619.tiff', width=4.5, height = 4, dpi=200)

PAN_L1 <- PAN_L1 %>% 
  mutate_at(vars(temp_C, intens_lux),
            funs(case_when(datetime>=as.POSIXct('2016-06-16 10:00', tz='GMT') & datetime<=as.POSIXct('2016-06-19 18:00', tz='GMT') & (loggernum==10796877 | loggernum==10796878) ~ NA_real_,
                           TRUE ~ .)))

#look at 0616 redeploy
ggplot(subset(PAN_L1, subset=(datetime>=as.POSIXct('2016-06-16', tz='GMT') & datetime < as.POSIXct('2016-06-23', tz='GMT'))), aes(x=datetime, y=temp_C, color=loggernum)) +
  geom_point() +
  labs(title='Panther Pond L1, June 2016 16-22, v2', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
                              "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  final_theme
# ggsave('figures/L1/2016/20160616-20160622v2.tiff', width=4.5, height = 4, dpi=200)


#CLEAN MONTH
ggplot(subset(PAN_L1, subset=(datetime>=as.POSIXct('2016-06-01', tz='GMT') &datetime < as.POSIXct('2016-07-01', tz='GMT'))), aes(x=datetime, y=temp_C, color=loggernum)) +
  geom_point() +
  labs(title='Panther Pond L1, June 2016', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  final_theme
ggplot(subset(PAN_L1, subset=(datetime>=as.POSIXct('2016-06-01', tz='GMT') &datetime < as.POSIXct('2016-07-01', tz='GMT'))), aes(x=datetime, y=temp_C, color=as.factor(depth_m))) +
  geom_point() +
  labs(title='Panther Pond L1, June 2016', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  final_theme
# ggsave('figures/L1/2016/201606 L1.tiff', width=4.5, height = 4, dpi=200)

####JUly2016####
#July 2016
ggplot(subset(PAN_L0, subset=(datetime>=as.POSIXct('2016-07-01', tz='GMT') &datetime < as.POSIXct('2016-08-01', tz='GMT'))), aes(x=datetime, y=temp_C, color=loggernum)) +
  geom_point() +
  labs(title='Panther Pond L0, July 2016', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  final_theme
# ggsave('figures/L0/2016/201607 L0.tiff', width=4.5, height = 4, dpi=200)

#download
ggplot(subset(PAN_L0, subset=(datetime>=as.POSIXct('2016-07-11 16:00', tz='GMT') & datetime < as.POSIXct('2016-07-11 20:00', tz='GMT'))), aes(x=datetime, y=temp_C, color=loggernum)) +
  geom_point() +
  labs(title='Panther Pond L0, July 2016 download', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  final_theme
# ggsave('figures/L0/2016/20160711 L0 download.tiff', width=4.5, height = 4, dpi=200)

ix=which(PAN_L1$datetime>=as.POSIXct('2016-07-11 18:50', tz='GMT') & PAN_L1$datetime<=as.POSIXct('2016-07-11 19:10', tz='GMT'))
for(i in c('temp_C', 'intens_lux')) {PAN_L1[ix,i]=NA}

#download
ggplot(subset(PAN_L1, subset=(datetime>=as.POSIXct('2016-07-11 16:00', tz='GMT') & datetime < as.POSIXct('2016-07-11 20:00', tz='GMT'))), aes(x=datetime, y=temp_C, color=loggernum)) +
  geom_point() +
  labs(title='Panther Pond L1, July 2016 download', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  final_theme
#ggsave('figures/L1/2016/20160711 L1 download.tiff', width=4.5, height = 4, dpi=200)

#bottom sensor 77 issue
ggplot(subset(PAN_L1, subset=(datetime>=as.POSIXct('2016-07-11', tz='GMT') & datetime < as.POSIXct('2016-07-15', tz='GMT'))), aes(x=datetime, y=temp_C, color=loggernum)) +
  geom_point() +
  labs(title='Panther Pond L0, July 2016 bottom sensor issue', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  final_theme
#ggsave('figures/L0/2016/20160711-16 L0 bottom 77.tiff', width=4.5, height = 4, dpi=200)

ix=which(PAN_L1$datetime>=as.POSIXct('2016-07-11 19:20', tz='GMT') & PAN_L1$datetime<=as.POSIXct('2016-07-14 18:20', tz='GMT') & (PAN_L1$loggernum==10796877 | PAN_L1$loggernum==10616541))
for(i in c('temp_C', 'intens_lux')) {PAN_L1[ix,i]=NA}

ggplot(subset(PAN_L1, subset=(datetime>=as.POSIXct('2016-07-11', tz='GMT') & datetime < as.POSIXct('2016-07-15', tz='GMT'))), aes(x=datetime, y=temp_C, color=loggernum)) +
  geom_point() +
  labs(title='Panther Pond L1, July 2016 bottom sensor issue', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  final_theme
# ggsave('figures/L1/2016/20160711-16 L1 bottom 77 41.tiff', width=4.5, height = 4, dpi=200)

#bottom sensor 78 issue
ggplot(subset(PAN_L1, subset=(datetime>=as.POSIXct('2016-07-11', tz='GMT') & datetime < as.POSIXct('2016-07-17', tz='GMT'))), aes(x=datetime, y=temp_C, color=loggernum)) +
  geom_point() +
  labs(title='Panther Pond L0, July 2016 bottom sensor issue', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  final_theme
# ggsave('figures/L0/2016/20160711-16 L0 bottom 78.tiff', width=4.5, height = 4, dpi=200)

ix=which(PAN_L1$datetime>=as.POSIXct('2016-07-11 19:20', tz='GMT') & PAN_L1$datetime<=as.POSIXct('2016-07-15 19:40', tz='GMT') & PAN_L1$loggernum==10796878)
for(i in c('temp_C', 'intens_lux')) {PAN_L1[ix,i]=NA}

ggplot(subset(PAN_L1, subset=(datetime>=as.POSIXct('2016-07-11', tz='GMT') & datetime < as.POSIXct('2016-07-17', tz='GMT'))), aes(x=datetime, y=temp_C, color=loggernum)) +
  geom_point() +
  labs(title='Panther Pond L1, July 2016 bottom sensor issue', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  final_theme
# ggsave('figures/L1/2016/20160711-17 L1 bottom 78.tiff', width=4.5, height = 4, dpi=200)

ggplot(subset(PAN_L1, subset=(datetime>=as.POSIXct('2016-07-01', tz='GMT') &datetime < as.POSIXct('2016-08-01', tz='GMT'))), aes(x=datetime, y=temp_C, color=loggernum)) +
  geom_point() +
  labs(title='Panther Pond L1, July 2016', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  final_theme
# ggsave('figures/L1/2016/201607 L1.tiff', width=4.5, height = 4, dpi=200)


####August 2016####
#August 2016
ggplot(subset(PAN_L0, subset=(datetime>=as.POSIXct('2016-08-01', tz='GMT') &datetime < as.POSIXct('2016-09-01', tz='GMT'))), aes(x=datetime, y=temp_C, color=loggernum)) +
  geom_point() +
  labs(title='Panther Pond L0, August 2016', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  final_theme
# ggsave('figures/L0/2016/201608 L0.tiff', width=4.5, height = 4, dpi=200)

#download
ggplot(subset(PAN_L0, subset=(datetime>=as.POSIXct('2016-08-11 09:00', tz='GMT') & datetime < as.POSIXct('2016-08-11 15:00', tz='GMT'))), aes(x=datetime, y=temp_C, color=loggernum)) +
  geom_point() +
  labs(title='Panther Pond L0, August 2016 download', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  final_theme
#ggsave('figures/L0/2016/20160811 L0 download.tiff', width=4.5, height = 4, dpi=200)

ix=which(PAN_L1$datetime>=as.POSIXct('2016-08-11 11:50', tz='GMT') & PAN_L1$datetime<=as.POSIXct('2016-08-11 12:20', tz='GMT'))
for(i in c('temp_C', 'intens_lux')) {PAN_L1[ix,i]=NA}

#download
ggplot(subset(PAN_L1, subset=(datetime>=as.POSIXct('2016-08-11 09:00', tz='GMT') & datetime < as.POSIXct('2016-08-11 15:00', tz='GMT'))), aes(x=datetime, y=temp_C, color=loggernum)) +
  geom_point() +
  labs(title='Panther Pond L1, August 2016 download', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  final_theme
#ggsave('figures/L1/2016/20160811 L1 download.tiff', width=4.5, height = 4, dpi=200)

#lower sensors issue
ggplot(subset(PAN_L1, subset=(datetime>=as.POSIXct('2016-08-09', tz='GMT') & datetime < as.POSIXct('2016-09-02', tz='GMT'))), aes(x=datetime, y=temp_C, color=loggernum)) +
  geom_point() +
  labs(title='Panther Pond L0, August 2016 lower sensors issue', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  final_theme
#ggsave('figures/L0/2016/201608 L0 bottom sensors.tiff', width=4.5, height = 4, dpi=200)

ggplot(subset(PAN_L1, subset=(datetime>=as.POSIXct('2016-08-28 12:00', tz='GMT') & datetime < as.POSIXct('2016-08-29 12:00', tz='GMT'))), aes(x=datetime, y=temp_C, color=loggernum)) +
  geom_point() +
  labs(title='Panther Pond L0, August 2016 lower sensors issue', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  final_theme
#ggsave('figures/L0/2016/201608 L0 bottom sensors end zoom.tiff', width=4.5, height = 4, dpi=200)


ix=which(PAN_L1$datetime>=as.POSIXct('2016-08-11 12:30', tz='GMT') & PAN_L1$datetime<=as.POSIXct('2016-08-28 23:40', tz='GMT') & 
           (PAN_L1$loggernum==10796878 | PAN_L1$loggernum==10796877 | PAN_L1$loggernum==10616545 | PAN_L1$loggernum==10616541))
for(i in c('temp_C', 'intens_lux')) {PAN_L1[ix,i]=NA}

ggplot(subset(PAN_L1, subset=(datetime>=as.POSIXct('2016-08-09', tz='GMT') & datetime < as.POSIXct('2016-09-02', tz='GMT'))), aes(x=datetime, y=temp_C, color=loggernum)) +
  geom_point() +
  labs(title='Panther Pond L1, August 2016 lower sensors issue', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  final_theme
#ggsave('figures/L1/2016/201608 L1 bottom sensors.tiff', width=4.5, height = 4, dpi=200)

ggplot(subset(PAN_L1, subset=(datetime>=as.POSIXct('2016-08-01', tz='GMT') &datetime < as.POSIXct('2016-09-01', tz='GMT'))), aes(x=datetime, y=temp_C, color=loggernum)) +
  geom_point() +
  labs(title='Panther Pond L1, August 2016', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  final_theme
# ggsave('figures/L1/2016/201608 L1.tiff', width=4.5, height = 4, dpi=200)



####September 2016####
#September 2016
ggplot(subset(PAN_L0, subset=(datetime>=as.POSIXct('2016-09-01', tz='GMT') &datetime < as.POSIXct('2016-10-01', tz='GMT'))), aes(x=datetime, y=temp_C, color=loggernum)) +
  geom_point() +
  labs(title='Panther Pond L0, September 2016', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  final_theme
# ggsave('figures/L0/2016/201609 L0.tiff', width=4.5, height = 4, dpi=200)

#download
ggplot(subset(PAN_L0, subset=(datetime>=as.POSIXct('2016-09-12 08:00', tz='GMT') & datetime < as.POSIXct('2016-09-12 12:00', tz='GMT'))), aes(x=datetime, y=temp_C, color=loggernum)) +
  geom_point() +
  labs(title='Panther Pond L0, September 2016 download', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  final_theme
#ggsave('figures/L0/2016/20160912 L0 download.tiff', width=4.5, height = 4, dpi=200)

#redeployment
ggplot(subset(PAN_L0, subset=(datetime>=as.POSIXct('2016-09-14 08:00', tz='GMT') & datetime < as.POSIXct('2016-09-14 16:00', tz='GMT'))), aes(x=datetime, y=temp_C, color=loggernum)) +
  geom_point() +
  labs(title='Panther Pond L0, September 2016 redeployment', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  final_theme
#ggsave('figures/L0/2016/20160914 L0 redeployment.tiff', width=4.5, height = 4, dpi=200)


ix=which(PAN_L1$datetime>=as.POSIXct('2016-09-12 9:30', tz='GMT') & PAN_L1$datetime<=as.POSIXct('2016-09-14 11:30', tz='GMT'))
for(i in c('temp_C', 'intens_lux')) {PAN_L1[ix,i]=NA}

#download
ggplot(subset(PAN_L1, subset=(datetime>=as.POSIXct('2016-09-12 08:00', tz='GMT') & datetime < as.POSIXct('2016-09-12 12:00', tz='GMT'))), aes(x=datetime, y=temp_C, color=loggernum)) +
  geom_point() +
  labs(title='Panther Pond L1, September 2016 download', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  final_theme
#ggsave('figures/L1/2016/20160912 L1 download.tiff', width=4.5, height = 4, dpi=200)

#redeployment
ggplot(subset(PAN_L1, subset=(datetime>=as.POSIXct('2016-09-14 08:00', tz='GMT') & datetime < as.POSIXct('2016-09-14 16:00', tz='GMT'))), aes(x=datetime, y=temp_C, color=loggernum)) +
  geom_point() +
  labs(title='Panther Pond L1, September 2016 redeployment', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  final_theme
#ggsave('figures/L1/2016/20160914 L1 redeployment.tiff', width=4.5, height = 4, dpi=200)

#want to look at the bigger picture - I think that a few of the sensor positions were changed
ggplot(subset(PAN_L1, subset=(datetime>=as.POSIXct('2016-09-10', tz='GMT') & datetime < as.POSIXct('2016-09-17', tz='GMT'))), aes(x=datetime, y=temp_C, color=loggernum)) +
  geom_point() +
  labs(title='Panther Pond L0, September 2016 data gap', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  final_theme
#ggsave('figures/L1/2016/201609 L1 sensor switch.tiff', width=4.5, height = 4, dpi=200)
#NOTES: I think a 45 is in place of 41, 44 in place of 45 and 41 came online in place of 45  (previously offline)

ix=which(PAN_L1$datetime>=as.POSIXlt('2016-09-12 12:10', tz='GMT') & PAN_L1$loggernum==10616544)
PAN_L1$depth_m[ix]=8
ix=which(PAN_L1$datetime>=as.POSIXlt('2016-09-12 12:10', tz='GMT') & PAN_L1$loggernum==10616545)
PAN_L1$depth_m[ix]=10
ix=which(PAN_L1$datetime>=as.POSIXlt('2016-09-12 12:10', tz='GMT') & PAN_L1$loggernum==10616541)
PAN_L1$depth_m[ix]=6

#remove 78 during 2016-09-12 download
ix = which(PAN_L1$download=='2016-09-12' & PAN_L1$loggernum==10796878)
for(i in c('temp_C', 'intens_lux')) {PAN_L1[ix,i]=NA}

#September 2016
ggplot(subset(PAN_L1, subset=(datetime>=as.POSIXct('2016-09-01', tz='GMT') &datetime < as.POSIXct('2016-10-01', tz='GMT'))), aes(x=datetime, y=temp_C, color=loggernum)) +
  geom_point() +
  labs(title='Panther Pond L1, September 2016', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  final_theme
ggplot(subset(PAN_L1, subset=(datetime>=as.POSIXct('2016-09-01', tz='GMT') &datetime < as.POSIXct('2016-10-01', tz='GMT'))), aes(x=datetime, y=temp_C, color=as.factor(depth_m))) +
  geom_point() +
  labs(title='Panther Pond L1, September 2016', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  final_theme
# ggsave('figures/L1/2016/201609 L1.tiff', width=4.5, height = 4, dpi=200)



####October 2016####
#October 2016
ggplot(subset(PAN_L0, subset=(datetime>=as.POSIXct('2016-10-01', tz='GMT') &datetime < as.POSIXct('2016-11-01', tz='GMT'))), aes(x=datetime, y=temp_C, color=loggernum)) +
  geom_point() +
  labs(title='Panther Pond L0, October 2016', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  final_theme
# ggsave('figures/L0/2016/201610 L0.tiff', width=4.5, height = 4, dpi=200)

#buoy removal
ggplot(subset(PAN_L0, subset=(datetime>=as.POSIXct('2016-10-07 12:00', tz='GMT') & datetime < as.POSIXct('2016-10-08 12:00', tz='GMT'))), aes(x=datetime, y=temp_C, color=loggernum)) +
  geom_point() +
  labs(title='Panther Pond L0, October 2016 removal', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  final_theme
#ggsave('figures/L0/2016/20161007 L0 removal.tiff', width=4.5, height = 4, dpi=200)

ix=which(PAN_L1$datetime>=as.POSIXct('2016-10-07 16:20', tz='GMT'))
for(i in c('temp_C', 'intens_lux')) {PAN_L1[ix,i]=NA}

#buoy removal
ggplot(subset(PAN_L1, subset=(datetime>=as.POSIXct('2016-10-07 12:00', tz='GMT') & datetime < as.POSIXct('2016-10-08 12:00', tz='GMT'))), aes(x=datetime, y=temp_C, color=loggernum)) +
  geom_point() +
  labs(title='Panther Pond L1, October 2016 removal', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  final_theme
#ggsave('figures/L1/2016/20161007 L1 removal.tiff', width=4.5, height = 4, dpi=200)

#October 2016
ggplot(subset(PAN_L1, subset=(datetime>=as.POSIXct('2016-10-01', tz='GMT') &datetime < as.POSIXct('2016-11-01', tz='GMT'))), aes(x=datetime, y=temp_C, color=loggernum)) +
  geom_point() +
  labs(title='Panther Pond L1, October 2016', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  final_theme
# ggsave('figures/L1/2016/201610 L1.tiff', width=4.5, height = 4, dpi=200)


####2016####
ggplot(PAN_L1, aes(x=datetime, y=temp_C, color=loggernum)) +
  geom_point() +
  labs(title='Panther Pond L1 2016', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  final_theme
# ggsave('figures/L1/2016/2016 L1.tiff', width=4.5, height = 4, dpi=200)

ggplot(PAN_L1, aes(x=datetime, y=temp_C, color=as.factor(depth_m))) +
  geom_point() +
  labs(title='Panther Pond L1 2016', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  final_theme
# ggsave('figures/L1/2016/2016 L1 depth.tiff', width=4.5, height = 4, dpi=200)

PAN_L1 %>% 
  select(datetime, depth_m, temp_C, intens_lux, loggernum, fileorig, download) %>% 
  mutate(datetime = as.character(datetime)) %>% 
  write_csv(., 'G:/My Drive/Panther Pond/r export/datasets/L1/2016 Panther L1 buoy data 30July2018.csv')


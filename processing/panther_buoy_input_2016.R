#*                  Bates College - Ewing Lab                        *
#*                                                                   *
#* TITLE:   panther_buoy_input_2016                                  *
#* AUTHOR:  B. Steele                                                *
#* DATE:    19Jul2018                                                *
#* PROJECT: Panther Pond datalogger data                             *
#* PURPOSE: collate raw data files downloaded from all data          *
#*          loggers. display data from downloads                     *

#v 2022-01-06 BGS: updating workflow to draw data files from dropbox and scripts in GH.

# SET UP WORKSPACE ----

# point to working directories
datadir = 'C:/Users/steeleb/Dropbox/PantherBuoy/data/raw/2016/'
rawdir = 'C:/Users/steeleb/Dropbox/PantherBuoy/data/L0/'
rawfigdir = 'C:/Users/steeleb/Dropbox/PantherBuoy/r-export/figures/L0/2016/'
procfigdir = 'C:/Users/steeleb/Dropbox/PantherBuoy/r-export/figures/L1/2016/'
procdir = 'C:/Users/steeleb/Dropbox/PantherBuoy/data/L1/'
annualfigdir = 'C:/Users/steeleb/Dropbox/PantherBuoy/data/L1/annual_plots/'

# load libraries
library(tidyverse) 
library(ggthemes)

#save final theme for ggplot
final_theme=theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        plot.title=element_text(size=16, face='bold', hjust=0.5)) #save as a grom

colorpal = scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                                       "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) 

#read in logger/depth info
logger_depth <- read.csv(file.path(datadir, 'sensor number depth.csv'),
         col.names = c('logger', 'sensorDepth_m'))


# COLLATE DATA ----
dirlist <- list.dirs(datadir, recursive = F) #get directory list of files
dirlist <- dirlist[!grepl('archive', dirlist)] #remove the archive folder

for(i in 1:length(dirlist)){ 
  files = list.files(dirlist[i]) #get list of files in [i] directory
  files = files[!grepl('hobo', files)] #remove the hobo folder
  
  for(j in 1:length(files)){
    df = read.csv(file.path(dirlist[i], files[j]),
                  skip = 1)
    #get TZ
    df$tz = substr(colnames(df)[2], 12, 21)
    
    #harmonize cols
    colnames(df)[1] = 'obsno' #these are always the same
    colnames(df)[2] = 'datetime'
    #find/replace
    cn = colnames(df)
    colnames(df)[grep('temp', cn, ignore.case = T)] = 'waterTemperature_degC'
    colnames(df)[grep('intensity', cn, ignore.case = T)] = 'luminousFlux_lux'
    colnames(df)[grep('batt', cn, ignore.case = T)] = 'batteryVoltage_v'
    colnames(df)[grep('attach', cn, ignore.case = T)] = 'coupAttach'
    colnames(df)[grep('detach', cn, ignore.case = T)] = 'coupDetach'
    colnames(df)[grep('connect', cn, ignore.case = T)] = 'coupConnect'
    colnames(df)[grep('end', cn, ignore.case = T)] = 'EOF'
    colnames(df)[grep('stop', cn, ignore.case = T)] = 'coupStop'

    #add metadata
    df$source_file = files[j]
    df$logger = strsplit(gsub("[^[:alnum:] ]", "", read.table(file.path(dirlist[i], files[j]), nrows=1)[1,3]), " +")[[1]]
    
    #format time
    if(df$tz[1] == 'GMT.04.00'){
      timez = 'Etc/GMT+4'
    } else if (df$tz[1] == 'GMT.05.00'){
      timez = 'Etc/GMT+5'
    } else {
      stop('Did not recognize timezone in file ', files[j], '. Fix in code.')
    }
    df$datetime = as.POSIXct(df$datetime, tz = timez, format = '%m/%d/%y %I:%M:%S %p')
    if(j == 1){
      logger_raw <- df
    } else {
      logger_raw <- full_join(logger_raw, df)
    }
  }
  
  if(i ==1){
    PAN_L0 <- logger_raw
  } else {
    PAN_L0 <- full_join(PAN_L0, logger_raw)
  }
}


## Save L0 dataset ----
PAN_L0 <- PAN_L0 %>% 
  mutate(logger = as.numeric(logger)) %>% 
  left_join(., logger_depth) %>% 
    mutate(sensorDepth_m = factor(sensorDepth_m, levels = c('0.5', '1', '2', '4', '6', '8', '10', '15', '20')))

ggplot(PAN_L0, aes(x=datetime, y=waterTemperature_degC, color=sensorDepth_m)) +
  geom_point() + 
  labs(title='Panther Pond Buoy Data, Level 0', 
       x='date', 
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                            "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  final_theme

write.csv(PAN_L0, file.path(rawdir,'PantherBuoy_L0_2016.csv'), row.names = F)

# VIEW AND CLEAN DATA ----
PAN_L1 <- PAN_L0

# save monthly graphs
range(PAN_L0$datetime)

monthlist = c('05', '06', '07', '08', '09', '10')

#add month for filter to the vertically-oriented data frame
PAN_L1 <- PAN_L1 %>% 
  mutate(month = as.character(format(datetime, '%m')))

#iterate over months creating and saving figures to 'rawfigdir'
for (i in 1: length(monthlist)){
  df = PAN_L1 %>% 
    filter(month == monthlist[i])
  plot <- ggplot(df, aes(x=datetime, y=waterTemperature_degC, color=sensorDepth_m)) +
    geom_point() +
    labs(title = paste0('2016-', monthlist[i], ' temp data - raw'), 
         y='temp (deg C)') +
    final_theme +
    scale_x_datetime(date_minor_breaks = '1 day') +
    scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3",
                                "#00664b", "#00e639", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc"))
  print(plot)
  filename = paste0('2016-', monthlist[i], '_L0_temp.png')
  ggsave(file.path(rawfigdir, filename), device = 'png')
}


####May 2016####
ggplot(subset(PAN_L0, subset=(datetime>=as.POSIXct('2016-05-07', tz=timez) & 
                                datetime < as.POSIXct('2016-05-08', tz=timez))), 
       aes(x=datetime, y=waterTemperature_degC, color=sensorDepth_m)) +
  geom_point() +
  labs(title='Panther Pond L0, May 2016 Deployment', 
       x='date',
       y='temperature (degrees C)') +
  colorpal +  
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme
deployment= as.POSIXct('2016-05-07 11:50', tz = timez)

ix=which(PAN_L1$datetime<deployment)
for(i in c('waterTemperature_degC', 'luminousFlux_lux')) {PAN_L1[ix,i]=NA}

ggplot(subset(PAN_L1, subset=(datetime>=as.POSIXct('2016-05-07', tz=timez) & 
                                datetime < as.POSIXct('2016-05-07 15:00', tz=timez))), 
       aes(x=datetime, y=waterTemperature_degC, color = sensorDepth_m)) +
  geom_point() +
  labs(title='Panther Pond L1, May 2016 Deployment', 
       x='date',
       y='temperature (degrees C)') +
  colorpal +   
  final_theme

ggplot(subset(PAN_L1, subset=(datetime>=as.POSIXct('2016-05-01', tz=timez) &
                                datetime < as.POSIXct('2016-06-01', tz=timez))), 
       aes(x=datetime, y=waterTemperature_degC, color = sensorDepth_m)) +
  geom_point() +
  labs(title='Panther Pond L1, May 2016', 
       x='date',
       y='temperature (degrees C)') +
  colorpal +   
  final_theme

#May 28
ggplot(subset(PAN_L0, subset=(datetime>=as.POSIXct('2016-05-28', tz=timez) & datetime < as.POSIXct('2016-05-29', tz=timez))), aes(x=datetime, y=waterTemperature_degC, color = sensorDepth_m)) +
  geom_point() +
  labs(title='Panther Pond L0, May 2016 Visit', 
       x='date',
       y='temperature (degrees C)') +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  colorpal +   
  final_theme

ix=which(PAN_L1$datetime>as.POSIXct('2016-05-28 14:40', tz=timez) & PAN_L1$datetime<as.POSIXct('2016-05-28 15:10', tz=timez))
for(i in c('waterTemperature_degC', 'luminousFlux_lux')) {PAN_L1[ix,i]=NA}

ggplot(subset(PAN_L1, subset=(datetime>=as.POSIXct('2016-05-28 12:00', tz=timez) & datetime < as.POSIXct('2016-05-28 18:00', tz=timez))), aes(x=datetime, y=waterTemperature_degC, color = sensorDepth_m)) +
  geom_point() +
  labs(title='Panther Pond L1, May 2016 Visit', 
       x='date',
       y='temperature (degrees C)') +
  colorpal+   
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme

ggplot(subset(PAN_L1, subset=(datetime>=as.POSIXct('2016-05-01', tz=timez) &datetime < as.POSIXct('2016-06-01', tz=timez))), aes(x=datetime, y=waterTemperature_degC, color = sensorDepth_m)) +
  geom_point() +
  labs(title='Panther Pond L1, May 2016', 
       x='date',
       y='temperature (degrees C)') +
  colorpal+   
  final_theme

####June 2016####
#June 5
ggplot(subset(PAN_L0, subset=(datetime>=as.POSIXct('2016-06-05 9:00', tz=timez) & datetime < as.POSIXct('2016-06-05 12:00', tz=timez))), aes(x=datetime, y=waterTemperature_degC, color = sensorDepth_m)) +
  geom_point() +
  labs(title='Panther Pond L0, June 2016 download 1', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
                              "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  final_theme

ix=which(PAN_L1$datetime>=as.POSIXct('2016-06-05 10:20', tz=timez) & PAN_L1$datetime<=as.POSIXct('2016-06-05 10:40', tz=timez))
for(i in c('waterTemperature_degC', 'luminousFlux_lux')) {PAN_L1[ix,i]=NA}

ggplot(subset(PAN_L1, subset=(datetime>=as.POSIXct('2016-06-05 9:00', tz=timez) & datetime < as.POSIXct('2016-06-05 12:00', tz=timez))), aes(x=datetime, y=waterTemperature_degC, color = sensorDepth_m)) +
  geom_point() +
  labs(title='Panther Pond L1, June 2016 download 1', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
                              "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  final_theme

#June 12
ggplot(subset(PAN_L0, subset=(datetime>=as.POSIXct('2016-06-12 8:00', tz=timez) & datetime < as.POSIXct('2016-06-12 15:00', tz=timez))), aes(x=datetime, y=waterTemperature_degC, color = sensorDepth_m)) +
  geom_point() +
  labs(title='Panther Pond L0, June 2016 download 2', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
                              "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  final_theme

#June 16
ggplot(subset(PAN_L0, subset=(datetime>=as.POSIXct('2016-06-16 8:00', tz=timez) & datetime < as.POSIXct('2016-06-16 12:00', tz=timez))), aes(x=datetime, y=waterTemperature_degC, color = sensorDepth_m)) +
  geom_point() +
  labs(title='Panther Pond L0, June 2016 redeploy', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
                              "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  final_theme

ix=which(PAN_L1$datetime>=as.POSIXct('2016-06-12 09:10', tz=timez) & PAN_L1$datetime<=as.POSIXct('2016-06-16 10:00', tz=timez))
for(i in c('waterTemperature_degC', 'luminousFlux_lux')) {PAN_L1[ix,i]=NA}

#June 12
ggplot(subset(PAN_L1, subset=(datetime>=as.POSIXct('2016-06-12 8:00', tz=timez) & datetime < as.POSIXct('2016-06-12 15:00', tz=timez))), aes(x=datetime, y=waterTemperature_degC, color = sensorDepth_m)) +
  geom_point() +
  labs(title='Panther Pond L1, June 2016 download 2', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
                              "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  final_theme

#June 16
ggplot(subset(PAN_L1, subset=(datetime>=as.POSIXct('2016-06-16 8:00', tz=timez) & datetime < as.POSIXct('2016-06-16 12:00', tz=timez))), aes(x=datetime, y=waterTemperature_degC, color = sensorDepth_m)) +
  geom_point() +
  labs(title='Panther Pond L1, June 2016 redeploy', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
                              "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  final_theme

#compare 6/5-12 to 6/16-20 for the top 3 loggers
ggplot(subset(PAN_L1, subset=(datetime>=as.POSIXct('2016-06-05', tz=timez) & datetime < as.POSIXct('2016-06-12', tz=timez))), aes(x=datetime, y=waterTemperature_degC, color = sensorDepth_m)) +
  geom_point() +
  labs(title='Panther Pond L1, June 2016 5-11', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
                              "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  coord_cartesian(ylim=c(16,24)) +
  final_theme

ggplot(subset(PAN_L1, subset=(datetime>=as.POSIXct('2016-06-16', tz=timez) & datetime < as.POSIXct('2016-06-22', tz=timez))), aes(x=datetime, y=waterTemperature_degC, color = sensorDepth_m)) +
  geom_point() +
  labs(title='Panther Pond L1, June 2016 16-22', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
                              "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  coord_cartesian(ylim=c(16,24)) +
  final_theme
#looks fine upon larger timeframe

#look at 0616 redeploy
ggplot(subset(PAN_L1, subset=(datetime>=as.POSIXct('2016-06-16', tz=timez) & datetime < as.POSIXct('2016-06-23', tz=timez))), aes(x=datetime, y=waterTemperature_degC, color = sensorDepth_m)) +
  geom_point() +
  labs(title='Panther Pond L1, June 2016 16-22', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
                              "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  final_theme

#look at 0619 
ggplot(subset(PAN_L1, subset=(datetime>=as.POSIXct('2016-06-19', tz=timez) & datetime < as.POSIXct('2016-06-20', tz=timez))), aes(x=datetime, y=waterTemperature_degC, color = sensorDepth_m)) +
  geom_point() +
  labs(title='Panther Pond L1, June 2016 19', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
                              "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  final_theme

PAN_L1 <- PAN_L1 %>% 
  mutate_at(vars(waterTemperature_degC, luminousFlux_lux),
            ~(case_when(datetime>=as.POSIXct('2016-06-16 10:00', tz=timez) & 
                             datetime<=as.POSIXct('2016-06-19 18:00', tz=timez) & 
                          (logger==10796877 | logger==10796878) ~ NA_real_,
                           TRUE ~ .)))

#look at 0616 redeploy
ggplot(subset(PAN_L1, subset=(datetime>=as.POSIXct('2016-06-16', tz=timez) & datetime < as.POSIXct('2016-06-23', tz=timez))), aes(x=datetime, y=waterTemperature_degC, color = sensorDepth_m)) +
  geom_point() +
  labs(title='Panther Pond L1, June 2016 16-22, v2', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
                              "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  final_theme


#CLEAN MONTH
ggplot(subset(PAN_L1, subset=(datetime>=as.POSIXct('2016-06-01', tz=timez) &datetime < as.POSIXct('2016-07-01', tz=timez))), aes(x=datetime, y=waterTemperature_degC, color = sensorDepth_m)) +
  geom_point() +
  labs(title='Panther Pond L1, June 2016', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  final_theme


####JUly2016####
#download
ggplot(subset(PAN_L0, subset=(datetime>=as.POSIXct('2016-07-11 16:00', tz=timez) & datetime < as.POSIXct('2016-07-11 20:00', tz=timez))), aes(x=datetime, y=waterTemperature_degC, color = sensorDepth_m)) +
  geom_point() +
  labs(title='Panther Pond L0, July 2016 download', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  final_theme

ix=which(PAN_L1$datetime>=as.POSIXct('2016-07-11 18:50', tz=timez) & 
           PAN_L1$datetime<=as.POSIXct('2016-07-11 19:10', tz=timez))
for(i in c('waterTemperature_degC', 'luminousFlux_lux')) {PAN_L1[ix,i]=NA}

#download
ggplot(subset(PAN_L1, subset=(datetime>=as.POSIXct('2016-07-11 16:00', tz=timez) & datetime < as.POSIXct('2016-07-11 20:00', tz=timez))), aes(x=datetime, y=waterTemperature_degC, color = sensorDepth_m)) +
  geom_point() +
  labs(title='Panther Pond L1, July 2016 download', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  final_theme

#bottom sensor 77 issue
ggplot(subset(PAN_L1, subset=(datetime>=as.POSIXct('2016-07-11', tz=timez) & 
                                datetime < as.POSIXct('2016-07-15', tz=timez))), 
       aes(x=datetime, y=waterTemperature_degC, color = as.character(logger))) +
  geom_point() +
  labs(title='Panther Pond L0, July 2016 bottom sensor issue', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  final_theme
#ggsave('figures/L0/2016/20160711-16 L0 bottom 77.tiff', width=4.5, height = 4, dpi=200)

ix=which(PAN_L1$datetime>=as.POSIXct('2016-07-11 19:20', tz=timez) & 
           PAN_L1$datetime<=as.POSIXct('2016-07-14 18:20', tz=timez) & 
           (PAN_L1$logger==10796877 | PAN_L1$logger==10616541))
for(i in c('waterTemperature_degC', 'luminousFlux_lux')) {PAN_L1[ix,i]=NA}

ggplot(subset(PAN_L1, subset=(datetime>=as.POSIXct('2016-07-11', tz=timez) & datetime < as.POSIXct('2016-07-15', tz=timez))), aes(x=datetime, y=waterTemperature_degC, color = sensorDepth_m)) +
  geom_point() +
  labs(title='Panther Pond L1, July 2016 bottom sensor issue', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  final_theme

#bottom sensor 78 issue
ggplot(subset(PAN_L1, subset=(datetime>=as.POSIXct('2016-07-11', tz=timez) & datetime < as.POSIXct('2016-07-17', tz=timez))), aes(x=datetime, y=waterTemperature_degC, color = sensorDepth_m)) +
  geom_point() +
  labs(title='Panther Pond L0, July 2016 bottom sensor issue', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  final_theme

ix=which(PAN_L1$datetime>=as.POSIXct('2016-07-11 19:20', tz=timez) & PAN_L1$datetime<=as.POSIXct('2016-07-15 19:40', tz=timez) & PAN_L1$logger==10796878)
for(i in c('waterTemperature_degC', 'luminousFlux_lux')) {PAN_L1[ix,i]=NA}

ggplot(subset(PAN_L1, subset=(datetime>=as.POSIXct('2016-07-11', tz=timez) & datetime < as.POSIXct('2016-07-17', tz=timez))), aes(x=datetime, y=waterTemperature_degC, color = sensorDepth_m)) +
  geom_point() +
  labs(title='Panther Pond L1, July 2016 bottom sensor issue', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  final_theme

ggplot(subset(PAN_L1, subset=(datetime>=as.POSIXct('2016-07-01', tz=timez) &datetime < as.POSIXct('2016-08-01', tz=timez))), aes(x=datetime, y=waterTemperature_degC, color = sensorDepth_m)) +
  geom_point() +
  labs(title='Panther Pond L1, July 2016', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  final_theme


####August 2016####
#download
ggplot(subset(PAN_L0, subset=(datetime>=as.POSIXct('2016-08-11 09:00', tz=timez) & datetime < as.POSIXct('2016-08-11 15:00', tz=timez))), aes(x=datetime, y=waterTemperature_degC, color = sensorDepth_m)) +
  geom_point() +
  labs(title='Panther Pond L0, August 2016 download', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  final_theme

ix=which(PAN_L1$datetime>=as.POSIXct('2016-08-11 11:50', tz=timez) & PAN_L1$datetime<=as.POSIXct('2016-08-11 12:20', tz=timez))
for(i in c('waterTemperature_degC', 'luminousFlux_lux')) {PAN_L1[ix,i]=NA}

#download
ggplot(subset(PAN_L1, subset=(datetime>=as.POSIXct('2016-08-11 09:00', tz=timez) & datetime < as.POSIXct('2016-08-11 15:00', tz=timez))), aes(x=datetime, y=waterTemperature_degC, color = sensorDepth_m)) +
  geom_point() +
  labs(title='Panther Pond L1, August 2016 download', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  final_theme

#lower sensors issue
ggplot(subset(PAN_L1, subset=(datetime>=as.POSIXct('2016-08-09', tz=timez) & datetime < as.POSIXct('2016-09-02', tz=timez))), aes(x=datetime, y=waterTemperature_degC, color = sensorDepth_m)) +
  geom_point() +
  labs(title='Panther Pond L0, August 2016 lower sensors issue', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  final_theme

ggplot(subset(PAN_L1, subset=(datetime>=as.POSIXct('2016-08-28 12:00', tz=timez) & datetime < as.POSIXct('2016-08-29 12:00', tz=timez))), aes(x=datetime, y=waterTemperature_degC, color = sensorDepth_m)) +
  geom_point() +
  labs(title='Panther Pond L0, August 2016 lower sensors issue', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  final_theme

ix=which(PAN_L1$datetime>=as.POSIXct('2016-08-11 12:30', tz=timez) & PAN_L1$datetime<=as.POSIXct('2016-08-28 23:40', tz=timez) & 
           (PAN_L1$logger==10796878 | PAN_L1$logger==10796877 | PAN_L1$logger==10616545 | PAN_L1$logger==10616541))
for(i in c('waterTemperature_degC', 'luminousFlux_lux')) {PAN_L1[ix,i]=NA}

ggplot(subset(PAN_L1, subset=(datetime>=as.POSIXct('2016-08-09', tz=timez) & datetime < as.POSIXct('2016-09-02', tz=timez))), aes(x=datetime, y=waterTemperature_degC, color = sensorDepth_m)) +
  geom_point() +
  labs(title='Panther Pond L1, August 2016 lower sensors issue', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  final_theme

ggplot(subset(PAN_L1, subset=(datetime>=as.POSIXct('2016-08-01', tz=timez) &datetime < as.POSIXct('2016-09-01', tz=timez))), aes(x=datetime, y=waterTemperature_degC, color = sensorDepth_m)) +
  geom_point() +
  labs(title='Panther Pond L1, August 2016', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  final_theme

####September 2016####
#download
ggplot(subset(PAN_L0, subset=(datetime>=as.POSIXct('2016-09-12 08:00', tz=timez) & datetime < as.POSIXct('2016-09-12 12:00', tz=timez))), aes(x=datetime, y=waterTemperature_degC, color = sensorDepth_m)) +
  geom_point() +
  labs(title='Panther Pond L0, September 2016 download', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  final_theme

#redeployment
ggplot(subset(PAN_L0, subset=(datetime>=as.POSIXct('2016-09-14 08:00', tz=timez) & datetime < as.POSIXct('2016-09-14 16:00', tz=timez))), aes(x=datetime, y=waterTemperature_degC, color = sensorDepth_m)) +
  geom_point() +
  labs(title='Panther Pond L0, September 2016 redeployment', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  final_theme

ix=which(PAN_L1$datetime>=as.POSIXct('2016-09-12 9:30', tz=timez) & PAN_L1$datetime<=as.POSIXct('2016-09-14 11:30', tz=timez))
for(i in c('waterTemperature_degC', 'luminousFlux_lux')) {PAN_L1[ix,i]=NA}

#download
ggplot(subset(PAN_L1, subset=(datetime>=as.POSIXct('2016-09-12 08:00', tz=timez) & datetime < as.POSIXct('2016-09-12 12:00', tz=timez))), aes(x=datetime, y=waterTemperature_degC, color = sensorDepth_m)) +
  geom_point() +
  labs(title='Panther Pond L1, September 2016 download', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  final_theme

#redeployment
ggplot(subset(PAN_L1, subset=(datetime>=as.POSIXct('2016-09-14 08:00', tz=timez) & datetime < as.POSIXct('2016-09-14 16:00', tz=timez))), aes(x=datetime, y=waterTemperature_degC, color = sensorDepth_m)) +
  geom_point() +
  labs(title='Panther Pond L1, September 2016 redeployment', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  final_theme

#want to look at the bigger picture - I think that a few of the sensor positions were changed
ggplot(subset(PAN_L1, subset=(datetime>=as.POSIXct('2016-09-10', tz=timez) & datetime < as.POSIXct('2016-09-17', tz=timez))), aes(x=datetime, y=waterTemperature_degC, color = sensorDepth_m)) +
  geom_point() +
  labs(title='Panther Pond L0, September 2016 data gap', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  final_theme
#ggsave('figures/L1/2016/201609 L1 sensor switch.tiff', width=4.5, height = 4, dpi=200)
#NOTES: I think a 45 is in place of 41, 44 in place of 45 and 41 came online in place of 45  (previously offline)

ix=which(PAN_L1$datetime>=as.POSIXlt('2016-09-12 12:10', tz=timez) & PAN_L1$logger=='10616544')
PAN_L1$sensorDepth_m[ix]=8
ix=which(PAN_L1$datetime>=as.POSIXlt('2016-09-12 12:10', tz=timez) & PAN_L1$logger=='10616545')
PAN_L1$sensorDepth_m[ix]=10
ix=which(PAN_L1$datetime>=as.POSIXlt('2016-09-12 12:10', tz=timez) & PAN_L1$logger=='10616541')
PAN_L1$sensorDepth_m[ix]=6

ggplot(subset(PAN_L1, subset=(datetime>=as.POSIXct('2016-09-10', tz=timez) & datetime < as.POSIXct('2016-09-17', tz=timez))), 
       aes(x=datetime, y=waterTemperature_degC, color = as.character(logger))) +
  geom_point() +
  labs(title='Panther Pond L0, September 2016 data gap', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  final_theme

#remove 78 during 2016-09-12 download this is too high comared to surrounding data
ix = which(PAN_L1$datetime >= as.Date('2016-08-28') & PAN_L1$datetime <= as.Date('2016-09-13') & PAN_L1$logger==10796878)
for(i in c('waterTemperature_degC', 'luminousFlux_lux')) {PAN_L1[ix,i]=NA}

#September 2016
ggplot(subset(PAN_L1, subset=(datetime>=as.POSIXct('2016-09-01', tz=timez) &datetime < as.POSIXct('2016-10-01', tz=timez))), aes(x=datetime, y=waterTemperature_degC, color = sensorDepth_m)) +
  geom_point() +
  labs(title='Panther Pond L1, September 2016', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  final_theme

####October 2016####
#buoy removal
ggplot(subset(PAN_L0, subset=(datetime>=as.POSIXct('2016-10-07 12:00', tz=timez) & datetime < as.POSIXct('2016-10-08 12:00', tz=timez))), aes(x=datetime, y=waterTemperature_degC, color = sensorDepth_m)) +
  geom_point() +
  labs(title='Panther Pond L0, October 2016 removal', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  final_theme

ix=which(PAN_L1$datetime>=as.POSIXct('2016-10-07 16:20', tz=timez))
for(i in c('waterTemperature_degC', 'luminousFlux_lux')) {PAN_L1[ix,i]=NA}

#buoy removal
ggplot(subset(PAN_L1, subset=(datetime>=as.POSIXct('2016-10-07 12:00', tz=timez) & datetime < as.POSIXct('2016-10-08 12:00', tz=timez))), aes(x=datetime, y=waterTemperature_degC, color = sensorDepth_m)) +
  geom_point() +
  labs(title='Panther Pond L1, October 2016 removal', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  final_theme

#October 2016
ggplot(subset(PAN_L1, subset=(datetime>=as.POSIXct('2016-10-01', tz=timez) &datetime < as.POSIXct('2016-11-01', tz=timez))), aes(x=datetime, y=waterTemperature_degC, color = sensorDepth_m)) +
  geom_point() +
  labs(title='Panther Pond L1, October 2016', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  final_theme

#### print monthly clean plots ####

for (i in 1: length(monthlist)){
  df = PAN_L1 %>% 
    filter(month == monthlist[i])
  plot <- ggplot(df, aes(x=datetime, y=waterTemperature_degC, color=sensorDepth_m)) +
    geom_point() +
    labs(title = paste0('2016-', monthlist[i], ' temp data - clean'), 
         y='temp (deg C)') +
    final_theme +
    scale_x_datetime(date_minor_breaks = '1 day') +
    scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3",
                                "#00664b", "#00e639", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc"))
  print(plot)
  filename = paste0('2016-', monthlist[i], '_L1_temp.png')
  ggsave(file.path(procfigdir, filename), device = 'png')
}

####2016####
ggplot(PAN_L1, aes(x=datetime, y=waterTemperature_degC, color = sensorDepth_m)) +
  geom_point() +
  labs(title='Panther Pond L1 2016', 
       x='date',
       y='temperature (degrees C)') +
  scale_x_datetime(date_minor_breaks = '1 month') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  final_theme
ggsave(file.path(annualfigdir, '2016_Temperature.png'))

colnames(PAN_L1)

PAN_L1 %>% 
  select(datetime, tz, sensorDepth_m, waterTemperature_degC, luminousFlux_lux, logger, source_file) %>% 
  mutate(datetime = as.character(datetime)) %>% 
  write.csv(., file.path(procdir, paste0('PantherBuoy_L1_2016_v', Sys.Date(), '.csv')), row.names = F)


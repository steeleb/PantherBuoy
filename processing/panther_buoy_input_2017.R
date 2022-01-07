#*                  Bates College - Ewing Lab                        *
#*                                                                   *
#* TITLE:   panther_buoy_input_2017                                  *
#* AUTHOR:  B. Steele                                                *
#* PROJECT: Panther Pond datalogger data                             *
#* PURPOSE: collate raw data files downloaded from all data          *
#*          loggers. display data from downloads                     *

#v 2022-01-06 BGS: updated DO data to have saturation values. updating workflow to draw data files from dropbox and scripts in GH.
 
# SET UP WORKSPACE ----

library(tidyverse) 
library(ggthemes)

#save final theme for ggplot
final_theme=theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        plot.title=element_text(size=16, face='bold', hjust=0.5)) #save as a grom

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

# point to working directories
datadir = 'C:/Users/steeleb/Dropbox/PantherBuoy/data/raw/2017/'
rawdir = 'C:/Users/steeleb/Dropbox/PantherBuoy/data/L0/'
rawfigdir = 'C:/Users/steeleb/Dropbox/PantherBuoy/r-export/figures/L0/2017/'
procfigdir = 'C:/Users/steeleb/Dropbox/PantherBuoy/r-export/figures/L1/2017/'
procdir = 'C:/Users/steeleb/Dropbox/PantherBuoy/data/L1/'
annualfigdir = 'C:/Users/steeleb/Dropbox/PantherBuoy/data/L1/annual_plots/'


# variable lists
do = c('waterTemperature_degC', 'oxygenDissolved_mgl', 'adj_oxygenDissolved_mgl', 'oxygenDissolved_percSat')


# TEMP DATA COLLATION ----
files = list.files(file.path(datadir, 'buoy/temp/')) #get list of files in [i] directory
files = files[!grepl('hobo', files)] #remove the hobo folder


for(j in 1:length(files)){
  df = read.csv(file.path(datadir, 'buoy/temp/', files[j]),
                skip = 1)
  #get TZ
  df$tz = substr(colnames(df)[2], 12, 21)
  
  colname_string = strsplit(gsub("\\.", " ",colnames(df)[3]), ' ')
  df$logger = last(colname_string[[1]])
  
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
  df$sensorDepth_m = as.character(read.table(file.path(datadir, 'buoy/temp/', files[j]), nrows=1)[1,3])
  
  #format time
  if(df$tz[1] == 'GMT.04.00'){
    timez = 'Etc/GMT+4'
  } else if (df$tz[1] == 'GMT.05.00'){
    timez = 'Etc/GMT+5'
  } else {
    stop('Did not recognize timezone in file ', files[j], '. Fix in code.')
  }
  df$datetime = as.POSIXct(df$datetime, tz = timez, format = '%m/%d/%y %I:%M:%S %p')
  print(files[j])
  if(j == 1){
    logger_raw <- df
  } else {
    logger_raw <- full_join(logger_raw, df)
  }
}

temp_L0 <- logger_raw %>% 
  mutate(sensorDepth_m = case_when(sensorDepth_m == 'Surface' ~ '0.1', 
                                   TRUE ~ sensorDepth_m)) 

## Save L0 dataset ----
temp_L0 <- temp_L0 %>% 
  mutate(sensorDepth_m = factor(sensorDepth_m, levels = c('0.1', '0.5', '1', '2', '4', '6', '8', '10', '15', '20')))

ggplot(PAN_L0, aes(x=datetime, y=waterTemperature_degC, color=sensorDepth_m)) +
  geom_point() + 
  labs(title='Panther Pond Buoy Data, Level 0', 
       x='date', 
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  final_theme

write.csv(temp_L0, file.path(rawdir,'PantherBuoy_L0_2016.csv'), row.names = F)

# VIEW AND CLEAN TEMP DATA ----
temp_L1 <- temp_L0

## save monthly graphs ----
range(temp_L0$datetime)

monthlist = c('05', '06', '07', '08', '09', '10')

#add month for filter to the vertically-oriented data frame
temp_L1 <- temp_L1 %>% 
  mutate(month = as.character(format(datetime, '%m')))

# iterate over months creating and saving figures to 'rawfigdir'
for (i in 1: length(monthlist)){
  df = temp_L1 %>% 
    filter(month == monthlist[i])
  plot <- ggplot(df, aes(x=datetime, y=waterTemperature_degC, color=sensorDepth_m)) +
    geom_point() +
    labs(title = paste0('2017-', monthlist[i], ' temp data - raw'), 
         y='temp (deg C)') +
    final_theme +
    scale_x_datetime(date_minor_breaks = '1 day') +
    scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3",
                                "#00664b", "#00e639", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc"))
  print(plot)
  filename = paste0('2017-', monthlist[i], '_L0_temp.png')
  ggsave(file.path(rawfigdir, 'temp/', filename), device = 'png')
}

# MAY 2017 #
ggplot(subset(temp_L1,
              subset=(datetime>= as.POSIXct('2017-05-30', tz=timez) & datetime < as.POSIXct('2017-05-31', tz=timez))),
       aes(x=datetime, y=waterTemperature_degC, color = sensorDepth_m)) +
  geom_point() +
  labs(title='Panther Pond L0 May 30 2017', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme 

temp_L1 <- temp_L1 %>% 
  mutate_at(vars(waterTemperature_degC, luminousFlux_lux),
            ~(case_when(datetime < as.POSIXct('2017-05-30 8:20', tz=timez) ~ NA_real_,
                            TRUE ~ .)))

ggplot(subset(temp_L1,
              subset=(datetime>= as.POSIXct('2017-05-01', tz=timez) & datetime < as.POSIXct('2017-06-01', tz=timez))),
       aes(x=datetime, y=waterTemperature_degC, color = sensorDepth_m)) +
  geom_point() +
  labs(title='Panther Pond L1 May 2017', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme 

ggplot(subset(temp_L1,
              subset=(datetime>= as.POSIXct('2017-06-10', tz=timez) & datetime < as.POSIXct('2017-06-11', tz=timez))),
       aes(x=datetime, y=waterTemperature_degC, color = sensorDepth_m)) +
  geom_point() +
  labs(title='Panther Pond L0 June 10 2017', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme 

temp_L1 <- temp_L1 %>% 
  mutate_at(vars(waterTemperature_degC, luminousFlux_lux),
            ~(case_when(datetime >= as.POSIXct('2017-06-10 13:10', tz=timez) & 
                              datetime < as.POSIXct('2017-06-10 13:50', tz=timez) & 
                              sensorDepth_m == 6 ~ NA_real_,
                            TRUE ~ .)))

ggplot(subset(temp_L1,
              subset=(datetime>= as.POSIXct('2017-06-10', tz=timez) & datetime < as.POSIXct('2017-06-11', tz=timez))),
       aes(x=datetime, y=waterTemperature_degC, color = sensorDepth_m)) +
  geom_point() +
  labs(title='Panther Pond L0 June 10 2017', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme 

temp_L1 <- temp_L1 %>% 
  mutate_at(vars(waterTemperature_degC, luminousFlux_lux),
            ~(case_when(datetime >= as.POSIXct('2017-06-10 19:30', tz=timez) & 
                              datetime < as.POSIXct('2017-06-10 19:50', tz=timez)  ~ NA_real_,
                            TRUE ~ .)))

ggplot(subset(temp_L1,
              subset=(datetime>= as.POSIXct('2017-06-12', tz=timez) & datetime < as.POSIXct('2017-06-13', tz=timez))),
       aes(x=datetime, y=waterTemperature_degC, color = sensorDepth_m)) +
  geom_point() +
  labs(title='Panther Pond L0 June 12 2017', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme 

temp_L1 <- temp_L1 %>% 
  mutate_at(vars(waterTemperature_degC, luminousFlux_lux),
            ~(case_when(datetime >= as.POSIXct('2017-06-12 11:00', tz=timez) & 
                              datetime < as.POSIXct('2017-06-12 11:30', tz=timez)  ~ NA_real_,
                            TRUE ~ .)))

ggplot(subset(temp_L1,
              subset=(datetime>= as.POSIXct('2017-06-01', tz=timez) & datetime < as.POSIXct('2017-07-01', tz=timez))),
       aes(x=datetime, y=waterTemperature_degC, color = sensorDepth_m)) +
  geom_point() +
  labs(title='Panther Pond L1 June 2017', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme 

ggplot(subset(temp_L1,
              subset=(datetime>= as.POSIXct('2017-07-09', tz=timez) & datetime < as.POSIXct('2017-07-10', tz=timez))),
       aes(x=datetime, y=waterTemperature_degC, color = sensorDepth_m)) +
  geom_point() +
  labs(title='Panther Pond L0 Jul 09 2017', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
    scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme 

temp_L1 <- temp_L1 %>% 
  mutate_at(vars(waterTemperature_degC, luminousFlux_lux),
            ~(case_when(datetime >= as.POSIXct('2017-07-09 9:50', tz=timez) & 
                              datetime < as.POSIXct('2017-07-09 14:10', tz=timez)  ~ NA_real_,
                            TRUE ~ .)))

ggplot(subset(temp_L1,
              subset=(datetime>= as.POSIXct('2017-07-14', tz=timez) & datetime < as.POSIXct('2017-07-15', tz=timez))),
       aes(x=datetime, y=waterTemperature_degC, color = sensorDepth_m)) +
  geom_point() +
  labs(title='Panther Pond L0 Jul 14 2017', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme 

temp_L1 <- temp_L1 %>% 
  mutate_at(vars(waterTemperature_degC, luminousFlux_lux),
            ~(case_when(datetime >= as.POSIXct('2017-07-14 10:50', tz=timez) & 
                              datetime < as.POSIXct('2017-07-14 11:20', tz=timez)  ~ NA_real_,
                            TRUE ~ .)))

ggplot(subset(temp_L1,
              subset=(datetime>= as.POSIXct('2017-08-01', tz=timez) & datetime < as.POSIXct('2017-08-02', tz=timez))),
       aes(x=datetime, y=waterTemperature_degC, color = sensorDepth_m)) +
  geom_point() +
  labs(title='Panther Pond L0 Aug 1 2017', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme 

temp_L1 <- temp_L1 %>% 
  mutate_at(vars(waterTemperature_degC, luminousFlux_lux),
            ~(case_when(datetime == as.POSIXct('2017-08-01 7:10', tz=timez) ~ NA_real_,
                            TRUE ~ .)))

ggplot(subset(temp_L1,
              subset=(datetime>= as.POSIXct('2017-08-01', tz=timez) & datetime < as.POSIXct('2017-09-01', tz=timez))),
       aes(x=datetime, y=waterTemperature_degC, color = sensorDepth_m)) +
  geom_point() +
  labs(title='Panther Pond L1 Aug 2017', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme 

ggplot(subset(temp_L1,
              subset=(datetime>= as.POSIXct('2017-09-12', tz=timez) & datetime < as.POSIXct('2017-09-13', tz=timez))),
       aes(x=datetime, y=waterTemperature_degC, color = sensorDepth_m)) +
  geom_point() +
  labs(title='Panther Pond L0 Sept 12 2017', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme 

temp_L1 <- temp_L1 %>% 
  mutate_at(vars(waterTemperature_degC, luminousFlux_lux),
            ~(case_when(datetime >= as.POSIXct('2017-09-12 10:10', tz=timez) &
                              datetime < as.POSIXct('2017-09-12 17:00', tz=timez) ~ NA_real_,
                            TRUE ~ .)))

ggplot(subset(temp_L1,
              subset=(datetime>= as.POSIXct('2017-09-13', tz=timez) & datetime < as.POSIXct('2017-09-14', tz=timez))),
       aes(x=datetime, y=waterTemperature_degC, color = sensorDepth_m)) +
  geom_point() +
  labs(title='Panther Pond L0 Sept 13 2017', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme 

temp_L1 <- temp_L1 %>% 
  mutate(waterTemperature_degC = case_when(datetime >= as.POSIXct('2017-09-13 10:40', tz=timez) &
                              datetime < as.POSIXct('2017-09-13 11:40', tz=timez) ~ NA_real_,
                            TRUE ~ waterTemperature_degC))

ggplot(subset(temp_L1,
              subset=(datetime>= as.POSIXct('2017-09-01', tz=timez) & datetime < as.POSIXct('2017-10-01', tz=timez))),
       aes(x=datetime, y=waterTemperature_degC, color = sensorDepth_m)) +
  geom_point() +
  labs(title='Panther Pond L1 Sept 2017', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme 

ggplot(subset(temp_L1,
              subset=(datetime>= as.POSIXct('2017-10-28', tz=timez) & datetime < as.POSIXct('2017-10-29', tz=timez))),
       aes(x=datetime, y=waterTemperature_degC, color = sensorDepth_m)) +
  geom_point() +
  labs(title='Panther Pond L0 Oct 28 2017', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme 

temp_L1 <- temp_L1 %>% 
  mutate_at(vars(waterTemperature_degC, luminousFlux_lux),
            ~(case_when(datetime >= as.POSIXct('2017-10-28 12:10', tz=timez) ~ NA_real_,
                            TRUE ~ .)))

ggplot(subset(temp_L1,
              subset=(datetime>= as.POSIXct('2017-10-01', tz=timez) & datetime < as.POSIXct('2017-11-01', tz=timez))),
       aes(x=datetime, y=waterTemperature_degC, color = sensorDepth_m)) +
  geom_point() +
  labs(title='Panther Pond L1 Oct 2017', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme 


## print monthly clean plots ####

for (i in 1: length(monthlist)){
  df = temp_L1 %>% 
    filter(month == monthlist[i])
  plot <- ggplot(df, aes(x=datetime, y=waterTemperature_degC, color=sensorDepth_m)) +
    geom_point() +
    labs(title = paste0('2017-', monthlist[i], ' temp data - clean'), 
         y='temp (deg C)') +
    final_theme +
    scale_x_datetime(date_minor_breaks = '1 day') +
    scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3",
                                "#00664b", "#00e639", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc"))
  print(plot)
  filename = paste0('2017-', monthlist[i], '_L1_temp.png')
  ggsave(file.path(procfigdir, 'temp/', filename), device = 'png')
}

# annual fig #

ggplot(temp_L1, aes(x=datetime, y=waterTemperature_degC, color = sensorDepth_m)) +
  geom_point() +
  labs(title='Panther Pond L1 2017', 
       x='date',
       y='temperature (degrees C)') +
  scale_x_datetime(date_minor_breaks = '1 month') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  final_theme
ggsave(file.path(annualfigdir, '2017_Temperature.png'))

## save clean temp file ----
temp_L1 %>%
  select(datetime, tz, sensorDepth_m, waterTemperature_degC, luminousFlux_lux, logger, source_file) %>% 
  mutate(datetime = as.character(datetime)) %>% 
  write.csv(., file.path(procdir, paste0('PantherBuoy_L1_temp_2017_v', Sys.Date(), '.csv')), row.names = F)


# DO DATA COLLATION ----
files = list.files(file.path(datadir, 'buoy/do/')) #get list of files in [i] directory
files = files[!grepl('hobo', files)] #remove the hobo folder


for(j in 1:length(files)){
  df = read.csv(file.path(datadir, 'buoy/do/', files[j]),
                skip = 1)
  #get TZ
  df$tz = substr(colnames(df)[2], 12, 21)
  
  colname_string = strsplit(gsub("\\.", " ",colnames(df)[3]), ' ')
  df$logger = last(colname_string[[1]])
  
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
  colnames(df)[grep('adj', cn, ignore.case = T)] = 'adj_oxygenDissolved_mgl'
  cn = colnames(df) ## need to re run this so that the adjusted DO doesn't get mixed in with the uncorrected.
  colnames(df)[grep('mg.L', cn, ignore.case = T)] = 'oxygenDissolved_mgl'
  colnames(df)[grep('percent', cn, ignore.case = T)] = 'oxygenDissolved_percSat'
  colnames(df)[grep('stop', cn, ignore.case = T)] = 'coupStop'
  
  #add metadata
  df$source_file = files[j]
  df$sensorDepth_m = as.character(read.table(file.path(datadir, 'buoy/do/', files[j]), nrows=1)[1,3])
  
  #format time
  if(df$tz[1] == 'GMT.04.00'){
    timez = 'Etc/GMT+4'
  } else if (df$tz[1] == 'GMT.05.00'){
    timez = 'Etc/GMT+5'
  } else {
    stop('Did not recognize timezone in file ', files[j], '. Fix in code.')
  }
  df$datetime = as.POSIXct(df$datetime, tz = timez, format = '%m/%d/%y %I:%M:%S %p')
  print(files[j])
  if(j == 1){
    logger_raw <- df
  } else {
    logger_raw <- full_join(logger_raw, df)
  }
}

DO_L0 <- logger_raw %>% 
  mutate(sensorDepth_m = case_when(sensorDepth_m == 'Bottom' ~ '19',
                                   sensorDepth_m == 'Surface' ~ '1.5',
                                   TRUE ~ ''))
DO_L0 <- DO_L0 %>% 
  mutate(sensorDepth_m = factor(sensorDepth_m, levels = c('1.5','19')))
colnames(DO_L0)

## Save L0 dataset ----
DO_L0_vert <- DO_L0 %>% 
  select(datetime:oxygenDissolved_percSat, tz:sensorDepth_m) %>% 
  pivot_longer(names_to = 'variable',
               values_to = 'value',
               -c(datetime, tz, logger, source_file, sensorDepth_m))

ggplot(DO_L0_vert, aes(x=datetime, y=value, color=sensorDepth_m)) +
  geom_point() + 
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title='Panther Pond Buoy Data, DO, Level 0', 
       x='date', 
       y='dissolved oxygen') +
  scale_color_colorblind() +   
  final_theme

write.csv(DO_L0, file.path(rawdir,'PantherBuoy_L0_2016.csv'), row.names = F)

# VIEW AND CLEAN DO DATA ####
DO_L1_vert <- DO_L0_vert 
DO_L1 <- DO_L0

#add month for filter to the vertically-oriented data frame
DO_L1_vert <- DO_L1_vert %>% 
  mutate(month = as.character(format(datetime, '%m')))

## save monthly raw plots ----
for (i in 1: length(monthlist)){
  df = DO_L1_vert %>% 
    filter(month == monthlist[i])
  plot <- ggplot(df, aes(x=datetime, y=value, color=sensorDepth_m)) +
    geom_point() + 
    facet_grid(variable ~ ., scales = 'free_y') +
    labs(title = paste0('2017-', monthlist[i], ' do data - raw'), 
         y='dissolved oxygen') +
    final_theme +
    scale_x_datetime(date_minor_breaks = '1 day') +
    scale_color_colorblind()
  print(plot)
  filename = paste0('2017-', monthlist[i], '_L0_do.png')
  ggsave(file.path(rawfigdir, 'do/', filename), device = 'png')
}

#deployment
ggplot(subset(DO_L1_vert,
              subset = (datetime >= as.POSIXct('2017-05-30', tz=timez) & 
                          datetime < as.POSIXct('2017-05-31', tz=timez))),
       aes(x=datetime, y=value, color=sensorDepth_m)) +
  geom_point() + 
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title='Panther Pond DO 2017', 
       x='date',
       y='do (mg/L)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_colorblind()

DO_L1 <- DO_L1 %>% 
  mutate_at(vars(all_of(do)),
            ~(case_when(datetime < as.POSIXct('2017-05-30 9:00', tz=timez) ~ NA_real_,
                           TRUE ~ .)))
DO_L1_vert <- DO_L1 %>% 
  select(datetime:oxygenDissolved_percSat, tz:sensorDepth_m) %>% 
  pivot_longer(names_to = 'variable',
               values_to = 'value',
               -c(datetime, tz, logger, source_file, sensorDepth_m))

#low do is malfunctioning until jun12
ggplot(subset(DO_L1_vert,
              subset = (datetime >= as.POSIXct('2017-06-10', tz=timez) & 
                          datetime < as.POSIXct('2017-06-11', tz=timez))),
       aes(x=datetime, y=value, color=sensorDepth_m)) +
  geom_point() + 
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title='Panther Pond DO 2017', 
       x='date',
       y='do (mg/L)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_colorblind()

DO_L1 <- DO_L1 %>% 
  mutate_at(vars(all_of(do)),
            ~(case_when(datetime >= as.POSIXct('2017-06-10 19:30', tz=timez) &
                          datetime < as.POSIXct('2017-06-10 19:50', tz=timez)~ NA_real_,
                           TRUE ~ .)))

DO_L1_vert <- DO_L1 %>% 
  select(datetime:oxygenDissolved_percSat, tz:sensorDepth_m) %>% 
  pivot_longer(names_to = 'variable',
               values_to = 'value',
               -c(datetime, tz, logger, source_file, sensorDepth_m))

ggplot(subset(DO_L1_vert,
              subset = (datetime >= as.POSIXct('2017-06-12', tz=timez) & 
                          datetime < as.POSIXct('2017-06-13', tz=timez))),
       aes(x=datetime, y=value, color=sensorDepth_m)) +
  geom_point() + 
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title='Panther Pond DO 2017', 
       x='date',
       y='do (mg/L)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_colorblind()


DO_L1 <- DO_L1 %>% 
  mutate_at(vars(all_of(do)),
            ~(case_when(datetime >= as.POSIXct('2017-06-12 11:00', tz=timez) &
                             datetime < as.POSIXct('2017-06-12 11:40', tz=timez)~ NA_real_,
                        sensorDepth_m == '19' & datetime < as.POSIXct('2017-06-12 11:40', tz=timez) ~ NA_real_,
                           TRUE ~ .)))
DO_L1_vert <- DO_L1 %>% 
  select(datetime:oxygenDissolved_percSat, tz:sensorDepth_m) %>% 
  pivot_longer(names_to = 'variable',
               values_to = 'value',
               -c(datetime, tz, logger, source_file, sensorDepth_m))

ggplot(subset(DO_L1_vert,
              subset = (datetime >= as.POSIXct('2017-07-02', tz=timez) & 
                          datetime < as.POSIXct('2017-07-03', tz=timez))),
       aes(x=datetime, y=value, color=sensorDepth_m)) +
  geom_point() + 
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title='Panther Pond DO 2017', 
       x='date',
       y='do (mg/L)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_colorblind()

DO_L1 <- DO_L1 %>% 
  mutate_at(vars(all_of(do)),
            ~(case_when(datetime >= as.POSIXct('2017-07-02 15:00', tz=timez) &
                             datetime < as.POSIXct('2017-07-02 15:30', tz=timez) &
                             sensorDepth_m == 19 ~ NA_real_,
                           TRUE ~ .)))

DO_L1_vert <- DO_L1 %>% 
  select(datetime:oxygenDissolved_percSat, tz:sensorDepth_m) %>% 
  pivot_longer(names_to = 'variable',
               values_to = 'value',
               -c(datetime, tz, logger, source_file, sensorDepth_m))

#low do malfunction from Jul9-14

ggplot(subset(DO_L1_vert,
              subset = (datetime >= as.POSIXct('2017-07-09', tz=timez) & 
                          datetime < as.POSIXct('2017-07-10', tz=timez))),
       aes(x=datetime, y=value, color=sensorDepth_m)) +
  geom_point() + 
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title='Panther Pond DO 2017', 
       x='date',
       y='do (mg/L)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_colorblind()

ggplot(subset(DO_L1_vert,
              subset = (datetime >= as.POSIXct('2017-07-14', tz=timez) & 
                          datetime < as.POSIXct('2017-07-15', tz=timez))),
       aes(x=datetime, y=value, color=sensorDepth_m)) +
  geom_point() + 
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title='Panther Pond DO 2017', 
       x='date',
       y='do (mg/L)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_colorblind()

DO_L1 <- DO_L1 %>% 
  mutate_at(vars(all_of(do)),
            ~(case_when(datetime >= as.POSIXct('2017-07-09 9:50', tz=timez) &
                             datetime < as.POSIXct('2017-07-14 11:30', tz=timez) &
                             sensorDepth_m == 19 ~ NA_real_,
                           datetime >= as.POSIXct('2017-07-14 10:50', tz=timez) &
                             datetime < as.POSIXct('2017-07-14 11:20', tz=timez) &
                             sensorDepth_m == 1.5 ~ NA_real_,
                           TRUE ~ .)))
DO_L1_vert <- DO_L1 %>% 
  select(datetime:oxygenDissolved_percSat, tz:sensorDepth_m) %>% 
  pivot_longer(names_to = 'variable',
               values_to = 'value',
               -c(datetime, tz, logger, source_file, sensorDepth_m))


ggplot(subset(DO_L1_vert,
             subset = (datetime >= as.POSIXct('2017-08-01', tz=timez) & 
                  datetime < as.POSIXct('2017-08-02', tz=timez))),
      aes(x=datetime, y=value, color=sensorDepth_m)) +
 geom_point() + 
 facet_grid(variable ~ ., scales = 'free_y') +
 labs(title='Panther Pond DO 2017', 
      x='date',
      y='do (mg/L)') +
 final_theme +
 scale_x_datetime(date_minor_breaks = '1 hour') +
 scale_color_colorblind()

DO_L1 <- DO_L1 %>% 
  mutate_at(vars(all_of(do)),
            ~(case_when(datetime == as.POSIXct('2017-08-01 7:10', tz=timez) ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate(do_flag = case_when(datetime == as.POSIXct('2017-08-01 7:20', tz=timez) ~ 'w',
                             TRUE ~ NA_character_))

DO_L1_vert <- DO_L1 %>% 
  select(datetime:oxygenDissolved_percSat, tz:sensorDepth_m) %>% 
  pivot_longer(names_to = 'variable',
               values_to = 'value',
               -c(datetime, tz, logger, source_file, sensorDepth_m))

ggplot(subset(DO_L1_vert,
              subset = (datetime >= as.POSIXct('2017-08-07', tz=timez) & 
                          datetime < as.POSIXct('2017-08-08', tz=timez))),
       aes(x=datetime, y=value, color=sensorDepth_m)) +
  geom_point() + 
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title='Panther Pond DO 2017', 
       x='date',
       y='do (mg/L)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_colorblind()

DO_L1 <- DO_L1 %>% 
  mutate_at(vars(all_of(do)),
            ~(case_when(datetime >= as.POSIXct('2017-08-07 8:30', tz=timez) &
                          datetime < as.POSIXct('2017-08-07 9:40', tz=timez) &
                          sensorDepth_m == 1.5 ~ NA_real_,
                        TRUE ~ .))) 
DO_L1_vert <- DO_L1 %>% 
  select(datetime:oxygenDissolved_percSat, tz:sensorDepth_m) %>% 
  pivot_longer(names_to = 'variable',
               values_to = 'value',
               -c(datetime, tz, logger, source_file, sensorDepth_m))

ggplot(subset(DO_L1_vert,
              subset = (datetime >= as.POSIXct('2017-08-01', tz=timez) & 
                          datetime < as.POSIXct('2017-09-01', tz=timez))),
       aes(x=datetime, y=value, color=sensorDepth_m)) +
  geom_point() + 
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title='Panther Pond DO 2017', 
       x='date',
       y='do (mg/L)') +
  final_theme +
    scale_x_datetime(date_minor_breaks = '1 day') +
  scale_color_colorblind()

ggplot(subset(DO_L1_vert,
             subset = (datetime >= as.POSIXct('2017-09-12', tz=timez) & 
                  datetime < as.POSIXct('2017-09-13', tz=timez))),
      aes(x=datetime, y=value, color=sensorDepth_m)) +
 geom_point() + 
 facet_grid(variable ~ ., scales = 'free_y') +
 labs(title='Panther Pond DO 2017', 
      x='date',
      y='do (mg/L)') +
 final_theme +
 scale_x_datetime(date_minor_breaks = '1 hour') +
 scale_color_colorblind()
       
DO_L1 <- DO_L1 %>% 
  mutate_at(vars(all_of(do)),
            ~(case_when(datetime >= as.POSIXct('2017-09-12 10:10', tz=timez) &
                             datetime < as.POSIXct('2017-09-12 17:00', tz=timez) &
                             sensorDepth_m == 1.5 ~ NA_real_,
                           datetime >= as.POSIXct('2017-09-12 10:10', tz=timez) &
                             datetime < as.POSIXct('2017-09-12 11:00', tz=timez) &
                             sensorDepth_m == 19 ~ NA_real_,
                           datetime >= as.POSIXct('2017-09-12 16:10', tz=timez) &
                             datetime < as.POSIXct('2017-09-12 17:00', tz=timez) &
                             sensorDepth_m == 19 ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate(do_flag = case_when(datetime == as.POSIXct('2017-09-12 17:10', tz=timez) ~ 'w',
                             TRUE ~ NA_character_))

DO_L1_vert <- DO_L1 %>% 
  select(datetime:oxygenDissolved_percSat, tz:sensorDepth_m) %>% 
  pivot_longer(names_to = 'variable',
               values_to = 'value',
               -c(datetime, tz, logger, source_file, sensorDepth_m))


ggplot(subset(DO_L1_vert,
              subset = (datetime >= as.POSIXct('2017-09-13', tz=timez) & 
                          datetime < as.POSIXct('2017-09-14', tz=timez))),
       aes(x=datetime, y=value, color=sensorDepth_m)) +
  geom_point() + 
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title='Panther Pond DO 2017', 
       x='date',
       y='do (mg/L)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_colorblind()

DO_L1 <- DO_L1 %>% 
  mutate_at(vars(all_of(do)),
            ~(case_when(datetime >= as.POSIXct('2017-09-13 10:40', tz=timez) &
                             datetime < as.POSIXct('2017-09-13 11:40', tz=timez) &
                             sensorDepth_m == 19 ~ NA_real_,
                           TRUE ~ .)))

DO_L1_vert <- DO_L1 %>% 
  select(datetime:oxygenDissolved_percSat, tz:sensorDepth_m) %>% 
  pivot_longer(names_to = 'variable',
               values_to = 'value',
               -c(datetime, tz, logger, source_file, sensorDepth_m))


ggplot(subset(DO_L1_vert,
              subset = (datetime >= as.POSIXct('2017-10-08', tz=timez) & 
                          datetime < as.POSIXct('2017-10-09', tz=timez))),
       aes(x=datetime, y=value, color=sensorDepth_m)) +
  geom_point() + 
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title='Panther Pond DO 2017', 
       x='date',
       y='do (mg/L)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_colorblind()

DO_L1 <- DO_L1 %>% 
  mutate_at(vars(all_of(do)),
            ~(case_when(datetime >= as.POSIXct('2017-10-08 7:40', tz=timez) &
                             datetime < as.POSIXct('2017-10-08 8:10', tz=timez) &
                          sensorDepth_m == 1.5 ~ NA_real_,
                           TRUE ~ .)))

DO_L1_vert <- DO_L1 %>% 
  select(datetime:oxygenDissolved_percSat, tz:sensorDepth_m) %>% 
  pivot_longer(names_to = 'variable',
               values_to = 'value',
               -c(datetime, tz, logger, source_file, sensorDepth_m))


ggplot(subset(DO_L1_vert,
              subset = (datetime >= as.POSIXct('2017-10-28', tz=timez) & 
                          datetime < as.POSIXct('2017-10-29', tz=timez))),
       aes(x=datetime, y=value, color=sensorDepth_m)) +
  geom_point() + 
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title='Panther Pond DO 2017', 
       x='date',
       y='do (mg/L)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_colorblind()

DO_L1 <- DO_L1 %>% 
  mutate_at(vars(all_of(do)),
            ~(case_when(datetime >= as.POSIXct('2017-10-28 12:10', tz=timez) ~ NA_real_,
                           TRUE ~ .)))

DO_L1_vert <- DO_L1 %>% 
  select(datetime:oxygenDissolved_percSat, tz:sensorDepth_m) %>% 
  pivot_longer(names_to = 'variable',
               values_to = 'value',
               -c(datetime, tz, logger, source_file, sensorDepth_m))

DO_L1 <- DO_L1 %>% 
  mutate(do_flag = case_when(is.na(do_flag) & (oxygenDissolved_mgl < 0 | oxygenDissolved_percSat <0)~ 'z',
                             !is.na(do_flag) & (oxygenDissolved_mgl <0| oxygenDissolved_percSat <0) ~ paste(do_flag, 'z', sep = ', '),
                             TRUE ~ do_flag),
         oxygenDissolved_mgl = case_when(oxygenDissolved_mgl < 0 ~ 0,
                            TRUE ~ oxygenDissolved_mgl), 
         oxygenDissolved_percSat = case_when(oxygenDissolved_percSat < 0 ~ 0,
                                         TRUE ~ oxygenDissolved_percSat))

DO_L1_vert <- DO_L1 %>% 
  select(datetime:oxygenDissolved_percSat, tz:sensorDepth_m) %>% 
  pivot_longer(names_to = 'variable',
               values_to = 'value',
               -c(datetime, tz, logger, source_file, sensorDepth_m))

## save monthly clean plots ----
for (i in 1: length(monthlist)){
  df = DO_L1_vert %>% 
    filter(month == monthlist[i])
  plot <- ggplot(df, aes(x=datetime, y=value, color=sensorDepth_m)) +
    geom_point() + 
    facet_grid(variable ~ ., scales = 'free_y') +
    labs(title = paste0('2017-', monthlist[i], ' do data - clean'), 
         y='dissolved oxygen') +
    final_theme +
    scale_x_datetime(date_minor_breaks = '1 day') +
    scale_color_colorblind()
  print(plot)
  filename = paste0('2017-', monthlist[i], '_L1_do.png')
  ggsave(file.path(procfigdir, 'do/', filename), device = 'png')
}

#annual plot
ggplot(DO_L1_vert, aes(x=datetime, y=value, color=sensorDepth_m)) +
  geom_point() + 
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title='Panther Pond DO L1 2017', 
       x='date',
       y='dissolved oxygen') +
  scale_color_colorblind() +   
  scale_x_datetime(date_minor_breaks = '1 month') +
  final_theme 
ggsave(file.path(annualfigdir, '2017_DO.png'))

## save clean DO file ----
DO_L1 %>% 
  mutate(datetime = as.character(datetime)) %>% 
  select(datetime, tz, sensorDepth_m, oxygenDissolved_mgl, oxygenDissolved_percSat, adj_oxygenDissolved_mgl, 
         waterTemperature_degC, do_flag, logger, source_file) %>% 
  write.csv(., file.path(procdir, paste0('PantherBuoy_L1_DO_2017_v', Sys.Date(), '.csv')), row.names = F)


# LITTORAL DATA COLLATION ####
files = list.files(file.path(datadir, 'littoral/temp/')) #get list of files in [i] directory
files = files[!grepl('hobo', files)] #remove the hobo folder


for(j in 1:length(files)){
  df = read.csv(file.path(datadir, 'littoral/temp/', files[j]),
                skip = 1)
  #get TZ
  df$tz = substr(colnames(df)[2], 12, 21)
  
  #get logger from colnames
  colname_string = strsplit(gsub("\\.", " ",colnames(df)[3]), ' ')
  df$logger = last(colname_string[[1]])
  
  #get location from filename
  location_string = strsplit(gsub('\\_', ' ', files[j]), ' ')
  df$location = first(location_string[[1]])
  
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
  colnames(df)[grep('adj', cn, ignore.case = T)] = 'adj_oxygenDissolved_mgl'
  cn = colnames(df) ## need to re run this so that the adjusted DO doesn't get mixed in with the uncorrected.
  colnames(df)[grep('mg.L', cn, ignore.case = T)] = 'oxygenDissolved_mgl'
  colnames(df)[grep('percent', cn, ignore.case = T)] = 'oxygenDissolved_percSat'
  colnames(df)[grep('stop', cn, ignore.case = T)] = 'coupStop'
  
  #add metadata
  df$source_file = files[j]

  #format time
  if(df$tz[1] == 'GMT.04.00'){
    timez = 'Etc/GMT+4'
  } else if (df$tz[1] == 'GMT.05.00'){
    timez = 'Etc/GMT+5'
  } else {
    stop('Did not recognize timezone in file ', files[j], '. Fix in code.')
  }
  df$datetime = as.POSIXct(df$datetime, tz = timez, format = '%m/%d/%y %I:%M:%S %p')
  print(files[j])
  if(j == 1){
    logger_raw <- df
  } else {
    logger_raw <- full_join(logger_raw, df)
  }
}

littoral_L0 <- logger_raw %>% 
  mutate(sensorDepth_m = case_when(location == 'rand' ~ 0.65,
                                   TRUE ~ 1))

colnames(littoral_L0)

## Save L0 dataset ----

ggplot(littoral_L0, aes(x=datetime, y=waterTemperature_degC)) +
  geom_point() + 
  facet_grid(location ~ . ) +
  labs(title='Panther Pond Littoral Data, temperature, Level 0', 
       x='date', 
       y='water temp (deg C)') +
  scale_color_colorblind() +   
  final_theme

write.csv(littoral_L0, file.path(rawdir,'PantherLittoral_L0_2017.csv'), row.names = F)

# VIEW AND CLEAN LITTORAL DATA ----

## save monthly raw plots ####
littoral_L1 <- littoral_L0

monthlist_short = c('07', '08', '09', '10')

#add month for filter to the vertically-oriented data frame
littoral_L1 <- littoral_L1 %>% 
  mutate(month = as.character(format(datetime, '%m')))

# iterate over months creating and saving figures to 'rawfigdir'
for (i in 1: length(monthlist_short)){
  df = littoral_L1 %>% 
    filter(month == monthlist_short[i])
  plot <- ggplot(df, aes(x=datetime, y=waterTemperature_degC)) +
    geom_point() +
    facet_grid(location ~ . ) +
    labs(title = paste0('2017-', monthlist_short[i], ' temp data - raw'), 
         y='temp (deg C)') +
    final_theme +
    scale_x_datetime(date_minor_breaks = '1 day')
  print(plot)
  filename = paste0('2017-', monthlist_short[i], '_littoral_L0_temp.png')
  ggsave(file.path(rawfigdir, 'littoral/', filename), device = 'png')
}

# Jul 25 sensors out
ggplot(subset(littoral_L1,
              subset=(datetime >= as.POSIXct('2017-07-25', tz=timez) & datetime < as.POSIXct('2017-07-26', tz=timez))),
       aes(x=datetime, y = waterTemperature_degC)) +
  geom_point() +
  facet_grid(location ~ .) +
  labs(title='Panther Pond Littoral Sites L0 Jul 25 2017', 
       x=NULL,
       y='temperature (degrees C)') +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme

littoral_L1 <- littoral_L1 %>% 
  mutate_at(vars(waterTemperature_degC, luminousFlux_lux),
            ~(case_when(location == 'connolly' & datetime < as.POSIXct('2017-07-25 10:00', tz=timez) ~ NA_real_,
                           location == 'rand' & datetime < as.POSIXct('2017-07-25 9:00', tz=timez) ~ NA_real_,
                           location == 'walker' & datetime < as.POSIXct('2017-07-25 10:30', tz=timez) ~ NA_real_,
                           location == 'wilson' & datetime < as.POSIXct('2017-07-25 8:30', tz=timez) ~ NA_real_,
                           location == 'relyea' & datetime < as.POSIXct('2017-07-25 10:50', tz=timez) ~ NA_real_,
                           TRUE ~ .)))

ggplot(subset(littoral_L1,
              subset=(datetime >= as.POSIXct('2017-07-25', tz=timez) & datetime < as.POSIXct('2017-07-26', tz=timez))),
       aes(x=datetime, y = waterTemperature_degC)) +
  geom_point() +
  facet_grid(location ~ .) +
  labs(title='Panther Pond Littoral Sites L1 Jul 25 2017', 
       x=NULL,
       y='temperature (degrees C)') +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme

ggplot(subset(littoral_L1,
              subset=(datetime >= as.POSIXct('2017-07-01', tz=timez) & datetime < as.POSIXct('2017-08-01', tz=timez))),
       aes(x=datetime, y = waterTemperature_degC)) +
  geom_point() +
  facet_grid(location ~ .) +
  labs(title='Panther Pond Littoral Sites L1 Jul 2017', 
       x=NULL,
       y='temperature (degrees C)') +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme

#oct 7 removal
ggplot(subset(littoral_L1,
              subset=(datetime >= as.POSIXct('2017-10-07', tz=timez) & datetime < as.POSIXct('2017-10-08', tz=timez))),
       aes(x=datetime, y = waterTemperature_degC)) +
  geom_point() +
  facet_grid(location ~ .) +
  labs(title='Panther Pond Littoral Sites L0 Oct 7 2017', 
       x=NULL,
       y='temperature (degrees C)') +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme

littoral_L1 <- littoral_L1 %>% 
  mutate_at(vars(waterTemperature_degC, luminousFlux_lux),
            ~(case_when(location == 'walker' & datetime >= as.POSIXct('2017-10-07 16:00', tz=timez) ~ NA_real_,
                            TRUE ~ .)))

ggplot(subset(littoral_L1,
              subset=(datetime >= as.POSIXct('2017-10-07', tz=timez) & datetime < as.POSIXct('2017-10-08', tz=timez))),
       aes(x=datetime, y = waterTemperature_degC)) +
  geom_point() +
  facet_grid(location ~ .) +
  labs(title='Panther Pond Littoral Sites L1 Oct 7 2017', 
       x=NULL,
       y='temperature (degrees C)') +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme

littoral_L1 <- littoral_L1 %>% 
  mutate(waterTemperature_degC = case_when(datetime >= as.POSIXct('2017-10-07 11:50', tz=timez) &
                              location == 'relyea' ~ NA_real_,
                            TRUE ~ waterTemperature_degC))

## print monthly clean plots ####

for (i in 1: length(monthlist_short)){
  df = littoral_L1 %>% 
    filter(month == monthlist_short[i])
  plot <- ggplot(df, aes(x=datetime, y=waterTemperature_degC)) +
    facet_grid(location ~ .) +
    geom_point() +
    labs(title = paste0('2017-', monthlist_short[i], 'littoral temp data - clean'), 
         y='temp (deg C)') +
    final_theme +
    scale_x_datetime(date_minor_breaks = '1 day')
  print(plot)
  filename = paste0('2017-', monthlist_short[i], '_littoral_L1_temp.png')
  ggsave(file.path(procfigdir, 'littoral/', filename), device = 'png')
}

# annual fig #

ggplot(littoral_L1, aes(x=datetime, y=waterTemperature_degC)) +
  geom_point() +
  facet_grid(location ~ .) +
  labs(title='Panther Pond L1 littoral 2017', 
       x='date',
       y='temperature (degrees C)') +
  scale_x_datetime(date_minor_breaks = '1 month')  +   
  final_theme
ggsave(file.path(annualfigdir, '2017_LittoralTemperature.png'))

## save clean temp file ----
littoral_L1 %>%
  select(datetime, tz, location, sensorDepth_m, waterTemperature_degC, luminousFlux_lux, logger, source_file) %>% 
  mutate(datetime = as.character(datetime)) %>% 
  write.csv(., file.path(procdir, paste0('PantherBuoy_L1_littoral_2017_v', Sys.Date(), '.csv')), row.names = F)


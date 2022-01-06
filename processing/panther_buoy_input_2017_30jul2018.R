library(tidyverse) 
library(readxl) 
library(ggthemes)

source('G:/My Drive/Panther Pond/r program/panther_buoy_input_2016_19jul2018.R')
PAN_16_L0 <- PAN_L0
PAN_16_L1 <- PAN_L1
rm(PAN_L0, PAN_L1)

#save final theme for ggplot
final_theme=theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        plot.title=element_text(size=16, face='bold', hjust=0.5)) #save as a grom


varnames = c('obsno', 'datetime', 'temp_C', 'intens_lux', 'attach', 'stop', 'eof')
varnames2 = c('obsno', 'datetime', 'temp_C', 'intens_lux', 'attach', 'connect', 'detach', 'stop', 'eof')
varnames3 = c('obsno', 'datetime', 'temp_C', 'intens_lux', 'attach', 'detach', 'stop', 'eof')
varnames4 = c('obsno', 'datetime', 'temp_C', 'intens_lux', 'batt', 'attach', 'stop', 'eof')

dovars = c('obsno', 'datetime', 'do_mgl', 'temp_C', 'attach', 'stop', 'eof')
dovars2 = c('obsno', 'datetime', 'do_mgl', 'temp_C', 'attach', 'detach', 'stop', 'eof')

litnames = c('obsno', 'datetime', 'temp_C', 'intens_lux', 'attach', 'stop', 'eof')
litnames2 = c('obsno', 'datetime', 'temp_C', 'intens_lux', 'attach', 'detach', 'stop', 'eof')


#### TEMP DATA ####
setwd('G:/My Drive/Panther Pond/raw data/2017/buoy/temp/')

#SURFACE
pan_surf_0506 <- read_csv('surf_17may-17jun.csv',
                         skip=2,
                         col_names = varnames3,
                         col_types = cols(.default = col_character())) %>% 
  mutate(loggernum = 10616536,
         fileorig = 'surf_17may-17jun.csv',
         download = '2017-06-10')

pan_surf_0607 <- read_csv('surf_17jun-17jul.csv',
                          skip=2,
                          col_names = varnames,
                          col_types = cols(.default = col_character())) %>% 
  mutate(loggernum = 10616536,
         fileorig = 'surf_17jun-17jul.csv',
         download = '2017-07-14')

pan_surf_0709 <- read_csv('surf_17jul-17sept.csv',
                          skip=2,
                          col_names = varnames3,
                          col_types = cols(.default = col_character())) %>% 
  mutate(loggernum = 10616536,
         fileorig = 'surf_17jul-17sept.csv',
         download = '2017-09-12')

pan_surf_0910 <- read_csv('surf_17sept-17oct.csv',
                          skip=2,
                          col_names = varnames2,
                          col_types = cols(.default = col_character())) %>% 
  mutate(loggernum = 10616536,
         fileorig = 'surf_17sept-17oct.csv',
         download = '2017-10-28')


#0.5 METERS
pan_0p5_0506 <- read_csv('0.5_meter_17may-17jun.csv',
                         skip=2,
                         col_names = varnames,
                         col_types = cols(.default = col_character())) %>% 
  mutate(loggernum = 10616537,
         fileorig = '0.5_meter_17may-17jun.csv',
         download = '2017-06-10')

pan_0p5_0607 <- read_csv('0.5_meter_17jun-17jul.csv',
                         skip=2,
                         col_names = varnames,
                         col_types = cols(.default = col_character())) %>% 
  mutate(loggernum = 10616537,
         fileorig = '0.5_meter_17jun-17jul.csv',
         download = '2017-07-14')

pan_0p5_0910 <- read_csv('0.5_meter_17sep-17oct.csv',
                         skip=2,
                         col_names = varnames2,
                         col_types = cols(.default = col_character())) %>% 
  mutate(loggernum = 10616537,
         fileorig = '0.5_meter_17sep=17oct.csv',
         download = '2017-10-28')


#2 meters
pan_2_0506 <- read_csv('2_meters_17may-17jun.csv',
                          skip=2,
                          col_names = varnames4,
                       col_types = cols(.default = col_character())) %>% 
  mutate(loggernum = 10616538,
         fileorig = '2_meters_17may-17jun.csv',
         download = '2017-06-10')

pan_2_0607 <- read_csv('2_meters_17jun-17jul.csv',
                          skip=2,
                          col_names = varnames,
                       col_types = cols(.default = col_character())) %>% 
  mutate(loggernum = 10616538,
         fileorig = '2_meters_17jun-17jul.csv',
         download = '2017-07-14')

pan_2_0709 <- read_csv('2_meters_17jul-17sept.csv',
                          skip=2,
                          col_names = varnames3,
                       col_types = cols(.default = col_character())) %>% 
  mutate(loggernum = 10616538,
         fileorig = '2_meters_17jul-17sept.csv',
         download = '2017-09-12')

pan_2_0910 <- read_csv('2_meters_17sept-17oct.csv',
                           skip=2,
                           col_names = varnames,
                       col_types = cols(.default = col_character())) %>% 
  mutate(loggernum = 10616538,
         fileorig = '2_meters_17sept-17oct.csv',
         download = '2017-10-28')


#4 meters
pan_4_0506 <- read_csv('4_meters_17may-17jun.csv',
                       skip=2,
                       col_names = varnames3,
                       col_types = cols(.default = col_character())) %>% 
  mutate(loggernum = 10616540,
         fileorig = '4_meters_17may-17jun.csv',
         download = '2017-06-10')

pan_4_0607 <- read_csv('4_meters_17jun-17jul.csv',
                       skip=2,
                       col_names = varnames,
                       col_types = cols(.default = col_character())) %>% 
  mutate(loggernum = 10616540,
         fileorig = '4_meters_17jun-17jul.csv',
         download = '2017-07-14')

pan_4_0709 <- read_csv('4_meters_17jul-17sept.csv',
                       skip=2,
                       col_names = varnames,
                       col_types = cols(.default = col_character())) %>% 
  mutate(loggernum = 10616540,
         fileorig = '4_meters_17jul-17sept.csv',
         download = '2017-09-12')

pan_4_0910 <- read_csv('4_meters_17sept-17oct.csv',
                       skip=2,
                       col_names = varnames,
                       col_types = cols(.default = col_character())) %>% 
  mutate(loggernum = 10616540,
         fileorig = '4_meters_17sept-17oct.csv',
         download = '2017-10-28')


#6 meters
pan_6_0506 <- read_csv('6_meters_17may-17jun.csv',
                       skip=2,
                       col_names = varnames3,
                       col_types = cols(.default = col_character())) %>% 
  mutate(loggernum = 10616541,
         fileorig = '6_meters_17may-17jun.csv',
         download = '2017-06-10')

pan_6_0607 <- read_csv('6_meters_17jun-17jul.csv',
                       skip=2,
                       col_names = varnames,
                       col_types = cols(.default = col_character())) %>% 
  mutate(loggernum = 10616541,
         fileorig = '6_meters_17jun-17jul.csv',
         download = '2017-07-14')

pan_6_0709 <- read_csv('6_meters_17jul-17sep.csv',
                       skip=2,
                       col_names = varnames,
                       col_types = cols(.default = col_character())) %>% 
  mutate(loggernum = 10616541,
         fileorig = '6_meters_17jul-17sep.csv',
         download = '2017-09-12')

pan_6_0910 <- read_csv('6_meters_17sept-17oct.csv',
                       skip=2,
                       col_names = varnames,
                       col_types = cols(.default = col_character())) %>% 
  mutate(loggernum = 10616541,
         fileorig = '6_meters_17sept-17oct.csv',
         download = '2017-10-28')


#8 meters
pan_8_0506 <- read_csv('8_meters_17may-17jun.csv',
                       skip=2,
                       col_names = varnames3,
                       col_types = cols(.default = col_character())) %>% 
  mutate(loggernum = 10616544,
         fileorig = '8_meters_17may-17jun.csv',
         download = '2017-06-10')

pan_8_0607 <- read_csv('8_meters_17jun-17jul.csv',
                       skip=2,
                       col_names = varnames,
                       col_types = cols(.default = col_character())) %>% 
  mutate(loggernum = 10616544,
         fileorig = '8_meters_17jun-17jul.csv',
         download = '2017-07-14')

pan_8_0910 <- read_csv('8_meters_17sept-17oct.csv',
                       skip=2,
                       col_names = varnames2,
                       col_types = cols(.default = col_character())) %>% 
  mutate(loggernum = 10616544,
         fileorig = '8_meters_17sept-17oct.csv',
         download = '2017-10-28')



#10 meters
pan_10_0506 <- read_csv('10_meters_17may-17jun.csv',
                       skip=2,
                       col_names = varnames,
                       col_types = cols(.default = col_character())) %>% 
  mutate(loggernum = 10616545,
         fileorig = '10_meters_17may-17jun.csv',
         download = '2017-06-10')

pan_10_0607 <- read_csv('10_meters_17jun-17jul.csv',
                       skip=2,
                       col_names = varnames,
                       col_types = cols(.default = col_character())) %>% 
  mutate(loggernum = 10616545,
         fileorig = '10_meters_17jun-17jul.csv',
         download = '2017-07-14')

pan_10_0709 <- read_csv('10_meters_17jul-17sept.csv',
                       skip=2,
                       col_names = varnames3,
                       col_types = cols(.default = col_character())) %>% 
  mutate(loggernum = 10616545,
         fileorig = '10_meters_17jul-17sept.csv',
         download = '2017-09-12')

pan_10_0910 <- read_csv('10_meters_17sept-17oct.csv',
                       skip=2,
                       col_names = varnames,
                       col_types = cols(.default = col_character())) %>% 
  mutate(loggernum = 10616545,
         fileorig = '10_meters_17sept-17oct.csv',
         download = '2017-10-28')


#15 meters
pan_15_0506 <- read_csv('15_meter_17may-17jun.csv',
                       skip=2,
                       col_names = varnames3,
                       col_types = cols(.default = col_character())) %>% 
  mutate(loggernum = 10796877,
         fileorig = '15_meter_17may-17jun.csv',
         download = '2017-06-10')

pan_15_0607 <- read_csv('15_meters_17jun-17jul.csv',
                       skip=2,
                       col_names = varnames3,
                       col_types = cols(.default = col_character())) %>% 
  mutate(loggernum = 10796877,
         fileorig = '15_meters_17jun-17jul.csv',
         download = '2017-07-14')

pan_15_0709 <- read_csv('15_meters_17jul-17sept.csv',
                       skip=2,
                       col_names = varnames,
                       col_types = cols(.default = col_character())) %>% 
  mutate(loggernum = 10796877,
         fileorig = '15_meters_17jul-17sept.csv',
         download = '2017-09-12')

pan_15_0910 <- read_csv('15_meters_17sept-17oct.csv',
                       skip=2,
                       col_names = varnames,
                       col_types = cols(.default = col_character())) %>% 
  mutate(loggernum = 10796877,
         fileorig = '15_meters_17sept-17oct.csv',
         download = '2017-10-28')


#20 meters
pan_20_0506 <- read_csv('20_meters_17may-17jun.csv',
                       skip=2,
                       col_names = varnames3,
                       col_types = cols(.default = col_character())) %>% 
  mutate(loggernum = 10796878,
         fileorig = '20_meters_17may-17jun.csv',
         download = '2017-06-10')

pan_20_0607 <- read_csv('20_meters_17jun-17jul.csv',
                       skip=2,
                       col_names = varnames,
                       col_types = cols(.default = col_character())) %>% 
  mutate(loggernum = 10796878,
         fileorig = '20_meters_17jun-17jul.csv',
         download = '2017-07-14')

pan_20_0709 <- read_csv('20_meters_17jul-17sept.csv',
                       skip=2,
                       col_names = varnames3,
                       col_types = cols(.default = col_character())) %>% 
  mutate(loggernum = 10796878,
         fileorig = '20_meters_17jul-17sept.csv',
         download = '2017-09-12')

pan_20_0910 <- read_csv('20_meters_17sept-17oct.csv',
                       skip=2,
                       col_names = varnames,
                       col_types = cols(.default = col_character())) %>% 
  mutate(loggernum = 10796878,
         fileorig = '20_meters_17sept-17oct.csv',
         download = '2017-10-28')


#### merge all files ####
temp_2017 <- full_join(pan_surf_0506, pan_surf_0607) %>% 
  full_join(., pan_surf_0709) %>%
  full_join(., pan_surf_0910) %>% 
  full_join(., pan_0p5_0506) %>% 
  full_join(., pan_0p5_0607) %>% 
  full_join(., pan_0p5_0910) %>% 
  full_join(., pan_2_0506) %>% 
  full_join(., pan_2_0607) %>% 
  full_join(., pan_2_0709) %>% 
  full_join(., pan_2_0910) %>% 
  full_join(., pan_4_0506) %>% 
  full_join(., pan_4_0607) %>% 
  full_join(., pan_4_0709) %>% 
  full_join(., pan_4_0910) %>%
  full_join(., pan_6_0506) %>% 
  full_join(., pan_6_0607) %>% 
  full_join(., pan_6_0709) %>% 
  full_join(., pan_6_0910) %>% 
  full_join(., pan_8_0506) %>% 
  full_join(., pan_8_0607) %>% 
  full_join(., pan_8_0910) %>% 
  full_join(., pan_10_0506) %>% 
  full_join(., pan_10_0607) %>% 
  full_join(., pan_10_0709) %>% 
  full_join(., pan_10_0910) %>% 
  full_join(., pan_15_0506) %>% 
  full_join(., pan_15_0607) %>% 
  full_join(., pan_15_0709) %>% 
  full_join(., pan_15_0910) %>% 
  full_join(., pan_20_0506) %>% 
  full_join(., pan_20_0607) %>% 
  full_join(., pan_20_0709) %>% 
  full_join(., pan_20_0910) %>% 
  mutate(depth_m = case_when(loggernum == 10616536 ~ 0.1,
                             loggernum == 10616537 ~ 0.5,
                             loggernum == 10616538 ~ 2,
                             loggernum == 10616540 ~ 4,
                             loggernum == 10616541 ~ 6,
                             loggernum == 10616544 ~ 8,
                             loggernum == 10616545 ~ 10,
                             loggernum == 10796877 ~ 15,
                             loggernum == 10796878 ~ 20)) %>% 
  arrange(datetime, depth_m) %>% 
  mutate(temp_C = as.numeric(temp_C),
         datetime = as.POSIXct(datetime, tz='UTC', format = '%m/%d/%y %I:%M:%S %p'),
         depth_m = factor(depth_m, levels = c(0.1, 0.5, 2, 4, 6, 8, 10, 15, 20)))

setwd('G:/My Drive/Panther Pond/r export/')

ggplot(temp_2017, aes(x=datetime, y=temp_C, color = depth_m)) +
  geom_point() +
  labs(title='Panther Pond L0 2017', 
       x='date',
       y='temperature (degrees C)') +
    scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  final_theme 
# ggsave('figures/L0/2017/temp/2017_L0.tiff', width=6, height = 4, dpi=200)

# temp_2017 %>%
#   mutate(datetime = as.character(datetime)) %>%
#   write_csv(., 'datasets/L0/PantherBuoy_L0_2017.csv')

#clean up workspace
rm(pan_surf_0506, pan_surf_0607, pan_surf_0709, pan_surf_0910, pan_0p5_0506, pan_0p5_0607, pan_0p5_0910,
   pan_2_0506, pan_2_0607, pan_2_0709, pan_2_0910, pan_4_0506, pan_4_0607, pan_4_0709, 
   pan_4_0910, pan_6_0506, pan_6_0607, pan_6_0709, pan_6_0910, pan_8_0506, pan_8_0607, 
   pan_8_0910, pan_10_0506, pan_10_0607, pan_10_0709, pan_10_0910, pan_15_0506, pan_15_0607, 
   pan_15_0709, pan_15_0910, pan_20_0506, pan_20_0607, pan_20_0709, pan_20_0910)

####print and clean monthly ####
temp_2017 <- temp_2017 %>% 
  mutate(temp_C = as.numeric(temp_C),
         intens_lux = as.numeric(intens_lux))
PAN_temp_L1_2017 <- temp_2017

# MAY 2017 #
ggplot(subset(temp_2017,
              subset=(datetime>= as.POSIXct('2017-05-01', tz='UTC') & datetime < as.POSIXct('2017-06-01', tz='UTC'))),
       aes(x=datetime, y=temp_C, color = depth_m)) +
  geom_point() +
  labs(title='Panther Pond L0 May 2017', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme 
# ggsave('figures/L0/2017/temp/052017_L0.tiff', width=6, height = 4, dpi=200)

ggplot(subset(temp_2017,
              subset=(datetime>= as.POSIXct('2017-05-30', tz='UTC') & datetime < as.POSIXct('2017-05-31', tz='UTC'))),
       aes(x=datetime, y=temp_C, color = depth_m)) +
  geom_point() +
  labs(title='Panther Pond L0 May 30 2017', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme 

PAN_temp_L1_2017 <- PAN_temp_L1_2017 %>% 
  mutate_at(vars(temp_C, intens_lux),
            funs(case_when(datetime < as.POSIXct('2017-05-30 8:20', tz='UTC') ~ NA_real_,
                            TRUE ~ .)))

ggplot(subset(PAN_temp_L1_2017,
              subset=(datetime>= as.POSIXct('2017-05-01', tz='UTC') & datetime < as.POSIXct('2017-06-01', tz='UTC'))),
       aes(x=datetime, y=temp_C, color = depth_m)) +
  geom_point() +
  labs(title='Panther Pond L1 May 2017', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme 
# ggsave('figures/L1/2017/temp/052017_L1.tiff', width=6, height = 4, dpi=200)


# JUNE 2017 #
ggplot(subset(temp_2017,
              subset=(datetime>= as.POSIXct('2017-06-01', tz='UTC') & datetime < as.POSIXct('2017-07-01', tz='UTC'))),
       aes(x=datetime, y=temp_C, color = depth_m)) +
  geom_point() +
  labs(title='Panther Pond L0 June 2017', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme 
# ggsave('figures/L0/2017/temp/062017_L0.tiff', width=6, height = 4, dpi=200)

ggplot(subset(temp_2017,
              subset=(datetime>= as.POSIXct('2017-06-10', tz='UTC') & datetime < as.POSIXct('2017-06-11', tz='UTC'))),
       aes(x=datetime, y=temp_C, color = depth_m)) +
  geom_point() +
  labs(title='Panther Pond L0 June 10 2017', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme 

PAN_temp_L1_2017 <- PAN_temp_L1_2017 %>% 
  mutate_at(vars(temp_C, intens_lux),
            funs(case_when(datetime >= as.POSIXct('2017-06-10 13:10', tz='UTC') & 
                              datetime < as.POSIXct('2017-06-10 13:50', tz='UTC') & 
                              depth_m == 6 ~ NA_real_,
                            TRUE ~ .)))

ggplot(subset(PAN_temp_L1_2017,
              subset=(datetime>= as.POSIXct('2017-06-10', tz='UTC') & datetime < as.POSIXct('2017-06-11', tz='UTC'))),
       aes(x=datetime, y=temp_C, color = depth_m)) +
  geom_point() +
  labs(title='Panther Pond L0 June 10 2017', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme 

PAN_temp_L1_2017 <- PAN_temp_L1_2017 %>% 
  mutate_at(vars(temp_C, intens_lux),
            funs(case_when(datetime >= as.POSIXct('2017-06-10 19:30', tz='UTC') & 
                              datetime < as.POSIXct('2017-06-10 19:50', tz='UTC')  ~ NA_real_,
                            TRUE ~ .)))

ggplot(subset(PAN_temp_L1_2017,
              subset=(datetime>= as.POSIXct('2017-06-12', tz='UTC') & datetime < as.POSIXct('2017-06-13', tz='UTC'))),
       aes(x=datetime, y=temp_C, color = depth_m)) +
  geom_point() +
  labs(title='Panther Pond L0 June 12 2017', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme 

PAN_temp_L1_2017 <- PAN_temp_L1_2017 %>% 
  mutate_at(vars(temp_C, intens_lux),
            funs(case_when(datetime >= as.POSIXct('2017-06-12 11:00', tz='UTC') & 
                              datetime < as.POSIXct('2017-06-12 11:30', tz='UTC')  ~ NA_real_,
                            TRUE ~ .)))

ggplot(subset(PAN_temp_L1_2017,
              subset=(datetime>= as.POSIXct('2017-06-01', tz='UTC') & datetime < as.POSIXct('2017-07-01', tz='UTC'))),
       aes(x=datetime, y=temp_C, color = depth_m)) +
  geom_point() +
  labs(title='Panther Pond L1 June 2017', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme 
# ggsave('figures/L1/2017/temp/062017_L1.tiff', width=6, height = 4, dpi=200)



# JULY 2017 #
ggplot(subset(temp_2017,
              subset=(datetime>= as.POSIXct('2017-07-01', tz='UTC') & datetime < as.POSIXct('2017-08-01', tz='UTC'))),
       aes(x=datetime, y=temp_C, color = depth_m)) +
  geom_point() +
  labs(title='Panther Pond L0 Jul 2017', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme 
# ggsave('figures/L0/2017/temp/072017_L0.tiff', width=6, height = 4, dpi=200)


ggplot(subset(temp_2017,
              subset=(datetime>= as.POSIXct('2017-07-09', tz='UTC') & datetime < as.POSIXct('2017-07-10', tz='UTC'))),
       aes(x=datetime, y=temp_C, color = depth_m)) +
  geom_point() +
  labs(title='Panther Pond L0 Jul 09 2017', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
    scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme 

PAN_temp_L1_2017 <- PAN_temp_L1_2017 %>% 
  mutate_at(vars(temp_C, intens_lux),
            funs(case_when(datetime >= as.POSIXct('2017-07-09 9:50', tz='UTC') & 
                              datetime < as.POSIXct('2017-07-09 14:10', tz='UTC')  ~ NA_real_,
                            TRUE ~ .)))

ggplot(subset(temp_2017,
              subset=(datetime>= as.POSIXct('2017-07-14', tz='UTC') & datetime < as.POSIXct('2017-07-15', tz='UTC'))),
       aes(x=datetime, y=temp_C, color = depth_m)) +
  geom_point() +
  labs(title='Panther Pond L0 Jul 14 2017', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme 

PAN_temp_L1_2017 <- PAN_temp_L1_2017 %>% 
  mutate_at(vars(temp_C, intens_lux),
            funs(case_when(datetime >= as.POSIXct('2017-07-14 10:50', tz='UTC') & 
                              datetime < as.POSIXct('2017-07-14 11:20', tz='UTC')  ~ NA_real_,
                            TRUE ~ .)))


ggplot(subset(PAN_temp_L1_2017,
              subset=(datetime>= as.POSIXct('2017-07-01', tz='UTC') & datetime < as.POSIXct('2017-08-01', tz='UTC'))),
       aes(x=datetime, y=temp_C, color = depth_m)) +
  geom_point() +
  labs(title='Panther Pond L1 Jul 2017', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme 
# ggsave('figures/L1/2017/temp/072017_L1.tiff', width=6, height = 4, dpi=200)



# AUGUST 2017 #
ggplot(subset(temp_2017,
              subset=(datetime>= as.POSIXct('2017-08-01', tz='UTC') & datetime < as.POSIXct('2017-09-01', tz='UTC'))),
       aes(x=datetime, y=temp_C, color = depth_m)) +
  geom_point() +
  labs(title='Panther Pond L0 Aug 2017', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme 
# ggsave('figures/L0/2017/temp/082017_L0.tiff', width=6, height = 4, dpi=200)

ggplot(subset(temp_2017,
              subset=(datetime>= as.POSIXct('2017-08-01', tz='UTC') & datetime < as.POSIXct('2017-08-02', tz='UTC'))),
       aes(x=datetime, y=temp_C, color = depth_m)) +
  geom_point() +
  labs(title='Panther Pond L0 Aug 1 2017', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme 

PAN_temp_L1_2017 <- PAN_temp_L1_2017 %>% 
  mutate_at(vars(temp_C, intens_lux),
            funs(case_when(datetime == as.POSIXct('2017-08-01 7:10', tz='UTC') ~ NA_real_,
                            TRUE ~ .)))

ggplot(subset(PAN_temp_L1_2017,
              subset=(datetime>= as.POSIXct('2017-08-01', tz='UTC') & datetime < as.POSIXct('2017-09-01', tz='UTC'))),
       aes(x=datetime, y=temp_C, color = depth_m)) +
  geom_point() +
  labs(title='Panther Pond L1 Aug 2017', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme 
# ggsave('figures/L1/2017/temp/082017_L1.tiff', width=6, height = 4, dpi=200)



# SEPTEMBER 2017 #
ggplot(subset(temp_2017,
              subset=(datetime>= as.POSIXct('2017-09-01', tz='UTC') & datetime < as.POSIXct('2017-10-01', tz='UTC'))),
       aes(x=datetime, y=temp_C, color = depth_m)) +
  geom_point() +
  labs(title='Panther Pond L0 Sept 2017', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme 
# ggsave('figures/L0/2017/temp/092017_L0.tiff', width=6, height = 4, dpi=200)

ggplot(subset(temp_2017,
              subset=(datetime>= as.POSIXct('2017-09-12', tz='UTC') & datetime < as.POSIXct('2017-09-13', tz='UTC'))),
       aes(x=datetime, y=temp_C, color = depth_m)) +
  geom_point() +
  labs(title='Panther Pond L0 Sept 12 2017', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme 

PAN_temp_L1_2017 <- PAN_temp_L1_2017 %>% 
  mutate_at(vars(temp_C, intens_lux),
            funs(case_when(datetime >= as.POSIXct('2017-09-12 10:10', tz='UTC') &
                              datetime < as.POSIXct('2017-09-12 17:00', tz='UTC') ~ NA_real_,
                            TRUE ~ .)))

ggplot(subset(temp_2017,
              subset=(datetime>= as.POSIXct('2017-09-13', tz='UTC') & datetime < as.POSIXct('2017-09-14', tz='UTC'))),
       aes(x=datetime, y=temp_C, color = depth_m)) +
  geom_point() +
  labs(title='Panther Pond L0 Sept 13 2017', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme 

PAN_temp_L1_2017 <- PAN_temp_L1_2017 %>% 
  mutate(temp_C = case_when(datetime >= as.POSIXct('2017-09-13 10:40', tz='UTC') &
                              datetime < as.POSIXct('2017-09-13 11:40', tz='UTC') ~ NA_real_,
                            TRUE ~ temp_C))

ggplot(subset(PAN_temp_L1_2017,
              subset=(datetime>= as.POSIXct('2017-09-01', tz='UTC') & datetime < as.POSIXct('2017-10-01', tz='UTC'))),
       aes(x=datetime, y=temp_C, color = depth_m)) +
  geom_point() +
  labs(title='Panther Pond L1 Sept 2017', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme 
# ggsave('figures/L1/2017/temp/092017_L1.tiff', width=6, height = 4, dpi=200)



# OCTOBER 2017 #
ggplot(subset(temp_2017,
              subset=(datetime>= as.POSIXct('2017-10-01', tz='UTC') & datetime < as.POSIXct('2017-11-01', tz='UTC'))),
       aes(x=datetime, y=temp_C, color = depth_m)) +
  geom_point() +
  labs(title='Panther Pond L0 Oct 2017', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme 
# ggsave('figures/L0/2017/temp/102017_L0.tiff', width=6, height = 4, dpi=200)

ggplot(subset(temp_2017,
              subset=(datetime>= as.POSIXct('2017-10-28', tz='UTC') & datetime < as.POSIXct('2017-10-29', tz='UTC'))),
       aes(x=datetime, y=temp_C, color = depth_m)) +
  geom_point() +
  labs(title='Panther Pond L0 Oct 28 2017', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme 

PAN_temp_L1_2017 <- PAN_temp_L1_2017 %>% 
  mutate_at(vars(temp_C, intens_lux),
            funs(case_when(datetime >= as.POSIXct('2017-10-28 12:10', tz='UTC') ~ NA_real_,
                            TRUE ~ .)))

ggplot(subset(PAN_temp_L1_2017,
              subset=(datetime>= as.POSIXct('2017-10-01', tz='UTC') & datetime < as.POSIXct('2017-11-01', tz='UTC'))),
       aes(x=datetime, y=temp_C, color = depth_m)) +
  geom_point() +
  labs(title='Panther Pond L1 Oct 2017', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme 
# ggsave('figures/L1/2017/temp/102017_L1.tiff', width=6, height = 4, dpi=200)

# 2017 #
ggplot(PAN_temp_L1_2017, aes(x=datetime, y=temp_C, color = depth_m)) +
  geom_point() +
  labs(title='Panther Pond L1 2017', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  final_theme 
# ggsave('figures/L1/2017/temp/2017_L1.tiff', width=6, height = 4, dpi=200)

# PAN_temp_L1_2017 %>% 
#   mutate(datetime = as.character(datetime)) %>% 
#   select(datetime, depth_m, temp_C, intens_lux, loggernum, fileorig, download) %>% 
#   write_csv(., 'datasets/L1/2017 Panther L1 buoy data 30July2018.csv')



#### DO DATA ####
setwd('G:/My Drive/Panther Pond/raw data/2017/buoy/DO/')

# TOP #
upper_2017_0607 <- read_csv('TopDO_17jun-17jul.csv', 
                            skip = 2,
                            col_names = dovars) %>% 
  mutate(fileorig = 'TopDO_17jun-17jul.csv',
         loggernum = 20126324,
         depth_m = 1.5)

upper_2017_0709 <- read_csv('TopDO_17jul-17sep.csv', 
                            skip = 2,
                            col_names = dovars2) %>% 
  mutate(fileorig = 'TopDO_17jul-17sep.csv',
         loggernum = 20126324,
         depth_m = 1.5)

upper_2017_0910 <- read_csv('TopDO_17sep-17oct.csv', 
                            skip = 2,
                            col_names = dovars) %>% 
  mutate(fileorig = 'TopDO_17sep-17oct.csv',
         loggernum = 20126324,
         depth_m = 1.5)

# BOTTOM #
lower_2017_0506 <- read_csv('BottomDO_17may-17jun.csv', 
                            skip = 2,
                            col_names = dovars) %>% 
  mutate(fileorig = 'BottomDO_17may-17jun.csv',
         loggernum = 20126328,
         depth_m = 19)

lower_2017_0607 <- read_csv('BottomDO_17jun-17jul.csv', 
                            skip = 2,
                            col_names = dovars) %>% 
  mutate(fileorig = 'BottomDO_17jun-17jul.csv',
         loggernum = 20126328,
         depth_m = 19)

lower_2017_0709 <- read_csv('BottomDO_17jul-17sep.csv', 
                            skip = 2,
                            col_names = dovars) %>% 
  mutate(fileorig = 'BottomDO_17jul-17sep.csv',
         loggernum = 20126328,
         depth_m = 19)

lower_2017_0910 <- read_csv('BottomDO_17sep-17oct.csv', 
                            skip = 2,
                            col_names = dovars) %>% 
  mutate(fileorig = 'BottomDO_17sep-17oct.csv',
         loggernum = 20126328,
         depth_m = 19)

#### merge all do files ####
do_2017 <- full_join(upper_2017_0607, upper_2017_0709) %>% 
  full_join(., upper_2017_0910) %>% 
  full_join(., lower_2017_0506) %>% 
  full_join(., lower_2017_0607) %>% 
  full_join(., lower_2017_0709) %>% 
  full_join(., lower_2017_0910) %>% 
  arrange(datetime, depth_m) %>% 
  mutate(datetime = as.POSIXct(datetime, tz='UTC', format = '%m/%d/%y %I:%M:%S %p'),
         depth_m = factor(depth_m, levels = c(1.5, 19)))

setwd('G:/My Drive/Panther Pond/r export/')

ggplot(do_2017, aes(x=datetime, y=do_mgl, color=depth_m)) +
  geom_point() +
  labs(title='Panther Pond DO L0 2017', 
       x='date',
       y='do (mg/L)') +
  scale_color_colorblind() +   
  final_theme 
# ggsave('figures/L0/2017/do/2017_L0.tiff', width=6, height = 4, dpi=200)

ggplot(do_2017, aes(x=datetime, y=temp_C, color=depth_m)) +
  geom_point() +
  labs(title='Panther Pond DO L0 2017', 
       x='date',
       y='temperature (degrees C)') +
  scale_color_colorblind() +   
  final_theme 

# do_2017 %>%
#   mutate(datetime = as.character(datetime)) %>%
#   write_csv(., 'datasets/L0/PantherBuoy_DO_L0_2017.csv')

#clean up workspace
rm(upper_2017_0607, upper_2017_0709, upper_2017_0910, lower_2017_0506, lower_2017_0607, 
   lower_2017_0709, lower_2017_0910)

#### print and clean DO monthly ####
PAN_do_L1_2017 <- do_2017

# MAY 2017 #
ggplot(subset(do_2017,
              subset = (datetime >= as.POSIXct('2017-05-01', tz='UTC') & 
                          datetime < as.POSIXct('2017-06-01', tz='UTC'))),
       aes(x=datetime, y=do_mgl, color=depth_m)) +
  geom_point() +
  labs(title='Panther Pond DO L0 May 2017', 
       x='date',
       y='do (mg/L)') +
  scale_color_manual(values=c("#ffbf00")) +   
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme 
# ggsave('figures/L0/2017/do/052017_L0.tiff', width=6, height = 4, dpi=200)

ggplot(subset(do_2017,
              subset = (datetime >= as.POSIXct('2017-05-30', tz='UTC') & 
                          datetime < as.POSIXct('2017-05-31', tz='UTC'))),
       aes(x=datetime, y=do_mgl, color=depth_m)) +
  geom_point() +
  labs(title='Panther Pond DO L0 May 2017', 
       x='date',
       y='do (mg/L)') +
  scale_color_manual(values=c("#ffbf00")) +   
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme 

PAN_do_L1_2017 <- PAN_do_L1_2017 %>% 
  mutate_at(vars(do_mgl, temp_C),
            funs(case_when(datetime < as.POSIXct('2017-05-30 12:10', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))

ggplot(subset(PAN_do_L1_2017,
              subset = (datetime >= as.POSIXct('2017-05-01', tz='UTC') & 
                          datetime < as.POSIXct('2017-06-01', tz='UTC'))),
       aes(x=datetime, y=do_mgl, color=depth_m)) +
  geom_point() +
  labs(title='Panther Pond DO L1 May 2017', 
       x='date',
       y='do (mg/L)') +
  scale_color_manual(values=c("#ffbf00")) +   
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme 
# ggsave('figures/L1/2017/do/052017_L1.tiff', width=6, height = 4, dpi=200)


# JUNE 2017 #
ggplot(subset(do_2017,
              subset = (datetime >= as.POSIXct('2017-06-01', tz='UTC') & 
                          datetime < as.POSIXct('2017-07-01', tz='UTC'))),
       aes(x=datetime, y=do_mgl, color=depth_m)) +
  geom_point() +
  labs(title='Panther Pond DO L0 June 2017', 
       x='date',
       y='do (mg/L)') +
  scale_color_colorblind() + 
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme 
# ggsave('figures/L0/2017/do/062017_L0.tiff', width=6, height = 4, dpi=200)

ggplot(subset(do_2017,
              subset = (datetime >= as.POSIXct('2017-06-10', tz='UTC') & 
                          datetime < as.POSIXct('2017-06-11', tz='UTC'))),
       aes(x=datetime, y=do_mgl, color=depth_m)) +
  geom_point() +
  labs(title='Panther Pond DO L0 June 10 2017', 
       x='date',
       y='do (mg/L)') +
  scale_color_colorblind() + 
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme 

PAN_do_L1_2017 <- PAN_do_L1_2017 %>% 
  mutate_at(vars(do_mgl, temp_C),
            funs(case_when(datetime == as.POSIXct('2017-06-10 19:40', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))

ggplot(subset(do_2017,
              subset = (datetime >= as.POSIXct('2017-06-12', tz='UTC') & 
                          datetime < as.POSIXct('2017-06-13', tz='UTC'))),
       aes(x=datetime, y=do_mgl, color=depth_m)) +
  geom_point() +
  labs(title='Panther Pond DO L0 June 12 2017', 
       x='date',
       y='do (mg/L)') +
  scale_color_colorblind() + 
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme 

PAN_do_L1_2017 <- PAN_do_L1_2017 %>% 
  mutate_at(vars(do_mgl, temp_C),
            funs(case_when(datetime >= as.POSIXct('2017-06-12 11:00', tz='UTC') &
                             datetime < as.POSIXct('2017-06-12 11:40', tz='UTC')~ NA_real_,
                           TRUE ~ .)))

ggplot(subset(PAN_do_L1_2017,
              subset = (datetime >= as.POSIXct('2017-06-01', tz='UTC') & 
                          datetime < as.POSIXct('2017-07-01', tz='UTC'))),
       aes(x=datetime, y=do_mgl, color=depth_m)) +
  geom_point() +
  labs(title='Panther Pond DO L1 June 2017', 
       x='date',
       y='do (mg/L)') +
  scale_color_colorblind() + 
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme 
# ggsave('figures/L1/2017/do/062017_L1.tiff', width=6, height = 4, dpi=200)

# JULY 2017 #
ggplot(subset(do_2017,
              subset = (datetime >= as.POSIXct('2017-07-01', tz='UTC') & 
                          datetime < as.POSIXct('2017-08-01', tz='UTC'))),
       aes(x=datetime, y=do_mgl, color=depth_m)) +
  geom_point() +
  labs(title='Panther Pond DO L0 July 2017', 
       x='date',
       y='do (mg/L)') +
  scale_color_colorblind() + 
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme 
# ggsave('figures/L0/2017/do/072017_L0.tiff', width=6, height = 4, dpi=200)

ggplot(subset(do_2017,
              subset = (datetime >= as.POSIXct('2017-07-02', tz='UTC') & 
                          datetime < as.POSIXct('2017-07-03', tz='UTC'))),
       aes(x=datetime, y=do_mgl, color=depth_m)) +
  geom_point() +
  labs(title='Panther Pond DO L0 July 2 2017', 
       x='date',
       y='do (mg/L)') +
  scale_color_colorblind() + 
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme 

PAN_do_L1_2017 <- PAN_do_L1_2017 %>% 
  mutate_at(vars(do_mgl, temp_C),
            funs(case_when(datetime >= as.POSIXct('2017-07-02 15:00', tz='UTC') &
                             datetime < as.POSIXct('2017-07-02 15:30', tz='UTC') &
                             depth_m == 19 ~ NA_real_,
                           TRUE ~ .)))

ggplot(subset(do_2017,
              subset = (datetime >= as.POSIXct('2017-07-09', tz='UTC') & 
                          datetime < as.POSIXct('2017-07-10', tz='UTC'))),
       aes(x=datetime, y=do_mgl, color=depth_m)) +
  geom_point() +
  labs(title='Panther Pond DO L0 July 9 2017', 
       x='date',
       y='do (mg/L)') +
  scale_color_colorblind() + 
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme 

ggplot(subset(do_2017,
              subset = (datetime >= as.POSIXct('2017-07-14', tz='UTC') & 
                          datetime < as.POSIXct('2017-07-15', tz='UTC'))),
       aes(x=datetime, y=do_mgl, color=depth_m)) +
  geom_point() +
  labs(title='Panther Pond DO L0 July 14 2017', 
       x='date',
       y='do (mg/L)') +
  scale_color_colorblind() + 
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme 

PAN_do_L1_2017 <- PAN_do_L1_2017 %>% 
  mutate_at(vars(do_mgl, temp_C),
            funs(case_when(datetime >= as.POSIXct('2017-07-09 9:50', tz='UTC') &
                             datetime < as.POSIXct('2017-07-14 11:20', tz='UTC') &
                             depth_m == 19 ~ NA_real_,
                           datetime >= as.POSIXct('2017-07-14 10:50', tz='UTC') &
                             datetime < as.POSIXct('2017-07-14 11:20', tz='UTC') &
                             depth_m == 1.5 ~ NA_real_,
                           TRUE ~ .)))

ggplot(subset(PAN_do_L1_2017,
              subset = (datetime >= as.POSIXct('2017-07-01', tz='UTC') & 
                          datetime < as.POSIXct('2017-08-01', tz='UTC'))),
       aes(x=datetime, y=do_mgl, color=depth_m)) +
  geom_point() +
  labs(title='Panther Pond DO L1 July 2017', 
       x='date',
       y='do (mg/L)') +
  scale_color_colorblind() + 
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme 
# ggsave('figures/L1/2017/do/072017_L1.tiff', width=6, height = 4, dpi=200)



# AUGUST 2017 #
ggplot(subset(do_2017,
              subset = (datetime >= as.POSIXct('2017-08-01', tz='UTC') & 
                          datetime < as.POSIXct('2017-09-01', tz='UTC'))),
       aes(x=datetime, y=do_mgl, color=depth_m)) +
  geom_point() +
  labs(title='Panther Pond DO L0 Aug 2017', 
       x='date',
       y='do (mg/L)') +
  scale_color_colorblind() + 
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme 
# ggsave('figures/L0/2017/do/082017_L0.tiff', width=6, height = 4, dpi=200)

ggplot(subset(do_2017,
              subset = (datetime >= as.POSIXct('2017-08-01', tz='UTC') & 
                          datetime < as.POSIXct('2017-08-02', tz='UTC'))),
       aes(x=datetime, y=do_mgl, color=depth_m)) +
  geom_point() +
  labs(title='Panther Pond DO L0 Aug 01 2017', 
       x='date',
       y='do (mg/L)') +
  scale_color_colorblind() + 
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme 

PAN_do_L1_2017 <- PAN_do_L1_2017 %>% 
  mutate_at(vars(do_mgl, temp_C),
            funs(case_when(datetime == as.POSIXct('2017-08-01 7:10', tz='UTC') ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate(do_flag = case_when(datetime == as.POSIXct('2017-08-01 7:20', tz='UTC') ~ 'w',
                             TRUE ~ NA_character_))

ggplot(subset(PAN_do_L1_2017,
              subset = (datetime >= as.POSIXct('2017-08-01', tz='UTC') & 
                          datetime < as.POSIXct('2017-09-01', tz='UTC'))),
       aes(x=datetime, y=do_mgl, color=depth_m)) +
  geom_point() +
  labs(title='Panther Pond DO L1 Aug 2017', 
       x='date',
       y='do (mg/L)') +
  scale_color_colorblind() + 
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme 
# ggsave('figures/L1/2017/do/082017_L1.tiff', width=6, height = 4, dpi=200)



# SEPT 2017 #
ggplot(subset(do_2017,
              subset = (datetime >= as.POSIXct('2017-09-01', tz='UTC') & 
                          datetime < as.POSIXct('2017-10-01', tz='UTC'))),
       aes(x=datetime, y=do_mgl, color=depth_m)) +
  geom_point() +
  labs(title='Panther Pond DO L0 Sept 2017', 
       x='date',
       y='do (mg/L)') +
  scale_color_colorblind() + 
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme 
# ggsave('figures/L0/2017/do/092017_L0.tiff', width=6, height = 4, dpi=200)

ggplot(subset(do_2017,
              subset = (datetime >= as.POSIXct('2017-09-12', tz='UTC') & 
                          datetime < as.POSIXct('2017-09-13', tz='UTC'))),
       aes(x=datetime, y=do_mgl, color=depth_m)) +
  geom_point() +
  labs(title='Panther Pond DO L0 Sept 2017', 
       x='date',
       y='do (mg/L)') +
  scale_color_colorblind() + 
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme 

PAN_do_L1_2017 <- PAN_do_L1_2017 %>% 
  mutate_at(vars(do_mgl, temp_C),
            funs(case_when(datetime >= as.POSIXct('2017-09-12 10:10', tz='UTC') &
                             datetime < as.POSIXct('2017-09-12 17:00', tz='UTC') &
                             depth_m == 1.5 ~ NA_real_,
                           datetime >= as.POSIXct('2017-09-12 10:10', tz='UTC') &
                             datetime < as.POSIXct('2017-09-12 11:00', tz='UTC') &
                             depth_m == 19 ~ NA_real_,
                           datetime >= as.POSIXct('2017-09-12 16:10', tz='UTC') &
                             datetime < as.POSIXct('2017-09-12 17:00', tz='UTC') &
                             depth_m == 19 ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate(do_flag = case_when(datetime == as.POSIXct('2017-09-12 17:10', tz='UTC') ~ 'w',
                             TRUE ~ NA_character_))

ggplot(subset(do_2017,
              subset = (datetime >= as.POSIXct('2017-09-13', tz='UTC') & 
                          datetime < as.POSIXct('2017-09-14', tz='UTC'))),
       aes(x=datetime, y=do_mgl, color=depth_m)) +
  geom_point() +
  labs(title='Panther Pond DO L0 Sept 13 2017', 
       x='date',
       y='do (mg/L)') +
  scale_color_colorblind() + 
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme 

PAN_do_L1_2017 <- PAN_do_L1_2017 %>% 
  mutate_at(vars(do_mgl, temp_C),
            funs(case_when(datetime >= as.POSIXct('2017-09-13 10:40', tz='UTC') &
                             datetime < as.POSIXct('2017-09-13 11:40', tz='UTC') &
                             depth_m == 19 ~ NA_real_,
                           TRUE ~ .)))

ggplot(subset(PAN_do_L1_2017,
              subset = (datetime >= as.POSIXct('2017-09-01', tz='UTC') & 
                          datetime < as.POSIXct('2017-10-01', tz='UTC'))),
       aes(x=datetime, y=do_mgl, color=depth_m)) +
  geom_point() +
  labs(title='Panther Pond DO L1 Sept 2017', 
       x='date',
       y='do (mg/L)') +
  scale_color_colorblind() + 
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme 
# ggsave('figures/L1/2017/do/092017_L1.tiff', width=6, height = 4, dpi=200)



# OCT 2017 #
ggplot(subset(do_2017,
              subset = (datetime >= as.POSIXct('2017-10-01', tz='UTC') & 
                          datetime < as.POSIXct('2017-11-01', tz='UTC'))),
       aes(x=datetime, y=do_mgl, color=depth_m)) +
  geom_point() +
  labs(title='Panther Pond DO L0 Oct 2017', 
       x='date',
       y='do (mg/L)') +
  scale_color_colorblind() + 
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme 
# ggsave('figures/L0/2017/do/102017_L0.tiff', width=6, height = 4, dpi=200)

ggplot(subset(do_2017,
              subset = (datetime >= as.POSIXct('2017-10-08', tz='UTC') & 
                          datetime < as.POSIXct('2017-10-09', tz='UTC'))),
       aes(x=datetime, y=do_mgl, color=depth_m)) +
  geom_point() +
  labs(title='Panther Pond DO L0 Oct 08 2017', 
       x='date',
       y='do (mg/L)') +
  scale_color_colorblind() + 
    scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme 

PAN_do_L1_2017 <- PAN_do_L1_2017 %>% 
  mutate_at(vars(do_mgl, temp_C),
            funs(case_when(datetime >= as.POSIXct('2017-10-08 7:40', tz='UTC') &
                             datetime < as.POSIXct('2017-10-08 8:10', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))

ggplot(subset(do_2017,
              subset = (datetime >= as.POSIXct('2017-10-28', tz='UTC') & 
                          datetime < as.POSIXct('2017-10-29', tz='UTC'))),
       aes(x=datetime, y=do_mgl, color=depth_m)) +
  geom_point() +
  labs(title='Panther Pond DO L0 Oct 28 2017', 
       x='date',
       y='do (mg/L)') +
  scale_color_colorblind() + 
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme 

PAN_do_L1_2017 <- PAN_do_L1_2017 %>% 
  mutate_at(vars(do_mgl, temp_C),
            funs(case_when(datetime >= as.POSIXct('2017-10-28 12:10', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))

ggplot(subset(PAN_do_L1_2017,
              subset = (datetime >= as.POSIXct('2017-10-01', tz='UTC') & 
                          datetime < as.POSIXct('2017-11-01', tz='UTC'))),
       aes(x=datetime, y=do_mgl, color=depth_m)) +
  geom_point() +
  labs(title='Panther Pond DO L1 Oct 2017', 
       x='date',
       y='do (mg/L)') +
  scale_color_colorblind() + 
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme 
# ggsave('figures/L1/2017/do/102017_L1.tiff', width=6, height = 4, dpi=200)


PAN_do_L1_2017 <- PAN_do_L1_2017 %>% 
  mutate(do_flag = case_when(is.na(do_flag) & do_mgl < 0 ~ 'z',
                             !is.na(do_flag) & do_mgl <0 ~ paste(do_flag, 'z', sep = ', '),
                             TRUE ~ do_flag),
         do_mgl = case_when(do_mgl < 0 ~ 0,
                            TRUE ~ do_mgl))

ggplot(PAN_do_L1_2017, aes(x=datetime, y=do_mgl, color=depth_m)) +
  geom_point() +
  labs(title='Panther Pond DO L1 2017', 
       x='date',
       y='do (mg/L)') +
  scale_color_colorblind() +   
  final_theme 
# ggsave('figures/L1/2017/do/2017_L1.tiff', width=6, height = 4, dpi=200)

PAN_do_L1_2017 %>% 
  mutate(datetime = as.character(datetime)) %>% 
  select(datetime, depth_m, do_mgl, temp_C, do_flag, loggernum, fileorig) %>% 
  write.csv(., 'G:/My Drive/Panther Pond/r export/datasets/L1/2017 L1 DO Panther.csv')



#### LITTORAL DATA ####

con_0708 <- read_csv('G:/My Drive/Panther Pond/raw data/2017/littoral/temp/connolly_17jul-17aug.csv',
                     skip = 2,
                     col_names = litnames) %>% 
  mutate(loggernum = 20182394,
         location = 'connolly',
         fileorig = 'connolly_17jul-17aug.csv')

con_0810 <- read_csv('G:/My Drive/Panther Pond/raw data/2017/littoral/temp/connolly_17aug-17oct.csv',
                     skip = 2,
                     col_names = litnames) %>% 
  mutate(loggernum = 20182394,
         location = 'connolly',
         fileorig = 'connolly_17aug-17oct.csv')

rand_0708 <- read_csv('G:/My Drive/Panther Pond/raw data/2017/littoral/temp/rand_17jul-17aug.csv',
                     skip = 2,
                     col_names = litnames) %>% 
  mutate(loggernum = 20182391,
         location = 'rand',
         fileorig = 'rand_17jul-17aug.csv')

rand_0810 <- read_csv('G:/My Drive/Panther Pond/raw data/2017/littoral/temp/rand_17aug-17oct.csv',
                     skip = 2,
                     col_names = litnames) %>% 
  mutate(loggernum = 20182391,
         location = 'rand',
         fileorig = 'rand_17aug-17oct.csv')


rel_0708 <- read_csv('G:/My Drive/Panther Pond/raw data/2017/littoral/temp/relyea_17jul-17aug.csv',
                      skip = 2,
                      col_names = litnames) %>% 
  mutate(loggernum = 20182396,
         location = 'relyea',
         fileorig = 'relyea_17jul-17aug.csv')

rel_0810 <- read_csv('G:/My Drive/Panther Pond/raw data/2017/littoral/temp/relyea_17aug-17oct.csv',
                      skip = 2,
                      col_names = litnames) %>% 
  mutate(loggernum = 20182396,
         location = 'relyea',
         fileorig = 'relyea_17aug-17oct.csv')


walk_0708 <- read_csv('G:/My Drive/Panther Pond/raw data/2017/littoral/temp/walker_17jul-17aug.csv',
                     skip = 2,
                     col_names = litnames) %>% 
  mutate(loggernum = 20182395,
         location = 'walker',
         fileorig = 'walker_17jul-17aug.csv')

walk_0810 <- read_csv('G:/My Drive/Panther Pond/raw data/2017/littoral/temp/walker_17aug-17oct.csv',
                     skip = 2,
                     col_names = litnames) %>% 
  mutate(loggernum = 20182395,
         location = 'walker',
         fileorig = 'walker_17aug-17oct.csv')

wils_0708 <- read_csv('G:/My Drive/Panther Pond/raw data/2017/littoral/temp/wilson_17jul-17aug.csv',
                      skip = 2,
                      col_names = litnames2) %>% 
  mutate(loggernum = 20182392,
         location = 'wilson',
         fileorig = 'wilson_17jul-17aug.csv')

wils_0810 <- read_csv('G:/My Drive/Panther Pond/raw data/2017/littoral/temp/wilson_17aug-17oct.csv',
                      skip = 2,
                      col_names = litnames) %>% 
  mutate(loggernum = 20182392,
         location = 'wilson',
         fileorig = 'wilson_17aug-17oct.csv')


####merge all files ####
littoral_2017 <- full_join(con_0708, con_0810) %>% 
  full_join(., rand_0708) %>% 
  full_join(., rand_0810) %>% 
  full_join(., walk_0708) %>% 
  full_join(., walk_0810) %>% 
  full_join(., wils_0708) %>% 
  full_join(., wils_0810) %>% 
  full_join(., rel_0708) %>% 
  full_join(., rel_0810) %>% 
  mutate(datetime = as.POSIXct(datetime, tz='UTC', format = '%m/%d/%y %I:%M:%S %p')) %>% 
  arrange(datetime, location) %>% 
  select(-attach, -detach, -eof, -stop)

ggplot(littoral_2017, aes(x=datetime, y = temp_C)) +
  geom_point() +
  facet_grid(location ~ .) +
  labs(title='Panther Pond Littoral Sites L0 2017', 
       x=NULL,
       y='temperature (degrees C)') +
  final_theme
# ggsave('figures/L0/2017/littoral/2017_L0.tiff', width=6, height = 4, dpi=200)

#clean up workspace
rm(con_0708, con_0810, rand_0708, rand_0810, walk_0708, walk_0810, wils_0708, wils_0810,
   rel_0708, rel_0810)

littoral_2017 %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., 'datasets/L0/PantherBuoy_littoral_L0_2017.csv')


#### print and clean lit monthly ####
littoral_2017_L1 <- littoral_2017

setwd('G:/My Drive/Panther Pond/r export/')

# JULY 2017 #
ggplot(subset(littoral_2017,
              subset=(datetime >= as.POSIXct('2017-07-01', tz='UTC') & datetime < as.POSIXct('2017-08-01', tz='UTC'))),
       aes(x=datetime, y = temp_C)) +
  geom_point() +
  facet_grid(location ~ .) +
  labs(title='Panther Pond Littoral Sites L0 Jul 2017', 
       x=NULL,
       y='temperature (degrees C)') +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme
# ggsave('figures/L0/2017/littoral/072017_L0.tiff', width=6, height = 4, dpi=200)

# Jul 25 sensors out
ggplot(subset(littoral_2017,
              subset=(datetime >= as.POSIXct('2017-07-25', tz='UTC') & datetime < as.POSIXct('2017-07-26', tz='UTC'))),
       aes(x=datetime, y = temp_C)) +
  geom_point() +
  facet_grid(location ~ .) +
  labs(title='Panther Pond Littoral Sites L0 Jul 25 2017', 
       x=NULL,
       y='temperature (degrees C)') +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme

littoral_2017_L1 <- littoral_2017_L1 %>% 
  mutate_at(vars(temp_C, intens_lux),
            funs(case_when(location == 'connolly' & datetime < as.POSIXct('2017-07-25 10:00', tz='UTC') ~ NA_real_,
                           location == 'rand' & datetime < as.POSIXct('2017-07-25 9:00', tz='UTC') ~ NA_real_,
                           location == 'walker' & datetime < as.POSIXct('2017-07-25 10:30', tz='UTC') ~ NA_real_,
                           location == 'wilson' & datetime < as.POSIXct('2017-07-25 8:30', tz='UTC') ~ NA_real_,
                           location == 'relyea' & datetime < as.POSIXct('2017-07-25 10:50', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))

ggplot(subset(littoral_2017_L1,
              subset=(datetime >= as.POSIXct('2017-07-25', tz='UTC') & datetime < as.POSIXct('2017-07-26', tz='UTC'))),
       aes(x=datetime, y = temp_C)) +
  geom_point() +
  facet_grid(location ~ .) +
  labs(title='Panther Pond Littoral Sites L1 Jul 25 2017', 
       x=NULL,
       y='temperature (degrees C)') +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme

ggplot(subset(littoral_2017_L1,
              subset=(datetime >= as.POSIXct('2017-07-01', tz='UTC') & datetime < as.POSIXct('2017-08-01', tz='UTC'))),
       aes(x=datetime, y = temp_C)) +
  geom_point() +
  facet_grid(location ~ .) +
  labs(title='Panther Pond Littoral Sites L1 Jul 2017', 
       x=NULL,
       y='temperature (degrees C)') +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme
# ggsave('figures/L1/2017/littoral/072017_L1.tiff', width=6, height = 4, dpi=200)


# AUGUST 2017 #
ggplot(subset(littoral_2017,
              subset=(datetime >= as.POSIXct('2017-08-01', tz='UTC') & datetime < as.POSIXct('2017-09-01', tz='UTC'))),
       aes(x=datetime, y = temp_C)) +
  geom_point() +
  facet_grid(location ~ .) +
  labs(title='Panther Pond Littoral Sites L0 Aug 2017', 
       x=NULL,
       y='temperature (degrees C)') +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme
# ggsave('figures/L0/2017/littoral/082017_L0.tiff', width=6, height = 4, dpi=200)

# SEPTEMBER 2017 #
ggplot(subset(littoral_2017,
              subset=(datetime >= as.POSIXct('2017-09-01', tz='UTC') & datetime < as.POSIXct('2017-10-01', tz='UTC'))),
       aes(x=datetime, y = temp_C)) +
  geom_point() +
  facet_grid(location ~ .) +
  labs(title='Panther Pond Littoral Sites L0 Sept 2017', 
       x=NULL,
       y='temperature (degrees C)') +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme
# ggsave('figures/L0/2017/littoral/092017_L0.tiff', width=6, height = 4, dpi=200)

# OCT 2017 #
ggplot(subset(littoral_2017,
              subset=(datetime >= as.POSIXct('2017-10-01', tz='UTC') & datetime < as.POSIXct('2017-11-01', tz='UTC'))),
       aes(x=datetime, y = temp_C)) +
  geom_point() +
  facet_grid(location ~ .) +
  labs(title='Panther Pond Littoral Sites L0 Oct 2017', 
       x=NULL,
       y='temperature (degrees C)') +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme
# ggsave('figures/L0/2017/littoral/102017_L0.tiff', width=6, height = 4, dpi=200)


#oct 7 removal
ggplot(subset(littoral_2017,
              subset=(datetime >= as.POSIXct('2017-10-07', tz='UTC') & datetime < as.POSIXct('2017-10-08', tz='UTC'))),
       aes(x=datetime, y = temp_C)) +
  geom_point() +
  facet_grid(location ~ .) +
  labs(title='Panther Pond Littoral Sites L0 Oct 7 2017', 
       x=NULL,
       y='temperature (degrees C)') +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme

littoral_2017_L1 <- littoral_2017_L1 %>% 
  mutate_at(vars(temp_C, intens_lux),
            funs(case_when(location == 'walker' & datetime >= as.POSIXct('2017-10-07 16:00', tz='UTC') ~ NA_real_,
                            TRUE ~ .)))

ggplot(subset(littoral_2017_L1,
              subset=(datetime >= as.POSIXct('2017-10-07', tz='UTC') & datetime < as.POSIXct('2017-10-08', tz='UTC'))),
       aes(x=datetime, y = temp_C)) +
  geom_point() +
  facet_grid(location ~ .) +
  labs(title='Panther Pond Littoral Sites L1 Oct 7 2017', 
       x=NULL,
       y='temperature (degrees C)') +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme

littoral_2017_L1 <- littoral_2017_L1 %>% 
  mutate(temp_C = case_when(datetime >= as.POSIXct('2017-10-07 11:50', tz='UTC') &
                              location == 'relyea' ~ NA_real_,
                            TRUE ~ temp_C))

ggplot(subset(littoral_2017_L1,
              subset=(datetime >= as.POSIXct('2017-10-01', tz='UTC') & datetime < as.POSIXct('2017-11-01', tz='UTC'))),
       aes(x=datetime, y = temp_C)) +
  geom_point() +
  facet_grid(location ~ .) +
  labs(title='Panther Pond Littoral Sites L1 Oct 2017', 
       x=NULL,
       y='temperature (degrees C)') +
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme
# ggsave('figures/L1/2017/littoral/102017_L1.tiff', width=6, height = 4, dpi=200)

ggplot(littoral_2017_L1, aes(x=datetime, y = temp_C)) +
  geom_point() +
  facet_grid(location ~ .) +
  labs(title='Panther Pond Littoral Sites L1 2017', 
       x=NULL,
       y='temperature (degrees C)') +
  final_theme
# ggsave('figures/L1/2017/littoral/2017_L1.tiff', width=6, height = 4, dpi=200)

littoral_2017_L1 %>% 
  select(datetime, location, temp_C, intens_lux, loggernum, fileorig) %>% 
  mutate(datetime = as.character(datetime)) %>% 
  write_csv('datasets/L1/2017 L1 Panther Littoral Sites.csv')

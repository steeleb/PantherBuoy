library(tidyverse) 
library(readxl) 
library(ggthemes)

source('G:/My Drive/Panther Pond/r program/panther_buoy_input_2017_30jul2018.R')

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


#### TEMP/DO DATA ####
setwd('G:/My Drive/Panther Pond/raw data/2018/buoy/')

#0.5m
pan_0p5_18 <- read_csv('0p5meter_midseason.csv',
                       skip=2,
                       col_names = varnames2,
                       col_types = cols(.default = col_character())) %>% 
  mutate(loggernum = 10796876,
         fileorig = '0p5meter_midseason.csv',
         depth_m = 0.5)

#1m
pan_1m_18 <- read_csv('1_meter_#37.csv',
                      skip=2,
                      col_names = varnames2,
                      col_types = cols(.default = col_character())) %>% 
  mutate(loggernum = 10616537,
         fileorig = '1_meter_#37.csv',
         depth_m = 1.28)


#1.5 do
pan_1p5_18 <- read_csv('Surface_do_324.csv',
                       skip=2,
                       col_names = dovars,
                       col_types = cols(.default = col_character())) %>% 
  mutate(loggernum = 20126324,
         fileorig = 'Surface_do_324.csv',
         depth_m = 1.58)

#2m
pan_2_18 <- read_csv('2_meters_#38.csv',
                     skip=2,
                     col_names = varnames2,
                     col_types = cols(.default = col_character())) %>% 
  mutate(loggernum = 10616538,
         fileorig = '2_meters_#38.csv',
         depth_m = 2)

#4m
pan_4_18 <- read_csv('4_meters_#40.csv',
                     skip=2,
                     col_names = varnames,
                     col_types = cols(.default = col_character())) %>% 
  mutate(loggernum = 10616540,
         fileorig = '4_meters_#40.csv',
         depth_m = 4)

#6m
pan_6_18 <- read_csv('6_meters_#41.csv',
                     skip=2,
                     col_names = varnames,
                     col_types = cols(.default = col_character())) %>% 
  mutate(loggernum = 10616541,
         fileorig = '6_meters_#41.csv',
         depth_m = 6)

#8m
pan_8_18 <- read_csv('8_meters_#44.csv',
                     skip=2,
                     col_names = varnames,
                     col_types = cols(.default = col_character())) %>% 
  mutate(loggernum = 10616544,
         fileorig = '8_meters_#44.csv',
         depth_m = 8)

#10m
pan_10_18 <- read_csv('10_meters_#45.csv',
                      skip=2,
                      col_names = varnames,
                      col_types = cols(.default = col_character())) %>% 
  mutate(loggernum = 10616545,
         fileorig = '10_meters_#45.csv',
         depth_m = 10)

#15m
pan_15_18 <- read_csv('15_meters_#77.csv',
                      skip=2,
                      col_names = varnames,
                      col_types = cols(.default = col_character())) %>% 
  mutate(loggernum = 10796877,
         fileorig = '15_meters_#77.csv',
         depth_m = 15)

#19m do
pan_19_18 <- read_csv('Bottom_do_328.csv',
                      skip=2,
                      col_names = dovars,
                      col_types = cols(.default = col_character())) %>% 
  mutate(loggernum = 20126328,
         fileorig = 'Bottom_do_328.csv',
         depth_m = 19)

#20m
pan_20_18 <- read_csv('20_meters_#78.csv',
                      skip=2,
                      col_names = varnames2,
                      col_types = cols(.default = col_character())) %>% 
  mutate(loggernum = 10796878,
         fileorig = '20_meters_#78.csv',
         depth_m = 20)

#### merge all files ####
buoy_2018 <- full_join(pan_0p5_18, pan_1m_18) %>%
  full_join(., pan_1p5_18) %>% 
  full_join(., pan_2_18) %>% 
  full_join(., pan_4_18) %>% 
  full_join(., pan_6_18) %>% 
  full_join(., pan_8_18) %>% 
  full_join(., pan_10_18) %>% 
  full_join(., pan_15_18) %>% 
  full_join(., pan_19_18) %>% 
  full_join(., pan_20_18) %>% 
  arrange(datetime, depth_m) %>% 
  mutate(datetime = as.POSIXct(datetime, tz='UTC', format = '%m/%d/%y %I:%M:%S %p')) %>% 
  select(-obsno, -attach, -connect, -detach, -stop, -eof) %>% 
  gather(variable, value, -datetime, -loggernum, -fileorig, -depth_m) %>% 
  mutate(value = as.numeric(value)) %>% 
  filter(!is.na(value))


setwd('G:/My Drive/Panther Pond/r export/')

ggplot(buoy_2018, aes(x=datetime, y=value, color = as.factor(depth_m))) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title='Panther Pond L0 2018', 
       x='date',
       y= NULL) +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  final_theme 
# ggsave('figures/L0/2018/2018_L0.tiff', width=6, height = 4, dpi=200)

# buoy_2018 %>% 
#   mutate(datetime = as.character(datetime)) %>%
#   write_csv(., 'datasets/L0/PantherBuoy_L0_2018.csv')

#clean up workspace
rm(pan_0p5_18, pan_1m_18, pan_1p5_18, pan_2_18, pan_4_18, pan_6_18, 
   pan_8_18, pan_10_18, pan_15_18, pan_19_18, pan_20_18)

####print and clean monthly ####
PAN_buoy_2018_L1 <- buoy_2018

# June 2018 #
ggplot(subset(buoy_2018,
              subset=(datetime>= as.POSIXct('2018-06-01', tz='UTC') & datetime < as.POSIXct('2018-07-01', tz='UTC'))),
       aes(x=datetime, y=value, color = as.factor(depth_m))) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title='Panther Pond L0 June 2018', 
       x='date',
       y=NULL) +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme 
# ggsave('figures/L0/2018/062018_L0.tiff', width=6, height = 4, dpi=200)

#deploy jun 12
ggplot(subset(buoy_2018,
              subset=(datetime>= as.POSIXct('2018-06-12', tz='UTC') & datetime < as.POSIXct('2018-06-13', tz='UTC'))),
       aes(x=datetime, y=value, color = as.factor(depth_m))) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title='Panther Pond L0 June 12 2018', 
       x='date',
       y=NULL) +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme 

PAN_buoy_2018_L1 <- PAN_buoy_2018_L1 %>% 
  mutate(value = case_when(datetime < as.POSIXct('2018-06-12 7:20', tz='UTC') ~ NA_real_,
                           TRUE ~ value))

#deploy jun 12
ggplot(subset(PAN_buoy_2018_L1,
              subset=(datetime>= as.POSIXct('2018-06-12', tz='UTC') & datetime < as.POSIXct('2018-06-13', tz='UTC'))),
       aes(x=datetime, y=value, color = as.factor(depth_m))) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title='Panther Pond L1 June 12 2018', 
       x='date',
       y=NULL) +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme 

# June 2018 #
ggplot(subset(PAN_buoy_2018_L1,
              subset=(datetime>= as.POSIXct('2018-06-01', tz='UTC') & datetime < as.POSIXct('2018-07-01', tz='UTC'))),
       aes(x=datetime, y=value, color = as.factor(depth_m))) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title='Panther Pond L1 June 2018', 
       x='date',
       y=NULL) +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme 
# ggsave('figures/L1/2018/062018_L1.tiff', width=6, height = 4, dpi=200)


# July 2018 #
ggplot(subset(buoy_2018,
              subset=(datetime>= as.POSIXct('2018-07-01', tz='UTC') & datetime < as.POSIXct('2018-08-01', tz='UTC'))),
       aes(x=datetime, y=value, color = as.factor(depth_m))) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title='Panther Pond L0 July 2018', 
       x='date',
       y=NULL) +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme 
# ggsave('figures/L0/2018/072018_L0.tiff', width=6, height = 4, dpi=200)
# 0.5 m is wrong, need to track down when it begins to be okay, then reprint the l1 graph

#July 5
ggplot(subset(buoy_2018,
              subset=(datetime>= as.POSIXct('2018-07-05', tz='UTC') & datetime < as.POSIXct('2018-07-06', tz='UTC'))),
       aes(x=datetime, y=value, color = as.factor(depth_m))) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title='Panther Pond L0 July 2018', 
       x='date',
       y=NULL) +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme 

PAN_buoy_2018_L1 <- PAN_buoy_2018_L1 %>% 
  mutate(value = case_when(datetime == as.POSIXct('2018-07-05 8:10', tz='UTC') ~ NA_real_,
                           TRUE ~ value))

ggplot(subset(PAN_buoy_2018_L1,
              subset=(datetime>= as.POSIXct('2018-07-05', tz='UTC') & datetime < as.POSIXct('2018-07-06', tz='UTC'))),
       aes(x=datetime, y=value, color = as.factor(depth_m))) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title='Panther Pond L1 July 2018', 
       x='date',
       y=NULL) +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme 

ggplot(subset(PAN_buoy_2018_L1,
              subset=(datetime>= as.POSIXct('2018-07-01', tz='UTC') & datetime < as.POSIXct('2018-08-01', tz='UTC'))),
       aes(x=datetime, y=value, color = as.factor(depth_m))) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title='Panther Pond L1 July 2018', 
       x='date',
       y=NULL) +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme 


# August 2018 #
ggplot(subset(buoy_2018,
              subset=(datetime>= as.POSIXct('2018-08-01', tz='UTC') & datetime < as.POSIXct('2018-09-01', tz='UTC'))),
       aes(x=datetime, y=value, color = as.factor(depth_m))) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title='Panther Pond L0 August 2018', 
       x='date',
       y=NULL) +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme 
# ggsave('figures/L0/2018/082018_L0.tiff', width=6, height = 4, dpi=200)

#aug 26 0.5 deployed
ggplot(subset(buoy_2018,
              subset=(datetime>= as.POSIXct('2018-08-26', tz='UTC') & datetime < as.POSIXct('2018-08-27', tz='UTC'))),
       aes(x=datetime, y=value, color = as.factor(depth_m))) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title='Panther Pond L0 August 26 2018', 
       x='date',
       y=NULL) +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme 

PAN_buoy_2018_L1 <- PAN_buoy_2018_L1 %>% 
  mutate(value = case_when(datetime < as.POSIXct('2018-08-26 10:30', tz='UTC') & depth_m == 0.5 ~ NA_real_,
                           TRUE ~ value))

#aug 26 0.5 deployed
ggplot(subset(PAN_buoy_2018_L1,
              subset=(datetime>= as.POSIXct('2018-08-26', tz='UTC') & datetime < as.POSIXct('2018-08-27', tz='UTC'))),
       aes(x=datetime, y=value, color = as.factor(depth_m))) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title='Panther Pond L1 August 26 2018', 
       x='date',
       y=NULL) +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme 

# August 2018 #
ggplot(subset(PAN_buoy_2018_L1,
              subset=(datetime>= as.POSIXct('2018-08-01', tz='UTC') & datetime < as.POSIXct('2018-09-01', tz='UTC'))),
       aes(x=datetime, y=value, color = as.factor(depth_m))) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title='Panther Pond L1 August 2018', 
       x='date',
       y=NULL) +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme 
# ggsave('figures/L1/2018/082018_L1.tiff', width=6, height = 4, dpi=200)

# July 2018 L1 #
ggplot(subset(PAN_buoy_2018_L1,
              subset=(datetime>= as.POSIXct('2018-07-01', tz='UTC') & datetime < as.POSIXct('2018-08-01', tz='UTC'))),
       aes(x=datetime, y=value, color = as.factor(depth_m))) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title='Panther Pond L1 July 2018', 
       x='date',
       y=NULL) +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme 
# ggsave('figures/L1/2018/072018_L1.tiff', width=6, height = 4, dpi=200)

# September 2018 #
ggplot(subset(buoy_2018,
              subset=(datetime>= as.POSIXct('2018-09-01', tz='UTC') & datetime < as.POSIXct('2018-10-01', tz='UTC'))),
       aes(x=datetime, y=value, color = as.factor(depth_m))) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title='Panther Pond L0 September 2018', 
       x='date',
       y=NULL) +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme 
# ggsave('figures/L0/2018/092018_L0.tiff', width=6, height = 4, dpi=200)

# October 2018 #
ggplot(subset(buoy_2018,
              subset=(datetime>= as.POSIXct('2018-10-01', tz='UTC') & datetime < as.POSIXct('2018-11-01', tz='UTC'))),
       aes(x=datetime, y=value, color = as.factor(depth_m))) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title='Panther Pond L0 October 2018', 
       x='date',
       y=NULL) +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme 
# ggsave('figures/L0/2018/102018_L0.tiff', width=6, height = 4, dpi=200)

#buoy out oct 19
ggplot(subset(buoy_2018,
              subset=(datetime>= as.POSIXct('2018-10-19', tz='UTC') & datetime < as.POSIXct('2018-10-20', tz='UTC'))),
       aes(x=datetime, y=value, color = as.factor(depth_m))) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title='Panther Pond L0 October 19 2018', 
       x='date',
       y=NULL) +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme 

PAN_buoy_2018_L1 <- PAN_buoy_2018_L1 %>% 
  mutate(value = case_when(datetime >= as.POSIXct('2018-10-19 13:50', tz='UTC') ~ NA_real_,
                           TRUE ~ value))

ggplot(subset(PAN_buoy_2018_L1,
              subset=(datetime>= as.POSIXct('2018-10-19', tz='UTC') & datetime < as.POSIXct('2018-10-20', tz='UTC'))),
       aes(x=datetime, y=value, color = as.factor(depth_m))) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title='Panther Pond L0 October 19 2018', 
       x='date',
       y=NULL) +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme 

# October 2018 #
ggplot(subset(PAN_buoy_2018_L1,
              subset=(datetime>= as.POSIXct('2018-10-01', tz='UTC') & datetime < as.POSIXct('2018-11-01', tz='UTC'))),
       aes(x=datetime, y=value, color = as.factor(depth_m))) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title='Panther Pond L1 October 2018', 
       x='date',
       y=NULL) +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme 
# ggsave('figures/L1/2018/102018_L1.tiff', width=6, height = 4, dpi=200)



# 2018 #
ggplot(PAN_buoy_2018_L1, aes(x=datetime, y=value, color = as.factor(depth_m))) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title='Panther Pond L1 2018', 
       x='date',
       y = NULL) +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  final_theme 
# ggsave('figures/L1/2018/2018_L1.tiff', width=6, height = 4, dpi=200)

# PAN_buoy_2018_L1 %>%
#   mutate(datetime = as.character(datetime)) %>%
#   write_csv(., 'datasets/L1/2018 Panther L1 buoy data 10Jun2020.csv')
# 


 
# #### LITTORAL DATA ####
# 
# con_0708 <- read_csv('G:/My Drive/Panther Pond/raw data/2018/littoral/temp/connolly_17jul-17aug.csv',
#                      skip = 2,
#                      col_names = litnames) %>% 
#   mutate(loggernum = 20182394,
#          location = 'connolly',
#          fileorig = 'connolly_17jul-17aug.csv')
# 
# con_0810 <- read_csv('G:/My Drive/Panther Pond/raw data/2018/littoral/temp/connolly_17aug-17oct.csv',
#                      skip = 2,
#                      col_names = litnames) %>% 
#   mutate(loggernum = 20182394,
#          location = 'connolly',
#          fileorig = 'connolly_17aug-17oct.csv')
# 
# rand_0708 <- read_csv('G:/My Drive/Panther Pond/raw data/2018/littoral/temp/rand_17jul-17aug.csv',
#                      skip = 2,
#                      col_names = litnames) %>% 
#   mutate(loggernum = 20182391,
#          location = 'rand',
#          fileorig = 'rand_17jul-17aug.csv')
# 
# rand_0810 <- read_csv('G:/My Drive/Panther Pond/raw data/2018/littoral/temp/rand_17aug-17oct.csv',
#                      skip = 2,
#                      col_names = litnames) %>% 
#   mutate(loggernum = 20182391,
#          location = 'rand',
#          fileorig = 'rand_17aug-17oct.csv')
# 
# 
# rel_0708 <- read_csv('G:/My Drive/Panther Pond/raw data/2018/littoral/temp/relyea_17jul-17aug.csv',
#                       skip = 2,
#                       col_names = litnames) %>% 
#   mutate(loggernum = 20182396,
#          location = 'relyea',
#          fileorig = 'relyea_17jul-17aug.csv')
# 
# rel_0810 <- read_csv('G:/My Drive/Panther Pond/raw data/2018/littoral/temp/relyea_17aug-17oct.csv',
#                       skip = 2,
#                       col_names = litnames) %>% 
#   mutate(loggernum = 20182396,
#          location = 'relyea',
#          fileorig = 'relyea_17aug-17oct.csv')
# 
# 
# walk_0708 <- read_csv('G:/My Drive/Panther Pond/raw data/2018/littoral/temp/walker_17jul-17aug.csv',
#                      skip = 2,
#                      col_names = litnames) %>% 
#   mutate(loggernum = 20182395,
#          location = 'walker',
#          fileorig = 'walker_17jul-17aug.csv')
# 
# walk_0810 <- read_csv('G:/My Drive/Panther Pond/raw data/2018/littoral/temp/walker_17aug-17oct.csv',
#                      skip = 2,
#                      col_names = litnames) %>% 
#   mutate(loggernum = 20182395,
#          location = 'walker',
#          fileorig = 'walker_17aug-17oct.csv')
# 
# wils_0708 <- read_csv('G:/My Drive/Panther Pond/raw data/2018/littoral/temp/wilson_17jul-17aug.csv',
#                       skip = 2,
#                       col_names = litnames2) %>% 
#   mutate(loggernum = 20182392,
#          location = 'wilson',
#          fileorig = 'wilson_17jul-17aug.csv')
# 
# wils_0810 <- read_csv('G:/My Drive/Panther Pond/raw data/2018/littoral/temp/wilson_17aug-17oct.csv',
#                       skip = 2,
#                       col_names = litnames) %>% 
#   mutate(loggernum = 20182392,
#          location = 'wilson',
#          fileorig = 'wilson_17aug-17oct.csv')
# 
# 
# ####merge all files ####
# littoral_2018 <- full_join(con_0708, con_0810) %>% 
#   full_join(., rand_0708) %>% 
#   full_join(., rand_0810) %>% 
#   full_join(., walk_0708) %>% 
#   full_join(., walk_0810) %>% 
#   full_join(., wils_0708) %>% 
#   full_join(., wils_0810) %>% 
#   full_join(., rel_0708) %>% 
#   full_join(., rel_0810) %>% 
#   mutate(datetime = as.POSIXct(datetime, tz='UTC', format = '%m/%d/%y %I:%M:%S %p')) %>% 
#   arrange(datetime, location) %>% 
#   select(-attach, -detach, -eof, -stop)
# 
# ggplot(littoral_2018, aes(x=datetime, y = temp_C)) +
#   geom_point() +
#   facet_grid(location ~ .) +
#   labs(title='Panther Pond Littoral Sites L0 2018', 
#        x=NULL,
#        y='temperature (degrees C)') +
#   final_theme
# # ggsave('figures/L0/2018/littoral/2018_L0.tiff', width=6, height = 4, dpi=200)
# 
# #clean up workspace
# rm(con_0708, con_0810, rand_0708, rand_0810, walk_0708, walk_0810, wils_0708, wils_0810,
#    rel_0708, rel_0810)
# 
# littoral_2018 %>%
#   mutate(datetime = as.character(datetime)) %>%
#   write_csv(., 'datasets/L0/PantherBuoy_littoral_L0_2018.csv')
# 
# 
# #### print and clean lit monthly ####
# littoral_2018_L1 <- littoral_2018
# 
# setwd('G:/My Drive/Panther Pond/r export/')
# 
# # JULY 2018 #
# ggplot(subset(littoral_2018,
#               subset=(datetime >= as.POSIXct('2018-07-01', tz='UTC') & datetime < as.POSIXct('2018-08-01', tz='UTC'))),
#        aes(x=datetime, y = temp_C)) +
#   geom_point() +
#   facet_grid(location ~ .) +
#   labs(title='Panther Pond Littoral Sites L0 Jul 2018', 
#        x=NULL,
#        y='temperature (degrees C)') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme
# # ggsave('figures/L0/2018/littoral/072018_L0.tiff', width=6, height = 4, dpi=200)
# 
# # Jul 25 sensors out
# ggplot(subset(littoral_2018,
#               subset=(datetime >= as.POSIXct('2018-07-25', tz='UTC') & datetime < as.POSIXct('2018-07-26', tz='UTC'))),
#        aes(x=datetime, y = temp_C)) +
#   geom_point() +
#   facet_grid(location ~ .) +
#   labs(title='Panther Pond Littoral Sites L0 Jul 25 2018', 
#        x=NULL,
#        y='temperature (degrees C)') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme
# 
# littoral_2018_L1 <- littoral_2018_L1 %>% 
#   mutate_at(vars(temp_C, intens_lux),
#             funs(case_when(location == 'connolly' & datetime < as.POSIXct('2018-07-25 10:00', tz='UTC') ~ NA_real_,
#                            location == 'rand' & datetime < as.POSIXct('2018-07-25 9:00', tz='UTC') ~ NA_real_,
#                            location == 'walker' & datetime < as.POSIXct('2018-07-25 10:30', tz='UTC') ~ NA_real_,
#                            location == 'wilson' & datetime < as.POSIXct('2018-07-25 8:30', tz='UTC') ~ NA_real_,
#                            location == 'relyea' & datetime < as.POSIXct('2018-07-25 10:50', tz='UTC') ~ NA_real_,
#                            TRUE ~ .)))
# 
# ggplot(subset(littoral_2018_L1,
#               subset=(datetime >= as.POSIXct('2018-07-25', tz='UTC') & datetime < as.POSIXct('2018-07-26', tz='UTC'))),
#        aes(x=datetime, y = temp_C)) +
#   geom_point() +
#   facet_grid(location ~ .) +
#   labs(title='Panther Pond Littoral Sites L1 Jul 25 2018', 
#        x=NULL,
#        y='temperature (degrees C)') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme
# 
# ggplot(subset(littoral_2018_L1,
#               subset=(datetime >= as.POSIXct('2018-07-01', tz='UTC') & datetime < as.POSIXct('2018-08-01', tz='UTC'))),
#        aes(x=datetime, y = temp_C)) +
#   geom_point() +
#   facet_grid(location ~ .) +
#   labs(title='Panther Pond Littoral Sites L1 Jul 2018', 
#        x=NULL,
#        y='temperature (degrees C)') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme
# # ggsave('figures/L1/2018/littoral/072018_L1.tiff', width=6, height = 4, dpi=200)
# 
# 
# # AUGUST 2018 #
# ggplot(subset(littoral_2018,
#               subset=(datetime >= as.POSIXct('2018-08-01', tz='UTC') & datetime < as.POSIXct('2018-09-01', tz='UTC'))),
#        aes(x=datetime, y = temp_C)) +
#   geom_point() +
#   facet_grid(location ~ .) +
#   labs(title='Panther Pond Littoral Sites L0 Aug 2018', 
#        x=NULL,
#        y='temperature (degrees C)') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme
# # ggsave('figures/L0/2018/littoral/082018_L0.tiff', width=6, height = 4, dpi=200)
# 
# # SEPTEMBER 2018 #
# ggplot(subset(littoral_2018,
#               subset=(datetime >= as.POSIXct('2018-09-01', tz='UTC') & datetime < as.POSIXct('2018-10-01', tz='UTC'))),
#        aes(x=datetime, y = temp_C)) +
#   geom_point() +
#   facet_grid(location ~ .) +
#   labs(title='Panther Pond Littoral Sites L0 Sept 2018', 
#        x=NULL,
#        y='temperature (degrees C)') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme
# # ggsave('figures/L0/2018/littoral/092018_L0.tiff', width=6, height = 4, dpi=200)
# 
# # OCT 2018 #
# ggplot(subset(littoral_2018,
#               subset=(datetime >= as.POSIXct('2018-10-01', tz='UTC') & datetime < as.POSIXct('2018-11-01', tz='UTC'))),
#        aes(x=datetime, y = temp_C)) +
#   geom_point() +
#   facet_grid(location ~ .) +
#   labs(title='Panther Pond Littoral Sites L0 Oct 2018', 
#        x=NULL,
#        y='temperature (degrees C)') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme
# # ggsave('figures/L0/2018/littoral/102018_L0.tiff', width=6, height = 4, dpi=200)
# 
# 
# #oct 7 removal
# ggplot(subset(littoral_2018,
#               subset=(datetime >= as.POSIXct('2018-10-07', tz='UTC') & datetime < as.POSIXct('2018-10-08', tz='UTC'))),
#        aes(x=datetime, y = temp_C)) +
#   geom_point() +
#   facet_grid(location ~ .) +
#   labs(title='Panther Pond Littoral Sites L0 Oct 7 2018', 
#        x=NULL,
#        y='temperature (degrees C)') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme
# 
# littoral_2018_L1 <- littoral_2018_L1 %>% 
#   mutate_at(vars(temp_C, intens_lux),
#             funs(case_when(location == 'walker' & datetime >= as.POSIXct('2018-10-07 16:00', tz='UTC') ~ NA_real_,
#                             TRUE ~ .)))
# 
# ggplot(subset(littoral_2018_L1,
#               subset=(datetime >= as.POSIXct('2018-10-07', tz='UTC') & datetime < as.POSIXct('2018-10-08', tz='UTC'))),
#        aes(x=datetime, y = temp_C)) +
#   geom_point() +
#   facet_grid(location ~ .) +
#   labs(title='Panther Pond Littoral Sites L1 Oct 7 2018', 
#        x=NULL,
#        y='temperature (degrees C)') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme
# 
# littoral_2018_L1 <- littoral_2018_L1 %>% 
#   mutate(temp_C = case_when(datetime >= as.POSIXct('2018-10-07 11:50', tz='UTC') &
#                               location == 'relyea' ~ NA_real_,
#                             TRUE ~ temp_C))
# 
# ggplot(subset(littoral_2018_L1,
#               subset=(datetime >= as.POSIXct('2018-10-01', tz='UTC') & datetime < as.POSIXct('2018-11-01', tz='UTC'))),
#        aes(x=datetime, y = temp_C)) +
#   geom_point() +
#   facet_grid(location ~ .) +
#   labs(title='Panther Pond Littoral Sites L1 Oct 2018', 
#        x=NULL,
#        y='temperature (degrees C)') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme
# # ggsave('figures/L1/2018/littoral/102018_L1.tiff', width=6, height = 4, dpi=200)
# 
# ggplot(littoral_2018_L1, aes(x=datetime, y = temp_C)) +
#   geom_point() +
#   facet_grid(location ~ .) +
#   labs(title='Panther Pond Littoral Sites L1 2018', 
#        x=NULL,
#        y='temperature (degrees C)') +
#   final_theme
# # ggsave('figures/L1/2018/littoral/2018_L1.tiff', width=6, height = 4, dpi=200)
# 
# littoral_2018_L1 %>% 
#   select(datetime, location, temp_C, intens_lux, loggernum, fileorig) %>% 
#   mutate(datetime = as.character(datetime)) %>% 
#   write_csv('datasets/L1/2018 L1 Panther Littoral Sites.csv')

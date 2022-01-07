library(tidyverse) 
library(readxl) 
library(ggthemes)

source('G:/My Drive/Panther Pond/r program/panther_buoy_input_2018_10jun2020.R')

#save final theme for ggplot
final_theme=theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        plot.title=element_text(size=16, face='bold', hjust=0.5)) #save as a grom


#### TEMP/DO DATA ####
setwd('G:/My Drive/Panther Pond/raw data/2019/buoy/')

#0.5m
pan_0p5_19a <- read_csv('10796876-2019a.csv',
                       skip=2,
                       col_names = varnames2,
                       col_types = cols(.default = col_character())) %>% 
  mutate(loggernum = 10796876,
         fileorig = '10796876-2019a.csv',
         depth_m = 0.5)
pan_0p5_19b <- read_csv('10796876-2019b.csv',
                        skip=2,
                        col_names = varnames2,
                        col_types = cols(.default = col_character())) %>% 
  mutate(loggernum = 10796876,
         fileorig = '10796876-2019b.csv',
         depth_m = 0.5)


#1m
pan_1m_19a <- read_csv('1_meter_#37-2019a.csv',
                      skip=2,
                      col_names = varnames2,
                      col_types = cols(.default = col_character())) %>% 
  mutate(loggernum = 10616537,
         fileorig = '1_meter_#37-2019a.csv',
         depth_m = 1)
pan_1m_19b <- read_csv('1_meter_#37-2019b.csv',
                      skip=2,
                      col_names = varnames2,
                      col_types = cols(.default = col_character())) %>% 
  mutate(loggernum = 10616537,
         fileorig = '1_meter_#37-2019b.csv',
         depth_m = 1)


#1.5 do
pan_1p5_19a <- read_csv('Surface_do_324a.csv',
                       skip=2,
                       col_names = dovars,
                       col_types = cols(.default = col_character())) %>% 
  mutate(loggernum = 20126324,
         fileorig = 'Surface_do_324a.csv',
         depth_m = 1.5)
pan_1p5_19b <- read_csv('Surface_do_324b.csv',
                        skip=2,
                        col_names = dovars,
                        col_types = cols(.default = col_character())) %>% 
  mutate(loggernum = 20126324,
         fileorig = 'Surface_do_324b.csv',
         depth_m = 1.5)

#2m
pan_2_19a <- read_csv('2_meters_#38-2019a.csv',
                     skip=2,
                     col_names = varnames2,
                     col_types = cols(.default = col_character())) %>% 
  mutate(loggernum = 10616538,
         fileorig = '2_meters_#38-2019a.csv',
         depth_m = 2)
pan_2_19b <- read_csv('2_meters_#38-2019b.csv',
                      skip=2,
                      col_names = varnames2,
                      col_types = cols(.default = col_character())) %>% 
  mutate(loggernum = 10616538,
         fileorig = '2_meters_#38-2019b.csv',
         depth_m = 2)

#4m
pan_4_19a <- read_csv('4_meters_#40-2019a.csv',
                      skip=2,
                      col_names = varnames,
                      col_types = cols(.default = col_character())) %>% 
  mutate(loggernum = 10616540,
         fileorig = '4_meters_#40-2019a.csv',
         depth_m = 4)
pan_4_19b <- read_csv('4_meters_#40-2019b.csv',
                      skip=2,
                      col_names = varnames,
                      col_types = cols(.default = col_character())) %>% 
  mutate(loggernum = 10616540,
         fileorig = '4_meters_#40-2019b.csv',
         depth_m = 4)

#6m
pan_6_19a <- read_csv('6_meters_#41-2019a.csv',
                     skip=2,
                     col_names = varnames,
                     col_types = cols(.default = col_character())) %>% 
  mutate(loggernum = 10616541,
         fileorig = '6_meters_#41-2019a.csv',
         depth_m = 6)
pan_6_19b <- read_csv('6_meters_#41-2019b.csv',
                      skip=2,
                      col_names = varnames,
                      col_types = cols(.default = col_character())) %>% 
  mutate(loggernum = 10616541,
         fileorig = '6_meters_#41-2019b.csv',
         depth_m = 6)

#8m
pan_8_19a <- read_csv('8_meters_#44-2019a.csv',
                      skip=2,
                      col_names = varnames,
                      col_types = cols(.default = col_character())) %>% 
  mutate(loggernum = 10616544,
         fileorig = '8_meters_#44-2019a.csv',
         depth_m = 8)
pan_8_19b <- read_csv('8_meters_#44-2019b.csv',
                      skip=2,
                      col_names = varnames,
                      col_types = cols(.default = col_character())) %>% 
  mutate(loggernum = 10616544,
         fileorig = '8_meters_#44-2019b.csv',
         depth_m = 8)

#10m
pan_10_19a <- read_csv('10_meters_#45-2019a.csv',
                       skip=2,
                       col_names = varnames,
                       col_types = cols(.default = col_character())) %>% 
  mutate(loggernum = 10616545,
         fileorig = '10_meters_#45-2019a.csv',
         depth_m = 10)
pan_10_19b <- read_csv('10_meters_#45-2019b.csv',
                       skip=2,
                       col_names = varnames,
                       col_types = cols(.default = col_character())) %>% 
  mutate(loggernum = 10616545,
         fileorig = '10_meters_#45-2019b.csv',
         depth_m = 10)

#15m
pan_15_19a <- read_csv('15_meters_#77-2019a.csv',
                      skip=2,
                      col_names = varnames,
                      col_types = cols(.default = col_character())) %>% 
  mutate(loggernum = 10796877,
         fileorig = '15_meters_#77-2019a.csv',
         depth_m = 15)
pan_15_19b <- read_csv('15_meters_#77-2019b.csv',
                       skip=2,
                       col_names = varnames,
                       col_types = cols(.default = col_character())) %>% 
  mutate(loggernum = 10796877,
         fileorig = '15_meters_#77-2019b.csv',
         depth_m = 15)

#19m do
pan_19_19a <- read_csv('Bottom_do_328a.csv',
                      skip=2,
                      col_names = dovars,
                      col_types = cols(.default = col_character())) %>% 
  mutate(loggernum = 20126328,
         fileorig = 'Bottom_do_328a.csv',
         depth_m = 19)
pan_19_19b <- read_csv('Bottom_do_328b.csv',
                       skip=2,
                       col_names = dovars,
                       col_types = cols(.default = col_character())) %>% 
  mutate(loggernum = 20126328,
         fileorig = 'Bottom_do_328b.csv',
         depth_m = 19)

#20m
pan_20_19a <- read_csv('20_meters_#78-2019a.csv',
                      skip=2,
                      col_names = varnames2,
                      col_types = cols(.default = col_character())) %>% 
  mutate(loggernum = 10796878,
         fileorig = '20_meters_#78-2019a.csv',
         depth_m = 20)
pan_20_19b <- read_csv('20_meters_#78-2019b.csv',
                       skip=2,
                       col_names = varnames2,
                       col_types = cols(.default = col_character())) %>% 
  mutate(loggernum = 10796878,
         fileorig = '20_meters_#78-2019b.csv',
         depth_m = 20)

#### merge all files ####
buoy_2019 <- full_join(pan_0p5_19a, pan_0p5_19b) %>% 
  full_join(., pan_1m_19a) %>%
  full_join(., pan_1m_19b) %>%
  full_join(., pan_1p5_19a) %>% 
  full_join(., pan_1p5_19b) %>% 
  full_join(., pan_2_19a) %>% 
  full_join(., pan_2_19b) %>% 
  full_join(., pan_4_19a) %>% 
  full_join(., pan_4_19b) %>% 
  full_join(., pan_6_19a) %>% 
  full_join(., pan_6_19b) %>% 
  full_join(., pan_8_19a) %>% 
  full_join(., pan_8_19b) %>% 
  full_join(., pan_10_19a) %>% 
  full_join(., pan_10_19b) %>% 
  full_join(., pan_15_19a) %>% 
  full_join(., pan_15_19b) %>% 
  full_join(., pan_19_19a) %>% 
  full_join(., pan_19_19b) %>% 
  full_join(., pan_20_19a) %>% 
  full_join(., pan_20_19b) %>% 
  arrange(datetime, depth_m) %>% 
  mutate(datetime = as.POSIXct(datetime, tz='UTC', format = '%m/%d/%y %I:%M:%S %p')) %>% 
  select(-obsno, -attach, -connect, -detach, -stop, -eof) %>% 
  gather(variable, value, -datetime, -loggernum, -fileorig, -depth_m) %>% 
  mutate(value = as.numeric(value)) %>% 
  filter(!is.na(value))


setwd('G:/My Drive/Panther Pond/r export/')

ggplot(buoy_2019, aes(x=datetime, y=value, color = as.factor(depth_m))) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title='Panther Pond L0 2019', 
       x='date',
       y= NULL) +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  final_theme 
# ggsave('figures/L0/2019/2019_L0.tiff', width=6, height = 4, dpi=200)

# buoy_2019 %>%
#   mutate(datetime = as.character(datetime)) %>%
#   write_csv(., 'datasets/L0/PantherBuoy_L0_2019.csv')

#clean up workspace
rm(pan_0p5_19a, pan_0p5_19b, pan_1m_19a, pan_1m_19b, pan_1p5_19a, 
   pan_1p5_19b, pan_2_19a, pan_2_19b, pan_4_19a, pan_4_19b, 
   pan_6_19a, pan_6_19b, pan_8_19a, pan_8_19b, pan_10_19a, pan_10_19b, 
   pan_15_19a, pan_15_19b, pan_19_19a, pan_19_19b, pan_20_19a, pan_20_19b)

####print and clean monthly ####
PAN_buoy_2019_L1 <- buoy_2019

# August 2019 #
ggplot(subset(buoy_2019,
              subset=(datetime>= as.POSIXct('2019-08-01', tz='UTC') & datetime < as.POSIXct('2019-09-01', tz='UTC'))),
       aes(x=datetime, y=value, color = as.factor(depth_m))) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title='Panther Pond L0 August 2019', 
       x='date',
       y=NULL) +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme 
ggsave('figures/L0/2019/082019_L0.tiff', width=6, height = 4, dpi=200)

#aug 2 sensors deployed
ggplot(subset(buoy_2019,
              subset=(datetime>= as.POSIXct('2019-08-02', tz='UTC') & datetime < as.POSIXct('2019-08-03', tz='UTC'))),
       aes(x=datetime, y=value, color = as.factor(depth_m))) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title='Panther Pond L0 August 02 2019', 
       x='date',
       y=NULL) +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme 

PAN_buoy_2019_L1 <- PAN_buoy_2019_L1 %>% 
  mutate(value = case_when(datetime < as.POSIXct('2019-08-02 8:50', tz='UTC') ~ NA_real_,
                           TRUE ~ value))

#aug 2
ggplot(subset(PAN_buoy_2019_L1,
              subset=(datetime>= as.POSIXct('2019-08-02', tz='UTC') & datetime < as.POSIXct('2019-08-03', tz='UTC'))),
       aes(x=datetime, y=value, color = as.factor(depth_m))) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title='Panther Pond L1 August 02 2019', 
       x='date',
       y=NULL) +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme 

#Aug 18-19 errors
ggplot(subset(buoy_2019,
              subset=(datetime>= as.POSIXct('2019-08-18 12:00', tz='UTC') & datetime < as.POSIXct('2019-08-19 12:00', tz='UTC'))),
       aes(x=datetime, y=value, color = as.factor(depth_m))) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title='Panther Pond L0 August 18-19 2019', 
       x='date',
       y=NULL) +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme 

PAN_buoy_2019_L1 <- PAN_buoy_2019_L1 %>% 
  mutate(value = case_when(datetime >= as.POSIXct('2019-08-18 15:20', tz='UTC') &
                             datetime < as.POSIXct('2019-08-19 7:30', tz='UTC')~ NA_real_,
                           TRUE ~ value))

#aug 18-19
ggplot(subset(PAN_buoy_2019_L1,
              subset=(datetime>= as.POSIXct('2019-08-18 12:00', tz='UTC') & datetime < as.POSIXct('2019-08-1912:00', tz='UTC'))),
       aes(x=datetime, y=value, color = as.factor(depth_m))) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title='Panther Pond L1 August 18-19 2019', 
       x='date',
       y=NULL) +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme 

#aug 28
ggplot(subset(buoy_2019,
              subset=(datetime>= as.POSIXct('2019-08-28', tz='UTC') & datetime < as.POSIXct('2019-08-29', tz='UTC'))),
       aes(x=datetime, y=value, color = as.factor(depth_m))) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title='Panther Pond L0 August 28 2019', 
       x='date',
       y=NULL) +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme 

PAN_buoy_2019_L1 <- PAN_buoy_2019_L1 %>% 
  mutate(value = case_when(datetime >= as.POSIXct('2019-08-28 7:20', tz='UTC') &
                             datetime < as.POSIXct('2019-08-28 8:00', tz='UTC') &
                             depth_m == 8 ~ NA_real_,
                           TRUE ~ value))

#aug 28
ggplot(subset(PAN_buoy_2019_L1,
              subset=(datetime>= as.POSIXct('2019-08-28', tz='UTC') & datetime < as.POSIXct('2019-08-29', tz='UTC'))),
       aes(x=datetime, y=value, color = as.factor(depth_m))) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title='Panther Pond L1 August 28 2019', 
       x='date',
       y=NULL) +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme 

# August 2019 #
ggplot(subset(PAN_buoy_2019_L1,
              subset=(datetime>= as.POSIXct('2019-08-01', tz='UTC') & datetime < as.POSIXct('2019-09-01', tz='UTC'))),
       aes(x=datetime, y=value, color = as.factor(depth_m))) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title='Panther Pond L1 August 2019', 
       x='date',
       y=NULL) +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme 
# ggsave('figures/L1/2019/082019_L1.tiff', width=6, height = 4, dpi=200)


# September 2019 #
ggplot(subset(buoy_2019,
              subset=(datetime>= as.POSIXct('2019-09-01', tz='UTC') & datetime < as.POSIXct('2019-10-01', tz='UTC'))),
       aes(x=datetime, y=value, color = as.factor(depth_m))) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title='Panther Pond L0 September 2019', 
       x='date',
       y=NULL) +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme 
# ggsave('figures/L0/2019/092019_L0.tiff', width=6, height = 4, dpi=200)

#sept 14
ggplot(subset(buoy_2019,
              subset=(datetime>= as.POSIXct('2019-09-14', tz='UTC') & datetime < as.POSIXct('2019-09-15', tz='UTC'))),
       aes(x=datetime, y=value, color = as.factor(depth_m))) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title='Panther Pond L0 September 2019', 
       x='date',
       y=NULL) +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme 

PAN_buoy_2019_L1 <- PAN_buoy_2019_L1 %>% 
  mutate(value = case_when(datetime >= as.POSIXct('2019-09-14 5:00', tz='UTC') &
                             datetime < as.POSIXct('2019-09-14 5:50', tz='UTC') &
                             depth_m == 2 ~ NA_real_,
                           TRUE ~ value))
ggplot(subset(PAN_buoy_2019_L1,
              subset=(datetime>= as.POSIXct('2019-09-14', tz='UTC') & datetime < as.POSIXct('2019-09-15', tz='UTC'))),
       aes(x=datetime, y=value, color = as.factor(depth_m))) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title='Panther Pond L0 September 2019', 
       x='date',
       y=NULL) +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme 

ggplot(subset(PAN_buoy_2019_L1,
              subset=(datetime>= as.POSIXct('2019-09-01', tz='UTC') & datetime < as.POSIXct('2019-10-01', tz='UTC'))),
       aes(x=datetime, y=value, color = as.factor(depth_m))) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title='Panther Pond L1 September 2019', 
       x='date',
       y=NULL) +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme 
# ggsave('figures/L1/2019/092019_L1.tiff', width=6, height = 4, dpi=200)


# October 2019 #
ggplot(subset(buoy_2019,
              subset=(datetime>= as.POSIXct('2019-10-01', tz='UTC') & datetime < as.POSIXct('2019-11-01', tz='UTC'))),
       aes(x=datetime, y=value, color = as.factor(depth_m))) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title='Panther Pond L0 October 2019', 
       x='date',
       y=NULL) +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme 
# ggsave('figures/L0/2019/102019_L0.tiff', width=6, height = 4, dpi=200)

#oct 11
ggplot(subset(PAN_buoy_2019_L1,
              subset=(datetime>= as.POSIXct('2019-10-11', tz='UTC') & 
                        datetime < as.POSIXct('2019-10-12', tz='UTC'))),
       aes(x=datetime, y=value, color = as.factor(depth_m))) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title='Panther Pond L0 October 11 2019', 
       x='date',
       y=NULL) +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme 

PAN_buoy_2019_L1 <- PAN_buoy_2019_L1 %>% 
  mutate(value = case_when(datetime >= as.POSIXct('2019-10-11 2:20', tz='UTC') &
                             datetime < as.POSIXct('2019-10-11 3:00', tz='UTC') &
                             depth_m == 4 ~ NA_real_,
                           TRUE ~ value))

ggplot(subset(PAN_buoy_2019_L1,
              subset=(datetime>= as.POSIXct('2019-10-11', tz='UTC') & 
                        datetime < as.POSIXct('2019-10-12', tz='UTC'))),
       aes(x=datetime, y=value, color = as.factor(depth_m))) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title='Panther Pond L1 October 11 2019', 
       x='date',
       y=NULL) +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme 

#buoy out oct 20
ggplot(subset(buoy_2019,
              subset=(datetime>= as.POSIXct('2019-10-20', tz='UTC') & 
                        datetime < as.POSIXct('2019-10-21', tz='UTC'))),
       aes(x=datetime, y=value, color = as.factor(depth_m))) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title='Panther Pond L0 October 19 2019', 
       x='date',
       y=NULL) +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme 

PAN_buoy_2019_L1 <- PAN_buoy_2019_L1 %>% 
  mutate(value = case_when(datetime >= as.POSIXct('2019-10-20 10:30', tz='UTC') ~ NA_real_,
                           TRUE ~ value))

ggplot(subset(PAN_buoy_2019_L1,
              subset=(datetime>= as.POSIXct('2019-10-20', tz='UTC') & datetime < as.POSIXct('2019-10-21', tz='UTC'))),
       aes(x=datetime, y=value, color = as.factor(depth_m))) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title='Panther Pond L0 October 20 2019', 
       x='date',
       y=NULL) +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme 

# October 2019 #
ggplot(subset(PAN_buoy_2019_L1,
              subset=(datetime>= as.POSIXct('2019-10-01', tz='UTC') & datetime < as.POSIXct('2019-11-01', tz='UTC'))),
       aes(x=datetime, y=value, color = as.factor(depth_m))) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title='Panther Pond L1 October 2019', 
       x='date',
       y=NULL) +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme 
# ggsave('figures/L1/2019/102019_L1.tiff', width=6, height = 4, dpi=200)



# 2019 #
ggplot(PAN_buoy_2019_L1, aes(x=datetime, y=value, color = as.factor(depth_m))) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title='Panther Pond L1 2019', 
       x='date',
       y = NULL) +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  final_theme 
# ggsave('figures/L1/2019/2019_L1.tiff', width=6, height = 4, dpi=200)

PAN_buoy_2019_L1 %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., 'datasets/L1/2019 Panther L1 buoy data 10Jun2020.csv')



# #### LITTORAL DATA ####
# 
# con_0708 <- read_csv('G:/My Drive/Panther Pond/raw data/2019/littoral/temp/connolly_17jul-17aug.csv',
#                      skip = 2,
#                      col_names = litnames) %>% 
#   mutate(loggernum = 20192394,
#          location = 'connolly',
#          fileorig = 'connolly_17jul-17aug.csv')
# 
# con_0810 <- read_csv('G:/My Drive/Panther Pond/raw data/2019/littoral/temp/connolly_17aug-17oct.csv',
#                      skip = 2,
#                      col_names = litnames) %>% 
#   mutate(loggernum = 20192394,
#          location = 'connolly',
#          fileorig = 'connolly_17aug-17oct.csv')
# 
# rand_0708 <- read_csv('G:/My Drive/Panther Pond/raw data/2019/littoral/temp/rand_17jul-17aug.csv',
#                      skip = 2,
#                      col_names = litnames) %>% 
#   mutate(loggernum = 20192391,
#          location = 'rand',
#          fileorig = 'rand_17jul-17aug.csv')
# 
# rand_0810 <- read_csv('G:/My Drive/Panther Pond/raw data/2019/littoral/temp/rand_17aug-17oct.csv',
#                      skip = 2,
#                      col_names = litnames) %>% 
#   mutate(loggernum = 20192391,
#          location = 'rand',
#          fileorig = 'rand_17aug-17oct.csv')
# 
# 
# rel_0708 <- read_csv('G:/My Drive/Panther Pond/raw data/2019/littoral/temp/relyea_17jul-17aug.csv',
#                       skip = 2,
#                       col_names = litnames) %>% 
#   mutate(loggernum = 20192396,
#          location = 'relyea',
#          fileorig = 'relyea_17jul-17aug.csv')
# 
# rel_0810 <- read_csv('G:/My Drive/Panther Pond/raw data/2019/littoral/temp/relyea_17aug-17oct.csv',
#                       skip = 2,
#                       col_names = litnames) %>% 
#   mutate(loggernum = 20192396,
#          location = 'relyea',
#          fileorig = 'relyea_17aug-17oct.csv')
# 
# 
# walk_0708 <- read_csv('G:/My Drive/Panther Pond/raw data/2019/littoral/temp/walker_17jul-17aug.csv',
#                      skip = 2,
#                      col_names = litnames) %>% 
#   mutate(loggernum = 20192395,
#          location = 'walker',
#          fileorig = 'walker_17jul-17aug.csv')
# 
# walk_0810 <- read_csv('G:/My Drive/Panther Pond/raw data/2019/littoral/temp/walker_17aug-17oct.csv',
#                      skip = 2,
#                      col_names = litnames) %>% 
#   mutate(loggernum = 20192395,
#          location = 'walker',
#          fileorig = 'walker_17aug-17oct.csv')
# 
# wils_0708 <- read_csv('G:/My Drive/Panther Pond/raw data/2019/littoral/temp/wilson_17jul-17aug.csv',
#                       skip = 2,
#                       col_names = litnames2) %>% 
#   mutate(loggernum = 20192392,
#          location = 'wilson',
#          fileorig = 'wilson_17jul-17aug.csv')
# 
# wils_0810 <- read_csv('G:/My Drive/Panther Pond/raw data/2019/littoral/temp/wilson_17aug-17oct.csv',
#                       skip = 2,
#                       col_names = litnames) %>% 
#   mutate(loggernum = 20192392,
#          location = 'wilson',
#          fileorig = 'wilson_17aug-17oct.csv')
# 
# 
# ####merge all files ####
# littoral_2019 <- full_join(con_0708, con_0810) %>% 
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
# ggplot(littoral_2019, aes(x=datetime, y = temp_C)) +
#   geom_point() +
#   facet_grid(location ~ .) +
#   labs(title='Panther Pond Littoral Sites L0 2019', 
#        x=NULL,
#        y='temperature (degrees C)') +
#   final_theme
# # ggsave('figures/L0/2019/littoral/2019_L0.tiff', width=6, height = 4, dpi=200)
# 
# #clean up workspace
# rm(con_0708, con_0810, rand_0708, rand_0810, walk_0708, walk_0810, wils_0708, wils_0810,
#    rel_0708, rel_0810)
# 
# littoral_2019 %>%
#   mutate(datetime = as.character(datetime)) %>%
#   write_csv(., 'datasets/L0/PantherBuoy_littoral_L0_2019.csv')
# 
# 
# #### print and clean lit monthly ####
# littoral_2019_L1 <- littoral_2019
# 
# setwd('G:/My Drive/Panther Pond/r export/')
# 
# # JULY 2019 #
# ggplot(subset(littoral_2019,
#               subset=(datetime >= as.POSIXct('2019-07-01', tz='UTC') & datetime < as.POSIXct('2019-08-01', tz='UTC'))),
#        aes(x=datetime, y = temp_C)) +
#   geom_point() +
#   facet_grid(location ~ .) +
#   labs(title='Panther Pond Littoral Sites L0 Jul 2019', 
#        x=NULL,
#        y='temperature (degrees C)') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme
# # ggsave('figures/L0/2019/littoral/072019_L0.tiff', width=6, height = 4, dpi=200)
# 
# # Jul 25 sensors out
# ggplot(subset(littoral_2019,
#               subset=(datetime >= as.POSIXct('2019-07-25', tz='UTC') & datetime < as.POSIXct('2019-07-26', tz='UTC'))),
#        aes(x=datetime, y = temp_C)) +
#   geom_point() +
#   facet_grid(location ~ .) +
#   labs(title='Panther Pond Littoral Sites L0 Jul 25 2019', 
#        x=NULL,
#        y='temperature (degrees C)') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme
# 
# littoral_2019_L1 <- littoral_2019_L1 %>% 
#   mutate_at(vars(temp_C, intens_lux),
#             funs(case_when(location == 'connolly' & datetime < as.POSIXct('2019-07-25 10:00', tz='UTC') ~ NA_real_,
#                            location == 'rand' & datetime < as.POSIXct('2019-07-25 9:00', tz='UTC') ~ NA_real_,
#                            location == 'walker' & datetime < as.POSIXct('2019-07-25 10:30', tz='UTC') ~ NA_real_,
#                            location == 'wilson' & datetime < as.POSIXct('2019-07-25 8:30', tz='UTC') ~ NA_real_,
#                            location == 'relyea' & datetime < as.POSIXct('2019-07-25 10:50', tz='UTC') ~ NA_real_,
#                            TRUE ~ .)))
# 
# ggplot(subset(littoral_2019_L1,
#               subset=(datetime >= as.POSIXct('2019-07-25', tz='UTC') & datetime < as.POSIXct('2019-07-26', tz='UTC'))),
#        aes(x=datetime, y = temp_C)) +
#   geom_point() +
#   facet_grid(location ~ .) +
#   labs(title='Panther Pond Littoral Sites L1 Jul 25 2019', 
#        x=NULL,
#        y='temperature (degrees C)') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme
# 
# ggplot(subset(littoral_2019_L1,
#               subset=(datetime >= as.POSIXct('2019-07-01', tz='UTC') & datetime < as.POSIXct('2019-08-01', tz='UTC'))),
#        aes(x=datetime, y = temp_C)) +
#   geom_point() +
#   facet_grid(location ~ .) +
#   labs(title='Panther Pond Littoral Sites L1 Jul 2019', 
#        x=NULL,
#        y='temperature (degrees C)') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme
# # ggsave('figures/L1/2019/littoral/072019_L1.tiff', width=6, height = 4, dpi=200)
# 
# 
# # AUGUST 2019 #
# ggplot(subset(littoral_2019,
#               subset=(datetime >= as.POSIXct('2019-08-01', tz='UTC') & datetime < as.POSIXct('2019-09-01', tz='UTC'))),
#        aes(x=datetime, y = temp_C)) +
#   geom_point() +
#   facet_grid(location ~ .) +
#   labs(title='Panther Pond Littoral Sites L0 Aug 2019', 
#        x=NULL,
#        y='temperature (degrees C)') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme
# # ggsave('figures/L0/2019/littoral/082019_L0.tiff', width=6, height = 4, dpi=200)
# 
# # SEPTEMBER 2019 #
# ggplot(subset(littoral_2019,
#               subset=(datetime >= as.POSIXct('2019-09-01', tz='UTC') & datetime < as.POSIXct('2019-10-01', tz='UTC'))),
#        aes(x=datetime, y = temp_C)) +
#   geom_point() +
#   facet_grid(location ~ .) +
#   labs(title='Panther Pond Littoral Sites L0 Sept 2019', 
#        x=NULL,
#        y='temperature (degrees C)') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme
# # ggsave('figures/L0/2019/littoral/092019_L0.tiff', width=6, height = 4, dpi=200)
# 
# # OCT 2019 #
# ggplot(subset(littoral_2019,
#               subset=(datetime >= as.POSIXct('2019-10-01', tz='UTC') & datetime < as.POSIXct('2019-11-01', tz='UTC'))),
#        aes(x=datetime, y = temp_C)) +
#   geom_point() +
#   facet_grid(location ~ .) +
#   labs(title='Panther Pond Littoral Sites L0 Oct 2019', 
#        x=NULL,
#        y='temperature (degrees C)') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme
# # ggsave('figures/L0/2019/littoral/102019_L0.tiff', width=6, height = 4, dpi=200)
# 
# 
# #oct 7 removal
# ggplot(subset(littoral_2019,
#               subset=(datetime >= as.POSIXct('2019-10-07', tz='UTC') & datetime < as.POSIXct('2019-10-08', tz='UTC'))),
#        aes(x=datetime, y = temp_C)) +
#   geom_point() +
#   facet_grid(location ~ .) +
#   labs(title='Panther Pond Littoral Sites L0 Oct 7 2019', 
#        x=NULL,
#        y='temperature (degrees C)') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme
# 
# littoral_2019_L1 <- littoral_2019_L1 %>% 
#   mutate_at(vars(temp_C, intens_lux),
#             funs(case_when(location == 'walker' & datetime >= as.POSIXct('2019-10-07 16:00', tz='UTC') ~ NA_real_,
#                             TRUE ~ .)))
# 
# ggplot(subset(littoral_2019_L1,
#               subset=(datetime >= as.POSIXct('2019-10-07', tz='UTC') & datetime < as.POSIXct('2019-10-08', tz='UTC'))),
#        aes(x=datetime, y = temp_C)) +
#   geom_point() +
#   facet_grid(location ~ .) +
#   labs(title='Panther Pond Littoral Sites L1 Oct 7 2019', 
#        x=NULL,
#        y='temperature (degrees C)') +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   final_theme
# 
# littoral_2019_L1 <- littoral_2019_L1 %>% 
#   mutate(temp_C = case_when(datetime >= as.POSIXct('2019-10-07 11:50', tz='UTC') &
#                               location == 'relyea' ~ NA_real_,
#                             TRUE ~ temp_C))
# 
# ggplot(subset(littoral_2019_L1,
#               subset=(datetime >= as.POSIXct('2019-10-01', tz='UTC') & datetime < as.POSIXct('2019-11-01', tz='UTC'))),
#        aes(x=datetime, y = temp_C)) +
#   geom_point() +
#   facet_grid(location ~ .) +
#   labs(title='Panther Pond Littoral Sites L1 Oct 2019', 
#        x=NULL,
#        y='temperature (degrees C)') +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   final_theme
# # ggsave('figures/L1/2019/littoral/102019_L1.tiff', width=6, height = 4, dpi=200)
# 
# ggplot(littoral_2019_L1, aes(x=datetime, y = temp_C)) +
#   geom_point() +
#   facet_grid(location ~ .) +
#   labs(title='Panther Pond Littoral Sites L1 2019', 
#        x=NULL,
#        y='temperature (degrees C)') +
#   final_theme
# # ggsave('figures/L1/2019/littoral/2019_L1.tiff', width=6, height = 4, dpi=200)
# 
# littoral_2019_L1 %>% 
#   select(datetime, location, temp_C, intens_lux, loggernum, fileorig) %>% 
#   mutate(datetime = as.character(datetime)) %>% 
#   write_csv('datasets/L1/2019 L1 Panther Littoral Sites.csv')

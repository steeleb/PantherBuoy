#this script collates the panther buoy data for 2020

# file created August 2021
# code by B. Steele (steeleb@caryinstitute.org / bsteele@bates.edu)

# store version date
v_date <- as.character(Sys.Date())

library(tidyverse) 
library(readxl) 
library(ggthemes)

#point to directories
raw_dir <- 'C:/Users/steeleb/Dropbox/PantherBuoy/data/raw/2020/csv/'
L0_dir <- 'C:/Users/steeleb/Dropbox/PantherBuoy/data/L0/'
L0_figdir <- 'C:/Users/steeleb/Dropbox/PantherBuoy/r-export/figures/L0/2020/'
L1_figdir <- 'C:/Users/steeleb/Dropbox/PantherBuoy/r-export/figures/L1/2020/'
L1_dir <- 'C:/Users/steeleb/Dropbox/PantherBuoy/data/L1/'

#save final theme for ggplot
final_theme=theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        plot.title=element_text(size=16, face='bold', hjust=0.5)) #save as a grom

#save varnames
varnames = c('obsno', 'datetime', 'temp_C', 'intens_lux', 'attach', 'stop', 'eof')
varnames2 = c('obsno', 'datetime', 'temp_C', 'intens_lux', 'attach', 'detach', 'connect', 'eof')
varnames3 = c('obsno', 'datetime', 'temp_C', 'intens_lux', 'eof')
varnames4 = c('obsno', 'datetime', 'temp_C', 'intens_lux', 'attach', 'connect', 'eof')
dovarnames = c('obsno', 'datetime', 'do_mgl', 'temp_C', 'do_adj_mgl', 'do_percsat', 'pressure_kpa', 'attach', 'stop', 'eof')
dovarnames2 = c('obsno', 'datetime', 'do_mgl', 'temp_C', 'do_adj_mgl', 'do_percsat', 'pressure_kpa', 'error', 'attach', 'stop', 'eof')

#### TEMP/DO DATA ####

#0.5m
pan_0p5_20a <- read_csv(file.path(raw_dir, '10796876-2020a.csv'),
                       skip=2,
                       col_names = varnames,
                       col_types = cols(.default = col_character())) %>% 
  mutate(loggernum = 10796876,
         fileorig = '10796876-2020a.csv',
         depth_m = 0.5)

pan_0p5_20b <- read_csv(file.path(raw_dir, '10796876-2020b.csv'),
                        skip=2,
                        col_names = varnames2,
                        col_types = cols(.default = col_character())) %>% 
  mutate(loggernum = 10796876,
         fileorig = '10796876-2020b.csv',
         depth_m = 0.5)


#1m
pan_1_20a <- read_csv(file.path(raw_dir, '1_meter_#37-2020a.csv'),
                      skip=2,
                      col_names = varnames,
                      col_types = cols(.default = col_character())) %>% 
  mutate(loggernum = 10616537,
         fileorig = '1_meter_#37-2020a.csv',
         depth_m = 1)
pan_1_20b <- read_csv(file.path(raw_dir, '1_meter_#37-2020b.csv'),
                      skip=2,
                      col_names = varnames2,
                      col_types = cols(.default = col_character())) %>% 
  mutate(loggernum = 10616537,
         fileorig = '1_meter_#37-2020b.csv',
         depth_m = 1)


#1.5 do
pan_1p5_20a <- read_csv(file.path(raw_dir, 'Surface_do_324_2020a.csv'),
                       skip=2,
                       col_names = dovarnames,
                       col_types = cols(.default = col_character())) %>% 
  mutate(loggernum = 20126324,
         fileorig = 'Surface_do_324_2020a.csv',
         depth_m = 1.5)
pan_1p5_20b <- read_csv(file.path(raw_dir, 'Surface_do_324_2020b.csv'),
                        skip=2,
                        col_names = dovarnames2,
                        col_types = cols(.default = col_character())) %>% 
  mutate(loggernum = 20126324,
         fileorig = 'Surface_do_324_2020b.csv',
         depth_m = 1.5)

#2m
pan_2_20a <- read_csv(file.path(raw_dir, '2_meters_#38-2020a.csv'),
                     skip=2,
                     col_names = varnames,
                     col_types = cols(.default = col_character())) %>% 
  mutate(loggernum = 10616538,
         fileorig = '2_meters_#38-2020a.csv',
         depth_m = 2)
pan_2_20b <- read_csv(file.path(raw_dir, '2_meters_#38-2020b.csv'),
                      skip=2,
                      col_names = varnames2,
                      col_types = cols(.default = col_character())) %>% 
  mutate(loggernum = 10616538,
         fileorig = '2_meters_#38-2020b.csv',
         depth_m = 2)

#4m
pan_4_20a <- read_csv(file.path(raw_dir, '4_meters_#40-2020a.csv'),
                      skip=2,
                      col_names = varnames,
                      col_types = cols(.default = col_character())) %>% 
  mutate(loggernum = 10616540,
         fileorig = '4_meters_#40-2020a.csv',
         depth_m = 4)
pan_4_20b <- read_csv(file.path(raw_dir, '4_meters_#40-2020b.csv'),
                      skip=2,
                      col_names = varnames2,
                      col_types = cols(.default = col_character())) %>% 
  mutate(loggernum = 10616540,
         fileorig = '4_meters_#40-2020b.csv',
         depth_m = 4)

#6m
pan_6_20a <- read_csv(file.path(raw_dir, '6_meters_#41-2020a.csv'),
                     skip=2,
                     col_names = varnames,
                     col_types = cols(.default = col_character())) %>% 
  mutate(loggernum = 10616541,
         fileorig = '6_meters_#41-2020a.csv',
         depth_m = 6)
pan_6_20b <- read_csv(file.path(raw_dir, '6_meters_#41-2020b.csv'),
                      skip=2,
                      col_names = varnames3,
                      col_types = cols(.default = col_character())) %>% 
  mutate(loggernum = 10616541,
         fileorig = '6_meters_#41-2020b.csv',
         depth_m = 6)

#8m
pan_8_20a <- read_csv(file.path(raw_dir, '8_meters_#44-2020a.csv'),
                      skip=2,
                      col_names = varnames,
                      col_types = cols(.default = col_character())) %>% 
  mutate(loggernum = 10616544,
         fileorig = '8_meters_#44-2020a.csv',
         depth_m = 8)
pan_8_20b <- read_csv(file.path(raw_dir, '8_meters_#44-2020b.csv'),
                      skip=2,
                      col_names = varnames2,
                      col_types = cols(.default = col_character())) %>% 
  mutate(loggernum = 10616544,
         fileorig = '8_meters_#44-2020b.csv',
         depth_m = 8)

#10m
pan_10_20a <- read_csv(file.path(raw_dir, '10_meters_#45-2020a.csv'),
                       skip=2,
                       col_names = varnames,
                       col_types = cols(.default = col_character())) %>% 
  mutate(loggernum = 10616545,
         fileorig = '10_meters_#45-2020a.csv',
         depth_m = 10)
pan_10_20b <- read_csv(file.path(raw_dir, '10_meters_#45-2020b.csv'),
                       skip=2,
                       col_names = varnames2,
                       col_types = cols(.default = col_character())) %>% 
  mutate(loggernum = 10616545,
         fileorig = '10_meters_#45-2020b.csv',
         depth_m = 10)

#15m
pan_15_20a <- read_csv(file.path(raw_dir, '15_meters_#77-2020a.csv'),
                      skip=2,
                      col_names = varnames,
                      col_types = cols(.default = col_character())) %>% 
  mutate(loggernum = 10796877,
         fileorig = '15_meters_#77-2020a.csv',
         depth_m = 15)
pan_15_20b <- read_csv(file.path(raw_dir, '15_meters_#77-2020b.csv'),
                       skip=2,
                       col_names = varnames4,
                       col_types = cols(.default = col_character())) %>% 
  mutate(loggernum = 10796877,
         fileorig = '15_meters_#77-2020b.csv',
         depth_m = 15)

#19m do
pan_19_20a <- read_csv(file.path(raw_dir, 'Bottom_do_328_2020a.csv'),
                      skip=2,
                      col_names = dovarnames,
                      col_types = cols(.default = col_character())) %>% 
  mutate(loggernum = 20126328,
         fileorig = 'Bottom_do_328_2020a.csv',
         depth_m = 19)
pan_19_20b <- read_csv(file.path(raw_dir, 'Bottom_do_328_2020b.csv'),
                       skip=2,
                       col_names = dovarnames,
                       col_types = cols(.default = col_character())) %>% 
  mutate(loggernum = 20126328,
         fileorig = 'Bottom_do_328_2020b.csv',
         depth_m = 19)

#20m
pan_20_20a <- read_csv(file.path(raw_dir, '20_meters_#78-2020a.csv'),
                      skip=2,
                      col_names = varnames,
                      col_types = cols(.default = col_character())) %>% 
  mutate(loggernum = 10796878,
         fileorig = '20_meters_#78-2020a.csv',
         depth_m = 20)
pan_20_20b <- read_csv(file.path(raw_dir, '20_meters_#78-2020b.csv'),
                       skip=2,
                       col_names = varnames2,
                       col_types = cols(.default = col_character())) %>% 
  mutate(loggernum = 10796878,
         fileorig = '20_meters_#78-2020b.csv',
         depth_m = 20)

#### merge all files ####
buoy_2020 <- full_join(pan_0p5_20a, pan_0p5_20b) %>% 
  full_join(., pan_1_20a) %>%
  full_join(., pan_1_20b) %>%
  full_join(., pan_1p5_20a) %>% 
  full_join(., pan_1p5_20b) %>% 
  full_join(., pan_2_20a) %>% 
  full_join(., pan_2_20b) %>% 
  full_join(., pan_4_20a) %>% 
  full_join(., pan_4_20b) %>% 
  full_join(., pan_6_20a) %>% 
  full_join(., pan_6_20b) %>% 
  full_join(., pan_8_20a) %>% 
  full_join(., pan_8_20b) %>% 
  full_join(., pan_10_20a) %>% 
  full_join(., pan_10_20b) %>% 
  full_join(., pan_15_20a) %>% 
  full_join(., pan_15_20b) %>% 
  full_join(., pan_19_20a) %>% 
  full_join(., pan_19_20b) %>% 
  full_join(., pan_20_20a) %>% 
  full_join(., pan_20_20b) %>% 
  arrange(datetime, depth_m) %>% 
  mutate(datetime = as.POSIXct(datetime, tz='UTC', format = '%m/%d/%y %I:%M:%S %p'))

colnames(buoy_2020)

#pivot to vertical
buoy_2020_vert <- buoy_2020 %>% 
  pivot_longer(cols = c(temp_C, intens_lux, do_adj_mgl, do_percsat, do_mgl), names_to = 'variable', values_to = 'value') %>% 
  mutate(value = as.numeric(value)) %>% 
  filter(!is.na(value))

#write csv of L0 buoy data
buoy_2020 %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., file.path(L0_dir, paste0('PantherBuoy_L0_2020_v', v_date, '.csv')))


ggplot(buoy_2020_vert, aes(x=datetime, y=value, color = as.factor(depth_m))) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title='Panther Pond L0 2020', 
       x='date',
       y= NULL) +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  final_theme 
ggsave(file.path(L0_figdir, '2020_L0.jpg'), width=12, height = 8, dpi=300)

#clean up workspace
rm(pan_0p5_20a, pan_0p5_20b, pan_1_20a, pan_1_20b, pan_1p5_20a, 
   pan_1p5_20b, pan_2_20a, pan_2_20b, pan_4_20a, pan_4_20b, 
   pan_6_20a, pan_6_20b, pan_8_20a, pan_8_20b, pan_10_20a, pan_10_20b, 
   pan_15_20a, pan_15_20b, pan_19_20a, pan_19_20b, pan_20_20a, pan_20_20b)

#### remove obviously wrong temp data in 1.5m sensor ####
range(buoy_2020_vert$value)
PAN_buoy_2020_L1_vert <- buoy_2020_vert %>% 
  select(-c(obsno, attach, stop, eof, detach, connect, error)) %>% 
  mutate(value = case_when(value < -5 ~ NA_real_,
                           TRUE ~ value)) %>% 
  filter(!is.na(value))

range(PAN_buoy_2020_L1_vert$value)

ggplot(PAN_buoy_2020_L1_vert, aes(x=datetime, y=value, color = as.factor(depth_m))) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title='Panther Pond L0 2020', 
       x='date',
       y= NULL) +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  final_theme 
ggsave(file.path(L0_figdir, '2020_L0p5.jpg'), width=12, height = 8, dpi=300)


####print and clean monthly ####

# April 2020 #
ggplot(subset(PAN_buoy_2020_L1_vert,
              subset=(datetime>= as.POSIXct('2020-04-01', tz='UTC') & datetime < as.POSIXct('2020-05-01', tz='UTC'))),
       aes(x=datetime, y=value, color = as.factor(depth_m))) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title='Panther Pond L0 April 2020', 
       x='date',
       y=NULL) +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme 
ggsave(file.path(L0_figdir, '042020_L0.jpg'), width=8, height = 6, dpi=200)

# deployment on April 23
ggplot(subset(PAN_buoy_2020_L1_vert,
              subset=(datetime>= as.POSIXct('2020-04-23', tz='UTC') & datetime < as.POSIXct('2020-04-24', tz='UTC'))),
       aes(x=datetime, y=value, color = as.factor(depth_m))) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title='Panther Pond L0 April 23 2020', 
       x='date',
       y=NULL) +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme 

PAN_buoy_2020_L1_vert <- PAN_buoy_2020_L1_vert %>% 
  mutate(value = case_when(datetime < as.POSIXct('2020-04-23 18:00', tz='UTC') ~ NA_real_,
                           TRUE ~ value))

# might need to recode the pct sat data - the lower sensor seems quite wrong.

# April 24
ggplot(subset(PAN_buoy_2020_L1_vert,
              subset=(datetime>= as.POSIXct('2020-04-24', tz='UTC') & datetime < as.POSIXct('2020-04-25', tz='UTC'))),
       aes(x=datetime, y=value, color = as.factor(depth_m))) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title='Panther Pond L0 April 23 2020', 
       x='date',
       y=NULL) +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme 

PAN_buoy_2020_L1_vert <- PAN_buoy_2020_L1_vert %>% 
  mutate(value = case_when(datetime >= as.POSIXct('2020-04-24 11:30', tz='UTC') & 
                             datetime < as.POSIXct('2020-04-24 11:50', tz='UTC') ~ NA_real_,
                           datetime < as.POSIXct('2020-04-24 11:50', tz='UTC') &
                             depth_m == 1.5 ~ NA_real_,
                           TRUE ~ value))

ggplot(subset(PAN_buoy_2020_L1_vert,
              subset=(datetime>= as.POSIXct('2020-04-24', tz='UTC') & datetime < as.POSIXct('2020-04-25', tz='UTC'))),
       aes(x=datetime, y=value, color = as.factor(depth_m))) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title='Panther Pond L0 April 23 2020', 
       x='date',
       y=NULL) +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme 

#april 25
ggplot(subset(PAN_buoy_2020_L1_vert,
              subset=(datetime>= as.POSIXct('2020-04-25', tz='UTC') & datetime < as.POSIXct('2020-04-26', tz='UTC'))),
       aes(x=datetime, y=value, color = as.factor(depth_m))) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title='Panther Pond L0 April 23 2020', 
       x='date',
       y=NULL) +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme 

PAN_buoy_2020_L1_vert <- PAN_buoy_2020_L1_vert %>% 
  mutate(value = case_when(datetime == as.POSIXct('2020-04-25 17:10', tz='UTC') ~ NA_real_,
                           datetime >= as.POSIXct('2020-04-24 11:30', tz='UTC') & 
                             datetime < as.POSIXct('2020-04-25 13:10', tz='UTC') &
                             depth_m == 19 ~ NA_real_,
                           TRUE ~ value))

# April 2020 #
ggplot(subset(PAN_buoy_2020_L1_vert,
              subset=(datetime>= as.POSIXct('2020-04-01', tz='UTC') & datetime < as.POSIXct('2020-05-01', tz='UTC'))),
       aes(x=datetime, y=value, color = as.factor(depth_m))) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title='Panther Pond L1 April 2020', 
       x='date',
       y=NULL) +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme 

ggsave(file.path(L1_figdir, '042020_L1.jpg'), width=8, height = 6, dpi=200)

# May 2020 #
ggplot(subset(PAN_buoy_2020_L1_vert,
              subset=(datetime>= as.POSIXct('2020-05-01', tz='UTC') & datetime < as.POSIXct('2020-06-01', tz='UTC'))),
       aes(x=datetime, y=value, color = as.factor(depth_m))) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title='Panther Pond L0 May 2020', 
       x='date',
       y=NULL) +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme 
ggsave(file.path(L0_figdir, '052020_L0.jpg'), width=8, height = 6, dpi=200)

#looks good, export to L1
ggplot(subset(PAN_buoy_2020_L1_vert,
              subset=(datetime>= as.POSIXct('2020-05-01', tz='UTC') & datetime < as.POSIXct('2020-06-01', tz='UTC'))),
       aes(x=datetime, y=value, color = as.factor(depth_m))) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title='Panther Pond L1 May 2020', 
       x='date',
       y=NULL) +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme 
ggsave(file.path(L1_figdir, '052020_L1.jpg'), width=8, height = 6, dpi=200)

# June 2020 #
ggplot(subset(PAN_buoy_2020_L1_vert,
              subset=(datetime>= as.POSIXct('2020-06-01', tz='UTC') & datetime < as.POSIXct('2020-07-01', tz='UTC'))),
       aes(x=datetime, y=value, color = as.factor(depth_m))) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title='Panther Pond L0 June 2020', 
       x='date',
       y=NULL) +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme 
ggsave(file.path(L0_figdir, '062020_L0.jpg'), width=8, height = 6, dpi=200)

#June 6
ggplot(subset(PAN_buoy_2020_L1_vert,
              subset=(datetime>= as.POSIXct('2020-06-06', tz='UTC') & datetime < as.POSIXct('2020-06-07', tz='UTC'))),
       aes(x=datetime, y=value, color = as.factor(depth_m))) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title='Panther Pond L0 June 2020', 
       x='date',
       y=NULL) +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme 

#everything is fine except lo-do, so leaving for now since that one might get recoded

# June 2020 #
ggplot(subset(PAN_buoy_2020_L1_vert,
              subset=(datetime>= as.POSIXct('2020-06-01', tz='UTC') & datetime < as.POSIXct('2020-07-01', tz='UTC'))),
       aes(x=datetime, y=value, color = as.factor(depth_m))) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title='Panther Pond L1 June 2020', 
       x='date',
       y=NULL) +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme 
ggsave(file.path(L1_figdir, '062020_L1.jpg'), width=8, height = 6, dpi=200)


# July 2020 #
ggplot(subset(PAN_buoy_2020_L1_vert,
              subset=(datetime>= as.POSIXct('2020-07-01', tz='UTC') & datetime < as.POSIXct('2020-08-01', tz='UTC'))),
       aes(x=datetime, y=value, color = as.factor(depth_m))) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title='Panther Pond L0 July 2020', 
       x='date',
       y=NULL) +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme 
ggsave(file.path(L0_figdir, '072020_L0.jpg'), width=8, height = 6, dpi=200)

#Jul 19
ggplot(subset(PAN_buoy_2020_L1_vert,
              subset=(datetime>= as.POSIXct('2020-07-19', tz='UTC') & datetime < as.POSIXct('2020-07-20', tz='UTC'))),
       aes(x=datetime, y=value, color = as.factor(depth_m))) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title='Panther Pond L0 July 2020', 
       x='date',
       y=NULL) +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme 

PAN_buoy_2020_L1_vert <- PAN_buoy_2020_L1_vert %>% 
  mutate(value = case_when(datetime >= as.POSIXct('2020-07-19 12:30', tz='UTC') &
                             datetime < as.POSIXct('2020-07-20', tz='UTC')~ NA_real_,
                           TRUE ~ value))

#jul 20
ggplot(subset(PAN_buoy_2020_L1_vert,
              subset=(datetime>= as.POSIXct('2020-07-20', tz='UTC') & datetime < as.POSIXct('2020-07-21', tz='UTC'))),
       aes(x=datetime, y=value, color = as.factor(depth_m))) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title='Panther Pond L0 July 2020', 
       x='date',
       y=NULL) +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme 

PAN_buoy_2020_L1_vert <- PAN_buoy_2020_L1_vert %>% 
  mutate(flag = case_when(datetime== as.POSIXct('2020-07-20 12:00', tz='UTC') & 
                            (variable == 'do_mgl' |variable == 'do_adj_mgl' |variable == 'do_percent)') ~ 'cp',
                          TRUE ~ ''))

# July 2020 #
ggplot(subset(PAN_buoy_2020_L1_vert,
              subset=(datetime>= as.POSIXct('2020-07-01', tz='UTC') & datetime < as.POSIXct('2020-08-01', tz='UTC'))),
       aes(x=datetime, y=value, color = as.factor(depth_m))) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title='Panther Pond L1 July 2020', 
       x='date',
       y=NULL) +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme 
ggsave(file.path(L1_figdir, '072020_L1.jpg'), width=8, height = 6, dpi=200)


# August 2020 #
ggplot(subset(PAN_buoy_2020_L1_vert,
              subset=(datetime>= as.POSIXct('2020-08-01', tz='UTC') & datetime < as.POSIXct('2020-09-01', tz='UTC'))),
       aes(x=datetime, y=value, color = as.factor(depth_m))) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title='Panther Pond L0 August 2020', 
       x='date',
       y=NULL) +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme 
ggsave(file.path(L0_figdir, '082020_L0.jpg'), width=8, height = 6, dpi=200)

#this looks good, save as L1 fig
ggplot(subset(PAN_buoy_2020_L1_vert,
              subset=(datetime>= as.POSIXct('2020-08-01', tz='UTC') & datetime < as.POSIXct('2020-09-01', tz='UTC'))),
       aes(x=datetime, y=value, color = as.factor(depth_m))) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title='Panther Pond L1 August 2020', 
       x='date',
       y=NULL) +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme 
ggsave(file.path(L1_figdir, '082020_L1.jpg'), width=8, height = 6, dpi=200)

# September 2020 #
ggplot(subset(PAN_buoy_2020_L1_vert,
              subset=(datetime>= as.POSIXct('2020-09-01', tz='UTC') & datetime < as.POSIXct('2020-10-01', tz='UTC'))),
       aes(x=datetime, y=value, color = as.factor(depth_m))) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title='Panther Pond L0 September 2020', 
       x='date',
       y=NULL) +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme 
ggsave(file.path(L0_figdir, '092020_L0.jpg'), width=8, height = 6, dpi=200)

#looks good, save as L1 image
ggplot(subset(PAN_buoy_2020_L1_vert,
              subset=(datetime>= as.POSIXct('2020-09-01', tz='UTC') & datetime < as.POSIXct('2020-10-01', tz='UTC'))),
       aes(x=datetime, y=value, color = as.factor(depth_m))) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title='Panther Pond L1 September 2020', 
       x='date',
       y=NULL) +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme 
ggsave(file.path(L1_figdir, '092020_L1.jpg'), width=8, height = 6, dpi=200)

# October 2020 #
ggplot(subset(PAN_buoy_2020_L1_vert,
              subset=(datetime>= as.POSIXct('2020-10-01', tz='UTC') & datetime < as.POSIXct('2020-11-01', tz='UTC'))),
       aes(x=datetime, y=value, color = as.factor(depth_m))) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title='Panther Pond L0 October 2020', 
       x='date',
       y=NULL) +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme 
ggsave(file.path(L0_figdir, '102020_L0.jpg'), width=8, height = 6, dpi=200)

#oct 8 something weird happening with 10m sensor
ggplot(subset(PAN_buoy_2020_L1_vert,
              subset=(datetime>= as.POSIXct('2020-10-08', tz='UTC') & datetime < as.POSIXct('2020-10-09', tz='UTC'))),
       aes(x=datetime, y=value, color = as.factor(depth_m))) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title='Panther Pond L0 October 2020', 
       x='date',
       y=NULL) +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme 

PAN_buoy_2020_L1_vert <- PAN_buoy_2020_L1_vert %>% 
  mutate(value = case_when(datetime >= as.POSIXct('2020-10-08 02:50', tz= 'UTC') &
                             depth_m == 10 ~ NA_real_,
                           TRUE ~ value))

#upper do begins to go intermittent on the 13th
ggplot(subset(PAN_buoy_2020_L1_vert,
              subset=(datetime>= as.POSIXct('2020-10-13', tz='UTC') & datetime < as.POSIXct('2020-10-15', tz='UTC'))),
       aes(x=datetime, y=value, color = as.factor(depth_m))) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title='Panther Pond L0 October 2020', 
       x='date',
       y=NULL) +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme 

PAN_buoy_2020_L1_vert <- PAN_buoy_2020_L1_vert %>% 
  mutate(flag = case_when(datetime >= as.POSIXct('2020-10-14', tz= 'UTC') &
                             depth_m == 1.5 ~ 'i',
                           TRUE ~ flag))

# removed from water on 24th
ggplot(subset(PAN_buoy_2020_L1_vert,
              subset=(datetime>= as.POSIXct('2020-10-24', tz='UTC') & datetime < as.POSIXct('2020-10-25', tz='UTC'))),
       aes(x=datetime, y=value, color = as.factor(depth_m))) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title='Panther Pond L0 October 2020', 
       x='date',
       y=NULL) +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  scale_x_datetime(date_minor_breaks = '1 hour') +
  final_theme 

PAN_buoy_2020_L1_vert <- PAN_buoy_2020_L1_vert %>% 
  mutate(value = case_when(datetime >= as.POSIXct('2020-10-24 9:20', tz= 'UTC') ~ NA_real_,
                           TRUE ~ value))

ggplot(subset(PAN_buoy_2020_L1_vert,
              subset=(datetime>= as.POSIXct('2020-10-01', tz='UTC') & datetime < as.POSIXct('2020-11-01', tz='UTC'))),
       aes(x=datetime, y=value, color = as.factor(depth_m))) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title='Panther Pond L1 October 2020', 
       x='date',
       y=NULL) +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  scale_x_datetime(date_minor_breaks = '1 day') +
  final_theme 
ggsave(file.path(L1_figdir, '102020_L1.jpg'), width=8, height = 6, dpi=200)

#recode and flag data below zero to zero 
PAN_buoy_2020_L1_vert <- PAN_buoy_2020_L1_vert %>% 
  mutate(flag = case_when(flag == '' & value < 0 ~ 'z',
                          flag != '' & value < 0 ~ paste('z', flag, sep = ', '),
                          TRUE ~ flag)) %>% 
  mutate(value = case_when(value < 0 ~ 0,
                           TRUE ~ value))

# 2020 #
ggplot(PAN_buoy_2020_L1_vert, aes(x=datetime, y=value, color = as.factor(depth_m), shape = flag)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title='Panther Pond L1 2020', 
       x='date',
       y = NULL) +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3",
                              "#00e639", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) +   
  final_theme 
ggsave(file.path(L1_figdir, '2020_L1.jpg'), width=12, height = 8, dpi=300)

colnames(PAN_buoy_2020_L1_vert)


#### save the file ####
PAN_buoy_2020_L1_vert %>%
  mutate(datetime = as.character(datetime)) %>%
  write_csv(., file.path(L1_dir, paste0('PantherBuoy_L1_2020_v', v_date, '.csv')))



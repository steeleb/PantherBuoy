#*********************************************************************
#*      Cary Institute of Ecosystem Studies (Millbrook, NY)          *
#*        for use by the Panther Pond Citizen Scientists             * 
#*                                                                   *
#* TITLE:   panther_datavisualization_11Jun2020.r                    *
#* AUTHOR:  B. Steele                                                *
#* SYSTEM:  Lenovo ThinkCentre, Windows 10, R 3.4.0                  *
#* DATE:    06Aug2018                                                *
#* PROJECT: Panther Pond datalogger data                             *
#* PURPOSE: display data using rlakeanalyzer                         *
#* UPDATED: 11Jun2020
#*********************************************************************
#*                     folder tree structure                         *
#*                                                                   *
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

library(tidyverse)
library(rLakeAnalyzer)

dir <- 'C:/Users/steeleb/Dropbox/PantherBuoy/data/L1/'
rds_dir <- 'C:/Users/steeleb/Dropbox/PantherBuoy/data/derivative/for visualization/'
fig_dir <- 'C:/Users/steeleb/Dropbox/PantherBuoy/r-export/figures/smears/'

#color palette for DO smears
color_palette <- colorRampPalette(c("darkred","red","orange", "#edf8b1","#41b6c4", "#225ea8","#253494", "#081d58","black"),bias = 1.5, space = "rgb")(n = 144)

#read in buoy data

PAN_2016 <- read_csv(file.path(dir,'2016 Panther L1 buoy data 30July2018.csv'),
                     col_types = 'TnnnicD') #pull in data
PAN_2017 <- read_csv(file.path(dir,'2017 Panther L1 buoy data 30July2018.csv'),
                     col_types = 'TnnnicD') #pull in data
PAN_2018 <- read_csv(file.path(dir,'2018 Panther L1 buoy data 10Jun2020.csv'),
                     col_types =  'Ticncn')
PAN_2019 <- read_csv(file.path(dir,'2019 Panther L1 buoy data 10Jun2020.csv'),
                     col_types = 'Ticncn')
PAN_2020 <- read_csv(file.path(dir, 'PantherBuoy_L1_2020_v2021-08-14.csv'),
                     col_types = 'Ticnccnc')

#### temp smears ####
PAN_2016_sub <- subset(PAN_2016, select=c(datetime, depth_m, temp_C)) %>% 
  mutate(depth_m = case_when(depth_m == 0.5 ~ 'wtr_0.5',
                             depth_m == 1 ~ 'wtr_1.0',
                             depth_m == 2 ~ 'wtr_2.0',
                             depth_m == 4 ~ 'wtr_4.0',
                             depth_m == 6 ~ 'wtr_6.0',
                             depth_m == 8 ~ 'wtr_8.0',
                             depth_m == 10 ~ 'wtr_10.0',
                             depth_m == 15 ~ 'wtr_15.0',
                             depth_m == 20 ~ 'wtr_20.0',
                             TRUE ~ '')) %>% 
  pivot_wider(names_from = depth_m, values_from = temp_C) %>% 
  select(datetime, wtr_0.5, wtr_1.0, wtr_2.0, wtr_4.0, wtr_6.0, wtr_8.0, wtr_10.0, wtr_15.0, wtr_20.0) %>% 
  arrange(datetime)

PAN_2017_sub <- subset(PAN_2017, select=c(datetime, depth_m, temp_C)) %>% 
  mutate(depth_m = case_when(depth_m == 0.1 ~ 'wtr_0.1',
                             depth_m == 0.5 ~ 'wtr_0.5',
                             depth_m == 2 ~ 'wtr_2.0',
                             depth_m == 4 ~ 'wtr_4.0',
                             depth_m == 6 ~ 'wtr_6.0',
                             depth_m == 8 ~ 'wtr_8.0',
                             depth_m == 10 ~ 'wtr_10.0',
                             depth_m == 15 ~ 'wtr_15.0',
                             depth_m == 20 ~ 'wtr_20.0',
                             TRUE ~ '')) %>% 
  pivot_wider(names_from = depth_m, values_from = temp_C) %>% 
  select(datetime, wtr_0.1, wtr_0.5, wtr_2.0, wtr_4.0, wtr_6.0, wtr_8.0, wtr_10.0, wtr_15.0, wtr_20.0) %>% 
  arrange(datetime)

PAN_2018_sub <- PAN_2018 %>% 
  filter(variable == 'temp_C') %>% 
  select(datetime, depth_m, value) %>% 
  mutate(depth_m = case_when(depth_m == 0.5 ~ 'wtr_0.5',
                             depth_m == 1.28 ~ 'wtr_1.28',
                             depth_m == 1.58 ~ 'wtr_1.58',
                             depth_m == 2 ~ 'wtr_2.0',
                             depth_m == 4 ~ 'wtr_4.0',
                             depth_m == 6 ~ 'wtr_6.0',
                             depth_m == 8 ~ 'wtr_8.0',
                             depth_m == 10 ~ 'wtr_10.0',
                             depth_m == 15 ~ 'wtr_15.0',
                             depth_m == 19 ~ 'wtr_19.0',
                             depth_m == 20 ~ 'wtr_20.0',
                             TRUE ~ '')) %>% 
  pivot_wider(names_from = depth_m, values_from = value) %>% 
  select(datetime, wtr_0.5, wtr_1.28, wtr_1.58, wtr_2.0, wtr_4.0, wtr_6.0, wtr_8.0, wtr_10.0, wtr_15.0, wtr_19.0, wtr_20.0) %>% 
  arrange(datetime)

PAN_2019_sub <- PAN_2019 %>% 
  filter(variable == 'temp_C') %>% 
  select(datetime, depth_m, value) %>% 
  mutate(depth_m = case_when(depth_m == 0.5 ~ 'wtr_0.5',
                             depth_m == 1 ~ 'wtr_1.0',
                             depth_m == 1.5 ~ 'wtr_1.5',
                             depth_m == 2 ~ 'wtr_2.0',
                             depth_m == 4 ~ 'wtr_4.0',
                             depth_m == 6 ~ 'wtr_6.0',
                             depth_m == 8 ~ 'wtr_8.0',
                             depth_m == 10 ~ 'wtr_10.0',
                             depth_m == 15 ~ 'wtr_15.0',
                             depth_m == 19 ~ 'wtr_19.0',
                             depth_m == 20 ~ 'wtr_20.0',
                             TRUE ~ '')) %>% 
  pivot_wider(names_from = depth_m, values_from = value) %>% 
  select(datetime, wtr_0.5, wtr_1.0, wtr_1.5, wtr_2.0, wtr_4.0, wtr_6.0, wtr_8.0, wtr_10.0, wtr_15.0, wtr_19.0, wtr_20.0) %>% 
  arrange(datetime)

PAN_2020_sub <- PAN_2020 %>% 
  filter(variable == 'temp_C') %>% 
  select(datetime, depth_m, value) %>% 
  mutate(depth_m = case_when(depth_m == 0.5 ~ 'wtr_0.5',
                             depth_m == 1 ~ 'wtr_1.0',
                             depth_m == 1.5 ~ 'wtr_1.5',
                             depth_m == 2 ~ 'wtr_2.0',
                             depth_m == 4 ~ 'wtr_4.0',
                             depth_m == 6 ~ 'wtr_6.0',
                             depth_m == 8 ~ 'wtr_8.0',
                             depth_m == 10 ~ 'wtr_10.0',
                             depth_m == 15 ~ 'wtr_15.0',
                             depth_m == 19 ~ 'wtr_19.0',
                             depth_m == 20 ~ 'wtr_20.0',
                             TRUE ~ '')) %>% 
  filter(!is.na(value)) %>% 
  pivot_wider(names_from = depth_m, values_from = value) %>% 
  select(datetime, wtr_0.5, wtr_1.0, wtr_1.5, wtr_2.0, wtr_4.0, wtr_6.0, wtr_8.0, wtr_10.0, wtr_15.0, wtr_19.0, wtr_20.0) %>% 
  arrange(datetime)



#create RDS files

#2016
curr.ts.temp.c.2016 <- PAN_2016_sub %>% 
  mutate(datetime = as.character(datetime))
write_delim(curr.ts.temp.c.2016, file.path(rds_dir, "curr.ts.temp.c.2016.txt"), delim='\t')

#2017
curr.ts.temp.c.2017 <- PAN_2017_sub%>% 
  mutate(datetime = as.character(datetime))
write_delim(curr.ts.temp.c.2017, file.path(rds_dir, "curr.ts.temp.c.2017.txt"), delim='\t')

#2018
curr.ts.temp.c.2018 <- PAN_2018_sub%>% 
  mutate(datetime = as.character(datetime))
write_delim(curr.ts.temp.c.2018, file.path(rds_dir, "curr.ts.temp.c.2018.txt"), delim='\t')

#2019
curr.ts.temp.c.2019 <- PAN_2019_sub%>% 
  mutate(datetime = as.character(datetime))
write_delim(curr.ts.temp.c.2019, file.path(rds_dir, "curr.ts.temp.c.2019.txt"), delim='\t')

#2020
curr.ts.temp.c.2020 <- PAN_2020_sub%>% 
  mutate(datetime = as.character(datetime))
write_delim(curr.ts.temp.c.2020, file.path(rds_dir, "curr.ts.temp.c.2020.txt"), delim='\t')

#open RDS file in place of above file
curr.ts.temp.c.2016 <- load.ts(file.path(rds_dir, "curr.ts.temp.c.2016.txt"))
curr.ts.temp.c.2017 <- load.ts(file.path(rds_dir, "curr.ts.temp.c.2017.txt"))
curr.ts.temp.c.2018 <- load.ts(file.path(rds_dir, "curr.ts.temp.c.2018.txt"))
curr.ts.temp.c.2019 <- load.ts(file.path(rds_dir, "curr.ts.temp.c.2019.txt"))
curr.ts.temp.c.2020 <- load.ts(file.path(rds_dir, "curr.ts.temp.c.2020.txt"))

# Generate heatmap
png(file.path(fig_dir, '2016 temp no fill.png'), width = 1500, height = 750, units = 'px')
wtr.heat.map(curr.ts.temp.c.2016, 
             xlim=c(as.POSIXct('2016-05-01', tz='UTC'), as.POSIXct('2016-11-01', tz='UTC')), 
             zlim=c(5,30), 
             plot.title=title(main="Panther Temperature Heat Map 2016", 
                              ylab="Depth (m)", 
                              xlab=NULL
             ), 
             plot.axes = { axis.POSIXct(side=1, x=curr.ts.temp.c.2016$datetime, at = (seq(as.POSIXct('2016-05-01', tz='UTC'), as.POSIXct('2016-11-01', tz='UTC'), by = "month")), format = "%b"); axis(2) },
             key.title=title(main="Temp (C)", 
                             font.main=1, 
                             cex.main=1)
)
dev.off()

png(file.path(fig_dir, '2017 temp no fill.png'), width = 1500, height = 750, units = 'px')
wtr.heat.map(curr.ts.temp.c.2017, 
             xlim=c(as.POSIXct('2017-05-01', tz='UTC'), as.POSIXct('2017-11-01', tz='UTC')), 
             zlim=c(5,30), 
             plot.title=title(main="Panther Temperature Heat Map 2017", 
                              ylab="Depth (m)", 
                              xlab=NULL
             ), 
             plot.axes = { axis.POSIXct(side=1, x=curr.ts.temp.c.2017$datetime, at = (seq(as.POSIXct('2017-05-01', tz='UTC'), as.POSIXct('2017-11-01', tz='UTC'), by = "month")), format = "%b"); axis(2) },
             key.title=title(main="Temp (C)", 
                             font.main=1, 
                             cex.main=1)
)
dev.off()

png(file.path(fig_dir, '2018 temp no fill.png'), width = 1500, height = 750, units = 'px')
wtr.heat.map(curr.ts.temp.c.2018, 
             xlim=c(as.POSIXct('2018-05-01', tz='UTC'), as.POSIXct('2018-11-01', tz='UTC')), 
             zlim=c(5,30), 
             plot.title=title(main="Panther Temperature Heat Map 2018", 
                              ylab="Depth (m)", 
                              xlab=NULL
             ), 
             plot.axes = { axis.POSIXct(side=1, x=curr.ts.temp.c.2018$datetime, at = (seq(as.POSIXct('2018-05-01', tz='UTC'), as.POSIXct('2018-11-01', tz='UTC'), by = "month")), format = "%b"); axis(2) },
             key.title=title(main="Temp (C)", 
                             font.main=1, 
                             cex.main=1)
)
dev.off()

png(file.path(fig_dir, '2019 temp no fill.png'), width = 1500, height = 750, units = 'px')
wtr.heat.map(curr.ts.temp.c.2019, 
             xlim=c(as.POSIXct('2019-05-01', tz='UTC'), as.POSIXct('2019-11-01', tz='UTC')), 
             zlim=c(5,30), 
             plot.title=title(main="Panther Temperature Heat Map 2019", 
                              ylab="Depth (m)", 
                              xlab=NULL
             ), 
             plot.axes = { axis.POSIXct(side=1, x=curr.ts.temp.c.2019$datetime, at = (seq(as.POSIXct('2019-05-01', tz='UTC'), as.POSIXct('2019-11-01', tz='UTC'), by = "month")), format = "%b"); axis(2) },
             key.title=title(main="Temp (C)", 
                             font.main=1, 
                             cex.main=1)
)
dev.off()

png(file.path(fig_dir, '2020 temp no fill.png'), width = 1500, height = 750, units = 'px')
wtr.heat.map(curr.ts.temp.c.2020, 
             xlim=c(as.POSIXct('2020-05-01', tz='UTC'), as.POSIXct('2020-11-01', tz='UTC')), 
             zlim=c(5,30), 
             plot.title=title(main="Panther Temperature Heat Map 2020", 
                              ylab="Depth (m)", 
                              xlab=NULL
             ), 
             plot.axes = { axis.POSIXct(side=1, x=curr.ts.temp.c.2020$datetime, at = (seq(as.POSIXct('2020-05-01', tz='UTC'), as.POSIXct('2020-11-01', tz='UTC'), by = "month")), format = "%b"); axis(2) },
             key.title=title(main="Temp (C)", 
                             font.main=1, 
                             cex.main=1)
)
dev.off()


#### DO SMEARS ####
PAN_DO_2017 <- read_csv(file.path(dir, '2017 L1 DO Panther.csv'),
                        col_types = 'iTnnncic') %>% 
  select(datetime, depth_m, do_mgl) %>% 
  arrange(depth_m) %>% 
  mutate(depth_m = case_when(depth_m == 19 ~ 'wtr_19.0',
                             depth_m == 1.5 ~ 'wtr_1.5',
                             TRUE ~ '')) %>% 
  pivot_wider(names_from = depth_m, values_from = do_mgl) %>% 
  arrange(datetime)

PAN_DO_2018 <- PAN_2018 %>% 
  filter(variable == 'do_mgl') %>% 
  select(datetime, depth_m, value) %>% 
  mutate(depth_m = case_when(depth_m == 1.58 ~ 'wtr_1.58',
                             depth_m == 19 ~ 'wtr_19.0',
                             TRUE ~ '')) %>% 
  pivot_wider(names_from = depth_m, values_from = value) %>% 
  select(datetime, wtr_1.58, wtr_19.0) %>% 
  arrange(datetime)

PAN_DO_2019 <- PAN_2019 %>% 
  filter(variable == 'do_mgl') %>% 
  select(datetime, depth_m, value) %>% 
  mutate(depth_m = case_when(depth_m == 1.5 ~ 'wtr_1.5',
                             depth_m == 19 ~ 'wtr_19.0',
                             TRUE ~ '')) %>% 
  pivot_wider(names_from = depth_m, values_from = value) %>% 
  select(datetime, wtr_1.5, wtr_19.0) %>% 
  arrange(datetime)

PAN_DO_2020 <- PAN_2020 %>% 
  filter(variable == 'do_mgl') %>% 
  select(datetime, depth_m, value) %>% 
  mutate(depth_m = case_when(depth_m == 1.5 ~ 'wtr_1.5',
                             depth_m == 19 ~ 'wtr_19.0',
                             TRUE ~ '')) %>% 
  filter(!is.na(value)) %>% 
  pivot_wider(names_from = depth_m, values_from = value) %>% 
  select(datetime, wtr_1.5, wtr_19.0) %>% 
  arrange(datetime)

#2017
curr.ts.do.c.2017 <- PAN_DO_2017 %>% 
  mutate(datetime = as.character(datetime))
write_delim(curr.ts.do.c.2017, file.path(rds_dir, "curr.ts.do.c.2017.txt"), delim='\t')

# 2018
curr.ts.do.c.2018 <- PAN_DO_2018 %>% 
  mutate(datetime = as.character(datetime))
write_delim(curr.ts.do.c.2018, file.path(rds_dir, "curr.ts.do.c.2018.txt"), delim='\t')

# 2019
curr.ts.do.c.2019 <- PAN_DO_2019 %>% 
  mutate(datetime = as.character(datetime))
write_delim(curr.ts.do.c.2019, file.path(rds_dir, "curr.ts.do.c.2019.txt"), delim='\t')

# 2020
curr.ts.do.c.2020 <- PAN_DO_2020 %>% 
  mutate(datetime = as.character(datetime))
write_delim(curr.ts.do.c.2020, file.path(rds_dir, "curr.ts.do.c.2020.txt"), delim='\t')


#open RDS file in place of above file
curr.ts.do.c.2017 <- load.ts(file.path(rds_dir, "curr.ts.do.c.2017.txt"))
curr.ts.do.c.2018 <- load.ts(file.path(rds_dir, "curr.ts.do.c.2018.txt"))
curr.ts.do.c.2019 <- load.ts(file.path(rds_dir, "curr.ts.do.c.2019.txt"))
curr.ts.do.c.2020 <- load.ts(file.path(rds_dir, "curr.ts.do.c.2020.txt"))

# Generate heatmap
png(file.path(fig_dir, '2017 do no fill.png'), width = 1500, height = 750, units = 'px')
wtr.heat.map(curr.ts.do.c.2017, 
             xlim=c(as.POSIXct('2017-05-01', tz='UTC'), as.POSIXct('2017-11-01', tz='UTC')), 
             zlim=c(0,10), 
             plot.title=title(main="Panther Dissolved Oxygen Heat Map 2017", 
                              ylab="Depth (m)", 
                              xlab=NULL
             ), 
             plot.axes = { axis.POSIXct(side=1, x=curr.ts.temp.c.2017$datetime, at = (seq(as.POSIXct('2017-05-01', tz='UTC'), as.POSIXct('2017-11-01', tz='UTC'), by = "month")), format = "%b"); axis(2) },
             key.title=title(main="DO (mg/L)", 
                             font.main=1, 
                             cex.main=1),
             col = color_palette
)
dev.off()

png(file.path(fig_dir, '2018 do no fill.png'), width = 1500, height = 750, units = 'px')
wtr.heat.map(curr.ts.do.c.2018, 
             xlim=c(as.POSIXct('2018-05-01', tz='UTC'), as.POSIXct('2018-11-01', tz='UTC')), 
             zlim=c(0,10), 
             plot.title=title(main="Panther Dissolved Oxygen Heat Map 2018", 
                              ylab="Depth (m)", 
                              xlab=NULL
             ), 
             plot.axes = { axis.POSIXct(side=1, x=curr.ts.temp.c.2018$datetime, at = (seq(as.POSIXct('2018-05-01', tz='UTC'), as.POSIXct('2018-11-01', tz='UTC'), by = "month")), format = "%b"); axis(2) },
             key.title=title(main="DO (mg/L)", 
                             font.main=1, 
                             cex.main=1),
             col = color_palette
)
dev.off()

png(file.path(fig_dir, '2019 do no fill.png'), width = 1500, height = 750, units = 'px')
wtr.heat.map(curr.ts.do.c.2019, 
             xlim=c(as.POSIXct('2019-05-01', tz='UTC'), as.POSIXct('2019-11-01', tz='UTC')), 
             zlim=c(0,10), 
             plot.title=title(main="Panther Dissolved Oxygen Heat Map 2019", 
                              ylab="Depth (m)", 
                              xlab=NULL
             ), 
             plot.axes = { axis.POSIXct(side=1, x=curr.ts.temp.c.2019$datetime, at = (seq(as.POSIXct('2019-05-01', tz='UTC'), as.POSIXct('2019-11-01', tz='UTC'), by = "month")), format = "%b"); axis(2) },
             key.title=title(main="DO (mg/L)", 
                             font.main=1, 
                             cex.main=1),
             col = color_palette
)
dev.off()

png(file.path(fig_dir, '2020 do no fill.png'), width = 1500, height = 750, units = 'px')
wtr.heat.map(curr.ts.do.c.2020, 
             xlim=c(as.POSIXct('2020-05-01', tz='UTC'), as.POSIXct('2020-11-01', tz='UTC')), 
             zlim=c(0,10), 
             plot.title=title(main="Panther Dissolved Oxygen Heat Map 2020", 
                              ylab="Depth (m)", 
                              xlab=NULL
             ), 
             plot.axes = { axis.POSIXct(side=1, x=curr.ts.temp.c.2020$datetime, at = (seq(as.POSIXct('2020-05-01', tz='UTC'), as.POSIXct('2020-11-01', tz='UTC'), by = "month")), format = "%b"); axis(2) },
             key.title=title(main="DO (mg/L)", 
                             font.main=1, 
                             cex.main=1),
             col = color_palette
)
dev.off()

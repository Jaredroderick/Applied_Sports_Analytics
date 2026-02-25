# SPM 5708 HW 4
# 4.1
# 1
library(baseballr)
library(dplyr)
# 2
bat_data <- fg_batter_leaders(startseason=2023, endseason=2023)
# 3 
bat_use <- bat_data %>% filter(PA >= 50)
# 4 Check cor(bat_use$IFH_pct, bat_use$EV / bat_use$Spd / bat_use$Contact_pct / bat_use$GB_pct)
cor(bat_use$IFH_pct,bat_use$EV)
cor(bat_use$IFH_pct, bat_use$Spd)
cor(bat_use$IFH_pct, bat_use$Contact_pct) 
cor(bat_use$IFH_pct, bat_use$GB_pct)
cat('Spd displays the strongest correlation w IFH_pct.')
# 5 
# Load Ohtani_2025.csv
Ohtani_2025 <- read.csv('Ohtani_2025.csv')
# Find the correlation btw Ohtani's EV / launch_speed and launch angle
cor(Ohtani_2025$launch_speed, Ohtani_2025$launch_angle, use='complete.obs')
library(ggplot2)
ggplot(Ohtani_2025, aes(x = launch_speed, y = launch_angle)) + geom_point(col = 'dodgerblue')
# 6
Ohtani_2025 <- Ohtani_2025 %>% mutate(is_hr=ifelse(events=="home_run","HR","NO"))
# 7
ggplot(Ohtani_2025, aes(x = launch_speed, y = launch_angle)) + geom_point(aes(col = is_hr))
# 8
hr_avg <- Ohtani_2025 %>% filter(events =='home_run') %>% summarize(mean_launch_angle = mean(launch_angle, na.rm = TRUE), mean_launch_speed = mean(launch_speed, na.rm = TRUE))

# 4.2
# 1
# Import the NCAAB_Efficiency.xlsx dataset
library(readxl)
NCAAB_Efficiency <- read_excel('NCAAB_Efficiency.xlsx')
# Calculate the correlation coeff for PPG, WinPct and OPPG, WinPct
cor(NCAAB_Efficiency$PPG, NCAAB_Efficiency$WinPct)
cor(NCAAB_Efficiency$OPPG, NCAAB_Efficiency$WinPct)
# 2
# Calculate the correlation btw ORtg and WinPct and DRtg and WinPct
cor(NCAAB_Efficiency$ORtg, NCAAB_Efficiency$WinPct)
cor(NCAAB_Efficiency$DRtg, NCAAB_Efficiency$WinPct)
# 3
cor(NCAAB_Efficiency$ORtg, NCAAB_Efficiency$DRtg)
# 4
ggplot(NCAAB_Efficiency, aes(x=ORtg, y=DRtg)) + geom_point()
# 5
cor(NCAAB_Efficiency$WinPct, NCAAB_Efficiency$Pace)
# 6
NCAAB_Tourney <- NCAAB_Efficiency %>% filter(Tourney == 1)
cor(NCAAB_Tourney$WinPct, NCAAB_Tourney$Pace)
ggplot(NCAAB_Tourney, aes(x = Pace, y = WinPct)) + geom_point()


# 4.3
# 1
# Import the PGCareers.csv dataset
PGCareers <- read.csv('PGCareers.csv')
PGCareers <- PGCareers %>% mutate(MPG = MP / G)
# 2
cor(PGCareers$MPG, PGCareers$Age)
# 3
ggplot(PGCareers,aes(x=Age,y=MPG))+geom_point()
# 4
ggplot(PGCareers,aes(x=Age,y=MPG)) + geom_point() + geom_smooth(method='loess')















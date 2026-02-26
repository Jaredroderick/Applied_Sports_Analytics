# SPM 5708 HW 6
# 6.1
# Load the data
library(readxl)
NFL <- read_excel('Gridiron.xlsx', sheet = 'NFL')
# 2. Check what is stronger linear relationship: 
cor(NFL$PassYPA, NFL$PF)
#  0.7577183
cor(NFL$TotPassYds, NFL$PF)
# 0.6402673
# 3
cor(NFL$OPassYPA, NFL$PA)
# 0.7116725
cor(NFL$OTotPassYds, NFL$PA)
# 0.4632597
# 4
NFL <- NFL %>% mutate(Margin = PF - PA)
# 5 
Margin_Model <- lm(Margin ~ PassYPA + OPassYPA + RushYPA + ORushYPA + TOC + TOF + OffPenYds + DefPenYds, data = NFL)
summary(Margin_Model)
# 9
cor(NFL$RushYPA, NFL$PassYPA)
# 0.1251568
library(ggplot2)
ggplot(NFL, aes(x = RushYPA, y = PassYPA)) + geom_point(aes(color = Margin)) 
# 10
# Import the NFL2018 sheet
NFL2018 <- read_excel('Gridiron.xlsx', sheet = 'NFL2018')
NFL2018 <- NFL2018 %>% mutate(Est_Margin = predict(Margin_Model, newdata = NFL2018, type = 'response'))
# Calculate the Mean Aboluste Error
NFL2018 <- NFL2018 %>% mutate(Margin_Resid = Margin - Est_Margin)
MAE_Margin <- mean(abs(NFL2018$Margin_Resid))
MAE_Margin
# 49.55999
# 11
NCAAF <- read_excel('Gridiron.xlsx', sheet = 'NCAAF')
# Create a model using the same variables but for NCAAF data
NCAAF_Margin_Model <- lm(Margin ~ PassYPA + OPassYPA + RushYPA + ORushYPA + TOC + TOF + OffPenYds + DefPenYds, data = NCAAF)
summary(NCAAF_Margin_Model)


# 6.2
# Load the data
MLB_Attendance <- read_excel('MLB_Attendance.xlsx', sheet = 'MLB_Attendance')
# 3
PctCap_Model <- lm(PctCap ~ GB_Pre + RS_PG + RA_PG + WPct_Pre + Pre_Streak + Temp + Wind + Start_Time + Opp_RS_PG + Opp_RA_PG + OppWPct_Pre + Opp_Pre_Streak, data = MLB_Attendance)
summary(PctCap_Model)
# Drop Opp_Pre_Streak and rerun
PctCap_Model_2 <- lm(PctCap ~ GB_Pre + RS_PG + RA_PG + WPct_Pre + Pre_Streak + Temp + Wind + Start_Time + Opp_RS_PG + Opp_RA_PG + OppWPct_Pre, data = MLB_Attendance)
summary(PctCap_Model_2)
# Drop WPct_Pre and rerun
PctCap_Model_3 <- lm(PctCap ~ GB_Pre + RS_PG + RA_PG + Pre_Streak + Temp + Wind + Start_Time + Opp_RS_PG + Opp_RA_PG + OppWPct_Pre, data = MLB_Attendance)
summary(PctCap_Model_3)
# Drop Pre_Streak and rerun
PctCap_Model_4 <- lm(PctCap ~ GB_Pre + RS_PG + RA_PG + Temp + Wind + Start_Time + Opp_RS_PG + Opp_RA_PG + OppWPct_Pre, data = MLB_Attendance)
summary(PctCap_Model_4)
# Drop OppWPct_Pre and rerun
PctCap_Model_5 <- lm(PctCap ~ GB_Pre + RS_PG + RA_PG + Temp + Wind + Start_Time + Opp_RS_PG + Opp_RA_PG, data = MLB_Attendance)
summary(PctCap_Model_5)
Att_21 <- read_excel('MLB_Attendance.xlsx', sheet = 'Att_2021')
Att_21 <- Att_21 %>% mutate(Est_PctCap = predict(PctCap_Model_5, newdata = Att_21, type = 'response'))
Att_21 <- Att_21 %>% mutate(PctCap_Resid = PctCap - Est_PctCap)
MAE_Att_21 <- mean(abs(Att_21$PctCap_Resid))
MAE_Att_21

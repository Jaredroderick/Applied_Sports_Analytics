# SPM5708 HW 5
# 5.1
# Import the data
library(readxl)
library(dplyr)
Mahomes <- read_excel('Mahomes.xlsx', sheet = 'Mahomes')
Warner <- read_excel('Mahomes.xlsx', sheet = 'Warner')
Marino <- read_excel('Mahomes.xlsx', sheet = 'Marino')
# Filter the data
Mahomes <- Mahomes %>% filter(Pos == 'QB' & Att >= 100)
Warner <- Warner %>% filter(Pos == 'QB' & Att >= 100)
Marino <- Marino %>% filter(Pos == 'QB' & Att >= 100)
# Standardize CmpPct, AdjNYPA, TD
Mahomes <- Mahomes %>% mutate(CmpPct_Z = as.numeric(scale(CmpPct, center = TRUE, scale = TRUE)),
                              AdjNYPA_Z = as.numeric(scale(AdjNYPA, center = TRUE ,scale = TRUE)),
                              TD_Z = as.numeric(scale(TD, center = TRUE, scale = TRUE)))
Warner <- Warner %>% mutate(CmpPct_Z = as.numeric(scale(CmpPct, center = TRUE, scale = TRUE)),
                              AdjNYPA_Z = as.numeric(scale(AdjNYPA, center = TRUE ,scale = TRUE)),
                              TD_Z = as.numeric(scale(TD, center = TRUE, scale = TRUE)))
Marino <- Marino %>% mutate(CmpPct_Z = as.numeric(scale(CmpPct, center = TRUE, scale = TRUE)),
                              AdjNYPA_Z = as.numeric(scale(AdjNYPA, center = TRUE ,scale = TRUE)),
                              TD_Z = as.numeric(scale(TD, center = TRUE, scale = TRUE)))
# Normalize CmpPct, AdjNYPA, TD
Mahomes <- Mahomes %>% mutate(CmpPct_Norm = pnorm(CmpPct_Z),
                              AdjNYPA_Norm = pnorm(AdjNYPA_Z),
                              TD_Norm = pnorm(TD_Z))
Warner <- Warner %>% mutate(CmpPct_Norm = pnorm(CmpPct_Z),
                              AdjNYPA_Norm = pnorm(AdjNYPA_Z),
                              TD_Norm = pnorm(TD_Z))
Marino <- Marino %>% mutate(CmpPct_Norm = pnorm(CmpPct_Z),
                              AdjNYPA_Norm = pnorm(AdjNYPA_Z),
                              TD_Norm = pnorm(TD_Z))
# Filter out only the 3 players
Mahomes_Comp <- Mahomes %>% filter(Player == 'Patrick Mahomes') 
Warner_Comp <- Warner %>% filter(Player == 'Kurt Warner')
Marino_Comp <- Marino %>% filter(Player == 'Dan Marino')
Full_Comp <- bind_rows(Mahomes_Comp, Warner_Comp, Marino_Comp)
Full_Comp <- Full_Comp %>% select('Player', 'CmpPct_Norm', 'AdjNYPA_Norm', 'TD_Norm')
View(Full_Comp)


# 5.1
# Import the data
fbs_portal <- read.csv('fbs_portal.csv')
# Paired sample two-sided t test 
# H0: mean[WR PFF_Grade_Pre_WT] = mean[WR PFF_Grade_Post_WT]
# HA: mean[WR PFF_Grade_Pre_WT] ≠ mean[WR PFF_Grade_Post_WT]
# Alpha = .05
WR_Data <- fbs_portal %>% filter(Position == 'WR')
t.test(WR_Data$PFF_Grade_Pre_WT, WR_Data$PFF_Grade_Post_WT, paired = TRUE)
# p-value = .0399
# Paired sample two-sided t test 
# H0: mean[TE PFF_Grade_Pre_WT] = mean[TE PFF_Grade_Post_WT]
# HA: mean[TE PFF_Grade_Pre_WT] ≠ mean[TE PFF_Grade_Post_WT]
# Alpha = .05
TE_Data <- fbs_portal %>% filter(Position == 'TE')
t.test(TE_Data$PFF_Grade_Pre_WT, TE_Data$PFF_Grade_Post_WT, paired = TRUE)
# p-value = .05785
# Paired sample one-sided t test 
# H0: mean[WR PFF_Grade_Pre_WT] >= mean[WR PFF_Grade_Post_WT] ; mean[WR PFF_Grade_Pre_WT] - mean[WR PFF_Grade_Post_WT] >= 0
# HA: mean[WR PFF_Grade_Pre_WT] < mean[WR PFF_Grade_Post_WT] ; mean[WR PFF_Grade_Pre_WT] - mean[WR PFF_Grade_Post_WT] < 0
# Alpha = .05
t.test(WR_Data$PFF_Grade_Pre_WT, WR_Data$PFF_Grade_Post_WT, paired = TRUE, alternative = 'less')
# p-value = .01995
# Paired sample one-sided t test 
# H0: mean[TE PFF_Grade_Pre_WT] >= mean[TE PFF_Grade_Post_WT] ; mean[TE PFF_Grade_Pre_WT] - mean[TE PFF_Grade_Post_WT] >= 0
# HA: mean[TE PFF_Grade_Pre_W]T < mean[TE PFF_Grade_Post_WT] ; mean[TE PFF_Grade_Pre_WT] - mean[TE PFF_Grade_Post_WT] < 0
# Alpha = .05
t.test(TE_Data$PFF_Grade_Pre_WT, TE_Data$PFF_Grade_Post_WT, paired = TRUE, alternative = 'less')
# p-value = .9711
# Create new dataset of 3 stars
three_stars_data <- fbs_portal %>% filter(Star_Rating == 3)
# Paired sample one-sided t test 
# H0: mean[Snap_Count_Pre_per_Season] >= mean[Snap_Count_Post_per_Season] ; mean[Snap_Count_Pre_per_Season] - mean[Snap_Count_Post_per_Season] >= 0
# HA: mean[Snap_Count_Pre_per_Season] < mean[Snap_Count_Post_per_Season] ; mean[Snap_Count_Pre_per_Season] - mean[Snap_Count_Post_per_Season] < 0
# Alpha = .05
t.test(three_stars_data$Snap_Count_Pre_per_Season, three_stars_data$Snap_Count_Post_per_Season, paired = TRUE, alternative = 'less')
# Create new dataset of 5 stars
five_stars_data <- fbs_portal %>% filter(Star_Rating == 5)
# Paired sample one-sided t test 
# H0: mean[Snap_Count_Pre_per_Season] >= mean[Snap_Count_Post_per_Season] ; mean[Snap_Count_Pre_per_Season] - mean[Snap_Count_Post_per_Season] >= 0
# HA: mean[Snap_Count_Pre_per_Season] < mean[Snap_Count_Post_per_Season] ; mean[Snap_Count_Pre_per_Season] - mean[Snap_Count_Post_per_Season] < 0
# Alpha = .05
t.test(five_stars_data$Snap_Count_Pre_per_Season, five_stars_data$Snap_Count_Post_per_Season, paired = TRUE, alternative = 'less')











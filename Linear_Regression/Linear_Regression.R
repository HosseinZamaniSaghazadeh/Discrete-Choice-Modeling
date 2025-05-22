##### Reading Data #####
require('dplyr')
require('car')
options(scipen = 999)
D_HH_HI <- read.csv('survey_household.csv', header = TRUE)

##### Problem 1 #####

#### Data Pre-processing ####
# URBAN = 1, in an urban area
D_HH_HI <- D_HH_HI %>% 
  mutate(URBAN = ifelse(urbrur== 1, 1,0))

# URBHHS
D_HH_HI <- D_HH_HI %>% 
  mutate(URBHHS = URBAN*hhsize)

# URBADL
D_HH_HI <- D_HH_HI %>% 
  mutate(URBADL = URBAN*numadlt)

# URBYOCH
D_HH_HI <- D_HH_HI %>% 
  mutate(URBYOCH = URBAN*youngchild)

# Dummy Variable Interacted to Both Intercept and Slope (m1)
m1 <- lm(cnttdhh ~ hhsize + numadlt + youngchild + URBAN + URBHHS
         + URBADL + URBYOCH, data = D_HH_HI)
summary(m1)

K1<-length(m1$coefficients)
av1<-anova(m1)
av1
SS1<-av1$'Sum Sq'
SSE1<-tail(SS1,n = 1)
SSE1

N1<-round(nrow(D_HH_HI))
df1 <- N1-K1
df1

# Dummy Variable Interacted to Just Intercept (m2)
m2 <- lm(cnttdhh ~ hhsize + numadlt + youngchild + URBAN, data = D_HH_HI)
summary(m2)

K2<-length(m2$coefficients)
av2<-anova(m2)
av2
SS2<-av2$'Sum Sq'
SSE2<-tail(SS2,n = 1)
SSE2

N2<-round(nrow(D_HH_HI))
df2 <- N2-K2
df2

F_test2<-((SSE2-SSE1)/(df2-df1))/(SSE1/df1)
F_test2
F_critical2 <- qf(0.95,df2-df1,df1)
F_critical2

# Dummy Variable Interacted to Just Slope (m3)
m3 <- lm(cnttdhh ~ hhsize + numadlt + youngchild + URBHHS
         + URBADL + URBYOCH, data = D_HH_HI)
summary(m3)

K3<-length(m3$coefficients)
av3<-anova(m3)
av3
SS3<-av3$'Sum Sq'
SSE3<-tail(SS3,n = 1)
SSE3

N3<-round(nrow(D_HH_HI))
df3 <- N3-K3
df3

F_test3<-((SSE3-SSE1)/(df3-df1))/(SSE1/df1)
F_test3
F_critical3 <- qf(0.95,df3-df1,df1)
F_critical3

MSE3 = SSE3/df3
MSE3

# Removing Insignificant Parameters of m3 (m4)
m4 <- lm(cnttdhh ~ hhsize + numadlt, data = D_HH_HI)
summary(m4)

K4<-length(m4$coefficients)
av4<-anova(m4)
av4
SS4<-av4$'Sum Sq'
SSE4<-tail(SS4,n = 1)
SSE4

N4<-round(nrow(D_HH_HI))
df4 <- N4-K4
df4

MSE4 = SSE4/df4
MSE4

F_test4<-((SSE4-SSE3)/(df4-df3))/(SSE3/df3)
F_test4
F_critical4 <- qf(0.95,df4-df3,df3)
F_critical4

#### Market Segmentation Based on Income (Exogenous) ####
D_HH_HI_LOWINC <- subset(D_HH_HI,  hhfaminc_imp <= 5 & hhfaminc_imp >= 1)
D_HH_HI_MEDINC <- subset(D_HH_HI,  hhfaminc_imp <= 8 & hhfaminc_imp >= 6)
D_HH_HI_HIGHINC <- subset(D_HH_HI,  hhfaminc_imp <= 11 & hhfaminc_imp >= 9)

# Low Income Family (m5)
m5 <- lm(cnttdhh ~ hhsize + numadlt + youngchild + URBHHS
         + URBADL + URBYOCH, data = D_HH_HI_LOWINC)
summary(m5)

K5<-length(m5$coefficients)
av5<-anova(m5)
av5
SS5<-av5$'Sum Sq'
SSE5<-tail(SS5,n = 1)
SSE5

N5<-round(nrow(D_HH_HI_LOWINC))
df5 <- N5-K5
df5

MSE5 = SSE5/df5
MSE5

# Medium  Income Family (m6)
m6 <- lm(cnttdhh ~ hhsize + numadlt + youngchild + URBHHS
         + URBADL + URBYOCH, data = D_HH_HI_MEDINC)
summary(m6)

K6<-length(m6$coefficients)
av6<-anova(m6)
av6
SS6<-av6$'Sum Sq'
SSE6<-tail(SS6,n = 1)
SSE6

N6<-round(nrow(D_HH_HI_MEDINC))
df6 <- N6-K6
df6

MSE6 = SSE6/df6
MSE6

# High  Income Family (m7)
m7 <- lm(cnttdhh ~ hhsize + numadlt + youngchild + URBHHS
         + URBADL + URBYOCH, data = D_HH_HI_HIGHINC)
summary(m7)

K7<-length(m7$coefficients)
av7<-anova(m7)
av7
SS7<-av7$'Sum Sq'
SSE7<-tail(SS7,n = 1)
SSE7

N7<-round(nrow(D_HH_HI_HIGHINC))
df7 <- N7-K7
df7

MSE7 = SSE7/df7
MSE7

#### Market Segmentation Test ####
m_segmented <- lm(cnttdhh ~ hhsize + numadlt + youngchild + URBHHS
                  + URBADL + URBYOCH + as.factor(hhfaminc_imp), data = D_HH_HI)
summary(m_segmented)

anova(m3, m_segmented)

##### Problem 2 #####
#### Data Pre-processing ####
# LOWINC = 1, Low Income
D_HH_HI <- D_HH_HI %>% 
  mutate(LOWINC = ifelse(hhfaminc_imp <= 5 & hhfaminc_imp >= 1, 1,0))

# MEDINC = 1, Medium Income
D_HH_HI <- D_HH_HI %>% 
  mutate(MEDINC = ifelse(hhfaminc_imp <= 8 & hhfaminc_imp >= 6, 1,0))

# LOWINCHHS
D_HH_HI <- D_HH_HI %>% 
  mutate(LOWINCHHS = LOWINC*hhsize)

# LOWINCADL
D_HH_HI <- D_HH_HI %>% 
  mutate(LOWINCADL = LOWINC*numadlt)

# LOWINCYOCH
D_HH_HI <- D_HH_HI %>% 
  mutate(LOWINCYOCH = LOWINC*youngchild)

# LOWINCURBHHS
D_HH_HI <- D_HH_HI %>% 
  mutate(LOWINCURBHHS = LOWINC*URBHHS)

# LOWINCURBADL
D_HH_HI <- D_HH_HI %>% 
  mutate(LOWINCURBADL = LOWINC*URBADL)

# LOWINCURBYOCH
D_HH_HI <- D_HH_HI %>% 
  mutate(LOWINCURBYOCH = LOWINC*URBYOCH)

# MEDINCHHS
D_HH_HI <- D_HH_HI %>% 
  mutate(MEDINCHHS = MEDINC*hhsize)

# MEDINCADL
D_HH_HI <- D_HH_HI %>% 
  mutate(MEDINCAD = MEDINC*numadlt)

# MEDINCYOCH
D_HH_HI <- D_HH_HI %>% 
  mutate(MEDINCYOCH = MEDINC*youngchild)

# MEDINCURBHHS
D_HH_HI <- D_HH_HI %>% 
  mutate(MEDINCURBHHS = MEDINC*URBHHS)

# MEDINCURBADL
D_HH_HI <- D_HH_HI %>% 
  mutate(MEDINCURBADL = MEDINC*URBADL)

# MEDINCURBYOCH
D_HH_HI <- D_HH_HI %>% 
  mutate(MEDINCURBYOCH = MEDINC*URBYOCH)

#### Market Segmentation Based on Income (m8-Endogenous) ####
m8 <- lm(cnttdhh ~ hhsize + numadlt + youngchild + URBHHS
         + URBADL + URBYOCH + LOWINC + MEDINC + LOWINCHHS + LOWINCADL
         + LOWINCYOCH + LOWINCURBHHS + LOWINCURBADL + LOWINCURBYOCH
         + MEDINCHHS + MEDINCAD + MEDINCYOCH + MEDINCURBHHS
         + MEDINCURBADL + MEDINCURBYOCH, data = D_HH_HI)
summary(m8)

K8<-length(m8$coefficients)
av8<-anova(m8)
av8
SS8<-av8$'Sum Sq'
SSE8<-tail(SS8,n = 1)
SSE8

N8<-round(nrow(D_HH_HI))
df8 <- N8-K8
df8

MSE8 = SSE8/df8
MSE8

##### Problem 3 #####

#### Data Pre-processing ####
# Creating ADLT0TO4 Column
D_HH_HI <- D_HH_HI %>% 
  mutate(ADLT0TO4 = numadlt + youngchild)

# URBADLT0TO4
D_HH_HI <- D_HH_HI %>% 
  mutate(URBADLT0TO4 = URBAN*ADLT0TO4)

# Dummy Variable Interacted to Both Intercept and Slope (m9)
m9 <- lm(cnttdhh ~ hhsize + ADLT0TO4 + URBAN + URBHHS
         + URBADLT0TO4, data = D_HH_HI)
summary(m9)

K9<-length(m9$coefficients)
av9<-anova(m9)
av9
SS9<-av9$'Sum Sq'
SSE9<-tail(SS9,n = 1)
SSE9

N9<-round(nrow(D_HH_HI))
df9 <- N9-K9
df9

MSE9 = SSE9/df9
MSE9

# Dummy Variable Interacted to Intercept Only (m10)
m10 <- lm(cnttdhh ~ hhsize + ADLT0TO4 + URBAN, data = D_HH_HI)
summary(m10)

K10<-length(m10$coefficients)
av10<-anova(m10)
av10
SS10<-av10$'Sum Sq'
SSE10<-tail(SS10,n = 1)
SSE10

N10<-round(nrow(D_HH_HI))
df10 <- N10-K10
df10

MSE10 = SSE10/df10
MSE10

F_test10<-((SSE10-SSE9)/(df10-df9))/(SSE9/df9)
F_test10
F_critical10 <- qf(0.95,df10-df9,df9)
F_critical10


# Dummy Variable Interacted to Just Slope (m11)
m11 <- lm(cnttdhh ~ hhsize + ADLT0TO4  + URBHHS
         + URBADLT0TO4, data = D_HH_HI)
summary(m11)

K11<-length(m11$coefficients)
av11<-anova(m11)
av11
SS11<-av11$'Sum Sq'
SSE11<-tail(SS11,n = 1)
SSE11

N11<-round(nrow(D_HH_HI))
df11 <- N11-K11
df11

MSE11 = SSE11/df11
MSE11

F_test11<-((SSE11-SSE9)/(df11-df9))/(SSE9/df9)
F_test11
F_critical11 <- qf(0.95,df11-df9,df9)
F_critical11

# Removing Insignificant Parameters of m11 (m12)
m12 <- lm(cnttdhh ~ hhsize + ADLT0TO4, data = D_HH_HI)
summary(m12)

K12<-length(m12$coefficients)
av12<-anova(m12)
av12
SS12<-av12$'Sum Sq'
SSE12<-tail(SS12,n = 1)
SSE12

N12<-round(nrow(D_HH_HI))
df12 <- N12 - K12
df12

MSE12 = SSE12/df12
MSE12

F_test12<-((SSE12-SSE11)/(df12-df11))/(SSE11/df11)
F_test12
F_critical12 <- qf(0.95,df12-df11,df11)
F_critical12

# Since m10 and m11 were close models, we compare m12 (insignificant-parameter-removed version of m10) to m10
F_test12prime<-((SSE12-SSE10)/(df12-df10))/(SSE10/df10)
F_test12prime
F_critical12prime <- qf(0.95,df12-df10,df10)
F_critical12prime

#### Market Segmentation Based on Car Ownership (Exogenous) ####
D_HH_HI_0VEH <- subset(D_HH_HI,  hhvehcnt == 0)
D_HH_HI_1VEH <- subset(D_HH_HI,  hhvehcnt == 1)
D_HH_HI_2PLUSVEH <- subset(D_HH_HI,  hhvehcnt >= 2)

# 0 Vehicle Family (m13)
m13 <- lm(cnttdhh ~ hhsize + ADLT0TO4, data = D_HH_HI_0VEH)
summary(m13)

K13<-length(m13$coefficients)
av13<-anova(m13)
av13
SS13<-av13$'Sum Sq'
SSE13<-tail(SS13,n = 1)
SSE13

N13<-round(nrow(D_HH_HI_0VEH))
df13 <- N13-K13
df13

MSE13 = SSE13/df13
MSE13

# 1 Vehicle Family (m14)
m14 <- lm(cnttdhh ~ hhsize + ADLT0TO4, data = D_HH_HI_1VEH)
summary(m14)

K14<-length(m14$coefficients)
av14<-anova(m14)
av14
SS14<-av14$'Sum Sq'
SSE14<-tail(SS14,n = 1)
SSE14

N14<-round(nrow(D_HH_HI_1VEH))
df14 <- N14-K14
df14

MSE14 = SSE14/df14
MSE14

# Plus 2 Vehicles Family (m15)
m15 <- lm(cnttdhh ~ hhsize + ADLT0TO4, data = D_HH_HI_2PLUSVEH)
summary(m15)

K15<-length(m15$coefficients)
av15<-anova(m15)
av15
SS15<-av15$'Sum Sq'
SSE15<-tail(SS15,n = 1)
SSE15

N15<-round(nrow(D_HH_HI_2PLUSVEH))
df15 <- N15-K15
df15

MSE15 = SSE15/df15
MSE15


#### Market Segmentation Test ####
m_segmented <- lm(cnttdhh ~ hhsize + ADLT0TO4 + as.factor(hhvehcnt), data = D_HH_HI)
summary(m_segmented)

anova(m12, m_segmented)

##### Problem 4 #####
#### Data Pre-processing ####
# NCAR0 = 0
D_HH_HI <- D_HH_HI %>% 
  mutate(NCAR0 = ifelse(hhvehcnt == 0, 1,0))

# NCAR1 = 1
D_HH_HI <- D_HH_HI %>% 
  mutate(NCAR1 = ifelse(hhvehcnt == 1, 1,0))

#  NCAR2 = 1
D_HH_HI <- D_HH_HI %>% 
  mutate(NCAR2 = ifelse(hhvehcnt >= 2, 1,0))

# VEH1HHS
D_HH_HI <- D_HH_HI %>% 
  mutate( VEH1HHS = NCAR1*hhsize)

# VEH1ADLT0TO4
D_HH_HI <- D_HH_HI %>% 
  mutate(VEH1ADLT0TO4 = NCAR1*ADLT0TO4)

# VEHPLUS2HHS
D_HH_HI <- D_HH_HI %>% 
  mutate( VEHPLUS2HHS = NCAR2*hhsize)

# VEHPLUS2ADLT0TO4
D_HH_HI <- D_HH_HI %>% 
  mutate(VEHPLUS2ADLT0TO4 = NCAR2*ADLT0TO4)

#### Market Segmentation Based on Income (Endogenous) ####
m16 <- lm(cnttdhh ~ hhsize + ADLT0TO4 + NCAR1 + NCAR2, data = D_HH_HI)
summary(m16)

K16<-length(m16$coefficients)
av16<-anova(m16)
av16
SS16<-av16$'Sum Sq'
SSE16<-tail(SS16,n = 1)
SSE16

N16<-round(nrow(D_HH_HI))
df16 <- N16-K16
df16

MSE16 = SSE16/df16
MSE16


#### Market Segmentation Based on Car Ownership (Exogenous) - Considering Modell 11 ####
# 0 Vehicle Family (m13prime)
m13prime <- lm(cnttdhh ~ hhsize + ADLT0TO4 + URBHHS, data = D_HH_HI_0VEH)
summary(m13prime)

K13prime<-length(m13prime$coefficients)
av13prime<-anova(m13prime)
av13prime
SS13prime<-av13prime$'Sum Sq'
SSE13prime<-tail(SS13prime,n = 1)
SSE13prime

N13prime<-round(nrow(D_HH_HI_0VEH))
df13prime <- N13prime-K13prime
df13prime

MSE13prime = SSE13prime/df13prime
MSE13prime

# 1 Vehicle Family (m14prime)
m14prime <- lm(cnttdhh ~ hhsize + ADLT0TO4 + URBHHS, data = D_HH_HI_1VEH)
summary(m14prime)

K14prime<-length(m14prime$coefficients)
av14prime<-anova(m14prime)
av14prime
SS14prime<-av14prime$'Sum Sq'
SSE14prime<-tail(SS14prime,n = 1)
SSE14prime

N14prime<-round(nrow(D_HH_HI_1VEH))
df14prime <- N14prime-K14prime
df14prime

MSE14prime = SSE14prime/df14prime
MSE14prime

# Plus 2 Vehicles Family (m15)
m15prime <- lm(cnttdhh ~ hhsize + ADLT0TO4 + URBHHS
               + URBADLT0TO4, data = D_HH_HI_2PLUSVEH)
summary(m15prime)

K15prime<-length(m15prime$coefficients)
av15prime<-anova(m15prime)
av15prime
SS15prime<-av15prime$'Sum Sq'
SSE15prime<-tail(SS15prime,n = 1)
SSE15prime

N15prime<-round(nrow(D_HH_HI_2PLUSVEH))
df15prime <- N15prime-K15prime
df15prime

MSE15prime = SSE15prime/df15prime
MSE15prime

#### Market Segmentation Test ####
m_segmentedprime <- lm(cnttdhh ~ hhsize + ADLT0TO4 + URBHHS
                       + URBADLT0TO4 + as.factor(hhvehcnt), data = D_HH_HI)
summary(m_segmentedprime)

anova(m11, m_segmentedprime)

##### Problem 4 #####
#### Data Pre-processing ####
# VEH1HHS
D_HH_HI <- D_HH_HI %>% 
  mutate( VEH1HHS = NCAR1*hhsize)

# VEH1ADLT0TO4
D_HH_HI <- D_HH_HI %>% 
  mutate(VEH1ADLT0TO4 = NCAR1*ADLT0TO4)

# VEH1URBHHS
D_HH_HI <- D_HH_HI %>% 
  mutate(VEH1URBHHS = NCAR1*URBHHS)

# VEH1URBADLT0TO4
D_HH_HI <- D_HH_HI %>% 
  mutate(VEH1URBADLT0TO4 = NCAR1*URBADLT0TO4)

# VEHPLUS2HHS
D_HH_HI <- D_HH_HI %>% 
  mutate( VEHPLUS2HHS = NCAR2*hhsize)

# VEHPLUS2ADLT0TO4
D_HH_HI <- D_HH_HI %>% 
  mutate(VEHPLUS2ADLT0TO4 = NCAR2*ADLT0TO4)

# VEHPLUS2URBHHS
D_HH_HI <- D_HH_HI %>% 
  mutate(VEHPLUS2URBHHS = NCAR2*URBHHS)

# VEHPLUS2URBADLT0TO4
D_HH_HI <- D_HH_HI %>% 
  mutate(VEHPLUS2URBADLT0TO4 = NCAR2*URBADLT0TO4)

#### Market Segmentation Based on Income (Endogenous) - Considering Modell 11 ####
m16prime <- lm(cnttdhh ~ hhsize + ADLT0TO4 + URBHHS
               + URBADLT0TO4 + NCAR1 + NCAR2, data = D_HH_HI)
summary(m16prime)

K16prime<-length(m16prime$coefficients)
av16prime<-anova(m16prime)
av16prime
SS16prime<-av16prime$'Sum Sq'
SSE16prime<-tail(SS16prime,n = 1)
SSE16prime

N16prime<-round(nrow(D_HH_HI))
df16prime <- N16prime-K16prime
df16prime

MSE16prime = SSE16prime/df16prime
MSE16prime
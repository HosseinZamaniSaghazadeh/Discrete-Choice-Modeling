require(tidyr)
require(plyr)
require(dplyr)
require(mlogit)

###################################### Date Preparation ##########################################
# Loading Raw Dataset
raw_data<-read.csv("MODEBIN3.csv",header = T)

# Create Necessary Variables
raw_data <- raw_data %>% 
  mutate(ALTS = ifelse(UNOTRN == 1,'Train',
                ifelse(UNOSHR == 1,'Shared','Car'))) %>%
  mutate(UNOCAR = 1-UNOTRN-UNOSHR)

raw_data <- raw_data %>% 
  mutate(TDUMMY_TRN = UNOTRN*TDUMMY) %>% 
  mutate(TDUMMY_SHR = UNOSHR*TDUMMY) %>%
  mutate(TDUMMY_CAR = UNOCAR*TDUMMY)

raw_data <- raw_data %>% 
  mutate(HHSIZE_TRN = UNOTRN*HHSIZE) %>% 
  mutate(HHSIZE_SHR = UNOSHR*HHSIZE) %>%
  mutate(HHSIZE_CAR = UNOCAR*HHSIZE)

raw_data <- raw_data %>% 
  mutate(MALE_TRN = UNOTRN*MALE) %>% 
  mutate(MALE_SHR = UNOSHR*MALE) %>%
  mutate(MALE_CAR = UNOCAR*MALE)

raw_data <- raw_data %>% 
  mutate(NUMVEH_TRN = UNOTRN*NUMVEH) %>% 
  mutate(NUMVEH_SHR = UNOSHR*NUMVEH) %>%
  mutate(NUMVEH_CAR = UNOCAR*NUMVEH)

raw_data <- raw_data %>% 
  mutate(WORKERS_TRN = UNOTRN*WORKERS) %>% 
  mutate(WORKERS_SHR = UNOSHR*WORKERS) %>%
  mutate(WORKERS_CAR = UNOCAR*WORKERS)

raw_data <- raw_data %>% 
  mutate(VEHWORK_TRN = UNOTRN*VEHWORK) %>% 
  mutate(VEHWORK_SHR = UNOSHR*VEHWORK) %>%
  mutate(VEHWORK_CAR = UNOCAR*VEHWORK)

raw_data <- raw_data %>% 
  mutate(NEWINC_TRN = UNOTRN*NEWINC/10000) %>% 
  mutate(NEWINC_SHR = UNOSHR*NEWINC/10000) %>%
  mutate(NEWINC_CAR = UNOCAR*NEWINC/10000)

raw_data <- raw_data %>% 
  mutate(AEMPDENS_TRN = UNOTRN*AEMPDENS) %>% 
  mutate(AEMPDENS_SHR = UNOSHR*AEMPDENS) %>%
  mutate(AEMPDENS_CAR = UNOCAR*AEMPDENS)

raw_data <- raw_data %>% 
  mutate(POPDENS_TRN = UNOTRN*POPDENS) %>% 
  mutate(POPDENS_SHR = UNOSHR*POPDENS) %>%
  mutate(POPDENS_CAR = UNOCAR*POPDENS)

raw_data <- raw_data %>% 
  mutate(DIST_TRN = UNOTRN*DIST) %>% 
  mutate(DIST_SHR = UNOSHR*DIST) %>%
  mutate(DIST_CAR = UNOCAR*DIST)

raw_data <- raw_data %>% 
  mutate(AGE_TRN = UNOTRN*AGE) %>% 
  mutate(AGE_SHR = UNOSHR*AGE) %>%
  mutate(AGE_CAR = UNOCAR*AGE)

######################################### Logit Model ##############################################
D <- mlogit.data(raw_data, shape="long", choice="CHOSEN", alt.var = "ALTS")

######################################### Part A ##############################################
### Base Model (Constants Only Model) Without Weights (m1) ###
m1 <- mlogit(CHOSEN~UNOTRN + UNOSHR|0, data = D)
summary(m1)

## Determine Choice Probabilities ##
Pr_Choice_m1<- predict(m1, newdata = D)
apply(Pr_Choice_m1, 2, mean)


### Base Model (Constants Only Model) With Weights (m2) ###
m2 <- mlogit(CHOSEN~UNOTRN + UNOSHR|0, data = D, weights = WEIGHT)
summary(m2)

## Determine Choice Probabilities ##
Pr_Choice_m2<- predict(m2, newdata = D)
apply(Pr_Choice_m2, 2, mean)


### Constants + LOS Model With Weights (IVTT + OVTT + TOTCOST) (m3_1) ###
m3_1 <- mlogit(CHOSEN~UNOTRN + UNOSHR + IVTT + OVTT + TOTCOST|0, data = D, weights = WEIGHT)
summary(m3_1)

## Determine Choice Probabilities ##
Pr_Choice_m3_1<- predict(m3_1, newdata = D)
apply(Pr_Choice_m3_1, 2, mean)

### Comparing m3_1 and m2 ###
## Log-Likelihood ##
LL_m2 = m2$logLik[1]
print(LL_m2)

LL_m3_1 = m3_1$logLik[1]
print(LL_m3_1)

## RHO-Squred ##
RHO_SQ_m3_1 = 1-(LL_m3_1/LL_m2)
print(RHO_SQ_m3_1)

## LRT ##
LR_m3_1 = -2*(LL_m2 - LL_m3_1)
print(LR_m3_1)
qchisq(0.95, df = 3)
lrtest(m2,m3_1)

### Constants + LOS Model With Weights (TOTCOST) (m3_2) ###
m3_2 <- mlogit(CHOSEN~UNOTRN + UNOSHR + TOTCOST|0, data = D, weights = WEIGHT)
summary(m3_2)

## Determine Choice Probabilities ##
Pr_Choice_m3_2<- predict(m3_2, newdata = D)
apply(Pr_Choice_m3_2, 2, mean)

### Comparing m3_2 and m2 ###
## Log-Likelihood ##
LL_m2 = m2$logLik[1]
print(LL_m2)

LL_m3_2 = m3_2$logLik[1]
print(LL_m3_2)

## RHO-Squred ##
RHO_SQ_m3_2 = 1-(LL_m3_2/LL_m2)
print(RHO_SQ_m3_2)

## LRT ##
LR_m3_2 = -2*(LL_m2 - LL_m3_2)
print(LR_m3_2)
qchisq(0.95, df = 1)
lrtest(m2,m3_2)

### Constants + LOS Model With Weights (TOTTIME) (m3_3) ###
m3_3 <- mlogit(CHOSEN~UNOTRN + UNOSHR + TOTTIME|0, data = D, weights = WEIGHT)
summary(m3_3)

## Determine Choice Probabilities ##
Pr_Choice_m3_3<- predict(m3_3, newdata = D)
apply(Pr_Choice_m3_3, 2, mean)

### Comparing m3_3 and m2 ###
## Log-Likelihood ##
LL_m2 = m2$logLik[1]
print(LL_m2)

LL_m3_3 = m3_3$logLik[1]
print(LL_m3_3)

## RHO-Squred ##
RHO_SQ_m3_3 = 1-(LL_m3_3/LL_m2)
print(RHO_SQ_m3_3)

## LRT ##
LR_m3_3 = -2*(LL_m2 - LL_m3_3)
print(LR_m3_3)
qchisq(0.95, df = 1)
lrtest(m2,m3_3)

### Constants + LOS Model With Weights (IVTT + OVTT) (m3_4) ###
m3_4 <- mlogit(CHOSEN~UNOTRN + UNOSHR + IVTT + OVTT|0, data = D, weights = WEIGHT)
summary(m3_4)

## Determine Choice Probabilities ##
Pr_Choice_m3_4<- predict(m3_4, newdata = D)
apply(Pr_Choice_m3_4, 2, mean)

### Comparing m3_4 and m2 ###
## Log-Likelihood ##
LL_m2 = m2$logLik[1]
print(LL_m2)

LL_m3_4 = m3_4$logLik[1]
print(LL_m3_4)

## RHO-Squred ##
RHO_SQ_m3_4 = 1-(LL_m3_4/LL_m2)
print(RHO_SQ_m3_4)

## LRT ##
LR_m3_4 = -2*(LL_m2 - LL_m3_4)
print(LR_m3_4)
qchisq(0.95, df = 2)
lrtest(m2,m3_4)

### Constants + LOS Model With Weights (TOTTIME + TOTCOST) (m3_5) ###
m3_5 <- mlogit(CHOSEN~UNOTRN + UNOSHR + TOTTIME + TOTCOST|0, data = D, weights = WEIGHT)
summary(m3_5)

## Determine Choice Probabilities ##
Pr_Choice_m3_5<- predict(m3_5, newdata = D)
apply(Pr_Choice_m3_5, 2, mean)

### Comparing m3_5 and m2 ###
## Log-Likelihood ##
LL_m2 = m2$logLik[1]
print(LL_m2)

LL_m3_5 = m3_5$logLik[1]
print(LL_m3_5)

## RHO-Squred ##
RHO_SQ_m3_5 = 1-(LL_m3_5/LL_m2)
print(RHO_SQ_m3_5)

## LRT ##
LR_m3_5 = -2*(LL_m2 - LL_m3_5)
print(LR_m3_5)
qchisq(0.95, df = 2)
lrtest(m2,m3_5)

### LRT Between m3_1 and m3_4###
lrtest(m3_4,m3_1)

######################################### Part B ##############################################
### Constants + LOS Model With Weights (TOTTIME + TOTCOST) + ASV (NUMVEH_TRN + NUMVEH_SHR) (m4) ###
m4 <- mlogit(CHOSEN~UNOTRN + UNOSHR + TOTTIME + TOTCOST + NUMVEH_TRN + NUMVEH_SHR|0, data = D, weights = WEIGHT)
summary(m4)

## Determine Choice Probabilities ##
Pr_Choice_m4<- predict(m4, newdata = D)
apply(Pr_Choice_m4, 2, mean)

### Comparing m3_5 and m4 ###
## LRT ##
qchisq(0.95, df = 2)
lrtest(m3_5,m4)

### Constants + LOS Model With Weights (TOTTIME + TOTCOST) + ASV (VEHWORK_TRN + VEHWORK_SHR) (m5) ###
m5 <- mlogit(CHOSEN~UNOTRN + UNOSHR + TOTTIME + TOTCOST + VEHWORK_TRN + VEHWORK_SHR|0, data = D, weights = WEIGHT)
summary(m5)

## Determine Choice Probabilities ##
Pr_Choice_m5<- predict(m5, newdata = D)
apply(Pr_Choice_m5, 2, mean)

### Comparing m3_5 and m5 ###
## LRT ##
qchisq(0.95, df = 2)
lrtest(m3_5,m5)


######################################### Part C ##############################################
### Constants + LOS Model + ASV With Weights (m6_1) ###
m6_1 <- mlogit(CHOSEN~UNOTRN + UNOSHR + TOTTIME + TOTCOST + VEHWORK_TRN + VEHWORK_SHR
               + TDUMMY_TRN + TDUMMY_SHR|0, data = D, weights = WEIGHT)
summary(m6_1)

## Determine Choice Probabilities ##
Pr_Choice_m6_1<- predict(m6_1, newdata = D)
apply(Pr_Choice_m6_1, 2, mean)

### Comparing m5 and m6_1 ###
## LRT ##
qchisq(0.95, df = 2)
lrtest(m5,m6_1)

### Constants + LOS Model + ASV With Weights (m6_2) ###
m6_2 <- mlogit(CHOSEN~UNOTRN + UNOSHR + TOTTIME + TOTCOST + VEHWORK_TRN + VEHWORK_SHR
               + HHSIZE_TRN + HHSIZE_SHR|0, data = D, weights = WEIGHT)
summary(m6_2)

## Determine Choice Probabilities ##
Pr_Choice_m6_2<- predict(m6_2, newdata = D)
apply(Pr_Choice_m6_2, 2, mean)

### Comparing m5 and m6_2 ###
## LRT ##
qchisq(0.95, df = 2)
lrtest(m5,m6_2)


### Constants + LOS Model + ASV With Weights (m6_3) ###
m6_3 <- mlogit(CHOSEN~UNOTRN + UNOSHR + TOTTIME + TOTCOST + VEHWORK_TRN + VEHWORK_SHR
               + MALE_TRN + MALE_SHR|0, data = D, weights = WEIGHT)
summary(m6_3)

## Determine Choice Probabilities ##
Pr_Choice_m6_3<- predict(m6_3, newdata = D)
apply(Pr_Choice_m6_3, 2, mean)

### Comparing m5 and m6_3 ###
## LRT ##
qchisq(0.95, df = 2)
lrtest(m5,m6_3)


### Constants + LOS Model + ASV With Weights (m6_4) ###
m6_4 <- mlogit(CHOSEN~UNOTRN + UNOSHR + TOTTIME + TOTCOST + VEHWORK_TRN + VEHWORK_SHR
               + NEWINC_TRN + NEWINC_SHR|0, data = D, weights = WEIGHT)
summary(m6_4)

## Determine Choice Probabilities ##
Pr_Choice_m6_4<- predict(m6_4, newdata = D)
apply(Pr_Choice_m6_4, 2, mean)

### Comparing m5 and m6_4 ###
## LRT ##
qchisq(0.95, df = 2)
lrtest(m5,m6_4)


### Constants + LOS Model + ASV With Weights (m6_5) ###
m6_5 <- mlogit(CHOSEN~UNOTRN + UNOSHR + TOTTIME + TOTCOST + VEHWORK_TRN + VEHWORK_SHR
               + AEMPDENS_TRN + AEMPDENS_SHR|0, data = D, weights = WEIGHT)
summary(m6_5)

## Determine Choice Probabilities ##
Pr_Choice_m6_5<- predict(m6_5, newdata = D)
apply(Pr_Choice_m6_5, 2, mean)

### Comparing m5 and m6_5 ###
## LRT ##
qchisq(0.95, df = 2)
lrtest(m5,m6_5)


### Constants + LOS Model + ASV With Weights (m6_6) ###
m6_6 <- mlogit(CHOSEN~UNOTRN + UNOSHR + TOTTIME + TOTCOST + VEHWORK_TRN + VEHWORK_SHR
               + POPDENS_TRN + POPDENS_SHR|0, data = D, weights = WEIGHT)
summary(m6_6)

## Determine Choice Probabilities ##
Pr_Choice_m6_6<- predict(m6_6, newdata = D)
apply(Pr_Choice_m6_6, 2, mean)

### Comparing m5 and m6_6 ###
## LRT ##
qchisq(0.95, df = 2)
lrtest(m5,m6_6)

### Constants + LOS Model + ASV With Weights (m6_7) ###
m6_7 <- mlogit(CHOSEN~UNOTRN + UNOSHR + TOTTIME + TOTCOST + VEHWORK_TRN + VEHWORK_SHR
               + DIST_TRN + DIST_SHR|0, data = D, weights = WEIGHT)
summary(m6_7)

## Determine Choice Probabilities ##
Pr_Choice_m6_7<- predict(m6_7, newdata = D)
apply(Pr_Choice_m6_7, 2, mean)

### Comparing m5 and m6_7 ###
## LRT ##
qchisq(0.95, df = 2)
lrtest(m5,m6_7)


### Constants + LOS Model + ASV With Weights (m6_8) ###
m6_8 <- mlogit(CHOSEN~UNOTRN + UNOSHR + TOTTIME + TOTCOST + VEHWORK_TRN + VEHWORK_SHR
               + AGE_TRN + AGE_SHR|0, data = D, weights = WEIGHT)
summary(m6_8)

## Determine Choice Probabilities ##
Pr_Choice_m6_8<- predict(m6_8, newdata = D)
apply(Pr_Choice_m6_8, 2, mean)

### Comparing m5 and m6_8 ###
## LRT ##
qchisq(0.95, df = 2)
lrtest(m5,m6_8)

### Constants + LOS Model + ASV With Weights (m6_7_1) ###
m6_7_1 <- mlogit(CHOSEN~UNOTRN + UNOSHR + TOTTIME + TOTCOST + VEHWORK_TRN + VEHWORK_SHR
               + DIST_TRN + DIST_SHR + TDUMMY_TRN + TDUMMY_SHR|0, data = D, weights = WEIGHT)
summary(m6_7_1)

## Determine Choice Probabilities ##
Pr_Choice_m6_7_1<- predict(m6_7_1, newdata = D)
apply(Pr_Choice_m6_7_1, 2, mean)

### Comparing m5 and m6_7_1 ###
## LRT ##
qchisq(0.95, df = 2)
lrtest(m6_7,m6_7_1)


### Constants + LOS Model + ASV With Weights (m6_7_1_1) ###
m6_7_1_1 <- mlogit(CHOSEN~UNOTRN + UNOSHR + TOTTIME + TOTCOST + VEHWORK_TRN + VEHWORK_SHR
                 + DIST_TRN + DIST_SHR + TDUMMY_TRN + TDUMMY_SHR + AEMPDENS_TRN + AEMPDENS_SHR|0, data = D, weights = WEIGHT)
summary(m6_7_1_1)

## Determine Choice Probabilities ##
Pr_Choice_m6_7_1_1<- predict(m6_7_1_1, newdata = D)
apply(Pr_Choice_m6_7_1_1, 2, mean)

### Comparing m5 and m6_7_1_1 ###
## LRT ##
qchisq(0.95, df = 2)
lrtest(m6_7_1,m6_7_1_1)


######################################### Part D ##############################################

### Constants + LOS Model + ASV With Weights (m6_7_1_2) ###
m6_7_1_2 <- mlogit(CHOSEN~UNOTRN + UNOSHR + IVTT + OVTT + TOTCOST + VEHWORK_TRN + VEHWORK_SHR
                 + DIST_TRN + DIST_SHR + TDUMMY_TRN + TDUMMY_SHR|0, data = D, weights = WEIGHT)
summary(m6_7_1_2)

## Determine Choice Probabilities ##
Pr_Choice_m6_7_1_2<- predict(m6_7_1_2, newdata = D)
apply(Pr_Choice_m6_7_1_2, 2, mean)

## Computing Marginal Effect of LOS Variables ##
Beta_IVTT <- coef(m6_7_1_2)["IVTT"]
Beta_OVTT <- coef(m6_7_1_2)["OVTT"]
Beta_TOTCOST <- coef(m6_7_1_2)["TOTCOST"]

# Compute marginal effect per alternative: β × P × (1 - P)
mfx_IVTT <- (Pr_Choice_m6_7_1_2 * (1 - Pr_Choice_m6_7_1_2)) * Beta_IVTT
mfx_OVTT <- (Pr_Choice_m6_7_1_2 * (1 - Pr_Choice_m6_7_1_2)) * Beta_OVTT
mfx_TOTCOST <- (Pr_Choice_m6_7_1_2 * (1 - Pr_Choice_m6_7_1_2)) * Beta_TOTCOST

# Average marginal effect for each mode
marginal_effect_IVTT <- colMeans(mfx_IVTT)
marginal_effect_OVTT <- colMeans(mfx_OVTT)
marginal_effect_TOTCOST <- colMeans(mfx_TOTCOST)

# Display result
print(round(marginal_effect_IVTT, 6))
print(round(marginal_effect_OVTT, 6))
print(round(marginal_effect_TOTCOST, 6))

## Computing Elasticity
# Means of the variables
mean_IVTT <- mean(D$IVTT)
mean_OVTT <- mean(D$OVTT)
mean_TOTCOST <- mean(D$TOTCOST)

# Mean predicted probabilities for each mode
mean_Pr <- colMeans(Pr_Choice_m6_7_1_2)

# Elasticities
elasticity_IVTT <- (marginal_effect_IVTT * mean_IVTT) / mean_Pr
elasticity_OVTT <- (marginal_effect_OVTT * mean_OVTT) / mean_Pr
elasticity_TOTCOST <- (marginal_effect_TOTCOST * mean_TOTCOST) / mean_Pr

# Display results
print(round(elasticity_IVTT, 6))
print(round(elasticity_OVTT, 6))
print(round(elasticity_TOTCOST, 6))

######################################### Part E ##############################################
D_policy <- D

# Reduce IVTT by 25% for Train and Shared alternatives
D_policy$IVTT[D_policy$alt %in% c("Train", "Shared")] <- 
  D_policy$IVTT[D_policy$alt %in% c("Train", "Shared")] * 0.75

# Increase IVTT by 30% for Auto (Car)
D_policy$IVTT[D_policy$alt == "Car"] <- 
  D_policy$IVTT[D_policy$alt == "Car"] * 1.30

## Determine Choice Probabilities ##
Pr_Choice_m6_7_1_2_policy <- predict(m6_7_1_2, newdata = D_policy)

# Original (base case)
base_shares <- colMeans(Pr_Choice_m6_7_1_2)

# New (after policy)
policy_shares <- colMeans(Pr_Choice_m6_7_1_2_policy)

# Change in shares
delta_shares <- policy_shares - base_shares

# Combine into a data frame
share_comparison <- data.frame(
  Mode = names(base_shares),
  Base = round(base_shares, 4),
  Policy = round(policy_shares, 4),
  Change = round(delta_shares, 4)
)

print(share_comparison)



###################################### Nested Logit ##########################################
## Multinomial Logit Model
m7 <- mlogit(CHOSEN~IVTT + OVTT + TOTCOST|VEHWORK + HHSIZE + TDUMMY, data = D, reflevel = 'Car', 
             weights = WEIGHT)
summary(m7)

## Nested Logit Structure 1
n1 <- mlogit(CHOSEN~IVTT + OVTT + TOTCOST|VEHWORK + HHSIZE + TDUMMY, data = D, reflevel = 'Car', 
             weights = WEIGHT, nests = list(Solo = c('Car'), Group = c('Train','Shared')), 
             un.nest.el = TRUE)
summary(n1)

#t-test
(coef(n1)['iv'] - 1) / sqrt(vcov(n1)['iv', 'iv'])

# LL Testing
qchisq(0.95, df = 1)
lrtest(m7,n1)


## Nested Logit Structure 2
n2 <- mlogit(CHOSEN~IVTT + OVTT + TOTCOST|VEHWORK + HHSIZE + TDUMMY, data = D, reflevel = 'Car', 
             weights = WEIGHT, nests = list(Not_shared = c('Car', 'Train'), Shared = c('Shared')), 
             un.nest.el = TRUE)
summary(n2)

#t-test
(coef(n2)['iv'] - 1) / sqrt(vcov(n2)['iv', 'iv'])

# LL Testing
qchisq(0.95, df = 1)
lrtest(m7,n2)


## Nested Logit Structure 3
n3 <- mlogit(CHOSEN~IVTT + OVTT + TOTCOST|VEHWORK + HHSIZE + TDUMMY, data = D, reflevel = 'Car', 
             weights = WEIGHT, nests = list(Auto = c('Car', 'Shared'), Transit = c('Train')), 
             un.nest.el = TRUE)
summary(n3)

#t-test
(coef(n3)['iv'] - 1) / sqrt(vcov(n3)['iv', 'iv'])

# LL Testing
qchisq(0.95, df = 1)
lrtest(m7,n3)


###################################### m8 #########################################
## Multinomial Logit Model
m8 <- mlogit(CHOSEN~IVTT + OVTT + TOTCOST|VEHWORK + HHSIZE + TDUMMY + DIST, 
             data = D, reflevel = 'Car', weights = WEIGHT)
summary(m8)

## Nested Logit Structure 1
n8_1 <- mlogit(CHOSEN~IVTT + OVTT + TOTCOST|VEHWORK + HHSIZE + TDUMMY + DIST, 
               data = D, reflevel = 'Car', weights = WEIGHT, 
               nests = list(Solo = c('Car'), Group = c('Train','Shared')), 
             un.nest.el = TRUE)
summary(n8_1)

#t-test
(coef(n8_1)['iv'] - 1) / sqrt(vcov(n8_1)['iv', 'iv'])

# LL Testing
qchisq(0.95, df = 1)
lrtest(m8,n8_1)


## Nested Logit Structure 2
n8_2 <- mlogit(CHOSEN~IVTT + OVTT + TOTCOST|VEHWORK + HHSIZE + TDUMMY + DIST, 
               data = D, reflevel = 'Car', 
             weights = WEIGHT, nests = list(Not_shared = c('Car', 'Train'), Shared = c('Shared')), 
             un.nest.el = TRUE)
summary(n8_2)

#t-test
(coef(n8_2)['iv'] - 1) / sqrt(vcov(n8_2)['iv', 'iv'])

# LL Testing
qchisq(0.95, df = 1)
lrtest(m8,n8_2)


## Nested Logit Structure 3
n8_3 <- mlogit(CHOSEN~IVTT + OVTT + TOTCOST|VEHWORK + HHSIZE + TDUMMY + DIST, 
             data = D, reflevel = 'Car', 
             weights = WEIGHT, nests = list(Auto = c('Car', 'Shared'), Transit = c('Train')), 
             un.nest.el = TRUE)
summary(n8_3)

#t-test
(coef(n8_3)['iv'] - 1) / sqrt(vcov(n8_3)['iv', 'iv'])

# LL Testing
qchisq(0.95, df = 1)
lrtest(m8,n8_3)


###################################### m9 #########################################
## Multinomial Logit Model
m9 <- mlogit(CHOSEN~IVTT + OVTT + TOTCOST|VEHWORK + HHSIZE + TDUMMY + DIST + MALE, 
             data = D, reflevel = 'Car', weights = WEIGHT)
summary(m9)

## Nested Logit Structure 1
n9_1 <- mlogit(CHOSEN~IVTT + OVTT + TOTCOST|VEHWORK + HHSIZE + TDUMMY + DIST + MALE, 
               data = D, reflevel = 'Car', weights = WEIGHT, 
               nests = list(Solo = c('Car'), Group = c('Train','Shared')), 
               un.nest.el = TRUE)
summary(n9_1)

#t-test
(coef(n9_1)['iv'] - 1) / sqrt(vcov(n9_1)['iv', 'iv'])

# LL Testing
qchisq(0.95, df = 1)
lrtest(m9,n9_1)


## Nested Logit Structure 2
n9_2 <- mlogit(CHOSEN~IVTT + OVTT + TOTCOST|VEHWORK + HHSIZE + TDUMMY + DIST + MALE, 
               data = D, reflevel = 'Car', 
               weights = WEIGHT, nests = list(Not_shared = c('Car', 'Train'), Shared = c('Shared')), 
               un.nest.el = TRUE)
summary(n9_2)

#t-test
(coef(n9_2)['iv'] - 1) / sqrt(vcov(n9_2)['iv', 'iv'])

# LL Testing
qchisq(0.95, df = 1)
lrtest(m9,n9_2)


## Nested Logit Structure 3
n9_3 <- mlogit(CHOSEN~IVTT + OVTT + TOTCOST|VEHWORK + HHSIZE + TDUMMY + DIST + MALE, 
               data = D, reflevel = 'Car', 
               weights = WEIGHT, nests = list(Auto = c('Car', 'Shared'), Transit = c('Train')), 
               un.nest.el = TRUE)
summary(n9_3)

#t-test
(coef(n9_3)['iv'] - 1) / sqrt(vcov(n9_3)['iv', 'iv'])

# LL Testing
qchisq(0.95, df = 1)
lrtest(m9,n9_3)


###################################### m10 #########################################
## Multinomial Logit Model
m10 <- mlogit(CHOSEN~IVTT + OVTT + TOTCOST|VEHWORK + HHSIZE + TDUMMY + DIST + MALE + NEWINC, 
             data = D, reflevel = 'Car', weights = WEIGHT)
summary(m10)

## Nested Logit Structure 1
n10_1 <- mlogit(CHOSEN~IVTT + OVTT + TOTCOST|VEHWORK + HHSIZE + TDUMMY + DIST + MALE + NEWINC, 
               data = D, reflevel = 'Car', weights = WEIGHT, 
               nests = list(Solo = c('Car'), Group = c('Train','Shared')), 
               un.nest.el = TRUE)
summary(n10_1)

#t-test
(coef(n10_1)['iv'] - 1) / sqrt(vcov(n10_1)['iv', 'iv'])

# LL Testing
qchisq(0.95, df = 1)
lrtest(m10,n10_1)


## Nested Logit Structure 2
n10_2 <- mlogit(CHOSEN~IVTT + OVTT + TOTCOST|VEHWORK + HHSIZE + TDUMMY + DIST + MALE + NEWINC, 
               data = D, reflevel = 'Car', 
               weights = WEIGHT, nests = list(Not_shared = c('Car', 'Train'), Shared = c('Shared')), 
               un.nest.el = TRUE)
summary(n10_2)

#t-test
(coef(n10_2)['iv'] - 1) / sqrt(vcov(n10_2)['iv', 'iv'])

# LL Testing
qchisq(0.95, df = 1)
lrtest(m10,n10_2)


## Nested Logit Structure 3
n10_3 <- mlogit(CHOSEN~IVTT + OVTT + TOTCOST|VEHWORK + HHSIZE + TDUMMY + DIST + MALE + NEWINC, 
               data = D, reflevel = 'Car', 
               weights = WEIGHT, nests = list(Auto = c('Car', 'Shared'), Transit = c('Train')), 
               un.nest.el = TRUE)
summary(n10_3)

#t-test
(coef(n10_3)['iv'] - 1) / sqrt(vcov(n10_3)['iv', 'iv'])

# LL Testing
qchisq(0.95, df = 1)
lrtest(m10,n10_3)


###################################### m11 #########################################
## Multinomial Logit Model
m11 <- mlogit(CHOSEN~IVTT + OVTT + TOTCOST|VEHWORK + HHSIZE + TDUMMY + DIST + MALE + 
               NEWINC + AEMPDENS, 
             data = D, reflevel = 'Car', weights = WEIGHT)
summary(m11)

## Nested Logit Structure 1
n11_1 <- mlogit(CHOSEN~IVTT + OVTT + TOTCOST|VEHWORK + HHSIZE + TDUMMY + DIST + MALE + 
                 NEWINC + AEMPDENS, 
               data = D, reflevel = 'Car', weights = WEIGHT, 
               nests = list(Solo = c('Car'), Group = c('Train','Shared')), 
               un.nest.el = TRUE)
summary(n11_1)

#t-test
(coef(n11_1)['iv'] - 1) / sqrt(vcov(n11_1)['iv', 'iv'])

# LL Testing
qchisq(0.95, df = 1)
lrtest(m11,n11_1)


## Nested Logit Structure 2
n11_2 <- mlogit(CHOSEN~IVTT + OVTT + TOTCOST|VEHWORK + HHSIZE + TDUMMY + DIST + MALE + 
                 NEWINC + AEMPDENS, 
               data = D, reflevel = 'Car', 
               weights = WEIGHT, nests = list(Not_shared = c('Car', 'Train'), Shared = c('Shared')), 
               un.nest.el = TRUE)
summary(n11_2)

#t-test
(coef(n11_2)['iv'] - 1) / sqrt(vcov(n11_2)['iv', 'iv'])

# LL Testing
qchisq(0.95, df = 1)
lrtest(m11,n11_2)


## Nested Logit Structure 3
n11_3 <- mlogit(CHOSEN~IVTT + OVTT + TOTCOST|VEHWORK + HHSIZE + TDUMMY + DIST + MALE + 
                 NEWINC + AEMPDENS, 
               data = D, reflevel = 'Car', 
               weights = WEIGHT, nests = list(Auto = c('Car', 'Shared'), Transit = c('Train')), 
               un.nest.el = TRUE)
summary(n11_3)

#t-test
(coef(n11_3)['iv'] - 1) / sqrt(vcov(n11_3)['iv', 'iv'])

# LL Testing
qchisq(0.95, df = 1)
lrtest(m11,n11_3)


###################################### m12 #########################################
## Multinomial Logit Model
m12 <- mlogit(CHOSEN~IVTT + OVTT + TOTCOST|VEHWORK + HHSIZE + TDUMMY + DIST + MALE + 
               NEWINC + AEMPDENS + POPDENS, 
             data = D, reflevel = 'Car', weights = WEIGHT)
summary(m12)

## Nested Logit Structure 1
n12_1 <- mlogit(CHOSEN~IVTT + OVTT + TOTCOST|VEHWORK + HHSIZE + TDUMMY + DIST + MALE + 
                 NEWINC + AEMPDENS + POPDENS, 
               data = D, reflevel = 'Car', weights = WEIGHT, 
               nests = list(Solo = c('Car'), Group = c('Train','Shared')), 
               un.nest.el = TRUE)
summary(n12_1)

#t-test
(coef(n12_1)['iv'] - 1) / sqrt(vcov(n12_1)['iv', 'iv'])

# LL Testing
qchisq(0.95, df = 1)
lrtest(m12,n12_1)


## Nested Logit Structure 2
n12_2 <- mlogit(CHOSEN~IVTT + OVTT + TOTCOST|VEHWORK + HHSIZE + TDUMMY + DIST + MALE + 
                 NEWINC + AEMPDENS + POPDENS, 
               data = D, reflevel = 'Car', 
               weights = WEIGHT, nests = list(Not_shared = c('Car', 'Train'), Shared = c('Shared')), 
               un.nest.el = TRUE)
summary(n12_2)

#t-test
(coef(n12_2)['iv'] - 1) / sqrt(vcov(n12_2)['iv', 'iv'])

# LL Testing
qchisq(0.95, df = 1)
lrtest(m12,n12_2)


## Nested Logit Structure 3
n12_3 <- mlogit(CHOSEN~IVTT + OVTT + TOTCOST|VEHWORK + HHSIZE + TDUMMY + DIST + MALE + 
                 NEWINC + AEMPDENS + POPDENS, 
               data = D, reflevel = 'Car', 
               weights = WEIGHT, nests = list(Auto = c('Car', 'Shared'), Transit = c('Train')), 
               un.nest.el = TRUE)
summary(n12_3)

#t-test
(coef(n12_3)['iv'] - 1) / sqrt(vcov(n12_3)['iv', 'iv'])

# LL Testing
qchisq(0.95, df = 1)
lrtest(m12,n12_3)


###################################### m13 #########################################
## Multinomial Logit Model
m13 <- mlogit(CHOSEN~IVTT + OVTT + TOTCOST|VEHWORK + HHSIZE + TDUMMY + DIST + MALE + 
               NEWINC + AEMPDENS + POPDENS + AGE, 
             data = D, reflevel = 'Car', weights = WEIGHT)
summary(m13)

## Nested Logit Structure 1
n13_1 <- mlogit(CHOSEN~IVTT + OVTT + TOTCOST|VEHWORK + HHSIZE + TDUMMY + DIST + MALE + 
                 NEWINC + AEMPDENS + POPDENS + AGE, 
               data = D, reflevel = 'Car', weights = WEIGHT, 
               nests = list(Solo = c('Car'), Group = c('Train','Shared')), 
               un.nest.el = TRUE)
summary(n13_1)

#t-test
(coef(n13_1)['iv'] - 1) / sqrt(vcov(n13_1)['iv', 'iv'])

# LL Testing
qchisq(0.95, df = 1)
lrtest(m13,n13_1)


## Nested Logit Structure 2
n13_2 <- mlogit(CHOSEN~IVTT + OVTT + TOTCOST|VEHWORK + HHSIZE + TDUMMY + DIST + MALE + 
                 NEWINC + AEMPDENS + POPDENS + AGE, 
               data = D, reflevel = 'Car', 
               weights = WEIGHT, nests = list(Not_shared = c('Car', 'Train'), Shared = c('Shared')), 
               un.nest.el = TRUE)
summary(n13_2)

#t-test
(coef(n13_2)['iv'] - 1) / sqrt(vcov(n13_2)['iv', 'iv'])

# LL Testing
qchisq(0.95, df = 1)
lrtest(m13,n13_2)


## Nested Logit Structure 3
n13_3 <- mlogit(CHOSEN~IVTT + OVTT + TOTCOST|VEHWORK + HHSIZE + TDUMMY + DIST + MALE + 
                 NEWINC + AEMPDENS + POPDENS + AGE, 
               data = D, reflevel = 'Car', 
               weights = WEIGHT, nests = list(Auto = c('Car', 'Shared'), Transit = c('Train')), 
               un.nest.el = TRUE)
summary(n13_3)

#t-test
(coef(n13_3)['iv'] - 1) / sqrt(vcov(n13_3)['iv', 'iv'])

# LL Testing
qchisq(0.95, df = 1)
lrtest(m13,n13_3)

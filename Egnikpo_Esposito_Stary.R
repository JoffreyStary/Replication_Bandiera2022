#######################################################################
############ RESEARCH METHODOLOGY - REPLICATION CODE PART  ############
#######################################################################

# =================================================================================
# For original paper 
#
# Replicate Paper : Social Incentives, Delivery Agents, 
#           and the Effectiveness of Development Interventions
#
# Referencees : Bandiera, O., Burgess, R., Deserranno, E., Morel, R., 
#           Sulaiman, M., & Rasul, I. (2023). Social incentives, delivery agents, 
#           and the effectiveness of development interventions. 
#           Journal of Political Economy Microeconomics, 1(1), 000-000.
#
# =================================================================================
# For replication exercise
#
# Authors : Edem EGNIKPO & Mathilde ESPOSITO & Joffrey STARY
# Date : February 22, 2023
# 
# Data Base : Replication Package :
# 				https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/T2WBLK
#
# =================================================================================



######## PART. 1
# NB : This code is divided into 2 parts. In each part, one must reload 
#  the data by setting its own path to the data.


#0.1 Necessary packages
install.packages("haven")
library(haven)
install.packages("cobalt")
library(cobalt)
install.packages('glmnet')
library(glmnet)
install.packages("mice")
library(mice)
install.packages('corrr')
library(corrr)
install.packages('tydiverse')
library(tidyverse)
install.packages("corrplot")
library(corrplot)
install.packages('MatchIt')
library(MatchIt)
install.packages('stargazer')
library('stargazer')

#0.2 Importing dataset
TandC.dta <- read_dta("/Users/mathildeesposito/Desktop/dataverse_files/SocialIncentives_Data_TandC.dta")
Tonly.dta <- read_dta("/Users/mathildeesposito/Desktop/dataverse_files/SocialIncentives_Data_Tonly.dta")
str(TandC.dta)
summary(TandC.dta)
nrow(TandC.dta)
str(Tonly.dta)
summary(Tonly.dta)
nrow(Tonly.dta)

# 0.3 Preliminary to Lasso (glmnet doesn't handle missing values)
md.pattern(TandC.dta) # There are only 10 obs. with no missing values
TandC.dta <- makeX(TandC.dta)
imputed_TandC <- mice(TandC.dta, m=1, maxit=1, method = 'pmm', seed = 500) # We do imputation using predictive mean matching
# We compute only 1 iteration given the length of the execution of mice
summary(imputed_TandC)
# We need to convert the mids object into a dataframe.
# We convert it to a long-format data.frame in which the original data and all imputed datasets are stacked onto each other
imputed_TandC <- complete(imputed_TandC)

# 0.4 Alternative method: suppress NA obs.
TandC.complete <- TandC.dta[complete.cases(TandC.dta), ]
outcome <- TandC.dta[,c('group4')] 
outcome <- as.data.frame(outcome)
TandC.complete <- as.data.frame(TandC.complete)
train <- TandC.complete[,-11]
trainX <- as.matrix(train)
typeof(outcome)
outcomeX <- as.matrix(outcome)

#0.3 Imputation of missing values before Lasso
TandC.dta[is.na(TandC.dta)] <- 0
outcome <- TandC.dta[,c('group4')] 
train <- TandC.dta[,-11]

#0.4 Checking the correlation before Lasso
M <- cor(TandC.dta)
M
head(round(M,2))
corrplot(M, method="circle", tl.cex = 0.2, type = 'upper') #not so strong we can apply Lasso

#subset rows
TandC.dta_H <- TandC.dta %>%
  select(ends_with('_B'))

# converting into matrices for glmnet
TandC.dta_H <- as.matrix(TandC.dta_H)
outcome <- makeX(outcome)
print(typeof(TandC.dta_H))
print(typeof(outcome))

#1. Lasso Regression on Household characteristics
lasso <- glmnet(TandC.dta_H, outcome, alpha = 1)
summary(lasso)
## Choosing optimal value for lambda by K-fold CV
cv_lasso <- cv.glmnet(TandC.dta_H, outcome, alpha = 1) 
## finding optimal lambda value that minimizes test MSE
best_lambda <- cv_lasso$lambda.min
best_lambda
## finding coefficients of best model
best_lasso <- glmnet(TandC.dta_H, outcome, alpha = 1, lambda = best_lambda) #playing with the lambda
lasso.coef <- coef(best_lasso)
lasso.coef
lasso.coef <- lasso.coef[lasso.coef!=0]
lasso.coef
par(mfrow = c(1,2))
plot(cv_lasso)
plot(lasso, xvar = "lambda", main = 'Lasso')

# acres_owned_B not selected, as adopt_mixed_cropping_ever_B, nb_mktcrops_B, netoutput2se_B
# However, it adds total_value_agri_assets2se_B, expenditures2se_B, age_B...

#2. Balance test on Household Characteristics (Table 1)
## Checking for balance across all variables that are strongly correlated with the outcome of interest
covs <- subset(TandC.dta, select = c(primary_B, acres_owned_B, wealth_index_B,
                                     consumption_food2se_B, know_seeds_B, belief_seeds_B,
                                     adopt_seeds_ever_B, nb_techniques_known_B, nb_techniques_belief_B,
                                     adopt_tech_ever_B, adopt_mixed_cropping_ever_B, hours_agri_day_B,
                                     acres_cultivated_B, nb_cropsN_B, nb_mktcrops_B,
                                     share_output_sold_B, netoutput2se_B))
bal.tab_1 <- bal.tab(covs, treat = TandC.dta$agri,  disp = c("means", "sds"))
print(bal.tab_1)

#2.bis Balance test on Household Characteristics (Table 1 of our report)
## Checking for balance across all variables that are strongly correlated with the outcome of interest
covs <- subset(TandC.dta, select = c(age_B, primary_B, wealth_index_B,
                                     consumption_food2se_B, know_seeds_B, belief_seeds_B,
                                     adopt_seeds_ever_B, nb_techniques_known_B, nb_techniques_belief_B,
                                     adopt_tech_ever_B, hours_agri_day_B,
                                     acres_cultivated_B, nb_cropsN_B, expenditures2se_B,
                                     share_output_sold_B, total_value_agri_assets2se_B))
bal.tab_1 <- bal.tab(covs, treat = TandC.dta$agri,  disp = c("means", "sds"))
print(bal.tab_1)

##Assessing balance graphically
m.out <- matchit(agri ~ age_B + primary_B + wealth_index_B + consumption_food2se_B + know_seeds_B +
                   belief_seeds_B + adopt_seeds_ever_B + nb_techniques_known_B + nb_techniques_belief_B +
                   adopt_tech_ever_B + hours_agri_day_B + acres_cultivated_B + nb_cropsN_B + expenditures2se_B +
                   share_output_sold_B + total_value_agri_assets2se_B, data = TandC.dta)
par(mfrow = c(1,3))
bal.plot(m.out, 'age_B', which = "both")
bal.plot(m.out, 'total_value_agri_assets2se_B', which = "both") 
bal.plot(m.out, 'expenditures2se_B', which = "both") 


#3. Lasso Regression on Village characteristics

#1. Lasso Regression on Household characteristics
lasso <- glmnet(TandC.dta_H, outcome, alpha = 1)
summary(lasso)
## Choosing optimal value for lambda by K-fold CV
cv_lasso <- cv.glmnet(TandC.dta_H, outcome, alpha = 1) 
## finding optimal lambda value that minimizes test MSE
best_lambda <- cv_lasso$lambda.min
best_lambda
## finding coefficients of best model
best_lasso <- glmnet(TandC.dta_H, outcome, alpha = 1, lambda = best_lambda) #playing with the lambda
lasso.coef <- coef(best_lasso)
lasso.coef
lasso.coef <- lasso.coef[lasso.coef!=0]
lasso.coef

#4. Balance test on Village Characteristics (Table A1)

## Preliminary : aggregate data by Village
attach(TandC.dta)
aggdata <-aggregate(TandC.dta, by= list(clus_unique),
                    FUN=mean, na.rm=TRUE)
print(aggdata)

## Balance test
covs <- subset(aggdata, select = c(nb_HH, perc_farmers, short_distance,
                                     branch_min, mean_wealth_index, sd_wealth_index))
bal.tab_A1 <- bal.tab(covs, treat = aggdata$agri,  disp = c("means", "sds"))
print(bal.tab_A1)

#5. Replication of Table 2

## Formating LateX table for estimates

itt1 <- lm(treated_F ~ agri, data=TandC.dta, na.action=na.omit)
itt2 <- lm(adopt_seedsY_MF_F ~ agri, data=TandC.dta, na.action=na.omit)
itt3 <- lm(trained_MF_F ~ agri, data=TandC.dta, na.action=na.omit)
itt4 <- lm(adopt_seedsY_BRACnotMF_F ~ agri, data=TandC.dta, na.action=na.omit)
itt5 <- lm(adopt_seedsY_notBRAC_F ~ agri, data=TandC.dta, na.action=na.omit)
itt6 <- lm(mean_obs_tech_F ~ agri, data=TandC.dta, na.action=na.omit)
itt7 <- lm(netoutput2se_F ~ agri, data=TandC.dta, na.action=na.omit)
itt8 <- lm(nb_mktcrops_F ~ agri, data=TandC.dta, na.action=na.omit)
itt9 <- lm(consumption_food2se_F ~ agri, data=TandC.dta, na.action=na.omit)
itt10 <- lm(total_cons_F ~ agri, data=TandC.dta, na.action=na.omit)
itt11 <- lm(total_value_agri_assets2se_F ~ agri, data=TandC.dta, na.action=na.omit)
stargazer(itt1, itt2, itt3, itt4, itt5, itt6, 
          title="ITT estimates", 
          df=FALSE, digits=5,
          type="latex")
stargazer(itt7, itt8, itt9, itt10, itt11,  
          title="ITT estimates", 
          df=FALSE, digits=5,
          type="latex")

##. Calculating robust standard errors (cluster village)
install.packages('estimatr')
library(estimatr)

itt1.1 <- lm_robust(treated_F ~ agri, data=TandC.dta,
                 se_type = "stata", clusters = clus_unique)
summary(itt1.1)

itt2.1 <- lm_robust(adopt_seedsY_MF_F ~ agri, data=TandC.dta,
                 se_type = "stata", clusters = clus_unique)
summary(itt2.1)

itt3.1 <- lm_robust(trained_MF_F ~ agri, data=TandC.dta,
                    se_type = "stata", clusters = clus_unique)
summary(itt3.1)

itt4.1 <- lm_robust(adopt_seedsY_BRACnotMF_F ~ agri, data=TandC.dta,
                    se_type = "stata", clusters = clus_unique)
summary(itt4.1)

itt5.1 <- lm_robust(adopt_seedsY_notBRAC_F ~ agri, data=TandC.dta,
                    se_type = "stata", clusters = clus_unique)
summary(itt5.1)

itt6.1 <- lm_robust(mean_obs_tech_F ~ agri, data=TandC.dta,
                    se_type = "stata", clusters = clus_unique)
summary(itt6.1)

itt7.1 <- lm_robust(netoutput2se_F ~ agri, data=TandC.dta,
                    se_type = "stata", clusters = clus_unique)
summary(itt7.1)

itt8.1 <- lm_robust(nb_mktcrops_F ~ agri, data=TandC.dta,
                     se_type = "stata", clusters = clus_unique)
summary(itt8.1)

itt9.1 <- lm_robust(consumption_food2se_F ~ agri, data=TandC.dta,
                     se_type = "stata", clusters = clus_unique)
summary(itt9.1)

itt10.1 <- lm_robust(total_cons_F ~ agri, data=TandC.dta,
                    se_type = "stata", clusters = clus_unique)
summary(itt10.1)

itt11.1 <- lm_robust(total_value_agri_assets2se_F ~ agri, data=TandC.dta,
                    se_type = "stata", clusters = clus_unique)
summary(itt11.1)

#####-----------------------------------------------#####################################
##########PART. 2

# One must reload with the following step and change the path to data.
### ATTRITION ###
library(haven)
data2 = read_dta("C:/MES PROJETS/Projet_methodo/SocialIncentives_Data_TandC.dta")
attach(data2)
library(sandwich)

#colonne 1
model1 <- lm(missing_endline ~ agri + branch1 + branch2 + branch3, data = data2)
model1$coefficients
model1$coefficients[2] # coefficient de la variable d'intérêt (agri)
vcov_clustered1 <- vcovCL(model1, cluster=data2$clus_unique) # matrice de var-cov clusterée
se_clustered1 <- sqrt(diag(vcov_clustered1)) # écart-type clusterés
se_clustered1[2] # écart-type clusteré pour la variable d'intérêt (agri)



#colonne 2
model2 = lm(missing_endline ~ agri + branch1 + branch2 + branch3 + wealth_index_B + primary_B + acres_owned_B + consumption_food2se_B + total_value_agri_assets2se_B +
              know_seeds_B +  belief_seeds_B + adopt_seeds_ever_B + nb_techniques_known_B + adopt_tech_ever_B +
              hours_agri_day_B + acres_cultivated_B + nb_cropsN_B + nb_mktcrops_B + share_output_sold_B + netoutput2se_B, data = data2)
summary(model2)
model2$coefficients
model2$coefficients[2] #coefficient de la variable d'intérêt (agri)
vcov_clustered2 <- vcovCL(model2, cluster=data2$clus_unique) # matrice de var-cov clusterée
se_clustered2 <- sqrt(diag(vcov_clustered2)) # écart-type clusterés
se_clustered2[2] # écart-type clusteré pour la variable d'intérêt (agri)

#Colonne 3

# On crée des interactions des covariates avec la variable de traitement (agri)

wealth_index_B_t = wealth_index_B*agri
primary_B_t = primary_B*agri
acres_owned_B_t = acres_owned_B*agri
consumption_food2se_B_t = consumption_food2se_B*agri
total_value_agri_assets2se_B_t = total_value_agri_assets2se_B*agri 
know_seeds_B_t = know_seeds_B*agri
belief_seeds_B_t = belief_seeds_B*agri
adopt_seeds_ever_B_t = adopt_seeds_ever_B*agri
nb_techniques_known_B_t = nb_techniques_known_B*agri 
adopt_tech_ever_B_t = adopt_tech_ever_B*agri 
hours_agri_day_B_t = hours_agri_day_B*agri
acres_cultivated_B_t = acres_cultivated_B*agri 
nb_cropsN_B_t = nb_cropsN_B*agri
nb_mktcrops_B_t = nb_mktcrops_B*agri 
share_output_sold_B_t = share_output_sold_B*agri 
netoutput2se_B_t = netoutput2se_B*agri

# introduire ces variables créées  dans la base originale
data3 = cbind(data2,wealth_index_B_t,primary_B_t,acres_owned_B_t,consumption_food2se_B_t,
              total_value_agri_assets2se_B_t,know_seeds_B_t,belief_seeds_B_t,
              adopt_seeds_ever_B_t,nb_techniques_known_B_t,adopt_tech_ever_B_t,
              hours_agri_day_B_t,acres_cultivated_B_t,nb_cropsN_B_t,nb_mktcrops_B_t,
              share_output_sold_B_t,netoutput2se_B_t)

# modèle de régression

model3 = lm(missing_endline ~ agri + branch1 + branch2 + branch3 + wealth_index_B + primary_B + acres_owned_B + consumption_food2se_B + total_value_agri_assets2se_B +
              know_seeds_B +  belief_seeds_B + adopt_seeds_ever_B + nb_techniques_known_B + adopt_tech_ever_B +
              hours_agri_day_B + acres_cultivated_B + nb_cropsN_B + nb_mktcrops_B + share_output_sold_B + netoutput2se_B +
              wealth_index_B_t + primary_B_t + acres_owned_B_t + consumption_food2se_B_t +
              total_value_agri_assets2se_B_t + know_seeds_B_t + belief_seeds_B_t +
              adopt_seeds_ever_B_t + nb_techniques_known_B_t + adopt_tech_ever_B_t +
              hours_agri_day_B_t + acres_cultivated_B_t + nb_cropsN_B_t + nb_mktcrops_B_t +
              share_output_sold_B_t + netoutput2se_B_t,data = data3)
model3$coefficients # coefficients estimés
model3$coefficients[2] # coefficient de la variable d'intérêt (agri)
vcov_clustered3 <- vcovCL(model3, cluster=data3$clus_unique) # matrice de var-cov clusterée
se_clustered3 <- sqrt(diag(vcov_clustered3)) # écart-type clusterés
se_clustered3[2] # écart-type clusteré pour la variable d'intérêt (agri)


# Colonne 4

# Interaction variable between Treated and exclusively tied to DA

data3$know_onlyMF_B[agri == 0] <- 0
data3$know_onlynotMF_B[agri == 0] <- 0
data3$know_both_B[agri == 0] <- 0
data3$know_none_B[agri == 0] <- 0



# regression model
model4 <- lm(missing_endline ~ know_onlyMF_B + know_onlynotMF_B + know_both_B + know_none_B  + branch1 + branch2 + branch3, data = data3)
model4$coefficients
model4$coefficients[2:3] # les coefficients de la variable d'intérêt ((know_onlyMF_B & know_onlynotMF_B))
vcov_clustered4 <- vcovCL(model4, cluster=data3$clus_unique) # matrice de var-cov clusterée
se_clustered4 <- sqrt(diag(vcov_clustered4)) # écart-type clusterés
se_clustered4[2:3] # écart-type clusteré pour les variables d'intérêt (know_onlyMF_B & know_onlynotMF_B)


# Colonne 5

model5 = lm(missing_endline ~ know_onlyMF_B + know_onlynotMF_B + know_both_B + know_none_B + branch1 + branch2 + branch3 + wealth_index_B + primary_B + acres_owned_B + consumption_food2se_B + total_value_agri_assets2se_B +
              know_seeds_B +  belief_seeds_B + adopt_seeds_ever_B + nb_techniques_known_B + adopt_tech_ever_B +
              hours_agri_day_B + acres_cultivated_B + nb_cropsN_B + nb_mktcrops_B + share_output_sold_B + netoutput2se_B, data = data3)
model5$coefficients # coefficients estimés
model5$coefficients[2:3] # les coefficients des variables  d'intérêt ((know_onlyMF_B & know_onlynotMF_B))
vcov_clustered5 <- vcovCL(model5, cluster=data3$clus_unique) # matrice de var-cov clusterée
se_clustered5 <- sqrt(diag(vcov_clustered5)) # écart-type clusterés
se_clustered5[2:3] # écart-type clusteré pour les variables d'intérêt (know_onlyMF_B & know_onlynotMF_B)

# Colonne 6 

# créeons des interactions des caratérisques des ménages avec les différentes variables de traitement
#(know_onlyMF, know_onlyMF_B, know_both_B et know_none_B)

# Interaction avec know_both_B
wealth_index_B_bth = wealth_index_B*data3$know_both_B
primary_B_bth = primary_B*data3$know_both_B
acres_owned_B_bth = acres_owned_B*data3$know_both_B
consumption_food2se_B_bth = consumption_food2se_B*data3$know_both_B
total_value_agri_assets2se_B_bth = total_value_agri_assets2se_B*data3$know_both_B 
know_seeds_B_bth = know_seeds_B*data3$know_both_B
belief_seeds_B_bth = belief_seeds_B*data3$know_both_B
adopt_seeds_ever_B_bth = adopt_seeds_ever_B*data3$know_both_B
nb_techniques_known_B_bth = nb_techniques_known_B*data3$know_both_B 
adopt_tech_ever_B_bth = adopt_tech_ever_B*data3$know_both_B 
hours_agri_day_B_bth = hours_agri_day_B*data3$know_both_B
acres_cultivated_B_bth = acres_cultivated_B*data3$know_both_B 
nb_cropsN_B_bth = nb_cropsN_B*data3$know_both_B
nb_mktcrops_B_bth = nb_mktcrops_B*data3$know_both_B 
share_output_sold_B_bth = share_output_sold_B*data3$know_both_B 
netoutput2se_B_bth = netoutput2se_B*data3$know_both_B

# Interaction avec know_onlynotMF_B
wealth_index_B_NMF = wealth_index_B*data3$know_onlynotMF_B
primary_B_NMF = primary_B*data3$know_onlynotMF_B
acres_owned_B_NMF = acres_owned_B*data3$know_onlynotMF_B
consumption_food2se_B_NMF = consumption_food2se_B*data3$know_onlynotMF_B
total_value_agri_assets2se_B_NMF = total_value_agri_assets2se_B*data3$know_onlynotMF_B 
know_seeds_B_NMF = know_seeds_B*data3$know_onlynotMF_B
belief_seeds_B_NMF = belief_seeds_B*data3$know_onlynotMF_B
adopt_seeds_ever_B_NMF = adopt_seeds_ever_B*data3$know_onlynotMF_B
nb_techniques_known_B_NMF = nb_techniques_known_B*data3$know_onlynotMF_B 
adopt_tech_ever_B_NMF = adopt_tech_ever_B*data3$know_onlynotMF_B 
hours_agri_day_B_NMF = hours_agri_day_B*data3$know_onlynotMF_B
acres_cultivated_B_NMF = acres_cultivated_B*data3$know_onlynotMF_B 
nb_cropsN_B_NMF = nb_cropsN_B*data3$know_onlynotMF_B
nb_mktcrops_B_NMF = nb_mktcrops_B*data3$know_onlynotMF_B 
share_output_sold_B_NMF = share_output_sold_B*data3$know_onlynotMF_B 
netoutput2se_B_NMF = netoutput2se_B*data3$know_onlynotMF_B

# Interaction avec know_onlyMF_B
wealth_index_B_MF = wealth_index_B*data3$know_onlyMF_B
primary_B_MF = primary_B*data3$know_onlyMF_B
acres_owned_B_MF = acres_owned_B*data3$know_onlyMF_B
consumption_food2se_B_MF = consumption_food2se_B*data3$know_onlyMF_B
total_value_agri_assets2se_B_MF = total_value_agri_assets2se_B*data3$know_onlyMF_B 
know_seeds_B_MF = know_seeds_B*data3$know_onlyMF_B
belief_seeds_B_MF = belief_seeds_B*data3$know_onlyMF_B
adopt_seeds_ever_B_MF = adopt_seeds_ever_B*data3$know_onlyMF_B
nb_techniques_known_B_MF = nb_techniques_known_B*data3$know_onlyMF_B 
adopt_tech_ever_B_MF = adopt_tech_ever_B*data3$know_onlyMF_B 
hours_agri_day_B_MF = hours_agri_day_B*data3$know_onlyMF_B
acres_cultivated_B_MF = acres_cultivated_B*data3$know_onlyMF_B 
nb_cropsN_B_MF = nb_cropsN_B*data3$know_onlyMF_B
nb_mktcrops_B_MF = nb_mktcrops_B*data3$know_onlyMF_B 
share_output_sold_B_MF = share_output_sold_B*data3$know_onlyMF_B 
netoutput2se_B_MF = netoutput2se_B*data3$know_onlyMF_B

# Interaction avec know_none_B
wealth_index_B_N = wealth_index_B*data3$know_none_B
primary_B_N = primary_B*data3$know_none_B
acres_owned_B_N = acres_owned_B*data3$know_none_B
consumption_food2se_B_N = consumption_food2se_B*data3$know_none_B
total_value_agri_assets2se_B_N = total_value_agri_assets2se_B*data3$know_none_B 
know_seeds_B_N = know_seeds_B*data3$know_none_B
belief_seeds_B_N = belief_seeds_B*data3$know_none_B
adopt_seeds_ever_B_N = adopt_seeds_ever_B*data3$know_none_B
nb_techniques_known_B_N = nb_techniques_known_B*data3$know_none_B 
adopt_tech_ever_B_N = adopt_tech_ever_B*data3$know_none_B 
hours_agri_day_B_N = hours_agri_day_B*data3$know_none_B
acres_cultivated_B_N = acres_cultivated_B*data3$know_none_B 
nb_cropsN_B_N = nb_cropsN_B*data3$know_none_B
nb_mktcrops_B_N = nb_mktcrops_B*data3$know_none_B 
share_output_sold_B_N = share_output_sold_B*data3$know_none_B 
netoutput2se_B_N = netoutput2se_B*data3$know_none_B


# introduire ces nouvelles variables créées dans la base de données déjà existante
selected_vars1 <- grep("_B_MF$" ,ls(), value = TRUE)
data4 <- cbind(data3, do.call(cbind, mget(selected_vars1)))

selected_vars2 <- grep("_B_NMF$" ,ls(), value = TRUE)
data5 <- cbind(data4, do.call(cbind, mget(selected_vars2)))

selected_vars3 <- grep("_B_N$" ,ls(), value = TRUE)
data6 <- cbind(data5, do.call(cbind, mget(selected_vars3)))

selected_vars4 <- grep("_B_bth$" ,ls(), value = TRUE)
data7 <- cbind(data6, do.call(cbind, mget(selected_vars4)))

# modèle de regression
model6 = lm(missing_endline ~ know_onlyMF_B + know_onlynotMF_B  + know_both_B + know_none_B +
              + branch1 + branch2 + branch3 + wealth_index_B + primary_B + acres_owned_B + consumption_food2se_B + total_value_agri_assets2se_B +
              know_seeds_B +  belief_seeds_B + adopt_seeds_ever_B + nb_techniques_known_B + adopt_tech_ever_B +
              hours_agri_day_B + acres_cultivated_B + nb_cropsN_B + nb_mktcrops_B + share_output_sold_B + netoutput2se_B + wealth_index_B_MF + primary_B_MF + 
              acres_owned_B_MF + consumption_food2se_B_MF + total_value_agri_assets2se_B_MF + 
              know_seeds_B_MF + belief_seeds_B_MF + adopt_seeds_ever_B_MF + nb_techniques_known_B_MF +
              adopt_tech_ever_B_MF + hours_agri_day_B_MF + acres_cultivated_B_MF + nb_cropsN_B_MF +
              nb_mktcrops_B_MF + share_output_sold_B_MF + netoutput2se_B_MF + wealth_index_B_NMF + primary_B_NMF + 
              acres_owned_B_NMF + consumption_food2se_B_NMF + total_value_agri_assets2se_B_NMF + 
              know_seeds_B_NMF + belief_seeds_B_NMF + adopt_seeds_ever_B_NMF + nb_techniques_known_B_NMF +
              adopt_tech_ever_B_NMF + hours_agri_day_B_NMF + acres_cultivated_B_NMF + nb_cropsN_B_NMF +
              nb_mktcrops_B_NMF + share_output_sold_B_NMF + netoutput2se_B_NMF + wealth_index_B_bth + primary_B_bth + 
              acres_owned_B_bth + consumption_food2se_B_bth + total_value_agri_assets2se_B_bth + 
              know_seeds_B_bth + belief_seeds_B_bth + adopt_seeds_ever_B_bth + nb_techniques_known_B_bth +
              adopt_tech_ever_B_bth + hours_agri_day_B_bth + acres_cultivated_B_bth + nb_cropsN_B_bth +
              nb_mktcrops_B_bth + share_output_sold_B_bth + netoutput2se_B_bth + wealth_index_B_N + primary_B_N + 
              acres_owned_B_N + consumption_food2se_B_N + total_value_agri_assets2se_B_N + 
              know_seeds_B_N + belief_seeds_B_N + adopt_seeds_ever_B_N + nb_techniques_known_B_N +
              adopt_tech_ever_B_N + hours_agri_day_B_N + acres_cultivated_B_N + nb_cropsN_B_N +
              nb_mktcrops_B_N + share_output_sold_B_N + netoutput2se_B_N,data = data7)
model6$coefficients # les coefficients estimés
model6$coefficients[2:3] # les coefficients des variables d'intérêt (know_onlyMF_B et know_onlynotMF_B)
vcov_clustered6 <- vcovCL(model6, cluster=data3$clus_unique) # matrice de var-cov clusterée
se_clustered6 <- sqrt(diag(vcov_clustered6)) # écart-type clusterés
se_clustered6[2:3] # écart-type clusteré pour les variables d'intérêt (know_onlyMF_B et know_onlynotMF_B)


# Obtenir le tableau résumant les résultats des régressions
install.packages("stargazer")
library(stargazer)
stargazer(model1, model2, model3, model4, model5, model6, title = "Attrition",align = TRUE, type = "text", out = "regression_results.txt")


##################################################################################



# differential attrition by characteristics of households in treatment and control groups
wealth_index_Batt = data2$wealth_index_B[missing_endline == 1 & agri == 1]
wealth_index_Bnatt = data2$wealth_index_B[missing_endline == 1 & agri == 0]
t.test( wealth_index_Batt, wealth_index_Bnatt)

primary_Batt = data2$primary_B[missing_endline == 1 & agri == 1]
primary_Bnatt = data2$primary_B[missing_endline == 1 & agri == 0]
t.test( primary_Batt, primary_Bnatt)

acres_owned_Batt = data2$acres_owned_B[missing_endline == 1 & agri == 1]
acres_owned_Bnatt = data2$acres_owned_B[missing_endline == 1 & agri == 0]
t.test( acres_owned_Batt, acres_owned_Bnatt)

consumption_food2se_Batt = data2$consumption_food2se_B[missing_endline == 1 & agri == 1]
consumption_food2se_Bnatt = data2$consumption_food2se_B[missing_endline == 1 & agri == 0]  
t.test(consumption_food2se_Batt,consumption_food2se_Bnatt)

total_value_agri_assets2se_Batt = data2$total_value_agri_assets2se_B[missing_endline == 1 & agri == 1]
total_value_agri_assets2se_Bnatt = data2$total_value_agri_assets2se_B[missing_endline == 1 & agri == 0]
resultat_t_test1 = t.test(total_value_agri_assets2se_Batt,total_value_agri_assets2se_Bnatt)# significatif

know_seeds_Batt = data2$know_seeds_B[missing_endline == 1 & agri == 1]
know_seeds_Bnatt = data2$know_seeds_B[missing_endline == 1 & agri == 0]  
t.test(know_seeds_Batt,know_seeds_Bnatt)

belief_seeds_Batt = data2$belief_seeds_B[missing_endline == 1 & agri == 1]
belief_seeds_Bnatt = data2$belief_seeds_B[missing_endline == 1 & agri == 0]  
t.test(belief_seeds_Batt,belief_seeds_Bnatt)

adopt_seeds_ever_Batt = data2$adopt_seeds_ever_B[missing_endline == 1 & agri == 1]
adopt_seeds_ever_Bnatt = data2$adopt_seeds_ever_B[missing_endline == 1 & agri == 0]  
t.test(adopt_seeds_ever_Batt,adopt_seeds_ever_Bnatt)

nb_techniques_known_Batt = data2$nb_techniques_known_B[missing_endline == 1 & agri == 1]
nb_techniques_known_Bnatt = data2$nb_techniques_known_B[missing_endline == 1 & agri == 0]
t.test(nb_techniques_known_Batt,nb_techniques_known_Bnatt)

adopt_tech_ever_Batt = data2$adopt_tech_ever_B[missing_endline == 1 & agri == 1]
adopt_tech_ever_Bnatt = data2$adopt_tech_ever_B[missing_endline == 1 & agri == 0]
resultat_t_test2 = t.test(adopt_tech_ever_Batt,adopt_tech_ever_Bnatt) #significatif

hours_agri_day_Batt = data2$hours_agri_day_B[missing_endline == 1 & agri == 1]
hours_agri_day_Bnatt = data2$hours_agri_day_B[missing_endline == 1 & agri == 0]
t.test(hours_agri_day_Batt,hours_agri_day_Bnatt)

acres_cultivated_Batt = data2$acres_cultivated_B[missing_endline == 1 & agri == 1]
acres_cultivated_Bnatt = data2$acres_cultivated_B[missing_endline == 1 & agri == 0]
t.test(acres_cultivated_Batt,acres_cultivated_Bnatt)

nb_cropsN_Batt = data2$nb_cropsN_B[missing_endline == 1 & agri == 1]
nb_cropsN_Bnatt = data2$nb_cropsN_B[missing_endline == 1 & agri == 0]  
t.test(nb_cropsN_Batt,nb_cropsN_Bnatt)

nb_mktcrops_Batt = data2$nb_mktcrops_B[missing_endline == 1 & agri == 1]
nb_mktcrops_Bnatt = data2$nb_mktcrops_B[missing_endline == 1 & agri == 0] 
t.test(nb_mktcrops_Batt,nb_mktcrops_Bnatt)

share_output_sold_Batt = data2$share_output_sold_B[missing_endline == 1 & agri == 1] 
share_output_sold_Bnatt = data2$share_output_sold_B[missing_endline == 1 & agri == 0]
t.test(share_output_sold_Batt,share_output_sold_Bnatt)

netoutput2se_Batt = data2$netoutput2se_B[missing_endline == 1 & agri == 1]
netoutput2se_Bnatt = data2$netoutput2se_B[missing_endline == 1 & agri == 0]
t.test(netoutput2se_Batt,netoutput2se_Bnatt)


# obtenir les différences significatives sur latex
install.packages("texreg")
library(textreg)
myresult =  list(resultat_t_test1, resultat_t_test2)
screenreg(myresult)
screenreg
texreg::screenreg(myresult)

resultat_t_test11 <- as.numeric(resultat_t_test1)
resultat_t_test2 <- as.numeric(resultat_t_test2)
# Conversion en table LaTeX
table <- paste("\\begin{table}[ht]",
               "\\centering",
               "\\begin{tabular}{lcc}",
               "\\hline",
               " & resultat_t_test1 & resultat_t_test2 \\\\",
               "\\hline",
               "t & ", resultat_t_test1$statistic, " & ", resultat_t_test2, " \\\\",
               "df & ", round(resultat_t_test1$parameter, 2), " & ", round(resultat_t_test2$parameter, 2), " \\\\",
               "p-value & ", format.pval(resultat_t_test1$p.value), " & ", format.pval(resultat_t_test2$p.value), " \\\\",
               "\\hline",
               "\\end{tabular}",
               "\\caption{Balance check for attrition}",
               "\\label{tab:attritionbis}",
               "\\end{table}", sep = "\n")

cat(table)





################################################################################  
### COVERAGE ###

library(haven)
data1 = read_dta("C:/MES PROJETS/Projet_methodo/data1.dta")
attach(data1)

# génération de la variable treated_exc_F
treated_exc_F <- ifelse(data1$treated_F == 1 & (data1$know_onlyMF_B == 1 | data1$know_onlynotMF_B == 1), 1, 0)

# remplacement de treated_exc_F par NA si treated_F est NA et know_onlyMF_B ou know_onlynotMF_B sont NA 
treated_exc_F[is.na(data1$treated_F) & (is.na(data1$know_onlyMF_B) | is.na(data1$know_onlynotMF_B))] <- NA


#### COLONNE (1)  #########


treated_exc_F = data1$treated_exc_F

# number of farmers tied to one of the two agents 
know_excl=(know_onlyMF + know_onlynotMF) 
# square of farmers tied to one of the two agents
know_onlyMF2=know_onlyMF * know_onlyMF 

# Regression of treated_exc_F on predictors
library(sandwich)
reg1 <- lm(treated_exc_F ~ know_onlyMF + know_excl + branch1 + branch2 + branch3, data=data1)
summary(reg1)
reg1$coefficients[2] # coefficient de la variable d'intérêt(know_onlyMF)
vcov <- vcovHC(reg1, type = "HC1") #  Matrice d'homoscédasticité-consistante des erreurs
se <- sqrt(diag(vcov)) # Calcul des erreurs standard robustes
se[2]         # écart type robuste pour les variabes d'intérêt 

# Calculation of mean of treated_exc_F
mean <- mean(data1$treated_exc_F)




#### COLONNE 2  #####


# Regression of treated_exc_F on predictors
library(sandwich)
reg3 <- lm(treated_exc_F ~  know_onlyMF + know_onlyMF2 + know_excl + branch1 + branch2 + branch3, data=data1)
summary(reg3)
reg3$coefficients[2:3] # coefficient de la variable d'intérêt (know_onlyMF2)
vcov1 <- vcovHC(reg3, type = "HC1") # Matrice d'homoscédasticité-consistante des erreurs
se1 <- sqrt(diag(vcov1)) # Calcul des erreurs standard robustes
se1[2:3]         # écart type robuste pour les variables d'interêt 


# Approfondissement Coverage


know_onlyMF3=know_onlyMF2 * know_onlyMF # créer la variable à la puissance 3
mydata = cbind(data1,know_onlyMF3) # ajouter cette nouvelle variable dans la base originale
reg4 <- lm(treated_exc_F ~  know_onlyMF + know_onlyMF2 + know_onlyMF3 + know_excl + branch1 + branch2 + branch3, data=mydata)
summary(reg4)
reg4$coefficients[2:4] # Les coefficients d'intérêt
vcov4 <- vcovHC(reg4, type = "HC1") #Matrice d'homoscédasticité-consistante des erreurs
se4 <- sqrt(diag(vcov4)) ## Calcul des erreurs standard robustes
se4[2:4] # écart type robuste pour les variables d'interêt 

stargazer(reg1, reg3, reg4, title = "Coverage",align = TRUE, type = "latex")


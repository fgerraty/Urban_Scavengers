###############################################################################
# Santa Cruz Urban Scavengers Project #########################################
# Author: Frankie Gerraty (frankiegerraty@gmail.com; fgerraty@ucsc.edu) #######
###############################################################################
# Script 04: Carrion Processing Analyses ######################################
#------------------------------------------------------------------------------


####################################
# PART 1: Import Data ##############
####################################

#Import .csvs from "processed data" folder ------------------------------------

urban_scavengers_summary <- read_csv("data/processed/urban_scavengers_summary.csv") %>% 
  
  #Create new columns for adjusted MaxN values (i.e., MaxN / # fish deployed) of the most documented scavenger species
  mutate(common_raven_adj = common_raven/n_fish_deployed,
         american_crow_adj = american_crow/n_fish_deployed,
         deer_mouse_adj = deer_mouse/n_fish_deployed) 


carcass_level_summary <- read_csv("data/processed/carcass_level_summary.csv") %>%    
  #Create new column "scav_prob" which classifies scavenging as 1/0 for each carcass (partially scavenged carcasses are classified as scavenged = 1)
  mutate(scav_prob = if_else(no_scavenge == TRUE, 0, 1),
         #Create new column "full_scav_prob" which classifies scavenging as 1/0 for each carcass that was fully scavenged AKA carcass removal)
         full_scav_prob = if_else(no_scavenge == FALSE & partial_scavenge == FALSE, 1, 0)) %>% 
  #bring environmental variables and adjusted species abundances into dataframe
  left_join(., urban_scavengers_summary[,c("site_name", "percent_developed_1km", "percent_agricultural_5km", "human_visitors_per_day", "domestic_dog_visitors_per_day", "common_raven_adj", "american_crow_adj", "deer_mouse_adj","richness", "diversity")], by = c("site_name")) %>% 
  
  #convert site name to factor for random effects
  mutate(site_name = factor(site_name)) 


########################################################################################
# PART 2: Does Urbanization Alone Influence Carrion Processing Metrics? ################
########################################################################################

# PART 2A: Analyze deployment data for the probability of any scavenging ----------

## Model fitting 

# G1: Analyze probability of scavenging using using 1km urbanization buffer with a mixed-effects logistic regression (generalized linear mixed effects model with binomial distribution and logit link)
g1=glmmTMB(scav_prob~ #scav_prob (1/0) binary probability of any scavenging activity
           percent_developed_1km + #1km urbanization extent
           (1|site_name), #site as random effect
         family = "binomial", #binomial distribution
         data=carcass_level_summary);summary(g1) 

# Check g1a assumptions with DHARMa package
g1_res = simulateResiduals(g1)
plot(g1_res, rank = T)
testDispersion(g1_res)
plotResiduals(g1_res, as.factor(carcass_level_summary$site_name), xlab = "Site", main=NULL)

# PART 2B: Analyze deployment data for the probability of carcass removal ----------

# Analyze probability of carcass removal using using 1km buffer with a mixed-effects logistic regression (generalized linear mixed effects model with binomial distribution and logit link)
g2=glmmTMB(full_scav_prob~ #full_scav_prob (1/0) binary probability of carcass removal
           percent_developed_1km+ #1km urbanization extent
           (1|site_name), #site as random effect
         family = "binomial", #binomial distribution
         data=carcass_level_summary);summary(g2)

# Check g2 assumptions with DHARMa package
g2_res = simulateResiduals(g2)
plot(g2_res, rank = T)
testDispersion(g2_res)
plotResiduals(g2_res, carcass_level_summary$site_name, xlab = "Site", main=NULL)



# PART 2C: Analyze deployment data for the time until first scavenging event----

#Create datafeame filtering out carcasses in which no scavenging occurred
temporal_df <- carcass_level_summary %>% 
  filter(hours_to_first_scavenging_event != "NA") %>% 
  mutate(site_name = factor(site_name)) #remove unused levels from factor

# Analyze time until first scavenging event using using 1km urbanization buffer with a generalized linear mixed-effects model with Gamma distribution and log link
g3=glmmTMB(hours_to_first_scavenging_event~ 
             percent_developed_1km+ #1km urbanization
             (1|site_name), #site as random effect
           family=Gamma(link = "log"), #binomial distribution
           data=temporal_df) #dataframe with no-scavenge carcasses removed
summary(g3)

# Check g3 assumptions with DHARMa package
g3_res = simulateResiduals(g3)
plot(g3_res, rank = T)
testDispersion(g3_res)
plotResiduals(g3_res, temporal_df$site_name, xlab = "Site", main=NULL)


# PART 2D: Analyze time until carcass removal using 1km urbanization buffer with a generalized linear mixed-effects model with Gamma distribution and log link ---------

#Create datafeame filtering out carcasses in which either (1) no scavenging occurred or (2) carcass was only partially scavenged
temporal_df2 <- carcass_level_summary %>% 
  filter(hours_to_full_scavenge != "NA")%>% 
  mutate(site_name = factor(site_name)) #remove unused levels from factor

# Analyze time until complete carcass removal using using 1km buffer with a generalized linear mixed-effects model with Gamma distribution and log link
g4=glmmTMB(hours_to_full_scavenge~
             percent_developed_1km+ #1km urbanization
             (1|site_name), #site as random effect
           family=Gamma(link = "log"), #binomial distribution
           data=temporal_df2) #dataframe with only complete carcass removal
summary(g4)

# Check g2a assumptions with DHARMa package
g4_res = simulateResiduals(g4)
plot(g4_res, rank = T)
testDispersion(g4_res)
plotResiduals(g4_res, temporal_df2$site_name, xlab = "Site", main=NULL)


####################################################################################
# PART 3: Do Any Other Variables Correlate With Carrion Processing Metrics? ########
####################################################################################

#########################################
# PART 3A: Probability of Scavenging ####
#########################################

# H1a - Hypothesis 2: Urbanization + human visitation predicts scavenging rates ------
h1a <- glmmTMB(scav_prob~ #scav_prob (1/0) binary probability of any scavenging activity
              percent_developed_1km +
              human_visitors_per_day +
              (1|site_name), #site as random effect
            family = "binomial", #binomial distribution
            data=carcass_level_summary);summary(h1a) 

# Check h1a assumptions with DHARMa package
h1a_res = simulateResiduals(h1a)
plot(h1a_res, rank = T)
testDispersion(h1a_res)
plotResiduals(h1a_res, carcass_level_summary$site_name, xlab = "Site", main=NULL)


# H1b - Hypothesis 3: Urbanization and domestic dog visitation predicts scavenging rates ----
h1b <- glmmTMB(scav_prob~ #scav_prob (1/0) binary probability of any scavenging activity
               percent_developed_1km +
               domestic_dog_visitors_per_day +
               (1|site_name), #site as random effect
             family = "binomial", #binomial distribution
             data=carcass_level_summary);summary(h1b) 

# Check h1b assumptions with DHARMa package
h1b_res = simulateResiduals(h1b)
plot(h1b_res, rank = T)
testDispersion(h1b_res)
plotResiduals(h1b_res, carcass_level_summary$site_name, xlab = "Site", main=NULL)


# H1c - Hypothesis 4: Agriculture + human visitation predicts scavenging rates ---------------------
h1c <- glmmTMB(scav_prob~ #scav_prob (1/0) binary probability of any scavenging activity
               percent_agricultural_5km + #3km agricultural extent
               human_visitors_per_day + #human visitation
               (1|site_name), #site as random effect
             family = "binomial", #binomial distribution
             data=carcass_level_summary);summary(h1c) 

# Check h1c assumptions with DHARMa package
h1c_res = simulateResiduals(h1c)
plot(h1c_res, rank = T)
testDispersion(h1c_res)
plotResiduals(h1c_res, carcass_level_summary$site_name, xlab = "Site", main=NULL)


# H1d - Hypothesis 5: Urbanization and deployment period predicts scavenging rates -----
h1d <- glmmTMB(scav_prob~ #scav_prob (1/0) binary probability of any scavenging activity
               percent_developed_1km + #1km urbanization extent
               deployment_type_AM_PM +
               (1|site_name), #site as random effect
             family = "binomial", #binomial distribution
             data=carcass_level_summary);summary(h1d) 

# Check h1d assumptions with DHARMa package
h1d_res = simulateResiduals(h1d)
plot(h1d_res, rank = T)
testDispersion(h1d_res)
plotResiduals(h1d_res, carcass_level_summary$site_name, xlab = "Site", main=NULL)


# H1e - Hypothesis 6: Deployment period and domestic dog visitation predict scavenging rates -----
h1e <- glmmTMB(scav_prob~ #scav_prob (1/0) binary probability of any scavenging activity
               deployment_type_AM_PM +
               domestic_dog_visitors_per_day +
               (1|site_name), #site as random effect
             family = "binomial", #binomial distribution
             data=carcass_level_summary);summary(h1e) 

# Check h1e assumptions with DHARMa package
h1e_res = simulateResiduals(h1e)
plot(h1e_res, rank = T)
testDispersion(h1e_res)
plotResiduals(h1e_res, carcass_level_summary$site_name, xlab = "Site", main=NULL)

# H1f - Hypothesis 7: scavenger abundance predicts scavenging rates -----
h1f <- glmmTMB(scav_prob~ #scav_prob (1/0) binary probability of any scavenging activity
               common_raven_adj +
               american_crow_adj +
               deer_mouse_adj +
               (1|site_name), #site as random effect
             family = "binomial", #binomial distribution
             data=carcass_level_summary);summary(h1f) 

# Check h1f assumptions with DHARMa package
h1f_res = simulateResiduals(h1f)
plot(h1f_res, rank = T)
testDispersion(h1f_res)
plotResiduals(h1f_res, carcass_level_summary$site_name, xlab = "Site", main=NULL)


# H1g - Hypothesis 8: Scavenger species richness predicts scavenging rates -----
h1g <- glmmTMB(scav_prob~ #scav_prob (1/0) binary probability of any scavenging activity
               richness +               
               (1|site_name), #site as random effect
               family = "binomial", #binomial distribution
               data=carcass_level_summary);summary(h1g) 

# Check h1g assumptions with DHARMa package
h1g_res = simulateResiduals(h1g)
plot(h1g_res, rank = T)
testDispersion(h1g_res)
plotResiduals(h1g_res, carcass_level_summary$site_name, xlab = "Site", main=NULL)


# H1h - Hypothesis 9: Deployment period and human visitation predict scavenging rates -----
h1h <- glmmTMB(scav_prob~ #scav_prob (1/0) binary probability of any scavenging activity
                 deployment_type_AM_PM +
                 human_visitors_per_day +
                 (1|site_name), #site as random effect
               family = "binomial", #binomial distribution
               data=carcass_level_summary);summary(h1h) 

# Check h1e assumptions with DHARMa package
h1h_res = simulateResiduals(h1h)
plot(h1h_res, rank = T)
testDispersion(h1h_res)
plotResiduals(h1h_res, carcass_level_summary$site_name, xlab = "Site", main=NULL)


# H1i - null model -----
h1i <- glmmTMB(scav_prob~ #scav_prob (1/0) binary probability of any scavenging activity
               1 +               
               (1|site_name), #site as random effect
             family = "binomial", #binomial distribution
             data=carcass_level_summary);summary(h1i) 

# Check h1h assumptions with DHARMa package
h1i_res = simulateResiduals(h1i)
plot(h1i_res, rank = T)
testDispersion(h1i_res)
plotResiduals(h1i_res, carcass_level_summary$site_name, xlab = "Site", main=NULL)

# Compare Models: ####

all_scav_prob_models <- aictab(cand.set=list(g1, h1a, h1b, h1c, h1d, h1e, h1f, h1g, h1h, h1i),
                   modnames=(c("Urbanization (1km)",
                               "Urbanization (1km) + Human Visitation",
                               "Urbanization (1km) + Domestic Dog Visitation***",
                               "Agricultural (5km) + Human Visitation",
                               "Urbanization (1km) + Deployment Type (Day/Night)",
                               "Domestic Dog Visitation* + Deployment Type (Day/Night)",
                               "Scavenger Abundance (Common Raven MaxN* + American Crow MaxN* + Deer Mouse MaxN)",
                               "Scavenger Species Richness",
                               "Human Visitation + Deployment Type (Day/Night)",
                               "null")),
                   second.ord=F) %>% 
  mutate(across(c('AIC', 'Delta_AIC', "ModelLik", "AICWt", "LL", "Cum.Wt"), round, digits = 3))

##############################################
# PART 3B: Probability of Carcass Removal ####
##############################################

# H2a - Hypothesis 2: Urbanization + human visitation predicts scavenging rates ------
h2a <- glmmTMB(full_scav_prob~ #full_scav_prob (1/0) binary probability of any scavenging activity
                 percent_developed_1km +
                 human_visitors_per_day +
                 (1|site_name), #site as random effect
               family = "binomial", #binomial distribution
               data=carcass_level_summary);summary(h2a) 

# Check h2a assumptions with DHARMa package
h2a_res = simulateResiduals(h2a)
plot(h2a_res, rank = T)
testDispersion(h2a_res)
plotResiduals(h2a_res, carcass_level_summary$site_name, xlab = "Site", main=NULL)


# H2b - Hypothesis 3: Urbanization and domestic dog visitation predicts scavenging rates ----
h2b <- glmmTMB(full_scav_prob~ #full_scav_prob (1/0) binary probability of any scavenging activity
                 percent_developed_1km +
                 domestic_dog_visitors_per_day +
                 (1|site_name), #site as random effect
               family = "binomial", #binomial distribution
               data=carcass_level_summary);summary(h2b) 

# Check h2b assumptions with DHARMa package
h2b_res = simulateResiduals(h2b)
plot(h2b_res, rank = T)
testDispersion(h2b_res)
plotResiduals(h2b_res, carcass_level_summary$site_name, xlab = "Site", main=NULL)


# H2c - Hypothesis 4: Agriculture + human visitation predicts scavenging rates ---------
h2c <- glmmTMB(full_scav_prob~ #full_scav_prob (1/0) binary probability of any scavenging activity
                 percent_agricultural_5km + #3km agricultural extent
                 human_visitors_per_day +
                 (1|site_name), #site as random effect
               family = "binomial", #binomial distribution
               data=carcass_level_summary);summary(h2c) 

# Check h2c assumptions with DHARMa package
h2c_res = simulateResiduals(h2c)
plot(h2c_res, rank = T)
testDispersion(h2c_res)
plotResiduals(h2c_res, carcass_level_summary$site_name, xlab = "Site", main=NULL)


# H2d - Hypothesis 5: Urbanization and deployment period predicts scavenging rates -----
h2d <- glmmTMB(full_scav_prob~ #full_scav_prob (1/0) binary probability of any scavenging activity
                 percent_developed_1km + #1km urbanization extent
                 deployment_type_AM_PM +
                 (1|site_name), #site as random effect
               family = "binomial", #binomial distribution
               data=carcass_level_summary);summary(h2d) 

# Check h2d assumptions with DHARMa package
h2d_res = simulateResiduals(h2d)
plot(h2d_res, rank = T)
testDispersion(h2d_res)
plotResiduals(h2d_res, carcass_level_summary$site_name, xlab = "Site", main=NULL)


# H2e - Hypothesis 6: Deployment period and domestic dog visitation predict scavenging rates -----
h2e <- glmmTMB(full_scav_prob~ #full_scav_prob (1/0) binary probability of any scavenging activity
                 deployment_type_AM_PM +
                 domestic_dog_visitors_per_day +
                 (1|site_name), #site as random effect
               family = "binomial", #binomial distribution
               data=carcass_level_summary);summary(h2e) 

# Check h2e assumptions with DHARMa package
h2e_res = simulateResiduals(h2e)
plot(h2e_res, rank = T)
testDispersion(h2e_res)
plotResiduals(h2e_res, carcass_level_summary$site_name, xlab = "Site", main=NULL)

# H2f - Hypothesis 7: Scavenger abundance predicts scavenging rates -----
h2f <- glmmTMB(full_scav_prob~ #full_scav_prob (1/0) binary probability of any scavenging activity
                 common_raven_adj +
                 american_crow_adj +
                 deer_mouse_adj +
                 (1|site_name), #site as random effect
               family = "binomial", #binomial distribution
               data=carcass_level_summary);summary(h2f) 

# Check h2f assumptions with DHARMa package
h2f_res = simulateResiduals(h2f)
plot(h2f_res, rank = T)
testDispersion(h2f_res)
plotResiduals(h2f_res, carcass_level_summary$site_name, xlab = "Site", main=NULL)


# H2g - Hypothesis 8: Scavenger species richness predicts scavenging rates -----
h2g <- glmmTMB(full_scav_prob~ #full_scav_prob (1/0) binary probability of any scavenging activity
                 richness +               
                 (1|site_name), #site as random effect
               family = "binomial", #binomial distribution
               data=carcass_level_summary);summary(h2g) 

# Check h2g assumptions with DHARMa package
h2g_res = simulateResiduals(h2g)
plot(h2g_res, rank = T)
testDispersion(h2g_res)
plotResiduals(h2g_res, carcass_level_summary$site_name, xlab = "Site", main=NULL)


# H2h - Hypothesis 9: Deployment period and human visitation predict scavenging rates -----
h2h <- glmmTMB(full_scav_prob~ #full_scav_prob (1/0) binary probability of any scavenging activity
                 deployment_type_AM_PM +
                 human_visitors_per_day +
                 (1|site_name), #site as random effect
               family = "binomial", #binomial distribution
               data=carcass_level_summary);summary(h2h) 

# Check h2e assumptions with DHARMa package
h2h_res = simulateResiduals(h2h)
plot(h2h_res, rank = T)
testDispersion(h2h_res)
plotResiduals(h2h_res, carcass_level_summary$site_name, xlab = "Site", main=NULL)


# h2i - null model -----
h2i <- glmmTMB(full_scav_prob~ #full_scav_prob (1/0) binary probability of any scavenging activity
                 1 +               
                 (1|site_name), #site as random effect
               family = "binomial", #binomial distribution
               data=carcass_level_summary);summary(h2i) 

# Check h2h assumptions with DHARMa package
h2i_res = simulateResiduals(h2h)
plot(h2i_res, rank = T)
testDispersion(h2i_res)
plotResiduals(h2i_res, carcass_level_summary$site_name, xlab = "Site", main=NULL)

# Compare Models: ####

all_removal_prob_models <- aictab(cand.set=list(g2, h2a, h2b, h2c, h2d, h2e, h2f, h2g, h2h, h2i),
                               modnames=(c("Urbanization (1km)",
                                           "Urbanization (1km) + Human Visitation",
                                           "Urbanization (1km) + Domestic Dog Visitation",
                                           "Agricultural (5km) + Human Visitation",
                                           "Urbanization (1km) + Deployment Type (Day/Night)",
                                           "Domestic Dog Visitation + Deployment Type (Day/Night)",
                                           "Scavenger Abundance (Common Raven MaxN* + American Crow MaxN + Deer Mouse MaxN)",
                                           "Scavenger Species Richness",
                                           "Human Visitation + Deployment Type (Day/Night)",
                                           "null")),
                               second.ord=F) %>% 
  mutate(across(c('AIC', 'Delta_AIC', "ModelLik", "AICWt", "LL", "Cum.Wt"), round, digits = 3))

#None of the models testing our hypotheses outperformed the null model. 


#################################################
# PART 3C: Time Until First Scavenging Event ####
#################################################

# H3a - Hypothesis 2: Urbanization + human visitation predicts scavenging rates --------
h3a <- glmmTMB(hours_to_first_scavenging_event~ 
                 percent_developed_1km +
               human_visitors_per_day +
               (1|site_name), #site as random effect
             family=Gamma(link = "log"), #gamma distribution with log link
             data=temporal_df);summary(h3a) #dataframe with no-scavenge carcasses removed

# Check h3a assumptions with DHARMa package
h3a_res = simulateResiduals(h3a)
plot(h3a_res, rank = T)
testDispersion(h3a_res)
plotResiduals(h3a_res, temporal_df$site_name, xlab = "Site", main=NULL)


# H3b - Hypothesis 3: Urbanization + domestic dog visitation predicts scavenging rates ---------------------
h3b <- glmmTMB(hours_to_first_scavenging_event~ 
               percent_developed_1km +
                 domestic_dog_visitors_per_day +
               (1|site_name), #site as random effect
             family=Gamma(link = "log"), #gamma distribution with log link
             data=temporal_df);summary(h3b) #dataframe with no-scavenge carcasses removed

# Check h3b assumptions with DHARMa package
h3b_res = simulateResiduals(h3b)
plot(h3b_res, rank = T)
testDispersion(h3b_res)
plotResiduals(h3b_res, temporal_df$site_name, xlab = "Site", main=NULL)


# H3c - Hypothesis 4: Agriculture + human visitation predicts scavenging rates ----------
h3c <- glmmTMB(hours_to_first_scavenging_event~ 
               percent_agricultural_5km + #3km agricultural extent
               human_visitors_per_day +
               (1|site_name), #site as random effect
               family=Gamma(link = "log"), #gamma distribution with log link
               data=temporal_df);summary(h3c) #dataframe with no-scavenge carcasses removed

# Check h3c assumptions with DHARMa package
h3c_res = simulateResiduals(h3c)
plot(h3c_res, rank = T)
testDispersion(h3c_res)
plotResiduals(h3c_res, temporal_df$site_name, xlab = "Site", main=NULL)


# H3d - Hypothesis 5: Urbanization and deployment period predicts scavenging rates -----
h3d <- glmmTMB(hours_to_first_scavenging_event~ 
               percent_developed_1km + #1km urbanization extent
               deployment_type_AM_PM+
               (1|site_name), #site as random effect
             family=Gamma(link = "log"), #gamma distribution with log link
             data=temporal_df);summary(h3d) #dataframe with no-scavenge carcasses removed

# Check h3d assumptions with DHARMa package
h3d_res = simulateResiduals(h3d)
plot(h3d_res, rank = T)
testDispersion(h3d_res)
plotResiduals(h3d_res, temporal_df$site_name, xlab = "Site", main=NULL)


# H3e - Hypothesis 6: Deployment period and domestic dog visitation predict scavenging rates -----
h3e <- glmmTMB(hours_to_first_scavenging_event~ 
                 deployment_type_AM_PM + 
               domestic_dog_visitors_per_day +
               (1|site_name), #site as random effect
             family=Gamma(link = "log"), #gamma distribution with log link
             data=temporal_df);summary(h3e) #dataframe with no-scavenge carcasses removed

# Check h3e assumptions with DHARMa package
h3e_res = simulateResiduals(h3e)
plot(h3e_res, rank = T)
testDispersion(h3e_res)
plotResiduals(h3e_res, temporal_df$site_name, xlab = "Site", main=NULL)

# H3f - Hypothesis 7: scavenger abundance predicts scavenging rates -----
h3f <- glmmTMB(hours_to_first_scavenging_event~ 
                 common_raven_adj +
                 american_crow_adj +
                 deer_mouse_adj +
               (1|site_name), #site as random effect
             family=Gamma(link = "log"), #gamma distribution with log link
             data=temporal_df);summary(h3f) #dataframe with no-scavenge carcasses removed

# Check h3f assumptions with DHARMa package
h3f_res = simulateResiduals(h3f)
plot(h3f_res, rank = T)
testDispersion(h3f_res)
plotResiduals(h3f_res, temporal_df$site_name, xlab = "Site", main=NULL)


# H3g - Hypothesis 8: Scavenger species richness predicts scavenging rates -----
h3g <- glmmTMB(hours_to_first_scavenging_event~ 
               richness +
               (1|site_name), #site as random effect
             family=Gamma(link = "log"), #gamma distribution with log link
             data=temporal_df);summary(h3g) #dataframe with no-scavenge carcasses removed

# Check h3e assumptions with DHARMa package
h3g_res = simulateResiduals(h3g)
plot(h3g_res, rank = T)
testDispersion(h3g_res)
plotResiduals(h3g_res, temporal_df$site_name, xlab = "Site", main=NULL)

# H3h - Hypothesis 6: Deployment period and human visitation predict scavenging rates -----
h3h <- glmmTMB(hours_to_first_scavenging_event~ 
                 deployment_type_AM_PM + 
                 human_visitors_per_day +
                 (1|site_name), #site as random effect
               family=Gamma(link = "log"), #gamma distribution with log link
               data=temporal_df);summary(h3h) #dataframe with no-scavenge carcasses removed

# Check h3h assumptions with DHARMa package
h3h_res = simulateResiduals(h3h)
plot(h3h_res, rank = T)
testDispersion(h3h_res)
plotResiduals(h3h_res, temporal_df$site_name, xlab = "Site", main=NULL)


# h3i - null model -----
h3i <- glmmTMB(hours_to_first_scavenging_event~ 
               1 +
               (1|site_name), #site as random effect
             family=Gamma(link = "log"), #gamma distribution with log link
             data=temporal_df);summary(h3i) #dataframe with no-scavenge carcasses removed

# Check h3i assumptions with DHARMa package
h3i_res = simulateResiduals(h3i)
plot(h3i_res, rank = T)
testDispersion(h3i_res)
plotResiduals(h3i_res, temporal_df$site_name, xlab = "Site", main=NULL)


# Compare Models: ####

all_first_scav_models <- aictab(cand.set=list(g3, h3a, h3b, h3c, h3d, 
                                              h3e, h3f, h3g, h3h, h3i),
                                  modnames=(c("Urbanization (1km)",
                                              "Urbanization (1km) + Human Visitation",
                                              "Urbanization (1km) + Domestic Dog Visitation",
                                              "Agricultural (5km) + Human Visitation",
                                              "Urbanization (1km) + Deployment Type (Day/Night)***",
                                              "Domestic Dog Visitation + Deployment Type (Day/Night)***",
                                              "Scavenger Abundance (Common Raven MaxN + American Crow MaxN + Deer Mouse MaxN)",
                                              "Scavenger Species Richness",
                                              "Human Visitation + Deployment Type (Day/Night)***",
                                              "null")),
                                second.ord=F) %>% 
  mutate(across(c('AIC', 'Delta_AIC', "ModelLik", "AICWt", "LL", "Cum.Wt"), round, digits = 3))

#################################################
# PART 3D: Time Until Carcass Removal ###########
#################################################

# h4a - Hypothesis 2: Urbanization + human visitation predicts scavenging rates --------
h4a <- glmmTMB(hours_to_full_scavenge~ 
                 percent_developed_1km +
                 human_visitors_per_day +
                 (1|site_name), #site as random effect
               family=Gamma(link = "log"), #gamma distribution with log link
               data=temporal_df2);summary(h4a) #dataframe with no-scavenge carcasses removed

# Check h4a assumptions with DHARMa package
h4a_res = simulateResiduals(h4a)
plot(h4a_res, rank = T)
testDispersion(h4a_res)
plotResiduals(h4a_res, temporal_df2$site_name, xlab = "Site", main=NULL)


# h4b - Hypothesis 3: Urbanization + domestic dog visitation predicts scavenging rates ---------------------
h4b <- glmmTMB(hours_to_full_scavenge~ 
                 percent_developed_1km +
                 domestic_dog_visitors_per_day +
                 (1|site_name), #site as random effect
               family=Gamma(link = "log"), #gamma distribution with log link
               data=temporal_df2);summary(h4b) #dataframe with no-scavenge carcasses removed

# Check h4b assumptions with DHARMa package
h4b_res = simulateResiduals(h4b)
plot(h4b_res, rank = T)
testDispersion(h4b_res)
plotResiduals(h4b_res, temporal_df2$site_name, xlab = "Site", main=NULL)


# h4c - Hypothesis 4: Agriculture + human visitation predicts scavenging rates --------
h4c <- glmmTMB(hours_to_full_scavenge~ 
                 percent_agricultural_5km + #5km agricultural extent
                 human_visitors_per_day +
                 (1|site_name), #site as random effect
               family=Gamma(link = "log"), #gamma distribution with log link
               data=temporal_df2);summary(h4c) #dataframe with no-scavenge carcasses removed

# Check h4c assumptions with DHARMa package
h4c_res = simulateResiduals(h4c)
plot(h4c_res, rank = T)
testDispersion(h4c_res)
plotResiduals(h4c_res, temporal_df2$site_name, xlab = "Site", main=NULL)


# h4d - Hypothesis 5: Urbanization and deployment period predicts scavenging rates -----
h4d <- glmmTMB(hours_to_full_scavenge~ 
                 percent_developed_1km + #1km urbanization extent
                 deployment_type_AM_PM+
                 (1|site_name), #site as random effect
               family=Gamma(link = "log"), #gamma distribution with log link
               data=temporal_df2);summary(h4d) #dataframe with no-scavenge carcasses removed

# Check h4d assumptions with DHARMa package
h4d_res = simulateResiduals(h4d)
plot(h4d_res, rank = T)
testDispersion(h4d_res)
plotResiduals(h4d_res, temporal_df2$site_name, xlab = "Site", main=NULL)


# h4e - Hypothesis 6: Deployment period and domestic dog visitation predict scavenging rates -----
h4e <- glmmTMB(hours_to_full_scavenge~ 
                 deployment_type_AM_PM + 
                 domestic_dog_visitors_per_day +
                 (1|site_name), #site as random effect
               family=Gamma(link = "log"), #gamma distribution with log link
               data=temporal_df2);summary(h4e) #dataframe with no-scavenge carcasses removed

# Check h4e assumptions with DHARMa package
h4e_res = simulateResiduals(h4e)
plot(h4e_res, rank = T)
testDispersion(h4e_res)
plotResiduals(h4e_res, temporal_df2$site_name, xlab = "Site", main=NULL)

# h4f - Hypothesis 7: scavenger abundance predicts scavenging rates -----
h4f <- glmmTMB(hours_to_full_scavenge~ 
                 common_raven_adj +
                 american_crow_adj +
                 deer_mouse_adj +
                 (1|site_name), #site as random effect
               family=Gamma(link = "log"), #gamma distribution with log link
               data=temporal_df2);summary(h4f) #dataframe with no-scavenge carcasses removed

# Check h4f assumptions with DHARMa package
h4f_res = simulateResiduals(h4f)
plot(h4f_res, rank = T)
testDispersion(h4f_res)
plotResiduals(h4f_res, temporal_df2$site_name, xlab = "Site", main=NULL)


# h4g - Hypothesis 8: Scavenger species richness predicts scavenging rates -----
h4g <- glmmTMB(hours_to_full_scavenge~ 
                 richness +
                 (1|site_name), #site as random effect
               family=Gamma(link = "log"), #gamma distribution with log link
               data=temporal_df2);summary(h4g) #dataframe with no-scavenge carcasses removed

# Check h4e assumptions with DHARMa package
h4g_res = simulateResiduals(h4g)
plot(h4g_res, rank = T)
testDispersion(h4g_res)
plotResiduals(h4g_res, temporal_df2$site_name, xlab = "Site", main=NULL)


# h4h - Hypothesis 9: Deployment period and human visitation predict scavenging rates -----
h4h <- glmmTMB(hours_to_full_scavenge~ 
                 deployment_type_AM_PM + 
                 human_visitors_per_day +
                 (1|site_name), #site as random effect
               family=Gamma(link = "log"), #gamma distribution with log link
               data=temporal_df2);summary(h4h) #dataframe with no-scavenge carcasses removed

# Check h4h assumptions with DHARMa package
h4h_res = simulateResiduals(h4h)
plot(h4h_res, rank = T)
testDispersion(h4h_res)
plotResiduals(h4h_res, temporal_df2$site_name, xlab = "Site", main=NULL)


# h4i - null model -----
h4i <- glmmTMB(hours_to_full_scavenge~ 
                 1 +
                 (1|site_name), #site as random effect
               family=Gamma(link = "log"), #gamma distribution with log link
               data=temporal_df2);summary(h4i) #dataframe with no-scavenge carcasses removed

# Check h4i assumptions with DHARMa package
h4i_res = simulateResiduals(h4i)
plot(h4i_res, rank = T)
testDispersion(h4i_res)
plotResiduals(h4i_res, temporal_df2$site_name, xlab = "Site", main=NULL)


# Compare Models: ####

all_removal_time_models <- aictab(cand.set=list(g4, h4a, h4b, h4c, h4d, 
                                              h4e, h4f, h4g, h4h, h4i),
                                modnames=(c("Urbanization (1km)",
                                            "Urbanization (1km) + Human Visitation",
                                            "Urbanization (1km) + Domestic Dog Visitation",
                                            "Agricultural (5km) + Human Visitation",
                                            "Urbanization (1km) + Deployment Type (Day/Night)***",
                                            "Domestic Dog Visitation + Deployment Type (Day/Night)***",
                                            "Scavenger Abundance (Common Raven MaxN + American Crow MaxN + Deer Mouse MaxN)",
                                            "Scavenger Species Richness",
                                            "Human Visitation + Deployment Type (Day/Night)***",
                                            "null")),
                                second.ord=F) %>% 
  mutate(across(c('AIC', 'Delta_AIC', "ModelLik", "AICWt", "LL", "Cum.Wt"), round, digits = 3))


#################################################
# PART 4: Export Model Summary Tables ###########
#################################################

#Table S7: Scavenging Probability Summary Table -------------------------------

scav_prob_gt <- gt(all_scav_prob_models)

scav_prob_summary <- 
  scav_prob_gt |>
  tab_header(
    title = "Carrion Processing Analysis Summary: (1) Probability of Carcass Scavenging",
    subtitle = "Linear Mixed-Effects Models with Binomial Distributions and Logit Link Functions"
  ) |>
  cols_label(Modnames = md("**Model terms**"),
             K = md("**K**"),
             AIC = md("**AIC**"),
             Delta_AIC = md("**∆AIC**"),
             ModelLik = md("**Model Likelihood**"),
             AICWt = md("**AIC Weight**"),
             LL = md("**Log Likelihood**"),
             Cum.Wt = md("**Cumulative Weight**"))
scav_prob_summary


#Export high-quality table
gtsave(scav_prob_summary, "output/supp_figures/scavenging_probability_table.pdf")


#Table S8: Carcass Removal Summary Table --------------------------------------

removal_prob_gt <- gt(all_removal_prob_models)

removal_prob_summary <- 
  removal_prob_gt |>
  tab_header(
    title = "Carrion Processing Analysis Summary: (2) Probability of Carcass Removal",
    subtitle = "Linear Mixed-Effects Models with Binomial Distributions and Logit Link Functions"
  ) |>
  cols_label(Modnames = md("**Model terms**"),
             K = md("**K**"),
             AIC = md("**AIC**"),
             Delta_AIC = md("**∆AIC**"),
             ModelLik = md("**Model Likelihood**"),
             AICWt = md("**AIC Weight**"),
             LL = md("**Log Likelihood**"),
             Cum.Wt = md("**Cumulative Weight**"))
removal_prob_summary


#Export high-quality table
gtsave(removal_prob_summary, "output/supp_figures/removal_probability_table.pdf")


#Table S9: Time to First Scavenge Summary Table --------------------------------

first_scav_gt <- gt(all_first_scav_models)

first_scav_summary <- 
  first_scav_gt |>
  tab_header(
    title = "Carrion Processing Analysis Summary: (3) Time until first scavenging event",
    subtitle = "Linear Mixed-Effects Models with Gamma Distributions and Log Link Functions"
  ) |>
  cols_label(Modnames = md("**Model terms**"),
             K = md("**K**"),
             AIC = md("**AIC**"),
             Delta_AIC = md("**∆AIC**"),
             ModelLik = md("**Model Likelihood**"),
             AICWt = md("**AIC Weight**"),
             LL = md("**Log Likelihood**"),
             Cum.Wt = md("**Cumulative Weight**"))
first_scav_summary


#Export high-quality table
gtsave(first_scav_summary, "output/supp_figures/time_to_first_scavenging_table.pdf")


#Table S10: Time to Carcass Removal Summary Table --------------------------------

removal_time_gt <- gt(all_removal_time_models)

removal_time_summary <- 
  removal_time_gt |>
  tab_header(
    title = "Carrion Processing Analysis Summary: (4) Time Until Carcass Removal",
    subtitle = "Linear Mixed-Effects Models with Gamma Distributions and Log Link Functions"
  ) |>
  cols_label(Modnames = md("**Model terms**"),
             K = md("**K**"),
             AIC = md("**AIC**"),
             Delta_AIC = md("**∆AIC**"),
             ModelLik = md("**Model Likelihood**"),
             AICWt = md("**AIC Weight**"),
             LL = md("**Log Likelihood**"),
             Cum.Wt = md("**Cumulative Weight**"))
removal_time_summary


#Export high-quality table
gtsave(removal_time_summary, "output/supp_figures/time_to_carcass_removal_table.pdf")

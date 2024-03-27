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
         deer_mouse_adj = deer_mouse/n_fish_deployed,
         rat_adj = rat/n_fish_deployed,
         domestic_dog_adj = domestic_dog/n_fish_deployed)


carcass_level_summary <- read_csv("data/processed/carcass_level_summary.csv") %>%    
  #Create new column "scav_prob" which classifies scavenging as 1/0 for each carcass (partially scavenged carcasses are classified as scavenged = 1)
  mutate(scav_prob = if_else(no_scavenge == TRUE, 0, 1),
         #Create new column "full_scav_prob" which classifies scavenging as 1/0 for each carcass that was fully scavenged AKA carcass removal)
         full_scav_prob = if_else(no_scavenge == FALSE & partial_scavenge == FALSE, 1, 0)) %>% 
  #bring environmental variables and adjusted species abundances into dataframe
  left_join(., urban_scavengers_summary[,c("site_name", "percent_developed_1km", "percent_agricultural_1km", "human_visitors_per_day", "domestic_dog_visitors_per_day", "common_raven_adj", "american_crow_adj", "deer_mouse_adj", "rat_adj", "domestic_dog_adj", "richness", "diversity")], by = c("site_name")) %>% 
#  filter(site_name!="Strawberry") %>% 
  
  #convert site name to factor for random effects
  mutate(site_name = factor(site_name)) 


##################################################################################
# PART 2: Does Urbanization Influence Carrion Processing Metrics? ################
##################################################################################

# PART 2A: Analyze deployment data for the probability of any scavenging ----------

## Model fitting 

# G1: Analyze probability of scavenging using using 1km urbanization buffer with a mixed-effects logistic regression (generalized linear mixed effects model with binomial distribution and logit link)
g1=glmer(scav_prob~ #scav_prob (1/0) binary probability of any scavenging activity
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
g2=glmer(full_scav_prob~ #full_scav_prob (1/0) binary probability of carcass removal
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

# H1a - Hypothesis 2: Human visitation predicts scavenging rates ---------------------
h1a <- glmer(scav_prob~ #scav_prob (1/0) binary probability of any scavenging activity
              human_visitors_per_day +
              (1|site_name), #site as random effect
            family = "binomial", #binomial distribution
            data=carcass_level_summary);summary(h1a) 

# Check h1a assumptions with DHARMa package
h1a_res = simulateResiduals(h1a)
plot(h1a_res, rank = T)
testDispersion(h1a_res)
plotResiduals(h1a_res, carcass_level_summary$site_name, xlab = "Site", main=NULL)


# H1b - Hypothesis 3: Domestic dog visitation predicts scavenging rates ---------------------
h1b <- glmer(scav_prob~ #scav_prob (1/0) binary probability of any scavenging activity
               domestic_dog_visitors_per_day +
               (1|site_name), #site as random effect
             family = "binomial", #binomial distribution
             data=carcass_level_summary);summary(h1b) 

# Check h1b assumptions with DHARMa package
h1b_res = simulateResiduals(h1b)
plot(h1b_res, rank = T)
testDispersion(h1b_res)
plotResiduals(h1b_res, carcass_level_summary$site_name, xlab = "Site", main=NULL)


# H1c - Hypothesis 4: Land cover predicts scavenging rates ---------------------
h1c <- glmer(scav_prob~ #scav_prob (1/0) binary probability of any scavenging activity
               percent_developed_1km + #1km urbanization extent
               percent_agricultural_1km + #1km agricultural extent
               (1|site_name), #site as random effect
             family = "binomial", #binomial distribution
             data=carcass_level_summary);summary(h1c) 

# Check h1c assumptions with DHARMa package
h1c_res = simulateResiduals(h1c)
plot(h1c_res, rank = T)
testDispersion(h1c_res)
plotResiduals(h1c_res, carcass_level_summary$site_name, xlab = "Site", main=NULL)


# H1d - Hypothesis 5: Urbanization and human visitation predict scavenging rates -----
h1d <- glmer(scav_prob~ #scav_prob (1/0) binary probability of any scavenging activity
               percent_developed_1km + #1km urbanization extent
               human_visitors_per_day +
               (1|site_name), #site as random effect
             family = "binomial", #binomial distribution
             data=carcass_level_summary);summary(h1d) 

# Check h1d assumptions with DHARMa package
h1d_res = simulateResiduals(h1d)
plot(h1d_res, rank = T)
testDispersion(h1d_res)
plotResiduals(h1d_res, carcass_level_summary$site_name, xlab = "Site", main=NULL)


# H1e - Hypothesis 6: Urbanization and domestic dog visitation predict scavenging rates -----
h1e <- glmer(scav_prob~ #scav_prob (1/0) binary probability of any scavenging activity
               percent_developed_1km + #1km urbanization extent
               domestic_dog_visitors_per_day +
               (1|site_name), #site as random effect
             family = "binomial", #binomial distribution
             data=carcass_level_summary);summary(h1e) 

# Check h1e assumptions with DHARMa package
h1e_res = simulateResiduals(h1e)
plot(h1e_res, rank = T)
testDispersion(h1e_res)
plotResiduals(h1e_res, carcass_level_summary$site_name, xlab = "Site", main=NULL)

# H1f - Hypothesis 7: Agriculture and human visitation predict scavenging rates -----
h1f <- glmer(scav_prob~ #scav_prob (1/0) binary probability of any scavenging activity
               percent_agricultural_1km + #1km urbanization extent
               human_visitors_per_day +
               (1|site_name), #site as random effect
             family = "binomial", #binomial distribution
             data=carcass_level_summary);summary(h1f) 

# Check h1f assumptions with DHARMa package
h1f_res = simulateResiduals(h1f)
plot(h1f_res, rank = T)
testDispersion(h1f_res)
plotResiduals(h1f_res, carcass_level_summary$site_name, xlab = "Site", main=NULL)


# H1g - Hypothesis 8: Agriculture and domestic dog visitation predict scavenging rates -----
h1g <- glmer(scav_prob~ #scav_prob (1/0) binary probability of any scavenging activity
               percent_agricultural_1km + #1km urbanization extent
               domestic_dog_visitors_per_day +
               (1|site_name), #site as random effect
             family = "binomial", #binomial distribution
             data=carcass_level_summary);summary(h1g) 

# Check h1e assumptions with DHARMa package
h1g_res = simulateResiduals(h1g)
plot(h1g_res, rank = T)
testDispersion(h1g_res)
plotResiduals(h1g_res, carcass_level_summary$site_name, xlab = "Site", main=NULL)


# H1h - Hypothesis 9: Recreation (human visitation and domestic dog visitation) predict scavenging rates -----
h1h <- glmer(scav_prob~ #scav_prob (1/0) binary probability of any scavenging activity
               human_visitors_per_day +
               domestic_dog_visitors_per_day +
               (1|site_name), #site as random effect
             family = "binomial", #binomial distribution
             data=carcass_level_summary);summary(h1h) 

# Check h1h assumptions with DHARMa package
h1h_res = simulateResiduals(h1h)
plot(h1h_res, rank = T)
testDispersion(h1h_res)
plotResiduals(h1h_res, carcass_level_summary$site_name, xlab = "Site", main=NULL)

# H1i - Hypothesis 10: Urbanization and recreation predict scavenging rates -----
h1i <- glmer(scav_prob~ #scav_prob (1/0) binary probability of any scavenging activity
               percent_developed_1km + 
               human_visitors_per_day +
               domestic_dog_visitors_per_day +
               (1|site_name), #site as random effect
             family = "binomial", #binomial distribution
             data=carcass_level_summary);summary(h1i) 

# Check h1i assumptions with DHARMa package
h1i_res = simulateResiduals(h1i)
plot(h1i_res, rank = T)
testDispersion(h1i_res)
plotResiduals(h1i_res, carcass_level_summary$site_name, xlab = "Site", main=NULL)


# H1j - Hypothesis 11: Land cover and recreation predict scavenging rates -------------
h1j <- glmer(scav_prob~ #scav_prob (1/0) binary probability of any scavenging activity
               percent_developed_1km + 
               percent_agricultural_1km +
               human_visitors_per_day +
               domestic_dog_visitors_per_day +
               (1|site_name), #site as random effect
             family = "binomial", #binomial distribution
             data=carcass_level_summary);summary(h1h) 

# Check h1j assumptions with DHARMa package
h1j_res = simulateResiduals(h1j)
plot(h1j_res, rank = T)
testDispersion(h1j_res)
plotResiduals(h1j_res, carcass_level_summary$site_name, xlab = "Site", main=NULL)

# H1k - Hypothesis 12: Scavenger abundance predicts scavenging rates -------------
h1k <- glmer(scav_prob~ #scav_prob (1/0) binary probability of any scavenging activity
               common_raven_adj +
               american_crow_adj +
               deer_mouse_adj +
               rat_adj +
               domestic_dog_adj +
               (1|site_name), #site as random effect
             family = "binomial", #binomial distribution
             data=carcass_level_summary);summary(h1k) 

# Check h1i assumptions with DHARMa package
h1k_res = simulateResiduals(h1k)
plot(h1k_res, rank = T)
testDispersion(h1k_res)
plotResiduals(h1k_res, carcass_level_summary$site_name, xlab = "Site", main=NULL)


# H1l - Hypothesis 13: Scavenger species richness predicts scavenging rates -------------
h1l <- glmer(scav_prob~ #scav_prob (1/0) binary probability of any scavenging activity
             richness +
             (1|site_name), #site as random effect
             family = "binomial", #binomial distribution
             data=carcass_level_summary);summary(h1l) 

# Check h1j assumptions with DHARMa package
h1l_res = simulateResiduals(h1l)
plot(h1l_res, rank = T)
testDispersion(h1l_res)
plotResiduals(h1l_res, carcass_level_summary$site_name, xlab = "Site", main=NULL)


# Compare Models: ####

arrange(AIC(h1a, h1b, h1c, h1d, h1e, h1f, h1g, h1h, h1i, h1j, h1k, h1l), AIC)



##############################################
# PART 3B: Probability of Carcass Removal ####
##############################################

# H2a - Hypothesis 2: Human visitation predicts scavenging rates ---------------------
h2a <- glmer(full_scav_prob~ #full_scav_prob (1/0) binary probability of carcass removal
               human_visitors_per_day +
               (1|site_name), #site as random effect
             family = "binomial", #binomial distribution
             data=carcass_level_summary);summary(h2a) 

# Check h2a assumptions with DHARMa package
h2a_res = simulateResiduals(h2a)
plot(h2a_res, rank = T)
testDispersion(h2a_res)
plotResiduals(h2a_res, carcass_level_summary$site_name, xlab = "Site", main=NULL)


# H2b - Hypothesis 3: Domestic dog visitation predicts scavenging rates ---------------------
h2b <- glmer(full_scav_prob~ #full_scav_prob (1/0) binary probability of carcass removal
               domestic_dog_visitors_per_day +
               (1|site_name), #site as random effect
             family = "binomial", #binomial distribution
             data=carcass_level_summary);summary(h2b) 

# Check h2b assumptions with DHARMa package
h2b_res = simulateResiduals(h2b)
plot(h2b_res, rank = T)
testDispersion(h2b_res)
plotResiduals(h2b_res, carcass_level_summary$site_name, xlab = "Site", main=NULL)


# H2c - Hypothesis 4: Land cover predicts scavenging rates ---------------------
h2c <- glmer(full_scav_prob~ #full_scav_prob (1/0) binary probability of carcass removal
               percent_developed_1km + #1km urbanization extent
               percent_agricultural_1km + #1km agricultural extent
               (1|site_name), #site as random effect
             family = "binomial", #binomial distribution
             data=carcass_level_summary);summary(h2c) 

# Check h2c assumptions with DHARMa package
h2c_res = simulateResiduals(h2c)
plot(h2c_res, rank = T)
testDispersion(h2c_res)
plotResiduals(h2c_res, carcass_level_summary$site_name, xlab = "Site", main=NULL)


# H2d - Hypothesis 5: Urbanization and human visitation predict scavenging rates -----
h2d <- glmer(full_scav_prob~ #full_scav_prob (1/0) binary probability of carcass removal
               percent_developed_1km + #1km urbanization extent
               human_visitors_per_day +
               (1|site_name), #site as random effect
             family = "binomial", #binomial distribution
             data=carcass_level_summary);summary(h2d) 

# Check h2d assumptions with DHARMa package
h2d_res = simulateResiduals(h2d)
plot(h2d_res, rank = T)
testDispersion(h2d_res)
plotResiduals(h2d_res, carcass_level_summary$site_name, xlab = "Site", main=NULL)


# H2e - Hypothesis 6: Urbanization and domestic dog visitation predict scavenging rates -----
h2e <- glmer(full_scav_prob~ #full_scav_prob (1/0) binary probability of carcass removal
               percent_developed_1km + #1km urbanization extent
               domestic_dog_visitors_per_day +
               (1|site_name), #site as random effect
             family = "binomial", #binomial distribution
             data=carcass_level_summary);summary(h2e) 

# Check h2e assumptions with DHARMa package
h2e_res = simulateResiduals(h2e)
plot(h2e_res, rank = T)
testDispersion(h2e_res)
plotResiduals(h2e_res, carcass_level_summary$site_name, xlab = "Site", main=NULL)

# H2f - Hypothesis 7: Agriculture and human visitation predict scavenging rates -----
h2f <- glmer(full_scav_prob~ #full_scav_prob (1/0) binary probability of carcass removal
               percent_agricultural_1km + #1km urbanization extent
               human_visitors_per_day +
               (1|site_name), #site as random effect
             family = "binomial", #binomial distribution
             data=carcass_level_summary);summary(h2f) 

# Check h2f assumptions with DHARMa package
h2f_res = simulateResiduals(h2f)
plot(h2f_res, rank = T)
testDispersion(h2f_res)
plotResiduals(h2f_res, carcass_level_summary$site_name, xlab = "Site", main=NULL)


# H2g - Hypothesis 8: Agriculture and domestic dog visitation predict scavenging rates -----
h2g <- glmer(full_scav_prob~ #full_scav_prob (1/0) binary probability of carcass removal
               percent_agricultural_1km + #1km urbanization extent
               domestic_dog_visitors_per_day +
               (1|site_name), #site as random effect
             family = "binomial", #binomial distribution
             data=carcass_level_summary);summary(h2g) 

# Check h2e assumptions with DHARMa package
h2g_res = simulateResiduals(h2g)
plot(h2g_res, rank = T)
testDispersion(h2g_res)
plotResiduals(h2g_res, carcass_level_summary$site_name, xlab = "Site", main=NULL)


# H2h - Hypothesis 9: Recreation (human visitation and domestic dog visitation) predict scavenging rates -----
h2h <- glmer(full_scav_prob~ #full_scav_prob (1/0) binary probability of carcass removal
               human_visitors_per_day +
               domestic_dog_visitors_per_day +
               (1|site_name), #site as random effect
             family = "binomial", #binomial distribution
             data=carcass_level_summary);summary(h2h) 

# Check h2h assumptions with DHARMa package
h2h_res = simulateResiduals(h2h)
plot(h2h_res, rank = T)
testDispersion(h2h_res)
plotResiduals(h2h_res, carcass_level_summary$site_name, xlab = "Site", main=NULL)

# H2i - Hypothesis 10: Urbanization and recreation predict scavenging rates -----
h2i <- glmer(full_scav_prob~ #full_scav_prob (1/0) binary probability of carcass removal
               percent_developed_1km + 
               human_visitors_per_day +
               domestic_dog_visitors_per_day +
               (1|site_name), #site as random effect
             family = "binomial", #binomial distribution
             data=carcass_level_summary);summary(h2i) 

# Check h2i assumptions with DHARMa package
h2i_res = simulateResiduals(h2i)
plot(h2i_res, rank = T)
testDispersion(h2i_res)
plotResiduals(h2i_res, carcass_level_summary$site_name, xlab = "Site", main=NULL)


# H2j - Hypothesis 11: Land cover and recreation predict scavenging rates -------------
h2j <- glmer(full_scav_prob~ #full_scav_prob (1/0) binary probability of carcass removal
               percent_developed_1km + 
               percent_agricultural_1km +
               human_visitors_per_day +
               domestic_dog_visitors_per_day +
               (1|site_name), #site as random effect
             family = "binomial", #binomial distribution
             data=carcass_level_summary);summary(h2h) 

# Check h2j assumptions with DHARMa package
h2j_res = simulateResiduals(h2j)
plot(h2j_res, rank = T)
testDispersion(h2j_res)
plotResiduals(h2j_res, carcass_level_summary$site_name, xlab = "Site", main=NULL)

# H2k - Hypothesis 12: Scavenger abundance predicts scavenging rates -------------
h2k <- glmer(full_scav_prob~ #full_scav_prob (1/0) binary probability of carcass removal
               common_raven_adj +
               american_crow_adj +
               deer_mouse_adj +
               rat_adj +
               domestic_dog_adj +
               (1|site_name), #site as random effect
             family = "binomial", #binomial distribution
             data=carcass_level_summary);summary(h2k) 

# Check h2i assumptions with DHARMa package
h2k_res = simulateResiduals(h2k)
plot(h2k_res, rank = T)
testDispersion(h2k_res)
plotResiduals(h2k_res, carcass_level_summary$site_name, xlab = "Site", main=NULL)


# H2l - Hypothesis 13: Scavenger species richness predicts scavenging rates -------------
h2l <- glmer(full_scav_prob~ #full_scav_prob (1/0) binary probability of carcass removal
               richness +
               (1|site_name), #site as random effect
             family = "binomial", #binomial distribution
             data=carcass_level_summary);summary(h2l) 

# Check h2j assumptions with DHARMa package
h2l_res = simulateResiduals(h2l)
plot(h2l_res, rank = T)
testDispersion(h2l_res)
plotResiduals(h2l_res, carcass_level_summary$site_name, xlab = "Site", main=NULL)


# Compare Models: ####

arrange(AIC(h2a, h2b, h2c, h2d, h2e, h2f, h2g, h2h, h2i, h2j, h2k, h2l), AIC)




#################################################
# PART 3C: Time Until First Scavenging Event ####
#################################################

# H3a - Hypothesis 2: Human visitation predicts scavenging rates ---------------------
h3a <- glmmTMB(hours_to_first_scavenging_event~ 
               human_visitors_per_day +
               (1|site_name), #site as random effect
             family=Gamma(link = "log"), #gamma distribution with log link
             data=temporal_df);summary(h3a) #dataframe with no-scavenge carcasses removed

# Check h3a assumptions with DHARMa package
h3a_res = simulateResiduals(h3a)
plot(h3a_res, rank = T)
testDispersion(h3a_res)
plotResiduals(h3a_res, temporal_df$site_name, xlab = "Site", main=NULL)


# H3b - Hypothesis 3: Domestic dog visitation predicts scavenging rates ---------------------
h3b <- glmmTMB(hours_to_first_scavenging_event~ 
               domestic_dog_visitors_per_day +
               (1|site_name), #site as random effect
             family=Gamma(link = "log"), #gamma distribution with log link
             data=temporal_df);summary(h3b) #dataframe with no-scavenge carcasses removed

# Check h3b assumptions with DHARMa package
h3b_res = simulateResiduals(h3b)
plot(h3b_res, rank = T)
testDispersion(h3b_res)
plotResiduals(h3b_res, temporal_df$site_name, xlab = "Site", main=NULL)


# H3c - Hypothesis 4: Land cover predicts scavenging rates ---------------------
h3c <- glmmTMB(hours_to_first_scavenging_event~ 
               percent_developed_1km + #1km urbanization extent
               percent_agricultural_1km + #1km agricultural extent
               (1|site_name), #site as random effect
             family=Gamma(link = "log"), #gamma distribution with log link
             data=temporal_df);summary(h3c) #dataframe with no-scavenge carcasses removed

# Check h3c assumptions with DHARMa package
h3c_res = simulateResiduals(h3c)
plot(h3c_res, rank = T)
testDispersion(h3c_res)
plotResiduals(h3c_res, temporal_df$site_name, xlab = "Site", main=NULL)


# H3d - Hypothesis 5: Urbanization and human visitation predict scavenging rates -----
h3d <- glmmTMB(hours_to_first_scavenging_event~ 
               percent_developed_1km + #1km urbanization extent
               human_visitors_per_day +
               (1|site_name), #site as random effect
             family=Gamma(link = "log"), #gamma distribution with log link
             data=temporal_df);summary(h3d) #dataframe with no-scavenge carcasses removed

# Check h3d assumptions with DHARMa package
h3d_res = simulateResiduals(h3d)
plot(h3d_res, rank = T)
testDispersion(h3d_res)
plotResiduals(h3d_res, temporal_df$site_name, xlab = "Site", main=NULL)


# H3e - Hypothesis 6: Urbanization and domestic dog visitation predict scavenging rates -----
h3e <- glmmTMB(hours_to_first_scavenging_event~ 
               percent_developed_1km + #1km urbanization extent
               domestic_dog_visitors_per_day +
               (1|site_name), #site as random effect
             family=Gamma(link = "log"), #gamma distribution with log link
             data=temporal_df);summary(h3e) #dataframe with no-scavenge carcasses removed

# Check h3e assumptions with DHARMa package
h3e_res = simulateResiduals(h3e)
plot(h3e_res, rank = T)
testDispersion(h3e_res)
plotResiduals(h3e_res, temporal_df$site_name, xlab = "Site", main=NULL)

# H3f - Hypothesis 7: Agriculture and human visitation predict scavenging rates -----
h3f <- glmmTMB(hours_to_first_scavenging_event~ 
               percent_agricultural_1km + #1km urbanization extent
               human_visitors_per_day +
               (1|site_name), #site as random effect
             family=Gamma(link = "log"), #gamma distribution with log link
             data=temporal_df);summary(h3f) #dataframe with no-scavenge carcasses removed

# Check h3f assumptions with DHARMa package
h3f_res = simulateResiduals(h3f)
plot(h3f_res, rank = T)
testDispersion(h3f_res)
plotResiduals(h3f_res, temporal_df$site_name, xlab = "Site", main=NULL)


# H3g - Hypothesis 8: Agriculture and domestic dog visitation predict scavenging rates -----
h3g <- glmmTMB(hours_to_first_scavenging_event~ 
               percent_agricultural_1km + #1km urbanization extent
               domestic_dog_visitors_per_day +
               (1|site_name), #site as random effect
             family=Gamma(link = "log"), #gamma distribution with log link
             data=temporal_df);summary(h3g) #dataframe with no-scavenge carcasses removed

# Check h3e assumptions with DHARMa package
h3g_res = simulateResiduals(h3g)
plot(h3g_res, rank = T)
testDispersion(h3g_res)
plotResiduals(h3g_res, temporal_df$site_name, xlab = "Site", main=NULL)


# H3h - Hypothesis 9: Recreation (human visitation and domestic dog visitation) predict scavenging rates -----
h3h <- glmmTMB(hours_to_first_scavenging_event~ 
               human_visitors_per_day +
               domestic_dog_visitors_per_day +
               (1|site_name), #site as random effect
             family=Gamma(link = "log"), #gamma distribution with log link
             data=temporal_df);summary(h3h) #dataframe with no-scavenge carcasses removed

# Check h3h assumptions with DHARMa package
h3h_res = simulateResiduals(h3h)
plot(h3h_res, rank = T)
testDispersion(h3h_res)
plotResiduals(h3h_res, temporal_df$site_name, xlab = "Site", main=NULL)

# H3i - Hypothesis 10: Urbanization and recreation predict scavenging rates -----
h3i <- glmmTMB(hours_to_first_scavenging_event~ 
               percent_developed_1km + 
               human_visitors_per_day +
               domestic_dog_visitors_per_day +
               (1|site_name), #site as random effect
             family=Gamma(link = "log"), #gamma distribution with log link
             data=temporal_df);summary(h3i) #dataframe with no-scavenge carcasses removed

# Check h3i assumptions with DHARMa package
h3i_res = simulateResiduals(h3i)
plot(h3i_res, rank = T)
testDispersion(h3i_res)
plotResiduals(h3i_res, temporal_df$site_name, xlab = "Site", main=NULL)


# H3j - Hypothesis 11: Land cover and recreation predict scavenging rates -------------
h3j <- glmmTMB(hours_to_first_scavenging_event~ 
               percent_developed_1km + 
               percent_agricultural_1km +
               human_visitors_per_day +
               domestic_dog_visitors_per_day +
               (1|site_name), #site as random effect
             family=Gamma(link = "log"), #gamma distribution with log link
             data=temporal_df);summary(h3h) #dataframe with no-scavenge carcasses removed

# Check h3j assumptions with DHARMa package
h3j_res = simulateResiduals(h3j)
plot(h3j_res, rank = T)
testDispersion(h3j_res)
plotResiduals(h3j_res, temporal_df$site_name, xlab = "Site", main=NULL)

# H3k - Hypothesis 12: Scavenger abundance predicts scavenging rates -------------
h3k <- glmmTMB(hours_to_first_scavenging_event~ 
               common_raven_adj +
               american_crow_adj +
               deer_mouse_adj +
               rat_adj +
               domestic_dog_adj +
               (1|site_name), #site as random effect
             family=Gamma(link = "log"), #gamma distribution with log link
             data=temporal_df);summary(h3k) #dataframe with no-scavenge carcasses removed

# Check h3k assumptions with DHARMa package
h3k_res = simulateResiduals(h3k)
plot(h3k_res, rank = T)
testDispersion(h3k_res)
plotResiduals(h3k_res, temporal_df$site_name, xlab = "Site", main=NULL)


# H3l - Hypothesis 13: Scavenger species richness predicts scavenging rates -------------
h3l <- glmmTMB(hours_to_first_scavenging_event~ 
               richness +
               (1|site_name), #site as random effect
             family=Gamma(link = "log"), #gamma distribution with log link
             data=temporal_df);summary(h3l) #dataframe with no-scavenge carcasses removed

# Check h3l assumptions with DHARMa package
h3l_res = simulateResiduals(h3l)
plot(h3l_res, rank = T)
testDispersion(h3l_res)
plotResiduals(h3l_res, temporal_df$site_name, xlab = "Site", main=NULL)


# Compare Models: ####

arrange(AIC(h3a, h3b, h3c, h3d, h3e, h3f, h3g, h3h, h3i, h3j, h3k, h3l), AIC)


#################################################
# PART 3D: Time Until Carcass Removal ###########
#################################################

# H4a - Hypothesis 2: Human visitation predicts scavenging rates ---------------------
h4a <- glmmTMB(hours_to_full_scavenge~ 
                 human_visitors_per_day +
                 (1|site_name), #site as random effect
               family=Gamma(link = "log"), #gamma distribution with log link
               data=temporal_df2);summary(h4a) #dataframe with only complete carcass removal

# Check h4a assumptions with DHARMa package
h4a_res = simulateResiduals(h4a)
plot(h4a_res, rank = T)
testDispersion(h4a_res)
plotResiduals(h4a_res, temporal_df2$site_name, xlab = "Site", main=NULL)


# H4b - Hypothesis 3: Domestic dog visitation predicts scavenging rates ---------------------
h4b <- glmmTMB(hours_to_full_scavenge~ 
                 domestic_dog_visitors_per_day +
                 (1|site_name), #site as random effect
               family=Gamma(link = "log"), #gamma distribution with log link
               data=temporal_df2);summary(h4b) #dataframe with only complete carcass removal

# Check h4b assumptions with DHARMa package
h4b_res = simulateResiduals(h4b)
plot(h4b_res, rank = T)
testDispersion(h4b_res)
plotResiduals(h4b_res, temporal_df2$site_name, xlab = "Site", main=NULL)


# H4c - Hypothesis 4: Land cover predicts scavenging rates ---------------------
h4c <- glmmTMB(hours_to_full_scavenge~ 
                 percent_developed_1km + #1km urbanization extent
                 percent_agricultural_1km + #1km agricultural extent
                 (1|site_name), #site as random effect
               family=Gamma(link = "log"), #gamma distribution with log link
               data=temporal_df2);summary(h4c) #dataframe with only complete carcass removal

# Check h4c assumptions with DHARMa package
h4c_res = simulateResiduals(h4c)
plot(h4c_res, rank = T)
testDispersion(h4c_res)
plotResiduals(h4c_res, temporal_df2$site_name, xlab = "Site", main=NULL)


# H4d - Hypothesis 5: Urbanization and human visitation predict scavenging rates -----
h4d <- glmmTMB(hours_to_full_scavenge~ 
                 percent_developed_1km + #1km urbanization extent
                 human_visitors_per_day +
                 (1|site_name), #site as random effect
               family=Gamma(link = "log"), #gamma distribution with log link
               data=temporal_df2);summary(h4d) #dataframe with only complete carcass removal

# Check h4d assumptions with DHARMa package
h4d_res = simulateResiduals(h4d)
plot(h4d_res, rank = T)
testDispersion(h4d_res)
plotResiduals(h4d_res, temporal_df2$site_name, xlab = "Site", main=NULL)


# H4e - Hypothesis 6: Urbanization and domestic dog visitation predict scavenging rates -----
h4e <- glmmTMB(hours_to_full_scavenge~ 
                 percent_developed_1km + #1km urbanization extent
                 domestic_dog_visitors_per_day +
                 (1|site_name), #site as random effect
               family=Gamma(link = "log"), #gamma distribution with log link
               data=temporal_df2);summary(h4e) #dataframe with only complete carcass removal

# Check h4e assumptions with DHARMa package
h4e_res = simulateResiduals(h4e)
plot(h4e_res, rank = T)
testDispersion(h4e_res)
plotResiduals(h4e_res, temporal_df2$site_name, xlab = "Site", main=NULL)

# H4f - Hypothesis 7: Agriculture and human visitation predict scavenging rates -----
h4f <- glmmTMB(hours_to_full_scavenge~ 
                 percent_agricultural_1km + #1km urbanization extent
                 human_visitors_per_day +
                 (1|site_name), #site as random effect
               family=Gamma(link = "log"), #gamma distribution with log link
               data=temporal_df2);summary(h4f) #dataframe with only complete carcass removal

# Check h4f assumptions with DHARMa package
h4f_res = simulateResiduals(h4f)
plot(h4f_res, rank = T)
testDispersion(h4f_res)
plotResiduals(h4f_res, temporal_df2$site_name, xlab = "Site", main=NULL)


# H4g - Hypothesis 8: Agriculture and domestic dog visitation predict scavenging rates -----
h4g <- glmmTMB(hours_to_full_scavenge~ 
                 percent_agricultural_1km + #1km urbanization extent
                 domestic_dog_visitors_per_day +
                 (1|site_name), #site as random effect
               family=Gamma(link = "log"), #gamma distribution with log link
               data=temporal_df2);summary(h4g) #dataframe with only complete carcass removal

# Check h4e assumptions with DHARMa package
h4g_res = simulateResiduals(h4g)
plot(h4g_res, rank = T)
testDispersion(h4g_res)
plotResiduals(h4g_res, temporal_df2$site_name, xlab = "Site", main=NULL)


# H4h - Hypothesis 9: Recreation (human visitation and domestic dog visitation) predict scavenging rates -----
h4h <- glmmTMB(hours_to_full_scavenge~ 
                 human_visitors_per_day +
                 domestic_dog_visitors_per_day +
                 (1|site_name), #site as random effect
               family=Gamma(link = "log"), #gamma distribution with log link
               data=temporal_df2);summary(h4h) #dataframe with only complete carcass removal

# Check h4h assumptions with DHARMa package
h4h_res = simulateResiduals(h4h)
plot(h4h_res, rank = T)
testDispersion(h4h_res)
plotResiduals(h4h_res, temporal_df2$site_name, xlab = "Site", main=NULL)

# H4i - Hypothesis 10: Urbanization and recreation predict scavenging rates -----
h4i <- glmmTMB(hours_to_full_scavenge~ 
                 percent_developed_1km + 
                 human_visitors_per_day +
                 domestic_dog_visitors_per_day +
                 (1|site_name), #site as random effect
               family=Gamma(link = "log"), #gamma distribution with log link
               data=temporal_df2);summary(h4i) #dataframe with only complete carcass removal

# Check h4i assumptions with DHARMa package
h4i_res = simulateResiduals(h4i)
plot(h4i_res, rank = T)
testDispersion(h4i_res)
plotResiduals(h4i_res, temporal_df2$site_name, xlab = "Site", main=NULL)


# H4j - Hypothesis 11: Land cover and recreation predict scavenging rates -------------
h4j <- glmmTMB(hours_to_full_scavenge~ 
                 percent_developed_1km + 
                 percent_agricultural_1km +
                 human_visitors_per_day +
                 domestic_dog_visitors_per_day +
                 (1|site_name), #site as random effect
               family=Gamma(link = "log"), #gamma distribution with log link
               data=temporal_df2);summary(h4h) #dataframe with only complete carcass removal

# Check h4j assumptions with DHARMa package
h4j_res = simulateResiduals(h4j)
plot(h4j_res, rank = T)
testDispersion(h4j_res)
plotResiduals(h4j_res, temporal_df2$site_name, xlab = "Site", main=NULL)

# H4k - Hypothesis 12: Scavenger abundance predicts scavenging rates -------------
h4k <- glmmTMB(hours_to_full_scavenge~ 
                 common_raven_adj +
                 american_crow_adj +
                 deer_mouse_adj +
                 rat_adj +
                 domestic_dog_adj +
                 (1|site_name), #site as random effect
               family=Gamma(link = "log"), #gamma distribution with log link
               data=temporal_df2);summary(h4k) #dataframe with only complete carcass removal

# Check h4k assumptions with DHARMa package
h4k_res = simulateResiduals(h4k)
plot(h4k_res, rank = T)
testDispersion(h4k_res)
plotResiduals(h4k_res, temporal_df2$site_name, xlab = "Site", main=NULL)


# H4l - Hypothesis 13: Scavenger species richness predicts scavenging rates -------------
h4l <- glmmTMB(hours_to_full_scavenge~ 
                 richness +
                 (1|site_name), #site as random effect
               family=Gamma(link = "log"), #gamma distribution with log link
               data=temporal_df2);summary(h4l) #dataframe with only complete carcass removal

# Check h4l assumptions with DHARMa package
h4l_res = simulateResiduals(h4l)
plot(h4l_res, rank = T)
testDispersion(h4l_res)
plotResiduals(h4l_res, temporal_df2$site_name, xlab = "Site", main=NULL)


# Compare Models: ####

arrange(AIC(h4a, h4b, h4c, h4d, h4e, h4f, h4g, h4h, h4i, h4j, h4k, h4l), AIC)


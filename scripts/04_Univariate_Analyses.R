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
  #convert site name to factor for random effects
  mutate(site_name = as.factor(site_name)) 


temp <- carcass_level_summary %>% 
  filter(site_name!="Strawberry")

##################################################################################
# PART 2: Does Urbanization Influence Carrion Processing Metrics? ################
##################################################################################

# PART 3A: Analyze deployment data for the probability of any scavenging ----------

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

# PART 3B: Analyze deployment data for the probability of carcass removal ----------

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



# PART 3C: Analyze deployment data for the time until first scavenging event----

#Create datafeame filtering out carcasses in which no scavenging occurred
temporal_df <- carcass_level_summary %>% 
  filter(hours_to_first_scavenging_event != "NA") %>% 
  mutate(site_name = factor(site_name)) #remove unused levels from factor

# Analyze time until first scavenging event using using 1km urbanization buffer with a generalized linear mixed-effects model with Gamma distribution and log link
g3=glmmTMB(hours_to_first_scavenging_event~ 
             percent_developed_1km+ #1km urbanization
             (1|site_name), #site as random effect
           family=Gamma(link = "log"), #gamma distribution with log link
           data=temporal_df) #dataframe with no-scavenge carcasses removed
summary(g3)

# Check g3 assumptions with DHARMa package
g3_res = simulateResiduals(g3)
plot(g3_res, rank = T)
testDispersion(g3_res)
plotResiduals(g3_res, temporal_df$site_name, xlab = "Site", main=NULL)


# PART 3D: Analyze time until carcass removal using 1km urbanization buffer with a generalized linear mixed-effects model with Gamma distribution and log link ---------

#Create datafeame filtering out carcasses in which either (1) no scavenging occurred or (2) carcass was only partially scavenged
temporal_df2 <- carcass_level_summary %>% 
  filter(hours_to_full_scavenge != "NA")%>% 
  mutate(site_name = factor(site_name)) #remove unused levels from factor

# Analyze time until complete carcass removal using using 1km buffer with a generalized linear mixed-effects model with Gamma distribution and log link
g4=glmmTMB(hours_to_full_scavenge~
             percent_developed_1km+ #1km urbanization
             (1|site_name), #site as random effect
           family=Gamma(link = "log"), #gamma distribution with log link
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

# H1: Analyze probability of scavenging using all predictors with a mixed-effects logistic regression (generalized linear mixed effects model with binomial distribution and logit link)

h1 <- glm(scav_prob~ #scav_prob (1/0) binary probability of any scavenging activity
              percent_developed_1km +
              percent_agricultural_1km + 
              human_visitors_per_day +
              domestic_dog_visitors_per_day + 
              common_raven_adj +
              american_crow_adj +
              deer_mouse_adj +
              rat_adj +
              domestic_dog_adj +
              richness +
              diversity, 
            #+
              #(1|site_name), #site as random effect
            family = "binomial", #binomial distribution
            data=temp);summary(h1) 



h2 <- glmer(scav_prob~ #scav_prob (1/0) binary probability of any scavenging activity
#              percent_developed_1km +
#              percent_agricultural_1km + #1km agricultural extent
#              human_visitors_per_day +
              domestic_dog_visitors_per_day + 
#                            common_raven_adj +
#                            american_crow_adj +
#                            deer_mouse_adj +
#                            rat_adj +
#                            domestic_dog_adj +
#              richness +
#              diversity +
              (1|site_name), #site as random effect
            family = "binomial", #binomial distribution
            data=carcass_level_summary);summary(h2) 


# Check g1b assumptions with DHARMa package
g1b_res = simulateResiduals(g1b)
plot(g1b_res, rank = T)
testDispersion(g1b_res)
plotResiduals(g1b_res, carcass_level_summary$site_name, xlab = "Site", main=NULL)

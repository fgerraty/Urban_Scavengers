###############################################################################
# Santa Cruz Urban Scavengers Project #########################################
# Author: Frankie Gerraty (frankiegerraty@gmail.com; fgerraty@ucsc.edu) #######
###############################################################################
# Script 02: Clean and Summarize Datasets #####################################
#------------------------------------------------------------------------------

####################################
# PART 1: Import and Clean Data ####
####################################

# PART 1A: Import and clean carcass deployment data ---------------------------

# Import carcass deployment data
deployments_raw <- read_csv("data/raw/Urban_Scavenger_Carcass_Deployments.csv")

#Clean carcass deployment data
deployments <- deployments_raw %>%
  clean_names()


# PART 1B: Import and clean scavenging data -----------------------------------

#Import camera trap data documenting all recorded videos / scavenging events
scav_data_raw <- read_csv("data/raw/Urban_Scavenger_Video_Scoring.csv", 
                          col_types = "nnctnnlllnnnnnnnnnnnnlll"
                         )

#Clean scavenger data
scav_data <- scav_data_raw %>%
  clean_names() %>% 
  rename(carcass_id = carcass_number)

#combine date and time for deployment duration calculations
scav_data$date_time = mdy_hms(paste(scav_data$date, scav_data$time))

#Export as .csv in "processed data" folder
write_csv(scav_data, "data/processed/scavenger_recordings.csv")


# PART 1C: Import and clean carcass mass data ----------------------------------

#Import camera trap data documenting all recorded videos / scavenging events
carcass_data_raw <- read_csv("data/raw/Urban_Scavenger_Carcasses.csv")

#Clean scavenger data
carcass_data <- carcass_data_raw %>%
  clean_names()


# PART 1D: Import buffers data from urbanization calculation script ------------
buffers <- read_csv("data/processed/buffers.csv") 


###########################################################
# PART 2: Summarize Datasets at Site Level ################
###########################################################

# Part 2A: summarize fish carcass mass and deployment data --------------------

carcass_summary <- 
  #Select relevant columns for summary
  inner_join(
    deployments[,c("site_name", "deployment_number", "carcass_id", "no_scavenge", "partial_scavenge", "camera_failure")], 
    carcass_data[,c("carcass_id", "mass_kg", "kg_consumed")],
    by=c("carcass_id")) %>% 
  #remove camera failure carcasses
  filter(camera_failure == FALSE) %>% 
  #remove the one instance when partially scavenged carcass was not weighed
  filter(!is.na(kg_consumed)) %>% 
  #calculate # and mass od fish deployed, removed, partially scavenged by site and deployment
  group_by(site_name) %>% 
  summarise(n_fish_deployed = n_distinct(carcass_id),
            n_removed = n_fish_deployed - (sum(partial_scavenge)+sum(no_scavenge)),
            n_partially_scavenged = sum(partial_scavenge),
            kg_deployed = sum(mass_kg),
            kg_consumed = sum(kg_consumed),
            proportion_removed = kg_consumed/kg_deployed)




# Part 2B: summarize scavenging assemblage by site -----------------------
scav_summary <- right_join(scav_data, 
                            deployments[,c("site_name", "deployment_number", "sd_number")], 
                            by=c("deployment_number", "sd_number"),
                            relationship = "many-to-many") %>% 
  pivot_longer(cols = deer_mouse:domestic_cat, #pivot longer
               names_to = "species",
               values_to = "count") %>%
  filter(!is.na(count)) %>% #Drop 0 values 
  #Select only scavengers / scavenging events
  filter(partial_scavenge == "TRUE" | 
           full_scavenge_carcass_removal == "TRUE") %>% 
  dplyr::select(site_name, deployment_number, species, count) %>% #Keep relevant columns
  group_by(site_name, species) %>% 
  #Calculate maximum number of confirmed individuals per species/site combination
  summarise(maxN = max(count), .groups = "drop") %>% 
  #Pivot wider for data export and community analyses
  pivot_wider(names_from = species, values_from = maxN, values_fill = 0)





# Part 2C: summarize human and domestic animal disturbance by site -------

#Summarize disturbance data
disturbance_summary <- inner_join(scav_data, 
                                  deployments[,c("site_name", "deployment_number", "sd_number")], 
                                  by=c("deployment_number", "sd_number"), 
                                  relationship = "many-to-many") %>% 
  #select relevant columns for summary
  dplyr::select(site_name, deployment_number, date_time, human_visitors, domestic_dog_visitors) %>% 

  
#First, group by both site name and deployment for summary
  
    group_by(site_name, deployment_number) %>% 
  summarise(

    #Calculate length of time from first to last video in each deployment using "difftime"
    deployment_length_days = as.numeric( #turn difftime value to numeric
      difftime(max(date_time),min(date_time), units = "days")),

    #Count number of recordings of humans, dogs, and cats per day
    human_visitors = sum(human_visitors, na.rm = TRUE),
    domestic_dog_visitors = sum(domestic_dog_visitors, na.rm = TRUE)) %>% 
  
#then ungroup deployment number to lump disturbance level from all deployments together
  ungroup(deployment_number) %>% 
  summarise(human_visitors_per_day = sum(human_visitors)/sum(deployment_length_days),
    domestic_dog_visitors_per_day = sum(domestic_dog_visitors)/sum(deployment_length_days),
    .groups = "drop")




# Part 2D: combine summaries into final site-level dataset used for analyses ------

urban_scavengers_summary <- left_join(buffers, carcass_summary, by="site_name") %>% 
  left_join(., scav_summary, by=c("site_name")) %>% 
  left_join(., disturbance_summary[,c("site_name","human_visitors_per_day", "domestic_dog_visitors_per_day")], by=c("site_name")) %>% 
  replace(is.na(.), 0) %>% #Replace NAs with 0s
  dplyr::select(site_name:percent_agricultural_5km, human_visitors_per_day, domestic_dog_visitors_per_day, n_fish_deployed:virginia_opossum) %>% 

# Part 2E: generate diversity metrics for each site ------
  
  mutate(richness = specnumber(.[18:29]),
         diversity = diversity(.[18:29]))


# Part 2E: Export as .csv in "processed data" folder --------------------------
write_csv(urban_scavengers_summary, "data/processed/urban_scavengers_summary.csv")


###########################################################
# PART 3: Summarize Datasets at Carcass Level #############
###########################################################

# Part 3A: Create temporary dataframes with columns for the date-times for when each carcass was deployed, scavenged for the first time, and completely scavenged. ------------------------

#Create temporary dataframe with a column for the date and time that the carcass was deployed
deploy_date_time <- scav_data %>% 
  filter(camera_trappers == "TRUE" & carcass_id != "NA") %>% 
  dplyr::select(carcass_id, date_time) %>% 
  rename(deployment_date_time = date_time) %>% 
  mutate(deployment_type_AM_PM = if_else(hour(deployment_date_time) > 12, "night", "day"))

#Create temporary dataframe with a column for the date and time that the first scavenging event took place
first_scavenger_date_time <- scav_data %>% 
  filter(partial_scavenge == "TRUE" | 
         full_scavenge_carcass_removal == "TRUE") %>% 
  group_by(carcass_id) %>% 
  summarise(first_scavenger_date_time = min(date_time))
  
#Create temporary dataframe with a column for the date and time that the carcass was fully scavenged
full_scavenge_date_time <- scav_data %>%
  filter(full_scavenge_carcass_removal == "TRUE") %>%
  group_by(carcass_id) %>% 
  summarise(full_scavenge_date_time = min(date_time))


# PART 3B: Bring temporary dataframes together and summarize scavenging efficiency metrics at the carcass level.

carcass_level_summary <- 
  #Select relevant columns for carcass-level summary
  inner_join(
    deployments[,c("site_name", "deployment_number", "carcass_id", "no_scavenge", "partial_scavenge", "camera_failure")], 
    carcass_data[,c("carcass_id", "mass_kg", "kg_consumed", "proportion_consumed")],
    by=c("carcass_id")) %>% 
  #remove camera failure carcasses
  filter(camera_failure == FALSE) %>% 
  #remove the one instance when partially scavenged carcass was not weighed
  filter(!is.na(kg_consumed)) %>% 
  

#combine with temporary dataframes created above
  left_join(., deploy_date_time, by=c("carcass_id")) %>% 
  left_join(., first_scavenger_date_time, by=c("carcass_id")) %>% 
  left_join(., full_scavenge_date_time, by=c("carcass_id")) %>% 
  
#calculate two metrics of scavenging efficiency: (1) time to first scavenging event (2) time until complete carcass scavenging
  
  mutate(
    #calculate time to first scavenging event
    hours_to_first_scavenging_event = 
      as.numeric(difftime(first_scavenger_date_time,
               deployment_date_time, units = c("hours"))),
    #calculate time until full scavenge
    hours_to_full_scavenge = as.numeric( #turn difftime value to numeric
      difftime(full_scavenge_date_time, deployment_date_time, units = c("hours")))) %>% 
  
#bring 1km urbanization level into dataframe
    left_join(., buffers[,c("site_name", "percent_developed_1km")], by = c("site_name")) %>% 
  dplyr::select(-deployment_number, -camera_failure)  #remove irrelevant columns


# Part 3B Export as .csv in "processed data" folder --------
write_csv(carcass_level_summary, "data/processed/carcass_level_summary.csv")


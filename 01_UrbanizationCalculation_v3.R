##########################################################################
# Santa Cruz Urban Scavengers Project ####################################
# Author: Frankie Gerraty (frankiegerraty@gmail.com; fgerraty@ucsc.edu) ##
##########################################################################
# Script 02: % Urbanization Calculations #################################
#-------------------------------------------------------------------------

# PART 1: Data Import ----------------------------------------------------
# Calculates % urbanization at multiple distances from study sites


# Import USGS National Land Cover Database (NLCD) 2019 (CONUS); originally downloaded from https://www.mrlc.gov/data/nlcd-2019-land-cover-conus
NLCD_2019_raw <- raster("data/raw/nlcd_2019_land_cover_l48_20210604/nlcd_2019_land_cover_l48_20210604.img")

# Import research sites 
sites_raw <- read_csv("data/raw/Urban_Scavenger_Sites.csv")

# Clean research sites data
sites <- sites_raw %>% 
  clean_names() %>% 
  rename(site_name = name)


# PART 2: Data Manipulation -----------------------------------------

#### NLCD Data Manipulation

# Crop NLCD dataset to the size of the study region 
# (minimizes computational demands of plotting and manipulation)
SantaCruzCounty_Crop <-c(-2310000, -2230000, 1840000 , 1910000)
NLCD_2019 <- crop(NLCD_2019_raw, SantaCruzCounty_Crop)


#### Sites Data Manipulation 

# Convert sites to sf object in WGS84 projection
sites_sf = st_as_sf(sites,coords=c("longitude","latitude"), crs=4326)

# Set coordinate reference system of "sites" to be the same as that of "NLCD_2019" for analyses
sites_sf <- st_transform(sites_sf, crs = proj4string(NLCD_2019))


# PART 3: Buffer Calculations ----------------------------------------

#### Extract raster pixel values within buffers

#1000m buffer extraction 
buffer_extract_1km <- extract(NLCD_2019, 
                              sites_sf, 
                              buffer = 1000) #sets 1000m buffer
#Summarize 1000m buffer extraction
summary_buffer_1km <- lapply(buffer_extract_1km, 
                             function(x){prop.table(table(x))})
# take a look to make sure correct execution
summary_buffer_1km


#3000m buffer extraction 
buffer_extract_3km <- extract(NLCD_2019, 
                              sites_sf, 
                              buffer = 3000) #sets 3000m buffer
#Summarize 3000m buffer extraction
summary_buffer_3km <- lapply(buffer_extract_3km, 
                             function(x){prop.table(table(x))})
#Take a look to make sure correct execution
summary_buffer_3km 



#5000m buffer extraction 
buffer_extract_5km <- extract(NLCD_2019, 
                              sites_sf, 
                              buffer = 5000) #sets 5000m buffer
#Summarize 3000m buffer extraction
summary_buffer_5km <- lapply(buffer_extract_5km, 
                             function(x){prop.table(table(x))})
#Take a look to make sure correct execution
summary_buffer_5km 



##### Extract the land class "number codes" from "buffer_extract_5km" to create a conversion between land class codes and development level (only 5km because 1km and 3km are encapsulated in 5km buffer)
NLCD_codes <- unique(unlist(buffer_extract_5km))

#Take a look
NLCD_codes

# Create conversion dataframe. Legend to the NLCD codes available here:
# https://www.mrlc.gov/data/legends/national-land-cover-database-class-legend-and-description
conversions <- data.frame(NLCD_class = NLCD_codes) %>% 
  mutate(class = case_when(
    NLCD_class == 11 ~ "Water", 
    NLCD_class > 20 & NLCD_class < 25 ~ "Developed", 
    NLCD_class > 30 & NLCD_class < 80 ~ "Undeveloped", 
    NLCD_class > 80 & NLCD_class < 83 ~ "Agricultural", 
    NLCD_class > 83 & NLCD_class < 96 ~ "Undeveloped",
  ))



#### Calculate urbanization level of 1km buffer ####

# convert to data frame
buffer_df_1km <- data.frame(site_name = rep(sites_sf$site_name, 
                                            lapply(summary_buffer_1km, length)),
                            NLCD_class = names(unlist(summary_buffer_1km)),
                            percent = unlist(summary_buffer_1km),
                            buffer_width = "1km") %>% 

# merge dataframe wirh land cover code conversions df
  merge(., conversions, by="NLCD_class") %>% 

## Calculate percent of land cover that is classified as developed
  filter(class != "Water") %>% # Remove "water" values
  # Combine all "developed" and "undeveloped" classes together
  group_by(site_name, class, buffer_width) %>% 
  #Calculate total percent cover of developed and undeveloped pixels, not accounting for water
  summarise(percent_cover = sum(percent)) %>% 
  # Calculate new percent cover values without "water" class, such that "percent_land_cover" is the percentage of land with developed/undeveloped classification within the buffer
  ungroup("class") %>% 
  mutate(percent_land_cover = percent_cover / sum(percent_cover)) %>% 
  filter(class != "Undeveloped")  #remove undeveloped classification

.groups = "drop"

#### Calculate urbanization level for 3km buffer ####

# Convert 3km buffer summary to dataframe
buffer_df_3km <- data.frame(site_name = rep(sites_sf$site_name, 
                                            lapply(summary_buffer_3km, length)),
                            NLCD_class = names(unlist(summary_buffer_3km)),
                            percent = unlist(summary_buffer_3km),
                            buffer_width = "3km") %>% 

# merge dataframe wirh land cover code conversions df
  merge(., conversions, by="NLCD_class") %>% 

# Calculate percent of land cover that is classified as developed
  filter(class != "Water") %>% # Remove "water" values
  # Combine all "developed" and "undeveloped" classes together
  group_by(site_name, class, buffer_width) %>% 
  #Calculate total percent cover of developed and undeveloped pixels, not accounting for water
  summarise(percent_cover = sum(percent)) %>% 
  # Calculate new percent cover values without "water" class, such that "percent_land_cover" is the percentage of land with developed/undeveloped classification within the buffer
  ungroup("class") %>% 
  mutate(percent_land_cover = percent_cover / sum(percent_cover)) %>% 
  filter(class != "Undeveloped")  #remove undeveloped classification

.groups = "drop"


#### Calculate urbanization level for 5km buffer ####

# Convert 5km buffer summary to dataframe
buffer_df_5km <- data.frame(site_name = rep(sites_sf$site_name, 
                                            lapply(summary_buffer_5km, length)),
                            NLCD_class = names(unlist(summary_buffer_5km)),
                            percent = unlist(summary_buffer_5km),
                            buffer_width = "5km") %>% 

# Create new dataframe column based on land cover code conversions
  merge(., conversions, by="NLCD_class") %>% 

# Calculate percent of land cover that is classified as developed
  filter(class != "Water") %>% # Remove "water" values
  # Combine all "developed" and "undeveloped" classes together
  group_by(site_name, class, buffer_width) %>% 
  #Calculate total percent cover of developed and undeveloped pixels, not accounting for water
  summarise(percent_cover = sum(percent)) %>% 
  # Calculate new percent cover values without "water" class, such that "percent_land_cover" is the percentage of land with developed/undeveloped classification within the buffer
  ungroup("class") %>% 
  mutate(percent_land_cover = percent_cover / sum(percent_cover)) %>% 
  filter(class != "Undeveloped")  #remove undeveloped classification

.groups = "drop"






# PART 4: Merge buffers into single dataframe ---------------------------------

buffer_longer <- rbind(buffer_df_1km, buffer_df_3km, buffer_df_5km) %>% 
  dplyr::select(-percent_cover) %>% 
  unite(class_buffer, c(class, buffer_width))

buffers <- left_join(sites, buffer_longer, by = "site_name") %>% 
  pivot_wider(names_from = class_buffer, values_from = percent_land_cover) %>% 
  rename(percent_developed_1km = "Developed_1km", 
         percent_developed_3km = "Developed_3km",
         percent_developed_5km = "Developed_5km",
         percent_agricultural_1km = "Agricultural_1km", 
         percent_agricultural_3km = "Agricultural_3km",
         percent_agricultural_5km = "Agricultural_5km") %>% 
#replace all NAs with 0s
  mutate_all(~replace(., is.na(.), 0)) %>% 
#reorder columns
  dplyr::select(site_name:percent_developed_1km, percent_developed_3km, percent_developed_5km, percent_agricultural_1km, percent_agricultural_3km, percent_agricultural_5km) 

#Export as .csv in "processed data" folder
write_csv(buffers, "data/processed/buffers.csv")


#PART 5: Plot buffers to double check that calculations executed correctly -----

plot_df <- buffer_longer %>% 
  mutate(type=if_else(class_buffer == "Developed_1km"|
                      class_buffer == "Developed_3km"|
                      class_buffer == "Developed_5km", "Development", "Agriculture"))



buffer_plot <- ggplot(data=plot_df, aes(x=site_name, y = percent_land_cover, fill=class_buffer))+
  geom_bar(stat = "identity", position = position_dodge())+
  facet_wrap(nrow=2, facets = "type")+
  labs(y = "% Land Cover Within Buffer Radius",
       x = "Site",
       fill = "Buffer Width")+
  theme_few()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(size = 12, colour = "black",face="bold"))+
  scale_fill_discrete(labels=c('1km', '3km', '5km', "1km", "3km", "5km"))

buffer_plot

#Export as PDF
pdf("output/extra_figures/buffers_plot.pdf", 
    width = 9, height = 6)

plot(buffer_plot)

dev.off()


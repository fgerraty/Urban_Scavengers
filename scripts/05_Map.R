##########################################################################
# Santa Cruz Urban Scavengers Project ####################################
# Author: Frankie Gerraty (frankiegerraty@gmail.com; fgerraty@ucsc.edu) ##
##########################################################################
# Script 05: Generate Map ################################################
#-------------------------------------------------------------------------

# Generate color scheme for the plot. Colors are associated with column "class"
color_palette <- c( "Developed" = "#FFA630", 
                    #"Agricultural" = "gray50", 
                    "Undeveloped" = "gray50", 
                    "Water" = "transparent")

#Create object with WGS84 projection
WGS84 <- "+proj=utm +zone=10 +datum=WGS84"

# Clean research sites data
sites <-  read_csv("data/raw/Urban_Scavenger_Sites.csv") %>% 
  clean_names() %>% 
  rename(site_name = name)

# Convert sites to sf object in WGS84 projection
sites_sf = st_as_sf(sites,coords=c("longitude","latitude"), crs=4326)


# Import USGS National Land Cover Database (NLCD) 2019 (CONUS); originally downloaded from https://www.mrlc.gov/data/nlcd-2019-land-cover-conus
NLCD_2019_raw <- raster("data/raw/nlcd_2019_land_cover_l48_20210604/nlcd_2019_land_cover_l48_20210604.img")

# Crop NLCD dataset to the size of the study region 
# (minimizes computational demands of plotting and manipulation)
SantaCruzCounty_Crop <-c(-2310000, -2230000, 1840000 , 1910000)
NLCD_2019 <- crop(NLCD_2019_raw, SantaCruzCounty_Crop)

#Project NLCD_2019 raster layer to WGS84 projection
NLCD_2019_WGS84 <- projectRaster(NLCD_2019, crs = WGS84, method = 'ngb')

#Convert NLCD_2019_Crop raster to dataframe 
NLCD_2019_WGS84_df <- as.data.frame(NLCD_2019_WGS84, xy = TRUE)

NLCD_2019_WGS84_df <- NLCD_2019_WGS84_df %>% 
  rename(Class = NLCD.Land.Cover.Class) %>% 
  mutate(Class = as.character(Class)) %>% 
  mutate(Class = dplyr::recode(Class, 
                               # Classify all levels of developed land as "developed"
                               "21" = "Developed",
                               "22" = "Developed",
                               "23" = "Developed",
                               "24" = "Developed",
                               #Classify all agricultural land as "undeveloped"
                               "81" = "Undeveloped",
                               "82" = "Undeveloped",
                               #Classify all other land as "undeveloped"
                               "31" = "Undeveloped",
                               "41" = "Undeveloped",
                               "42" = "Undeveloped",
                               "43" = "Undeveloped",
                               "51" = "Undeveloped",
                               "52" = "Undeveloped",
                               "71" = "Undeveloped",
                               "90" = "Undeveloped",
                               "95" = "Undeveloped",
                               #Classify "Unclassified" as "Open Water"
                               "11" = "Water",
                               "NA" = "Water",
                               "0" = "Water"
  ))


#Generate the base map  
base_map <- ggplot()+
  # Add raster data to map
  geom_raster(data = NLCD_2019_WGS84_df, aes(x=x, y=y, fill=Class))+
  # Color "class" according to color scheme "color_palette"
  scale_fill_manual(name = "Urbanization\nLevel", 
                    values = color_palette, 
                    na.value = "transparent",
                    limits = c('Developed', 'Undeveloped')
  ) +
  # Add study sites as points to the map
  geom_sf(data = sites_sf, size = 3)+
  #Set boundaries of the plot
  coord_sf(crs = WGS84,
           xlim = c(562000, 600000), 
           ylim = c(4088000 , 4110000), 
           expand = TRUE) +
  #Remove x and y axis labels
  labs ( x = element_blank(), 
         y = element_blank())+
  theme_classic()+
  theme(legend.position = c(0.1, 0.4),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=2))+
  # Add scale bar
  annotation_scale(location = "bl", width_hint = 0.2)+
  # Add north arrow
  annotation_north_arrow(
    location = "bl", which_north = "true",
    height = unit(1, "cm"), width = unit(1, "cm"),
    pad_y = unit(.75, "cm"),
    style = north_arrow_fancy_orienteering())


#Make locator map

states <- map_data("state")
ca_df <- subset(states, region == "california")

locator<- ggplot(data = ca_df, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) + 
  geom_polygon(color = "black", fill = "lightgrey")+
  geom_rect(    
    aes(xmin = -122.324507, 
        xmax = -121.880425, 
        ymin = 36.904612,
        ymax = 37.155623),
    fill = "transparent",
    colour = "red",
    linewidth = 1)+
  geom_segment(
    x = -119, y = 37.025,
    xend = -121.6 , yend = 37.025,
    lineend = "round",
    linejoin = "round",
    linewidth = 0.8, 
    arrow = arrow(length = unit(0.07, "inches")),
    colour = "red")+ # Also accepts "red", "blue' etc
  theme_void()


#Export maps as high-quality PDFs, which wiull later be combined using illustrator for the final publication-ready map figure
pdf("output/extra_figures/base_map.pdf", 
    width = 8, height = 5.5)

plot(base_map)

dev.off()


pdf("output/extra_figures/locator_map.pdf", 
    width = 5, height = 5)

plot(locator)

dev.off()

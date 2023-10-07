library(FedData)
library(magrittr)
library(terra)


crop <- st_bbox(c(xmin = -122.9, xmax = -121.4, ymax = 36.6, ymin = 37.5), crs = st_crs(4326)) %>% st_as_sfc()


nlcd_crop <- get_nlcd(
  template = crop,
  year = 2019,
  label = "studyarea"
)



# Convert sites to sf object in WGS84 projection
sites_sf = st_as_sf(sites,coords=c("longitude","latitude"), crs=4326)

# Set coordinate reference system of "sites" to be the same as that of "nlcd_crop" for analyses
sites_sf <- st_transform(sites_sf, crs = crs(nlcd_crop))

sites_spat <- terra::vect(sites_sf)

#1000m buffer extraction 
buffer_extract_1km <- terra::extract(nlcd_crop, 
                              sites_sf, 
                              buffer = 1000) #sets 1000m buffer
#Summarize 1000m buffer extraction
summary_buffer_1km <- lapply(buffer_extract_1km, 
                             function(x){prop.table(table(x))})
# take a look to make sure correct execution
summary_buffer_1km



#3000m buffer extraction 
buffer_extract_3km <- extract(nlcd_crop, 
                              sites_sf, 
                              buffer = 3000) #sets 3000m buffer
#Summarize 3000m buffer extraction
summary_buffer_3km <- lapply(buffer_extract_3km, 
                             function(x){prop.table(table(x))})
#Take a look to make sure correct execution
summary_buffer_3km 



#5000m buffer extraction 
buffer_extract_5km <- extract(nlcd_crop, 
                              sites_sf, 
                              buffer = 5000) #sets 5000m buffer
#Summarize 3000m buffer extraction
summary_buffer_5km <- lapply(buffer_extract_5km, 
                             function(x){prop.table(table(x))})
#Take a look to make sure correct execution
summary_buffer_5km 


values_list <- list()

for (i in 1:length(sites_spat)) {
  values_1km <- extract(nlcd_crop, buffers_1km[i, ])
  values_3km <- extract(nlcd_crop, buffers_3km[i, ])
  values_5km <- extract(nlcd_crop, buffers_5km[i, ])
  
  # Store the extracted values for each point in a list
  values_list[[i]] <- list(values_1km = values_1km, values_3km = values_3km, values_5km = values_5km)
}
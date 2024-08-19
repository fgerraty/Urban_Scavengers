##########################################################################
# Santa Cruz Urban Scavengers Project ####################################
# Author: Frankie Gerraty (frankiegerraty@gmail.com; fgerraty@ucsc.edu) ##
##########################################################################
# Script 08: Scavenger Activity Analysis and Partitioning Plots ##########
#-------------------------------------------------------------------------

####################################
# PART 1: Import Data ##############
####################################

deployments <- read_csv("data/raw/Urban_Scavenger_Carcass_Deployments.csv") %>% 
  clean_names()

scav_data <- read_csv("data/processed/scavenger_recordings.csv",
                                   col_types = "nnctnnlllnnnnnnnnnnnnlllT")

carcass_level_summary <- read_csv("data/processed/carcass_level_summary.csv") 

##########################################################################
# PART 2: Data Manipulation and Activity Plots (Figs 7, S2) ##############
##########################################################################

# PART 2A: Data Manipulation ---------------------------------------------------

#Create "activity df" to determine frequency captured scavenging events for each species during each 1-hour time period of the 24-hour day (i.e. 2-3AM) across all sites, deployments, and carcasses.
activity_df <- scav_data %>%
  pivot_longer(cols = deer_mouse:domestic_cat,
               names_to = "species",
               values_to = "count") %>%
  dplyr::select("time","species","count") %>%
  transform(species = str_replace(species, "number_","")) %>%
  transform(species = str_replace(species, "_scavenge","")) %>%
  transform(species = str_replace(species, "_scavenging","")) %>%
  filter(!is.na(count)) %>%
  mutate(time=hms(time)) %>%
  mutate(hour=hour(time)) %>%
  group_by(species, hour) %>%
  summarize(freq = n())


#Create temporary dataframe with all species/hour combinations with "freq", the frequency of recorded scavenging events
temp_df <- data.frame(species = rep(unique(activity_df$species), each = 24),
                      hour = rep(0:23, n_distinct(activity_df$species)),
                      freq = 0)

#Combine temporary dataframe with "activity_df" and filter to that there is a value (>= 0) for each species/hour combination
activity_df <- rbind(temp_df, activity_df)%>% #combine dataframes
  group_by(species, hour) %>% #group by species + hour
  mutate(freq = max(freq)) %>% #keep the maximum value for each species/hour combo
  distinct() #remove duplicates


# PART 2B: Plot Scavenger Activity for All Species (Figure S5) ------------------

#Set up polar coordinate system for plotting
cp <- coord_polar(start=0)
cp$is_free <- function() TRUE


# Plot with all species
activity_plot_all_spp <- ggplot(data = activity_df, 
                                aes(x = hour, y = freq)) +
  geom_bar(fill = "gray60", 
           stat = "identity",
           position = position_nudge (x=.5))+
  cp+
  facet_wrap("species", scales="free",
             labeller = as_labeller(c(american_crow='American Crow', 
                                      common_raven='Common Raven', 
                                      coyote='Coyote', 
                                      deer_mouse='Deer Mouse',
                                      domestic_cat='Domestic Cat', 
                                      domestic_dog='Domestic Dog', 
                                      gray_fox='Gray Fox', 
                                      rat='Rat',
                                      raccoon='Raccoon',
                                      striped_skunk='Striped Skunk', 
                                      virginia_opossum='Virginia Opossum', 
                                      western_gull='Western Gull')))+
  theme_bw()+
  theme(aspect.ratio = 1)+
  scale_x_continuous(limits = c(-.0001, 24), breaks = 0:23)+
  labs(x = "", 
       y = "No. Recorded Scavenging Events")+
  facetted_pos_scales(y=list(
                      species == "coyote" ~ scale_y_continuous(breaks = c(0, 1)),
                      species == "domestic_dog" ~ scale_y_continuous(breaks = c(0:2)),
                      species == "gray_fox" ~ scale_y_continuous(breaks = c(0, 1)),
                      species == "raccoon" ~ scale_y_continuous(breaks = c(0, 1)),
                      species == "striped_skunk" ~ scale_y_continuous(breaks = c(0, 1)),
                      species == "western_gull" ~ scale_y_continuous(breaks = c(0:2))))+
  theme(strip.background = element_blank(),
        strip.text = element_text(size=14),
        axis.title.y = element_text(size=16))+
  #Add vertical lines representing the mean time of day of carcass deployments in morning and evening
  geom_vline(xintercept = c(8.05, 18.58), linetype = "dashed", color = "grey30")

activity_plot_all_spp

#Export high-quality figure of activity plot (all species)

pdf("output/supp_figures/activity_plot_all_scavengers.pdf", 
    width = 10, height = 8)

plot(activity_plot_all_spp)

dev.off()


# PART 2C: Plot Scavenger Activity for Select Species (Figure 7) ----------------

#Generate color palette
scavenger_palette <-c("american_crow" = "#fc8d62", "common_raven" = "#66c2a5","deer_mouse" = "#8da0cb",  "coyote" = "#e78ac3")
#Strip labels for faceting
scavenger_labels <- c("deer_mouse" = "Deer Mouse", "common_raven" = "Common Raven", "american_crow" = "American Crow", "coyote" = "Coyote")
#Strip colors for faceting
strip <- strip_themed(background_x = elem_list_rect(fill = c("#fc8d62","#66c2a5","#e78ac3","#8da0cb")))



# Plot selected species together

activity_plot<- ggplot(data = filter(activity_df, species == "deer_mouse" | species == "american_crow" | species == "common_raven" | species == "coyote"), 
       aes(x = hour, y = freq, fill = species)) +
  geom_bar(stat = "identity",
           position = position_nudge (x=.5))+
  cp+
  theme_bw()+
  theme(aspect.ratio = 1)+
  facet_wrap2("species", nrow = 1, 
              scales="free", strip = strip, 
              labeller = as_labeller(scavenger_labels))+
  scale_x_continuous(limits = c(-.0001, 24), breaks = 0:23)+
  labs(x = "", 
       y = "No. Recorded \nScavenging Events", 
       fill = "")+
  scale_fill_manual(values = scavenger_palette)+
  theme(legend.position ="none",
        strip.text = element_text(size=14),
        axis.title.y = element_text(size=14))+
  #Add vertical lines representing the mean time of day of carcass deployments in morning and evening
  geom_vline(xintercept = c(8.05, 18.58), linetype = "dashed", color = "grey60")
activity_plot


#Export high-quality figure of activity analysis plot (selected species)

pdf("output/main_figures/activity_plot.pdf", 
    width = 9, height = 3)

plot(activity_plot)

dev.off()

##########################################################################
# PART 3: Create Scavenger Partitioning Plot (Figure 3) ##################
##########################################################################

# PART 3A : Data Manipulation --------------------------------------------------

#Modify scav_data dataframe to get AM/PM type
scav_data <- right_join(scav_data, carcass_level_summary[,c("carcass_id", "deployment_type_AM_PM")], by = "carcass_id")


#Create object with species column names
df_names = c("american_crow", 
             "common_raven", 
             "coyote", 
             "deer_mouse",
             "domestic_cat", 
             "domestic_dog", 
             "gray_fox", 
             "rat",
             "raccoon",
             "striped_skunk", 
             "virginia_opossum", 
             "western_gull")

#Create summary dataframe
scavengers_summary <- data.frame(common_name = c("American Crow", 
                                                 "Common Raven", 
                                                 "Coyote", 
                                                 "Deer Mouse",
                                                 "Domestic Cat", 
                                                 "Domestic Dog", 
                                                 "Gray Fox", 
                                                 "Rat",
                                                 "Raccoon",
                                                 "Striped Skunk", 
                                                 "Virginia Opossum", 
                                                 "Western Gull"),
                                 scientific_name = c("Corvus brachyrhynchos",
                                                     "Corvus corax",
                                                     "Canis latrans", 
                                                     "Peromyscus spp.",
                                                     "Felis catus",
                                                     "Canis lupus familiaris",
                                                     "Urocyon cinereoargenteus",
                                                     "Rattus spp.",
                                                     "Procyon lotor",
                                                     "Mephitis mephitis",
                                                     "Didelphis virginiana",
                                                     "Larus occidentalis"),
                                 invasive = c("", "", "", "", "Yes", "Yes", "", "Yes", "", "", "Yes", ""),
                                 #Run loop to calculate number of video recordings
                                 n_video_recordings = sapply(df_names, function(name) {
                                   filter(scav_data, !is.na(.data[[name]])) %>%
                                     count() %>%
                                     pull(n)
                                 }),
                                 #Run loop to calculate number of carcasses scavenged
                                 n_carcasses_detected = sapply(df_names, function(name) {
                                   filter(scav_data, !is.na(.data[[name]])) %>%
                                     dplyr::select(carcass_id) %>% 
                                     unique() %>% 
                                     count() %>%
                                     pull(n)
                                 })
                                 ,
                                 #Run loop to calculate number of carcasses removed
                                 n_carcasses_removed = sapply(df_names, function(name) {
                                   filter(scav_data, !is.na(.data[[name]]) & full_scavenge_carcass_removal == TRUE) %>%
                                     dplyr::select(carcass_id) %>% 
                                     unique() %>% 
                                     count() %>%
                                     pull(n)
                                 }),
                                 
                                 n_carcasses_detected_day = sapply(df_names, function(name) {
                                   filter(scav_data, !is.na(.data[[name]]) & deployment_type_AM_PM == "day") %>%
                                     dplyr::select(carcass_id) %>% 
                                     unique() %>% 
                                     count() %>%
                                     pull(n)
                                 }),
                                 n_carcasses_detected_night = sapply(df_names, function(name) {
                                   filter(scav_data, !is.na(.data[[name]]) & deployment_type_AM_PM == "night") %>%
                                     dplyr::select(carcass_id) %>% 
                                     unique() %>% 
                                     count() %>%
                                     pull(n)
                                 }),
                                 
                                 n_carcasses_removed_day = sapply(df_names, function(name) {
                                   filter(scav_data, !is.na(.data[[name]]) & full_scavenge_carcass_removal == TRUE & deployment_type_AM_PM == "day") %>%
                                     dplyr::select(carcass_id) %>% 
                                     unique() %>% 
                                     count() %>%
                                     pull(n)
                                 }),
                                 
                                 
                                 n_carcasses_removed_night = sapply(df_names, function(name) {
                                   filter(scav_data, !is.na(.data[[name]]) & full_scavenge_carcass_removal == TRUE & deployment_type_AM_PM == "night") %>%
                                     dplyr::select(carcass_id) %>% 
                                     unique() %>% 
                                     count() %>%
                                     pull(n)
                                 }), 
                                 n_sites = c(7, 11, 5, 8, 1, 6, 1, 3, 1, 1, 1, 2),
                                 sum_maxN = c(10, 15, 5, 15, 1,7,1, 3, 1, 1, 1, 3))



# PART 3B: Create Pie Charts (Fig 3A, 3D) ---------------------------------------

plot_df <- scavengers_summary %>% 
  dplyr::select(common_name, n_carcasses_detected, n_carcasses_removed, n_carcasses_detected_day, n_carcasses_detected_night, n_carcasses_removed_day, n_carcasses_removed_night) %>% 
  dplyr::filter(common_name %in% c("Common Raven",
                                   "American Crow",
                                   "Deer Mouse",
                                   "Rat",
                                   "Domestic Dog",
                                   "Coyote"))  

plot_df[nrow(plot_df) + 1,] <- c("Other", 15, 14, 3, 12, 3, 12)


plot_df <- data.frame(common_name = plot_df$common_name, 
                      sapply(plot_df[,2:7],as.numeric))

plot_df <- plot_df %>% 
  mutate(prop_detected = n_carcasses_detected/sum(plot_df$n_carcasses_detected) *100,
         prop_removed = n_carcasses_removed/sum(plot_df$n_carcasses_removed)*100,
         prop_detected_day = n_carcasses_detected_day/sum(plot_df$n_carcasses_detected_day) *100,
         prop_detected_night = n_carcasses_detected_night/sum(plot_df$n_carcasses_detected_night) *100,
         prop_removed_day = n_carcasses_removed_day/sum(plot_df$n_carcasses_removed_day) *100,
         prop_removed_night = n_carcasses_removed_night/sum(plot_df$n_carcasses_removed_night) *100
  )


#Manually reorder species for plotting from greatest to least scavenging
f <- factor(c("American Crow","Common Raven","Coyote","Deer Mouse", "Domestic Dog", "Other","Rat"), levels = c("Common Raven", "American Crow","Deer Mouse", "Rat", "Domestic Dog", "Coyote", "Other"))


#Create plot of partitioning of all scavenging activity 
detected_plot <- ggplot(plot_df, aes(x="", y=n_carcasses_detected, fill=fct_relevel(f))) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() + 
  scale_fill_manual(values = c("#66C2A5","#FC8D62", "#8DA0CB", "#A6D854","#FFD92F","#E78AC3","grey50"), name = "Scavenger Species")+
  theme(legend.position="none")



detected_plot

# PART 3C: Create plot of partitioning of carcass removal activity (Fig 3B,3C,3E,3F) ----------------------
removed_plot <- ggplot(plot_df, aes(x="", y=n_carcasses_removed, fill=fct_relevel(f))) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() + 
  scale_fill_manual(values = c("#66C2A5","#FC8D62", "#8DA0CB", "#A6D854","#FFD92F","#E78AC3","grey50"), name = "Scavenger Species")+
  theme(legend.position="none")

removed_plot



plot_df2 <- plot_df %>% 
  dplyr::select(1:7) %>% 
  pivot_longer(cols = 2:7, names_to = "facet", values_to = "count") %>% 
  filter(facet %in% c("n_carcasses_detected_day", "n_carcasses_detected_night", "n_carcasses_removed_day", "n_carcasses_removed_night")) %>% 
  arrange(factor(common_name, levels = c("Common Raven", "American Crow","Deer Mouse", "Rat", "Domestic Dog", "Coyote", "Other")))


facet_names <- list(
  "n_carcasses_detected_day"="Day",
  "n_carcasses_detected_night"="Night",
  'n_carcasses_removed_day'="Day",
  "n_carcasses_removed_night"="Night")


facet_labeller <- function(variable,value){
  return(facet_names[value])
}



waffle_plot <- ggplot(plot_df2, aes(fill = common_name, values = count))+
  geom_waffle(color = "white", size = .25, n_rows = 10, flip = TRUE, na.rm = TRUE)+
  facet_wrap(facets = "facet", nrow = 2, strip.position = "bottom", labeller=facet_labeller)  +
  scale_x_discrete() + 
  scale_y_continuous(labels = function(x) x * 10, # make this multiplyer the same as n_rows
                     expand = c(0,0))+
  coord_equal()+
  theme_minimal()+
  theme(panel.grid = element_blank(), axis.ticks.y = element_line())+
  scale_fill_manual(values = c("#66C2A5","#FC8D62", "#8DA0CB", "#A6D854","#FFD92F","#E78AC3","grey50"),
                    breaks = c("Common Raven", "American Crow","Deer Mouse", "Rat", 
                               "Domestic Dog", "Coyote", "Other"),
                    name = "Scavenger Species")+
  theme(legend.key.size = unit(1, 'cm'))+
  theme(panel.spacing.y=unit(3, "lines"))

waffle_plot


# PART 3D: Export Fig 3 Plots ------------------------------------------------------------

#Export high-quality figure of all plots for Figure 3, which are annotated in Illustrator

pdf("output/extra_figures/detection_pie.pdf", 
    width = 6, height = 5)

plot(detected_plot)

dev.off()

#Export high-quality figure of plots

pdf("output/extra_figures/removal_pie.pdf", 
    width = 6, height = 5)

plot(removed_plot)

dev.off()


#Export high-quality figure of plots

pdf("output/extra_figures/waffle.pdf", 
    width = 9.5, height = 4.5)

plot(waffle_plot)

dev.off()



##########################################################################
# PART 4: Export Scavenger Summary Table (Table S1) ######################
##########################################################################

# Data Cleaning 

scavengers_summary_temp <- scavengers_summary %>% 
  dplyr::select(common_name, scientific_name, invasive, n_video_recordings, n_carcasses_detected, n_carcasses_removed, n_sites, sum_maxN)


#convert to "gt" object for exporting publication-quality table
scavengers_summary_gt <- gt(scavengers_summary_temp)

scavengers_summary_1 <- 
  scavengers_summary_gt |>
  tab_header(
    title = "Vertebrate Scavenger Species, Invasive  Abundance Measures"
  ) |>
  cols_label(common_name = md("**Common Name**"),
             scientific_name = md("**Scientific Name**"),
             invasive = md("**Considered Invasive in Region?**"),
             n_video_recordings = md("**# Documented Scavenging Events**"),
             n_carcasses_detected = md("**# Carcasses Scavenged**"),
             n_carcasses_removed = md("**# Carcasses Removed**"),
             n_sites = md("**# Sites Recorded**"),
             sum_maxN = md("**Sum of MaxN for All Sites**")
  ) |>
  cols_width(common_name ~ px(100),
             scientific_name ~ px(200),
             invasive ~ px(100),
             n_video_recordings ~ px(125),
             n_carcasses_detected ~ px(100),
             n_carcasses_removed ~ px(100),
             n_sites ~ px(100) ,
             sum_maxN ~ px(100)) |>
  tab_style(
    style = list(
      cell_text(style = "italic")
    ),
    locations = cells_body(
      columns = scientific_name
    )
  )


final_summary <- rm_header(data = scavengers_summary_1)


# Show the gt Table
final_summary

#Export high-quality table
gtsave(final_summary, "output/supp_figures/scavenger_species_summary_table.png", vwidth = 900, vheight = 580)


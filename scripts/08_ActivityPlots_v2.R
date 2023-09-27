##########################################################################
# Santa Cruz Urban Scavengers Project ####################################
# Author: Frankie Gerraty (frankiegerraty@gmail.com; fgerraty@ucsc.edu) ##
##########################################################################
# Script 08: Scavenger Activity Analysis Plots ###########################
#-------------------------------------------------------------------------

# PART 1: Import Cleaned Datasets ----------------------------------------
deployments <- read_csv("data/processed/deployments.csv")
scav_data <- read_csv("data/processed/scavenger_recordings.csv")


# PART 2: Data Manipulation ----------------------------------------------

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


# PART 2: Plot Scavenger Activity for All Species -----------------------------

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
       y = "# Recorded Scavenging Events")+
  facetted_pos_scales(y=list(
                      species == "coyote" ~ scale_y_continuous(breaks = c(0, 1)),
                      species == "domestic_dog" ~ scale_y_continuous(breaks = c(0:2)),
                      species == "gray_fox" ~ scale_y_continuous(breaks = c(0, 1)),
                      species == "raccoon" ~ scale_y_continuous(breaks = c(0, 1)),
                      species == "striped_skunk" ~ scale_y_continuous(breaks = c(0, 1)),
                      species == "western_gull" ~ scale_y_continuous(breaks = c(0:2))))+
  theme(strip.background = element_blank(),
        strip.text = element_text(size=14),
        axis.title.y = element_text(size=16))

activity_plot_all_spp

#Export high-quality figure of activity plot (all species)

pdf("output/supp_figures/activity_plot_all_spp.pdf", 
    width = 10, height = 8)

plot(activity_plot_all_spp)

dev.off()


# PART 2: Plot Scavenger Activity for Select Species --------------------------

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
       y = "# Recorded \nScavenging Events", 
       fill = "")+
  scale_fill_manual(values = scavenger_palette)+
  theme(legend.position ="none",
        strip.text = element_text(size=14),
        axis.title.y = element_text(size=14))
activity_plot


#Export high-quality figure of activity analysis plot (selected species)

pdf("output/main_figures/activity_plot.pdf", 
    width = 9, height = 3)

plot(activity_plot)

dev.off()

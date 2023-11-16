##########################################################################
# Santa Cruz Urban Scavengers Project ####################################
# Author: Frankie Gerraty (frankiegerraty@gmail.com; fgerraty@ucsc.edu) ##
##########################################################################
# Script 04: Scavenging Assemblage Visualization NMDS ####################
#-------------------------------------------------------------------------

#Load dataset
urban_scavengers_summary <- read_csv("data/processed/urban_scavengers_summary.csv") %>% 
  filter(site_name != "Strawberry")

#set seed for reproducability
set.seed(99)

# PART 1: Set up datasets for use in vegan for NMDS ----------------------

#sites
sites <- urban_scavengers_summary[,1]

#scav_assemblage
scav_assemblage <- urban_scavengers_summary[,18:29]


nMDS <- metaMDS(scav_assemblage, k=2, trymax = 1000, maxit = 10000)
#Check stress (less than 0.1 is great)
nMDS$stress

#Extract coordinates of nMDS points
nMDS_coords <- nMDS$points
#combine nMDS coordinates with site name
nMDS_coords <- cbind(sites, nMDS_coords)

#Add site name, percent_developed_1km, and human_visitors_per_day (the best predictor from PERMANOVA analysis) to nMDS_coords dataframe
nMDS_coords <- left_join(nMDS_coords, urban_scavengers_summary[,c(1,4)]) %>% 
  mutate(percent_urbanized_1km=percent_developed_1km*100)



#create environmental fit df to test against
envirofit_df <- cbind(scav_assemblage, percent_urbanized_1km = urban_scavengers_summary$percent_developed_1km*100, human_visitation = urban_scavengers_summary$human_visitors_per_day)


#Determine species driving divergence in ordination space
envirofit <- envfit(nMDS, #metaMDS output
                    envirofit_df, #dataframe to test against (Can include enviro variables)
             permutations = 999, na.rm = TRUE)
#take a look at output
envirofit

#extract coordinates from envirofit
envirofit_coords = as.data.frame(scores(envirofit, "vectors"))
envirofit_coords <- cbind(envirofit_coords, species = rownames(envirofit_coords)) %>% 
  filter(species == "american_crow"|
           species == "deer_mouse"|
           species == "percent_urbanized_1km" |
           species == "human_visitation") 

envirofit_coords <- envirofit_coords %>% 
  mutate(name = c("American Crow **", "Deer Mouse ***", 
                  "Percent Urbanized (1km) ***", "Human Visitation *"),
         pal = c("grey", "grey", "black", "black"),
         y_shift = c(0, .06, -.05, -.05), 
         x_shift = c(.25, 0, 0.1, -.1))


# Plot 1: Continuous urbanization gradient NMDS plot --------------------------

pal <- wes_palette("Zissou1", 100, type = "continuous")

# GGPlot
plot1 <- ggplot(data=nMDS_coords, aes(x=MDS1, y=MDS2, color = percent_urbanized_1km))+
  geom_point(size=6)+ 
  scale_color_gradientn(colors = wes_palette("Zissou1", type = "continuous"), 
                        name="Percent \nUrbanized \n(1km)")+
  geom_segment(data = envirofit_coords,
               aes(x = 0, y = 0, 
                   xend = NMDS1, yend = NMDS2), 
               linewidth =1, colour = envirofit_coords$pal,
               arrow = arrow(length = unit(0.25, "cm"))) +
  theme_classic()+
  labs(x = "NMDS1",
       y = "NMDS2")+
  geom_text(data = envirofit_coords, 
            aes(x = NMDS1+x_shift, y = NMDS2+y_shift, label = name),
            size = 4, inherit.aes = FALSE)+
  theme(strip.background = element_blank(),
        strip.text.x = element_blank(),
        axis.line.y = element_blank(), axis.line.x = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        legend.position=c(.07,.815),
        legend.background = element_rect(linewidth=.75, colour = "black"))+
  ylim(c(-1.1, 1.1))

plot1

#Export high-quality figure of nMDS plot

pdf("output/main_figures/NMDS_plot.pdf", 
    width = 8, height = 6)

plot(plot1)

dev.off()

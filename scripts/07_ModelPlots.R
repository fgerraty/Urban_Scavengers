##########################################################################
# Santa Cruz Urban Scavengers Project ####################################
# Author: Frankie Gerraty (frankiegerraty@gmail.com; fgerraty@ucsc.edu) ##
##########################################################################
# Script 07: Model Plots #################################################
#-------------------------------------------------------------------------

####################################
# PART 1: Import Data ##############
####################################

#Import .csvs from "processed data" folder ------------------------------------

urban_scavengers_summary <- read_csv("data/processed/urban_scavengers_summary.csv") 

carcass_level_summary <- read_csv("data/processed/carcass_level_summary.csv") %>%    
  #Create new column "scav_prob" which classifies scavenging as 1/0 for each carcass (partially scavenged carcasses are classified as scavenged = 1)
  mutate(scav_prob = if_else(no_scavenge == TRUE, 0, 1),
         #Create new column "full_scav_prob" which classifies scavenging as 1/0 for each carcass that was fully scavenged AKA carcass removal)
         full_scav_prob = if_else(no_scavenge == FALSE & 
                                  partial_scavenge == FALSE, 1, 0),
         site_name = as.factor(site_name)) %>% 
  #bring environmental variables for univariate plots into DF
  left_join(., urban_scavengers_summary[,c("site_name", "percent_developed_1km",  "domestic_dog_visitors_per_day")], by = c("site_name"))


############################################################################
# PART 2: Urbanization Effects on Select Scavenger Species #################
############################################################################

# Part 2A: Prepare Plot Aesthestics ---------------------------------------------

#Generate color palette
scavenger_palette <-c("american_crow" = "#fc8d62", "common_raven" = "#66c2a5","deer_mouse" = "#8da0cb",  "coyote" = "#e78ac3")
#Strip labels for faceting
scavenger_labels <- c("deer_mouse" = "Deer Mouse ***", "common_raven" = "Common Raven", "american_crow" = "American Crow **", "coyote" = "Coyote")
#Strip colors for faceting
american_crow_strip <- strip_themed(background_x = elem_list_rect(fill = c("#fc8d62")))
common_raven_strip <- strip_themed(background_x = elem_list_rect(fill = c("#66c2a5")))
deer_mouse_strip <- strip_themed(background_x = elem_list_rect(fill = c("#8da0cb")))
coyote_strip <- strip_themed(background_x = elem_list_rect(fill = c("#e78ac3")))


plot_df <- urban_scavengers_summary %>% 
  pivot_longer(cols = american_crow:virginia_opossum,
               names_to = "species", 
               values_to = "maxN") %>% 
  mutate(percent_developed_1km = percent_developed_1km)



# Part 2B: American Crow Plot -------------------------------------------------
#Generate American Crow model and use emmeans() function for generating model line and confidence intervals

american_crow_mod=glm.nb(american_crow ~ percent_developed_1km +
                           offset(log(n_fish_deployed)),
                         data=urban_scavengers_summary);summary(american_crow_mod)

american_crow_mod_emm <- emmeans(american_crow_mod, ~percent_developed_1km,
                                 at = list(percent_developed_1km = seq(0,1, length.out = 101)), 
                                 type = "response", offset = log(1))

american_crow_mod_df <- as.data.frame(american_crow_mod_emm) %>% 
  mutate(species = "american_crow")

#Generate American Crow Plot
american_crow_plot <- ggplot(data=american_crow_mod_df,
                             aes(x=percent_developed_1km,
                                 y=response,
                                 color = species))+
  facet_wrap2("species", strip = american_crow_strip, 
              labeller = as_labeller(scavenger_labels))+
  geom_line() +
  geom_ribbon(aes(ymin = asymp.LCL, ymax = asymp.UCL), 
              colour = NA, alpha = 0.2) +
  geom_point(data = filter(plot_df, species == "american_crow"), 
             aes(x= percent_developed_1km, y=maxN/n_fish_deployed), 
             alpha = .5, size = 3)+
  scale_color_manual(values = scavenger_palette, guide = "none")+
  theme_few()+
  labs(element_blank())+
  theme(axis.title=element_blank())+
  scale_x_continuous(breaks = c(0, 0.25, 0.50, 0.75, 1), labels = c(0,25,50,75,100))


# Part 2C: Deer Mouse Plot -------------------------------------------------
#Generate Deer Mouse model and use emmeans() function for generating model line and confidence intervals

deer_mouse_mod=glm.nb(deer_mouse ~ percent_developed_1km +
                        offset(log(n_fish_deployed)),
                      data=urban_scavengers_summary);summary(deer_mouse_mod)

deer_mouse_mod_emm <- emmeans(deer_mouse_mod, ~percent_developed_1km,
                              at = list(percent_developed_1km = seq(0,1, length.out = 101)), 
                              type = "response", offset = log(1))

deer_mouse_mod_df <- as.data.frame(deer_mouse_mod_emm) %>% 
  mutate(species = "deer_mouse")

#Generate Deer Mouse Plot
deer_mouse_plot <- ggplot(data=deer_mouse_mod_df,
                          aes(x=percent_developed_1km,
                              y=response,
                              color = species))+
  facet_wrap2("species", strip = deer_mouse_strip, 
              labeller = as_labeller(scavenger_labels))+
  geom_line() +
  geom_ribbon(aes(ymin = asymp.LCL, ymax = asymp.UCL), 
              colour = NA, alpha = 0.2) +
  geom_point(data = filter(plot_df, species == "deer_mouse"), 
             aes(x= percent_developed_1km, y=maxN/n_fish_deployed), 
             alpha = .5, size = 3)+
  xlim(c(0,1))+
  scale_color_manual(values = scavenger_palette, guide = "none")+
  theme_few()+
  labs(element_blank())+
  theme(axis.title=element_blank())+
  scale_x_continuous(breaks = c(0, 0.25, 0.50, 0.75, 1), labels = c(0,25,50,75,100))

# Part 2D: Common Raven Plot -------------------------------------------------
#Generate Common Raven model and use emmeans() function for generating model line and confidence intervals

common_raven_mod=glm.nb(common_raven ~ percent_developed_1km +
                          offset(log(n_fish_deployed)),
                        data=urban_scavengers_summary);summary(common_raven_mod)

common_raven_mod_emm <- emmeans(common_raven_mod, ~percent_developed_1km,
                                at = list(percent_developed_1km = seq(0,1, length.out = 101)), 
                                type = "response", offset = log(1))

common_raven_mod_df <- as.data.frame(common_raven_mod_emm) %>% 
  mutate(species = "common_raven")

#Generate Common Raven Plot
common_raven_plot <- ggplot(data=common_raven_mod_df,
                            aes(x=percent_developed_1km,
                                y=response,
                                color = species))+
  facet_wrap2("species", strip = common_raven_strip, 
              labeller = as_labeller(scavenger_labels))+
  geom_line() +
  geom_ribbon(aes(ymin = asymp.LCL, ymax = asymp.UCL), 
              colour = NA, alpha = 0.2) +
  geom_point(data = filter(plot_df, species == "common_raven"), 
             aes(x= percent_developed_1km, y=maxN/n_fish_deployed), 
             alpha = .5, size = 3)+
  xlim(c(0,1))+
  scale_color_manual(values = scavenger_palette, guide = "none")+
  theme_few()+
  labs(element_blank())+
  theme(axis.title=element_blank())+
  scale_x_continuous(breaks = c(0, 0.25, 0.50, 0.75, 1), labels = c(0,25,50,75,100))

# Part 2E: Coyote Plot -------------------------------------------------
#Generate Coyote model and use emmeans() function for generating model line and confidence intervals

coyote_mod=glm.nb(coyote ~ percent_developed_1km +
                 offset(log(n_fish_deployed)),
               data=urban_scavengers_summary);summary(coyote_mod)

coyote_mod_emm <- emmeans(coyote_mod, ~percent_developed_1km,
                       at = list(percent_developed_1km = seq(0,1, length.out = 101)), 
                       type = "response", offset = log(1))

coyote_mod_df <- as.data.frame(coyote_mod_emm) %>% 
  mutate(species = "coyote")

#Generate Coyote Plot
coyote_plot <- ggplot(data=coyote_mod_df,
                   aes(x=percent_developed_1km,
                       y=response,
                       color = species))+
  facet_wrap2("species", strip = coyote_strip, 
              labeller = as_labeller(scavenger_labels))+
  geom_line() +
  geom_ribbon(aes(ymin = asymp.LCL, ymax = asymp.UCL), 
              colour = NA, alpha = 0.2) +
  geom_point(data = filter(plot_df, species == "coyote"), 
             aes(x= percent_developed_1km, y=maxN/n_fish_deployed), 
             alpha = .5, size = 3)+
  xlim(c(0,1))+
  scale_color_manual(values = scavenger_palette, guide = "none")+
  theme_few()+
  labs(element_blank())+
  theme(axis.title=element_blank())+
  scale_x_continuous(breaks = c(0, 0.25, 0.50, 0.75, 1), labels = c(0,25,50,75,100))

# Part 2F: Combine Plots -------------------------------------------------
combined_plot <- plot_grid(american_crow_plot, deer_mouse_plot, common_raven_plot, coyote_plot)+
  draw_label("Percent Urbanized (1km)", x=0.5, y=0, 
             angle= 0) +
  draw_label("Scavenger Abundance \n(MaxN / n carcasses deployed)", x=0, y=0.5, 
             vjust= 0, 
             angle=90)+
  theme(plot.margin = unit(c(0.5,0.5,0.5,1), "cm"))


combined_plot


#Export high-quality figure of combined plot

pdf("output/extra_figures/urbanization_select_scavengers_glm.pdf", 
    width = 8, height = 6)

plot(combined_plot)

dev.off()


############################################################################
# PART 3: Urbanization Effects on All Scavenger Species ####################
############################################################################

# Part 3A: American Crow Plot -------------------------------------------------
#Generate American Crow model and use emmeans() function for generating model line and confidence intervals

american_crow_mod=glm.nb(american_crow ~ percent_developed_1km +
                           offset(log(n_fish_deployed)),
                         data=urban_scavengers_summary);summary(american_crow_mod)

american_crow_mod_emm <- emmeans(american_crow_mod, ~percent_developed_1km,
                                 at = list(percent_developed_1km = seq(0,1, length.out = 101)), 
                                 type = "response", offset = log(1))

american_crow_mod_df <- as.data.frame(american_crow_mod_emm) %>% 
  mutate(species = "american_crow")

#Generate American Crow Plot
american_crow_plot_2 <- ggplot(data=american_crow_mod_df,
                               aes(x=percent_developed_1km,
                                   y=response))+
  facet_wrap("species", 
             labeller = as_labeller(c(american_crow='American Crow')))+
  geom_line() +
  geom_ribbon(aes(ymin = asymp.LCL, ymax = asymp.UCL), 
              colour = NA, alpha = 0.2) +
  geom_point(data = filter(plot_df, species == "american_crow"), 
             aes(x= percent_developed_1km, y=maxN/n_fish_deployed), 
             alpha = .5, size = 3)+
  theme_few()+
  labs(element_blank())+
  theme(axis.title=element_blank())+
  scale_x_continuous(breaks = c(0, 0.25, 0.50, 0.75, 1), labels = c(0,25,50,75,100))


# Part 3B: Common Raven Plot -------------------------------------------------
# Generate Common Raven model and use emmeans() function for generating model line and confidence intervals

common_raven_mod=glm.nb(common_raven ~ percent_developed_1km +
                          offset(log(n_fish_deployed)),
                        data=urban_scavengers_summary);summary(common_raven_mod)

common_raven_mod_emm <- emmeans(common_raven_mod, ~percent_developed_1km,
                                at = list(percent_developed_1km = seq(0,1, length.out = 101)), 
                                type = "response", offset = log(1))

common_raven_mod_df <- as.data.frame(common_raven_mod_emm) %>% 
  mutate(species = "common_raven")

#Generate Common Raven Plot
common_raven_plot_2 <- ggplot(data=common_raven_mod_df,
                              aes(x=percent_developed_1km,
                                  y=response))+
  facet_wrap("species", 
             labeller = as_labeller(c(common_raven='Common Raven')))+
  geom_line() +
  geom_ribbon(aes(ymin = asymp.LCL, ymax = asymp.UCL), 
              colour = NA, alpha = 0.2) +
  geom_point(data = filter(plot_df, species == "common_raven"), 
             aes(x= percent_developed_1km, y=maxN/n_fish_deployed), 
             alpha = .5, size = 3)+
  theme_few()+
  labs(element_blank())+
  theme(axis.title=element_blank())+
  scale_x_continuous(breaks = c(0, 0.25, 0.50, 0.75, 1), labels = c(0,25,50,75,100))

# Part 3C: Coyote Plot -------------------------------------------------
#Generate Coyote model and use emmeans() function for generating model line and confidence intervals

coyote_mod=glm.nb(coyote ~ percent_developed_1km +
                    offset(log(n_fish_deployed)),
                  data=urban_scavengers_summary);summary(coyote_mod)

coyote_mod_emm <- emmeans(coyote_mod, ~percent_developed_1km,
                          at = list(percent_developed_1km = seq(0,1, length.out = 101)), 
                          type = "response", offset = log(1))

coyote_mod_df <- as.data.frame(coyote_mod_emm) %>% 
  mutate(species = "coyote")

#Generate Coyote Plot
coyote_plot_2 <- ggplot(data=coyote_mod_df,
                        aes(x=percent_developed_1km,
                            y=response))+
  facet_wrap("species", 
             labeller = as_labeller(c(coyote='Coyote')))+
  geom_line() +
  geom_ribbon(aes(ymin = asymp.LCL, ymax = asymp.UCL), 
              colour = NA, alpha = 0.2) +
  geom_point(data = filter(plot_df, species == "coyote"), 
             aes(x= percent_developed_1km, y=maxN/n_fish_deployed), 
             alpha = .5, size = 3)+
  theme_few()+
  labs(element_blank())+
  theme(axis.title=element_blank())+
  scale_x_continuous(breaks = c(0, 0.25, 0.50, 0.75, 1), labels = c(0,25,50,75,100))


# Part 3D: Deer Mouse Plot -------------------------------------------------
#Generate Deer Mouse model and use emmeans() function for generating model line and confidence intervals

deer_mouse_mod=glm.nb(deer_mouse ~ percent_developed_1km +
                        offset(log(n_fish_deployed)),
                      data=urban_scavengers_summary);summary(deer_mouse_mod)

deer_mouse_mod_emm <- emmeans(deer_mouse_mod, ~percent_developed_1km,
                              at = list(percent_developed_1km = seq(0,1, length.out = 101)), 
                              type = "response", offset = log(1))

deer_mouse_mod_df <- as.data.frame(deer_mouse_mod_emm) %>% 
  mutate(species = "deer_mouse")

#Generate Deer Mouse Plot
deer_mouse_plot_2 <- ggplot(data=deer_mouse_mod_df,
                            aes(x=percent_developed_1km,
                                y=response))+
  facet_wrap("species", 
             labeller = as_labeller(c(deer_mouse='Deer Mouse')))+
  geom_line() +
  geom_ribbon(aes(ymin = asymp.LCL, ymax = asymp.UCL), 
              colour = NA, alpha = 0.2) +
  geom_point(data = filter(plot_df, species == "deer_mouse"), 
             aes(x= percent_developed_1km, y=maxN/n_fish_deployed), 
             alpha = .5, size = 3)+
  theme_few()+
  labs(element_blank())+
  theme(axis.title=element_blank())+
  scale_x_continuous(breaks = c(0, 0.25, 0.50, 0.75, 1), labels = c(0,25,50,75,100))

# Part 3E: Domestic Cat Plot -------------------------------------------------
#Generate Domestic Cat model and use emmeans() function for generating model line and confidence intervals

domestic_cat_mod=glm.nb(domestic_cat ~ percent_developed_1km +
                          offset(log(n_fish_deployed)),
                        data=urban_scavengers_summary);summary(domestic_cat_mod)

domestic_cat_mod_emm <- emmeans(domestic_cat_mod, ~percent_developed_1km,
                                at = list(percent_developed_1km = seq(0,1, length.out = 101)), 
                                type = "response", offset = log(1))

domestic_cat_mod_df <- as.data.frame(domestic_cat_mod_emm) %>% 
  mutate(species = "domestic_cat")

#Generate Domestic Cat Plot
domestic_cat_plot_2 <- ggplot(data=domestic_cat_mod_df,
                              aes(x=percent_developed_1km,
                                  y=response))+
  facet_wrap("species", 
             labeller = as_labeller(c(domestic_cat='Domestic Cat')))+
  geom_line() +
  geom_ribbon(aes(ymin = asymp.LCL, ymax = asymp.UCL), 
              colour = NA, alpha = 0.2) +
  geom_point(data = filter(plot_df, species == "domestic_cat"), 
             aes(x= percent_developed_1km, y=maxN/n_fish_deployed), 
             alpha = .5, size = 3)+
  theme_few()+
  labs(element_blank())+
  theme(axis.title=element_blank())+
  scale_x_continuous(breaks = c(0, 0.25, 0.50, 0.75, 1), labels = c(0,25,50,75,100))+
  coord_cartesian(ylim = c(0,.15))


# Part 3F: Domestic Dog Plot -------------------------------------------------
#Generate Domestic Dog model and use emmeans() function for generating model line and confidence intervals

domestic_dog_mod=glm.nb(domestic_dog ~ percent_developed_1km +
                          offset(log(n_fish_deployed)),
                        data=urban_scavengers_summary);summary(domestic_dog_mod)

domestic_dog_mod_emm <- emmeans(domestic_dog_mod, ~percent_developed_1km,
                                at = list(percent_developed_1km = seq(0,1, length.out = 101)), 
                                type = "response", offset = log(1))

domestic_dog_mod_df <- as.data.frame(domestic_dog_mod_emm) %>% 
  mutate(species = "domestic_dog")

#Generate Domestic Dog Plot
domestic_dog_plot_2 <- ggplot(data=domestic_dog_mod_df,
                              aes(x=percent_developed_1km,
                                  y=response))+
  facet_wrap("species", 
             labeller = as_labeller(c(domestic_dog='Domestic Dog')))+
  geom_line() +
  geom_ribbon(aes(ymin = asymp.LCL, ymax = asymp.UCL), 
              colour = NA, alpha = 0.2) +
  geom_point(data = filter(plot_df, species == "domestic_dog"), 
             aes(x= percent_developed_1km, y=maxN/n_fish_deployed), 
             alpha = .5, size = 3)+
  theme_few()+
  labs(element_blank())+
  theme(axis.title=element_blank())+
  scale_x_continuous(breaks = c(0, 0.25, 0.50, 0.75, 1), labels = c(0,25,50,75,100))


# Part 3G: Gray Fox Plot -------------------------------------------------
#Generate Gray Fox model and use emmeans() function for generating model line and confidence intervals

gray_fox_mod=glm.nb(gray_fox ~ percent_developed_1km +
                      offset(log(n_fish_deployed)),
                    data=urban_scavengers_summary);summary(gray_fox_mod)

gray_fox_mod_emm <- emmeans(gray_fox_mod, ~percent_developed_1km,
                            at = list(percent_developed_1km = seq(0,1, length.out = 101)), 
                            type = "response", offset = log(1))

gray_fox_mod_df <- as.data.frame(gray_fox_mod_emm) %>% 
  mutate(species = "gray_fox")

#Generate Gray Fox Plot
gray_fox_plot_2 <- ggplot(data=gray_fox_mod_df,
                          aes(x=percent_developed_1km,
                              y=response))+
  facet_wrap("species", 
             labeller = as_labeller(c(gray_fox='Gray Fox')))+
  geom_line() +
  geom_ribbon(aes(ymin = asymp.LCL, ymax = asymp.UCL), 
              colour = NA, alpha = 0.2) +
  geom_point(data = filter(plot_df, species == "gray_fox"), 
             aes(x= percent_developed_1km, y=maxN/n_fish_deployed), 
             alpha = .5, size = 3)+
  theme_few()+
  labs(element_blank())+
  theme(axis.title=element_blank())+
  scale_x_continuous(breaks = c(0, 0.25, 0.50, 0.75, 1), labels = c(0,25,50,75,100))+
  coord_cartesian(ylim = c(0,.15))

# Part 3H: Raccoon Plot -------------------------------------------------
#Generate Raccoon model and use emmeans() function for generating model line and confidence intervals

raccoon_mod=glm.nb(raccoon ~ percent_developed_1km +
                     offset(log(n_fish_deployed)),
                   data=urban_scavengers_summary);summary(raccoon_mod)

raccoon_mod_emm <- emmeans(raccoon_mod, ~percent_developed_1km,
                           at = list(percent_developed_1km = seq(0,1, length.out = 101)), 
                           type = "response", offset = log(1))

raccoon_mod_df <- as.data.frame(raccoon_mod_emm) %>% 
  mutate(species = "raccoon")

#Generate Raccoon Plot
raccoon_plot_2 <- ggplot(data=raccoon_mod_df,
                         aes(x=percent_developed_1km,
                             y=response))+
  facet_wrap("species", 
             labeller = as_labeller(c(raccoon='Raccoon')))+
  geom_line() +
  geom_ribbon(aes(ymin = asymp.LCL, ymax = asymp.UCL), 
              colour = NA, alpha = 0.2) +
  geom_point(data = filter(plot_df, species == "raccoon"), 
             aes(x= percent_developed_1km, y=maxN/n_fish_deployed), 
             alpha = .5, size = 3)+
  theme_few()+
  labs(element_blank())+
  theme(axis.title=element_blank())+
  scale_x_continuous(breaks = c(0, 0.25, 0.50, 0.75, 1), labels = c(0,25,50,75,100))+
  coord_cartesian(ylim = c(0,.15))

# Part 3I: Rat Plot -------------------------------------------------
#Generate Rat model and use emmeans() function for generating model line and confidence intervals

rat_mod=glm.nb(rat ~ percent_developed_1km +
                 offset(log(n_fish_deployed)),
               data=urban_scavengers_summary);summary(rat_mod)

rat_mod_emm <- emmeans(rat_mod, ~percent_developed_1km,
                       at = list(percent_developed_1km = seq(0,1, length.out = 101)), 
                       type = "response", offset = log(1))

rat_mod_df <- as.data.frame(rat_mod_emm) %>% 
  mutate(species = "rat")

#Generate Rat Plot
rat_plot_2 <- ggplot(data=rat_mod_df,
                     aes(x=percent_developed_1km,
                         y=response))+
  facet_wrap("species", 
             labeller = as_labeller(c(rat='Rat')))+
  geom_line() +
  geom_ribbon(aes(ymin = asymp.LCL, ymax = asymp.UCL), 
              colour = NA, alpha = 0.2) +
  geom_point(data = filter(plot_df, species == "rat"), 
             aes(x= percent_developed_1km, y=maxN/n_fish_deployed), 
             alpha = .5, size = 3)+
  theme_few()+
  labs(element_blank())+
  theme(axis.title=element_blank())+
  scale_x_continuous(breaks = c(0, 0.25, 0.50, 0.75, 1), labels = c(0,25,50,75,100))+
  coord_cartesian(ylim = c(0,.15))

# Part 3J: Striped Skunk Plot -------------------------------------------------
#Generate Striped Skunk model and use emmeans() function for generating model line and confidence intervals

striped_skunk_mod=glm.nb(striped_skunk ~ percent_developed_1km +
                           offset(log(n_fish_deployed)),
                         data=urban_scavengers_summary);summary(striped_skunk_mod)

striped_skunk_mod_emm <- emmeans(striped_skunk_mod, ~percent_developed_1km,
                                 at = list(percent_developed_1km = seq(0,1, length.out = 101)), 
                                 type = "response", offset = log(1))

striped_skunk_mod_df <- as.data.frame(striped_skunk_mod_emm) %>% 
  mutate(species = "striped_skunk")

#Generate Striped Skunk Plot
striped_skunk_plot_2 <- ggplot(data=striped_skunk_mod_df,
                               aes(x=percent_developed_1km,
                                   y=response))+
  facet_wrap("species", 
             labeller = as_labeller(c(striped_skunk='Striped Skunk')))+
  geom_line() +
  geom_ribbon(aes(ymin = asymp.LCL, ymax = asymp.UCL), 
              colour = NA, alpha = 0.2) +
  geom_point(data = filter(plot_df, species == "striped_skunk"), 
             aes(x= percent_developed_1km, y=maxN/n_fish_deployed), 
             alpha = .5, size = 3)+
  theme_few()+
  labs(element_blank())+
  theme(axis.title=element_blank())+
  scale_x_continuous(breaks = c(0, 0.25, 0.50, 0.75, 1), labels = c(0,25,50,75,100))+
  coord_cartesian(ylim = c(0,.15))

# Part 3K: Virginia Opossum Plot -------------------------------------------------
#Generate Virginia Opossum model and use emmeans() function for generating model line and confidence intervals

virginia_opossum_mod=glm.nb(virginia_opossum ~ percent_developed_1km +
                              offset(log(n_fish_deployed)),
                            data=urban_scavengers_summary);summary(virginia_opossum_mod)

virginia_opossum_mod_emm <- emmeans(virginia_opossum_mod, ~percent_developed_1km,
                                    at = list(percent_developed_1km = seq(0,1, length.out = 101)), 
                                    type = "response", offset = log(1))

virginia_opossum_mod_df <- as.data.frame(virginia_opossum_mod_emm) %>% 
  mutate(species = "virginia_opossum")

#Generate Virginia Opossum Plot
virginia_opossum_plot_2 <- ggplot(data=virginia_opossum_mod_df,
                                  aes(x=percent_developed_1km,
                                      y=response))+
  facet_wrap("species", 
             labeller = as_labeller(c(virginia_opossum='Virginia Opossum')))+
  geom_line() +
  geom_ribbon(aes(ymin = asymp.LCL, ymax = asymp.UCL), 
              colour = NA, alpha = 0.2) +
  geom_point(data = filter(plot_df, species == "virginia_opossum"), 
             aes(x= percent_developed_1km, y=maxN/n_fish_deployed), 
             alpha = .5, size = 3)+
  theme_few()+
  labs(element_blank())+
  theme(axis.title=element_blank())+
  scale_x_continuous(breaks = c(0, 0.25, 0.50, 0.75, 1), labels = c(0,25,50,75,100))+
  coord_cartesian(ylim = c(0,.15))

# Part 3L: Western Gull Plot -------------------------------------------------
#Generate Western Gull model and use emmeans() function for generating model line and confidence intervals

western_gull_mod=glm.nb(western_gull ~ percent_developed_1km +
                          offset(log(n_fish_deployed)),
                        data=urban_scavengers_summary);summary(western_gull_mod)

western_gull_mod_emm <- emmeans(western_gull_mod, ~percent_developed_1km,
                                at = list(percent_developed_1km = seq(0,1, length.out = 101)), 
                                type = "response", offset = log(1))

western_gull_mod_df <- as.data.frame(western_gull_mod_emm) %>% 
  mutate(species = "western_gull")

#Generate Western Gull Plot
western_gull_plot_2 <- ggplot(data=western_gull_mod_df,
                              aes(x=percent_developed_1km,
                                  y=response))+
  facet_wrap("species", 
             labeller = as_labeller(c(western_gull='Western Gull')))+
  geom_line() +
  geom_ribbon(aes(ymin = asymp.LCL, ymax = asymp.UCL), 
              colour = NA, alpha = 0.2) +
  geom_point(data = filter(plot_df, species == "western_gull"), 
             aes(x= percent_developed_1km, y=maxN/n_fish_deployed), 
             alpha = .5, size = 3)+
  theme_few()+
  labs(element_blank())+
  theme(axis.title=element_blank())+
  scale_x_continuous(breaks = c(0, 0.25, 0.50, 0.75, 1), labels = c(0,25,50,75,100))+
  coord_cartesian(ylim = c(0,.15))

# Part 3M: Combine Plots and Export --------------------------------------------

combined_plot_2 <- plot_grid(american_crow_plot_2, common_raven_plot_2, coyote_plot_2,
                             deer_mouse_plot_2, domestic_cat_plot_2, domestic_dog_plot_2, 
                             gray_fox_plot_2, raccoon_plot_2, rat_plot_2, 
                             striped_skunk_plot_2, virginia_opossum_plot_2, western_gull_plot_2)+
  draw_label("Percent Urbanized (1km)", x=0.5, y=0, 
             angle= 0) +
  draw_label("Scavenger Abundance \n(MaxN / n carcasses deployed)", x=0, y=0.5, 
             vjust= 0, 
             angle=90)+
  theme(plot.margin = unit(c(0.5,0.5,0.5,1), "cm"))


combined_plot_2

#Export high-quality figure of combined plot

pdf("output/supp_figures/urbanization_all_scavengers_glm.pdf", 
    width = 8, height = 6)

plot(combined_plot_2)

dev.off()


#############################################################################
# PART 4: Urbanization Effects on Carrion Processing Plots ##################
#############################################################################

# PART 4A: Urbanization Effect on Probability of ANY scavenging -------------------

#Recreate model g1 from "04_Univariate_Analyses.R", investigating effects of urbanization (1km) on the probability of ANY scavenging with a mixed-effects logistic regression (generalized linear mixed effects model with binomial distribution and logit link)
g1=glmer(scav_prob~ #scav_prob (1/0) binary probability of any scavenging activity
           percent_developed_1km + #1km urbanization extent
           (1|site_name), #site as random effect
         family = "binomial", #binomial distribution
         data=carcass_level_summary);summary(g1) 


#Create dataframe for generating a line and error bar representing model g1
g1_plot=data.frame(percent_developed_1km=rep(0:100)/100, site_name = "Blacks")
#predict probability of scavenging using the model
g1_plot$scav_prob <- predict(g1,
                              newdata=g1_plot,
                              type="response", 
                              re.form=NA) #added extra step for mixed effects models

#Code for generating confidence intervals for GLMER model with a logit link. 

#predict mean values on link/logit scale
g1_plot$pred_scav_prob_link=predict(g1,newdata=g1_plot,re.form=NA,type="link")
#function for bootstrapping
pf1 = function(fit) {   predict(fit, g1_plot) } 
#bootstrap to estimate uncertainty in predictions
bb=bootMer(g1,nsim=1000,FUN=pf1,seed=999) 
#Calculate SEs from bootstrap samples on link scale
g1_plot$SE=apply(bb$t, 2, sd) 
#predicted mean + 1 SE on response scale
g1_plot$pSE=plogis(g1_plot$pred_scav_prob_link+g1_plot$SE) 
# predicted mean - 1 SE on response scale
g1_plot$mSE=plogis(g1_plot$pred_scav_prob_link-g1_plot$SE) 



#Generate ggplot
scav_prob_plot1 <- ggplot(carcass_level_summary, aes(x=percent_developed_1km, y=scav_prob))+ 
  geom_jitter(size = 4, height = 0.03, alpha = .4, shape = 16)+ 
  geom_ribbon(data=g1_plot, 
              aes(x=percent_developed_1km,
                 ymin=mSE,ymax=pSE),
              alpha=0.3,linetype=0)+
  geom_line(data=g1_plot,aes(x=percent_developed_1km,y=scav_prob))+
  theme_few()+
  labs(y = "Probability of Carcass Scavenging", x="")+
  scale_x_continuous(breaks = c(0, 0.25, 0.50, 0.75, 1), labels = c(0,25,50,75,100))+
  theme(axis.title.y = element_text(size = 20))
  

scav_prob_plot1


#Export high-quality figure of scavenging probability plot

pdf("output/extra_figures/scavenge_probability_plot.pdf", 
    width = 8, height = 6)

plot(scav_prob_plot1)

dev.off()




# PART 4B: Urbanization Effect on Probability of Carcass Removal ---------------

#Recreate model g2 from "04_Univariate_Analyses.R", analyzing the probability of carcass removal using using 1km buffer with a mixed-effects logistic regression (generalized linear mixed effects model with binomial distribution and logit link)
g2=glmer(full_scav_prob~ #full_scav_prob (1/0) binary probability of carcass removal
           percent_developed_1km+ #1km urbanization extent
           (1|site_name), #site as random effect
         family = "binomial", #binomial distribution
         data=carcass_level_summary);summary(g2)

#Create dataframe for generating a line and error bar representing model g2
g2_plot=data.frame(percent_developed_1km=rep(0:100)/100, site_name = "Blacks")
#predict probability of scavenging using the model
g2_plot$scav_prob <- predict(g2,
                              newdata=g2_plot,
                              type="response", 
                              re.form=NA) #added extra step for mixed effects models

#Code for generating confidence intervals for GLMER model with a logit link. 

#predict mean values on link/logit scale
g2_plot$pred_scav_prob_link=predict(g2,newdata=g2_plot,re.form=NA,type="link")
#function for bootstrapping
pf1 = function(fit) {   predict(fit, g2_plot) } 
#bootstrap to estimate uncertainty in predictions
bb=bootMer(g2,nsim=1000,FUN=pf1,seed=999) 
#Calculate SEs from bootstrap samples on link scale
g2_plot$SE=apply(bb$t, 2, sd) 
#predicted mean + 1 SE on response scale
g2_plot$pSE=plogis(g2_plot$pred_scav_prob_link+g2_plot$SE) 
# predicted mean - 1 SE on response scale
g2_plot$mSE=plogis(g2_plot$pred_scav_prob_link-g2_plot$SE) 



#Generate ggplot
scav_prob_plot2 <- ggplot(carcass_level_summary, aes(x=percent_developed_1km, y=scav_prob))+ 
  geom_jitter(size = 4, height = 0.03, alpha = .4, shape = 16)+
  geom_ribbon(data=g2_plot,
              aes(x=percent_developed_1km,
                  ymin=mSE,ymax=pSE),
              alpha=0.3,linetype=0)+
  geom_line(data=g2_plot,aes(x=percent_developed_1km,y=scav_prob))+
  theme_few()+
  labs(y = "Probability of Complete Carcass Removal", x="")+
  scale_x_continuous(breaks = c(0, 0.25, 0.50, 0.75, 1), labels = c(0,25,50,75,100))+
  theme(axis.title.y = element_text(size = 20))


scav_prob_plot2


#Export high-quality figure of scavenging probability plot

pdf("output/extra_figures/total_scavenge_probability_plot.pdf", 
    width = 8, height = 6)

plot(scav_prob_plot2)

dev.off()




# PART 4C: Urbanization Effect on Time to First Scavenging Event ---------------

#Recreate model g3 from "04_Univariate_Analyses.R", to analyze time until first scavenging event using using 1km urbanization buffer with a generalized linear mixed-effects model with Gamma distribution and log link. 


#Create datafeame filtering out carcasses in which no scavenging occurred
temporal_df <- carcass_level_summary %>% 
  filter(hours_to_first_scavenging_event != "NA") %>% 
  mutate(site_name = factor(site_name)) #remove unused levels from factor

#Create model
g3=glmmTMB(hours_to_first_scavenging_event~ 
             percent_developed_1km+ #1km urbanization
             (1|site_name), #site as random effect
           family=Gamma(link = "log"), #gamma distribution with log link
           data=temporal_df) #dataframe with no-scavenge carcasses removed
summary(g3)

#Create dataframe for generating a line and error bar representing model g3
g3_plot=data.frame(percent_developed_1km=rep(0:100)/100, site_name = "Blacks")
#predict probability of scavenging using the model
g3_plot$hours_to_first_scavenging_event <- predict(g3,
                              newdata=g3_plot,
                              type="response", 
                              re.form=NA) #added extra step for mixed effects models

#Code for generating confidence intervals for glmmTMB model with a log link. 

#predict mean values on link/log scale
g3_plot$pred_scav_prob_link=predict(g3,newdata=g3_plot,re.form=NA,type="link")
#function for bootstrapping
pf1 = function(fit) {   predict(fit, g3_plot) } 
#bootstrap to estimate uncertainty in predictions
bb=bootMer(g3,nsim=1000,FUN=pf1,seed=999) 
#Calculate SEs from bootstrap samples on link scale
g3_plot$SE=apply(bb$t, 2, sd) 
#predicted mean + 1 SE on response scale
g3_plot$pSE=exp(g3_plot$pred_scav_prob_link+g3_plot$SE) 
# predicted mean - 1 SE on response scale
g3_plot$mSE=exp(g3_plot$pred_scav_prob_link-g3_plot$SE) 



#Generate ggplot
temporal_scav_plot1 <- ggplot(temporal_df, aes(x=percent_developed_1km, y=hours_to_first_scavenging_event))+ 
  geom_point(colour = "black", alpha = .4, size=4, shape=16)+
  geom_ribbon(data=g3_plot,
              aes(x=percent_developed_1km,
                  ymin=mSE,ymax=pSE),
              alpha=0.3,linetype=0)+
  geom_line(data=g3_plot,aes(x=percent_developed_1km,y=hours_to_first_scavenging_event))+
  theme_few()+
  labs(y = "Hours Until First Scavenging Event", x="")+
  scale_x_continuous(breaks = c(0, 0.25, 0.50, 0.75, 1), labels = c(0,25,50,75,100))+
  theme(axis.title.y = element_text(size = 20))


temporal_scav_plot1

#Export high-quality figure of temporal plot 1

pdf("output/extra_figures/hours_until_first_scavenge_plot.pdf", 
    width = 8, height = 6)

plot(temporal_scav_plot1)

dev.off()



# PART 4D: Urbanization Effect on Time to Full Scavenging ---------------

#Recreate model g4 from "04_Univariate_Analyses.R", investigating time until carcass removal using 1km urbanization buffer with a generalized linear mixed-effects model with Gamma distribution and log link

#Create datafeame filtering out carcasses in which either (1) no scavenging occurred or (2) carcass was only partially scavenged
temporal_df2 <- carcass_level_summary %>% 
  filter(hours_to_full_scavenge != "NA")%>% 
  mutate(site_name = factor(site_name)) #remove unused levels from factor

#Create model
g4=glmmTMB(hours_to_full_scavenge~
             percent_developed_1km+ #1km urbanization
             (1|site_name), #site as random effect
           family=Gamma(link = "log"), #gamma distribution with log link
           data=temporal_df2) #dataframe with only complete carcass removal
summary(g4)


#Create dataframe for generating a line and error bar representing model g4
g4_plot=data.frame(percent_developed_1km=rep(0:100)/100, site_name = "Blacks")
#predict probability of scavenging using the model
g4_plot$hours_to_full_scavenge <- predict(g4,
                                                    newdata=g4_plot,
                                                    type="response", 
                                                    re.form=NA) #added extra step for mixed effects models

#Code for generating confidence intervals for glmmTMB model with a logit link. 

#predict mean values on link/log scale
g4_plot$pred_scav_prob_link=predict(g4,newdata=g4_plot,re.form=NA,type="link")
#function for bootstrapping
pf1 = function(fit) {   predict(fit, g4_plot) } 
#bootstrap to estimate uncertainty in predictions
bb=bootMer(g4,nsim=1000,FUN=pf1,seed=999) 
#Calculate SEs from bootstrap samples on link scale
g4_plot$SE=apply(bb$t, 2, sd) 
#predicted mean + 1 SE on response scale
g4_plot$pSE=exp(g4_plot$pred_scav_prob_link+g4_plot$SE) 
# predicted mean - 1 SE on response scale
g4_plot$mSE=exp(g4_plot$pred_scav_prob_link-g4_plot$SE) 



#Generate ggplot
temporal_scav_plot2 <- ggplot(temporal_df2, aes(x=percent_developed_1km, y=hours_to_full_scavenge))+ 
  geom_point(colour = "black", alpha = .4, size=4, shape=16)+
  geom_ribbon(data=g4_plot,
              aes(x=percent_developed_1km,
                  ymin=mSE,ymax=pSE),
              alpha=0.3,linetype=0)+
  geom_line(data=g4_plot,aes(x=percent_developed_1km,y=hours_to_full_scavenge))+
  theme_few()+
  labs(y = "Hours Until Carcass Removal", x="")+
  scale_x_continuous(breaks = c(0, 0.25, 0.50, 0.75, 1), labels = c(0,25,50,75,100))+
  theme(axis.title.y = element_text(size = 20))


temporal_scav_plot2

#Export high-quality figure of temporal plot

pdf("output/extra_figures/hours_until_full_scavenge_plot.pdf", 
    width = 8, height = 6)

plot(temporal_scav_plot2)

dev.off()


##############################################################################
# PART 5: Domestic Dog and Urbanization Effect on Carrion Processing Plot ####
##############################################################################


#Recreate model h1b from "04_Univariate_Analyses.R", analyzing the probability of carcass removal using using 1km buffer with a mixed-effects logistic regression (generalized linear mixed effects model with binomial distribution and logit link)
h1b <- glmmTMB(scav_prob~ #scav_prob (1/0) binary probability of any scavenging activity
                 percent_developed_1km +
                 domestic_dog_visitors_per_day +
                 (1|site_name), #site as random effect
               family = "binomial", #binomial distribution
               data=carcass_level_summary);summary(h1b) 


#Create dataframe for generating a line and error bar representing model h1b
h1b_plot=data.frame(domestic_dog_visitors_per_day=rep(0:25,5), 
                    percent_developed_1km = rep(c(0,.25,.5,.75, 1), each = 26),
                    site_name = "Blacks") #Choose random site (wont impact output)

#predict probability of scavenging using the model
h1b_plot$scav_prob <- predict(h1b,
                              newdata=h1b_plot,
                              type="response", 
                              re.form=NA)#added extra step for mixed effects models

#Code for generating confidence intervals for GLMER model with a logit link. 

#predict mean values on link/logit scale
h1b_plot$pred_scav_prob_link=predict(h1b,newdata=h1b_plot,re.form=NA,type="link")
#function for bootstrapping
pf1 = function(fit) {   predict(fit, h1b_plot) } 
#bootstrap to estimate uncertainty in predictions
bb=bootMer(h1b,nsim=1000,FUN=pf1,seed=999) 
#Calculate SEs from bootstrap samples on link scale
h1b_plot$SE=apply(bb$t, 2, sd) 
#predicted mean + 1 SE on response scale
h1b_plot$pSE=plogis(h1b_plot$pred_scav_prob_link+h1b_plot$SE) 
# predicted mean - 1 SE on response scale
h1b_plot$mSE=plogis(h1b_plot$pred_scav_prob_link-h1b_plot$SE) 



#Turn percent_developed_1km variables into factors for plotting
h1b_plot <- h1b_plot %>% 
  mutate(percent_developed_1km = factor(percent_developed_1km, levels = c(1, .75, .5, .25, 0)))

carcass_level_summary_2 <- carcass_level_summary %>% 
  mutate(percent_developed_1km = factor(percent_developed_1km, levels = c(1, .75, .5, .25, 0)))


#Generate ggplot
dog_urbanization_plot <- ggplot(carcass_level_summary_2, 
               aes(x=domestic_dog_visitors_per_day, y=scav_prob))+ 
  geom_jitter(size = 4, height = 0.03, alpha = .4, shape = 16)+
  geom_ribbon(data=h1b_plot,
              aes(x=domestic_dog_visitors_per_day,
                  ymin=mSE,ymax=pSE, fill = percent_developed_1km),
              alpha=0.3,linetype=0)+
  geom_line(data=h1b_plot,aes(x=domestic_dog_visitors_per_day,
                              y=scav_prob, 
                              color = percent_developed_1km))+
  theme_few()+
  labs(y = "Probability of Carcass Scavenging", x="Domestic Dog Visitation (No. recordings/day)", color = "Percent\nUrbanized\n(1km)")+
  scale_color_manual(values = rev(wes_palette("Zissou1", 
                                             n=5,
                                          type = "discrete")), 
                     name="Percent \nUrbanized \n(1km)",
                     labels = c("100","75","50","25","0"))+
  scale_fill_manual(values = rev(wes_palette("Zissou1", 
                                         n=5,
                                         type = "discrete")),
                    name="Percent \nUrbanized \n(1km)",
                    labels = c("100","75","50","25","0"))

dog_urbanization_plot

#Export high-quality figure of dog and urbanization plot

pdf("output/extra_figures/dog_urbanization_glmm.pdf", 
    width = 8, height = 4.5)

plot(dog_urbanization_plot)

dev.off()





##############################################################################
# PART 6: Domestic Dog and Diel Period Carrion Processing Plot ###############
##############################################################################


#Recreate model h1e from "04_Univariate_Analyses.R", analyzing the probability of carcass removal using using 1km buffer with a mixed-effects logistic regression (generalized linear mixed effects model with binomial distribution and logit link)

h1e <- glmmTMB(scav_prob~ #scav_prob (1/0) binary probability of any scavenging activity
                 deployment_type_AM_PM +
                 domestic_dog_visitors_per_day +
                 (1|site_name), #site as random effect
               family = "binomial", #binomial distribution
               data=carcass_level_summary);summary(h1e) 


#Create dataframe for generating a line and error bar representing model h1e
h1e_plot=data.frame(domestic_dog_visitors_per_day=rep(0:25,2), 
                    deployment_type_AM_PM = rep(c("day", "night"), each = 26),
                    site_name = "Blacks") #Choose random site (wont impact output)

#predict probability of scavenging using the model
h1e_plot$scav_prob <- predict(h1e,
                              newdata=h1e_plot,
                              type="response", 
                              re.form=NA)#added extra step for mixed effects models

#Code for generating confidence intervals for GLMER model with a logit link. 

#predict mean values on link/logit scale
h1e_plot$pred_scav_prob_link=predict(h1e,newdata=h1e_plot,re.form=NA,type="link")
#function for bootstrapping
pf1 = function(fit) {   predict(fit, h1e_plot) } 
#bootstrap to estimate uncertainty in predictions
bb=bootMer(h1e,nsim=1000,FUN=pf1,seed=999) 
#Calculate SEs from bootstrap samples on link scale
h1e_plot$SE=apply(bb$t, 2, sd) 
#predicted mean + 1 SE on response scale
h1e_plot$pSE=plogis(h1e_plot$pred_scav_prob_link+h1e_plot$SE) 
# predicted mean - 1 SE on response scale
h1e_plot$mSE=plogis(h1e_plot$pred_scav_prob_link-h1e_plot$SE) 


#Generate ggplot
dog_period_plot <- ggplot(carcass_level_summary, 
                          aes(x=domestic_dog_visitors_per_day, 
                              y=scav_prob, 
                              color = deployment_type_AM_PM))+ 
  geom_jitter(size = 4, height = 0.03, alpha = .4, shape = 16)+
  geom_ribbon(data=h1e_plot,
              aes(x=domestic_dog_visitors_per_day,
                  ymin=mSE,ymax=pSE, fill = deployment_type_AM_PM),
              alpha=0.3,linetype=0)+
  geom_line(data=h1e_plot,aes(x=domestic_dog_visitors_per_day,
                              y=scav_prob, 
                              color = deployment_type_AM_PM))+
  theme_few()+
  labs(y = "Probability of Carcass Scavenging", 
       x="Domestic Dog Visitation (# recordings/day)", 
       color = "Deployment\nType",
       fill = "Deployment\nType",)+
  scale_color_manual(values = c("#FFB921","#003f7d"),
                     labels = c("Day", "Night"))+
  scale_fill_manual(values = c("#FFB921","#003f7d"),
                    labels = c("Day", "Night"))

dog_period_plot


#Export high-quality figure of dog and diel period plot

pdf("output/supp_figures/dog_period_glmm.pdf", 
    width = 8, height = 4.5)

plot(dog_period_plot)

dev.off()


##############################################################################
# PART 7: Time of Day and Urbanization Effect on Carrion Processing Plot #####
##############################################################################

##############################################
# PART 7A: Time to First Scavenging Event ####
##############################################

#Recreate model h3d from "04_Univariate_Analyses.R", analyzing the time to first stavenging event using using 1km urbanization buffer and deployment time (day/night) with a mixed-effects gamma regression 

h3d <- glmmTMB(hours_to_first_scavenging_event~ 
                 percent_developed_1km + #1km urbanization extent
                 deployment_type_AM_PM+
                 (1|site_name), #site as random effect
               family=Gamma(link = "log"), #gamma distribution with log link
               data=temporal_df);summary(h3d) #dataframe with no-scavenge carcasses removed



#Create dataframe for generating a line and error bar representing model h3d
h3d_plot=data.frame(percent_developed_1km = rep(c(0,.25,.5,.75, 1), each = 2),
                    deployment_type_AM_PM = rep(c("day", "night"), 5),
                    site_name = "Blacks") #Choose random site (wont impact output)

#predict probability of scavenging using the model
h3d_plot$hours_to_first_scavenging_event <- predict(h3d,
                              newdata=h3d_plot,
                              type="response", 
                              re.form=NA)#added extra step for mixed effects models

#Code for generating confidence intervals for GLMER model with a log link. 

#predict mean values on link/log scale
h3d_plot$pred_scav_prob_link=predict(h3d,newdata=h3d_plot,re.form=NA,type="link")
#function for bootstrapping
pf1 = function(fit) {   predict(fit, h3d_plot) } 
#bootstrap to estimate uncertainty in predictions
bb=bootMer(h3d,nsim=1000,FUN=pf1,seed=999) 
#Calculate SEs from bootstrap samples on link scale
h3d_plot$SE=apply(bb$t, 2, sd) 
#predicted mean + 1 SE on response scale
h3d_plot$pSE=exp(h3d_plot$pred_scav_prob_link+h3d_plot$SE) 
# predicted mean - 1 SE on response scale
h3d_plot$mSE=exp(h3d_plot$pred_scav_prob_link-h3d_plot$SE) 



urbanization_period_plot1 <- ggplot(temporal_df, aes(x=percent_developed_1km, y=hours_to_first_scavenging_event, color = deployment_type_AM_PM))+ 
  geom_point(alpha = .5, size=4, shape=16)+
  geom_ribbon(data=h3d_plot,
              aes(x=percent_developed_1km,
                  ymin=mSE,ymax=pSE, fill = deployment_type_AM_PM),
              alpha=0.3,linetype=0)+
  geom_line(data=h3d_plot,aes(x=percent_developed_1km,
                             y=hours_to_first_scavenging_event,
                             color =deployment_type_AM_PM))+
  theme_few()+
  labs(y = "Hours Until First Scavenging Event", 
       x="Percent Urbanized (1km)",
       color = "Deployment\nType",
       fill = "Deployment\nType")+
  scale_x_continuous(breaks = c(0, 0.25, 0.50, 0.75, 1), 
                     labels = c(0,25,50,75,100))+
  scale_color_manual(values = c("#FFB921","#003f7d"),
                     labels = c("Day", "Night"))+
  scale_fill_manual(values = c("#FFB921","#003f7d"),
                    labels = c("Day", "Night"))


urbanization_period_plot1

#Export high-quality figure of plot

pdf("output/extra_figures/urbanization_period_glmm1.pdf", 
    width = 8, height = 4.5)

plot(urbanization_period_plot1)

dev.off()


##############################################
# PART 7B: Time to Carcass Removal ###########
##############################################


#Recreate model h4d from "04_Univariate_Analyses.R", analyzing the time to carcass removal using using 1km urbanization buffer and deployment time (day/night) with a mixed-effects gamma regression 

h4d <- glmmTMB(hours_to_full_scavenge~ 
                 percent_developed_1km + #1km urbanization extent
                 deployment_type_AM_PM+
                 (1|site_name), #site as random effect
               family=Gamma(link = "log"), #gamma distribution with log link
               data=temporal_df2);summary(h4d) #dataframe with no-scavenge carcasses removed


#Create dataframe for generating a line and error bar representing model h4d
h4d_plot=data.frame(percent_developed_1km = rep(c(0,.25,.5,.75, 1), each = 2),
                    deployment_type_AM_PM = rep(c("day", "night"), 5),
                    site_name = "Blacks") #Choose random site (wont impact output)

#predict probability of scavenging using the model
h4d_plot$hours_to_full_scavenge <- predict(h4d,
                                           newdata=h4d_plot,
                                           type="response", 
                                           re.form=NA)#added extra step for mixed effects models

#Code for generating confidence intervals for GLMER model with a log link. 

#predict mean values on link/log scale
h4d_plot$pred_scav_prob_link=predict(h4d,newdata=h4d_plot,re.form=NA,type="link")
#function for bootstrapping
pf1 = function(fit) {   predict(fit, h4d_plot) } 
#bootstrap to estimate uncertainty in predictions
bb=bootMer(h4d,nsim=1000,FUN=pf1,seed=999) 
#Calculate SEs from bootstrap samples on link scale
h4d_plot$SE=apply(bb$t, 2, sd) 
#predicted mean + 1 SE on response scale
h4d_plot$pSE=exp(h4d_plot$pred_scav_prob_link+h4d_plot$SE) 
# predicted mean - 1 SE on response scale
h4d_plot$mSE=exp(h4d_plot$pred_scav_prob_link-h4d_plot$SE) 



urbanization_period_plot2 <- ggplot(temporal_df2, aes(x=percent_developed_1km, y=hours_to_full_scavenge, color = deployment_type_AM_PM))+ 
  geom_point(alpha = .5, size=4, shape=16)+
  geom_ribbon(data=h4d_plot,
              aes(x=percent_developed_1km,
                  ymin=mSE,ymax=pSE, fill = deployment_type_AM_PM),
              alpha=0.3,linetype=0)+
  geom_line(data=h4d_plot,aes(x=percent_developed_1km,
                              y=hours_to_full_scavenge,
                              color =deployment_type_AM_PM))+
  theme_few()+
  labs(y = "Hours Until Carcass Removal", 
       x="Percent Urbanized (1km)",
       color = "Deployment\nType",
       fill = "Deployment\nType")+
  scale_x_continuous(breaks = c(0, 0.25, 0.50, 0.75, 1), 
                     labels = c(0,25,50,75,100))+
  scale_color_manual(values = c("#FFB921","#003f7d"),
                     labels = c("Day", "Night"))+
  scale_fill_manual(values = c("#FFB921","#003f7d"),
                    labels = c("Day", "Night"))


urbanization_period_plot2

#Export high-quality figure of plot

pdf("output/extra_figures/urbanization_period_glmm2.pdf", 
    width = 8, height = 4.5)

plot(urbanization_period_plot2)

dev.off()

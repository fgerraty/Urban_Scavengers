##########################################################################
# Santa Cruz Urban Scavengers Project ####################################
# Author: Frankie Gerraty (frankiegerraty@gmail.com; fgerraty@ucsc.edu) ##
##########################################################################
# Script 00: Load packages ###############################################
#-------------------------------------------------------------------------

# Part 1: Load Packages --------------------------------------------------

# Load packages
packages<- c("tidyverse", "readr", "janitor", "raster", "rasterVis", "sf", "mapview", "ggspatial", "mapsf", "car", "ggthemes", "lme4", "cowplot", "lubridate", "vegan", "scatterplot3d", "ggthemes", "ggh4x", "MASS", "AICcmodavg", "mvabund", "gt", "webshot2", "glmmTMB", "DHARMa", "wesanderson", "AICcPermanova", "maps", "emmeans", "waffle")

pacman::p_load(packages, character.only = TRUE)


###############################################################################
# Santa Cruz Urban Scavengers Project #########################################
# Author: Frankie Gerraty (frankiegerraty@gmail.com; fgerraty@ucsc.edu) #######
###############################################################################
# Script 03: Scavenging Assemblage Analyses ###################################
#------------------------------------------------------------------------------

##########################################################
# PART 1: Multivariate Analysis using PERMANOVA ##########
##########################################################

#First, generate the correct version of the akaike_adjusted_rsq() function from AICcPermanova version 0.0.3; which is non-functional in AICcPermanova version 0.0.2. The adjusted R squared is calculated as: \deqn{Adjusted R^2 = 1 - (RSS / (N - k - 1)) * ((N - 1) / (N - k - 1))} where RSS is the residual sum of squares, N is the sample size, and k is the number of predictors. The R squared is adjusted based on the weight of evidence in favor of each model, which is calculated as: \deqn{w_i = exp(-0.5 * DeltaAICc_i) / sum(exp(-0.5 * DeltaAICc))} where w_i is the weight of evidence in favor of the ith model, and DeltaAICc_i is the difference in AICc between the ith model and the best model. Model averaging uses the weights to combine the performance of different models in the final calculation of the adjusted R squared.

akaike_adjusted_rsq <- function(DF) {
  AICc <- DeltaAICc <- max_vif <- AICWeight <- Model <- k <- N <- form <- value <- Variable <- Number_of_models <- Full_Akaike_Adjusted_RSq <- NULL
  Result <- DF |>
    dplyr::select(-AICc, -DeltaAICc, -matches("Model"), -max_vif, -k, -N) |>
    dplyr::mutate_if(is.numeric, ~ ifelse(is.na(.x), 0, .x)) |>
    tidyr::pivot_longer(cols = c(-form, -AICWeight), names_to = "Variable") |>
    dplyr::mutate(Full_Akaike_Adjusted_RSq = value * AICWeight, Number_of_models = ifelse(value == 0, 0, 1)) |>
    dplyr::group_by(Variable) |>
    dplyr::summarise_if(is.numeric, sum) |>
    dplyr::select(-value, -AICWeight) |>
    dplyr::arrange(dplyr::desc(Number_of_models), dplyr::desc(Full_Akaike_Adjusted_RSq))
  
  return(Result)
}

#Import dataset
urban_scavengers_summary <- read_csv("data/processed/urban_scavengers_summary.csv") %>% 
  filter(site_name != "Strawberry") #Remove strawberry beach from analysis b/c no scavenging assemblage


# PART 1A: Set up PERMANOVA analysis -------------------------------------------

#set seed for reproducability
set.seed(999) 

# Create object composed of all of the scavenger species and their maxN values, divided by the number of fish deployed at each site
scav_assemblage <- data.frame(urban_scavengers_summary[,18:29]/urban_scavengers_summary$n_fish_deployed)

#Create object of the predictor values 
predictors <- urban_scavengers_summary[,c(4:11)]

#Generate Bray-Curtis distance matrix
distance_matrix <- vegdist(scav_assemblage, method = "bray")

# PART 1B: Generate and filter modeling suite ----------------------------------

#Generate all possible first-order models for this set of predictors, which results in 256 possible models
AllModels <- make_models(vars = c("percent_developed_1km", "percent_developed_3km", "percent_developed_5km", "percent_agricultural_1km", "percent_agricultural_3km", "percent_agricultural_5km", "human_visitors_per_day", "domestic_dog_visitors_per_day"))

#Filter out models with high degree of collinearity (having a maximum value of VIF of 5 or more), leading to 64 possible non-collinear models
NonCollinear <- filter_vif(all_forms = AllModels, env_data = predictors)

# PART 1C: Fit and summarize PERMANOVA models ----------------------------------

#Fit the 64 non-collinear PERMANOVA models - uses the vegan adonis2() function
Fitted <- fit_models(
  all_forms = NonCollinear,
  veg_data = scav_assemblage,
  env_data = predictors,
  log = FALSE,
  method = "bray")

#Filter models for those with (1) delta AICc less than 2 and (2) maximum VIF no greater than 5.
Selected <- select_models(Fitted, delta_aicc = 2)
Selected #take a look at output of 6 top models with delta AICc less than 2

#Create summarized r squared weighted by AICc
Summary <- akaike_adjusted_rsq(Selected)
Summary #take a look at summary output of each predictor's adjusted r squared calculated using AIC and model averaging

# PART 1D: Look at top models to ensure a good fit -----------------------------

#Take a look at the eleven top models to see if they fit well
top_model <- adonis2(distance_matrix ~ predictors$percent_developed_1km)
top_model_2 <- adonis2(distance_matrix ~ predictors$percent_developed_3km)
top_model_3 <- adonis2(distance_matrix ~ predictors$percent_developed_5km)
top_model_4 <- adonis2(distance_matrix ~ predictors$percent_developed_1km+predictors$percent_agricultural_3km)
top_model_5 <- adonis2(distance_matrix ~ predictors$percent_developed_1km+predictors$percent_agricultural_5km)
top_model_6 <- adonis2(distance_matrix ~ predictors$percent_developed_1km+predictors$human_visitors_per_day)
top_model_7 <- adonis2(distance_matrix ~ predictors$percent_developed_3km+predictors$percent_agricultural_3km)
top_model_8 <- adonis2(distance_matrix ~ predictors$percent_developed_3km+predictors$percent_agricultural_5km)
top_model_9 <- adonis2(distance_matrix ~ predictors$percent_developed_1km+predictors$domestic_dog_visitors_per_day)
top_model_10 <- adonis2(distance_matrix ~ predictors$percent_developed_3km+predictors$human_visitors_per_day)
top_model_11 <- adonis2(distance_matrix ~ predictors$percent_developed_3km+predictors$domestic_dog_visitors_per_day)



#Yes, the top three models fit well, but models 4-11 do not have significant additive terms so they are basically single-term models.
top_model; top_model_2; top_model_3; top_model_4; top_model_5; top_model_6; top_model_7; top_model_8; top_model_9; top_model_10; top_model_11


# PART 1E: Make publication-quality tables for export ----------------------------

#Selected models table ####
selected_models <- Selected %>% 
  #Replace characters from models for better descriptions
  mutate(model_terms = str_replace(form, "Distance ~ ", ""),
         model_terms = str_replace(model_terms, "percent_developed_", "Urbanization Extent "),
         model_terms = str_replace(model_terms, "percent_agricultural_", "Agricultural Extent "),
         model_terms = str_replace(model_terms, "percent_agricultural_", "Agricultural Extent "),
         model_terms = str_replace(model_terms, "domestic_dog_visitors_per_day", "Domestic Dog Visitation"),
         model_terms = str_replace(model_terms, "human_visitors_per_day", "Human Visitation"))%>% 
  #Select important variables
  dplyr::select(model_terms, AICc, DeltaAICc, AICWeight)


#convert to "gt" object for exporting publication-quality table
selected_models_summary_table_gt <- gt(selected_models)

selected_models_summary <- 
  selected_models_summary_table_gt |>
  tab_header(
    title = "PERMANOVA Top Models Summary (∆AICc <2)"
  ) |>
  cols_label(model_terms = md("**Model terms**"),
             AICc = md("**AICc**"),
             DeltaAICc = md("**∆AICc**"),
             AICWeight = md("**AIC Weight**"))

# Show the gt Table
selected_models_summary

#Export high-quality table
gtsave(selected_models_summary, "output/supp_figures/PERMANOVA_top_models_table.pdf")


#Summarized predictors table ####
summarized_predictors <- Summary %>% 
  mutate(Predictor = str_replace(Variable, "percent_developed_", "Urbanization Extent "),
         Predictor = str_replace(Predictor, "percent_agricultural_", "Agricultural Extent "),
         Predictor = str_replace(Predictor, "percent_agricultural_", "Agricultural Extent "),
         Predictor = str_replace(Predictor, "domestic_dog_visitors_per_day", "Domestic Dog Visitation"),
         Predictor = str_replace(Predictor, "human_visitors_per_day", "Human Visitation")) %>% dplyr::select(Predictor, Full_Akaike_Adjusted_RSq,Number_of_models) %>% 
  arrange(desc(Full_Akaike_Adjusted_RSq))
  


#convert to "gt" object for exporting publication-quality table
summarized_predictors_gt <- gt(summarized_predictors)

PERMANOVA_predictors_summary <- 
  summarized_predictors_gt |>
  tab_header(
    title = "PERMANOVA Top Predictors Summary"
  ) |>
  cols_label(Predictor = md("**Predictor**"),
             Full_Akaike_Adjusted_RSq = md("**Aikake Adjusted Rsq**"),
             Number_of_models = md("**Number of Top Models**"))

# Show the gt Table
PERMANOVA_predictors_summary

#Export high-quality table
gtsave(PERMANOVA_predictors_summary, "output/supp_figures/PERMANOVA_top_predictors_table.pdf")


# All fitted models table ####

all_fitted_models <- select_models(Fitted, delta_aicc = 11) %>% 
  #Replace characters from models for better descriptions
  mutate(model_terms = str_replace(form, "Distance ~ ", ""),
         model_terms = str_replace(model_terms, "percent_developed_", "Urbanization Extent "),
         model_terms = str_replace(model_terms, "percent_agricultural_", "Agricultural Extent "),
         model_terms = str_replace(model_terms, "percent_agricultural_", "Agricultural Extent "),
         model_terms = str_replace(model_terms, "domestic_dog_visitors_per_day", "Domestic Dog Visitation"),
         model_terms = str_replace(model_terms, "human_visitors_per_day", "Human Visitation"),
         model_terms = if_else(model_terms == "1", "Null Model", model_terms))%>% 
  #Select important variables
  dplyr::select(model_terms, AICc, DeltaAICc, AICWeight)



#convert to "gt" object for exporting publication-quality table
all_fitted_models_gt <- gt(all_fitted_models)

all_fitted_models_summary <- 
  all_fitted_models_gt |>
  tab_header(
    title = "All Fitted PERMANOVA Models"
  ) |>
  cols_label(model_terms = md("**Model terms**"),
             AICc = md("**AICc**"),
             DeltaAICc = md("**∆AICc**"),
             AICWeight = md("**AIC Weight**"))

# Show the gt Table
all_fitted_models_summary

#Export high-quality table
gtsave(all_fitted_models_summary, "output/supp_figures/PERMANOVA_all_models_table.pdf")



#################################################################
# PART 2: Multivariate Analysis using Multivariate GLM ##########
#################################################################

# PART 2A: Set up manyglm analysis --------------------------------------------

#set seed for reproducability
set.seed(99) 

# Create mvabund object composed of all of the scavenger species and their maxN values 
scav_assemblage <- mvabund(urban_scavengers_summary[,18:29])

#take a look at the abundance data
boxplot(urban_scavengers_summary[,18:29], horizontal = TRUE, las = 2, main = "Abundance")

#check mean-variance relationship
meanvar.plot(scav_assemblage)

# PART 2B: Create ManyGLM models for each of 64 models (including null model) tested in PERMANOVA approach (i.e. in the NonCollinear dataframe) -----------


f1 <- manyglm(scav_assemblage ~ urban_scavengers_summary$percent_developed_1km, 
              family = "negative_binomial", #negative binomial distribution
              offset = log(urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f2 <- manyglm(scav_assemblage ~ urban_scavengers_summary$percent_developed_3km, 
              family = "negative_binomial", #negative binomial distribution
              offset = log(urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f3 <- manyglm(scav_assemblage ~ urban_scavengers_summary$percent_developed_5km, 
              family = "negative_binomial", #negative binomial distribution
              offset = log(urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f4 <- manyglm(scav_assemblage ~ urban_scavengers_summary$percent_agricultural_1km, 
              family = "negative_binomial", #negative binomial distribution
              offset = log(urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f5 <- manyglm(scav_assemblage ~ urban_scavengers_summary$percent_agricultural_3km, 
              family = "negative_binomial", #negative binomial distribution
              offset = log(urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f6 <- manyglm(scav_assemblage ~ urban_scavengers_summary$percent_agricultural_5km, 
              family = "negative_binomial", #negative binomial distribution
              offset = log(urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f7 <- manyglm(scav_assemblage ~ urban_scavengers_summary$human_visitors_per_day, 
              family = "negative_binomial", #negative binomial distribution
              offset = log(urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f8 <- manyglm(scav_assemblage ~ urban_scavengers_summary$domestic_dog_visitors_per_day, 
              family = "negative_binomial", #negative binomial distribution
              offset = log(urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f9 <- manyglm(scav_assemblage ~ 
                urban_scavengers_summary$percent_developed_1km +
                urban_scavengers_summary$percent_agricultural_1km, 
              family = "negative_binomial", #negative binomial distribution
              offset = log(urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f10 <- manyglm(scav_assemblage ~ urban_scavengers_summary$percent_developed_1km +
                 urban_scavengers_summary$percent_agricultural_3km, 
               family = "negative_binomial", #negative binomial distribution
               offset = log(urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f11 <- manyglm(scav_assemblage ~ urban_scavengers_summary$percent_developed_1km +
                 urban_scavengers_summary$percent_agricultural_5km, 
               family = "negative_binomial", #negative binomial distribution
               offset = log(urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f12 <- manyglm(scav_assemblage ~ urban_scavengers_summary$percent_developed_1km +
                 urban_scavengers_summary$human_visitors_per_day, 
               family = "negative_binomial", #negative binomial distribution
               offset = log(urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f13 <- manyglm(scav_assemblage ~ urban_scavengers_summary$percent_developed_1km +
                 urban_scavengers_summary$domestic_dog_visitors_per_day, 
               family = "negative_binomial", #negative binomial distribution
               offset = log(urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f14 <- manyglm(scav_assemblage ~ 
                 urban_scavengers_summary$percent_developed_3km +
                 urban_scavengers_summary$percent_agricultural_1km, 
               family = "negative_binomial", #negative binomial distribution
               offset = log(urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f15 <- manyglm(scav_assemblage ~ urban_scavengers_summary$percent_developed_3km +
                 urban_scavengers_summary$percent_agricultural_3km, 
               family = "negative_binomial", #negative binomial distribution
               offset = log(urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f16 <- manyglm(scav_assemblage ~ urban_scavengers_summary$percent_developed_3km +
                 urban_scavengers_summary$percent_agricultural_5km, 
               family = "negative_binomial", #negative binomial distribution
               offset = log(urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f17 <- manyglm(scav_assemblage ~ urban_scavengers_summary$percent_developed_3km +
                 urban_scavengers_summary$human_visitors_per_day, 
               family = "negative_binomial", #negative binomial distribution
               offset = log(urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f18 <- manyglm(scav_assemblage ~ urban_scavengers_summary$percent_developed_3km +
                 urban_scavengers_summary$domestic_dog_visitors_per_day, 
               family = "negative_binomial", #negative binomial distribution
               offset = log(urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f19 <- manyglm(scav_assemblage ~ 
                 urban_scavengers_summary$percent_developed_5km +
                 urban_scavengers_summary$percent_agricultural_1km, 
               family = "negative_binomial", #negative binomial distribution
               offset = log(urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f20 <- manyglm(scav_assemblage ~ urban_scavengers_summary$percent_developed_5km +
                 urban_scavengers_summary$percent_agricultural_3km, 
               family = "negative_binomial", #negative binomial distribution
               offset = log(urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f21 <- manyglm(scav_assemblage ~ urban_scavengers_summary$percent_developed_5km +
                 urban_scavengers_summary$percent_agricultural_5km, 
               family = "negative_binomial", #negative binomial distribution
               offset = log(urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f22 <- manyglm(scav_assemblage ~ urban_scavengers_summary$percent_developed_5km +
                 urban_scavengers_summary$human_visitors_per_day, 
               family = "negative_binomial", #negative binomial distribution
               offset = log(urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f23 <- manyglm(scav_assemblage ~ urban_scavengers_summary$percent_developed_5km +
                 urban_scavengers_summary$domestic_dog_visitors_per_day, 
               family = "negative_binomial", #negative binomial distribution
               offset = log(urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f24 <- manyglm(scav_assemblage ~ urban_scavengers_summary$percent_agricultural_1km +
                 urban_scavengers_summary$human_visitors_per_day, 
               family = "negative_binomial", #negative binomial distribution
               offset = log(urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f25 <- manyglm(scav_assemblage ~ urban_scavengers_summary$percent_agricultural_1km +
                 urban_scavengers_summary$domestic_dog_visitors_per_day, 
               family = "negative_binomial", #negative binomial distribution
               offset = log(urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f26 <- manyglm(scav_assemblage ~ urban_scavengers_summary$percent_agricultural_3km +
                 urban_scavengers_summary$human_visitors_per_day, 
               family = "negative_binomial", #negative binomial distribution
               offset = log(urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f27 <- manyglm(scav_assemblage ~ urban_scavengers_summary$percent_agricultural_3km +
                 urban_scavengers_summary$domestic_dog_visitors_per_day, 
               family = "negative_binomial", #negative binomial distribution
               offset = log(urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f28 <- manyglm(scav_assemblage ~ urban_scavengers_summary$percent_agricultural_5km +
                 urban_scavengers_summary$human_visitors_per_day, 
               family = "negative_binomial", #negative binomial distribution
               offset = log(urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f29 <- manyglm(scav_assemblage ~ urban_scavengers_summary$percent_agricultural_5km +
                 urban_scavengers_summary$domestic_dog_visitors_per_day, 
               family = "negative_binomial", #negative binomial distribution
               offset = log(urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f30 <- manyglm(scav_assemblage ~ urban_scavengers_summary$human_visitors_per_day +
                 urban_scavengers_summary$domestic_dog_visitors_per_day, 
               family = "negative_binomial", #negative binomial distribution
               offset = log(urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f31 <- manyglm(scav_assemblage ~ urban_scavengers_summary$percent_developed_1km+
                 urban_scavengers_summary$percent_agricultural_1km+ 
                 urban_scavengers_summary$human_visitors_per_day, 
               family = "negative_binomial", #negative binomial distribution
               offset = log(urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f32 <- manyglm(scav_assemblage ~ urban_scavengers_summary$percent_developed_1km+
                 urban_scavengers_summary$percent_agricultural_1km+ 
                 urban_scavengers_summary$domestic_dog_visitors_per_day, 
               family = "negative_binomial", #negative binomial distribution
               offset = log(urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f33 <- manyglm(scav_assemblage ~ urban_scavengers_summary$percent_developed_1km+
                 urban_scavengers_summary$percent_agricultural_3km+ 
                 urban_scavengers_summary$human_visitors_per_day, 
               family = "negative_binomial", #negative binomial distribution
               offset = log(urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f34 <- manyglm(scav_assemblage ~ urban_scavengers_summary$percent_developed_1km+
                 urban_scavengers_summary$percent_agricultural_3km+ 
                 urban_scavengers_summary$domestic_dog_visitors_per_day, 
               family = "negative_binomial", #negative binomial distribution
               offset = log(urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f35 <- manyglm(scav_assemblage ~ urban_scavengers_summary$percent_developed_1km+
                 urban_scavengers_summary$percent_agricultural_5km+ 
                 urban_scavengers_summary$human_visitors_per_day, 
               family = "negative_binomial", #negative binomial distribution
               offset = log(urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f36 <- manyglm(scav_assemblage ~ urban_scavengers_summary$percent_developed_1km+
                 urban_scavengers_summary$percent_agricultural_5km+ 
                 urban_scavengers_summary$domestic_dog_visitors_per_day, 
               family = "negative_binomial", #negative binomial distribution
               offset = log(urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f37 <- manyglm(scav_assemblage ~ urban_scavengers_summary$percent_developed_1km+
                 urban_scavengers_summary$human_visitors_per_day+
                 urban_scavengers_summary$domestic_dog_visitors_per_day, 
               family = "negative_binomial", #negative binomial distribution
               offset = log(urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f38 <- manyglm(scav_assemblage ~ urban_scavengers_summary$percent_developed_3km+
                 urban_scavengers_summary$percent_agricultural_1km+ 
                 urban_scavengers_summary$human_visitors_per_day, 
               family = "negative_binomial", #negative binomial distribution
               offset = log(urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f39 <- manyglm(scav_assemblage ~ urban_scavengers_summary$percent_developed_3km+
                 urban_scavengers_summary$percent_agricultural_1km+ 
                 urban_scavengers_summary$domestic_dog_visitors_per_day, 
               family = "negative_binomial", #negative binomial distribution
               offset = log(urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f40 <- manyglm(scav_assemblage ~ urban_scavengers_summary$percent_developed_3km+
                 urban_scavengers_summary$percent_agricultural_3km+ 
                 urban_scavengers_summary$human_visitors_per_day, 
               family = "negative_binomial", #negative binomial distribution
               offset = log(urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f41 <- manyglm(scav_assemblage ~ urban_scavengers_summary$percent_developed_3km+
                 urban_scavengers_summary$percent_agricultural_3km+ 
                 urban_scavengers_summary$domestic_dog_visitors_per_day, 
               family = "negative_binomial", #negative binomial distribution
               offset = log(urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f42 <- manyglm(scav_assemblage ~ urban_scavengers_summary$percent_developed_3km+
                 urban_scavengers_summary$percent_agricultural_5km+ 
                 urban_scavengers_summary$human_visitors_per_day, 
               family = "negative_binomial", #negative binomial distribution
               offset = log(urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f43 <- manyglm(scav_assemblage ~ urban_scavengers_summary$percent_developed_3km+
                 urban_scavengers_summary$percent_agricultural_5km+ 
                 urban_scavengers_summary$domestic_dog_visitors_per_day, 
               family = "negative_binomial", #negative binomial distribution
               offset = log(urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f44 <- manyglm(scav_assemblage ~ urban_scavengers_summary$percent_developed_3km+
                 urban_scavengers_summary$human_visitors_per_day+
                 urban_scavengers_summary$domestic_dog_visitors_per_day, 
               family = "negative_binomial", #negative binomial distribution
               offset = log(urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f45 <- manyglm(scav_assemblage ~ urban_scavengers_summary$percent_developed_5km+
                 urban_scavengers_summary$percent_agricultural_1km+ 
                 urban_scavengers_summary$human_visitors_per_day, 
               family = "negative_binomial", #negative binomial distribution
               offset = log(urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f46 <- manyglm(scav_assemblage ~ urban_scavengers_summary$percent_developed_5km+
                 urban_scavengers_summary$percent_agricultural_1km+ 
                 urban_scavengers_summary$domestic_dog_visitors_per_day, 
               family = "negative_binomial", #negative binomial distribution
               offset = log(urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f47 <- manyglm(scav_assemblage ~ urban_scavengers_summary$percent_developed_5km+
                 urban_scavengers_summary$percent_agricultural_3km+ 
                 urban_scavengers_summary$human_visitors_per_day, 
               family = "negative_binomial", #negative binomial distribution
               offset = log(urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f48 <- manyglm(scav_assemblage ~ urban_scavengers_summary$percent_developed_5km+
                 urban_scavengers_summary$percent_agricultural_3km+ 
                 urban_scavengers_summary$domestic_dog_visitors_per_day, 
               family = "negative_binomial", #negative binomial distribution
               offset = log(urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f49 <- manyglm(scav_assemblage ~ urban_scavengers_summary$percent_developed_5km+
                 urban_scavengers_summary$percent_agricultural_5km+ 
                 urban_scavengers_summary$human_visitors_per_day, 
               family = "negative_binomial", #negative binomial distribution
               offset = log(urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f50 <- manyglm(scav_assemblage ~ urban_scavengers_summary$percent_developed_5km+
                 urban_scavengers_summary$percent_agricultural_5km+ 
                 urban_scavengers_summary$domestic_dog_visitors_per_day, 
               family = "negative_binomial", #negative binomial distribution
               offset = log(urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f51 <- manyglm(scav_assemblage ~ urban_scavengers_summary$percent_developed_5km+
                 urban_scavengers_summary$human_visitors_per_day+
                 urban_scavengers_summary$domestic_dog_visitors_per_day, 
               family = "negative_binomial", #negative binomial distribution
               offset = log(urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f52 <- manyglm(scav_assemblage ~ urban_scavengers_summary$percent_agricultural_1km+
                 urban_scavengers_summary$human_visitors_per_day+
                 urban_scavengers_summary$domestic_dog_visitors_per_day, 
               family = "negative_binomial", #negative binomial distribution
               offset = log(urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f53 <- manyglm(scav_assemblage ~ urban_scavengers_summary$percent_agricultural_3km+
                 urban_scavengers_summary$human_visitors_per_day+
                 urban_scavengers_summary$domestic_dog_visitors_per_day, 
               family = "negative_binomial", #negative binomial distribution
               offset = log(urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f54 <- manyglm(scav_assemblage ~ urban_scavengers_summary$percent_agricultural_5km+
                 urban_scavengers_summary$human_visitors_per_day+
                 urban_scavengers_summary$domestic_dog_visitors_per_day, 
               family = "negative_binomial", #negative binomial distribution
               offset = log(urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale

f55 <- manyglm(scav_assemblage ~ urban_scavengers_summary$percent_developed_1km+
                 urban_scavengers_summary$percent_agricultural_1km+
                 urban_scavengers_summary$human_visitors_per_day+
                 urban_scavengers_summary$domestic_dog_visitors_per_day, 
               family = "negative_binomial", #negative binomial distribution
               offset = log(urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f56 <- manyglm(scav_assemblage ~ urban_scavengers_summary$percent_developed_1km+
                 urban_scavengers_summary$percent_agricultural_3km+
                 urban_scavengers_summary$human_visitors_per_day+
                 urban_scavengers_summary$domestic_dog_visitors_per_day, 
               family = "negative_binomial", #negative binomial distribution
               offset = log(urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f57 <- manyglm(scav_assemblage ~ urban_scavengers_summary$percent_developed_1km+
                 urban_scavengers_summary$percent_agricultural_5km+
                 urban_scavengers_summary$human_visitors_per_day+
                 urban_scavengers_summary$domestic_dog_visitors_per_day, 
               family = "negative_binomial", #negative binomial distribution
               offset = log(urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale

f58 <- manyglm(scav_assemblage ~ urban_scavengers_summary$percent_developed_3km+
                 urban_scavengers_summary$percent_agricultural_1km+
                 urban_scavengers_summary$human_visitors_per_day+
                 urban_scavengers_summary$domestic_dog_visitors_per_day, 
               family = "negative_binomial", #negative binomial distribution
               offset = log(urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f59 <- manyglm(scav_assemblage ~ urban_scavengers_summary$percent_developed_3km+
                 urban_scavengers_summary$percent_agricultural_3km+
                 urban_scavengers_summary$human_visitors_per_day+
                 urban_scavengers_summary$domestic_dog_visitors_per_day, 
               family = "negative_binomial", #negative binomial distribution
               offset = log(urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f60 <- manyglm(scav_assemblage ~ urban_scavengers_summary$percent_developed_3km+
                 urban_scavengers_summary$percent_agricultural_5km+
                 urban_scavengers_summary$human_visitors_per_day+
                 urban_scavengers_summary$domestic_dog_visitors_per_day, 
               family = "negative_binomial", #negative binomial distribution
               offset = log(urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale


f61 <- manyglm(scav_assemblage ~ urban_scavengers_summary$percent_developed_5km+
                 urban_scavengers_summary$percent_agricultural_1km+
                 urban_scavengers_summary$human_visitors_per_day+
                 urban_scavengers_summary$domestic_dog_visitors_per_day, 
               family = "negative_binomial", #negative binomial distribution
               offset = log(urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f62 <- manyglm(scav_assemblage ~ urban_scavengers_summary$percent_developed_5km+
                 urban_scavengers_summary$percent_agricultural_3km+
                 urban_scavengers_summary$human_visitors_per_day+
                 urban_scavengers_summary$domestic_dog_visitors_per_day, 
               family = "negative_binomial", #negative binomial distribution
               offset = log(urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f63 <- manyglm(scav_assemblage ~ urban_scavengers_summary$percent_developed_5km+
                 urban_scavengers_summary$percent_agricultural_5km+
                 urban_scavengers_summary$human_visitors_per_day+
                 urban_scavengers_summary$domestic_dog_visitors_per_day, 
               family = "negative_binomial", #negative binomial distribution
               offset = log(urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
#Create null model
null <- manyglm(scav_assemblage ~ 1, 
                family = "negative_binomial", #negative binomial distribution
                offset = log(urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale



# PART 2C: Compare models using AIC --------------------------------------------

# create summary table of model suite
manyglm_summary_table <- data.frame (
  
  #Pull model terms names out from NonCollinear data frame
  model_terms  = NonCollinear$form, 
  
  #Extract residual degrees of freedom from fitted manyglm models                                    
  df = c(f1$df.residual, f2$df.residual,
         f3$df.residual, f4$df.residual,
         f5$df.residual, f6$df.residual,
         f7$df.residual, f8$df.residual,
         f9$df.residual, f10$df.residual,
         f11$df.residual, f12$df.residual,
         f13$df.residual, f14$df.residual,
         f15$df.residual, f16$df.residual,
         f17$df.residual, f18$df.residual,
         f19$df.residual, f20$df.residual,
         f21$df.residual, f22$df.residual,
         f23$df.residual, f24$df.residual,
         f25$df.residual, f26$df.residual,
         f27$df.residual, f28$df.residual,
         f29$df.residual, f30$df.residual,
         f31$df.residual, f32$df.residual,
         f33$df.residual, f34$df.residual,
         f35$df.residual, f36$df.residual,
         f37$df.residual, f38$df.residual,
         f39$df.residual, f40$df.residual,
         f41$df.residual, f42$df.residual,
         f43$df.residual, f44$df.residual,
         f45$df.residual, f46$df.residual,
         f47$df.residual, f48$df.residual,
         f49$df.residual, f50$df.residual,
         f51$df.residual, f52$df.residual,
         f53$df.residual, f54$df.residual,
         f55$df.residual, f56$df.residual,
         f57$df.residual, f58$df.residual,
         f59$df.residual, f60$df.residual,
         f61$df.residual, f62$df.residual,
         f63$df.residual, 
         null$df.residual),
  
  #Extract AIC from each ManyGLM
  AIC = c(f1$AICsum, f2$AICsum,
          f3$AICsum, f4$AICsum,
          f5$AICsum, f6$AICsum,
          f7$AICsum, f8$AICsum,
          f9$AICsum, f10$AICsum,
          f11$AICsum, f12$AICsum,
          f13$AICsum, f14$AICsum,
          f15$AICsum, f16$AICsum,
          f17$AICsum, f18$AICsum,
          f19$AICsum, f20$AICsum,
          f21$AICsum, f22$AICsum,
          f23$AICsum, f24$AICsum,
          f25$AICsum, f26$AICsum,
          f27$AICsum, f28$AICsum,
          f29$AICsum, f30$AICsum,
          f31$AICsum, f32$AICsum,
          f33$AICsum, f34$AICsum,
          f35$AICsum, f36$AICsum,
          f37$AICsum, f38$AICsum,
          f39$AICsum, f40$AICsum,
          f41$AICsum, f42$AICsum,
          f43$AICsum, f44$AICsum,
          f45$AICsum, f46$AICsum,
          f47$AICsum, f48$AICsum,
          f49$AICsum, f50$AICsum,
          f51$AICsum, f52$AICsum,
          f53$AICsum, f54$AICsum,
          f55$AICsum, f56$AICsum,
          f57$AICsum, f58$AICsum,
          f59$AICsum, f60$AICsum,
          f61$AICsum, f62$AICsum,
          f63$AICsum, 
          null$AICsum)) %>% 
  # Rename model term column to remove "Distance ~ " from each cell
  mutate(model_terms = str_replace(model_terms, "Distance ~ ", ""),
         model_terms = str_replace(model_terms, "percent_developed_", "Urbanization Extent "),
         model_terms = str_replace(model_terms, "percent_agricultural_", "Agricultural Extent "),
         model_terms = str_replace(model_terms, "domestic_dog_visitors_per_day", "Domestic Dog Visitation"),
         model_terms = str_replace(model_terms, "human_visitors_per_day", "Human Visitation"),
         model_terms = if_else(model_terms == "1", "Null Model", model_terms)) %>%
  arrange(AIC) #Rearrange from lowest to highest AIC models
  
  # Filter models for only those with delta AIC less than 2
selected_manyglm <- manyglm_summary_table %>% 
  filter(AIC <= min(AIC)+2)


# PART 2D: Take a look at the top models to see if they are a good fit and check assumptions ----------

#Top model (f13): percent_developed_1km + domestic_dog_visitors_per_day
anova.manyglm(f13, p.uni = "adjusted")
#Yes, clearly both percent_developed_1km and domestic_dog_visitors_per_day have significant multivariate effects

#Plot model to make sure no trend in residuals vs. fitted plot
plot(f13) #Nope, a cloud of points. Looks good. 

#Second top model (f12): percent_developed_1km + human_visitors_per_day
anova.manyglm(f12, p.uni = "adjusted")
#Yes, clearly both percent_developed_1km and human_visitors_per_day have significant multivariate effects

#Plot model to make sure no trend in residuals vs. fitted plot
plot(f12) #Nope, a cloud of points. Looks good. 

#Third top model f28: percent_agricultural_5km + human_visitors_per_day
anova.manyglm(f28, p.uni = "adjusted")
#This model has one  predictor term with significant multivariate effects: human_visitors_per_day

#Plot model to make sure no trend in residuals vs. fitted plot
plot(f28) #Nope, a cloud of points. Looks good. 


#Fourth top model f1: percent_developed_1km
anova.manyglm(f1, p.uni = "adjusted")
#Yes, significant multivariate effects for this single-term model

#Plot model to make sure no trend in residuals vs. fitted plot
plot(f1) #Nope, a cloud of points. Looks good. 

# PART 2E: Make publication-quality tables for export ----------------------------

# All MvGLM models table####

#convert to "gt" object for exporting publication-quality table
manyglm_summary_table_gt <- gt(manyglm_summary_table)

manyglm_summary <- 
  manyglm_summary_table_gt |>
  tab_header(
    title = "All MvGLM Models"
  ) |>
  cols_label(model_terms = md("**Model terms**"),
             df = md("**df**"),
             AIC = md("**AIC**"))

# Show the gt Table
manyglm_summary

#Export high-quality table
gtsave(manyglm_summary, "output/supp_figures/MvGLM_all_models_table.pdf")


#Selected MvGLM models table ####

#convert to "gt" object for exporting publication-quality table
selected_manyglm_gt <- gt(selected_manyglm)

selected_manyglm_summary <- 
  selected_manyglm_gt |>
  tab_header(
    title = "MvGLM Top Models Summary (∆AIC <2)"
  ) |>
  cols_label(model_terms = md("**Model terms**"),
             df = md("**df**"),
             AIC = md("**AIC**"))

# Show the gt Table
selected_manyglm_summary

#Export high-quality table
gtsave(selected_manyglm_summary, "output/supp_figures/MvGLM_summary_table.pdf")


###################################################################
# PART 3: Adjusted Multivariate Analysis using PERMANOVA ##########
###################################################################

#WARNING: The following code overwrites objects previously created in parts 1 and 2 of this script. Do not run segments of this script out of order as it may influence analytical results and summary table outputs. 

#Import dataset
adjusted_urban_scavengers_summary <- read_csv("data/processed/adjusted_urban_scavengers_summary.csv") %>% 
  filter(site_name != "Strawberry") #Remove strawberry beach from analysis b/c no scavenging assemblage


# PART 1A: Set up PERMANOVA analysis -------------------------------------------

#set seed for reproducability
set.seed(999) 

# Create object composed of all of the scavenger species and their maxN values, divided by the number of fish deployed at each site
scav_assemblage <- data.frame(adjusted_urban_scavengers_summary[,18:29]/adjusted_urban_scavengers_summary$n_fish_deployed)

#Create object of the predictor values 
predictors <- adjusted_urban_scavengers_summary[,c(4:11)]

#Generate Bray-Curtis distance matrix
distance_matrix <- vegdist(scav_assemblage, method = "bray")

# PART 1B: Generate and filter modeling suite ----------------------------------

#Generate all possible first-order models for this set of predictors, which results in 256 possible models
AllModels <- make_models(vars = c("percent_developed_1km", "percent_developed_3km", "percent_developed_5km", "percent_agricultural_1km", "percent_agricultural_3km", "percent_agricultural_5km", "human_visitors_per_day", "domestic_dog_visitors_per_day"))

#Filter out models with high degree of collinearity (having a maximum value of VIF of 5 or more), leading to 64 possible non-collinear models
NonCollinear <- filter_vif(all_forms = AllModels, env_data = predictors)

# PART 1C: Fit and summarize PERMANOVA models ----------------------------------

#Fit the 64 non-collinear PERMANOVA models - uses the vegan adonis2() function
Fitted <- fit_models(
  all_forms = NonCollinear,
  veg_data = scav_assemblage,
  env_data = predictors,
  log = FALSE,
  method = "bray")

#Filter models for those with (1) delta AICc less than 2 and (2) maximum VIF no greater than 5.
Selected <- select_models(Fitted, delta_aicc = 2)
Selected #take a look at output of 6 top models with delta AICc less than 2

#Create summarized r squared weighted by AICc
Summary <- akaike_adjusted_rsq(Selected)
Summary #take a look at summary output of each predictor's adjusted r squared calculated using AIC and model averaging

# PART 1D: Look at top models to ensure a good fit -----------------------------

#Take a look at the eleven top models to see if they fit well
top_model <- adonis2(distance_matrix ~ predictors$percent_developed_1km)
top_model_2 <- adonis2(distance_matrix ~ predictors$percent_developed_3km)
top_model_3 <- adonis2(distance_matrix ~ predictors$percent_developed_5km)
top_model_4 <- adonis2(distance_matrix ~ predictors$percent_developed_1km+predictors$percent_agricultural_3km)
top_model_5 <- adonis2(distance_matrix ~ predictors$percent_developed_3km+predictors$percent_agricultural_3km)
top_model_6 <- adonis2(distance_matrix ~ predictors$percent_developed_1km+predictors$percent_agricultural_5km)
top_model_7 <- adonis2(distance_matrix ~ predictors$percent_developed_1km+predictors$human_visitors_per_day)
top_model_8 <- adonis2(distance_matrix ~ predictors$percent_developed_5km+predictors$percent_agricultural_3km)
top_model_9 <- adonis2(distance_matrix ~ predictors$percent_developed_3km+predictors$human_visitors_per_day)
top_model_10 <- adonis2(distance_matrix ~ predictors$percent_developed_3km+predictors$percent_agricultural_5km)
top_model_11 <- adonis2(distance_matrix ~ predictors$percent_developed_1km+predictors$percent_agricultural_1km)
top_model_12 <- adonis2(distance_matrix ~ predictors$percent_developed_5km+predictors$percent_agricultural_5km)
top_model_13 <- adonis2(distance_matrix ~ predictors$percent_developed_3km+predictors$percent_agricultural_1km)
top_model_14 <- adonis2(distance_matrix ~ predictors$percent_developed_5km+predictors$human_visitors_per_day)
top_model_15 <- adonis2(distance_matrix ~ predictors$percent_developed_3km+predictors$domestic_dog_visitors_per_day)
top_model_16 <- adonis2(distance_matrix ~ predictors$percent_developed_1km+predictors$domestic_dog_visitors_per_day)

#Yes, the top three models fit well, but models 4-16 do not have significant additive terms so they are basically single-term models.
top_model; top_model_2; top_model_3; top_model_4; top_model_5; top_model_6; top_model_7; top_model_8; top_model_9; top_model_10; top_model_11; top_model_12; top_model_13; top_model_14; top_model_15; top_model_16


# PART 1E: Make publication-quality tables for export ----------------------------

#Selected models table ####
selected_models <- Selected %>% 
  #Replace characters from models for better descriptions
  mutate(model_terms = str_replace(form, "Distance ~ ", ""),
         model_terms = str_replace(model_terms, "percent_developed_", "Urbanization Extent "),
         model_terms = str_replace(model_terms, "percent_agricultural_", "Agricultural Extent "),
         model_terms = str_replace(model_terms, "percent_agricultural_", "Agricultural Extent "),
         model_terms = str_replace(model_terms, "domestic_dog_visitors_per_day", "Domestic Dog Visitation"),
         model_terms = str_replace(model_terms, "human_visitors_per_day", "Human Visitation"))%>% 
  #Select important variables
  dplyr::select(model_terms, AICc, DeltaAICc, AICWeight)


#convert to "gt" object for exporting publication-quality table
selected_models_summary_table_gt <- gt(selected_models)

selected_models_summary <- 
  selected_models_summary_table_gt |>
  tab_header(
    title = "PERMANOVA Top Models Summary (∆AICc <2) (Adjusted MaxN Values)"
  ) |>
  cols_label(model_terms = md("**Model terms**"),
             AICc = md("**AICc**"),
             DeltaAICc = md("**∆AICc**"),
             AICWeight = md("**AIC Weight**"))

# Show the gt Table
selected_models_summary

#Export high-quality table
gtsave(selected_models_summary, "output/supp_figures/adjusted_PERMANOVA_top_models_table.pdf")


#Summarized predictors table ####
summarized_predictors <- Summary %>% 
  mutate(Predictor = str_replace(Variable, "percent_developed_", "Urbanization Extent "),
         Predictor = str_replace(Predictor, "percent_agricultural_", "Agricultural Extent "),
         Predictor = str_replace(Predictor, "percent_agricultural_", "Agricultural Extent "),
         Predictor = str_replace(Predictor, "domestic_dog_visitors_per_day", "Domestic Dog Visitation"),
         Predictor = str_replace(Predictor, "human_visitors_per_day", "Human Visitation")) %>% dplyr::select(Predictor, Full_Akaike_Adjusted_RSq,Number_of_models) %>% 
  arrange(desc(Full_Akaike_Adjusted_RSq))



#convert to "gt" object for exporting publication-quality table
summarized_predictors_gt <- gt(summarized_predictors)

PERMANOVA_predictors_summary <- 
  summarized_predictors_gt |>
  tab_header(
    title = "PERMANOVA Top Predictors Summary (Adjusted MaxN Values)"
  ) |>
  cols_label(Predictor = md("**Predictor**"),
             Full_Akaike_Adjusted_RSq = md("**Aikake Adjusted Rsq**"),
             Number_of_models = md("**Number of Top Models**"))

# Show the gt Table
PERMANOVA_predictors_summary

#Export high-quality table
gtsave(PERMANOVA_predictors_summary, "output/supp_figures/adjusted_PERMANOVA_top_predictors_table.pdf")


# All fitted models table ####

all_fitted_models <- select_models(Fitted, delta_aicc = 11) %>% 
  #Replace characters from models for better descriptions
  mutate(model_terms = str_replace(form, "Distance ~ ", ""),
         model_terms = str_replace(model_terms, "percent_developed_", "Urbanization Extent "),
         model_terms = str_replace(model_terms, "percent_agricultural_", "Agricultural Extent "),
         model_terms = str_replace(model_terms, "percent_agricultural_", "Agricultural Extent "),
         model_terms = str_replace(model_terms, "domestic_dog_visitors_per_day", "Domestic Dog Visitation"),
         model_terms = str_replace(model_terms, "human_visitors_per_day", "Human Visitation"),
         model_terms = if_else(model_terms == "1", "Null Model", model_terms))%>% 
  #Select important variables
  dplyr::select(model_terms, AICc, DeltaAICc, AICWeight)



#convert to "gt" object for exporting publication-quality table
all_fitted_models_gt <- gt(all_fitted_models)

all_fitted_models_summary <- 
  all_fitted_models_gt |>
  tab_header(
    title = "All Fitted PERMANOVA Models (Adjusted MaxN Values)"
  ) |>
  cols_label(model_terms = md("**Model terms**"),
             AICc = md("**AICc**"),
             DeltaAICc = md("**∆AICc**"),
             AICWeight = md("**AIC Weight**"))

# Show the gt Table
all_fitted_models_summary

#Export high-quality table
gtsave(all_fitted_models_summary, "output/supp_figures/adjusted_PERMANOVA_all_models_table.pdf")



##########################################################################
# PART 4: Adjusted Multivariate Analysis using Multivariate GLM ##########
##########################################################################

#WARNING: The following code overwrites objects previously created in parts 1 and 2 of this script. Do not run segments of this script out of order as it may influence analytical results and summary table outputs. 

# PART 2A: Set up manyglm analysis --------------------------------------------

#set seed for reproducability
set.seed(99) 

# Create mvabund object composed of all of the scavenger species and their maxN values 
scav_assemblage <- mvabund(adjusted_urban_scavengers_summary[,18:29])

#take a look at the abundance data
boxplot(adjusted_urban_scavengers_summary[,18:29], horizontal = TRUE, las = 2, main = "Abundance")

#check mean-variance relationship
meanvar.plot(scav_assemblage)

# PART 2B: Create ManyGLM models for each of 64 models (including null model) tested in PERMANOVA approach (i.e. in the NonCollinear dataframe) -----------


f1 <- manyglm(scav_assemblage ~ adjusted_urban_scavengers_summary$percent_developed_1km, 
              family = "negative_binomial", #negative binomial distribution
              offset = log(adjusted_urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f2 <- manyglm(scav_assemblage ~ adjusted_urban_scavengers_summary$percent_developed_3km, 
              family = "negative_binomial", #negative binomial distribution
              offset = log(adjusted_urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f3 <- manyglm(scav_assemblage ~ adjusted_urban_scavengers_summary$percent_developed_5km, 
              family = "negative_binomial", #negative binomial distribution
              offset = log(adjusted_urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f4 <- manyglm(scav_assemblage ~ adjusted_urban_scavengers_summary$percent_agricultural_1km, 
              family = "negative_binomial", #negative binomial distribution
              offset = log(adjusted_urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f5 <- manyglm(scav_assemblage ~ adjusted_urban_scavengers_summary$percent_agricultural_3km, 
              family = "negative_binomial", #negative binomial distribution
              offset = log(adjusted_urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f6 <- manyglm(scav_assemblage ~ adjusted_urban_scavengers_summary$percent_agricultural_5km, 
              family = "negative_binomial", #negative binomial distribution
              offset = log(adjusted_urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f7 <- manyglm(scav_assemblage ~ adjusted_urban_scavengers_summary$human_visitors_per_day, 
              family = "negative_binomial", #negative binomial distribution
              offset = log(adjusted_urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f8 <- manyglm(scav_assemblage ~ adjusted_urban_scavengers_summary$domestic_dog_visitors_per_day, 
              family = "negative_binomial", #negative binomial distribution
              offset = log(adjusted_urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f9 <- manyglm(scav_assemblage ~ 
                adjusted_urban_scavengers_summary$percent_developed_1km +
                adjusted_urban_scavengers_summary$percent_agricultural_1km, 
              family = "negative_binomial", #negative binomial distribution
              offset = log(adjusted_urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f10 <- manyglm(scav_assemblage ~ adjusted_urban_scavengers_summary$percent_developed_1km +
                 adjusted_urban_scavengers_summary$percent_agricultural_3km, 
               family = "negative_binomial", #negative binomial distribution
               offset = log(adjusted_urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f11 <- manyglm(scav_assemblage ~ adjusted_urban_scavengers_summary$percent_developed_1km +
                 adjusted_urban_scavengers_summary$percent_agricultural_5km, 
               family = "negative_binomial", #negative binomial distribution
               offset = log(adjusted_urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f12 <- manyglm(scav_assemblage ~ adjusted_urban_scavengers_summary$percent_developed_1km +
                 adjusted_urban_scavengers_summary$human_visitors_per_day, 
               family = "negative_binomial", #negative binomial distribution
               offset = log(adjusted_urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f13 <- manyglm(scav_assemblage ~ adjusted_urban_scavengers_summary$percent_developed_1km +
                 adjusted_urban_scavengers_summary$domestic_dog_visitors_per_day, 
               family = "negative_binomial", #negative binomial distribution
               offset = log(adjusted_urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f14 <- manyglm(scav_assemblage ~ 
                 adjusted_urban_scavengers_summary$percent_developed_3km +
                 adjusted_urban_scavengers_summary$percent_agricultural_1km, 
               family = "negative_binomial", #negative binomial distribution
               offset = log(adjusted_urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f15 <- manyglm(scav_assemblage ~ adjusted_urban_scavengers_summary$percent_developed_3km +
                 adjusted_urban_scavengers_summary$percent_agricultural_3km, 
               family = "negative_binomial", #negative binomial distribution
               offset = log(adjusted_urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f16 <- manyglm(scav_assemblage ~ adjusted_urban_scavengers_summary$percent_developed_3km +
                 adjusted_urban_scavengers_summary$percent_agricultural_5km, 
               family = "negative_binomial", #negative binomial distribution
               offset = log(adjusted_urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f17 <- manyglm(scav_assemblage ~ adjusted_urban_scavengers_summary$percent_developed_3km +
                 adjusted_urban_scavengers_summary$human_visitors_per_day, 
               family = "negative_binomial", #negative binomial distribution
               offset = log(adjusted_urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f18 <- manyglm(scav_assemblage ~ adjusted_urban_scavengers_summary$percent_developed_3km +
                 adjusted_urban_scavengers_summary$domestic_dog_visitors_per_day, 
               family = "negative_binomial", #negative binomial distribution
               offset = log(adjusted_urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f19 <- manyglm(scav_assemblage ~ 
                 adjusted_urban_scavengers_summary$percent_developed_5km +
                 adjusted_urban_scavengers_summary$percent_agricultural_1km, 
               family = "negative_binomial", #negative binomial distribution
               offset = log(adjusted_urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f20 <- manyglm(scav_assemblage ~ adjusted_urban_scavengers_summary$percent_developed_5km +
                 adjusted_urban_scavengers_summary$percent_agricultural_3km, 
               family = "negative_binomial", #negative binomial distribution
               offset = log(adjusted_urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f21 <- manyglm(scav_assemblage ~ adjusted_urban_scavengers_summary$percent_developed_5km +
                 adjusted_urban_scavengers_summary$percent_agricultural_5km, 
               family = "negative_binomial", #negative binomial distribution
               offset = log(adjusted_urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f22 <- manyglm(scav_assemblage ~ adjusted_urban_scavengers_summary$percent_developed_5km +
                 adjusted_urban_scavengers_summary$human_visitors_per_day, 
               family = "negative_binomial", #negative binomial distribution
               offset = log(adjusted_urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f23 <- manyglm(scav_assemblage ~ adjusted_urban_scavengers_summary$percent_developed_5km +
                 adjusted_urban_scavengers_summary$domestic_dog_visitors_per_day, 
               family = "negative_binomial", #negative binomial distribution
               offset = log(adjusted_urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f24 <- manyglm(scav_assemblage ~ adjusted_urban_scavengers_summary$percent_agricultural_1km +
                 adjusted_urban_scavengers_summary$human_visitors_per_day, 
               family = "negative_binomial", #negative binomial distribution
               offset = log(adjusted_urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f25 <- manyglm(scav_assemblage ~ adjusted_urban_scavengers_summary$percent_agricultural_1km +
                 adjusted_urban_scavengers_summary$domestic_dog_visitors_per_day, 
               family = "negative_binomial", #negative binomial distribution
               offset = log(adjusted_urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f26 <- manyglm(scav_assemblage ~ adjusted_urban_scavengers_summary$percent_agricultural_3km +
                 adjusted_urban_scavengers_summary$human_visitors_per_day, 
               family = "negative_binomial", #negative binomial distribution
               offset = log(adjusted_urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f27 <- manyglm(scav_assemblage ~ adjusted_urban_scavengers_summary$percent_agricultural_3km +
                 adjusted_urban_scavengers_summary$domestic_dog_visitors_per_day, 
               family = "negative_binomial", #negative binomial distribution
               offset = log(adjusted_urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f28 <- manyglm(scav_assemblage ~ adjusted_urban_scavengers_summary$percent_agricultural_5km +
                 adjusted_urban_scavengers_summary$human_visitors_per_day, 
               family = "negative_binomial", #negative binomial distribution
               offset = log(adjusted_urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f29 <- manyglm(scav_assemblage ~ adjusted_urban_scavengers_summary$percent_agricultural_5km +
                 adjusted_urban_scavengers_summary$domestic_dog_visitors_per_day, 
               family = "negative_binomial", #negative binomial distribution
               offset = log(adjusted_urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f30 <- manyglm(scav_assemblage ~ adjusted_urban_scavengers_summary$human_visitors_per_day +
                 adjusted_urban_scavengers_summary$domestic_dog_visitors_per_day, 
               family = "negative_binomial", #negative binomial distribution
               offset = log(adjusted_urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f31 <- manyglm(scav_assemblage ~ adjusted_urban_scavengers_summary$percent_developed_1km+
                 adjusted_urban_scavengers_summary$percent_agricultural_1km+ 
                 adjusted_urban_scavengers_summary$human_visitors_per_day, 
               family = "negative_binomial", #negative binomial distribution
               offset = log(adjusted_urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f32 <- manyglm(scav_assemblage ~ adjusted_urban_scavengers_summary$percent_developed_1km+
                 adjusted_urban_scavengers_summary$percent_agricultural_1km+ 
                 adjusted_urban_scavengers_summary$domestic_dog_visitors_per_day, 
               family = "negative_binomial", #negative binomial distribution
               offset = log(adjusted_urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f33 <- manyglm(scav_assemblage ~ adjusted_urban_scavengers_summary$percent_developed_1km+
                 adjusted_urban_scavengers_summary$percent_agricultural_3km+ 
                 adjusted_urban_scavengers_summary$human_visitors_per_day, 
               family = "negative_binomial", #negative binomial distribution
               offset = log(adjusted_urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f34 <- manyglm(scav_assemblage ~ adjusted_urban_scavengers_summary$percent_developed_1km+
                 adjusted_urban_scavengers_summary$percent_agricultural_3km+ 
                 adjusted_urban_scavengers_summary$domestic_dog_visitors_per_day, 
               family = "negative_binomial", #negative binomial distribution
               offset = log(adjusted_urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f35 <- manyglm(scav_assemblage ~ adjusted_urban_scavengers_summary$percent_developed_1km+
                 adjusted_urban_scavengers_summary$percent_agricultural_5km+ 
                 adjusted_urban_scavengers_summary$human_visitors_per_day, 
               family = "negative_binomial", #negative binomial distribution
               offset = log(adjusted_urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f36 <- manyglm(scav_assemblage ~ adjusted_urban_scavengers_summary$percent_developed_1km+
                 adjusted_urban_scavengers_summary$percent_agricultural_5km+ 
                 adjusted_urban_scavengers_summary$domestic_dog_visitors_per_day, 
               family = "negative_binomial", #negative binomial distribution
               offset = log(adjusted_urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f37 <- manyglm(scav_assemblage ~ adjusted_urban_scavengers_summary$percent_developed_1km+
                 adjusted_urban_scavengers_summary$human_visitors_per_day+
                 adjusted_urban_scavengers_summary$domestic_dog_visitors_per_day, 
               family = "negative_binomial", #negative binomial distribution
               offset = log(adjusted_urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f38 <- manyglm(scav_assemblage ~ adjusted_urban_scavengers_summary$percent_developed_3km+
                 adjusted_urban_scavengers_summary$percent_agricultural_1km+ 
                 adjusted_urban_scavengers_summary$human_visitors_per_day, 
               family = "negative_binomial", #negative binomial distribution
               offset = log(adjusted_urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f39 <- manyglm(scav_assemblage ~ adjusted_urban_scavengers_summary$percent_developed_3km+
                 adjusted_urban_scavengers_summary$percent_agricultural_1km+ 
                 adjusted_urban_scavengers_summary$domestic_dog_visitors_per_day, 
               family = "negative_binomial", #negative binomial distribution
               offset = log(adjusted_urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f40 <- manyglm(scav_assemblage ~ adjusted_urban_scavengers_summary$percent_developed_3km+
                 adjusted_urban_scavengers_summary$percent_agricultural_3km+ 
                 adjusted_urban_scavengers_summary$human_visitors_per_day, 
               family = "negative_binomial", #negative binomial distribution
               offset = log(adjusted_urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f41 <- manyglm(scav_assemblage ~ adjusted_urban_scavengers_summary$percent_developed_3km+
                 adjusted_urban_scavengers_summary$percent_agricultural_3km+ 
                 adjusted_urban_scavengers_summary$domestic_dog_visitors_per_day, 
               family = "negative_binomial", #negative binomial distribution
               offset = log(adjusted_urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f42 <- manyglm(scav_assemblage ~ adjusted_urban_scavengers_summary$percent_developed_3km+
                 adjusted_urban_scavengers_summary$percent_agricultural_5km+ 
                 adjusted_urban_scavengers_summary$human_visitors_per_day, 
               family = "negative_binomial", #negative binomial distribution
               offset = log(adjusted_urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f43 <- manyglm(scav_assemblage ~ adjusted_urban_scavengers_summary$percent_developed_3km+
                 adjusted_urban_scavengers_summary$percent_agricultural_5km+ 
                 adjusted_urban_scavengers_summary$domestic_dog_visitors_per_day, 
               family = "negative_binomial", #negative binomial distribution
               offset = log(adjusted_urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f44 <- manyglm(scav_assemblage ~ adjusted_urban_scavengers_summary$percent_developed_3km+
                 adjusted_urban_scavengers_summary$human_visitors_per_day+
                 adjusted_urban_scavengers_summary$domestic_dog_visitors_per_day, 
               family = "negative_binomial", #negative binomial distribution
               offset = log(adjusted_urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f45 <- manyglm(scav_assemblage ~ adjusted_urban_scavengers_summary$percent_developed_5km+
                 adjusted_urban_scavengers_summary$percent_agricultural_1km+ 
                 adjusted_urban_scavengers_summary$human_visitors_per_day, 
               family = "negative_binomial", #negative binomial distribution
               offset = log(adjusted_urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f46 <- manyglm(scav_assemblage ~ adjusted_urban_scavengers_summary$percent_developed_5km+
                 adjusted_urban_scavengers_summary$percent_agricultural_1km+ 
                 adjusted_urban_scavengers_summary$domestic_dog_visitors_per_day, 
               family = "negative_binomial", #negative binomial distribution
               offset = log(adjusted_urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f47 <- manyglm(scav_assemblage ~ adjusted_urban_scavengers_summary$percent_developed_5km+
                 adjusted_urban_scavengers_summary$percent_agricultural_3km+ 
                 adjusted_urban_scavengers_summary$human_visitors_per_day, 
               family = "negative_binomial", #negative binomial distribution
               offset = log(adjusted_urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f48 <- manyglm(scav_assemblage ~ adjusted_urban_scavengers_summary$percent_developed_5km+
                 adjusted_urban_scavengers_summary$percent_agricultural_3km+ 
                 adjusted_urban_scavengers_summary$domestic_dog_visitors_per_day, 
               family = "negative_binomial", #negative binomial distribution
               offset = log(adjusted_urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f49 <- manyglm(scav_assemblage ~ adjusted_urban_scavengers_summary$percent_developed_5km+
                 adjusted_urban_scavengers_summary$percent_agricultural_5km+ 
                 adjusted_urban_scavengers_summary$human_visitors_per_day, 
               family = "negative_binomial", #negative binomial distribution
               offset = log(adjusted_urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f50 <- manyglm(scav_assemblage ~ adjusted_urban_scavengers_summary$percent_developed_5km+
                 adjusted_urban_scavengers_summary$percent_agricultural_5km+ 
                 adjusted_urban_scavengers_summary$domestic_dog_visitors_per_day, 
               family = "negative_binomial", #negative binomial distribution
               offset = log(adjusted_urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f51 <- manyglm(scav_assemblage ~ adjusted_urban_scavengers_summary$percent_developed_5km+
                 adjusted_urban_scavengers_summary$human_visitors_per_day+
                 adjusted_urban_scavengers_summary$domestic_dog_visitors_per_day, 
               family = "negative_binomial", #negative binomial distribution
               offset = log(adjusted_urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f52 <- manyglm(scav_assemblage ~ adjusted_urban_scavengers_summary$percent_agricultural_1km+
                 adjusted_urban_scavengers_summary$human_visitors_per_day+
                 adjusted_urban_scavengers_summary$domestic_dog_visitors_per_day, 
               family = "negative_binomial", #negative binomial distribution
               offset = log(adjusted_urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f53 <- manyglm(scav_assemblage ~ adjusted_urban_scavengers_summary$percent_agricultural_3km+
                 adjusted_urban_scavengers_summary$human_visitors_per_day+
                 adjusted_urban_scavengers_summary$domestic_dog_visitors_per_day, 
               family = "negative_binomial", #negative binomial distribution
               offset = log(adjusted_urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f54 <- manyglm(scav_assemblage ~ adjusted_urban_scavengers_summary$percent_agricultural_5km+
                 adjusted_urban_scavengers_summary$human_visitors_per_day+
                 adjusted_urban_scavengers_summary$domestic_dog_visitors_per_day, 
               family = "negative_binomial", #negative binomial distribution
               offset = log(adjusted_urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale

f55 <- manyglm(scav_assemblage ~ adjusted_urban_scavengers_summary$percent_developed_1km+
                 adjusted_urban_scavengers_summary$percent_agricultural_1km+
                 adjusted_urban_scavengers_summary$human_visitors_per_day+
                 adjusted_urban_scavengers_summary$domestic_dog_visitors_per_day, 
               family = "negative_binomial", #negative binomial distribution
               offset = log(adjusted_urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f56 <- manyglm(scav_assemblage ~ adjusted_urban_scavengers_summary$percent_developed_1km+
                 adjusted_urban_scavengers_summary$percent_agricultural_3km+
                 adjusted_urban_scavengers_summary$human_visitors_per_day+
                 adjusted_urban_scavengers_summary$domestic_dog_visitors_per_day, 
               family = "negative_binomial", #negative binomial distribution
               offset = log(adjusted_urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f57 <- manyglm(scav_assemblage ~ adjusted_urban_scavengers_summary$percent_developed_1km+
                 adjusted_urban_scavengers_summary$percent_agricultural_5km+
                 adjusted_urban_scavengers_summary$human_visitors_per_day+
                 adjusted_urban_scavengers_summary$domestic_dog_visitors_per_day, 
               family = "negative_binomial", #negative binomial distribution
               offset = log(adjusted_urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale

f58 <- manyglm(scav_assemblage ~ adjusted_urban_scavengers_summary$percent_developed_3km+
                 adjusted_urban_scavengers_summary$percent_agricultural_1km+
                 adjusted_urban_scavengers_summary$human_visitors_per_day+
                 adjusted_urban_scavengers_summary$domestic_dog_visitors_per_day, 
               family = "negative_binomial", #negative binomial distribution
               offset = log(adjusted_urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f59 <- manyglm(scav_assemblage ~ adjusted_urban_scavengers_summary$percent_developed_3km+
                 adjusted_urban_scavengers_summary$percent_agricultural_3km+
                 adjusted_urban_scavengers_summary$human_visitors_per_day+
                 adjusted_urban_scavengers_summary$domestic_dog_visitors_per_day, 
               family = "negative_binomial", #negative binomial distribution
               offset = log(adjusted_urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f60 <- manyglm(scav_assemblage ~ adjusted_urban_scavengers_summary$percent_developed_3km+
                 adjusted_urban_scavengers_summary$percent_agricultural_5km+
                 adjusted_urban_scavengers_summary$human_visitors_per_day+
                 adjusted_urban_scavengers_summary$domestic_dog_visitors_per_day, 
               family = "negative_binomial", #negative binomial distribution
               offset = log(adjusted_urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale


f61 <- manyglm(scav_assemblage ~ adjusted_urban_scavengers_summary$percent_developed_5km+
                 adjusted_urban_scavengers_summary$percent_agricultural_1km+
                 adjusted_urban_scavengers_summary$human_visitors_per_day+
                 adjusted_urban_scavengers_summary$domestic_dog_visitors_per_day, 
               family = "negative_binomial", #negative binomial distribution
               offset = log(adjusted_urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f62 <- manyglm(scav_assemblage ~ adjusted_urban_scavengers_summary$percent_developed_5km+
                 adjusted_urban_scavengers_summary$percent_agricultural_3km+
                 adjusted_urban_scavengers_summary$human_visitors_per_day+
                 adjusted_urban_scavengers_summary$domestic_dog_visitors_per_day, 
               family = "negative_binomial", #negative binomial distribution
               offset = log(adjusted_urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
f63 <- manyglm(scav_assemblage ~ adjusted_urban_scavengers_summary$percent_developed_5km+
                 adjusted_urban_scavengers_summary$percent_agricultural_5km+
                 adjusted_urban_scavengers_summary$human_visitors_per_day+
                 adjusted_urban_scavengers_summary$domestic_dog_visitors_per_day, 
               family = "negative_binomial", #negative binomial distribution
               offset = log(adjusted_urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale
#Create null model
null <- manyglm(scav_assemblage ~ 1, 
                family = "negative_binomial", #negative binomial distribution
                offset = log(adjusted_urban_scavengers_summary$n_fish_deployed)) #offset on link (log) scale



# PART 2C: Compare models using AIC --------------------------------------------

# create summary table of model suite
manyglm_summary_table <- data.frame (
  
  #Pull model terms names out from NonCollinear data frame
  model_terms  = NonCollinear$form, 
  
  #Extract residual degrees of freedom from fitted manyglm models                                    
  df = c(f1$df.residual, f2$df.residual,
         f3$df.residual, f4$df.residual,
         f5$df.residual, f6$df.residual,
         f7$df.residual, f8$df.residual,
         f9$df.residual, f10$df.residual,
         f11$df.residual, f12$df.residual,
         f13$df.residual, f14$df.residual,
         f15$df.residual, f16$df.residual,
         f17$df.residual, f18$df.residual,
         f19$df.residual, f20$df.residual,
         f21$df.residual, f22$df.residual,
         f23$df.residual, f24$df.residual,
         f25$df.residual, f26$df.residual,
         f27$df.residual, f28$df.residual,
         f29$df.residual, f30$df.residual,
         f31$df.residual, f32$df.residual,
         f33$df.residual, f34$df.residual,
         f35$df.residual, f36$df.residual,
         f37$df.residual, f38$df.residual,
         f39$df.residual, f40$df.residual,
         f41$df.residual, f42$df.residual,
         f43$df.residual, f44$df.residual,
         f45$df.residual, f46$df.residual,
         f47$df.residual, f48$df.residual,
         f49$df.residual, f50$df.residual,
         f51$df.residual, f52$df.residual,
         f53$df.residual, f54$df.residual,
         f55$df.residual, f56$df.residual,
         f57$df.residual, f58$df.residual,
         f59$df.residual, f60$df.residual,
         f61$df.residual, f62$df.residual,
         f63$df.residual, 
         null$df.residual),
  
  #Extract AIC from each ManyGLM
  AIC = c(f1$AICsum, f2$AICsum,
          f3$AICsum, f4$AICsum,
          f5$AICsum, f6$AICsum,
          f7$AICsum, f8$AICsum,
          f9$AICsum, f10$AICsum,
          f11$AICsum, f12$AICsum,
          f13$AICsum, f14$AICsum,
          f15$AICsum, f16$AICsum,
          f17$AICsum, f18$AICsum,
          f19$AICsum, f20$AICsum,
          f21$AICsum, f22$AICsum,
          f23$AICsum, f24$AICsum,
          f25$AICsum, f26$AICsum,
          f27$AICsum, f28$AICsum,
          f29$AICsum, f30$AICsum,
          f31$AICsum, f32$AICsum,
          f33$AICsum, f34$AICsum,
          f35$AICsum, f36$AICsum,
          f37$AICsum, f38$AICsum,
          f39$AICsum, f40$AICsum,
          f41$AICsum, f42$AICsum,
          f43$AICsum, f44$AICsum,
          f45$AICsum, f46$AICsum,
          f47$AICsum, f48$AICsum,
          f49$AICsum, f50$AICsum,
          f51$AICsum, f52$AICsum,
          f53$AICsum, f54$AICsum,
          f55$AICsum, f56$AICsum,
          f57$AICsum, f58$AICsum,
          f59$AICsum, f60$AICsum,
          f61$AICsum, f62$AICsum,
          f63$AICsum, 
          null$AICsum)) %>% 
  # Rename model term column to remove "Distance ~ " from each cell
  mutate(model_terms = str_replace(model_terms, "Distance ~ ", ""),
         model_terms = str_replace(model_terms, "percent_developed_", "Urbanization Extent "),
         model_terms = str_replace(model_terms, "percent_agricultural_", "Agricultural Extent "),
         model_terms = str_replace(model_terms, "domestic_dog_visitors_per_day", "Domestic Dog Visitation"),
         model_terms = str_replace(model_terms, "human_visitors_per_day", "Human Visitation"),
         model_terms = if_else(model_terms == "1", "Null Model", model_terms)) %>%
  arrange(AIC) #Rearrange from lowest to highest AIC models

# Filter models for only those with delta AIC less than 2
selected_manyglm <- manyglm_summary_table %>% 
  filter(AIC <= min(AIC)+2)


# PART 2D: Take a look at the top models to see if they are a good fit and check assumptions ----------

#Top model (f12): percent_developed_1km + human_visitors_per_day
anova.manyglm(f12, p.uni = "adjusted")
#Yes, clearly both percent_developed_1km and human_visitors_per_day have significant multivariate effects

#Plot model to make sure no trend in residuals vs. fitted plot
plot(f12) #Nope, a cloud of points. Looks good. 


# PART 2E: Make publication-quality tables for export ----------------------------

# All MvGLM models table####

#convert to "gt" object for exporting publication-quality table
manyglm_summary_table_gt <- gt(manyglm_summary_table)

manyglm_summary <- 
  manyglm_summary_table_gt |>
  tab_header(
    title = "All MvGLM Models (Adjusted MaxN Values)"
  ) |>
  cols_label(model_terms = md("**Model terms**"),
             df = md("**df**"),
             AIC = md("**AIC**"))

# Show the gt Table
manyglm_summary

#Export high-quality table
gtsave(manyglm_summary, "output/supp_figures/adjusted_MvGLM_all_models_table.pdf")


#Selected MvGLM models table ####

#convert to "gt" object for exporting publication-quality table
selected_manyglm_gt <- gt(selected_manyglm)

selected_manyglm_summary <- 
  selected_manyglm_gt |>
  tab_header(
    title = "MvGLM Top Models Summary (∆AIC <2) (Adjusted MaxN Values)"
  ) |>
  cols_label(model_terms = md("**Model terms**"),
             df = md("**df**"),
             AIC = md("**AIC**"))

# Show the gt Table
selected_manyglm_summary

#Export high-quality table
gtsave(selected_manyglm_summary, "output/supp_figures/adjusted_MvGLM_summary_table.pdf")

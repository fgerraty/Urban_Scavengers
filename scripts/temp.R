
#Recreate model h1g from "04_Univariate_Analyses.R", analyzing the probability of carcass removal using using 1km buffer with a mixed-effects logistic regression (generalized linear mixed effects model with binomial distribution and logit link)
h1g <- glmer(scav_prob~ #scav_prob (1/0) binary probability of any scavenging activity
               percent_agricultural_1km + #1km urbanization extent
               domestic_dog_visitors_per_day +
               (1|site_name), #site as random effect
             family = "binomial", #binomial distribution
             data=carcass_level_summary);summary(h1g) 


  #Create dataframe for generating a line and error bar representing model g2
  h1g_plot=data.frame(domestic_dog_visitors_per_day=rep(0:25,6), 
                      percent_agricultural_1km = rep(c(0,.1,.2,.3,.4,.5), each = 26),
                      site_name = "Blacks") 
  
  
  
  
  
  #predict probability of scavenging using the model
  h1g_plot$scav_prob <- predict(h1g,
                               newdata=h1g_plot,
                               type="response", 
                               re.form=NA)#added extra step for mixed effects models
  
  #Code for generating confidence intervals for GLMER model with a logit link. 
  
  #predict mean values on link/logit scale
  h1g_plot$pred_scav_prob_link=predict(h1g,newdata=h1g_plot,re.form=NA,type="link")
  #function for bootstrapping
  pf1 = function(fit) {   predict(fit, h1g_plot) } 
  #bootstrap to estimate uncertainty in predictions
  bb=bootMer(h1g,nsim=1000,FUN=pf1,seed=999) 
  #Calculate SEs from bootstrap samples on link scale
  h1g_plot$SE=apply(bb$t, 2, sd) 
  #predicted mean + 1 SE on response scale
  h1g_plot$pSE=plogis(h1g_plot$pred_scav_prob_link+h1g_plot$SE) 
  # predicted mean - 1 SE on response scale
  h1g_plot$mSE=plogis(h1g_plot$pred_scav_prob_link-h1g_plot$SE) 
  
  
  
  
  h1g_plot <- h1g_plot %>% 
    mutate(percent_agricultural_1km = factor(percent_agricultural_1km))
  
  carcass_level_summary <- carcass_level_summary %>% 
    mutate(percent_agricultural_1km = factor(percent_agricultural_1km))
  
  
  #Generate ggplot
  temp <- ggplot(carcass_level_summary, 
                 aes(x=domestic_dog_visitors_per_day, y=scav_prob))+ 
    geom_jitter(size = 4, height = 0.03, alpha = .4, shape = 16)+
    geom_ribbon(data=h1g_plot,
                aes(x=domestic_dog_visitors_per_day,
                    ymin=mSE,ymax=pSE, fill = percent_agricultural_1km),
                alpha=0.1,linetype=0)+
    geom_line(data=h1g_plot,aes(x=domestic_dog_visitors_per_day,y=scav_prob, color = percent_agricultural_1km))+
    theme_few()+
#    labs(y = "Probability of Complete Carcass Removal", x="")+
#    theme(axis.title.y = element_text(size = 20))+
    scale_color_viridis_d(labels = c(0,10,20,30,40,50))+
    scale_fill_viridis_d(guide = "none")
    
  
  
  temp

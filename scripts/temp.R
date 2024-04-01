

mod1 <- glmer(scav_prob~ #scav_prob (1/0) binary probability of any scavenging activity
               percent_agricultural_1km +
               (1|site_name), #site as random effect
             family = "binomial", #binomial distribution
             data=carcass_level_summary);summary(mod1) 

# Check h1b assumptions with DHARMa package
mod1_res = simulateResiduals(mod1)
plot(mod1_res, rank = T)
testDispersion(mod1_res)
plotResiduals(mod1_res, carcass_level_summary$site_name, xlab = "Site", main=NULL)


mod1_resids <- resid(mod1)


mod2 <-glmer(percent_agricultural_1km~ #scav_prob (1/0) binary probability of any scavenging activity
               domestic_dog_visitors_per_day +
                       (1|site_name), #site as random effect
                     family = "binomial", #binomial distribution
                     data=carcass_level_summary);summary(mod2) 

mod2_resids <- resid(mod2)


df <- data.frame(mod1_resids, mod2_resids)

ggplot(df, aes(x=mod2_resids, y=mod1_resids))+
 #        geom_jitter(width = 1, height = .15)+
  geom_point()


  geom_smooth(method = "glm")




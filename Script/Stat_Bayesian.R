# Bayesian approach
library("brms")
library("mice")
library("car")
library("lme4")
library("tidyverse")

# ==============================================================================
# Daily travel distance
# Histogram 
# Distribution

hist(Data_move1$Distance)
hist(Data_move1$Pland)
hist(Data_move1$Pd)
hist(Data_move1$Enn_mn)
hist(Data_move1$Connect)
hist(Data_move1$Clumpy)
hist(Data_move1$Contag)

# Outliers 
Data_move2_dist <- Data_move1 %>% filter(Contag > 10)

na_count_dist <- sapply(Data_move2_dist, function(x) sum(is.na(x)))
print(na_count_dist)

dim(Data_move3_dist)

Data_move3_dist <-  subset(Data_move2_dist, !is.na(Distance))

vif(lmer(Distance ~ scale(Pland) + scale(Pd) + scale(Enn_mn) + scale(Area_mn) +
           scale(Connect) + scale(Contag) + scale(Clumpy) + (1|Koala_id), 
         data = Data_move3_dist))


vif(lmer(Distance ~ Pland + Pd + Connect + Contag + Enn_mn + (1|Koala_id), 
         data = Data_move3_dist))

dim(Data_move3_dist)

# Get ideas for priors
get_prior(Distance ~ Pland + Pd + 
            Connect + Contag + Enn_mn + (1|Koala_id),
          data = Data_move3_dist,
          family = student())



model_dist_brm <- brm(Distance ~ Pland + Pd + Enn_mn +
                        Connect + Contag + (1|Koala_id),
                    data = Data_move3_dist,
                    family = student(), # or use a different family if appropriate
                    prior = c(
                      set_prior("normal(0, 5)", class = "b"),  # Priors for fixed effects
                      set_prior("student_t(3, 0, 71.1)", class = "sd")),  # Prior for random effects
                    chains = 4, # Number of chains
                    iter = 10000, # Number of iterations per chain
                    warmup = 1000, # Number of warmup iterations
                    control = list(adapt_delta = 0.95) # Controls for the sampling
)

summary(model_dist_brm)

plot(model_dist_brm)

plot(conditional_effects(model_dist_brm), points = getOption("brms.plot_points", TRUE), # Plotting with points
     )

# Extract the data for plotting from the brms model
effects_dist <- conditional_effects(model_dist_brm)

# Plotting variables
# Extract a specific effect (e.g., the first effect)
# Pland 

effects_dist_pland <- effects_dist[[1]]  # You can access other effects by changing the index

plotDist_pland <- ggplot(effects_dist_pland, aes(x = Pland, y = estimate__)) +
  geom_line(col = "red", size = 0.75) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), fill = "red", alpha = 0.1) +
  geom_point(data = Data_move3_dist, aes(x = Pland, y = Distance),
             color = "blue", pch = 18, cex = 1, alpha = 0.45) +  
  labs(title = "", x = "Proportion of tree cover (%)", y = "Distance (m)") +
  theme_bw()

# Pd 
effects_dist_pd <- effects_dist[[2]]  # You can access other effects by changing the index

plotDist_pd <- ggplot(effects_dist_pd, aes(x = Pd, y = estimate__)) +
  geom_line(col = "red", size = 0.75) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), fill = "red", alpha = 0.1) +
  geom_point(data = Data_move3_dist, aes(x = Pd, y = Distance),
             color = "blue", pch = 18, cex = 1, alpha = 0.45) +  
  labs(title = "", x = "Patch density (per ha)", y = "Distance (m)") +
  theme_bw()


# Connect 
effects_dist_connect <- effects_dist[[4]]  # You can access other effects by changing the index

plotDist_connect <- ggplot(effects_dist_connect, aes(x = Connect, y = estimate__)) +
  geom_line(col = "red", size = 0.75) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), fill = "red", alpha = 0.1) +
  
  geom_point(data = Data_move3_dist, aes(x = Connect, y = Distance),
             color = "blue", pch = 18, cex = 1, alpha = 0.45) +  
  labs(title = "", x = "Connectivity", y = "Distance (m)") +
  theme_bw()


# ==============================================================================

?brm
str(Data_move1)
hist(Data_move1$Home_range)
Data_move2_hr <- Data_move1 %>% filter(Home_range < 500 & Contag > 10 & Enn_mn < 400)
dim(Data_move2_hr)

hist(Data_move2_hr$Enn_mn)

hist(Data_move2_hr$Pland)
hist(Data_move2_hr$Pd)
hist(Data_move2_hr$Area_mn)
hist(Data_move2_hr$Connect)
hist(Data_move2_hr$Clumpy)
hist(Data_move2_hr$Contag)

na_count_hr <- sapply(Data_move2_hr, function(x) sum(is.na(x)))
print(na_count_hr)

# Multicollinearity
vif(lmer(Distance ~ scale(Pland) + scale(Pd) + scale(Enn_mn) + scale(Area_mn) +
           scale(Connect) + scale(Contag) + scale(Clumpy) + (1|Koala_id), 
         data = Data_move2_hr))

vif(lmer(Home_range ~ scale(Pland) + scale(Pd) + 
           scale(Connect) + scale(Contag) + (1|Koala_id),
         data = Data_move2_hr))


# Get ideas for priors
get_prior(Home_range ~ Pland + Pd + 
            Connect + Contag + (1|Koala_id),
          data = Data_move2_hr,
          family = student())


# Fitting the model
model_hr_brm <- brm(Home_range ~ Pland + Pd + 
                      Connect + Contag + (1|Koala_id),
                    data = Data_move2_hr,
                    family = student(), # or use a different family if appropriate
                    prior = c(
                      set_prior("normal(0, 5)", class = "b"),  # Priors for fixed effects
                      set_prior("student_t(3, 0, 25.5)", class = "sd")  # Prior for random effects
                    ),
                    chains = 4, # Number of chains
                    iter = 10000, # Number of iterations per chain
                    warmup = 1000, # Number of warmup iterations
                    control = list(adapt_delta = 0.95)) # Controls for the sampling

summary(model_hr_brm)

plot(model_hr_brm)

plot(conditional_effects(model_dist_brm), points = getOption("brms.plot_points", TRUE), # Plotting with points
)

stanplot(model_hr_brm, type = "trace", pars = c("b_Pd"))

# Plotting variables
# Extract the data for plotting from the brms model
effects_hr <- conditional_effects(model_hr_brm)

# Plotting variables
# Extract a specific effect (e.g., the first effect)
# Pland 

effects_hr_pland <- effects_hr[[1]]  # You can access other effects by changing the index

plotHR_pland <- ggplot(effects_hr_pland, aes(x = Pland, y = estimate__)) +
  geom_line(col = "red", size = 0.75) +
    geom_ribbon(aes(ymin = lower__, ymax = upper__), fill = "red", alpha = 0.1) +
  
  geom_point(data = Data_move2_hr, aes(x = Pland, y = Home_range),
             color = "blue", pch = 18, cex = 1, alpha = 0.45) +  
  labs(title = "", x = "Proportion of tree cover (%)", y = "Home range (ha)") +
  theme_bw()

# Pd 
effects_hr_pd <- effects_hr[[2]]  # You can access other effects by changing the index

plotHR_pd <- ggplot(effects_hr_pd, aes(x = Pd, y = estimate__)) +
  geom_line(col = "red", size = 0.75) +
    geom_ribbon(aes(ymin = lower__, ymax = upper__), fill = "red", alpha = 0.1) +
  geom_point(data = Data_move2_hr, aes(x = Pd, y = Home_range),
             color = "blue", pch = 18, cex = 1, alpha = 0.45) +  
  labs(title = "", x = "Patch density (per ha)", y = "Home range (ha)") +
  theme_bw()


# Connect 
effects_hr_connect <- effects_hr[[3]]  # You can access other effects by changing the index

plotHR_connect <- ggplot(effects_hr_connect, aes(x = Connect, y = estimate__)) +
  geom_line(col = "red", size = 0.75) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), fill = "red", alpha = 0.1) +
  
  geom_point(data = Data_move2_hr, aes(x = Connect, y = Home_range),
             color = "blue", pch = 18, cex = 1, alpha = 0.45) +  
  labs(title = "", x = "Connectivity", y = "Home range (ha)") +
  theme_bw()



# # ==============================================================================
# Plotting four significant variables
library(cowplot)

sig_plots <- plot_grid(plotDist_pd, plotDist_connect, plotDist_pland,
                       plotHR_pd, plotHR_connect, plotHR_pland,
                       ncol = 3, nrow = 2,
                       labels = c('A.', 'B.', 'C.', 'D.', 'E.', 'F'),
                       label_size = 10)

ggsave(filename = "./Output/EffectPlots.jpg", plot = sig_plots, 
       height = 5, width = 7.5, dpi = 300)

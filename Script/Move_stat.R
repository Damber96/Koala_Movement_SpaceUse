# Load data
Data_move <- read.csv("./Data/Result_movement_frag_18Sep2024.csv") 
str(Data_move)

# Note: frag metrics with no value in cell equals to N/A in the given data

# ------------------------------------------------------------------------------
# Checking correlation between frag metrics

# Proportion of the targeted patch types in the focal ara
Pland # : 0 ≦ CPLAND < 100 CPLAND approaches 0 when core area of the patch becomes increasingly rare in the landscape,
# because of increasing smaller patches and/or more convoluted patch shapes. 
# PLAND approaches 100 when the entire landscape consists of a single patch type

# Patch density
Pd # Number of patches per ha

cor.test(Data_move$Pland, Data_move$Pd)

# Shape metrics (I will go with Pafrac only)
cor.test(Data_move$Frac_mn, Data_move$Para_mn)
cor.test(Data_move$Frac_mn, Data_move$Pafrac)
cor.test(Data_move$Para_mn, Data_move$Pafrac) # High -ve correlation

cor.test(Data_move$Pland, Data_move$Pd)


Pafrac # 1 ≦ PAFRAC ≦ 2 A fractal dimension greater than 1 findicates an increase in patch shape complexity.
# PAFRAC approaches 1 for shapes with very simple perimeters such as squares,
# and approaches 2 for shapes with highly convoluted, plane-filling perimeters. 
# PAFRAC is undefined and reported as "N/A" if all patches are the same size or there is < 10 patches.

# Euclidean distance to the nearest neighbouring patch (same class)
ENN_mn # >0, but reported as N/A if the patch has no neighbours

# Aggregation metrics
Clumpy # Clumpiness index (-1 to 1: -1 disagreegated, 0 random, and 1 aggregated)

# Connectance metric
Connect # Connectance index (0 to 100: 0 when focal area consists of a single patch or none patches are connected,
# and, 100 when every patches are connected)

Area_mn #Mean patch size of a class, eg., woody patches

cor.test(Data_move$Clumpy, Data_move$Connect)
names(Data_move)

 # LMM
library(lme4)
library(car)
library(MuMIn)
library(tidyverse)
library(ggplot2)
# update.packages()

?lmer

str(Data_move)

Data_move_noAnn <- Data_move %>% filter(!Season == "Annual")
unique(Data_move$Koala_id)

View(Data_move)

# I want to omit some koalas which do not cover both seasons from the Annual season
Data_move1 <- Data_move %>%
  filter(!(Koala_id %in% c("Brook", "Buddha", "Canning",
                           "Creek", "Forrest", "Geronimo",
                           "Hero", "Holland", "Kathy",
                           "Lana", "Lily", "Millie",
                           "Murray", "Oscar", "Sneaky",
                           "Speedy", "Wilga", "Wilga") & Season == "Annual"))


# Summary statistics of fragmentation
summary(Data_move1)


unique(Data_move1$Season)
dim(Data_move1)
                      
hist(Data_move1$Age)


# Outliers and distribution
Data_move2_hr <- Data_move1 %>% filter(Home_range < 500 & Enn_mn < 400 & Contag > 10)
dim(Data_move2_hr)

hist(Data_move2$Home_range)

hist(Data_move2$Pland)
hist(Data_move2$Pd)
hist(Data_move2$Enn_mn)
hist(Data_move2$Connect)
hist(Data_move2$Clumpy)
hist(Data_move2$Contag)

Data_move3_hr <-  subset(Data_move2_hr, !is.na(Enn_mn) & !is.na(Clumpy))
dim(Data_move4)

Data_move4_hr <-  subset(Data_move2_hr, !is.na(Enn_mn))

# Collinearity 
vif(lmer(Home_range ~ scale(Pland) + scale(Pd) + scale(Enn_mn) + scale(Area_mn) +
           scale(Clumpy) + scale(Connect) + scale(Contag) + (1|Koala_id),
         data = Data_move3))

vif(lmer(Home_range ~ scale(Pland) + scale(Pd) + scale(Enn_mn) + 
           scale(Connect) + scale(Contag) + (1|Koala_id), # Omitting Clumpy and Area_mn
         data = Data_move4_hr))


hist(Data_move4$Home_range)



colSums(is.na(Data_move)) # Checking if variables hava NA


model_hr <- lmer(Home_range ~ scale(Pland) + scale(Pd) + scale(Enn_mn) + 
                   scale(Connect) + scale(Contag) + (1|Koala_id),
                 data = Data_move4_hr)


options(na.action = "na.fail")
dredged_hr <- dredge(model_hr)
print(dredged_hr)

Best_hr <- get.models(dredged_hr, 1)[[1]]
summary(Best_hr)
library(sjPlot)
tab_model(Best_hr, show.re.var = T)

# Plot significant variables
plotHR_pd <- plot_model(Best_hr, type = "eff", terms = c("Pd")) +
 geom_point(data = Data_move3, aes(x = Pd, y = Home_range),
    color = "blue", pch = 18, cex = 1.25, alpha = 0.5) +
  ggtitle("") +
  theme_bw() +
  labs(x = "Patch density (per ha)", y = "Home range (ha)")
 # scale_y_continuous(limits = c(0, 400))  # Specify your desired range here

plotHR_connect <- plot_model(Best_hr, type = "eff", terms = c("Connect")) +
  geom_point(data = Data_move3, aes(x = Connect, y = Home_range),
      color = "blue", pch = 18, cex = 1, alpha = 0.45) +
  ggtitle("") +
  theme_bw() +
  labs(x = "Connectivity", y = "") 
View(Data_move3)


plot_model(Best_hr, type = "eff", terms = c("Pland")) +
  geom_point(data = Data_move2, aes(x = Pd, y = Home_range),
             color = "blue", pch = 18, cex = 1.25, alpha = 0.5) +
  ggtitle("") +
  theme_bw() +
  labs(x = "Pland", y = "Home range (ha)")
# Marginally significant plots 
plotHR_enn <- plot_model(Best_hr, type = "eff", terms = c("Enn_mn")) +
  geom_point(data = Data_move3, aes(x = Enn_mn, y = Home_range),
             color = "blue", pch = 18, cex = 1, alpha = 0.45) +
  ggtitle("") +
  theme_bw() +
  labs(x = "Euclidean distance between neighbouring patches (m)", y = "Home range (ha)") 



# ==============================================================================
# With Daily distance as response variable

# Distribution

hist(Data_move5_dist$Distance)
hist(Data_move5_dist$Pland)
hist(Data_move5_dist$Pd)
hist(Data_move5_dist$Enn_mn)
hist(Data_move5_dist$Connect)
hist(Data_move5_dist$Clumpy)
hist(Data_move5_dist$Contag)

# Outliers 
Data_move2_dist <- Data_move1 %>% filter(Enn_mn < 400 & Contag > 10)
Data_move2_dist1 <- Data_move1 %>% filter(Contag > 10)

hist(Data_move2_dist$Enn_mn)

# Better approach for outlier removal
variables <- c("Distance", "Pland", "Pd", "Enn_mn", "Connect", "Clumpy", "Contag")

Data_move1_NaOutl <- Data_move1

for (var in variables) {
  outliers <- boxplot(Data_move1_NaOutl[[var]], plot = FALSE)$out
  Data_move1_NaOutl <- Data_move1_NaOutl[!Data_move1_NaOutl[[var]] %in% outliers, ]
}

dim(Data_move1)
dim(Data_move1_NaOutl)


# Remove NA
is.na(Data_move2_dist)
na_count <- sapply(Data_move2_dist, function(x) sum(is.na(x)))
print(na_count)


Data_move3_dist <-  subset(Data_move2_dist, !is.na(Distance) & !is.na(Enn_mn) & !is.na(Clumpy))
dim(Data_move3_dist)
Data_move4_dist <-  subset(Data_move2_dist, !is.na(Distance) & !is.na(Enn_mn))
dim(Data_move4_dist)
Data_move5_dist <-  subset(Data_move2_dist1, !is.na(Distance))

is.na(Data_move3$Clumpy)


str(Data_move1)
vif(lmer(Distance ~ scale(Pland) + scale(Pd) + scale(Enn_mn) + scale(Area_mn) +
           scale(Connect) + scale(Contag) + scale(Clumpy) + (1|Koala_id), 
         data = Data_move3_dist))

View(Data_move3)

vif(lmer(Distance ~ scale(Pland) + scale(Pd) + scale(Area_mn) + scale(Enn_mn) + 
           scale(Connect) + scale(Contag) + (1|Koala_id), # Omitting clumpy & Area_mn
         data = Data_move4_dist))

vif(lmer(Distance ~ scale(Pland) + scale(Pd) + scale(Area_mn) +
           scale(Connect) + scale(Contag) + (1|Koala_id), # Omitting clumpy & Area_mn
         data = Data_move5_dist))

vif(lmer(Distance ~ scale(Pland) + scale(Pd) + scale(Connect) +
           + scale(Contag) + (1|Koala_id), # Omitting clumpy & Area_mn
         data = Data_move5_dist))


cor.test(Data_move2_2$Connect, Data_move2_2$Pd)

model_dist <- lmer(Distance ~ scale(Pland) + scale(Pd) +
                     scale(Connect) + scale(Contag) + (1|Koala_id),
                   data = Data_move5_dist)


options(na.action = "na.fail")
dredged_dist <- dredge(model_dist)
print(dredged_dist)

Best_dist <- get.models(dredged_dist, 1)[[1]]
summary(Best_dist)
tab_model(Best_dist, show.re.var = T)


# Avg_dist <- model.avg(dredged_dist, subset = delta <= 2)
# summary(Avg_dist)

# Plot significant variables
plotDist_pd <- plot_model(Best_dist, type = "eff", terms = c("Pd")) +
  geom_point(data = Data_move4, aes(x = Pd, y = Distance),
             color = "blue", pch = 18, cex = 1, alpha = 0.45) +
  ggtitle("") +
  theme_bw() +
  labs(x = "Patch density (per ha)", y = "Distance travelled (m/day)")

plotDist_connect <- plot_model(Best_dist, type = "eff", terms = c("Connect")) +
  geom_point(data = Data_move4, aes(x = Connect, y = Distance),
             color = "blue", pch = 18, cex = 1, alpha = 0.45) +
    ggtitle("") +
  theme_bw() +
  labs(x = "Connectivity", y = "")


plotDist_pland <- plot_model(Best_dist, type = "eff", terms = c("Pland")) +
  geom_point(data = Data_move4, aes(x = Pland, y = Distance),
             color = "blue", pch = 18, cex = 1, alpha = 0.45) +
  ggtitle("") +
  theme_bw() +
  labs(x = "Proportion of tree cover (%)", y = "")


# ==============================================================================
# Plotting four significant variables
library(cowplot)

sig_plots <- plot_grid(plotDist_pd, plotDist_connect, plotDist_pland,
                       plotHR_pd, plotHR_connect,
                       ncol = 3, nrow = 2,
                       labels = c('A.', 'B.', 'C.', 'D.', 'E.'),
                       label_size = 10)

ggsave(filename = "./Output/EffectPlots.jpg", plot = sig_plots, 
       height = 5, width = 7.5, dpi = 300)
# ==============================================================================
# SPEED as a response variable

Dat_move_speed <- Data_move1[!is.na(Data_move1$Speed), ]
dim(Dat_move_speed)


cor.test(Dat_move_speed$Speed, Dat_move_speed$Distance, method = "pearson") 

# With Speed as response variable
vif(lmer(Speed ~ Pland + Pd + Area_mn + Connect + Contag + (1|Koala_id),
         data = Dat_move_speed))

model_speed <- lmer(Speed ~ Pland + Pd + Area_mn + Connect + Contag + (1|Koala_id),
                    data = Dat_move_speed)

# options(na.action = "na.fail")
dredged_speed <- dredge(model_speed)
print(dredged_speed)

Best_speed <- get.models(dredged_speed, 1)[[1]]
summary(Best_speed)
tab_model(Best_speed, show.re.var = T)

# Examine correlation between significant variables
cor.test(Dat_move_speed$Pland, Dat_move_speed$Pd)

# Plot significant variables
plot_model(Best_speed, type = "eff", terms = c("Connect")) +
  ggtitle("") +
  theme_minimal() +
  labs(x = "Connectivity", y = "Predicted Speed (m/hr)")

plot_model(Best_speed, type = "eff", terms = c("Pd")) +
  ggtitle("") +
  theme_minimal() +
  labs(x = "Patch density (per ha)", y = "Predicted Speed (m/hr)")


# ==============================================================================
# ------------------------------------------------------------------------------
Data_move3 <- Data_move2
Data_move3$Pland <- scale(Data_move3$Pland)
Data_move3$Pd <- scale(Data_move3$Pd)

Data_move3$Area_mn <- scale(Data_move3$Area_mn)
Data_move3$Connect <- scale(Data_move3$Connect)
Data_move3$Contag <- scale(Data_move3$Contag)


cor.test(Data_move3$Sinuos, Data_move3$Straight)

# With Straightness index as response variable
vif(lmer(Straight ~ Pland + Pd + Area_mn + Connect + Contag + (1|Koala_id),
         data = Data_move3))

dim(Data_move3)

model_straight <- lmer(Straight ~ Pland + Pd + Area_mn + Connect + Contag + (1|Koala_id),
                       data = Data_move3)

# options(na.action = "na.fail")
dredged_straight <- dredge(model_straight)
print(dredged_straight)

Best_straight <- get.models(dredged_straight, 1)[[1]]
summary(Best_straight)
tab_model(Best_straight, show.re.var = T)


# ------------------------------------------------------------------------------
# With Sinuosity index as response variable

vif(lmer(Sinuos ~ Pland + Pd + Area_mn + Connect + Contag + (1|Koala_id),
         data = Data_move3))

model_sinuos <- lmer(Sinuos ~ Pland + Pd + Area_mn + Connect + Contag + (1|Koala_id),
                     data = Data_move2)

# options(na.action = "na.fail")
dredged_sinuos <- dredge(model_sinuos)
print(dredged_sinuos)

Best_sinuos <- get.models(dredged_sinuos, 1)[[1]]
summary(Best_sinuos)
tab_model(Best_sinuos, show.re.var = T)



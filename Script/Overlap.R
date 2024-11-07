# Overlap
library(ctmm)
library(tidyverse)

?overlap
overlap(UDS_disp)
Over_annual <- overlap(UDS)

options(max.print = 1000000)
# write.csv(overlap(UDS), "./Output/HR_overlap.csv", row.names = FALSE)

sink("./Output/HR_overlap.csv")
print(Over_annual$CI[,,"est"])
sink()


extent(UDS_disp)


# ==============================================================================
# Pairwise distance
DISTS_NicVsWend <- distances(disp_telemetry[c(2,6)], FITS_disp[c(2,6)])

plot(DISTS_NicVsWend$est ~ DISTS_NicVsWend$timestamp,
     type = "l",
     col = "#5e548e",
     ylab = "Separation distance (m)",
     xlab = "")

# Encounter rate
DISTS_NicVsWend$ER <- ifelse(DISTS_NicVsWend$est <= 25, 1, 0)
plot(DISTS_NicVsWend$ER ~ DISTS_NicVsWend$timestamp, 
     xlab = "", ylab = "ER", main = "Scatter plot")



# ==============================================================================
# Load overlap (revised data)
overlap_matrix <- read.csv("./Data/Overlap.csv") # this data has GMT timestamp, need to convert into local time
str(overlap_matrix)


# Convert from wide to long format using pivot_longer
overlap_long <- overlap_matrix %>% 
  pivot_longer(cols = -Koala_id,  # Assuming 'Individual1' is the column with individual names
               names_to = "Koala_ID", 
               values_to = "Overlap_value") %>%
  filter(!is.na(Overlap_value) & Overlap_value > 0) %>%  # Remove rows with NA overlap values
  rename(Individual1 = Koala_id, Individual2 = Koala_ID) 

str(overlap_long)

# Load sex related data
koala_sex <- read.csv("./Data/Koala_Sex_forOverlap.csv") # this data has GMT timestamp, need to convert into local time
View(koala_sex)


overlap_long1 <- overlap_long %>%
  left_join(koala_sex, by = c("Individual1" = "Koala_id")) %>%
  rename(Sex1 = Sex) %>%
  left_join(koala_sex, by = c("Individual2" = "Koala_id")) %>%
  rename(Sex2 = Sex) %>%
  mutate(Pair = paste(Individual1, Individual2, sep = "-"),
         Pair_sex = paste(Sex1, Sex2, sep = "-")) %>%
  select(Pair, Pair_sex, Overlap_value)

View(overlap_long1)

head(overlap_long1, n = 11)


# Counting overlapped numbers
# Preparing dataframe

library(tidyverse)

overlap_long_2 <- overlap_long1 %>% 
  separate(Pair_sex, into = c("Pair1_sex", "Pair2_sex"), sep = "-") %>% 
 # mutate(Pair1 = str_extract(Pair, "^[^-]*"),
         mutate(Pair1 = map_chr(Pair, ~ str_split(.x, "-")[[1]][1]),
                Pair2 = map_chr(Pair, ~ str_split(.x, "-")[[1]][2]))

head(overlap_long2, n = 15)

# Exclude some animals
exclude_animals <- c("Alicia1", "Nicole1", "Nicole2", "Wendy1", "Wendy1", "Wayne1",
                    "Wayne2",  "Millie", "Forrest")

overlap_long2 <- overlap_long_2 %>%
  # Remove rows where either Pair1 or Pair2 contains any of the excluded animals
  filter(!Pair1 %in% exclude_animals & !Pair2 %in% exclude_animals)

head(overlap_long2)

 # =============================================================================
# List of males and females

males <- c("Buddha", "Canning", "Chadwick", "Ferrell", "Forrest", "Geronimo", 
           "Hardy", "Hero", "Holland", "Kenny", "Logan", "Murray", "Oscar", "Ronald",
           "Sneaky", "Speedy", "Wonka", "Yogi")

females <- c("Angelina", "Anne", "Banshee", "Brook", "Creek", 
           "Eucalyptus", "Jessie", "Kathy", "Kay", "Kokoda", "Lana", "Laxmi",
           "Lesa", "Lily", "Michelle", "Millie", "Roxette",
           "Wilga", "Willow")



# -------------------- Number of females overlapped by a single male
Overlap_MaleVsFem <- overlap_long2 %>%
  
filter((Pair1 %in% males & Pair2 %in% females) |
         (Pair1 %in% females & Pair2_sex %in% males)) %>% 
  
  group_by(Pair1) %>%
  
  summarise(n_males = n_distinct(Pair2))

print(Overlap_MaleVsFem, n = nrow(Overlap_MaleVsFem)) # Some animals has less than 10 records.



# Summarising overlap (Male Vs Female)
overlap_long2 %>%
  filter((Pair1 %in% males & Pair2 %in% females) |
           (Pair1 %in% females & Pair2_sex %in% males)) %>% 
  summarize(mean_MF = mean(Overlap_value),
            sd_MF = sd(Overlap_value),
            median_MF = median(Overlap_value),
            min_MF = min(Overlap_value),
            max_MF = max(Overlap_value))




# -------------------- Number of males overlapping males ranges
Overlap_MaleVsMale <- overlap_long2 %>%
  filter(Pair1 %in% males &
           Pair2 %in% males) %>%
  group_by(Pair1) %>%
  summarise(n_males = n_distinct(Pair2))

print(Overlap_MaleVsMale, n = nrow(Overlap_MaleVsMale)) # Some animals has less than 10 records.

# Summarising overlap (Male Vs males)
overlap_long2 %>%
  filter(Pair1 %in% males &
           Pair2 %in% males & Overlap_value > 0) %>%
  summarize(mean_Males = mean(Overlap_value),
            sd_Males = sd(Overlap_value),
            median_Males = median(Overlap_value),
            min_Males = min(Overlap_value),
            max_MalesF = max(Overlap_value))



# -------------------- Number of females overlapping females ranges
Overlap_FemVsFem <- overlap_long2 %>%
  filter(Pair1 %in% females &
           Pair2 %in% females) %>%
  group_by(Pair1) %>%
  summarise(n_males = n_distinct(Pair2))

print(Overlap_FemVsFem, n = nrow(Overlap_FemVsFem)) # Some animals has less than 10 records.

# Summarising overlap (Female Vs Female)

  overlap_long2 %>%
    filter(Pair1 %in% females &
             Pair2 %in% females) %>%
    summarize(mean_Fem = mean(Overlap_value),
              sd_Fem = sd(Overlap_value),
              median_Fem = median(Overlap_value),
              min_Fem = min(Overlap_value),
              max_FemF = max(Overlap_value))

# Statistical test
  hist(overlap_long2$Overlap_value)
  shapiro.test((overlap_long2$Overlap_value)) # Non-parametric
  
  # Step 1: Create a new variable for pair types
  overlap_long2 <- overlap_long2 %>%
    mutate(Pair_type = case_when(
      Pair1_sex == "M" & Pair2_sex == "M" ~ "Male-Male",
      Pair1_sex == "F" & Pair2_sex == "F" ~ "Female-Female",
      (Pair1_sex == "M" & Pair2_sex == "F") | (Pair1_sex == "F" & Pair2_sex == "M") ~ "Male-Female"
    ))

  
  # Step 2: Kruskal-Wallis test for group differences
  library(rstatix)
  
  
  kruskal_test_result <- overlap_long2 %>%
    kruskal_test(Overlap_value ~ Pair_type)
  
  print(kruskal_test_result)

  # Step 3: Post-hoc Dunn test (if Kruskal-Wallis is significant)
  
    dunn_test_result <- overlap_long2 %>%
      dunn_test(Overlap_value ~ Pair_type, p.adjust.method = "bonferroni")
    
    print(dunn_test_result)
  
    # Step 4: Boxplot to visualize the differences
        ggplot(overlap_long2, aes(x = Pair_type, y = Overlap_value, fill = Pair_type)) +
      geom_boxplot() +
      labs(title = "Overlap Value by Pair Type",
           x = "Pair Type",
           y = "Overlap Value") +
      theme_minimal()
  
  # ============================================================================
        # Counting dyads available
        overlap_long2 %>%
          group_by(Pair_type) %>%
          summarise(count = n())
        
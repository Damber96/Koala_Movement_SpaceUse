
library(lubridate)
library(tidyverse)


#Load file
# Records outside the end dates of collars are excluded in this data.

Data_K <- read.csv("./Data/Data_Koala_Upto_31Aug2024_Final.csv") # this data has GMT timestamp, need to convert into local time
count_recordsAll <- Data_K %>% group_by(koala_id) %>% count(n())

print(count_recordsAll, n = nrow(count_recordsAll)) # Some animals has less than 10 records.


print(count_recordsAll)

str(Data_K)

unique(Data_K$ceres_tag)

unique(Data_K$location_a)

#converting GMT into local time (Nepal)

# Convert to POSIXct (a type of datetime object in R)
Data_K$timestamp <- as.POSIXct(Data_K$date_time, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
head(Data_K$timestamp)


# Omitting the first seven days of tagging
dim(Data_K)

Data_K1 <- Data_K %>%
  group_by(koala_id) %>%
  filter(as.numeric(difftime(timestamp, min(timestamp), units = "days")) > 7)

names(Data_K1)

View(Data_K1$koala_id == "Forrest")


View(Data_K2)

Data_K2 <- Data_K1 ## create another file name to ease for comparison
# The timestamp format you provided, "2023-04-24T07:43:48.000Z," follows the ISO 8601 standard.
# The "Z" at the end of the timestamp indicates that the time is in Coordinated Universal Time (UTC).
# So, the timestamps in your data are in UTC, not the local time of Brisbane, Australia. 
#If you need to convert these times to the local time in Brisbane, you'll need to account 
#for the time difference, including any daylight saving time adjustments if applicable.

Data_K2$timestamp <- with_tz(Data_K2$timestamp, "Australia/Brisbane") #finally set the time
View(Data_K2)

sum(is.na(Data_K2))

Data_K2 %>% group_by(accuracy) %>% summarise(count = n(), proportion = n() / nrow(Data_K2))

# Retaining records up to 5m accuracy represents 99% of total records

# Retain accuracy upto 10m
Data_K2_accuracy <- Data_K2 %>% filter(grepl("^<= 2m", accuracy) | grepl("^<= 5m", accuracy) | grepl("^<= 10m", accuracy))

View(Data_K2_accuracy)


# Counting records
count_id_records <- Data_K2_accuracy %>% group_by(koala_id) %>% count(n()) 

print(count_id_records, n = nrow(count_id_records)) # Some animals has less than 10 records.

# I want to include animals having more than 10 records for home range analysis
Data_forHomeRange <- Data_K2_accuracy %>% 
  group_by(koala_id) %>% 
  filter(n() >= 10) %>%
  ungroup()

Data_forHomeRange # Use this data for home range analysis





# ==============================================================================
# Continue data cleaning for distance analysis

count_date_id <- Data_K2_accuracy %>% group_by(koala_id, date = as.Date(timestamp)) %>% count(count = n()) 
print(count_date_id, n = nrow(count_date_id)) 
# View(count_date_id) # Some animals has only a single record from a day (and max records = 6/day).

dim(Data_K2_accuracy)
# I want to include at least 3 records
Data_K3 <- Data_K2_accuracy %>% 
  group_by(koala_id, as.Date(timestamp)) %>% 
  filter(n() >= 3)

dim(Data_K3)

count_date_id1 <- Data_K3 %>% group_by(koala_id, date = as.Date(timestamp)) %>% count(count = n()) 
View(count_date_id1)

count_only3Recs <- Data_K3 %>% group_by(koala_id) %>% count(n()) 

print(count_only3Recs, n = nrow(count_only3Recs)) # Some animals has less than 10 records.

#Following could be used for ordering, although I am not using it here

Data_K4 <- Data_K3 %>% arrange(koala_id, timestamp) 

# For excluding duplicated records (But I did not have to use this)
# Data_K5a <- Data_K5 %>% group_by(koala_id, timestamp) %>% filter(row_number() == 1) %>% ungroup()

Data_K4_df <- as.data.frame(Data_K4)
dat_TimLag <- Data_K4_df


dat_TimLag$TIMLag <- Data_K4_df$duration_s/3600

view(dat_TimLag)

unique(Data_K4_df$koala_id)

dim(Data_K4_df)

# write.csv(Data_K4_df, "./Output/Data_KoalaMovement.csv")





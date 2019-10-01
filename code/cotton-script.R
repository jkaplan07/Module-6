########################################################################################
# Julia Kaplan
# Summary: Tidying and analyzing cotton production trends in NC
# Date: September 25, 2019
########################################################################################
#Study objective: The primary objective of this homework assignment is to answer the following questions:
 #1) How have yield and area harvested of cotton changed across all of the NC agricultural districts over time?
 #2) What were the top 3 cotton producing counties in NC in terms of total lbs of cotton for 2018?
  
# Clear workspace & load packages ----
rm(list=ls(all=TRUE))
library(tidyverse)

# 2. Read & inspect the dataset ----
cotton <- read_csv("data/cotton-usda-nass.csv")

str(cotton)#check structure of data
head(cotton) #check top of data - week_ending, zip_code, region, watershed have NAs.
tail(cotton) #check bottom of data - week_ending has NAs.

#check n's
dim(cotton)

#validate with external source
summary(cotton)

#Relavent columns (from assignment: 
  #year: The year in which the measurements were collected. Should be dbl data type: IT IS
  #state: The state in which the measurements were collected. Should be chr data type: IT IS
  #ag_district: The agricultural district in which the measurements were collected; each district is organized geographically and composed of several counties. Should be chr data type: IT IS
  #county: The county in which the measurements were collected. Should be chr data type: IT IS
  #data_item: The kind of measurement, either acres harvested or yield. Should be chr data type: IT IS
  #value: The measurement of either acres harvested or yield. Should be dbl data type: CURRENT SET AS CHARACTER.

#When you inspect your data, it's very important that you always check to see what the data type is in each column. Are the data types what you would expect? 
 #Currently, all the columns are formatted as expected other than value which should be dbl but is character.

# 3.1. Create a NC data subset ----
#filter to rows that correspond to measurements from NC and select only the columns that are needed (in the bulleted list above).
cotton %>%
  #filtering not needed since dataset already was subset to only NC
  select(year, state, ag_district, county, data_item, value) -> cotton_relevant #only keep relevant columns and output relevant column dataset
  
# 3.2. Divide the data_item column ----
cotton_relevant %>%
  separate (data_item, #separate data_item into: 
          into = c("cotton_type", "measurement"),
          sep = " - ") -> cotton_separate #separate using " - " as what to separate 2 types of info by
  
# 3.3. Convert the value column to numeric type ----
cotton_separate %>%
  #filter data to remove any record with (D)
  filter (value != "(D)")

cotton_separate$value <- as.numeric (cotton_separate$value)  #takes value and convert to numeric. 
str(cotton_separate) #confirmed that value is now numeric

# 4. Visualizing trends ----
cotton_separate %>%
  ggplot (mapping = aes(x = year, y = value)) +
  geom_jitter() +
  theme_minimal () +
  facet_grid (rows = vars(measurement), cols = vars(ag_district), "free_y") +
  labs (x = "Year", y = " ", title = "Cotton Production in NC") +
  theme (axis.text.x = element_text(angle = 90)) 
   

# 5. Summarize data from 2018 ----
#Next, we’ll answer the second question of this exercise: What were the top 3 cotton producing counties in NC in terms of total lbs of cotton for 2018? #original dataset only had through 2017, so that is what is run here.
cotton_separate %>%
  filter(year == 2018) %>% #only keep 2018 %>%
  spread(key = measurement, value = value) %>% #create 2 new columns
  mutate (acres = `ACRES HARVESTED`) %>% #rename
  mutate (yield = `YIELD, MEASURED IN LB / ACRE`) %>% #rename
  mutate (total_lbs = acres*yield) %>% #calculate total lbs
  mutate (rank = min_rank(desc(total_lbs))) %>% #calculate the rank
  arrange (rank) %>% #arrange by rank
  select (county, total_lbs) -> cotton_2018 #only keep county, total lbs
top_n(cotton_2018, 3) #show top 3 producing counties

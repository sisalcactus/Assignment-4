# Shawn Chang
# BTC1855H
# Assignment 4

# Reading the data into a data frame #### (make sure that column names do not have spaces in them).

library(dplyr)                                                                  # this package is selected to ensure that the column names are correct, containing no spaces

ufo_df <- read.csv("ufo_subset.csv")                                            # this is needed to load and read the data to allow for downstream work

# Finding the rows lacking "shape" information
rows_missing_shape <- which(ufo_df$shape == "")                                 # this is needed to identify the specific row numbers (for rows containing empty cells in the "shape" column) 
rows_missing_shape                                                              # this is needed to display all row numbers where Shape is not described

# Cleaning the data
ufo_df_clean <- ufo_df %>%
  mutate(shape = case_when(shape == "" ~ "unknown", .default = shape)) %>%      # this is needed to replace empty "shape" cells with "unknown" while keeping the rest unchanged
  filter(country != "") %>%                                                     # this is needed to filter out (remove) all rows with empty cells in the "country" column
  
  
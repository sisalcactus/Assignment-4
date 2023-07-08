# Shawn Chang
# BTC1855H
# Assignment 4

# Reading the data into a data frame #### (make sure that column names do not have spaces in them).

library(dplyr)                                                                  # this package is selected to ensure that the column names are correct, containing no spaces

ufo_df <- read.csv("ufo_subset.csv")                                            # this is needed to load and read the data to allow for downstream work

# Finding the rows lacking "shape" information
rows_missing_shape <- which(ufo_df$shape == "")                                 # this is needed to identify the specific row numbers (for rows containing empty cells in the "shape" column) 
rows_missing_shape                                                              # this is needed to display all row numbers where "shape" values are absent

# Cleaning the data
ufo_df_clean <- ufo_df %>%
  mutate(shape = case_when(shape == "" ~ "unknown", .default = shape)) %>%      # this is needed to replace empty "shape" cells with "unknown" while keeping the rest unchanged
  filter(!grepl("\\(hoax", tolower(comments)) & 
        !grepl("possible hoax", tolower(comments)) &
        !grepl("hoax\\?", tolower(comments)) &
        !grepl("either a hoax", tolower(comments)) & 
        !grepl("hoax in modesto", tolower(comments)) &
        !grepl("not intentional hoax", tolower(comments)) &
        !grepl("is a hoax", tolower(comments)) &
        !grepl("false information", tolower(comments)) &
        !grepl("false alarm", tolower(comments)) &
        !grepl("note\\: * possib", tolower(comments)) &
        !grepl("note\\: * star", tolower(comments)) &
        !grepl("note\\: * venus", tolower(comments)) &
        !grepl("note\\: * jupiter", tolower(comments)) &
        !grepl("note\\: * venus", tolower(comments)) &
        !grepl("note\\: * sirius", tolower(comments))) %>%
  filter(country != "") %>%                                                     # this is needed to filter out (remove) all rows with empty cells in the "country" column
  mutate(date_posted = as.Date(date_posted, tryFormats = "%d-%m-%Y")) %>%       # this is needed to set the date_posted column values in the proper format (date format), with the tryFormats part added to specify the current format
  mutate(datetime = as.Date(datetime)) %>%                                      # this is needed to set the datetime column values also in the proper format (tryFormats not needed as the dates are recognized by default)
  rename(date_observed = datetime) %>%                                          # this is needed to update the datetime column (now just dates)
  mutate(report_delay = date_posted - date_observed) %>%                        # this is needed to add a new column (report_delay) capturing the difference between the posted date and the sighting date
  filter(report_delay >= 0)                                                     # this is needed to filter out the rows where the posted date precedes the sighting date (meaning the difference would be a negative value)
  


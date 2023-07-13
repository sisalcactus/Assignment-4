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
  filter(country != "") %>%                                                     # this is needed to filter out (remove) all rows with empty cells in the "country" column
  mutate(date_posted = as.Date(date_posted, tryFormats = "%d-%m-%Y")) %>%       # this is needed to set the date_posted column values in the proper format (date format), with the tryFormats part added to specify the current format
  mutate(datetime = as.Date(datetime)) %>%                                      # this is needed to set the datetime column values also in the proper format (tryFormats not needed as the dates are recognized by default)
  mutate(is_hoax = case_when(
    !grepl("not a hoax", tolower(comments)) &
    grepl("hoax", tolower(comments)) |
    grepl("false information", tolower(comments)) |
    grepl("false alarm", tolower(comments)) ~
    "TRUE",
    .default = "FALSE"
  ))

# Creating the table to show the percentage of hoaxes per country 
total_hoax_count <- sum(ufo_df_clean$is_hoax == "TRUE")
hoaxes_per_country_table <- ufo_df_clean %>%
  group_by(country) %>%
  summarise(percentage_of_hoaxes = round(sum(is_hoax == "TRUE") / total_hoax_count * 100, 2))

# Continuing onto adding report_delay
ufo_df_clean2 <- ufo_df_clean %>%
  rename(date_observed = datetime) %>%                                          # this is needed to update the datetime column (now just dates)
  mutate(report_delay = date_posted - date_observed) %>%                        # this is needed to add a new column (report_delay) capturing the difference between the posted date and the sighting date
  filter(report_delay >= 0)                                                   # this is needed to filter out the rows where the posted date precedes the sighting date (meaning the difference would be a negative value)

# Creating the table to show the average report_delay per country 
average_report_delay_per_country_table <- ufo_df_clean2 %>%
  group_by(country) %>%
  summarise(average_report_delay = round(mean(report_delay), 1))

#



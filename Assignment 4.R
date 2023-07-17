# Shawn Chang
# BTC1855H
# Assignment 4

# Reading the data into a data frame ####

# Everyone should have the dplyr package installed
# but if you don't have it, please start with "install.packages("dplyr")" before continuing

library(dplyr)                                                                  # this package is selected for streamlined coding and helping to ensure that the column names are correct, containing no spaces

ufo_df <- read.csv("ufo_subset.csv")                                            # this is needed to load and read the data into a dataframe to allow for downstream work (the csv format is correct and the column names do not have unwanted spaces); this properly shows the header row as well

# Finding the rows lacking "shape" information ####
rows_missing_shape <- which(ufo_df$shape == "")                                 # this is needed to identify the specific row numbers (for rows containing empty cells in the "shape" column) 
rows_missing_shape                                                              # this is needed to display all row numbers where "shape" values are absent

# Cleaning the data ####
# from imputing "shape" rows with "unknown" where needed to creating is_hoax column, in one combined step)
ufo_df_clean <- ufo_df %>%
  mutate(shape = case_when(shape == "" ~ "unknown", .default = shape)) %>%      # this is needed to replace empty "shape" cells with "unknown" while keeping the rest of that column unchanged
  filter(country != "") %>%                                                     # this is needed to filter out (remove) all rows with empty cells in the "country" column
  mutate(datetime = as.Date(datetime)) %>%                                      # this is needed to set the datetime column values in the proper format (tryFormats not needed as the dates are recognized by default); the time portion is not kept as it is not important (checked with Dr. K)
  rename(date_observed = datetime) %>%                                          # this is needed to update the name of the "datetime" column (since this fixed version now contains just dates and no times)
  mutate(date_posted = as.Date(date_posted, tryFormats = "%d-%m-%Y")) %>%       # this is needed to set the "date_posted" column values in the proper format (date format), with the tryFormats part added to specify the current format
  mutate(is_hoax = case_when(                                                   # this mutate() part is needed to create a new column (is_hoax)
    !grepl("not a hoax", tolower(comments)) &                                     # where all rows with mentions of "hoax" (except the one that said "This is not a hoax"), "false information", and "false alarm" are labelled "TRUE" and all others "FALSE"
    grepl("hoax", tolower(comments)) |                                            # as such, this hoax filter is designed to catch only those that are highly likely or obviously hoaxes (to the best of my knowledge); removing any other could be too strict, causing us to lose valuable data
    grepl("false information", tolower(comments)) |                             # this one represents the comment that said "Sorry for the false information...", meaning it is definitely a hoax
    grepl("false alarm", tolower(comments)) ~                                   # this one represents the comment that said "Sorry - false alarm; the bright orange light I saw was just a flare." so definitely a hoax
    TRUE,                                                                       # the logical TRUE and FALSE are selected as we want a Boolean column
    .default = FALSE
  ))
View(ufo_df_clean)                                                              # this is to visualize the modified data

# Creating the table to show the percentage of hoax sightings per country ####
total_hoax_count <- sum(ufo_df_clean$is_hoax == TRUE)                                       # this is needed to find the total number of sightings classified as "hoax" for downstream calculation (as the denominator)
hoaxes_per_country_table <- ufo_df_clean %>%                                                # this is needed to create the table of interest with "country" being the category of interest (since we want to find the number of hoaxes for each country)
  group_by(country) %>%                                                                     # this and the next line of code are needed to create a table showing the percentages of interest (rounded to 2 decimal places), with sum() used to count the number of hoax sightings for each country
  summarise(percentage_of_hoaxes = round(sum(is_hoax == "TRUE") / total_hoax_count * 100, 2))
View(hoaxes_per_country_table)                                                  # this is to visualize the table

# Continuing onto adding report_delay and onward ####
ufo_df_clean2 <- ufo_df_clean %>%
  mutate(report_delay = date_posted - date_observed) %>%                        # this is needed to add a new column (report_delay) capturing the difference, in days, between the posted date and the observation date
  filter(report_delay >= 0)                                                     # this is needed to filter out (remove) the rows where the posted date precedes the sighting date (meaning the difference would be a negative value, which would not make sense)
View(ufo_df_clean2)                                                             # this is to visualize the data

# Creating the table to show the average report_delay per country ####
avg_report_delay_per_country_table <- ufo_df_clean2 %>%                             # this is needed to create the table of interest, again with "country" being the category of interest (since we want to find the mean report_delays per country)
  group_by(country) %>%
  summarise(average_report_delay = round(mean(report_delay[report_delay > 0]), 1))  # we round to 1 decimal place; it does not make sense to have report_delays being less than 0 (since that means people reported before the sighting) hence the subsetting ([report_delay > 0]) to account only for rows where sighting dates preceded posted dates
View(avg_report_delay_per_country_table)                                        # this is to visualize the table

# Reviewing the "duration.seconds" column ####
sum(ufo_df_clean2$duration.seconds == "")                             # check for missingness (empty string: ""): this is needed to check if there is any missing value (i.e., empty cell)
sum(is.na(ufo_df_clean2$duration.seconds))                            # check for missingness (NA): this is needed to check if there is any missing value (i.e., NA")
is.numeric(ufo_df_clean2$duration.seconds)                            # check for format: this is needed to check if all values in this column are numeric and they are
head(sort(ufo_df_clean2$duration.seconds, decreasing = T), 100)       # check for range: this is needed to identify the 100 largest values of this entire data
head(sort(ufo_df_clean2$duration.seconds), 100)                       # check for range: this is needed to identify the 100 smallest values of this entire data (should be above 0)
str(ufo_df_clean2$duration.seconds)                                   # check for overall structure

# Plotting the duration in seconds ####
  # The biggest problem is that the range is vast, from 0.02 to 82800000, but I did not want to narrow the range as these sightings are extraordinary and valuable so each (of the cleaned dataset) deserves to be accounted for!
  # To visualize the data, I chose to scale it logarithmically (shown below)
  hist(log10(ufo_df_clean2$duration.seconds), main = "Histogram of log(sighting duration in seconds)", xlab = "Log of sighting duration in seconds (base 10)", col = blues9) 
    # hist() is the function to plot the histogram (overall presenting a nice bell curve!); I edited the title (using "main") and x axis label (using "xlab") but not the y axis label since the default is what I want
    # for better visualization, I added some colour!



## General Comments
# The code meets the specified assignment requirements and it behaves as expected 
# The code is easy to understand. There are no unnecessary comments, and the code is self explanatory. 
# I like how you also outlined what packages the user would need to install to run this code.
# There are no redundant operations. Great use of piping to avoid creating more data set versions than necessary!

## Suggestions
# I would consider shortening the vector names (ie. line 50). One example is delay_per_country or country_delay. 

## Summary
# Overall the code looks good and functions as expected. 
# There are no significant concerns to address. 
# The commit history is shown in github so one can follow along with code development. 

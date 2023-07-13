# Shawn Chang
# BTC1855H
# Assignment 4

# Reading the data into a data frame ####

# Everyone should have the dplyr package installed
# but if you don't have it, please start with "install.packages("dplyr")" before continuing

library(dplyr)                                                                  # this package is selected to ensure that the column names are correct, containing no spaces

ufo_df <- read.csv("ufo_subset.csv")                                            # this is needed to load and read the data to allow for downstream work

# Finding the rows lacking "shape" information ####
rows_missing_shape <- which(ufo_df$shape == "")                                 # this is needed to identify the specific row numbers (for rows containing empty cells in the "shape" column) 
rows_missing_shape                                                              # this is needed to display all row numbers where "shape" values are absent

# Cleaning the data ####
# from imputing "Shape" rows with "unknown" where needed to creating is_hoax, in one combined step)
ufo_df_clean <- ufo_df %>%
  mutate(shape = case_when(shape == "" ~ "unknown", .default = shape)) %>%      # this is needed to replace empty "shape" cells with "unknown" while keeping the rest unchanged
  filter(country != "") %>%                                                     # this is needed to filter out (remove) all rows with empty cells in the "country" column
  mutate(date_posted = as.Date(date_posted, tryFormats = "%d-%m-%Y")) %>%       # this is needed to set the date_posted column values in the proper format (date format), with the tryFormats part added to specify the current format
  mutate(datetime = as.Date(datetime)) %>%                                      # this is needed to set the datetime column values also in the proper format (tryFormats not needed as the dates are recognized by default)
  mutate(is_hoax = case_when(                                                   # this mutate() part is needed to create a new column (is_hoax)
    !grepl("not a hoax", tolower(comments)) &                                     # where all rows with mentions of "hoax" (except the one that said "not a hoax"), "false information", and "false alarm" are labelled "TRUE" and all others "FALSE"
    grepl("hoax", tolower(comments)) |                                            # the hoax filter is designed to catch only those that are highly likely or obviously hoaxes (to the best of my knowledge); removing any other could be too strict, causing us to lose valuable data
    grepl("false information", tolower(comments)) |                             # this one said "Sorry for the false information...", meaning it is definitely a hoax
    grepl("false alarm", tolower(comments)) ~                                   # this one said "Sorry - false alarm; the bright orange light I saw was just a flare." so definitely a hoax
    "TRUE",
    .default = "FALSE"
  ))
View(ufo_df_clean)                                                              # this is to visualize the data

# Creating the table to show the percentage of hoaxes per country ####
total_hoax_count <- sum(ufo_df_clean$is_hoax == "TRUE")                                       # this is needed to find the total number of sightings classified as "hoax" for downstream calculation (as the denominator)
hoaxes_per_country_table <- ufo_df_clean %>%                                                  # this is needed to create the table of interest with "country" being the category of interest (since we want to find the numnber of hoaxes for each country)
  group_by(toupper(country)) %>%                                                              # toupper() used to make the country upper case since that should be the convention; this and the next line of code are needed to create a table showing the percentages of interest (rounded to 2 decimal places), with sum() used to count the number of hoax sightings for each country
  summarise(percentage_of_hoaxes = round(sum(is_hoax == "TRUE") / total_hoax_count * 100, 2))
View(hoaxes_per_country_table)                                                  # this is to visualize the table

# Continuing onto adding report_delay ####
ufo_df_clean2 <- ufo_df_clean %>%
  rename(date_observed = datetime) %>%                                          # this is needed to update the datetime column (now just dates)
  mutate(report_delay = date_posted - date_observed) %>%                        # this is needed to add a new column (report_delay) capturing the difference between the posted date and the sighting date
  filter(report_delay >= 0)                                                     # this is needed to filter out the rows where the posted date precedes the sighting date (meaning the difference would be a negative value)
View(ufo_df_clean2)                                                             # this is to visualize the data

# Creating the table to show the average report_delay per country ####
avg_report_delay_per_country_table <- ufo_df_clean2 %>%                             # this is needed to create the table of interest, again with "country" being the category of interest (since we want to find the mean report_delays per country)
  group_by(country) %>%
  summarise(average_report_delay = round(mean(report_delay[report_delay > 0]), 1))  # it does not make sense to have report_delays being less than 0 (since that means people reported before the sighting) hence the subsetting ([report_delay > 0])
View(avg_report_delay_per_country_table)                                        # this is to visualize the table

# Reviewing the "duration.seconds" column ####
is.numeric(ufo_df_clean2$duration.seconds)                            # check for format and missingness (NAs): this is needed to check if all values in this column are numeric and they are (implying there is no NA value)
sum(ufo_df_clean2$duration.seconds == "")                             # check for missingness (""): this is needed to check if there is any missing value (i.e., empty cell)
head(sort(ufo_df_clean2$duration.seconds, decreasing = T), 100)       # check for range: this is needed to identify the top 100 largest values of this dataset
head(sort(ufo_df_clean2$duration.seconds), 100)                       # check for range: this is needed to identify the top 100 smallest values of this dataset

# Plotting the duration in seconds ####
  # I did not want to remove any data: these sightings are extraordinary so each deserves to be accounted for!
  # To visualize the data, I chose to scale it logarithmically (shown below)
  hist(log10(ufo_df_clean2$duration.seconds), main = "Histogram of log(duration in seconds)", xlab = "log of duration in seconds (base 10)", col = blues9) 
    # hist() is the function to plot the histogram; I edited the title (using "main") and x label (using "xlab") but not the y label since the default is what I want
    # for better visualization, I added some colour!




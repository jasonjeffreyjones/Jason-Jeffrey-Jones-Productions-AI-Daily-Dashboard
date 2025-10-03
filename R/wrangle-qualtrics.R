# This script wrangles the entire Qualtrics file into analysis form.

# Read the input file from latest data/qualtrics-download-YYYY-MM-DD.zip (file is 'Daily AI Support.csv')
# Write the output file to qualtrics-responses.csv

# Use a directory check to see where the script is running.
baseDirectory = "/home/ec2-user/ai_daily/" # Assume on the AWS server.
if (dir.exists(baseDirectory)) {
  # If on the AWS server, also be sure to explicitly point to libraries.
  .libPaths(c("/home/ec2-user/miniconda3/lib/R/library", .libPaths()))
} else {
  baseDirectory = "J:/Web Stuff/jasonjones.ninja/social-science-dashboard-inator/jjjp-ai-daily-dashboard/"
}

suppressMessages(library(tidyverse))

#unzip(paste0(baseDirectory, "data/qualtrics-download.zip"), list = TRUE)

# Find the latest qualtrics-download-YYYY-MM-DD.zip file in the data directory.
dataFiles = list.files(paste0(baseDirectory, "data/"), pattern = "qualtrics-download-\\d{4}-\\d{2}-\\d{2}\\.zip")
dataFiles = sort(dataFiles, decreasing = TRUE)
latestFileName = dataFiles[1]

rawQualtrics = read_csv(unzip(paste0(baseDirectory, "data/", latestFileName), "Daily AI Support.csv"), show_col_types = FALSE)

# Keep only valid data rows.
rawQualtrics = rawQualtrics %>% filter(consent == "Consent to continue")
rawQualtrics = rawQualtrics %>% filter(Finished == "1")

# Keep the columns we need.
rawQualtrics = rawQualtrics %>% select(PROLIFIC_PID, StartDate, primary_response, text_response, risk_self_report, generalized_trust, Q1, Q2)

# Rename prompt columns.
rawQualtrics = rawQualtrics %>% rename(Prolific_PID = PROLIFIC_PID) %>% 
  rename(Obs_Date = StartDate) %>% 
  rename(Support = primary_response) %>% 
  rename(Text_Response = text_response) %>% 
  rename(Risk_Preference = risk_self_report) %>% 
  rename(Generalized_Trust = generalized_trust) %>% 
  rename(Party_1 = Q1) %>% 
  rename(Party_2 = Q2)

# Format Obs_Date.
rawQualtrics = rawQualtrics %>% mutate(Obs_Date = substr(Obs_Date, 1, 10))

# Convert to Likert numeric values.
rawQualtrics = rawQualtrics %>% mutate(Support = if_else(Support == "Strongly agree", "3", Support) ) %>% 
  mutate(Support = if_else(Support == "Agree", "2", Support) ) %>% 
  mutate(Support = if_else(Support == "Somewhat agree", "1", Support) ) %>% 
  mutate(Support = if_else(Support == "Neither agree nor disagree", "0", Support) ) %>% 
  mutate(Support = if_else(Support == "Somewhat disagree", "-1", Support) ) %>% 
  mutate(Support = if_else(Support == "Disagree", "-2", Support) ) %>% 
  mutate(Support = if_else(Support == "Strongly disagree", "-3", Support) ) %>% 
  mutate(Support = as.numeric(Support) )

# Get rid of weird apostrophe.
rawQualtrics = rawQualtrics %>% mutate(Generalized_Trust = gsub("can’t", "can't", x=Generalized_Trust) )

# Write to csv.
outputFileName = paste0(baseDirectory, "data/qualtrics-responses.csv")
write_csv(rawQualtrics, outputFileName)

print(paste("wrangle-qualtrics.R wrote to", outputFileName))


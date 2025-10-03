# This script wrangles the most recent prolific demographic file.
# It formats the demo columns and joins them to the pre-wrangled qualtrics data.
# Joined, no ID data is saved as the canonical daily file.

# We write to a file with the name jjjp-ai-support-daily-YYYY-MM-DD.csv

# Use a directory check to see where the script is running.
baseDirectory = "/home/ec2-user/ai_daily/" # Assume on the AWS server.
if (dir.exists(baseDirectory)) {
  # If on the AWS server, also be sure to explicitly point to libraries.
  .libPaths(c("/home/ec2-user/miniconda3/lib/R/library", .libPaths()))
} else {
  baseDirectory = "J:/Web Stuff/jasonjones.ninja/social-science-dashboard-inator/jjjp-ai-daily-dashboard/"
}

suppressMessages(library(tidyverse))

# Find the latest prolific-demographics-download-YYYY-MM-DD.zip file in the data directory.
dataFiles = list.files(paste0(baseDirectory, "data/"), pattern = "prolific-demographics-download-\\d{4}-\\d{2}-\\d{2}\\.csv")
dataFiles = sort(dataFiles, decreasing = TRUE)
latestFileName = dataFiles[1]

# Load the demographic file.
prolific_demographics = read_csv(paste0(baseDirectory, "data/", latestFileName), show_col_types = FALSE )

# Keep only demographic columns of interest.
prolific_demographics = select(prolific_demographics, c("Participant id","Age","Sex", "Ethnicity simplified", "Country of birth", "Country of residence", "Nationality", "Language", "Student status", "Employment status"))

# Rename columns.
prolific_demographics = rename(prolific_demographics, 
                               Prolific_PID = `Participant id`, 
                               Ethnicity = `Ethnicity simplified`,
                               BirthCountry = `Country of birth`,
                               ResidenceCountry = `Country of residence`,
                               Student = `Student status`,
                               Employment = `Employment status`, )

# Load the pre-wrangled Qualtrics response data file.
responses = read_csv(paste0(baseDirectory, "data/qualtrics-responses.csv"), show_col_types = FALSE )

# JOIN demographics with responses.
joinedData = inner_join(responses, prolific_demographics, by="Prolific_PID")

# Remove Prolific_PID.
joinedData = joinedData %>% select(-Prolific_PID)

# Demographics look OK.
#summary(as.integer(joinedData$Age))
#table(joinedData$Sex)

# I want to move the Text_Response column to the end.
joinedData = joinedData %>% relocate(Text_Response, .after = last_col())

# Write to csv.
outputFileName = paste0(baseDirectory, "data/",
                        "jjjp-ai-support-daily-",
                        format(Sys.Date(), "%Y-%m-%d"), 
                        ".csv")
write_csv(joinedData, outputFileName)

print(paste("wrangle-prolific-demographics.R wrote to", outputFileName))
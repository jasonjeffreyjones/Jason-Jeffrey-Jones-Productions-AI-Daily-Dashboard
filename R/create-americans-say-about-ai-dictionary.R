# This script creates key-value pairs and files for americans-say-about-ai.html
# Assumes wrangle-qualtrics.R has run.

# Use a directory check to see where the script is running.
baseDirectory = "/home/ec2-user/ai_daily/" # Assume on the AWS server.
if (dir.exists(baseDirectory)) {
  # If on the AWS server, also be sure to explicitly point to libraries.
  .libPaths(c("/home/ec2-user/miniconda3/lib/R/library", .libPaths()))
} else {
  baseDirectory = "J:/Web Stuff/jasonjones.ninja/social-science-dashboard-inator/jjjp-ai-daily-dashboard/"
}

suppressMessages(library(tidyverse))
suppressMessages(library(jsonlite))
suppressMessages(library(xml2))

# Initialize an empty named list to hold key-value pairs.
keyValuePairs <- list()

keyValuePairs[["WEEKLY_START_DATE"]] = "2024-02-16"
keyValuePairs[["DAILY_START_DATE"]] = "2024-04-18"

# Provide the date that this script ran as TODAY_DATE.
todayDate = format(Sys.Date(), "%Y-%m-%d")
keyValuePairs[["TODAY_DATE"]] = todayDate

# Load the most recent jjjp-ai-support-daily-YYYY-MM-DD.csv file.
aiDailyFiles = list.files(paste0(baseDirectory, "data/"), pattern = "jjjp-ai-support-daily-\\d{4}-\\d{2}-\\d{2}\\.csv")
aiDailyFiles = sort(aiDailyFiles, decreasing = TRUE)
latestFileName = aiDailyFiles[1]
aiDaily = read_csv(paste0(baseDirectory, "data/", latestFileName), show_col_types = FALSE)

# Get the most recent date from the prepared OSF file.
dataMostRecentDate = max(aiDaily$Obs_Date)
keyValuePairs[["DATA_MOST_RECENT_DATE"]] = dataMostRecentDate

aiDaily = aiDaily %>%
  select(Obs_Date, Support, Age, Sex, Text_Response) %>% 
  filter(!is.na(Support))
keyValuePairs[["TOTAL_RESPONDENTS"]] = nrow(aiDaily)

## Count and format quotes.

# Filter to usable respondents.
aiDaily = aiDaily %>%
  filter(!is.na(Text_Response)) %>% 
  mutate(Text_Response = trimws(Text_Response)) %>% 
  filter(nchar(Text_Response) > 2)

keyValuePairs[["TOTAL_QUOTES"]] = nrow(aiDaily)

# Randomly order rows and choose the top 100.
# Build up the HTML.
htmlOutput = aiDaily %>% 
  slice_sample(n = 100) %>% 
  mutate(Agree_Level = recode(Support, 
                              `-3` = "Strongly disagree",
                              `-2` = "Disagree",
                              `-1` = "Somewhat disagree",
                              `0`  = "Neither agree nor disagree",
                              `1`  = "Somewhat agree",
                              `2`  = "Agree",
                              `3`  = "Strongly agree") ) %>% 
  mutate(Text_Response = map_chr(Text_Response, ~ xml_text(read_html(paste0("<html><body>", ., "</body></html>") )))) %>%
  mutate(Text_Response = str_remove_all(Text_Response, "\\\\")) %>%  # REMOVE all backslashes
  mutate(sayQuotes = paste0(
    "<div class=\"response-card\">\n", 
    Text_Response, "\n",
    "<div class=\"attribution\">",
    Age, " year old ", Sex, " respondent<br>",
    "<em>Support further AI development?</em> <strong>", Agree_Level, "</strong><br>\n", 
    "on ", Obs_Date, 
    "</div></div>\n") ) %>%
  pull(sayQuotes) %>%  # Extract as a vector
  paste(collapse = "")

keyValuePairs[["SAY_QUOTES"]] = htmlOutput

# Write the final list to a JSON file
write_json(keyValuePairs, paste0(baseDirectory, "json/", "americans-say-about-ai.json"))

print(paste("create-americans-say-about-ai.R wrote to", paste0(baseDirectory, "json/", "americans-say-about-ai.json") ))

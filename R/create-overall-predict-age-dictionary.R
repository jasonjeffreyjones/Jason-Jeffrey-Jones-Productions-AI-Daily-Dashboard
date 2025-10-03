# This script creates key-value pairs and files for overall-predict-age.html
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

# Provide a color for this predictor and use it for points and lines below.
keyValuePairs[["AGE_HEX_COLOR"]] = "#7C1158"

# Let's filter any useless data.
aiDaily = aiDaily %>%
  filter(Age != "CONSENT_REVOKED") %>% 
  filter(!is.na(Age)) %>% 
  mutate(Age = as.numeric(Age)) %>% 
  filter(!is.na(Support))

# Visualize single-variable relationship with Support.
aiDaily %>%
  select(Age, Support) %>%
  group_by(Age) %>%
  filter(n() > 2) %>%
  summarise(Support_Mean = mean(Support), 
            n = n(),
            SE = sd(Support, na.rm = TRUE) / sqrt(n),  # Standard Error
            CI_Lower = Support_Mean - qt(0.975, df = n - 1) * SE,
            CI_Upper = Support_Mean + qt(0.975, df = n - 1) * SE) %>%
  ggplot(aes(x = Age, y = Support_Mean)) +
  annotate(geom="rect", xmin = -Inf, xmax = Inf, ymin=0, ymax=Inf, fill="green", alpha=0.1) +
  annotate(geom="rect", xmin = -Inf, xmax = Inf, ymin=-Inf, ymax=0, fill="red", alpha=0.1) +
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2) +
  geom_point() +
  geom_smooth(method = "lm", linetype = "dashed", color = "#7C1158") +
  ggtitle("AI Support by Age", "Mean agreement with 'I support further\ndevelopment of artificial intelligence.'") +
  xlab("Age in Years") + 
  ylab("Mean Response") +
  # Set a sensible set of breaks on the x-axis.
  scale_x_continuous(limits = c(18,NA), breaks = seq(20, 120, by = 10), minor_breaks = NULL) +
  # Force full y scale to -3 through +3. Add numbers and labels.
  scale_y_continuous(limits = c(-3,3), breaks = -3:3, labels = c("Strongly disagree = -3", 
                                                                 "Disagree = -2", 
                                                                 "Somewhat disagree = -1", 
                                                                 "Neither agree nor disagree = 0", 
                                                                 "Somewhat agree = 1", 
                                                                 "Agree = 2", 
                                                                 "Strongly agree = 3")) +
  theme(legend.position = "none") +
  labs(caption = paste0("Source: JJJ Pro Artificial Intelligence Daily Dashboard\n\uA9 Jason Jeffrey Jones")) +
  theme(plot.caption = element_text(size=10, color = "#666666"))
ggsave(paste0(baseDirectory, "images/", "age-ai-relationship.svg"), width = 8.29, height = 4 )

keyValuePairs[["AGE_FIGURE_FILENAME"]] = paste0("age-ai-relationship.svg?v=", format(max(aiDaily$Obs_Date), "%Y-%m-%d"))

# Prepare statement.
ageFit = aiDaily %>%
  select(Age, Support) %>%
  lm(formula = Support ~ Age)

#summary(ageFit)

# Capture the summary output as text.
summaryOutput = capture.output(summary(ageFit))
keyValuePairs[["AGE_LM_FIT_SUMMARY_TABLE"]] = paste0(summaryOutput, collapse = "\n")

# Set the results text programmatically.
summaryFit = summary(ageFit)

# Extract the coefficients table
coefficientsTable <- summaryFit$coefficients

# Print the coefficients table to see all variables
#print(coefficientsTable)

# Extract the coefficient and p-value for this variable.
thisCoefficient = coefficientsTable["Age", "Estimate"]
thisP <- coefficientsTable["Age", "Pr(>|t|)"]

# AGE_TOPLINE_STATEMENT based on interpretation
if (thisP < 0.05 && thisCoefficient > 0) {
  keyValuePairs[["AGE_TOPLINE_STATEMENT"]] = "<li><strong>Age is a statistically significant predictor of AI Support.</strong></li>\n"
  keyValuePairs[["AGE_TOPLINE_STATEMENT"]] = paste(keyValuePairs[["AGE_TOPLINE_STATEMENT"]], "<li><strong>Higher Age is associated with greater AI Support.</strong></li>\n")
} else if (thisP < 0.05 && thisCoefficient < 0) {
  keyValuePairs[["AGE_TOPLINE_STATEMENT"]] = "<li><strong>Age is a statistically significant predictor of AI Support.</strong></li>\n"
  keyValuePairs[["AGE_TOPLINE_STATEMENT"]] = paste(keyValuePairs[["AGE_TOPLINE_STATEMENT"]], "<li><strong>Lower Age is associated with greater AI Support.</strong></li>\n")
} else {
  keyValuePairs[["AGE_TOPLINE_STATEMENT"]] = "<li>Age is NOT a statistically significant predictor of AI Support.</li>"
}

# Add minor statements.
minorStatements = ""
minorStatements = paste0(minorStatements, "<li>Results update daily.</li>\n")
minorStatements = paste0(minorStatements, "<li>Based on responses from daily samples of American adults.</li>\n")
minorStatements = paste0(minorStatements, "<li>Aggregated over the entire data collection period.</li>\n")
minorStatements = paste0(minorStatements, "<li>Data collection began: ", "2024-02-16", ".</li>\n")
minorStatements = paste0(minorStatements, "<li>Most recent data collected on: ", dataMostRecentDate, ".</li>\n")
totalObservations = aiDaily %>% nrow()
keyValuePairs[["TOTAL_RESPONDENTS"]] = totalObservations
minorStatements = paste0(minorStatements, "<li>Total Observations with valid Age and response: ", totalObservations, ".</li>\n")
minorStatements = paste0(minorStatements, "<li>Age values were excluded if there were less than 2 respondents.</li>\n")

keyValuePairs[["AGE_MINOR_STATEMENTS"]] = minorStatements

# Write the final list to a JSON file
write_json(keyValuePairs, paste0(baseDirectory, "json/", "overall-predict-age.json"))

print(paste("create-overall-predict-age.R wrote to", paste0(baseDirectory, "json/", "overall-predict-age.json") ))

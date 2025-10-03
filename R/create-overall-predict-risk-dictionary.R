# This script creates key-value pairs and files for overall-predict-risk.html
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
keyValuePairs[["RISK_HEX_COLOR"]] = "#B22222"

## Risk Preference

# Visualize single-variable relationship with Support.
aiDaily %>%
  select(Risk_Preference, Support) %>%
  filter(!is.na(Support)) %>% 
  group_by(Risk_Preference) %>%
  summarise(Support_Mean = mean(Support), 
            n = n(),
            SE = sd(Support, na.rm = TRUE) / sqrt(n),  # Standard Error
            CI_Lower = Support_Mean - qt(0.975, df = n - 1) * SE,
            CI_Upper = Support_Mean + qt(0.975, df = n - 1) * SE) %>%
  ggplot(aes(x = Risk_Preference, y = Support_Mean)) +
  annotate(geom="rect", xmin = -Inf, xmax = Inf, ymin=0, ymax=Inf, fill="green", alpha=0.1) +
  annotate(geom="rect", xmin = -Inf, xmax = Inf, ymin=-Inf, ymax=0, fill="red", alpha=0.1) +
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", linetype = "dashed", color = "#B22222") +
  ggtitle("AI Support by Stated Risk Preference", "Mean agreement with 'I support further\ndevelopment of artificial intelligence.'") +
  xlab("Stated Risk Preference\n0 means: 'not at all willing to take risks'\n10 means: 'very willing to take risks'") + 
  ylab("Mean Response") +
  # Set a sensible set of breaks on the x-axis.
  scale_x_continuous(limits = c(0,10), breaks = 0:10, minor_breaks = NULL) +
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
ggsave(paste0(baseDirectory, "images/", "risk-ai-relationship.svg"), width = 8.29, height = 4 )

keyValuePairs[["RISK_FIGURE_FILENAME"]] = paste0("risk-ai-relationship.svg?v=", format(max(aiDaily$Obs_Date), "%Y-%m-%d"))

# Prepare statement.
riskFit = aiDaily %>%
  select(Risk_Preference, Support) %>%
  lm(formula = Support ~ Risk_Preference)

#summary(riskFit)

# Capture the summary output as text.
summaryOutput = capture.output(summary(riskFit))
keyValuePairs[["RISK_LM_FIT_SUMMARY_TABLE"]] = paste0(summaryOutput, collapse = "\n")

# Set the results text programmatically.
summaryFit = summary(riskFit)

# Extract the coefficients table
coefficientsTable <- summaryFit$coefficients

# Print the coefficients table to see all variables
#print(coefficientsTable)

# Extract the coefficient and p-value for this variable.
thisCoefficient = coefficientsTable["Risk_Preference", "Estimate"]
thisP <- coefficientsTable["Risk_Preference", "Pr(>|t|)"]

# RISK_TOPLINE_STATEMENT based on interpretation
if (thisP < 0.05 && thisCoefficient > 0) {
  keyValuePairs[["RISK_TOPLINE_STATEMENT"]] = "<li><strong>Stated Risk Preference is a statistically significant predictor of AI Support.</strong></li>\n"
  keyValuePairs[["RISK_TOPLINE_STATEMENT"]] = paste(keyValuePairs[["RISK_TOPLINE_STATEMENT"]], "<li><strong>Higher Stated Risk Preference is associated with greater AI Support.</strong></li>\n")
} else if (thisP < 0.05 && thisCoefficient < 0) {
  keyValuePairs[["RISK_TOPLINE_STATEMENT"]] = "<li><strong>Stated Risk Preference is a statistically significant predictor of AI Support.</strong></li>\n"
  keyValuePairs[["RISK_TOPLINE_STATEMENT"]] = paste(keyValuePairs[["RISK_TOPLINE_STATEMENT"]], "<li><strong>Lower Stated Risk Preference is associated with greater AI Support.</strong></li>\n")
} else {
  keyValuePairs[["RISK_TOPLINE_STATEMENT"]] = "<li>Stated Risk Preference is NOT a statistically significant predictor of AI Support.</li>"
}

# Add minor statements.
minorStatements = ""
minorStatements = paste0(minorStatements, "<li>Results update daily.</li>\n")
minorStatements = paste0(minorStatements, "<li>Based on responses from daily samples of American adults.</li>\n")
minorStatements = paste0(minorStatements, "<li>Aggregated over the entire data.</li>\n")
minorStatements = paste0(minorStatements, "<li>Data collection began: ", "2024-02-16", ".</li>\n")
minorStatements = paste0(minorStatements, "<li>Most recent data collected on: ", dataMostRecentDate, ".</li>\n")
totalObservations = aiDaily %>% filter(!is.na(Support)) %>% nrow()
keyValuePairs[["TOTAL_RESPONDENTS"]] = totalObservations
minorStatements = paste0(minorStatements, "<li>Total Observations: ", totalObservations, ".</li>\n")

keyValuePairs[["RISK_MINOR_STATEMENTS"]] = minorStatements

# Write the final list to a JSON file
write_json(keyValuePairs, paste0(baseDirectory, "json/", "overall-predict-risk.json"))

print(paste("create-overall-predict-risk.R wrote to", paste0(baseDirectory, "json/", "overall-predict-risk.json") ))

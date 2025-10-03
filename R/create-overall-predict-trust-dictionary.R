# This script creates key-value pairs and files for overall-predict-trust.html
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
keyValuePairs[["TRUST_HEX_COLOR"]] = "#8B4500"

## Generalized Trust

# Create short Trust label from Generalized_Trust response.
aiDaily = aiDaily %>%
  filter(!is.na(Support)) %>% 
  filter(!is.na(Generalized_Trust)) %>% 
  mutate(Trust = ifelse(Generalized_Trust == "Most people can be trusted.", "Trusting", NA) ) %>%
  mutate(Trust = ifelse(Generalized_Trust == "You can't be too careful in dealing with people.", "Careful", Trust) )
  
# Coarsen to monthly resolution.
# Instead of raw Obs_Date, mark each observation with the month.
aiDaily = aiDaily %>% mutate(Month = floor_date(Obs_Date, "month"))

# Number the months.  That will be useful for regression.
aiDaily = aiDaily %>% mutate(Month_Number = time_length(interval(min(Month), Month), "months") )

# Inspect.
#aiDaily %>% select(Month, Month_Number, Trust) %>% 
#   group_by(Month, Month_Number, Trust) %>% 
#   summarise(n = n())

# Visualize Trust x Month results.  Analysis follows.
aiDaily %>%
  select(Month, Trust, Support) %>%
  group_by(Month, Trust) %>%
  summarise(Support_Mean = mean(Support), 
            n = n(),
            SE = sd(Support, na.rm = TRUE) / sqrt(n),  # Standard Error
            CI_Lower = Support_Mean - qt(0.975, df = n - 1) * SE,
            CI_Upper = Support_Mean + qt(0.975, df = n - 1) * SE) %>%
  ggplot(aes(x = Month, y = Support_Mean, color = Trust)) +
  annotate(geom="rect", xmin = as.Date(-Inf), xmax = as.Date(Inf), ymin=0, ymax=Inf, fill="green", alpha=0.1) +
  annotate(geom="rect", xmin = as.Date(-Inf), xmax = as.Date(Inf), ymin=-Inf, ymax=0, fill="red", alpha=0.1) +
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2, position = position_dodge(width = 5)) +
  geom_point(size = 2, position = position_dodge(width = 5)) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +
  ggtitle("AI Support by Generalized Trust over Time", "Monthly mean agreement with 'I support further\ndevelopment of artificial intelligence.'") +
  xlab("Month") + ylab("Mean Response") +
  # Set a sensible set of breaks on the date x-axis.
  scale_x_date(date_breaks = "1 month", 
               date_labels = "%b\n%Y",
               minor_breaks = NULL) +
  # Force full y scale. Add numbers and labels.
  scale_y_continuous(limits = c(-3,3), breaks = -3:3, labels = c("Strongly disagree = -3", 
                                                                 "Disagree = -2", 
                                                                 "Somewhat disagree = -1", 
                                                                 "Neither agree nor disagree = 0", 
                                                                 "Somewhat agree = 1", 
                                                                 "Agree = 2", 
                                                                 "Strongly agree = 3")) +
  scale_color_manual(values = c("Careful" = "#006B6B", "Trusting" = "#8B4500") ) +
  labs(caption = paste0("Source: JJJ Pro Artificial Intelligence Daily Dashboard\n\uA9 Jason Jeffrey Jones")) +
  theme(plot.caption = element_text(size=10, color = "#666666"),
        legend.position = c(0.15, 0.25),
        legend.background = element_rect(fill = alpha("white", 0.8)),)
ggsave(paste0(baseDirectory, "images/", "trust-time-ai-relationship.svg"), width = 8.29, height = 4 )

keyValuePairs[["TRUST_FIGURE_FILENAME"]] = paste0("trust-time-ai-relationship.svg?v=", format(max(aiDaily$Obs_Date), "%Y-%m-%d"))

# Prepare statement.
trustFit = aiDaily %>%
  select(Month_Number, Trust, Support) %>%
  lm(formula = Support ~ Month_Number * Trust)

#summary(trustFit)

# Capture the summary output as text.
summaryOutput = capture.output(summary(trustFit))
keyValuePairs[["TRUST_LM_FIT_SUMMARY_TABLE"]] = paste0(summaryOutput, collapse = "\n")

# Set the results text programmatically.
summaryFit = summary(trustFit)

# Extract the coefficients table
coefficientsTable <- summaryFit$coefficients

# Print the coefficients table to see all variables
#print(coefficientsTable)

# Extract the coefficient and p-value for this variable.
trustCoefficient = coefficientsTable["TrustTrusting", "Estimate"]
trustP <- coefficientsTable["TrustTrusting", "Pr(>|t|)"]

# TRUST_TOPLINE_STATEMENT based on interpretation
if (trustP < 0.05 && trustCoefficient > 0) {
  keyValuePairs[["TRUST_TOPLINE_STATEMENT"]] = "<li><strong>Generalized Trust is a statistically significant predictor of AI Support.</strong></li>\n"
  keyValuePairs[["TRUST_TOPLINE_STATEMENT"]] = paste(keyValuePairs[["TRUST_TOPLINE_STATEMENT"]], "<li><strong>Higher Generalized Trust is associated with greater AI Support.</strong></li>\n")
} else if (trustP < 0.05 && trustCoefficient < 0) {
  keyValuePairs[["TRUST_TOPLINE_STATEMENT"]] = "<li><strong>Generalized Trust is a statistically significant predictor of AI Support.</strong></li>\n"
  keyValuePairs[["TRUST_TOPLINE_STATEMENT"]] = paste(keyValuePairs[["TRUST_TOPLINE_STATEMENT"]], "<li><strong>Lower Generalized Trust is associated with greater AI Support.</strong></li>\n")
} else {
  keyValuePairs[["TRUST_TOPLINE_STATEMENT"]] = "<li>Generalized Trust is NOT a statistically significant predictor of AI Support.</li>"
}

# Add time interpretation to TRUST_TOPLINE_STATEMENT
timeCoefficient = coefficientsTable["Month_Number", "Estimate"]
timeP <- coefficientsTable["Month_Number", "Pr(>|t|)"]

if (timeP < 0.05 && timeCoefficient > 0) {
  keyValuePairs[["TRUST_TOPLINE_STATEMENT"]] = paste(keyValuePairs[["TRUST_TOPLINE_STATEMENT"]], "<li><strong>Over time, AI Support increased.</strong></li>\n")
} else if (timeP < 0.05 && timeCoefficient < 0) {
  keyValuePairs[["TRUST_TOPLINE_STATEMENT"]] = paste(keyValuePairs[["TRUST_TOPLINE_STATEMENT"]], "<li><strong>Over time, AI Support decreased.</strong></li>\n")
} else {
  keyValuePairs[["TRUST_TOPLINE_STATEMENT"]] = "<li>Time is NOT a statistically significant predictor of AI Support.</li>"
}

# Add interaction interpretation to TRUST_TOPLINE_STATEMENT
# Extract interaction term (Month_Number:TrustTrusting)
interactionCoefficient <- coefficientsTable["Month_Number:TrustTrusting", "Estimate"]
interactionP <- coefficientsTable["Month_Number:TrustTrusting", "Pr(>|t|)"]

if (interactionP < 0.05) {
  keyValuePairs[["TRUST_TOPLINE_STATEMENT"]] = paste(keyValuePairs[["TRUST_TOPLINE_STATEMENT"]], "<li><strong>Careful respondents' AI Support and Trusting respondents' AI Support changed at different rates over time.</strong></li>")
}
# No else necessary. Say nothing if the interaction is not significant (p >= 0.05)


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

keyValuePairs[["TRUST_MINOR_STATEMENTS"]] = minorStatements

# Write the final list to a JSON file
write_json(keyValuePairs, paste0(baseDirectory, "json/", "overall-predict-trust.json"))

print(paste("create-overall-predict-trust.R wrote to", paste0(baseDirectory, "json/", "overall-predict-trust.json") ))

# This script creates key-value pairs and files for ai-support-trend.html
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

# Coarsen to monthly resolution.
# Instead of raw Obs_Date, mark each observation with the month.
aiDaily = aiDaily %>% mutate(Month = floor_date(Obs_Date, "month"))

# Number the months.  That will be useful for regression.
aiDaily = aiDaily %>% mutate(Month_Number = time_length(interval(min(Month), Month), "months") )

# Visualize trend results.  Analysis follows.
aiDaily %>%
  select(Month, Support) %>%
  group_by(Month) %>%
  summarise(Support_Mean = mean(Support, na.rm = TRUE), 
            n = n(),
            SE = sd(Support, na.rm = TRUE) / sqrt(n),  # Standard Error
            CI_Lower = Support_Mean - qt(0.975, df = n - 1) * SE,
            CI_Upper = Support_Mean + qt(0.975, df = n - 1) * SE) %>%
  ggplot(aes(x = Month, y = Support_Mean)) +
  annotate(geom="rect", xmin = as.Date(-Inf), xmax = as.Date(Inf), ymin=0, ymax=Inf, fill="green", alpha=0.1) +
  annotate(geom="rect", xmin = as.Date(-Inf), xmax = as.Date(Inf), ymin=-Inf, ymax=0, fill="red", alpha=0.1) +
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle("American Adults' AI Support over Time", "Monthly mean agreement with 'I support further\ndevelopment of artificial intelligence.'") +
  xlab("Month") + ylab("Mean Response") +
  # Set a sensible set of breaks on the date x-axis.
  scale_x_date(date_breaks = "1 month", 
               date_labels = "%b\n%Y",
               minor_breaks = NULL) +
  # Force full y scale to 1 through 7. Add numbers and labels.
  scale_y_continuous(limits = c(-3,3), breaks = -3:3, labels = c("Strongly disagree = -3", 
                                                                 "Disagree = -2", 
                                                                 "Somewhat disagree = -1", 
                                                                 "Neither agree nor disagree = 0", 
                                                                 "Somewhat agree = 1", 
                                                                 "Agree = 2", 
                                                                 "Strongly agree = 3")) +
  labs(caption = paste0("Source: JJJ Pro Artificial Intelligence Daily Dashboard\n\uA9 Jason Jeffrey Jones")) +
  theme(plot.caption = element_text(size=10, color = "#666666") )
ggsave(paste0(baseDirectory, "images/", "ai-support-trend.svg"), width = 8.29, height = 4 )

keyValuePairs[["TREND_FIGURE_FILENAME"]] = paste0("ai-support-trend.svg?v=", format(max(aiDaily$Obs_Date), "%Y-%m-%d"))

# Test for reliable effect of Month.
aiTrendMonthNumberFit = aiDaily %>%
  select(Month_Number, Support) %>%
  lm(formula = Support ~ Month_Number)

# summary(aiTrendMonthNumberFit)

# Capture the summary output as text
summary_output = capture.output(summary(aiTrendMonthNumberFit))

keyValuePairs[["TREND_LM_FIT_SUMMARY_TABLE"]] = paste0(summary_output, collapse = "\n")

# Set the results text programmatically.
summary_fit = summary(aiTrendMonthNumberFit)

# Extract the coefficients table
coefficients_table <- summary_fit$coefficients

# Print the coefficients table to see all variables
#print(coefficients_table)

# TREND_OVERALL_STATEMENT
# Extract the coefficient and p-value for Month_Number
month_number_coefficient <- coefficients_table["Month_Number", "Estimate"]
month_number_p_value <- coefficients_table["Month_Number", "Pr(>|t|)"]

if (month_number_p_value < 0.05 && month_number_coefficient > 0) {
  keyValuePairs[["TREND_OVERALL_STATEMENT"]] = "<strong>Yes, <span style=\"color: #056517;\">AI Support increased</span> over time.</strong>"
} else if (month_number_p_value < 0.05 && month_number_coefficient < 0) {
  keyValuePairs[["TREND_OVERALL_STATEMENT"]] = "<strong>Yes, <span style=\"color: #d60a22;\">AI Support decreased</span> over time.</strong>"
} else {
  keyValuePairs[["TREND_OVERALL_STATEMENT"]] = "<strong>No trend in AI Support reached statistical significance.</strong>"
}

# Add minor statements.
minorStatements = ""
minorStatements = paste0(minorStatements, "<li>Results update daily.</li>\n")
minorStatements = paste0(minorStatements, "<li>Based on responses from daily samples of American adults.</li>\n")
minorStatements = paste0(minorStatements, "<li>Aggregated to monthly estimates of AI Support.</li>\n")

annualRateChange = as.character(round(month_number_coefficient * 12.0, 3))
if (month_number_p_value < 0.05 && month_number_coefficient > 0) {
  minorStatements = paste0(minorStatements, "<li>The data suggest an <strong>annual</strong> rate of change of +", annualRateChange, ".</li>\n")
} else if (month_number_p_value < 0.05 && month_number_coefficient < 0) {
  minorStatements = paste0(minorStatements, "<li>The data suggest an <strong>annual</strong> rate of change of ", annualRateChange, ".</li>\n")
} else {
  # Say nothing if p >= 0.05
  minorStatements = paste0(minorStatements, "")
}

minorStatements = paste0(minorStatements, "<li>Data collection began: ", "2024-02-16", ".</li>\n")
minorStatements = paste0(minorStatements, "<li>Most recent data collected on: ", dataMostRecentDate, ".</li>\n")
totalObservations = aiDaily %>% filter(!is.na(Support)) %>% nrow()
keyValuePairs[["TOTAL_RESPONDENTS"]] = totalObservations
minorStatements = paste0(minorStatements, "<li>Total Observations: ", totalObservations, ".</li>\n")

keyValuePairs[["TREND_MINOR_STATEMENTS"]] = minorStatements

# Write the final list to a JSON file
write_json(keyValuePairs, paste0(baseDirectory, "json/", "ai-support-trend.json"))

print(paste("create-ai-support-trend-dictionary.R wrote to", paste0(baseDirectory, "json/", "ai-support-trend.json") ))

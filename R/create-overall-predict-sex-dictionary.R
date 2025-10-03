# This script creates key-value pairs and files for overall-predict-sex.html
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
keyValuePairs[["SEX_HEX_COLOR"]] = "#016062"

## Sex

# Filter to usable respondents.
aiDaily = aiDaily %>%
  filter(!is.na(Support)) %>% 
  filter(!is.na(Sex)) %>% 
  filter(Sex == "Female" | Sex == "Male")

# Count Female and Male.
keyValuePairs[["SEX_FEMALE_COUNT"]] = aiDaily %>% filter(Sex == "Female") %>% nrow()
keyValuePairs[["SEX_MALE_COUNT"]] = aiDaily %>% filter(Sex == "Male") %>% nrow()

# Coarsen to monthly resolution.
# Instead of raw Obs_Date, mark each observation with the month.
aiDaily = aiDaily %>% mutate(Month = floor_date(Obs_Date, "month"))

# Number the months.  That will be useful for regression.
aiDaily = aiDaily %>% mutate(Month_Number = time_length(interval(min(Month), Month), "months") )

# Inspect.
#aiDaily %>% select(Month, Month_Number, Sex) %>% 
#   group_by(Month, Month_Number, Sex) %>% 
#   summarise(n = n())

# Visualize Sex x Month results.  Analysis follows.
aiDaily %>%
  select(Month, Sex, Support) %>%
  group_by(Month, Sex) %>%
  summarise(Support_Mean = mean(Support), 
            n = n(),
            SE = sd(Support, na.rm = TRUE) / sqrt(n),  # Standard Error
            CI_Lower = Support_Mean - qt(0.975, df = n - 1) * SE,
            CI_Upper = Support_Mean + qt(0.975, df = n - 1) * SE) %>%
  ggplot(aes(x = Month, y = Support_Mean, color = Sex)) +
  annotate(geom="rect", xmin = as.Date(-Inf), xmax = as.Date(Inf), ymin=0, ymax=Inf, fill="green", alpha=0.1) +
  annotate(geom="rect", xmin = as.Date(-Inf), xmax = as.Date(Inf), ymin=-Inf, ymax=0, fill="red", alpha=0.1) +
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2, position = position_dodge(width = 5)) +
  geom_point(size = 2, position = position_dodge(width = 5)) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +
  ggtitle("AI Support by Sex over Time", "Monthly mean agreement with 'I support further\ndevelopment of artificial intelligence.'") +
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
  scale_color_manual(values = c("Female" = "#1FC3AA", "Male" = "#8624F5")) +
  labs(caption = paste0("Source: JJJ Pro Artificial Intelligence Daily Dashboard\n\uA9 Jason Jeffrey Jones")) +
  theme(plot.caption = element_text(size=10, color = "#666666"),
        legend.position = c(0.15, 0.25),
        legend.background = element_rect(fill = alpha("white", 0.8)),)
ggsave(paste0(baseDirectory, "images/", "sex-time-ai-relationship.svg"), width = 8.29, height = 4 )

keyValuePairs[["SEX_FIGURE_FILENAME"]] = paste0("sex-time-ai-relationship.svg?v=", format(max(aiDaily$Obs_Date), "%Y-%m-%d"))

# Prepare statement.
sexFit = aiDaily %>%
  select(Month_Number, Sex, Support) %>%
  lm(formula = Support ~ Month_Number * Sex)

#summary(sexFit)

# Capture the summary output as text.
summaryOutput = capture.output(summary(sexFit))
keyValuePairs[["SEX_LM_FIT_SUMMARY_TABLE"]] = paste0(summaryOutput, collapse = "\n")

# Set the results text programmatically.
summaryFit = summary(sexFit)

# Extract the coefficients table
coefficientsTable <- summaryFit$coefficients

# Print the coefficients table to see all variables
#print(coefficientsTable)

# Extract the coefficient and p-value for this variable.
sexCoefficient = coefficientsTable["SexMale", "Estimate"]
sexP <- coefficientsTable["SexMale", "Pr(>|t|)"]

# SEX_TOPLINE_STATEMENT based on interpretation
if (sexP < 0.05 && sexCoefficient > 0) {
  keyValuePairs[["SEX_TOPLINE_STATEMENT"]] = "<li><strong>Sex is a statistically significant predictor of AI Support.</strong></li>\n"
  keyValuePairs[["SEX_TOPLINE_STATEMENT"]] = paste(keyValuePairs[["SEX_TOPLINE_STATEMENT"]], "<li><strong>Males expressed greater AI Support.</strong></li>\n")
} else if (sexP < 0.05 && sexCoefficient < 0) {
  keyValuePairs[["SEX_TOPLINE_STATEMENT"]] = "<li><strong>Sex is a statistically significant predictor of AI Support.</strong></li>\n"
  keyValuePairs[["SEX_TOPLINE_STATEMENT"]] = paste(keyValuePairs[["SEX_TOPLINE_STATEMENT"]], "<li><strong>Females expressed greater AI Support.</strong></li>\n")
} else {
  keyValuePairs[["SEX_TOPLINE_STATEMENT"]] = "<li>Sex is NOT a statistically significant predictor of AI Support.</li>"
}

# Add time interpretation to SEX_TOPLINE_STATEMENT
timeCoefficient = coefficientsTable["Month_Number", "Estimate"]
timeP <- coefficientsTable["Month_Number", "Pr(>|t|)"]

if (timeP < 0.05 && timeCoefficient > 0) {
  keyValuePairs[["SEX_TOPLINE_STATEMENT"]] = paste(keyValuePairs[["SEX_TOPLINE_STATEMENT"]], "<li><strong>Over time, AI Support increased.</strong></li>\n")
} else if (timeP < 0.05 && timeCoefficient < 0) {
  keyValuePairs[["SEX_TOPLINE_STATEMENT"]] = paste(keyValuePairs[["SEX_TOPLINE_STATEMENT"]], "<li><strong>Over time, AI Support decreased.</strong></li>\n")
} else {
  keyValuePairs[["SEX_TOPLINE_STATEMENT"]] = "<li>Time is NOT a statistically significant predictor of AI Support.</li>"
}

# Add interaction interpretation to SEX_TOPLINE_STATEMENT
# Extract interaction term (Month_Number:SexMale)
interactionCoefficient <- coefficientsTable["Month_Number:SexMale", "Estimate"]
interactionP <- coefficientsTable["Month_Number:SexMale", "Pr(>|t|)"]

if (interactionP < 0.05) {
  keyValuePairs[["SEX_TOPLINE_STATEMENT"]] = paste(keyValuePairs[["SEX_TOPLINE_STATEMENT"]], "<li><strong>Male and Female respondents' AI Support changed at different rates over time.</strong></li>")
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

keyValuePairs[["SEX_MINOR_STATEMENTS"]] = minorStatements

# Write the final list to a JSON file
write_json(keyValuePairs, paste0(baseDirectory, "json/", "overall-predict-sex.json"))

print(paste("create-overall-predict-sex.R wrote to", paste0(baseDirectory, "json/", "overall-predict-sex.json") ))

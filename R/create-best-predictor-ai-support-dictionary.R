# This script creates key-value pairs and files for best-predictor-ai-support.html
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

# Create a coalesced Party variable.
aiDaily = aiDaily %>% mutate(Party = Party_1) %>% 
  mutate(Party = if_else(Party_2 == "Lean Democratic", "Democrat", Party, missing = Party) ) %>% 
  mutate(Party = if_else(Party_2 == "Lean Republican", "Republican", Party, missing = Party) )

# Create a more concise Generalized_Trust variable.
aiDaily = aiDaily %>% mutate(Generalized_Trust = ifelse(Generalized_Trust == "Most people can be trusted.", "Trusting", "Careful") ) %>% 
  mutate(Generalized_Trust = factor(Generalized_Trust, levels = c("Careful", "Trusting")) )

# TODO Choose colors for each variable.
# TODO use them for points and lines below.
keyValuePairs[["RISK_HEX_COLOR"]] = "#B22222"
keyValuePairs[["TRUST_HEX_COLOR"]] = "#8B4500"


### Begin single-variable models.

## Risk Preference

# Visualize single-variable relationship with Support.
aiDaily %>%
  select(Risk_Preference, Support) %>%
  filter(!is.na(Support)) %>% 
  group_by(Risk_Preference) %>%
  summarise(Support_Mean = mean(Support), 
            n = n()) %>%
  ggplot(aes(x = Risk_Preference, y = Support_Mean)) +
  annotate(geom="rect", xmin = -Inf, xmax = Inf, ymin=0, ymax=Inf, fill="green", alpha=0.1) +
  annotate(geom="rect", xmin = -Inf, xmax = Inf, ymin=-Inf, ymax=0, fill="red", alpha=0.1) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +
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

# Extract the residuals from the model
#residuals <- residuals(riskFit)

# Calculate RMSE
#rmse <- sqrt(mean(residuals^2))
#rmse

#r_squared <- summary_fit$r.squared
#r_squared = round(100 * r_squared, 1)

#keyValuePairs[["POLARIZATION_TOPLINE_STATEMENT"]] = paste0("<li><strong>Together, political party affiliation and time explain ", r_squared, "% of the variation in AI Support.</strong></li>")


## Generalized Trust

# Visualize single-variable relationship with Support.
aiDaily %>%
  select(Generalized_Trust, Support) %>%
  filter(!is.na(Generalized_Trust)) %>% 
  filter(!is.na(Support)) %>% 
  group_by(Generalized_Trust) %>%
  summarise(Support_Mean = mean(Support), 
            sd = sd(Support),
            n = n(),
            se = sd / sqrt(n),
            error_low = Support_Mean - se,
            error_high = Support_Mean + se
  ) %>%
  ggplot(aes(x = Generalized_Trust, y = Support_Mean, shape=Generalized_Trust, color=Generalized_Trust, group=Generalized_Trust)) +
  annotate(geom="rect", xmin = -Inf, xmax = Inf, ymin=0, ymax=Inf, fill="green", alpha=0.1) +
  annotate(geom="rect", xmin = -Inf, xmax = Inf, ymin=-Inf, ymax=0, fill="red", alpha=0.1) +
  geom_pointrange(aes(ymin = error_low, ymax = error_high), linewidth = 1 ) + 
  scale_color_manual(values = c("Careful" = "#0047AB", "Trusting" = "darkorange4")) +
  ggtitle("AI Support by Generalized Trust", "Mean agreement with 'I support further\ndevelopment of artificial intelligence.'") +
  xlab("Generalized Trust\nCareful means: 'You can't be too careful in dealing with people.'\nTrusting means: 'Most people can be trusted.'") + 
  ylab("Mean Response") +
  # Force full y scale to -3 through +3. Add numbers and labels.
  scale_y_continuous(limits = c(-3,3), breaks = -3:3, labels = c("Strongly disagree = -3", 
                                                                 "Disagree = -2", 
                                                                 "Somewhat disagree = -1", 
                                                                 "Neither agree nor disagree = 0", 
                                                                 "Somewhat agree = 1", 
                                                                 "Agree = 2", 
                                                                 "Strongly agree = 3")) +
  labs(caption = paste0("Source: JJJ Pro Artificial Intelligence Daily Dashboard\n\uA9 Jason Jeffrey Jones")) +
  theme(plot.caption = element_text(size=10, color = "#666666")) +
  theme(legend.position = "none")
ggsave(paste0(baseDirectory, "images/", "trust-ai-relationship.svg"), width = 8.29, height = 4 )

keyValuePairs[["TRUST_FIGURE_FILENAME"]] = paste0("trust-ai-relationship.svg?v=", format(max(aiDaily$Obs_Date), "%Y-%m-%d"))

# Prepare statement.
trustFit = aiDaily %>%
  select(Generalized_Trust, Support) %>%
  filter(!is.na(Generalized_Trust)) %>% 
  filter(!is.na(Support)) %>% 
  lm(formula = Support ~ Generalized_Trust)

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
thisCoefficient = coefficientsTable["Generalized_TrustTrusting", "Estimate"]
thisP <- coefficientsTable["Generalized_TrustTrusting", "Pr(>|t|)"]

# TRUST_TOPLINE_STATEMENT based on interpretation
if (thisP < 0.05 && thisCoefficient > 0) {
  keyValuePairs[["TRUST_TOPLINE_STATEMENT"]] = "<li><strong>Generalized Trust is a statistically significant predictor of AI Support.</strong></li>\n"
  keyValuePairs[["TRUST_TOPLINE_STATEMENT"]] = paste(keyValuePairs[["TRUST_TOPLINE_STATEMENT"]], "<li><strong>'You can't be too careful in dealing with people.' is associated with greater AI Support.</strong></li>\n")
} else if (thisP < 0.05 && thisCoefficient < 0) {
  keyValuePairs[["TRUST_TOPLINE_STATEMENT"]] = "<li><strong>Generalized Trust is a statistically significant predictor of AI Support.</strong></li>\n"
  keyValuePairs[["TRUST_TOPLINE_STATEMENT"]] = paste(keyValuePairs[["TRUST_TOPLINE_STATEMENT"]], "<li><strong>'Most people can be trusted.' is associated with greater AI Support.</strong></li>\n")
} else {
  keyValuePairs[["TRUST_TOPLINE_STATEMENT"]] = "<li>Generalized Trust is NOT a statistically significant predictor of AI Support.</li>"
}



# Write the final list to a JSON file
write_json(keyValuePairs, paste0(baseDirectory, "json/", "best-predictor-ai-support.json"))

print(paste("create-best-predictor-ai-support.R wrote to", paste0(baseDirectory, "json/", "best-predictor-ai-support.json") ))

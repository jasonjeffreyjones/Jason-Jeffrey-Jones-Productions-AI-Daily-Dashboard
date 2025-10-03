# This script creates key-value pairs and files for ai-polarization.html
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

# Inspect the coalesce results.
# aiDaily %>% select(Party_1, Party_2, Party) %>% 
#   slice_sample(n=20) %>% 
#   print(n=Inf)
# 
# aiDaily %>% select(Party_1, Party_2, Party) %>% 
#   group_by(Party_1, Party_2, Party) %>% 
#   summarise(n = n())

# Coalesce worked as expected.  How many for each final value of Party?
# aiDaily %>% select(Party) %>% 
#   group_by(Party) %>% 
#   summarise(n = n())

# Coarsen to monthly resolution.
# Instead of raw Obs_Date, mark each observation with the month.
aiDaily = aiDaily %>% mutate(Month = floor_date(Obs_Date, "month"))

# Inspect results.
# typeof(aiDaily$Month)
# class(aiDaily$Month)
# aiDaily %>% group_by(Month) %>%
#   summarize(n = n())

# Number the months.  That will be useful for regression.
aiDaily = aiDaily %>% mutate(Month_Number = time_length(interval(min(Month), Month), "months") )

# Inspect.
# aiDaily %>% select(Month, Month_Number) %>% 
#   group_by(Month, Month_Number) %>% 
#   summarise(n = n())

# Visualize polarization results.  Analysis follows.
aiDaily %>%
  select(Month, Party, Support) %>%
  filter(Party %in% c("Democrat", "Republican")) %>% 
  group_by(Month, Party) %>%
  summarise(Support_Mean = mean(Support, na.rm = TRUE), 
            n = n()) %>%
  ggplot(aes(x = Month, y = Support_Mean, color = Party, shape = Party)) +
  annotate(geom="rect", xmin = as.Date(-Inf), xmax = as.Date(Inf), ymin=0, ymax=Inf, fill="green", alpha=0.1) +
  annotate(geom="rect", xmin = as.Date(-Inf), xmax = as.Date(Inf), ymin=-Inf, ymax=0, fill="red", alpha=0.1) +
  geom_point(size = 2.5, position = position_dodge(width = 5)) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +
  ggtitle("AI Support by Political Party over Time", "Monthly mean agreement with 'I support further\ndevelopment of artificial intelligence.'") +
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
  scale_shape_manual(values = c("Democrat" = 16, "Republican" = 18) ) +
  scale_color_manual(values = c("Democrat" = "#2B83BA", "Republican" = "#D73027") ) +
  labs(caption = paste0("Source: JJJ Pro Artificial Intelligence Daily Dashboard\n\uA9 Jason Jeffrey Jones")) +
  theme(plot.caption = element_text(size=10, color = "#666666"),
        legend.position = c(0.15, 0.25),
        legend.background = element_rect(fill = alpha("white", 0.8)),)
ggsave(paste0(baseDirectory, "images/", "ai-polarization.svg"), width = 8.29, height = 4 )

keyValuePairs[["POLARIZATION_FIGURE_FILENAME"]] = paste0("ai-polarization.svg?v=", format(max(aiDaily$Obs_Date), "%Y-%m-%d"))

keyValuePairs[["DEMOCRATS_HEX_COLOR"]] = "#2B83BA"
keyValuePairs[["REPUBLICANS_HEX_COLOR"]] = "#D73027"

# Look for effects of Party, Month and an interaction.
aiPolarizationMonthNumberFit = aiDaily %>%
  select(Month_Number, Party, Support) %>%
  filter(Party %in% c("Democrat", "Republican")) %>% 
  lm(formula = Support ~ Month_Number * Party)

# summary(aiPolarizationMonthNumberFit)

# Capture the summary output as text
summary_output = capture.output(summary(aiPolarizationMonthNumberFit))

keyValuePairs[["LM_FIT_SUMMARY_TABLE"]] = paste0(summary_output, collapse = "\n")

# Set the results text programmatically.
summary_fit = summary(aiPolarizationMonthNumberFit)

# Extract the coefficients table
coefficients_table <- summary_fit$coefficients

# Print the coefficients table to see all variables
#print(coefficients_table)

# POLARIZATION_INTERACTION_STATEMENT
# Extract the coefficient and p-value for the interaction term (Month_Number:PartyRepublican)
interaction_coefficient <- coefficients_table["Month_Number:PartyRepublican", "Estimate"]
interaction_p_value <- coefficients_table["Month_Number:PartyRepublican", "Pr(>|t|)"]

# POLARIZATION_INTERACTION_STATEMENT based on interaction interpretation
if (interaction_p_value < 0.05) {
  keyValuePairs[["POLARIZATION_INTERACTION_STATEMENT"]] = "<li><strong>Republican AI Support and Democrat AI Support changed at different rates over time.</strong></li>"
} else {
  # Say nothing if the interaction is not significant (p >= 0.05)
  keyValuePairs[["POLARIZATION_INTERACTION_STATEMENT"]] = ""
}

# Extract the coefficient and p-value for Party (e.g., PartyRepublican)
# Assuming the base level is "Democrat" and the contrast is "Republican"
party_coefficient <- coefficients_table["PartyRepublican", "Estimate"]
party_p_value <- coefficients_table["PartyRepublican", "Pr(>|t|)"]

# POLARIZATION_OVERALL_STATEMENT
if (party_p_value < 0.05 && party_coefficient > 0) {
  keyValuePairs[["POLARIZATION_OVERALL_STATEMENT"]] = "<li><strong>Initially, Republicans more strongly supported AI development than Democrats.</strong></li>"
} else if (party_p_value < 0.05 && party_coefficient < 0) {
  keyValuePairs[["POLARIZATION_OVERALL_STATEMENT"]] = "<li><strong>Initially, Democrats more strongly supported AI development than Republicans.</strong></li>"
} else {
  # Say nothing if p >= 0.05
  keyValuePairs[["POLARIZATION_OVERALL_STATEMENT"]] = ""
}

# Extract the coefficient and p-value for Month_Number
month_number_coefficient <- coefficients_table["Month_Number", "Estimate"]
month_number_p_value <- coefficients_table["Month_Number", "Pr(>|t|)"]

# MONTH_TREND_STATEMENT
if (month_number_p_value < 0.05 && month_number_coefficient > 0) {
  keyValuePairs[["MONTH_TREND_STATEMENT"]] = "<li><strong>Over time, AI Support increased.</strong></li>"
} else if (month_number_p_value < 0.05 && month_number_coefficient < 0) {
  keyValuePairs[["MONTH_TREND_STATEMENT"]] = "<li><strong>Over time, AI Support decreased.</strong></li>"
} else {
  # Say nothing if p >= 0.05
  keyValuePairs[["MONTH_TREND_STATEMENT"]] = ""
}

r_squared <- summary_fit$r.squared
r_squared = round(100 * r_squared, 1)

keyValuePairs[["POLARIZATION_TOPLINE_STATEMENT"]] = paste0("<li><strong>Together, political party affiliation and time explain ", r_squared, "% of the variation in AI Support.</strong></li>")

# Write the final list to a JSON file
write_json(keyValuePairs, paste0(baseDirectory, "json/", "ai-polarization.json"))

print(paste("create-ai-polarization-dictionary.R wrote to", paste0(baseDirectory, "json/", "ai-polarization.json") ))

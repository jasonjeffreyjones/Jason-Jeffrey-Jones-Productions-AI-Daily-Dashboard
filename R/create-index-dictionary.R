# This script creates key-value pairs and files for index.html
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

# Load data.
aiDaily = read_csv(paste0(baseDirectory, "data/qualtrics-responses.csv"), show_col_types = FALSE)

# Some presets for a daily visualization.
dailyStartDate = as.Date("2024-04-18")

# Now that we have more than one year of data, use rolling 366-day window.
dailyMostRecentDate = max(aiDaily$Obs_Date)
reachBackToDate = dailyMostRecentDate - days(366)

# If day of the month is less than five, we need to add right margin.
rightMarginAdd = ifelse(day(dailyMostRecentDate) < 5, 10, 0)
# Also an issue at the end of each month :)
rightMarginAdd = ifelse(day(dailyMostRecentDate) >= 28, 10, rightMarginAdd)

aiDaily %>% 
  select(Obs_Date, Support) %>%
  filter(Obs_Date >= reachBackToDate) %>% 
  group_by(Obs_Date) %>%
  summarise(Support_Mean = mean(Support, na.rm = TRUE), 
            n = n()) %>%
  ggplot(aes(x = Obs_Date, y = Support_Mean)) +
  annotate(geom="rect", xmin = as.Date(-Inf), xmax = as.Date(Inf), ymin=0, ymax=Inf, fill="green", alpha=0.1) +
  annotate(geom="rect", xmin = as.Date(-Inf), xmax = as.Date(Inf), ymin=-Inf, ymax=0, fill="red", alpha=0.1) +
  #geom_point() +
  geom_line() +
  ggtitle("'I support further development of artificial intelligence.'", "Mean response, seven-point scale from -3 to +3") +
  xlab("Survey Date") + ylab("Mean Response") +
  # Set a sensible set of breaks on the date x-axis.
  scale_x_date(limits = c(reachBackToDate, dailyMostRecentDate), date_breaks = "1 months", minor_breaks = NULL, date_labels = "%b %d\n%Y", expand = expansion(add = 1) ) +
  # Force full y scale to 1 through 7. Add numbers and labels.
  scale_y_continuous(limits = c(-3,3), breaks = -3:3, labels = c("Strongly disagree = -3", 
                                                                 "Disagree = -2", 
                                                                 "Somewhat disagree = -1", 
                                                                 "Neither agree nor disagree = 0", 
                                                                 "Somewhat agree = 1", 
                                                                 "Agree = 2", 
                                                                 "Strongly agree = 3")) +
  # Increase right margin if needed, holding others at whatever value they currently contain.
  theme(plot.margin = margin(
    t = theme_get()$plot.margin[[1]],
    r = theme_get()$plot.margin[[2]] + unit(rightMarginAdd, "points"),
    b = theme_get()$plot.margin[[3]],
    l = theme_get()$plot.margin[[4]]
  )) + 
  labs(caption = paste0("Source: JJJ Pro Artificial Intelligence Daily Dashboard\n\uA9 Jason Jeffrey Jones")) +
  theme(plot.caption = element_text(size=10, color = "#666666"))
ggsave(paste0(baseDirectory, "images/", "daily-support.svg"), width = 8.29, height = 4 )

# Save a key-value pair
keyValuePairs[["DAILY_SUPPORT_FIGURE_FILENAME"]] = paste0("daily-support.svg?v=", format(dailyMostRecentDate, "%Y-%m-%d"))

# Let us use the series of daily means and calculate some interesting stats.
aiDailySupportMeans = aiDaily %>% 
  select(Obs_Date, Support) %>%
  filter(Obs_Date >= reachBackToDate) %>% 
  group_by(Obs_Date) %>%
  summarise(Support_Mean = mean(Support, na.rm = TRUE), 
            n = n()) %>%
  arrange(Obs_Date)
  
keyValuePairs[["TODAY_DATE"]] = aiDailySupportMeans %>% 
  slice_tail(n = 1) %>% 
  pull(Obs_Date)

keyValuePairs[["TODAY_MEAN"]] = aiDailySupportMeans %>% 
  slice_tail(n = 1) %>% 
  pull(Support_Mean) %>% 
  round(digits = 2) %>% 
  format(nsmall = 2)

keyValuePairs[["PREVIOUS_DAY_MEAN"]] = aiDailySupportMeans %>% 
  slice_tail(n = 2) %>% # Last two rows.
  slice_head(n = 1) %>% # Only the second-to-last row.
  pull(Support_Mean) %>% 
  round(digits = 2) %>% 
  format(nsmall = 2)

dayDeltaRaw = as.numeric(keyValuePairs[["TODAY_MEAN"]]) - as.numeric(keyValuePairs[["PREVIOUS_DAY_MEAN"]])
dayDeltaRaw = dayDeltaRaw %>% round(digits = 2) %>% format(nsmall = 2)
keyValuePairs[["DAY_DELTA_RAW"]] = ifelse(as.numeric(dayDeltaRaw) > 0, paste0("+", dayDeltaRaw), dayDeltaRaw)

keyValuePairs[["INCREASED_OR_DECREASED"]] = ifelse(as.numeric(dayDeltaRaw) > 0, "INCREASED", "DECREASED")
keyValuePairs[["INCREASED_OR_DECREASED_HEX_COLOR"]] = ifelse(as.numeric(dayDeltaRaw) > 0, "#056517", "#d60a22")
# Handle the possibility that today exactly equals previous day.
keyValuePairs[["INCREASED_OR_DECREASED"]] = ifelse(as.numeric(dayDeltaRaw) == 0, "WAS UNCHANGED", keyValuePairs[["INCREASED_OR_DECREASED"]])
keyValuePairs[["INCREASED_OR_DECREASED_HEX_COLOR"]] = ifelse(as.numeric(dayDeltaRaw) == 0, "#666666", keyValuePairs[["INCREASED_OR_DECREASED_HEX_COLOR"]])

# TODO percentage doesn't make sense in this range. Don't care for now.
#dayDeltaPercent = as.numeric(keyValuePairs[["TODAY_MEAN"]]) - as.numeric(keyValuePairs[["PREVIOUS_DAY_MEAN"]])
#dayDeltaPercent = 100 * (dayDeltaPercent / as.numeric(keyValuePairs[["PREVIOUS_DAY_MEAN"]]) )
#dayDeltaPercent = dayDeltaPercent %>% round(digits = 2) %>% format(nsmall = 2)
#keyValuePairs[["DAY_DELTA_PERCENT"]] = ifelse(as.numeric(dayDeltaPercent) > 0, paste0("+", dayDeltaPercent), dayDeltaPercent)

keyValuePairs[["365_DAY_LOW"]] = aiDailySupportMeans %>% 
  slice_tail(n = 365) %>% 
  filter(Support_Mean == min(Support_Mean, na.rm = TRUE)) %>% 
  slice_tail(n = 1) %>% # Most recent if there were ties.
  mutate(Support_Mean = format(round(Support_Mean, 2), nsmall = 2) ) %>% 
  mutate(Date_and_Value = paste0(Support_Mean, "<br>on ", as.character(Obs_Date) ) ) %>% 
  pull(Date_and_Value)

keyValuePairs[["365_DAY_HIGH"]] = aiDailySupportMeans %>% 
  slice_tail(n = 365) %>% 
  filter(Support_Mean == max(Support_Mean, na.rm = TRUE)) %>% 
  slice_tail(n = 1) %>% # Most recent if there were ties.
  mutate(Support_Mean = format(round(Support_Mean, 2), nsmall = 2) ) %>% 
  mutate(Date_and_Value = paste0(Support_Mean, "<br>on ", as.character(Obs_Date) ) ) %>% 
  pull(Date_and_Value)

# Write the final list to a JSON file
write_json(keyValuePairs, paste0(baseDirectory, "json/", "index.json"))

print(paste("create-index-dictionary.R wrote to", paste0(baseDirectory, "json/", "index.json") ))

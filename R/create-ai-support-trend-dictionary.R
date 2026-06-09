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
suppressMessages(library(lubridate))
suppressMessages(library(scales))

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
  scale_x_date(date_breaks = "2 months", 
               date_labels = "%b\n%Y",
               date_minor_breaks = "1 month") +
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

# Additional visualizations.  These will be in a carousel on the dashboard.
# Keep only valid support responses for plotting/distribution summaries.
aiDailyValid = aiDaily %>%
  filter(!is.na(Support), Support %in% -3:3)

# -----------------------------
# Figure 2: stacked proportions by month
# reds on bottom, greens on top
# -----------------------------
support_breaks <- c(
  "Strongly disagree",
  "Disagree",
  "Somewhat disagree",
  "Neither agree nor disagree",
  "Somewhat agree",
  "Agree",
  "Strongly agree"
)

supportLevelColors <- c(
  "Strongly disagree"          = "#7f0000",  # darkest red
  "Disagree"                   = "#cb181d",
  "Somewhat disagree"          = "#fcae91",
  "Neither agree nor disagree" = "#fff7bc",  # pale yellow
  "Somewhat agree"             = "#c7e9c0",
  "Agree"                      = "#41ab5d",
  "Strongly agree"             = "#005a32"   # darkest green
)

monthlyComposition <- aiDailyValid %>%
  mutate(
    Support_Factor = factor(
      Support,
      levels = c(-3, -2, -1, 0, 1, 2, 3),
      labels = support_breaks
    )
  ) %>%
  count(Month, Support_Factor, .drop = FALSE) %>%
  group_by(Month) %>%
  mutate(Proportion = n / sum(n)) %>%
  ungroup() %>%
  arrange(Month)

monthAxisTable <- monthlyComposition %>%
  distinct(Month) %>%
  arrange(Month) %>%
  mutate(
    Month_Label = format(Month, "%b\n%Y"),
    Axis_Label  = if_else(row_number() %% 2 == 1, Month_Label, "")
  )

monthlyComposition <- monthlyComposition %>%
  left_join(monthAxisTable, by = "Month") %>%
  mutate(
    Month_Label = factor(Month_Label, levels = monthAxisTable$Month_Label)
  )

stackedPlot <- monthlyComposition %>%
  ggplot(aes(x = Month_Label, y = Proportion, fill = Support_Factor)) +
  geom_col(width = 1, position = position_stack(reverse = TRUE)) +
  ggtitle(
    "Distribution of AI Support Responses over Time",
    "Monthly proportions at each response level"
  ) +
  xlab("Month") +
  ylab("Proportion of responses") +
  scale_x_discrete(
    labels = setNames(monthAxisTable$Axis_Label, monthAxisTable$Month_Label),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    labels = scales::percent_format(accuracy = 1),
    expand = c(0, 0)
  ) +
  scale_fill_manual(
    values = supportLevelColors,
    breaks = support_breaks,
    limits = support_breaks,
    drop = FALSE
  ) +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(
    fill = "Response level",
    caption = paste0(
      "Source: JJJ Pro Artificial Intelligence Daily Dashboard\n\uA9 Jason Jeffrey Jones"
    )
  ) +
  theme(
    plot.caption = element_text(size = 10, color = "#666666")
  )

ggsave(
  paste0(baseDirectory, "images/", "ai-support-distribution.svg"),
  plot = stackedPlot,
  width = 8.29,
  height = 4
)

# -----------------------------
# Figure 3: zoomed mean plot with path under points
# -----------------------------
monthlySummary <- aiDailyValid %>%
  group_by(Month) %>%
  summarise(
    Support_Mean = mean(Support),
    n = n(),
    SE = sd(Support) / sqrt(n),
    CI_Lower = Support_Mean - qt(0.975, df = n - 1) * SE,
    CI_Upper = Support_Mean + qt(0.975, df = n - 1) * SE,
    .groups = "drop"
  )

yMinZoom <- floor(min(monthlySummary$CI_Lower, na.rm = TRUE))
yMaxZoom <- ceiling(max(monthlySummary$CI_Upper, na.rm = TRUE))

zoomedTrendPlot <- monthlySummary %>%
  ggplot(aes(x = Month, y = Support_Mean)) +
  annotate(
    geom = "rect",
    xmin = as.Date(-Inf), xmax = as.Date(Inf),
    ymin = 0, ymax = Inf,
    fill = "green", alpha = 0.1
  ) +
  annotate(
    geom = "rect",
    xmin = as.Date(-Inf), xmax = as.Date(Inf),
    ymin = -Inf, ymax = 0,
    fill = "red", alpha = 0.1
  ) +
  geom_path(linewidth = 0.7) +
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2) +
  geom_point(size = 2) +
  ggtitle(
    "American Adults' AI Support over Time (Zoom)",
    "Monthly mean agreement with 'I support further\ndevelopment of artificial intelligence.'"
  ) +
  xlab("Month") +
  ylab("Mean Response") +
  scale_x_date(
    date_breaks = "2 months",
    date_labels = "%b\n%Y",
    date_minor_breaks = "1 month"
  ) +
  scale_y_continuous(
    limits = c(yMinZoom, yMaxZoom),
    breaks = seq(yMinZoom, yMaxZoom, by = 1)
  ) +
  labs(caption = paste0(
    "Full scale: -3 Strongly disagree to +3 Strongly agree.\nSource: JJJ Pro Artificial Intelligence Daily Dashboard\n\uA9 Jason Jeffrey Jones"
  )) +
  theme(
    plot.caption = element_text(size = 10, color = "#666666")
  )

ggsave(
  paste0(baseDirectory, "images/", "ai-support-trend-zoom.svg"),
  plot = zoomedTrendPlot,
  width = 8.29,
  height = 4
)

# Add figure filenames to dictionary.
cacheBustDate <- format(max(aiDaily$Obs_Date), "%Y-%m-%d")

keyValuePairs[["TREND_FIGURE_FILENAME"]] =
  paste0("ai-support-trend.svg?v=", cacheBustDate)

keyValuePairs[["TREND_FIGURE_2_FILENAME"]] =
  paste0("ai-support-distribution.svg?v=", cacheBustDate)

keyValuePairs[["TREND_FIGURE_3_FILENAME"]] =
  paste0("ai-support-trend-zoom.svg?v=", cacheBustDate)

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

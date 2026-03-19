# Install required packages if missing
if(!require(readr)) install.packages("readr")
if(!require(dplyr)) install.packages("dplyr")
if(!require(ggplot2)) install.packages("ggplot2")

library(readr)
library(dplyr)
library(ggplot2)

# Load the data collected from the Game (Exported from Supabase)
# Assumes you downloaded your Supabase 'trials' table as 'trials_data.csv'
if(!file.exists("Study/trials_rows.csv")) {
  stop("Please place the 'trials_data.csv' file in the Study folder before running.")
}

trials <- read_csv("Study/trials_rows.csv", show_col_types = FALSE)

cat("=== OVERALL BEHAVIORAL PERFORMANCE ===\n")
overall_acc <- mean(trials$correct, na.rm = TRUE)
cat(sprintf("Overall Prediction Accuracy: %.2f%%\n", overall_acc * 100))

cat("\n=== EXPERIMENTAL MANIPULATION: JIF VISIBILITY ===\n")
# Does seeing the journal name change prediction accuracy?
manipulation_stats <- trials %>%
  group_by(if_visible) %>%
  summarize(
    trials_count = n(),
    accuracy = mean(correct, na.rm = TRUE),
    avg_rt_ms = mean(reaction_time, na.rm = TRUE)
  )
print(manipulation_stats)

cat("\n=== PERFORMANCE BY CAREER LEVEL ===\n")
career_stats <- trials %>%
  group_by(career) %>%
  summarize(
    trials_count = n(),
    accuracy = mean(correct, na.rm = TRUE)
  ) %>%
  arrange(desc(accuracy))
print(career_stats)

# Plot: Effect of Journal Visibility on Accuracy
p1 <- ggplot(manipulation_stats, aes(x = as.factor(if_visible), y = accuracy, fill = as.factor(if_visible))) +
  geom_bar(stat = "identity", width = 0.6) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  labs(title = "Effect of JIF Visibility on Prediction Accuracy",
       x = "JIF Visible to Player",
       y = "Accuracy (%)",
       fill = "Visible") +
  theme_minimal()

ggsave("accuracy_by_visibility.png", plot = p1, width = 6, height = 4)
cat("Saved plot to accuracy_by_visibility.png\n")

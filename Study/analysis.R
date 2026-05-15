# ==============================================================================
# 1. SETUP AND PACKAGES ----
# ==============================================================================

# if(!require(renv)) install.packages("renv") save current environment with renv so that project stays reproducible
# library(renv)
# renv::snapshot()

# Install required packages if missing
# if(!require(readr)) install.packages("readr")
# if(!require(dplyr)) install.packages("dplyr")
# if(!require(ggplot2)) install.packages("ggplot2")
# if(!require(skimr)) install.packages("skimr")
# if(!require(tidyverse)) install.packages("tidyverse")
# if(!require(quanteda)) install.packages("quanteda", type = "binary")
# if(!require(quanteda.textstats)) install.packages("quanteda.textstats")
# if(!require(lme4)) install.packages("lme4")
# if(!require(sjPlot)) install.packages("sjPlot")
# if(!require(car)) install.packages("car")
# if(!require(mgcv)) install.packages("mgcv")
# if(!require(arm)) install.packages("arm")
# if(!require(ggeffects)) install.packages("ggeffects")
# if(!require(pROC)) install.packages("pROC")
# if(!require(broom.mixed)) install.packages("broom.mixed")
# if(!require(performance)) install.packages("performance")
# if(!require(insight)) install.packages("insight")
# if(!require(lmerTest)) install.packages("lmerTest")
# if(!require(DHARMa)) install.packages("DHARMa")
# if(!require(lattice)) install.packages("lattice")
# if(!require(patchwork)) install.packages("patchwork")
# if(!require(scales)) install.packages("scales")
# if(!require(detectseparation)) install.packages("detectseparation")
# if(!require(equatiomatic)) install.packages("equatiomatic")
# if(!require(DiagrammeR)) install.packages("DiagrammeR")
# if(!require(survey)) install.packages("survey")
# if(!require(ggcorrplot)) install.packages("ggcorrplot")
# if(!require(hexbin)) install.packages("hexbin")

library(readr)
library(dplyr)
library(ggplot2)
library(skimr)
library(tidyverse)
library(quanteda)
library(quanteda.textstats)
library(lme4)
library(sjPlot)
library(car)
library(arm)
library(ggeffects)
library(pROC)
library(broom.mixed)
library(performance)
library(insight)
library(lmerTest)
library(mgcv)
library(DHARMa)
library(lattice)
library(patchwork)
library(scales)
library(detectseparation)
library(equatiomatic)
library(DiagrammeR)
library(survey)
library(ggcorrplot)
library(hexbin)

set.seed(31415)
options(scipen = 9999)

# ==============================================================================
# 2. DATA IMPORT AND INITIAL CLEANING ----
# ==============================================================================

# Load the data collected from the Game (Exported from Supabase)
# Assumes you downloaded your Supabase 'trials' table as 'trials_data.csv'
if(!file.exists("Study/trials_rows.csv")) {
  stop("Please place the 'trials_data.csv' file in the Study folder before running.")
}

trials <- read_csv("Study/trials_rows.csv", show_col_types = FALSE)
scores <- read_csv("Study/scores_rows.csv", show_col_types = FALSE)

#filter setup test trials
trials <- trials %>% 
  filter(ymd_hms(timestamp) > ymd_hms("2026-05-04 08:15:00"))
scores <- scores %>% 
  filter(ymd_hms(timestamp) > ymd_hms("2026-05-04 08:15:00"))

# ==============================================================================
# 3. DATA INSPECTION AND FEATURE ENGINEERING ----
# ==============================================================================

# Inspect variables
skim(scores)
skim(trials)

# Create variables

#unique identifier for replication--originalstudy pairing
trials$study_id <- paste(trials$ref_o, "*", trials$ref_r)

#in_research
trials <- trials %>%
  mutate(in_research = if_else(career %in% c("I am a postdoc", "I am a professor", "I am a phd student") | education == "PhD", 1, 0))
scores <- scores %>%
  mutate(in_research = if_else(career %in% c("I am a postdoc", "I am a professor", "I am a phd student") | education == "PhD", 1, 0))

#readability measures
my_corpus <- corpus(trials$abstract_o)
readability_scores <- textstat_readability(my_corpus, measure = c("Flesch.Kincaid", "SMOG"))
trials$abstract_Flesch <- readability_scores$Flesch.Kincaid
trials$abstract_SMOG <- readability_scores$SMOG

#length of abstracts
trials$abstract_length <- nchar(trials$abstract_o)
# ==============================================================================
# 4. EXPLORATORY ANALYSIS: REACTION TIME (UNFILTERED) ----
# ==============================================================================

# reaction time
ggplot(trials, aes(x = reaction_time)) +
  geom_density(fill = "lightblue", alpha = 0.5) +
  geom_vline(aes(xintercept = mean(reaction_time, na.rm = TRUE)), color = "red", linetype = "dashed") +
  geom_vline(aes(xintercept = median(reaction_time, na.rm = TRUE)), color = "blue", linetype = "dotdash") +
  geom_text(aes(x = mean(reaction_time, na.rm = TRUE), y = 0, 
                label = paste("Mean:", round(mean(reaction_time, na.rm = TRUE), 0)/1000, " s")), 
            color = "red", angle = 0, vjust = -0.5, hjust = 0, size = 3.5) +
  geom_text(aes(x = median(reaction_time, na.rm = TRUE), y = 0, 
                label = paste("Median:", round(median(reaction_time, na.rm = TRUE), 0)/1000, " s")), 
            color = "blue", angle = 0, vjust = 1.5, hjust = 0, size = 3.5) +
  theme_minimal()+
  ylab("density")

ggplot(trials, aes(y = reaction_time)) +
  geom_boxplot(fill = "steelblue", alpha = 0.7, outlier.color = "red") +
  labs(title = "Boxplot Reactiontimes", y = "Reactiontimes (ms)") +
  theme_minimal() +
  theme(axis.text.x = element_blank())

trials %>%
  summarise(
    n_under_2000   = sum(reaction_time < 2000, na.rm = TRUE),
    n_under_3000   = sum(reaction_time < 3000, na.rm = TRUE),
    pct_under_2000 = mean(reaction_time < 2000, na.rm = TRUE) * 100,
    pct_under_3000 = mean(reaction_time < 3000, na.rm = TRUE) * 100,
    total_trials  = sum(!is.na(reaction_time))
  )

ggplot(trials, aes(x = round_number, y = reaction_time)) +
  geom_point(alpha = 0.5, color = "darkslategray") +
  geom_smooth(method = "gam", color = "firebrick", size = 1.2) +
  labs(
    title = "Reaction Time Development Across Rounds",
    subtitle = "Are reaction times deacreasing?",
    x = "Round Number",
    y = "Reaction Time (ms)"
  ) +
  scale_x_continuous(breaks = seq(1, 20, by = 2)) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14))

# ==============================================================================
# 5. EXPLORATORY ANALYSIS: TAB SWITCHES AND CORRECTNESS ----
# ==============================================================================

# tab_switch
scores %>%
  group_by(session_id) %>%
  summarise(
    max_switches = max(tab_switches),
    final_score  = max(final_score),
    focus_group = if_else(max_switches == 0, "0 Switches", "1+ Switches")
  ) %>%
  ggplot(aes(x = focus_group, y = final_score, fill = focus_group)) +
  geom_boxplot(alpha = 0.7, outlier.color = "red") +
  labs(title = "Influence of Tabswitch on final score",
       x = "group", y = "final score") +
  theme_minimal() + theme(legend.position = "none")

# ==============================================================================
# 6. SESSION VALIDATION AND BOT/STRAIGHTLINER DETECTION ----
# ==============================================================================

# criteria for filtering of suspicious sessions
has_suspicious_run <- function(responses, rts, min_run, abs_rt_cutoff) {
  r <- rle(responses)
  run_lengths <- r$lengths
  run_end <- cumsum(run_lengths)
  run_start <- c(1, head(run_end, -1) + 1)
  
  any(sapply(seq_along(run_lengths), function(i) {
    if (run_lengths[i] >= min_run) {
      run_rts <- rts[run_start[i]:run_end[i]]
      median(run_rts) < abs_rt_cutoff
    } else {
      FALSE
    }
  }))
}

session_flags <- trials %>%
  group_by(session_id) %>% 
  arrange(round_number, .by_group = TRUE) %>% 
  summarise(
    n_trials = n(),
    flag_too_short = n_trials < 5,
    # Bot-detection via low RT-Variance
    cv_rt = sd(reaction_time) / median(reaction_time),
    flag_bot = cv_rt < 0.1,  
    flag_straightliner = has_suspicious_run(player_guess, reaction_time,
                                            min_run = 10, abs_rt_cutoff = 2000),# we believe its unlikely that one participant is guessing the same option 10 times in a row and is also doing that very quickly
    
    .groups = "drop"
  )

session_flags %>%
  summarise(
    too_short = sum(flag_too_short),
    bot = sum(flag_bot, na.rm = TRUE),
    straightliner = sum(flag_straightliner),
    total_suspicious = sum(flag_too_short | flag_bot | flag_straightliner),
    total_sessions = n()
  )

sessions_valid <- session_flags %>%
  filter(!flag_too_short, !flag_bot, !flag_straightliner) %>%
  pull(session_id)

# ==============================================================================
# 7. DATA FILTERING (MACRO & MICRO LEVEL) ----
# ==============================================================================

trials_filtered <- trials %>%
  filter(session_id %in% sessions_valid) %>%
  filter(!is.na(reaction_time)) %>% 
  filter(tab_switches == 0) %>% #filter all trials with tab switch
  filter(reaction_time >= 2000) %>% #filter all short trials with <=2s
  filter(reaction_time <= 900000)#discard trials with more than  or exactly 15 min time

# ==============================================================================
# 8. POST-FILTERING EVALUATION AND VISUALIZATION ----
# ==============================================================================

ggplot(trials_filtered, aes(x = reaction_time)) +
  geom_density(fill = "lightblue", alpha = 0.5) +
  geom_vline(aes(xintercept = mean(reaction_time, na.rm = TRUE)), color = "red", linetype = "dashed") +
  geom_vline(aes(xintercept = median(reaction_time, na.rm = TRUE)), color = "blue", linetype = "dotdash") +
  geom_text(aes(x = mean(reaction_time, na.rm = TRUE), y = 0, 
                label = paste("Mean:", round(mean(reaction_time, na.rm = TRUE), 0)/1000, " s")), 
            color = "red", angle = 0, vjust = -0.5, hjust = 0, size = 3.5) +
  geom_text(aes(x = median(reaction_time, na.rm = TRUE), y = 0, 
                label = paste("Median:", round(median(reaction_time, na.rm = TRUE), 0)/1000, " s")), 
            color = "blue", angle = 0, vjust = 1.5, hjust = 0, size = 3.5) +
  theme_minimal()+
  ylab("density")

ggplot(trials_filtered, aes(x = "", y = reaction_time / 1000)) +
  geom_boxplot(fill = "skyblue", outlier.color = "red", alpha = 0.7) +
  scale_y_continuous(labels = label_number(accuracy = 1, suffix = "s")) +
  labs(title = "Distribution of filtered reaction times",
       y = "reaction time in seconds",
       x = "") +
  theme_minimal()

# readability
plot(density(trials_filtered$abstract_Flesch, na.rm = TRUE)) #the more the easier, 100 very easy, under 30 very hard
plot(density(trials_filtered$abstract_SMOG, na.rm = TRUE)) #more means more difficult to understand, higher than 17 very hard to understand for people outside the scientific community

trials_filtered %>%
  filter(!is.na(abstract_SMOG)) %>% 
  mutate(reaction_time_s = reaction_time / 1000) %>% 
  
  ggplot(aes(x = abstract_SMOG, y = reaction_time_s)) +
  geom_point(color = "darkslategray", alpha = 0.6, size = 2.5) +
  geom_smooth(method = "lm", color = "firebrick", se = TRUE, size = 1) +
  coord_cartesian(xlim = c(10, 25)) + 
  labs(
    title = "Impact of Abstract Readability on Reaction Time",
    subtitle = "Simple scatterplot evaluating textual difficulty against response speed",
    x = "Abstract Readability (SMOG Score)",
    y = "Reaction Time (seconds)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14))

#length of abstract
plot(density(trials_filtered$abstract_length))
trials_filtered %>%
  filter(!is.na(abstract_length)) %>% 
  mutate(reaction_time_s = reaction_time / 1000) %>% 
  
  ggplot(aes(x = abstract_length, y = reaction_time_s)) +
  geom_point(color = "darkslategray", alpha = 0.6, size = 2.5) +
  geom_smooth(method = "lm", color = "firebrick", se = TRUE, size = 1) +
  labs(
    title = "Impact of Abstract Length on Reaction Time",
    subtitle = "scatterplot evaluating text length against response speed",
    x = "Abstract Length (Character Count)",
    y = "Reaction Time (seconds)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14))

cor.test(trials_filtered$abstract_length, trials_filtered$reaction_time, method = "spearman", exact = FALSE)

# readability vs length
plot(density(trials_filtered$abstract_length))
trials_filtered %>%
  filter(!is.na(abstract_length)) %>% 
  ggplot(aes(x = abstract_length, y = abstract_SMOG)) +
  geom_point(color = "darkslategray", alpha = 0.6, size = 2.5) +
  geom_smooth(method = "lm", color = "firebrick", se = TRUE, size = 1) +
  labs(
    title = "Correlation of Abstract Length and SMOG",
    subtitle = "scatterplot evaluating text length against readability score",
    x = "Abstract Length (Character Count)",
    y = "Measure of Gobbledygook"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14))
cor.test(trials_filtered$abstract_length, trials_filtered$abstract_SMOG, method = "spearman", exact = FALSE)
# ==============================================================================
# 9. SAMPLE CHARACTERIZATION ----
# ==============================================================================

scores %>%
  dplyr::select(career, education, discipline) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Category") %>%
  filter(!is.na(Category)) %>% # Handles the 4 missing values in discipline
  group_by(Variable, Category) %>%
  summarise(Absolute_Count = n(), .groups = "drop_last") %>%
  mutate(Relative_Percentage = (Absolute_Count / sum(Absolute_Count)) * 100) %>%
  arrange(Variable, desc(Absolute_Count))

ggplot(scores, aes(x = career, fill = final_level)) +
  geom_bar(position = "stack") +
  labs(
    title = "Highest Game Level Achieved Across Career Stages",
    x = "Career Phase",
    y = "Number of Sessions (Absolute)", # it should later be maybe adjusted on player level so that multiple sessions are combined per person
    fill = "Final Level"
  ) +
  scale_y_continuous(breaks = seq(0, 10, by = 1)) + 
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )

ggplot(scores, aes(x = education, fill = final_level)) +
  geom_bar(position = "stack") +
  labs(
    title = "Highest Game Level Achieved Across Highest Education",
    x = "Highest Education",
    y = "Number of Sessions (Absolute)", # it should later be maybe adjusted on player level so that multiple sessions are combined per person
    fill = "Final Level"
  ) +
  scale_y_continuous(breaks = seq(0, 10, by = 1)) + 
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )

ggplot(scores, aes(x = factor(in_research), fill = final_level)) +
  geom_bar(position = "stack") +
  labs(
    title = "Highest Game Level Achieved Experience in Research vs No experience",
    x = "Experience in Research (1 = phd and/or phd student, post doc or prof)",
    y = "Number of Sessions (Absolute)", 
    fill = "Final Level"
  ) +
  scale_x_discrete(labels = c("0" = "not in research", "1" = "in research")) +
  scale_y_continuous(breaks = seq(0, 10, by = 1)) + 
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 0, hjust = 0.5), 
    legend.position = "right"
  )


# ==============================================================================
# 10. PREDICTION ACCURACY  ----
# ==============================================================================


cat("=== OVERALL BEHAVIORAL PERFORMANCE ===\n")
overall_acc <- mean(trials_filtered$correct, na.rm = TRUE)
cat(sprintf("Overall Prediction Accuracy: %.2f%%\n", overall_acc * 100))

# player guess vs true outcome
ggplot(trials_filtered, aes(x = player_guess, fill = player_guess)) +
  geom_bar(alpha = 0.8) +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5, fontface = "bold") +
  facet_wrap(~ true_outcome, labeller = label_both) +
  scale_fill_manual(values = c("#e41a1c", "#377eb8")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Player Guesses by True Outcome", x = "Player Guess", y = "Count") +
  theme_light() + theme(legend.position = "none")

cat("\n=== EXPERIMENTAL MANIPULATION: JIF VISIBILITY ===\n")
# Does seeing the JIF change prediction accuracy?
manipulation_stats <- trials_filtered %>%
  group_by(if_visible) %>%
  summarize(
    trials_count = n(),
    accuracy = mean(correct, na.rm = TRUE),
    avg_rt_ms = mean(reaction_time, na.rm = TRUE)
  )
print(manipulation_stats)

cat("\n=== PERFORMANCE BY CAREER LEVEL ===\n")
career_stats <- trials_filtered %>%
  group_by(career) %>%
  summarize(
    trials_count = n(),
    accuracy = mean(correct, na.rm = TRUE)
  ) %>%
  arrange(desc(accuracy))
print(career_stats)

research_stats <- trials_filtered %>%
  group_by(in_research) %>%
  summarize(
    trials_count = n(),
    accuracy = mean(correct, na.rm = TRUE)
  ) %>%
  arrange(desc(accuracy))
print(research_stats)

# ==============================================================================
# 11. IF VISIBILITY EFFECTS ON ACCURACY ----
# ==============================================================================

#correct vs if_visible
visibility_table <- trials_filtered %>%
  group_by(if_visible, correct) %>%
  summarise(count = n(), .groups = "drop_last") %>%
  mutate(percentage = (count / sum(count)) * 100)

print(visibility_table)

ggplot(trials_filtered, aes(x = if_visible, fill = correct)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("#f4a582", "darkgreen")) +
  labs(
    title = "Impact of Visual Cues on Accuracy",
    subtitle = "Does item visibility increase correct choices?",
    x = "Item is Visible (if_visible)",
    y = "Proportion of Trials",
    fill = "Answer Correct?"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14))

#player_guess vs if_visible
ggplot(trials_filtered, aes(x = impact_factor, fill = player_guess)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ if_visible, labeller = label_both) +
  scale_fill_manual(values = c("failure" = "#e41a1c", "success" = "#377eb8")) +
  labs(
    title = "Impact Factor Distribution for Player Guesses",
    subtitle = "When low impact factor is present, higher rates of failure are guessed",
    x = "Impact Factor",
    y = "Density",
    fill = "Player Guess"
  ) +
  theme_light() +
  theme(legend.position = "bottom")

#### HIGHLIGHT PLOT 1
#player_guess vs if_visible vs in_research
trials_filtered %>%
  mutate(in_research = factor(in_research, 
                              levels = c(0, 1), 
                              labels = c("Not in Research", "In Research"))) %>%
  
  ggplot(aes(x = impact_factor, fill = player_guess)) +
  geom_density(alpha = 0.5) +
  facet_grid(if_visible ~ in_research, labeller = label_both) +
  scale_fill_manual(values = c("failure" = "#e41a1c", "success" = "#377eb8")) +
  labs(
    title = "Impact Factor Distribution for Player Guesses",
    subtitle = "Analysis segmented by item visibility and research experience",
    x = "Impact Factor",
    y = "Density",
    fill = "Player Guess"
  ) +
  theme_light() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    strip.text = element_text(face = "bold", size = 10),
    legend.position = "bottom"
  )
#### HIGHLIGHT PLOT 2
#correct vs if_visible vs in_research
trials_filtered %>%
  mutate(in_research = factor(in_research, 
                              levels = c(0, 1), 
                              labels = c("Not in Research", "In Research"))) %>%
  
  ggplot(aes(x = impact_factor, fill = correct)) +
  geom_density(alpha = 0.5) +
  facet_grid(if_visible ~ in_research, labeller = label_both) +
  scale_fill_manual(values = c("FALSE" = "#e41a1c", "TRUE" = "#377eb8")) +
  labs(
    title = "Impact Factor Distribution for Correctness of Guesses",
    subtitle = "Analysis segmented by item visibility and research experience",
    x = "Impact Factor",
    y = "Density",
    fill = "Correctness of Guess"
  ) +
  theme_light() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    strip.text = element_text(face = "bold", size = 10),
    legend.position = "bottom"
  )

# ==============================================================================
# 12. MULTI LEVEL MODEL ---- https://pmc.ncbi.nlm.nih.gov/articles/PMC6945753/
# ==============================================================================

###############################################################################
#  PRE-REGISTERED MULTILEVEL ANALYSIS (frequentist only)
#  Predicting players' replication guesses from impact-factor visibility,
#  research experience, and true outcome.
#
#  Levels:
#    Level 2 : participants (personal_code)              -- always modelled
#    Level 2': studies      (study_id)                      -- only if N > 5000
#              (crossed with personal_code, not nested)
#    Level 1 : trials within participants
###############################################################################


###############################################################################
# 1. DATA PREPARATION --------------------------------------------------------
###############################################################################

# Assume `trials_filtered` is the cleaned trial-level data frame
# Variables expected:
#   personal_code     (subject ID)
#   study_id          (stimulus / study ID; optional level 3)
#   player_guess      ("success"/"failure")
#   in_research       (between-subject: research experience yes/no)
#   if_visible        (within-subject: impact factor visible yes/no)
#   impact_factor     (`impact_factor` is always the *true* value of the study, regardless of
#                      whether it was shown to the participant. Therefore we keep it on every trial.)
#   true_outcome      (binary: did the study actually replicate)
#   reaction_time     (ms)
#   abstract_length   (n words / characters)

model_data <- trials_filtered %>%
  mutate(
    player_guess_numeric = if_else(player_guess == "success", 1, 0),
    if_visible           = factor(if_visible,   levels = c(FALSE, TRUE)),
    true_outcome         = factor(true_outcome),
    in_research          = factor(in_research),
    personal_code        = factor(personal_code),
    study_id             = factor(study_id),
  ) %>%
  filter(!is.na(impact_factor),
         !is.na(reaction_time),
         !is.na(abstract_length),
         !is.na(true_outcome))

###############################################################################
# 2. DISTRIBUTION CHECKS & DATA-DRIVEN TRANSFORMATION RULE -------------------
#    Pre-registered rule: log1p() if |skew| > 1, then z-standardise.
###############################################################################

continuous_vars <- c("impact_factor", "reaction_time", "abstract_length")

plot_dist <- function(var) {
  ggplot(model_data, aes_string(var)) +
    geom_histogram(aes(y = ..density..), bins = 40,
                   fill = "steelblue", alpha = .7, color = "white") +
    geom_density(color = "firebrick", linewidth = .8) +
    labs(title = paste("Distribution of", var)) +
    theme_minimal()
}
wrap_plots(lapply(continuous_vars, plot_dist), ncol = 2)

# continous variable densities split up by group variables
group_vars    <- c("in_research", "if_visible", "true_outcome")

plot_density_by <- function(var, group_var, data = model_data) {
  ggplot(data, aes(x = .data[[var]],
                   fill  = .data[[group_var]],
                   color = .data[[group_var]])) +
    geom_density(alpha = 0.35, linewidth = 0.8) +
    labs(title = paste(var, "by", group_var),
         x = var, fill = group_var, color = group_var) +
    theme_minimal()
}

for (v in continuous_vars) {
  print(
    wrap_plots(lapply(group_vars, function(g) plot_density_by(v, g)),
               ncol = 3, title = paste("Density:", v))
  )
}

# data driven transformation rule
skew <- function(x) {
  x <- x[!is.na(x)]; mean((x - mean(x))^3) / sd(x)^3
}
skew_tab <- sapply(continuous_vars, function(v) skew(model_data[[v]]))
print(round(skew_tab, 2))

do_log <- abs(skew_tab) > 1   # pre-registered transformation rule

transform_var <- function(x, lg) {
  if (lg) as.numeric(scale(log1p(x))) else as.numeric(scale(x))
}
model_data <- model_data %>%
  mutate(
    impact_factor_z   = transform_var(impact_factor,   do_log["impact_factor"]),
    reaction_time_z   = transform_var(reaction_time,   do_log["reaction_time"]),
    abstract_length_z = transform_var(abstract_length, do_log["abstract_length"])
  )

wrap_plots(lapply(c("impact_factor_z", "reaction_time_z", "abstract_length_z"),
                  plot_dist), ncol = 2)

# distribution of player_guess vs true_outcome
p_guess_dist <- model_data %>%
  count(player_guess_numeric) %>%
  mutate(
    prop  = n / sum(n),
    label = scales::percent(prop, accuracy = 0.3),
    cat   = if_else(player_guess_numeric == 1, "Success (1)", "Failure (0)")
  ) %>%
  ggplot(aes(cat, prop, fill = cat)) +
  geom_col(width = 0.5, alpha = 0.85) +
  geom_text(aes(label = label), vjust = -0.4, size = 4) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1.05)) +
  scale_fill_manual(values = c("Success (1)" = "steelblue",
                               "Failure (0)" = "firebrick")) +
  labs(title = "Distribution of players guess", x = NULL, y = "Proportion") +
  theme_minimal() + theme(legend.position = "none")
p_guess_dist

p_outcome_dist <- model_data %>%
  count(true_outcome) %>%
  mutate(
    prop  = n / sum(n),
    label = scales::percent(prop, accuracy = 0.3)
  ) %>%
  ggplot(aes(true_outcome, prop, fill = true_outcome)) +
  geom_col(width = 0.5, alpha = 0.85) +
  geom_text(aes(label = label), vjust = -0.4, size = 4) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1.05)) +
  labs(title = "Distribution of true outcome", x = NULL, y = "Proportion") +
  theme_minimal() + theme(legend.position = "none")

wrap_plots(p_guess_dist, p_outcome_dist, ncol = 2)

#correlation matrices Level 1 (trial level) / partly point biserial when binary variable is involved
level1_cor_data <- model_data %>%
  transmute(
    guess_success     = player_guess_numeric,
    true_outcome_num  = as.numeric(true_outcome=="success"),
    if_visible_num    = as.numeric(if_visible == "TRUE"),
    impact_factor_z,
    reaction_time_z,
    abstract_length_z
  )

cor_l1 <- cor(level1_cor_data, use = "pairwise.complete.obs")

ggcorrplot(cor_l1,
           method   = "square",
           type     = "lower",
           lab      = TRUE,
           lab_size = 3.5,
           colors   = c("firebrick", "white", "steelblue"),
           title    = "Level 1 (Trial): Correlationmatrix",
           ggtheme  = theme_minimal())

#correlation matrices Level 2 (person level) / partly point biserial when binary variable is involved
level2_cor_data <- model_data %>%
  group_by(personal_code) %>%
  summarise(
    in_research_num = as.numeric(as.character(first(in_research))),
    mean_guess      = mean(player_guess_numeric),
    prop_visible    = mean(if_visible == "TRUE"),
    prop_success    = mean(as.numeric(true_outcome=="success")),
    mean_RT         = mean(reaction_time),
    mean_IF         = mean(impact_factor),
    mean_abstract   = mean(abstract_length),
    n_trials        = n(),
    .groups = "drop"
  ) %>%
  dplyr::select(-personal_code)

cor_l2 <- cor(level2_cor_data, use = "pairwise.complete.obs")

ggcorrplot(cor_l2,
           method   = "square",
           type     = "lower",
           lab      = TRUE,
           lab_size = 3.5,
           colors   = c("firebrick", "white", "steelblue"),
           title    = "Level 2 (Person): Correlationmatrix",
           ggtheme  = theme_minimal())

###############################################################################
# 3. DESCRIPTIVE ANALYSES AT EVERY LEVEL --------------------------------------
###############################################################################

svy_design <- svydesign(ids = ~personal_code, data = model_data)

# Metric Variables: Mean and cluster corrected Standard Errors + 95% CI
svy_cont <- svymean(
  ~player_guess_numeric + impact_factor + reaction_time + abstract_length,
  design = svy_design
)
ci_cont <- confint(svy_cont, level = 0.95)

trial_desc_survey <- data.frame(
  Variable = rownames(ci_cont),
  Mean     = coef(svy_cont),
  SE       = as.numeric(SE(svy_cont)),
  CI_lower = ci_cont[, 1],
  CI_upper = ci_cont[, 2]
) %>%
  mutate(across(where(is.numeric), ~round(.x, 4)))

cat("=== Metric Variables (cluster-corrected via personal_code) ===\n")
print(trial_desc_survey, row.names = FALSE)

# Categorical Variables: Proportions with cluster-corrected Standard Errors
cat_list <- list(
  if_visible   = ~I(if_visible == "TRUE"),
  true_outcome = ~I(true_outcome == "success"),
  in_research  = ~I(as.numeric(as.character(in_research)) == 1)
)

cat("\n=== Categorical Variables (cluster-corrected Proportions) ===\n")
for (nm in names(cat_list)) {
  sv  <- svymean(cat_list[[nm]], design = svy_design, na.rm = TRUE)
  civ <- confint(sv, level = 0.95)
  cat(sprintf("%-14s  prop = %.3f  SE = %.4f  95%% CI [%.3f, %.3f]\n",
              nm, coef(sv), SE(sv), civ[1], civ[2]))
}

# # --- 3a. Trial-level (Level 1) ----------------------------------------------- NOT CORRECTED FOR clusters
# trial_desc <- model_data %>%
#   summarise(
#     n_trials        = n(),
#     p_guess_success = mean(player_guess_numeric),
#     p_visible       = mean(as.logical(if_visible)),
#     p_true_outcome  = mean(true_outcome == "success"), 
#     M_RT            = mean(reaction_time),  SD_RT  = sd(reaction_time),
#     M_abstract      = mean(abstract_length), SD_abs = sd(abstract_length),
#     M_IF            = mean(impact_factor),   SD_IF  = sd(impact_factor)
#   )
# print(trial_desc)

# --- 3b. Participant-level (Level 2) -----------------------------------------
participant_desc <- model_data %>%
  group_by(personal_code, in_research) %>%
  summarise(
    n_trials       = n(),
    n_visible      = sum(if_visible == "TRUE"),
    n_true_success = sum(true_outcome == "success"),
    mean_guess     = mean(player_guess_numeric),
    mean_RT        = mean(reaction_time),
    .groups = "drop"
  )

cat("Participants:", nrow(participant_desc),
    " | Median trials/participant:", median(participant_desc$n_trials),
    " | Range:", paste(range(participant_desc$n_trials), collapse = "-"), "\n")

ggplot(participant_desc, aes(n_trials)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  labs(title = "Number of trials per participant",
       x = "n trials", y = "n participants") +
  theme_minimal()

ggplot(participant_desc, aes(in_research, mean_guess, fill = in_research)) +
  geom_boxplot(alpha = .6, outlier.shape = NA) +
  geom_jitter(width = .15, alpha = .4) +
  labs(title = "Mean 'success' rate per participant by research experience") +
  theme_minimal()+
  geom_hline(yintercept = mean(trials_filtered$true_outcome == "success"))

# Spaghetti plot: each subject's success rate by IF visibility
spaghetti <- model_data %>%
  group_by(personal_code, in_research, if_visible) %>%
  summarise(p_success = mean(player_guess_numeric), .groups = "drop")

ggplot(spaghetti, aes(if_visible, p_success, group = personal_code,
                      color = in_research)) +
  geom_line(alpha = .25) +
  stat_summary(aes(group = in_research), fun = mean, geom = "line",
               linewidth = 1.3) +
  labs(title = "Within-subject change in 'success' rate by IF visibility",
       y = "P(guess = success)") +
  theme_minimal()



# bivariate plots with clustermembership 

person_agg <- model_data %>%
  group_by(personal_code, in_research) %>%
  summarise(
    mean_guess = mean(player_guess_numeric),
    mean_IF_z  = mean(impact_factor_z),
    mean_RT_z  = mean(reaction_time_z),
    mean_abs_z = mean(abstract_length_z),
    .groups = "drop"
  )

make_biv_plot <- function(x_var_trial,   
                          x_var_person,  
                          x_label,
                          version = c("in_research", "personal_code")) {
  version <- match.arg(version)
  
  y_scale <- scale_y_continuous(breaks = c(0, 1),
                                labels = c("Failure (0)", "Success (1)"))
  
  if (version == "in_research") {
    ggplot() +
      geom_jitter(data  = model_data,
                  aes(x = .data[[x_var_trial]], y = player_guess_numeric),
                  width = 0.04, height = 0.025,
                  alpha = 0.04, size = 0.4, color = "grey50") +
      geom_point(data = person_agg,
                 aes(x     = .data[[x_var_person]], y = mean_guess,
                     color = in_research),
                 size = 2, alpha = 0.75) +
      geom_smooth(data = person_agg,
                  aes(x    = .data[[x_var_person]], y = mean_guess,
                      color = in_research, fill = in_research),
                  method = "loess", se = TRUE,
                  linewidth = 1.1, alpha = 0.18) +
      scale_color_brewer(palette = "Set1", name = "in_research") +
      scale_fill_brewer( palette = "Set1", name = "in_research") +
      y_scale +
      labs(title    = paste(x_label, "vs. P(guess = success)"),
           subtitle = "Grey = Trials (jittered, transparent) | dots & smooth = mean per person",
           x = x_label, y = "P(guess = success)") +
      theme_minimal()
    
  } else {
    ggplot() +
       geom_hex(data = model_data,
               aes(x = .data[[x_var_trial]], y = player_guess_numeric),
               bins = 40, alpha = 0.75) +
      scale_fill_gradient(low = "grey92", high = "grey20",
                          name = "n Trials") +
      geom_point(data = person_agg,
                 aes(x = .data[[x_var_person]], y = mean_guess),
                 color = "steelblue", size = 1.5, alpha = 0.6) +
      geom_smooth(data = person_agg,
                  aes(x = .data[[x_var_person]], y = mean_guess),
                  method = "loess", color = "firebrick",
                  se = FALSE, linewidth = 1.3) +
      y_scale +
      labs(title    = paste(x_label, "vs. P(guess = success)"),
           subtitle = "Hexbin = Trial-density | blue dots = mean per person | red smooth (global, no CI)",
           x = x_label, y = "P(guess = success)") +
      theme_minimal()
  }
}

# ── impact_factor_z -
wrap_plots(
  make_biv_plot("impact_factor_z", "mean_IF_z", "impact_factor_z", "in_research"),
  make_biv_plot("impact_factor_z", "mean_IF_z", "impact_factor_z", "personal_code"),
  ncol = 1
)

# ── reaction_time_z ─
wrap_plots(
  make_biv_plot("reaction_time_z", "mean_RT_z", "reaction_time_z", "in_research"),
  make_biv_plot("reaction_time_z", "mean_RT_z", "reaction_time_z", "personal_code"),
  ncol = 1
)

# ── abstract_length_z ─
wrap_plots(
  make_biv_plot("abstract_length_z", "mean_abs_z", "abstract_length_z", "in_research"),
  make_biv_plot("abstract_length_z", "mean_abs_z", "abstract_length_z", "personal_code"),
  ncol = 1
)


# --- 3c. Study-level (study_id) – only inspect, model only if N is large -------
study_desc <- model_data %>%
  group_by(study_id) %>%
  summarise(
    n_times_drawn = n(),
    p_success     = mean(player_guess_numeric),
    .groups = "drop"
  )
cat("Unique studies:", nrow(study_desc),
    " | Median draws/study:", median(study_desc$n_times_drawn), "\n")

ggplot(study_desc, aes(n_times_drawn)) +
  geom_histogram(bins = 30, fill = "darkorange", color = "white") +
  labs(title = "How often each study was drawn") + theme_minimal()

###############################################################################
# 4. NULL MODEL ---------------------------------------------------------------
###############################################################################

null_model <- glmer(
  player_guess_numeric ~ 1 + (1 | personal_code),
  data = model_data, family = binomial(link = "logit"),
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))
)
summary(null_model)
icc_null <- performance::icc(null_model); print(icc_null)

###############################################################################
# 5. INTERMEDIATE MODEL  (between-subject + design factors only) -------------
###############################################################################

intermediate_model <- glmer(
  player_guess_numeric ~ in_research * if_visible + true_outcome +
    (1 | personal_code),
  data = model_data, family = binomial(link = "logit"),
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))
)
summary(intermediate_model)
icc_intermediate <- performance::icc(intermediate_model); print(icc_intermediate)

###############################################################################
# 6. FINAL MODEL --------------------------------------------------------------
# Fixed:  in_research * if_visible * impact_factor_z + true_outcome
#         + reaction_time_z + abstract_length_z
# Random: per participant – starting correlated, falling back step-by-step.
#
# Pre-registered fallback chain:
#   step1: correlated   (1 + if_visible + if_visible:impact_factor_z + true_outcome | personal_code)
#   step2: uncorrelated (||) version of step 1
#   step3: drop true_outcome from random part
#   step4: replace if_visible:impact_factor_z by impact_factor_z
#   step5: drop impact_factor_z
#   step6: random intercept only
#   step7: ordinary glm() (no random effects)
#
# If the FULL fixed-effects spec (3-way interaction) cannot converge in any
# step, we additionally fall back to a 2-way interaction model
# (in_research * if_visible + impact_factor_z + ...).
###############################################################################

# ── 1. Optional crossed (1 | study_id), only if huge N ───────────────────────

use_study_level <- length(unique(model_data$personal_code)) > 5000

# ── 2. Functions ────────────────────────────────────────────────────────────

build_formula <- function(re_term, fixed_3way = TRUE, study_level = FALSE,
                          quad_abstract = FALSE) {
  fixed <- if (fixed_3way) {
    "in_research * if_visible * impact_factor_z"
  } else {
    "in_research * if_visible + impact_factor_z"
  }
  abstract_terms <- if (quad_abstract) {
    "abstract_length_z + I(abstract_length_z^2)"
  } else {
    "abstract_length_z"
  }
  base <- paste0("player_guess_numeric ~ ", fixed,
                 " + true_outcome + reaction_time_z + ", abstract_terms)
  re   <- if (re_term == "1") "(1 | personal_code)" else
    paste0("(", re_term, " personal_code)")
  if (study_level) re <- paste0(re, " + (1 | study_id)")
  as.formula(paste(base, "+", re))
}

re_chain <- list(
  step1 = "1 + if_visible + if_visible:impact_factor_z + true_outcome |",
  step2 = "1 + if_visible + if_visible:impact_factor_z + true_outcome ||",
  step3 = "1 + if_visible + if_visible:impact_factor_z ||",
  step4 = "1 + if_visible + impact_factor_z ||",
  step5 = "1 + if_visible ||",
  step6 = "1"
)

try_glmer <- function(form, data) {
  tryCatch(
    glmer(form, data = data, family = binomial(link = "logit"),
          control = glmerControl(optimizer = "bobyqa",
                                 optCtrl = list(maxfun = 1e5))),
    error = function(e) { message("ERROR: ", e$message); NULL },
    warning = function(w) {
      m <- suppressWarnings(
        glmer(form, data = data, family = binomial(link = "logit"),
              control = glmerControl(optimizer = "bobyqa",
                                     optCtrl = list(maxfun = 1e5))))
      attr(m, "warn") <- conditionMessage(w); m
    }
  )
}

fit_with_fallback <- function(data, fixed_3way = TRUE, study_level = FALSE,
                              quad_abstract = FALSE) {
  for (step in names(re_chain)) {
    re_term <- re_chain[[step]]
    form    <- build_formula(re_term, fixed_3way, study_level, quad_abstract)
    cat("\n== Trying RE step:", step, " | fixed 3-way:", fixed_3way, "==\n")
    m <- try_glmer(form, data)
    if (is.null(m)) next
    sing <- isSingular(m, tol = 1e-4)
    msgs <- m@optinfo$conv$lme4$messages
    cat("  singular:", sing,
        " | conv messages:", ifelse(is.null(msgs), "none", msgs), "\n")
    if (!sing && is.null(msgs)) {
      attr(m, "re_step")    <- step
      attr(m, "fixed_spec") <- if (fixed_3way) "3-way" else "2-way"
      return(m)
    }
  }
  cat("All RE structures failed – falling back to glm() (no RE).\n")
  fixed <- if (fixed_3way) "in_research * if_visible * impact_factor_z" else
    "in_research * if_visible + impact_factor_z"
  abstract_terms <- if (quad_abstract) {
    "abstract_length_z + I(abstract_length_z^2)"
  } else {
    "abstract_length_z"
  }
  m <- glm(as.formula(paste0("player_guess_numeric ~ ", fixed,
                             " + true_outcome + reaction_time_z + ", abstract_terms)),
           data = data, family = binomial(link = "logit"))
  attr(m, "re_step")    <- "glm_no_re"
  attr(m, "fixed_spec") <- if (fixed_3way) "3-way" else "2-way"
  m
}

# ── 3. Linearity check via GAM ──────────────────────────────────────────────

gam_check <- gam(
  player_guess_numeric ~ s(impact_factor_z, by = in_research) +
    s(reaction_time_z) + s(abstract_length_z) +
    in_research + if_visible + true_outcome +
    s(personal_code, bs = "re"),
  data   = model_data |> mutate(personal_code = factor(personal_code)),
  family = binomial,
  method = "REML"
)

quad_abstract <- summary(gam_check)$s.table["s(abstract_length_z)", "p-value"] < .05
if (quad_abstract) {
  cat(">> Linearity check: abstract_length_z is significant non linear",
      "– quadratic term is used.\n")
} else {
  cat(">> Linearity check: abstract_length_z is linear",
      "– no quadratic term will be used.\n")
}

# ── 4. Main model ───────────────────────────────────────────────────────────

final_model <- fit_with_fallback(model_data, fixed_3way = TRUE,
                                 study_level = use_study_level,
                                 quad_abstract = quad_abstract)
if (is.null(final_model)) {
  cat("\n>>> 3-way fixed spec did not converge anywhere; ",
      "falling back to 2-way fixed spec.\n")
  final_model <- fit_with_fallback(model_data, fixed_3way = FALSE,
                                   study_level = use_study_level,
                                   quad_abstract = quad_abstract)
}

if (inherits(final_model, "glmerMod") && attr(final_model, "re_step") == "step1") {
  vc <- VarCorr(final_model)
  print(vc, comp = c("Variance", "Std.Dev.", "Corr"))
  as.data.frame(vc) #shows correlation of random effects in case of | personal code
}

summary(final_model)

cat("RE step      :", attr(final_model, "re_step"), "\n")
cat("Fixed spec   :", attr(final_model, "fixed_spec"), "\n")
cat("Formula      :", deparse1(formula(final_model)), "\n")

equatiomatic::preview_eq(final_model)
extract_eq(final_model, use_coefs = TRUE, wrap = TRUE, terms_per_line = 3) |> 
  preview_eq()

if (inherits(final_model, "glmerMod")) {
  icc_final <- performance::icc(final_model); print(icc_final)
}


###############################################################################
# 7. ASSUMPTION CHECKS --------------------------------------------------------
###############################################################################

# 7a. Convergence / singularity / separation
if (inherits(final_model, "glmerMod")) {
  cat("Singular?:", isSingular(final_model, tol = 1e-4), "\n")
  if (!is.null(final_model@optinfo$conv$lme4$messages))
    warning(paste(final_model@optinfo$conv$lme4$messages, collapse = " | "))
}

detect_sep <- glm(player_guess_numeric ~ in_research * if_visible * impact_factor_z +
      true_outcome + reaction_time_z + abstract_length_z,
    data = model_data, family = binomial, method = "detect_separation")

if(detect_sep$outcome == TRUE){
  warning(paste("Perfect separation, model coefficients and standard errors are not valid"))
  print(detect_sep)
}

# 7b. Multicollinearity
cat("\n=== VIF (look for non-interaction terms < 10) ===\n")
print(tryCatch(car::vif(final_model), error = function(e) e$message))

# 7c. Random-effect normality
if (inherits(final_model, "glmerMod")) {
  re <- ranef(final_model)$personal_code
  par(mfrow = c(ceiling(ncol(re)/2), 2))
  for (cn in colnames(re)) {
    qqnorm(re[[cn]], main = paste("QQ-Plot:", cn))
    qqline(re[[cn]], col = "firebrick")
  }
  par(mfrow = c(1, 1))
}

# 7d. Linearity of continuous predictors on logit scale

gam_check <- gam(
  player_guess_numeric ~ 
    s(impact_factor_z, by = in_research) +   
    s(reaction_time_z) + 
    s(abstract_length_z) +
    in_research + if_visible + true_outcome +
    s(personal_code, bs = "re"),
  data    = model_data,
  family  = binomial,
  method  = "REML"
)

summary(gam_check)        # edf ~ 1 -> linear; > 2-3 -> non-linear
plot(gam_check, pages = 1, residuals = TRUE, shade = TRUE)

# 7e. DHARMa simulation-based residuals
if (inherits(final_model, "glmerMod")) {
  sim_res <- simulateResiduals(final_model, n = 500)
  plot(sim_res); testDispersion(sim_res); testOutliers(sim_res)
}



###############################################################################
# 8. RESULT TABLES (NULL / INTERMEDIATE / FINAL) ------------------------------
###############################################################################

make_or_table <- function(model) {
  est  <- if (inherits(model, "glmerMod")) fixef(model) else coef(model)
  smry <- summary(model)$coefficients
  ci   <- tryCatch(confint(model, method = "Wald"),
                   error = function(e) confint.default(model))
  ci   <- ci[names(est), , drop = FALSE]
  data.frame(
    Term       = names(est),
    Odds_Ratio = exp(est),
    CI_Lower   = exp(ci[, 1]),
    CI_Upper   = exp(ci[, 2]),
    p_value    = smry[names(est), "Pr(>|z|)"]
  )
}

or_null         <- make_or_table(null_model)
or_intermediate <- make_or_table(intermediate_model)
or_final        <- make_or_table(final_model)
print(round(or_final[, -1], 3))

auc_for <- function(model, data) {
  p <- predict(model, type = "response")
  as.numeric(pROC::auc(pROC::roc(data$player_guess_numeric, p, quiet = TRUE)))
}

fit_table <- tibble(
  Model = c("Null", "Intermediate", "Final"),
  N     = c(nobs(null_model), nobs(intermediate_model), nobs(final_model)),
  ICC   = c(icc_null$ICC_adjusted,
            icc_intermediate$ICC_adjusted,
            if (inherits(final_model, "glmerMod")) icc_final$ICC_adjusted else NA),
  AIC   = c(AIC(null_model), AIC(intermediate_model), AIC(final_model)),
  BIC   = c(BIC(null_model), BIC(intermediate_model), BIC(final_model)),
  AUC   = c(auc_for(null_model, model_data),
            auc_for(intermediate_model, model_data),
            auc_for(final_model, model_data))
)
print(fit_table)


###############################################################################
# 9. VISUALISATIONS -----------------------------------------------------------
###############################################################################

# 9. Diagram of Model

re_step    <- attr(final_model, "re_step")
fixed_spec <- attr(final_model, "fixed_spec")

re_label <- switch(re_step,
                   step1     = "(1 + if_visible + if_visible:impact_factor_z + true_outcome | personal_code) [correlated]",
                   step2     = "(1 + if_visible + if_visible:impact_factor_z + true_outcome || personal_code) [uncorrelated]",
                   step3     = "(1 + if_visible + if_visible:impact_factor_z || personal_code) [uncorrelated]",
                   step4     = "(1 + if_visible + impact_factor_z || personal_code) [uncorrelated]",
                   step5     = "(1 + if_visible || personal_code) [uncorrelated]",
                   step6     = "(1 | personal_code) [intercept only]",
                   glm_no_re = "no random effects (glm fallback)"
)

abs_label   <- if (quad_abstract) "abstract_length_z, abstract_length_z^2" else "abstract_length_z"
fixed_label <- if (fixed_spec == "3-way") "in_research * if_visible * impact_factor_z" else
  "in_research * if_visible + impact_factor_z"

# Table (a)
if (use_study_level) {
  mlm_table <- data.frame(
    Level    = c("Study (Level 3)", "Person (Level 2)", "Observation (Level 1)"),
    Subindex = c("k = 1,...,K", "j = 1,...,J_k", "i = 1,...,n_jk"),
    X_Vars   = c("—", "in_research",
                 paste0(fixed_label, ", true_outcome, reaction_time_z, ", abs_label)),
    Y_Var    = c("—", "—", "player_guess_numeric"),
    RE       = c("(1 | study_id)", re_label, "—")
  )
} else {
  mlm_table <- data.frame(
    Level    = c("Person (Level 2)", "Observation (Level 1)"),
    Subindex = c("j = 1,...,J", "i = 1,...,n_j"),
    X_Vars   = c("in_research",
                 paste0(fixed_label, ", true_outcome, reaction_time_z, ", abs_label)),
    Y_Var    = c("—", "player_guess_numeric"),
    RE       = c(re_label, "—")
  )
}
print(mlm_table, row.names = FALSE)

# Flowchart (b)
build_mlm_dot <- function(use_study_level, re_label, fixed_label, abs_label) {
  
  l1_info <- paste0(
    "Level 1: Trial [i = 1,...,n_j]\n",
    "Y: player_guess_numeric\n",
    "X: ", fixed_label, "\n",
    "Controls: true_outcome, reaction_time_z, ", abs_label
  )
  l2_info <- paste0(
    "Level 2: Person [j = 1,...,J]\n",
    "X: in_research\n",
    "RE: ", re_label
  )
  
  if (use_study_level) {
    l1_info <- gsub("n_j", "n_jk", l1_info)
    l2_info <- gsub("J\\]", "J_k]", l2_info)
    l3_info <- "Level 3: Study [k = 1,...,K]\nRE: (1 | study_id)"
    
    dot <- paste0(
      'digraph mlm {
  graph [rankdir=TB, nodesep=0.4, ranksep=0.6, fontname=Helvetica]
  node [fontname=Helvetica, fontsize=11, style=filled, shape=box]

  L3box [label="', l3_info, '", fillcolor="#E8F4E8", shape=note, fontsize=10]
  L2box [label="', l2_info, '", fillcolor="#E8EEF8", shape=note, fontsize=10]
  L1box [label="', l1_info, '", fillcolor="#F8F0E8", shape=note, fontsize=10]

  pop [label="Sample Population", fillcolor="#DDDDDD", width=3]
  s1  [label="study_id 1",        fillcolor="#B8D4B8"]
  sd  [label="...",               shape=plaintext, fillcolor=white]
  sk  [label="study_id K",        fillcolor="#B8D4B8"]
  p11 [label="personal_code\n1.1",  fillcolor="#B8C8E8"]
  p1d [label="...",                  shape=plaintext, fillcolor=white]
  p1j [label="personal_code\n1.J1", fillcolor="#B8C8E8"]
  o1  [label="trial 1.1.1", fillcolor="#E8D4A8"]
  od  [label="...",          shape=plaintext, fillcolor=white]
  on  [label="trial 1.1.n",  fillcolor="#E8D4A8"]

  pop -> s1; pop -> sd [style=invis]; pop -> sk
  s1 -> p11; s1 -> p1d [style=invis]; s1 -> p1j
  p11 -> o1; p11 -> od [style=invis]; p11 -> on

  {rank=same; L3box; pop}
  {rank=same; L2box; s1; sd; sk}
  {rank=same; L1box; p11; p1d; p1j}
  L3box -> L2box -> L1box [style=invis]
}'
    )
  } else {
    dot <- paste0(
      'digraph mlm {
  graph [rankdir=TB, nodesep=0.4, ranksep=0.6, fontname=Helvetica]
  node [fontname=Helvetica, fontsize=11, style=filled, shape=box]

  L2box [label="', l2_info, '", fillcolor="#E8EEF8", shape=note, fontsize=10]
  L1box [label="', l1_info, '", fillcolor="#F8F0E8", shape=note, fontsize=10]

  pop [label="Sample Population", fillcolor="#DDDDDD", width=2.5]
  p1  [label="personal_code 1", fillcolor="#B8C8E8"]
  pd  [label="...",              shape=plaintext, fillcolor=white]
  pj  [label="personal_code J", fillcolor="#B8C8E8"]
  o1  [label="trial 1.1", fillcolor="#E8D4A8"]
  od  [label="...",        shape=plaintext, fillcolor=white]
  on  [label="trial 1.n",  fillcolor="#E8D4A8"]

  pop -> p1; pop -> pd [style=invis]; pop -> pj
  p1 -> o1; p1 -> od [style=invis]; p1 -> on

  {rank=same; L2box; pop}
  {rank=same; L1box; p1; pd; pj}
  L2box -> L1box [style=invis]
}'
    )
  }
  dot
}

grViz(build_mlm_dot(use_study_level, re_label, fixed_label, abs_label))

# 9a. Coefficient plot
plot_model(final_model, type = "est", transform = "exp",
           show.values = TRUE, value.offset = .3,
           title = "Final GLMM: predictors of P(guess = success)",
           vline.color = "firebrick", sort.est = TRUE) + theme_minimal()

# 9b. 3-way interaction (only meaningful if 3-way kept in fixed part)
if (attr(final_model, "fixed_spec") == "3-way") {
  ggpredict(final_model,
            terms = c("impact_factor_z", "in_research", "if_visible")) |>
    plot() +
    labs(title = "Impact factor x Research experience x Visibility")
}

# 9c. Subject-level random effects
if (inherits(final_model, "glmerMod")) {
  print(lattice::dotplot(ranef(final_model, condVar = TRUE)))
  broom.mixed::tidy(final_model, effects = "ran_vals", conf.int = TRUE) |>
    ggplot(aes(estimate, reorder(level, estimate))) +
    geom_point() +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = .2) +
    facet_wrap(~ term, scales = "free_x") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "firebrick") +
    theme_minimal() + labs(y = NULL, x = "Random effect (logit scale)")
}

# 9d. ROC curve
roc_obj <- pROC::roc(model_data$player_guess_numeric,
                     predict(final_model, type = "response"),
                     quiet = TRUE)
plot(roc_obj, main = paste("Final model AUC =", round(pROC::auc(roc_obj), 3)))

###############################################################################
# 10. EXPORT ------------------------------------------------------------------
###############################################################################
# saveRDS(list(
#   null_model         = null_model,
#   intermediate_model = intermediate_model,
#   final_model        = final_model,
#   final_model_3way   = if (exists("final_model_3way")) final_model_3way else NULL,
#   two_way_model      = two_way_model,
#   aic_comparison     = aic_comparison,
#   lrt_result         = lrt_result,
#   re_step_used       = attr(final_model, "re_step"),
#   fixed_spec_used    = attr(final_model, "fixed_spec"),
#   fit_table          = fit_table,
#   or_null            = or_null,
#   or_intermediate    = or_intermediate,
#   or_final           = or_final,
#   transformations    = do_log,
#   used_study_level   = use_study_level
# ), file = "multilevel_results.rds")
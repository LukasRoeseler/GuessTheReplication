# Install required packages if missing
if(!require(readr)) install.packages("readr")
if(!require(dplyr)) install.packages("dplyr")
if(!require(jsonlite)) install.packages("jsonlite")

library(readr)
library(dplyr)
library(jsonlite)

cat("Downloading and reading flora.csv...\n")
url <- "https://raw.githubusercontent.com/forrtproject/FReD-data/refs/heads/main/output/flora.csv"
data <- read_csv(url, show_col_types = FALSE)

cat("Processing data...\n")
clean_data <- data %>%
  # Convert to lowercase to ensure consistency
  mutate(outcome = tolower(outcome)) %>%
  # Filter out "mixed" and keep only "successful" and "failed"
  filter(outcome %in% c("successful", "failed")) %>%
  # Map variables to the structure expected by the game
  mutate(
    # Game expects "success" and "failure"
    outcome = ifelse(outcome == "successful", "success", "failure"),
    ref_o = title_o,
    ref_r = title_r,
    journal = journal_o,
    # Since flora.csv lacks an abstract, we use the original APA reference to give the player context
    abstract = apa_ref_o 
  ) %>%
  # Select only the columns needed for the lightweight web game
  select(ref_o, ref_r, journal, abstract, outcome, outcome_quote) %>%
  # Remove rows with missing crucial data
  filter(!is.na(ref_o) & !is.na(outcome))

# --- FIX: Smart path resolution ---
# Check if we are in the 'Study' folder (so 'Game' is in the parent dir)
# or if we are in the root folder (so 'Game' is in the current dir)
if (dir.exists("../Game")) {
  out_dir <- "../Game"
} else if (dir.exists("Game")) {
  out_dir <- "Game"
} else {
  # If it doesn't exist at all, create it in the current directory
  cat("Game folder not found. Creating 'Game' directory...\n")
  dir.create("Game", showWarnings = FALSE)
  out_dir <- "Game"
}

output_path <- file.path(out_dir, "dataset.json")
# ----------------------------------

cat("Writing JSON to", output_path, "...\n")
write_json(clean_data, output_path, pretty = TRUE)

cat(sprintf("Success! Exported %d clean trials.\n", nrow(clean_data)))
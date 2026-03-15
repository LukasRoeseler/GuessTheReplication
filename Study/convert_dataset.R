# Install required packages if missing
packages <- c("readr", "dplyr", "jsonlite", "httr", "stringr")
for (p in packages) {
  if (!require(p, character.only = TRUE)) install.packages(p)
}
library(readr)
library(dplyr)
library(jsonlite)
library(httr)
library(stringr)

cat("Downloading flora.csv...\n")
url <- "https://raw.githubusercontent.com/forrtproject/FReD-data/refs/heads/main/output/flora.csv"
data <- read_csv(url, show_col_types = FALSE)

# Helper function to fetch abstract from Crossref safely
get_crossref_abstract <- function(doi) {
  if (is.na(doi) || doi == "") return(NA)
  
  # Crossref API URL
  api_url <- paste0("https://api.crossref.org/works/", URLencode(doi))
  
  tryCatch({
    res <- GET(api_url, user_agent("GuessTheReplication/1.0 (mailto:your-email@example.com)"))
    if (status_code(res) == 200) {
      content <- content(res, as = "parsed", type = "application/json")
      abs <- content$message$abstract
      if (!is.null(abs)) {
        # Clean up JATS XML tags (e.g., <jats:p>, </jats:title>) often returned by Crossref
        clean_abs <- str_replace_all(abs, "<[^>]+>", " ")
        clean_abs <- str_squish(clean_abs)
        return(clean_abs)
      }
    }
    return(NA)
  }, error = function(e) {
    return(NA)
  })
}

cat("Filtering and mapping dataset...\n")
clean_data <- data %>%
  mutate(outcome = tolower(outcome)) %>%
  filter(outcome %in% c("successful", "failed")) %>%
  # Filter out sub_rep from outcome quote
  filter(!grepl("sub_rep", outcome_quote, ignore.case = TRUE)) %>%
  mutate(
    outcome = ifelse(outcome == "successful", "success", "failure"),
    ref_o = title_o,
    ref_r = title_r,
    journal = journal_o,
    doi_o = doi_o,
    doi_r = doi_r,
    fallback_abstract = apa_ref_o 
  ) %>%
  select(ref_o, ref_r, journal, doi_o, doi_r, fallback_abstract, outcome, outcome_quote) %>%
  filter(!is.na(ref_o) & !is.na(outcome))

# Sample down for speed during prototype phase, or run on all (can take ~5 mins for 300+ DOIs)
# To run on the whole dataset, leave the code as is.
cat(sprintf("Fetching abstracts via Crossref for %d studies. This may take a few minutes...\n", nrow(clean_data)))

# Fetch abstracts with a progress bar
clean_data$abstract <- NA
pb <- txtProgressBar(min = 0, max = nrow(clean_data), style = 3)
for (i in seq_len(nrow(clean_data))) {
  Sys.sleep(0.1) # Polite delay for API
  fetched <- get_crossref_abstract(clean_data$doi_o[i])
  clean_data$abstract[i] <- ifelse(is.na(fetched), clean_data$fallback_abstract[i], fetched)
  setTxtProgressBar(pb, i)
}
close(pb)

# Final cleanup
final_data <- clean_data %>% select(ref_o, ref_r, journal, abstract, outcome, outcome_quote, doi_o, doi_r)

# --- Smart path resolution ---
if (dir.exists("../Game")) { out_dir <- "../Game" } 
else if (dir.exists("Game")) { out_dir <- "Game" } 
else { dir.create("Game", showWarnings = FALSE); out_dir <- "Game" }

json_path <- file.path(out_dir, "dataset.json")
csv_path <- file.path(out_dir, "dataset.csv")

cat("\nWriting files...\n")
write_json(final_data, json_path, pretty = TRUE)
write_csv(final_data, csv_path)

cat(sprintf("Success! Exported %d clean trials to %s and %s\n", nrow(final_data), json_path, csv_path))

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

# Clean DOI prefix so we just have the raw 10.xxxx/...
clean_doi <- function(doi_string) {
  if (is.na(doi_string)) return(NA)
  str_replace(doi_string, "^https?://(dx\\.)?doi\\.org/", "")
}

# Helper function to fetch abstract from Crossref safely
get_crossref_abstract <- function(doi) {
  if (is.na(doi) || doi == "") return(NA)
  api_url <- paste0("https://api.crossref.org/works/", URLencode(doi))
  
  tryCatch({
    res <- GET(api_url, user_agent("GuessTheReplication/1.0 (mailto:your-email@example.com)"), timeout(5))
    if (status_code(res) == 200) {
      content <- content(res, as = "parsed", type = "application/json")
      abs <- content$message$abstract
      if (!is.null(abs)) {
        # Clean up XML tags
        clean_abs <- str_replace_all(abs, "<[^>]+>", " ")
        return(str_squish(clean_abs))
      }
    }
    return(NA)
  }, error = function(e) { return(NA) })
}

# Helper function to fetch Impact Factor equivalent from OpenAlex
get_openalex_jif <- function(journal_name) {
  if (is.na(journal_name) || journal_name == "") return(NA)
  
  api_url <- paste0("https://api.openalex.org/sources?search=", URLencode(journal_name))
  
  tryCatch({
    res <- GET(api_url, user_agent("GuessTheReplication/1.0 (mailto:your-email@example.com)"), timeout(5))
    if (status_code(res) == 200) {
      content <- content(res, as = "parsed", type = "application/json")
      if (length(content$results) > 0) {
        # 2yr_mean_citedness is the exact formula for the traditional Impact Factor
        jif <- content$results[[1]]$summary_stats$`2yr_mean_citedness`
        if (!is.null(jif)) return(round(as.numeric(jif), 2))
      }
    }
    return(NA)
  }, error = function(e) { return(NA) })
}

cat("Filtering dataset...\n")
clean_data <- data %>%
  mutate(outcome = tolower(outcome)) %>%
  filter(outcome %in% c("successful", "failed")) %>%
  filter(!grepl("sub_rep", outcome_quote, ignore.case = TRUE)) %>%
  mutate(
    outcome = ifelse(outcome == "successful", "success", "failure"),
    ref_o = title_o,
    ref_r = title_r,
    journal = journal_o,
    doi_o = sapply(doi_o, clean_doi),
    doi_r = sapply(doi_r, clean_doi)
  ) %>%
  select(ref_o, ref_r, journal, doi_o, doi_r, outcome, outcome_quote) %>%
  filter(!is.na(ref_o) & !is.na(outcome))

# --- NEW: Fetch Impact Factors for Unique Journals ---
unique_journals <- unique(clean_data$journal[!is.na(clean_data$journal)])
cat(sprintf("Fetching Impact Factors via OpenAlex for %d unique journals...\n", length(unique_journals)))

journal_jifs <- data.frame(journal = unique_journals, impact_factor = NA, stringsAsFactors = FALSE)
pb_jif <- txtProgressBar(min = 0, max = nrow(journal_jifs), style = 3)

for(i in seq_len(nrow(journal_jifs))) {
  Sys.sleep(0.1) # Polite delay
  journal_jifs$impact_factor[i] <- get_openalex_jif(journal_jifs$journal[i])
  setTxtProgressBar(pb_jif, i)
}
close(pb_jif)

# Merge the Impact Factors back into the main dataset
clean_data <- clean_data %>% left_join(journal_jifs, by = "journal")

# --- Fetch Abstracts ---
cat(sprintf("\nFetching abstracts via Crossref for %d studies. This may take a few minutes...\n", nrow(clean_data)))

clean_data$abstract <- NA
pb_abs <- txtProgressBar(min = 0, max = nrow(clean_data), style = 3)
for (i in seq_len(nrow(clean_data))) {
  Sys.sleep(0.1) # Polite delay
  clean_data$abstract[i] <- get_crossref_abstract(clean_data$doi_o[i])
  setTxtProgressBar(pb_abs, i)
}
close(pb_abs)

# CRITICAL FIX: Remove any studies where abstract is NA or very short
cat("\nDropping studies with missing abstracts...\n")
final_data <- clean_data %>% 
  filter(!is.na(abstract) & str_length(abstract) > 30) %>% 
  select(ref_o, ref_r, journal, impact_factor, abstract, outcome, outcome_quote, doi_o, doi_r)

# --- Smart path resolution ---
if (dir.exists("../Game")) { 
  out_dir <- "../Game" 
} else if (dir.exists("Game")) { 
  out_dir <- "Game" 
} else { 
  dir.create("Game", showWarnings = FALSE)
  out_dir <- "Game" 
}
# -----------------------------------------------------------------------------

json_path <- file.path(out_dir, "dataset.json")
csv_path <- file.path(out_dir, "dataset.csv")

cat("Writing files...\n")
write_json(final_data, json_path, pretty = TRUE)
write_csv(final_data, csv_path)

cat(sprintf("Success! Exported %d fully complete trials to %s and %s\n", nrow(final_data), json_path, csv_path))
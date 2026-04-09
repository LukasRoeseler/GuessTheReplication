#####################
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

# ==============================================================================
# 1. Download the full FReD dataset
# ==============================================================================
cat("Downloading flora.csv...\n")
url <- "https://raw.githubusercontent.com/forrtproject/FReD-data/refs/heads/main/output/flora.csv"
data <- read_csv(url, show_col_types = FALSE)

# ==============================================================================
# 2. Helper functions
# ==============================================================================

# --- Clean DOI prefix so we just have the raw 10.xxxx/... ---
clean_doi <- function(doi_string) {
  if (is.na(doi_string)) return(NA)
  str_replace(doi_string, "^https?://(dx\\.)?doi\\.org/", "")
}

# --- Fetch abstract from Crossref ---
get_crossref_abstract <- function(doi) {
  if (is.na(doi) || doi == "") return(NA)
  api_url <- paste0("https://api.crossref.org/works/", URLencode(doi))
  
  tryCatch({
    res <- GET(api_url,
               user_agent("GuessTheReplication/1.0 (mailto:lukas.roeseler@uni-muenster.de)"),
               timeout(5))
    if (status_code(res) == 200) {
      content <- content(res, as = "parsed", type = "application/json")
      abs <- content$message$abstract
      if (!is.null(abs)) {
        clean_abs <- str_replace_all(abs, "<[^>]+>", " ")
        clean_abs <- str_replace_all(clean_abs, "[\r\n]+", " ")
        clean_abs <- str_squish(clean_abs)
        clean_abs <- str_replace(clean_abs, "^(?i)abstract\\s*[:\\.\\-]?\\s*", "")
        return(clean_abs)
      }
    }
    return(NA)
  }, error = function(e) { return(NA) })
}

# --- Fallback: fetch abstract from OpenAlex ---
get_openalex_abstract <- function(doi) {
  if (is.na(doi) || doi == "") return(NA)
  api_url <- paste0("https://api.openalex.org/works/https://doi.org/",
                    URLencode(doi),
                    "?mailto=lukas.roeseler@uni-muenster.de")
  
  tryCatch({
    res <- GET(api_url,
               user_agent("GuessTheReplication/1.0 (mailto:lukas.roeseler@uni-muenster.de)"),
               timeout(5))
    if (status_code(res) == 200) {
      content <- content(res, as = "parsed", type = "application/json")
      inv_idx <- content$abstract_inverted_index
      if (!is.null(inv_idx) && length(inv_idx) > 0) {
        word_positions <- unlist(inv_idx)
        words          <- rep(names(inv_idx), lengths(inv_idx))
        reconstructed  <- words[order(word_positions)]
        abs_text       <- paste(reconstructed, collapse = " ")
        
        clean_abs <- str_replace_all(abs_text, "[\r\n]+", " ")
        clean_abs <- str_squish(clean_abs)
        clean_abs <- str_replace(clean_abs, "^(?i)abstract\\s*[:\\.\\-]?\\s*", "")
        return(clean_abs)
      }
    }
    return(NA)
  }, error = function(e) { return(NA) })
}

# --- Fallback: fetch abstract from Semantic Scholar ---
get_semanticscholar_abstract <- function(doi) {
  if (is.na(doi) || doi == "") return(NA)
  api_url <- paste0("https://api.semanticscholar.org/graph/v1/paper/DOI:",
                    URLencode(doi),
                    "?fields=abstract")
  
  tryCatch({
    res <- GET(api_url,
               user_agent("GuessTheReplication/1.0 (mailto:lukas.roeseler@uni-muenster.de)"),
               timeout(5))
    if (status_code(res) == 200) {
      content <- content(res, as = "parsed", type = "application/json")
      abs <- content$abstract
      if (!is.null(abs) && nchar(abs) > 0) {
        clean_abs <- str_replace_all(abs, "[\r\n]+", " ")
        clean_abs <- str_squish(clean_abs)
        clean_abs <- str_replace(clean_abs, "^(?i)abstract\\s*[:\\.\\-]?\\s*", "")
        return(clean_abs)
      }
    }
    return(NA)
  }, error = function(e) { return(NA) })
}

# --- Fetch Impact Factor (2yr_mean_citedness) from OpenAlex ---
get_openalex_jif <- function(journal_name) {
  if (is.na(journal_name) || journal_name == "") return(NA)
  api_url <- paste0("https://api.openalex.org/sources?search=", URLencode(journal_name))
  
  tryCatch({
    res <- GET(api_url,
               user_agent("GuessTheReplication/1.0 (mailto:lukas.roeseler@uni-muenster.de)"),
               timeout(5))
    if (status_code(res) == 200) {
      content <- content(res, as = "parsed", type = "application/json")
      if (length(content$results) > 0) {
        jif <- content$results[[1]]$summary_stats$`2yr_mean_citedness`
        if (!is.null(jif)) return(round(as.numeric(jif), 2))
      }
    }
    return(NA)
  }, error = function(e) { return(NA) })
}

# ==============================================================================
# 3. Fetch Impact Factors for ALL journals in the FULL dataset
# ==============================================================================
all_unique_journals <- unique(data$journal_o[!is.na(data$journal_o)])
cat(sprintf("Fetching Impact Factors via OpenAlex for %d unique journals (full dataset)...\n",
            length(all_unique_journals)))

journal_jifs <- data.frame(journal_o      = all_unique_journals,
                           impact_factor  = NA_real_,
                           stringsAsFactors = FALSE)

pb_jif <- txtProgressBar(min = 0, max = nrow(journal_jifs), style = 3)
for (i in seq_len(nrow(journal_jifs))) {
  Sys.sleep(0.1)
  journal_jifs$impact_factor[i] <- get_openalex_jif(journal_jifs$journal_o[i])
  setTxtProgressBar(pb_jif, i)
}
close(pb_jif)

# Merge Impact Factors into the FULL original dataset and save
data_with_if <- data %>% left_join(journal_jifs, by = "journal_o")

# --- Smart path resolution ---
if (dir.exists("../Game")) {
  out_dir <- "../Game"
} else if (dir.exists("Game")) {
  out_dir <- "Game"
} else {
  dir.create("Game", showWarnings = FALSE)
  out_dir <- "Game"
}

flora_if_path <- file.path(out_dir, "flora_with_if.csv")
cat(sprintf("\nSaving full dataset with Impact Factors to %s ...\n", flora_if_path))
write_csv(data_with_if, flora_if_path)

# ==============================================================================
# 4. Filter and clean the dataset for the game
# ==============================================================================
cat("Filtering dataset...\n")

# DOIs to explicitly drop
bad_dois <- c(
  "10.1037/cou0000131",
  "10.1017/s1930297500001170",
  "10.1037/0022-3514.94.2.94.2.231",
  "10.1073/pnas.0908963106",
  "10.1093/geronb/gbx099",
  "10.1177/0956797615624492",
  "10.1177/0956797619854706",
  "10.3389/fpsyg.2014.01276",
  "10.1210/jc.2009-0545"
)

# Outcome quotes to replace with the fallback message
bad_quotes <- c(
  "no",
  "no.",
  "yes stated in replicated column",
  "es stated in the rep. s1+s2 column",
  "yes stated in the rep. s1+s2 column",
  "no marked in replicate column",
  "yes marked in replicate column",
  "subjective replication success rating 0",
  "subjective replication success rating 1",
  "subjective replication success = 0",
  "subjective replication success = 1",
  "replicationsuccess marked as yes",
  "crossed in replication outcome column in supplementary table 2.",
  "ticked in replication outcome column in supplementary table 2.",
  "yes in replicationsuccess column",
  "yes stated in the rep. s1 column",
  "no in replicationsuccess column",
  "no marked in replicate (r) column",
  "no stated in the rep. s1+s2 column",
  "subjective replication success: 0",
  "table 2 null hypothesis significance tests <.001"
)

# Replacement message for bad quotes
fallback_quote <- "No outcome quote available; outcome was coded based on meta-analytical data"

# Long note strings to strip from outcome_quote
long_note_0 <- paste0(
  "0 [In figure 2, authors quantified subjective success on a more ",
  "nuanced scale from 0, 0.25, 0.5, 0.75 and 1. I am coding 0 as ",
  "'failed', 1 as 'success' and anything inbetween as 'mixed']."
)
long_note_1 <- paste0(
  "1 [In figure 2, authors quantified subjective success on a more ",
  "nuanced scale from 0, 0.25, 0.5, 0.75 and 1. I am coding 0 as ",
  "'failed', 1 as 'success' and anything inbetween as 'mixed']."
)

clean_data <- data %>%
  # 1. Clean DOIs
  mutate(
    doi_o = sapply(doi_o, clean_doi),
    doi_r = sapply(doi_r, clean_doi)
  ) %>%
  
  # 2. Exclude specific studies by DOI
  filter(!doi_o %in% bad_dois) %>%
  filter(!grepl("10.1126/science.aaf0918", doi_r, ignore.case = TRUE)) %>%
  filter(!grepl("10.17605/osf.io/fmd75", doi_r, ignore.case = TRUE)) %>%
  
  # 3. Recode specific DOI outcome before filtering
  mutate(
    outcome = ifelse(
      grepl("10.1037/xhp0000331", doi_r, ignore.case = TRUE),
      "failed", outcome
    )
  ) %>%
  mutate(outcome = tolower(outcome)) %>%
  filter(outcome %in% c("successful", "failed")) %>%
  
  # 4. Clean the outcome_quote
  mutate(
    outcome_quote = str_remove_all(outcome_quote, fixed(long_note_0)),
    outcome_quote = str_remove_all(outcome_quote, fixed(long_note_1)),
    outcome_quote = str_remove_all(outcome_quote, "[\"\u201C\u201D\u2018\u2019']"),
    outcome_quote = str_squish(outcome_quote)
  ) %>%
  
  # 5. Replace bad quotes and sub_rep quotes with fallback message
  mutate(
    outcome_quote = ifelse(
      tolower(outcome_quote) %in% bad_quotes |
        grepl("sub_rep", outcome_quote, ignore.case = TRUE) |
        outcome_quote == "",
      fallback_quote,
      outcome_quote
    )
  ) %>%
  
  # 6. Map the final variables for the game
  mutate(
    outcome = ifelse(outcome == "successful", "success", "failure"),
    ref_o   = title_o,
    ref_r   = title_r,
    journal = journal_o,
    study_o = ifelse(
      grepl("\\d", study_o),
      str_extract(study_o, "\\d+[a-zA-Z]*"),
      NA_character_
    )
  ) %>%
  select(ref_o, ref_r, apa_ref_o, apa_ref_r, journal, study_o,
         doi_o, doi_r, outcome, outcome_quote) %>%
  filter(!is.na(ref_o) & !is.na(outcome))

# ==============================================================================
# 5. Merge previously fetched Impact Factors into the game dataset
# ==============================================================================
game_jifs <- journal_jifs %>% rename(journal = journal_o)
clean_data <- clean_data %>% left_join(game_jifs, by = "journal")

# ==============================================================================
# 6. Fetch abstracts (Crossref -> OpenAlex -> Semantic Scholar)
# ==============================================================================
cat(sprintf(
  "\nFetching abstracts via Crossref (with OpenAlex and Semantic Scholar fallbacks) for %d studies.\nThis may take a few minutes...\n",
  nrow(clean_data)
))

clean_data$abstract <- NA_character_
pb_abs <- txtProgressBar(min = 0, max = nrow(clean_data), style = 3)

for (i in seq_len(nrow(clean_data))) {
  Sys.sleep(0.1)
  
  # Try Crossref first
  abs_text <- get_crossref_abstract(clean_data$doi_o[i])
  
  # Fallback 1: OpenAlex
  if (is.na(abs_text) || nchar(abs_text) < 30) {
    abs_text <- get_openalex_abstract(clean_data$doi_o[i])
  }
  
  # Fallback 2: Semantic Scholar
  if (is.na(abs_text) || nchar(abs_text) < 30) {
    abs_text <- get_semanticscholar_abstract(clean_data$doi_o[i])
  }
  
  clean_data$abstract[i] <- abs_text
  setTxtProgressBar(pb_abs, i)
}
close(pb_abs)

# ==============================================================================
# 7. Final filtering and export
# ==============================================================================

# Remove studies with missing or very short abstracts only
cat("\nDropping studies with missing or very short abstracts...\n")
final_data <- clean_data %>%
  filter(!is.na(abstract) & nchar(abstract) > 30) %>%
  select(ref_o, ref_r, apa_ref_o, apa_ref_r, journal, impact_factor,
         study_o, abstract, outcome, outcome_quote, doi_o, doi_r)

# --- Write output files ---
json_path    <- file.path(out_dir, "dataset.json")
csv_path     <- file.path(out_dir, "dataset.csv")
dataset_path <- file.path(out_dir, "journaldata.csv")

cat("Writing files...\n")
write_json(final_data, json_path, pretty = TRUE)
write_csv(final_data, csv_path)
write_csv(game_jifs, dataset_path)

cat(sprintf("Success! Exported %d fully complete trials to %s and %s\n",
            nrow(final_data), json_path, csv_path))
cat(sprintf("Full dataset with Impact Factors saved to %s\n", flora_if_path))
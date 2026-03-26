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

clean_doi <- function(doi_string) {
  if (is.na(doi_string)) return(NA)
  str_replace(doi_string, "^https?://(dx\\.)?doi\\.org/", "")
}

get_crossref_abstract <- function(doi) {
  if (is.na(doi) || doi == "") return(NA)
  api_url <- paste0("https://api.crossref.org/works/", URLencode(doi))
  
  tryCatch({
    res <- GET(api_url, user_agent("GuessTheReplication/1.0"), timeout(5))
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

get_openalex_abstract <- function(doi) {
  if (is.na(doi) || doi == "") return(NA)
  api_url <- paste0("https://api.openalex.org/works/https://doi.org/", URLencode(doi))
  
  tryCatch({
    res <- GET(api_url, user_agent("GuessTheReplication/1.0"), timeout(5))
    if (status_code(res) == 200) {
      content <- content(res, as = "parsed", type = "application/json")
      inv_idx <- content$abstract_inverted_index
      if (!is.null(inv_idx) && length(inv_idx) > 0) {
        word_positions <- unlist(inv_idx)
        words <- rep(names(inv_idx), lengths(inv_idx))
        reconstructed <- words[order(word_positions)]
        abs_text <- paste(reconstructed, collapse = " ")
        clean_abs <- str_replace_all(abs_text, "[\r\n]+", " ")
        clean_abs <- str_squish(clean_abs)
        clean_abs <- str_replace(clean_abs, "^(?i)abstract\\s*[:\\.\\-]?\\s*", "")
        return(clean_abs)
      }
    }
    return(NA)
  }, error = function(e) { return(NA) })
}

get_openalex_jif <- function(journal_name) {
  if (is.na(journal_name) || journal_name == "") return(NA)
  api_url <- paste0("https://api.openalex.org/sources?search=", URLencode(journal_name))
  tryCatch({
    res <- GET(api_url, user_agent("GuessTheReplication/1.0"), timeout(5))
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

cat("Filtering dataset...\n")

bad_quotes <- c(
  "no", "no.", "yes stated in replicated column", "es stated in the rep. s1+s2 column",
  "yes stated in the rep. s1+s2 column", "no marked in replicate column", "yes marked in replicate column",
  "subjective replication success rating 0", "subjective replication success rating 1",
  "replicationsuccess marked as yes", "crossed in replication outcome column in supplementary table 2.",
  "ticked in replication outcome column in supplementary table 2.", "yes in replicationsuccess column",
  "yes stated in the rep. s1 column", "no in replicationsuccess column", "no marked in replicate (r) column",
  "no stated in the rep. s1+s2 column", "subjective replication success: 0",
  "table 2 null hypothesis significance tests <.001"
)

clean_data <- data %>%
  mutate(outcome = ifelse(grepl("10.1037/xhp0000331", doi_r, ignore.case = TRUE), "failed", outcome)) %>%
  mutate(outcome = tolower(outcome)) %>%
  filter(outcome %in% c("successful", "failed")) %>%
  filter(!grepl("10.1126/science.aaf0918", doi_r, ignore.case = TRUE)) %>%
  filter(!grepl("10.17605/osf.io/fmd75", doi_r, ignore.case = TRUE)) %>%
  mutate(
    outcome_quote = str_remove_all(outcome_quote, fixed("0 [In figure 2, authors quantified subjective success on a more nuanced scale from 0, 0.25, 0.5, 0.75 and 1. I am coding 0 as 'failed', 1 as 'success' and anything inbetween as 'mixed'].")),
    outcome_quote = str_remove_all(outcome_quote, fixed("1 [In figure 2, authors quantified subjective success on a more nuanced scale from 0, 0.25, 0.5, 0.75 and 1. I am coding 0 as 'failed', 1 as 'success' and anything inbetween as 'mixed'].")),
    outcome_quote = str_remove_all(outcome_quote, "(?i)subjective replication success( rating)?:?\\s?[01]"),
    # NEW: Remove all quotation marks from the string
    outcome_quote = str_remove_all(outcome_quote, "[\"“'”‘’]"), 
    outcome_quote = str_squish(outcome_quote)
  ) %>%
  filter(!tolower(outcome_quote) %in% bad_quotes) %>%
  filter(!grepl("sub_rep", outcome_quote, ignore.case = TRUE)) %>%
  filter(outcome_quote != "") %>%
  mutate(
    outcome = ifelse(outcome == "successful", "success", "failure"),
    ref_o = title_o,
    ref_r = title_r,
    apa_ref_o = apa_ref_o,
    apa_ref_r = apa_ref_r,
    journal = journal_o,
    study_o = ifelse(grepl("\\d", study_o), str_extract(study_o, "\\d+[a-zA-Z]*"), NA_character_),
    doi_o = sapply(doi_o, clean_doi),
    doi_r = sapply(doi_r, clean_doi)
  ) %>%
  select(ref_o, ref_r, apa_ref_o, apa_ref_r, journal, study_o, doi_o, doi_r, outcome, outcome_quote) %>%
  filter(!is.na(ref_o) & !is.na(outcome))

unique_journals <- unique(clean_data$journal[!is.na(clean_data$journal)])
cat(sprintf("Fetching Impact Factors via OpenAlex for %d unique journals...\n", length(unique_journals)))

journal_jifs <- data.frame(journal = unique_journals, impact_factor = NA, stringsAsFactors = FALSE)
pb_jif <- txtProgressBar(min = 0, max = nrow(journal_jifs), style = 3)
for(i in seq_len(nrow(journal_jifs))) {
  Sys.sleep(0.1) 
  journal_jifs$impact_factor[i] <- get_openalex_jif(journal_jifs$journal[i])
  setTxtProgressBar(pb_jif, i)
}
close(pb_jif)

clean_data <- clean_data %>% left_join(journal_jifs, by = "journal")

cat(sprintf("\nFetching abstracts via Crossref (and OpenAlex fallback) for %d studies. This may take a few minutes...\n", nrow(clean_data)))
clean_data$abstract <- NA
pb_abs <- txtProgressBar(min = 0, max = nrow(clean_data), style = 3)
for (i in seq_len(nrow(clean_data))) {
  Sys.sleep(0.1) 
  abs_text <- get_crossref_abstract(clean_data$doi_o[i])
  if (is.na(abs_text) || str_length(abs_text) < 30) {
    abs_text <- get_openalex_abstract(clean_data$doi_o[i])
  }
  clean_data$abstract[i] <- abs_text
  setTxtProgressBar(pb_abs, i)
}
close(pb_abs)

cat("\nDropping studies with missing abstracts or abstracts starting with 'Significance'...\n")
final_data <- clean_data %>% 
  filter(!is.na(abstract) & str_length(abstract) > 30) %>% 
  # NEW: Drop if abstract starts with "significance"
  filter(!str_detect(abstract, "^(?i)significance")) %>%
  select(ref_o, ref_r, apa_ref_o, apa_ref_r, journal, impact_factor, study_o, abstract, outcome, outcome_quote, doi_o, doi_r)

if (dir.exists("../Game")) { out_dir <- "../Game" } else if (dir.exists("Game")) { out_dir <- "Game" } else { dir.create("Game", showWarnings = FALSE); out_dir <- "Game" }

json_path <- file.path(out_dir, "dataset.json")
csv_path <- file.path(out_dir, "dataset.csv")

write_json(final_data, json_path, pretty = TRUE)
write_csv(final_data, csv_path)
cat(sprintf("Success! Exported %d fully complete trials to %s and %s\n", nrow(final_data), json_path, csv_path))

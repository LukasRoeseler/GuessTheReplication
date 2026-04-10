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

# --- Helper Functions ---
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
        clean_abs <- str_squish(str_replace_all(str_replace_all(abs, "<[^>]+>", " "), "[\r\n]+", " "))
        return(str_replace(clean_abs, "^(?i)abstract\\s*[:\\.\\-]?\\s*", ""))
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
        abs_text <- paste(words[order(word_positions)], collapse = " ")
        clean_abs <- str_squish(str_replace_all(abs_text, "[\r\n]+", " "))
        return(str_replace(clean_abs, "^(?i)abstract\\s*[:\\.\\-]?\\s*", ""))
      }
    }
    return(NA)
  }, error = function(e) { return(NA) })
}

get_semanticscholar_abstract <- function(doi) {
  if (is.na(doi) || doi == "") return(NA)
  api_url <- paste0("https://api.semanticscholar.org/graph/v1/paper/DOI:", URLencode(doi), "?fields=abstract")
  tryCatch({
    res <- GET(api_url, user_agent("GuessTheReplication/1.0"), timeout(5))
    if (status_code(res) == 200) {
      content <- content(res, as = "parsed", type = "application/json")
      abs <- content$abstract
      if (!is.null(abs) && abs != "") {
        clean_abs <- str_squish(str_replace_all(abs, "[\r\n]+", " "))
        return(str_replace(clean_abs, "^(?i)abstract\\s*[:\\.\\-]?\\s*", ""))
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

# --- Step 1: Load Datasets ---
cat("Downloading live flora.csv...\n")
fresh_flora <- read_csv("https://raw.githubusercontent.com/forrtproject/FReD-data/refs/heads/main/output/flora.csv", show_col_types = FALSE)

cat("Downloading existing Game datasets from GitHub...\n")
old_dataset <- tryCatch(read_csv("https://raw.githubusercontent.com/LukasRoeseler/GuessTheReplication/refs/heads/main/Game/dataset.csv", show_col_types = FALSE), error = function(e) data.frame())
journaldata <- tryCatch(read_csv("https://raw.githubusercontent.com/LukasRoeseler/GuessTheReplication/refs/heads/main/Game/journaldata.csv", show_col_types = FALSE), error = function(e) data.frame(journal=character(), impact_factor=numeric()))


# --- Step 2: Update Journal Impact Factors ---
cat("Updating Journal Impact Factors...\n")

# 1. Retry fetching NAs in existing journaldata
na_jifs <- which(is.na(journaldata$impact_factor))
if(length(na_jifs) > 0) {
  cat(sprintf("Retrying OpenAlex for %d previously failed journals...\n", length(na_jifs)))
  for(i in na_jifs) {
    Sys.sleep(0.1)
    journaldata$impact_factor[i] <- get_openalex_jif(journaldata$journal[i])
  }
}

# 2. Fetch JIFs for brand new journals in fresh_flora
fresh_journals <- unique(fresh_flora$journal_o[!is.na(fresh_flora$journal_o)])
new_journals <- setdiff(fresh_journals, journaldata$journal)

if(length(new_journals) > 0) {
  cat(sprintf("Fetching OpenAlex for %d brand new journals...\n", length(new_journals)))
  new_jifs <- data.frame(journal = new_journals, impact_factor = NA, stringsAsFactors = FALSE)
  for(i in seq_len(nrow(new_jifs))) {
    Sys.sleep(0.1)
    new_jifs$impact_factor[i] <- get_openalex_jif(new_jifs$journal[i])
  }
  journaldata <- bind_rows(journaldata, new_jifs)
}


# --- Step 3: Process and Clean the FLoRA dataset ---
cat("Cleaning dataset and applying inclusion rules...\n")

bad_dois <- c(
  "10.1037/cou0000131", "10.1017/s1930297500001170", "10.1037/0022-3514.94.2.94.2.231",
  "10.1073/pnas.0908963106", "10.1093/geronb/gbx099", "10.1177/0956797615624492",
  "10.1177/0956797619854706", "10.3389/fpsyg.2014.01276", "10.1210/jc.2009-0545"
)

bad_quotes <- c(
  "no", "no.", "yes stated in replicated column", "es stated in the rep. s1+s2 column",
  "yes stated in the rep. s1+s2 column", "no marked in replicate column", "yes marked in replicate column",
  "subjective replication success rating 0", "subjective replication success rating 1",
  "subjective replication success = 0", "subjective replication success = 1",
  "replicationsuccess marked as yes", "crossed in replication outcome column in supplementary table 2.",
  "ticked in replication outcome column in supplementary table 2.", "yes in replicationsuccess column",
  "yes stated in the rep. s1 column", "no in replicationsuccess column", "no marked in replicate (r) column",
  "no stated in the rep. s1+s2 column", "subjective replication success: 0",
  "table 2 null hypothesis significance tests <.001"
)

candidate_data <- fresh_flora %>%
  mutate(
    doi_o_clean = sapply(doi_o, clean_doi),
    doi_r_clean = sapply(doi_r, clean_doi)
  ) %>%
  mutate(outcome_clean = ifelse(grepl("10.1037/xhp0000331", doi_r_clean, ignore.case = TRUE), "failed", outcome)) %>%
  mutate(outcome_clean = tolower(outcome_clean)) %>%
  filter(outcome_clean %in% c("successful", "failed")) %>%
  filter(!grepl("10.1126/science.aaf0918", doi_r_clean, ignore.case = TRUE)) %>%
  filter(!grepl("10.17605/osf.io/fmd75", doi_r_clean, ignore.case = TRUE)) %>%
  mutate(
    quote_temp = str_remove_all(outcome_quote, fixed("0 [In figure 2, authors quantified subjective success on a more nuanced scale from 0, 0.25, 0.5, 0.75 and 1. I am coding 0 as 'failed', 1 as 'success' and anything inbetween as 'mixed'].")),
    quote_temp = str_remove_all(quote_temp, fixed("1 [In figure 2, authors quantified subjective success on a more nuanced scale from 0, 0.25, 0.5, 0.75 and 1. I am coding 0 as 'failed', 1 as 'success' and anything inbetween as 'mixed'].")),
    quote_temp = str_remove_all(quote_temp, "(?i)subjective replication success\\s*=\\s*[01]"),
    quote_temp = str_remove_all(quote_temp, "[\"“'”‘’]"),
    quote_temp = str_squish(quote_temp),
    
    raw_quote_lower = tolower(quote_temp),
    
    is_bad_quote = (raw_quote_lower %in% bad_quotes) |
      grepl("sub_rep", raw_quote_lower, ignore.case = TRUE) |
      grepl("subjective replication success", raw_quote_lower, ignore.case = TRUE) |
      is.na(raw_quote_lower) | raw_quote_lower == "",
    
    is_bad_doi = doi_o_clean %in% bad_dois,
    
    outcome_quote_final = ifelse(
      is_bad_quote | is_bad_doi,
      "No quote available (e.g., replication outcome was indicated via a value in a table)",
      quote_temp
    ),
    
    outcome = ifelse(outcome_clean == "successful", "success", "failure"),
    ref_o = title_o,
    ref_r = title_r,
    journal = journal_o,
    study_o_clean = ifelse(grepl("\\d", study_o), str_extract(study_o, "\\d+[a-zA-Z]*"), NA_character_)
  ) %>%
  select(ref_o, ref_r, apa_ref_o, apa_ref_r, journal, study_o = study_o_clean, 
         doi_o = doi_o_clean, doi_r = doi_r_clean, outcome, outcome_quote = outcome_quote_final) %>%
  filter(!is.na(ref_o) & !is.na(outcome)) %>%
  left_join(journaldata, by = "journal")


# --- Step 4: Map existing abstracts and fetch missing ones ---

# Isolate previously fetched abstracts (if any exist locally)
if(nrow(old_dataset) > 0) {
  known_abstracts <- old_dataset %>% select(doi_o, doi_r, abstract)
  candidate_data <- candidate_data %>% left_join(known_abstracts, by = c("doi_o", "doi_r"))
} else {
  candidate_data$abstract <- NA
}

# Identify rows that STILL need an abstract
needs_abstract <- which(is.na(candidate_data$abstract) | str_length(candidate_data$abstract) < 30)

if(length(needs_abstract) > 0) {
  cat(sprintf("\nFetching missing abstracts via APIs for %d studies...\n", length(needs_abstract)))
  
  pb_abs <- txtProgressBar(min = 0, max = length(needs_abstract), style = 3)
  for (idx in seq_along(needs_abstract)) {
    i <- needs_abstract[idx]
    doi <- candidate_data$doi_o[i]
    
    Sys.sleep(0.1)
    abs_text <- get_crossref_abstract(doi)
    
    if (is.na(abs_text) || str_length(abs_text) < 30) {
      abs_text <- get_openalex_abstract(doi)
    }
    if (is.na(abs_text) || str_length(abs_text) < 30) {
      Sys.sleep(0.5) # Semantic scholar limit
      abs_text <- get_semanticscholar_abstract(doi)
    }
    
    candidate_data$abstract[i] <- abs_text
    setTxtProgressBar(pb_abs, idx)
  }
  close(pb_abs)
} else {
  cat("\nAll abstracts are already cached! No API fetching needed.\n")
}

# --- Step 5: Finalizing Datasets ---

cat("\nCompiling final game dataset...\n")
final_dataset <- candidate_data %>% 
  filter(!is.na(abstract) & str_length(abstract) > 30) %>% 
  select(ref_o, ref_r, apa_ref_o, apa_ref_r, journal, impact_factor, study_o, abstract, outcome, outcome_quote, doi_o, doi_r)

cat("Compiling full analytical flora dataset...\n")
# Create flora_with_if by mapping clean keys
full_flora_with_if <- fresh_flora %>%
  left_join(journaldata, by = c("journal_o" = "journal")) %>%
  mutate(
    temp_doi_o = sapply(doi_o, clean_doi),
    temp_doi_r = sapply(doi_r, clean_doi),
    study_key = paste(temp_doi_o, temp_doi_r, sep="_"),
    included_in_guther = study_key %in% paste(final_dataset$doi_o, final_dataset$doi_r, sep="_")
  ) %>%
  select(-temp_doi_o, -temp_doi_r, -study_key)

# --- Smart path resolution ---
if (dir.exists("../Game")) { out_dir <- "../Game" } else if (dir.exists("Game")) { out_dir <- "Game" } else { dir.create("Game", showWarnings = FALSE); out_dir <- "Game" }

json_path <- file.path(out_dir, "dataset.json")
csv_path <- file.path(out_dir, "dataset.csv")
flora_if_path <- file.path(out_dir, "flora_with_if.csv")
journal_path <- file.path(out_dir, "journaldata.csv")

cat("\nWriting files locally...\n")
write_json(final_dataset, json_path, pretty = TRUE)
write_csv(final_dataset, csv_path)
write_csv(journaldata, journal_path)
write_csv(full_flora_with_if, flora_if_path)

cat(sprintf("Success! Exported %d trials to %s and %s\n", nrow(final_dataset), json_path, csv_path))
cat(sprintf("Saved full analytical dataset with IFs to %s\n", flora_if_path))
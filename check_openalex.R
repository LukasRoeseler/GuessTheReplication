# Run this interactively to check:
test <- get_openalex_jif("Psychological Science")
cat("Test JIF for 'Psychological Science':", test, "\n")
# Should print something like: Test JIF for 'Psychological Science': 5.08

res <- GET("https://api.openalex.org/sources?search=Psychological%20Science&mailto=lukas.roeseler@uni-muenster.de",
           timeout(3))
str(content(res, as = "parsed")$results[[1]]$summary_stats, max.level = 1)




library(httr)

res <- GET("https://api.openalex.org/sources?search=Psychological%20Science",
           user_agent("GuessTheReplication/1.0 (mailto:lukas.roeseler@uni-muenster.de)"),
           timeout(10))

cat("Status:", status_code(res), "\n")

if (status_code(res) == 200) {
  content <- content(res, as = "parsed", type = "application/json")
  cat("Results found:", length(content$results), "\n")
  cat("First result:", content$results[[1]]$display_name, "\n")
  jif <- content$results[[1]]$summary_stats$`2yr_mean_citedness`
  cat("2yr_mean_citedness:", ifelse(is.null(jif), "NULL", jif), "\n")
} else {
  cat("Response body:\n")
  cat(content(res, as = "text", encoding = "UTF-8"), "\n")
}

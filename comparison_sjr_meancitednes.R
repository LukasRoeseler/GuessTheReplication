# Compare OpenAlex Mean Citedness with JIF


# Journal Data (OpenAlex mean citedness)
mc <- read.csv("Game/journaldata.csv")
names(mc) <- c("Title", "mc")
mc$mc <- as.numeric(sub(",", ".", mc$mc, fixed = TRUE))
# Scimago Journal Rank (free and similar to JIF whereas Clarivates Journal Citation Report is paywalled)
jr <- read.csv("Study/scimagojr 2024.csv", sep = ";")
jr$SJR <- as.numeric(sub(",", ".", jr$SJR, fixed = TRUE))
jr$mc2 <- as.numeric(sub(",", ".", jr$Citations...Doc...2years., fixed = TRUE))
# Merge datasets by journal name
mcjr <- merge(mc, jr, by = "Title")

# number of journals for which both values are available
nrow(mc)
nrow(mcjr)

# Relationship
cor.test(mcjr$SJR, mcjr$mc, method = "spearman")
cor.test(mcjr$mc2, mcjr$mc, method = "spearman")

# Plot
p1 <- ggplot(mcjr, aes(x = SJR, y = mc)) + geom_point() + xlab("Scimago Journal Rank") + 
  ylab("OpenAlex Mean Citedness") + 
  theme_bw() + geom_abline(intercept = 0, slope = 1, linetype = "dashed")

p2 <- ggplot(mcjr, aes(x = mc2, y = mc)) + geom_point() + xlab("Citations per Docs (2 years)") + 
  ylab("OpenAlex Mean Citedness") + 
  theme_bw() + geom_abline(intercept = 0, slope = 1, linetype = "dashed")

p1 + p2
ggsave("Study/oa_metrics_comparison.jpg", width = 1200, height = 600, units = "px", scale = 2.5)

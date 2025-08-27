# Domain search script for each Council's ecosystem approach

source(here::here("R/Domainsearch.R"))

results <- search_domain_for_string(
  base_url = "https://www.mafmc.org/",
  search_string = c("ecosystem approach|ecosystem indicator|ecosystem report|ecosystem status"),
  max_pages = 1000,        # Maximum pages to crawl; 
  max_depth = 3,          # How deep to crawl; 
  doc_extensions = c("pdf", "doc", "docx", "txt"),
  parallel = FALSE,        # Use parallel processing--Don't, too many download errors
  cache_dir = NULL,      # Cache downloaded content
  delay = 1,               # Seconds between requests
  save_filename = "MAFMC_ecosystemind.csv"
)

# Analyze results
analyze_search_results(results)

# Export found documents
export_found_documents(results, format = "csv")

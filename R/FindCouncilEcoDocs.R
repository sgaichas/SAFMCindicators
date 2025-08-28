# Domain search script for each Council's ecosystem approach

source(here::here("R/Domainsearch.R"))


##### MAFMC

results <- search_domain_for_string(
  base_url = "https://www.mafmc.org/",
  search_string = c("ecosystem approach|ecosystem indicator|ecosystem report|ecosystem status|ecosystem based"),
  max_pages = 500,        # Maximum pages to crawl; 
  max_depth = 3,          # How deep to crawl; 
  doc_extensions = c("pdf", "doc", "docx", "txt"),
  parallel = FALSE,        # Use parallel processing--Don't, too many download errors
  cache_dir = NULL,      # Cache downloaded content
  delay = 1,               # Seconds between requests
  save_filename = "MAFMCecosearch.csv"
)

# Analyze results
analyze_search_results(results)

# Export found documents
export_found_documents(results, format = "csv")


######## NEFMC

results <- search_domain_for_string(
  base_url = "https://www.nefmc.org/",
  search_string = c("ecosystem approach|ecosystem indicator|ecosystem report|ecosystem status|ecosystem based"),
  max_pages = 500,        # Maximum pages to crawl; 
  max_depth = 3,          # How deep to crawl; 
  doc_extensions = c("pdf", "doc", "docx", "txt"),
  parallel = FALSE,        # Use parallel processing--Don't, too many download errors
  cache_dir = NULL,      # Cache downloaded content
  delay = 1,               # Seconds between requests
  save_filename = "NEFMCecosearch.csv"
)

# Analyze results
analyze_search_results(results)

# Export found documents
export_found_documents(results, format = "csv")

######## SAFMC

results <- search_domain_for_string(
  base_url = "https://safmc.net/",
  search_string = c("ecosystem approach|ecosystem indicator|ecosystem report|ecosystem status|ecosystem based"),
  max_pages = 500,        # Maximum pages to crawl; 
  max_depth = 3,          # How deep to crawl; 
  doc_extensions = c("pdf", "doc", "docx", "txt"),
  parallel = FALSE,        # Use parallel processing--Don't, too many download errors
  cache_dir = NULL,      # Cache downloaded content
  delay = 1,               # Seconds between requests
  save_filename = "SAFMCecosearch.csv"
)

# Analyze results
analyze_search_results(results)

# Export found documents
export_found_documents(results, format = "csv")

######## NPFMC

results <- search_domain_for_string(
  base_url = "https://www.npfmc.org/",
  search_string = c("ecosystem approach|ecosystem indicator|ecosystem report|ecosystem status|ecosystem based"),
  max_pages = 500,        # Maximum pages to crawl; 
  max_depth = 3,          # How deep to crawl; 
  doc_extensions = c("pdf", "doc", "docx", "txt"),
  parallel = FALSE,        # Use parallel processing--Don't, too many download errors
  cache_dir = NULL,      # Cache downloaded content
  delay = 1,               # Seconds between requests
  save_filename = "NPFMCecosearch.csv"
)

# Analyze results
analyze_search_results(results)

# Export found documents
export_found_documents(results, format = "csv")

######## PFMC

results <- search_domain_for_string(
  base_url = "https://www.pcouncil.org/",
  search_string = c("ecosystem approach|ecosystem indicator|ecosystem report|ecosystem status|ecosystem based"),
  max_pages = 500,        # Maximum pages to crawl; 
  max_depth = 3,          # How deep to crawl; 
  doc_extensions = c("pdf", "doc", "docx", "txt"),
  parallel = FALSE,        # Use parallel processing--Don't, too many download errors
  cache_dir = NULL,      # Cache downloaded content
  delay = 1,               # Seconds between requests
  save_filename = "PFMCecosearch.csv"
)

# Analyze results
analyze_search_results(results)

# Export found documents
export_found_documents(results, format = "csv")

######## WPFMC

results <- search_domain_for_string(
  base_url = "https://www.wpcouncil.org/",
  search_string = c("ecosystem approach|ecosystem indicator|ecosystem report|ecosystem status|ecosystem based"),
  max_pages = 500,        # Maximum pages to crawl; 
  max_depth = 3,          # How deep to crawl; 
  doc_extensions = c("pdf", "doc", "docx", "txt"),
  parallel = FALSE,        # Use parallel processing--Don't, too many download errors
  cache_dir = NULL,      # Cache downloaded content
  delay = 1,               # Seconds between requests
  save_filename = "WPFMCecosearch.csv"
)

# Analyze results
analyze_search_results(results)

# Export found documents
export_found_documents(results, format = "csv")

######## GFMC

results <- search_domain_for_string(
  base_url = "https://gulfcouncil.org/",
  search_string = c("ecosystem approach|ecosystem indicator|ecosystem report|ecosystem status|ecosystem based"),
  max_pages = 500,        # Maximum pages to crawl; 
  max_depth = 3,          # How deep to crawl; 
  doc_extensions = c("pdf", "doc", "docx", "txt"),
  parallel = FALSE,        # Use parallel processing--Don't, too many download errors
  cache_dir = NULL,      # Cache downloaded content
  delay = 1,               # Seconds between requests
  save_filename = "GFMCecosearch.csv"
)

# Analyze results
analyze_search_results(results)

# Export found documents
export_found_documents(results, format = "csv")

######## CFMC

results <- search_domain_for_string(
  base_url = "https://www.caribbeanfmc.com/",
  search_string = c("ecosystem approach|ecosystem indicator|ecosystem report|ecosystem status|ecosystem based"),
  max_pages = 500,        # Maximum pages to crawl; 
  max_depth = 3,          # How deep to crawl; 
  doc_extensions = c("pdf", "doc", "docx", "txt"),
  parallel = FALSE,        # Use parallel processing--Don't, too many download errors
  cache_dir = NULL,      # Cache downloaded content
  delay = 1,               # Seconds between requests
  save_filename = "CFMCecosearch.csv"
)

# Analyze results
analyze_search_results(results)

# Export found documents
export_found_documents(results, format = "csv")





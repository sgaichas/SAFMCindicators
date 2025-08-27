# New Claude code from instruction
# "search an entire website domain for documents containing a certain string"
# My changes: 
#     * replace magrittr pipe with native pipe
#     * wrap all ignore.case calls in stringr::regex, 
#       see https://stringr.tidyverse.org/articles/regular-expressions.html
#     * change ignore.case to ignore_case
#     * fix introduced NAs in crawl_website with additional filtering of NA values
#     * remove extra base:: in line 155, should be digest::digest()
#     * and remove all the rest of the base:: for readability
#     * added envir=environment() to the call to parallel::clusterExport to pass arguments
#     * however, parallel processing results in MANY more download errors so not using


# R Code to Search Entire Website Domain for Documents Containing Specific String

# Required packages
library(rvest)
library(httr)
library(stringr)
library(purrr)
library(dplyr)
library(infix)

# Step 1: Website crawler to find all pages
crawl_website <- function(base_url, max_pages = 100, max_depth = 3, 
                          exclude_patterns = c("login", "admin", "private"),
                          delay = 1) {
  
  # Initialize
  visited_urls <- character(0)
  urls_to_visit <- base_url
  all_urls <- character(0)
  current_depth <- 0
  
  # Extract domain for filtering
  domain <- stringr::str_extract(base_url, "https?://[^/]+")
  
  cat(paste("Starting crawl of domain:", domain, "\n"))
  
  while (length(urls_to_visit) > 0 && 
         length(visited_urls) < max_pages && 
         current_depth <= max_depth) {
    
    current_batch <- urls_to_visit
    urls_to_visit <- character(0)
    current_depth <- current_depth + 1
    
    cat(paste("Crawling depth", current_depth, "- checking", 
                    length(current_batch), "URLs\n"))
    
    for (url in current_batch) {
      
      if (url %in% visited_urls) next
      
      # Skip excluded patterns
      if (any(stringr::str_detect(url, stringr::regex(exclude_patterns, ignore_case = TRUE)))) {
        next
      }
      
      tryCatch({
        # Rate limiting
        if (delay > 0) Sys.sleep(delay)
        
        # Fetch page
        page <- rvest::read_html(url)
        visited_urls <- c(visited_urls, url)
        all_urls <- c(all_urls, url)
        
        cat(".")
        
        # Extract links from current page
        links <- page |>
          rvest::html_nodes("a") |>
          rvest::html_attr("href")
        
        # Process links
        links <- links[!is.na(links)]
        links <- xml2::url_absolute(links, url)
        links <- links[!is.na(links)] # blank links become NA again
        
        # Filter for same domain and not already visited
        new_links <- links[stringr::str_starts(links, domain) & 
                             !links %in% visited_urls &
                             !links %in% urls_to_visit]
        
        # Remove fragments and duplicates
        new_links <- stringr::str_remove(new_links, "#.*$")
        new_links <- unique(new_links)
        
        urls_to_visit <- c(urls_to_visit, new_links)
        
      }, error = function(e) {
        cat("x")  # Mark failed URLs
      })
      
      if (length(visited_urls) >= max_pages) break
    }
    
    cat(paste("\nCompleted depth", current_depth, "- found", 
                    length(visited_urls), "total pages\n"))
  }
  
  return(unique(all_urls))
}

# Step 2: Extract all document links from multiple pages
extract_all_document_links <- function(page_urls, doc_extensions = c("pdf", "doc", "docx", "txt", "xls", "xlsx", "ppt", "pptx")) {
  
  cat("Extracting document links from all pages...\n")
  
  all_doc_links <- character(0)
  
  for (i in seq_along(page_urls)) {
    
    tryCatch({
      page <- rvest::read_html(page_urls[i])
      
      # Extract all links
      links <- page |>
        rvest::html_nodes("a") |>
        rvest::html_attr("href")
      
      links <- links[!is.na(links)]
      links <- xml2::url_absolute(links, page_urls[i])
      
      # Filter for documents
      doc_pattern <- paste0("\\.(", paste(doc_extensions, collapse = "|"), ")($|\\?)")
      doc_links <- links[stringr::str_detect(links, stringr::regex(doc_pattern, ignore_case = TRUE))]
      
      all_doc_links <- c(all_doc_links, doc_links)
      
      if (i %% 10 == 0) {
        cat(paste("Processed", i, "of", length(page_urls), "pages\n"))
      }
      
    }, error = function(e) {
      # Skip pages that fail to load
    })
  }
  
  # Remove duplicates
  all_doc_links <- unique(all_doc_links)
  
  cat(paste("Found", length(all_doc_links), "unique document links\n"))
  
  return(all_doc_links)
}

# Step 3: Advanced document content checker with caching
check_document_for_string_cached <- function(doc_url, search_string, cache_dir = NULL) {
  
  # Create cache directory if specified
  if (!is.null(cache_dir) && !dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }
  
  # Generate cache file name
  if (!is.null(cache_dir)) {
    url_hash <- digest::digest(doc_url, algo = "md5")
    cache_file <- file.path(cache_dir, paste0(url_hash, ".txt"))
    
    # Check if cached
    if (file.exists(cache_file)) {
      cached_content <- readLines(cache_file, warn = FALSE) |>
        paste(collapse = "\n")
      
      found <- stringr::str_detect(cached_content, stringr::regex(stringr::fixed(search_string), ignore_case = TRUE))
      
      return(list(
        url = doc_url,
        found = found,
        cached = TRUE,
        content_length = nchar(cached_content)
      ))
    }
  }
  
  # If not cached, process normally
  tryCatch({
    file_ext <- stringr::str_extract(doc_url, "\\.[^.\\?]+(?=\\?|$)")
    file_ext <- stringr::str_remove(file_ext, "^\\.")
    
    temp_file <- tempfile(fileext = paste0(".", file_ext))
    
    # Download with better headers
    response <- httr::GET(doc_url, 
                          httr::write_disk(temp_file, overwrite = TRUE),
                          httr::user_agent("Mozilla/5.0 (compatible; R-webcrawler)"),
                          httr::timeout(30))
    
    if (httr::status_code(response) != 200) {
      return(list(url = doc_url, found = FALSE, error = "Download failed"))
    }
    
    # Extract text
    text_content <- extract_text_from_file(temp_file, file_ext)
    
    # Cache the content if cache_dir specified
    if (!is.null(cache_dir)) {
      writeLines(text_content, cache_file)
    }
    
    # Search for string
    found <- stringr::str_detect(text_content, stringr::regex(stringr::fixed(search_string), ignore_case = TRUE))
    
    # Clean up
    unlink(temp_file)
    
    return(list(
      url = doc_url,
      found = found,
      file_type = file_ext,
      content_length = nchar(text_content),
      cached = FALSE,
      error = NULL
    ))
    
  }, error = function(e) {
    return(list(url = doc_url, found = FALSE, error = e$message))
  })
}

# Step 4: Enhanced text extraction with more formats
extract_text_from_file <- function(file_path, file_ext) {
  
  text_content <- ""
  
  tryCatch({
    
    if (file_ext %in% c("txt", "csv", "log", "md")) {
      # Plain text files
      text_content <- readLines(file_path, warn = FALSE) |>
        paste(collapse = "\n")
      
    } else if (file_ext == "pdf") {
      # PDF files
      if (requireNamespace("pdftools", quietly = TRUE)) {
        text_content <- pdftools::pdf_text(file_path) |>
          paste(collapse = "\n")
      }
      
    } else if (file_ext %in% c("doc", "docx")) {
      # Word documents
      if (requireNamespace("textreadr", quietly = TRUE)) {
        text_content <- textreadr::read_document(file_path)
      } else if (requireNamespace("officer", quietly = TRUE)) {
        doc <- officer::read_docx(file_path)
        text_content <- officer::docx_summary(doc)$text |>
          paste(collapse = "\n")
      }
      
    } else if (file_ext %in% c("xls", "xlsx")) {
      # Excel files
      if (requireNamespace("readxl", quietly = TRUE)) {
        sheets <- readxl::excel_sheets(file_path)
        all_data <- purrr::map(sheets, ~{
          tryCatch({
            readxl::read_excel(file_path, sheet = .x, col_types = "text")
          }, error = function(e) data.frame())
        })
        text_content <- all_data |>
          purrr::map(~paste(unlist(.x), collapse = " ")) |>
          paste(collapse = "\n")
      }
      
    } else if (file_ext %in% c("ppt", "pptx")) {
      # PowerPoint files
      if (requireNamespace("officer", quietly = TRUE)) {
        ppt <- officer::read_pptx(file_path)
        text_content <- officer::pptx_summary(ppt)$text |>
          paste(collapse = "\n")
      }
      
    } else if (file_ext %in% c("rtf")) {
      # RTF files
      if (requireNamespace("striprtf", quietly = TRUE)) {
        text_content <- striprtf::read_rtf(file_path)
      }
    }
    
  }, error = function(e) {
    warning(paste("Error extracting text from", file_path, ":", e$message))
  })
  
  return(text_content)
}

# Step 5: Main function to search entire domain
search_domain_for_string <- function(base_url, search_string, 
                                     max_pages = 50, max_depth = 2,
                                     doc_extensions = c("pdf", "doc", "docx", "txt", "xls", "xlsx"),
                                     parallel = TRUE, max_workers = 4,
                                     cache_dir = "document_cache",
                                     delay = 1,
                                     save_results = TRUE) {
  
  cat("=== DOMAIN-WIDE DOCUMENT SEARCH ===\n")
  cat(paste("Domain:", base_url, "\n"))
  cat(paste("Search string:", search_string, "\n"))
  cat(paste("Max pages:", max_pages, "\n"))
  cat(paste("Max depth:", max_depth, "\n\n"))
  
  # Step 1: Crawl website for all pages
  start_time <- Sys.time()
  all_pages <- crawl_website(base_url, max_pages, max_depth, delay = delay)
  
  if (length(all_pages) == 0) {
    cat("No pages found. Check if the URL is accessible.\n")
    return(data.frame())
  }
  
  # Step 2: Extract all document links
  all_doc_links <- extract_all_document_links(all_pages, doc_extensions)
  
  if (length(all_doc_links) == 0) {
    cat("No documents found on the website.\n")
    return(data.frame())
  }
  
  # Step 3: Search documents for string
  cat("\nSearching documents for string...\n")
  
  if (parallel && requireNamespace("parallel", quietly = TRUE)) {
    # Parallel processing
    cl <- parallel::makeCluster(min(max_workers, parallel::detectCores() - 1))
    
    # Export required functions and packages
    parallel::clusterExport(cl, c("check_document_for_string_cached", 
                                  "extract_text_from_file", "search_string", "cache_dir"),
                            envir=environment())
    
    parallel::clusterEvalQ(cl, {
      library(httr)
      library(stringr)
    })
    
    results <- parallel::parLapply(cl, all_doc_links, function(url) {
      check_document_for_string_cached(url, search_string, cache_dir)
    })
    
    parallel::stopCluster(cl)
    
  } else {
    # Sequential processing
    results <- purrr::map(all_doc_links, ~{
      if (length(all_doc_links) > 20) cat(".")
      check_document_for_string_cached(.x, search_string, cache_dir)
    })
  }
  
  # Step 4: Process results
  cat("\nProcessing results...\n")
  
  results_df <- purrr::map_dfr(results, function(result) {
    data.frame(
      url = result$url %||% "",
      found = result$found %||% FALSE,
      file_type = result$file_type %||% "unknown",
      content_length = result$content_length %||% 0,
      cached = result$cached %||% FALSE,
      error = result$error %||% NA,
      stringsAsFactors = FALSE
    )
  })
  
  # Add metadata
  results_df$search_string <- search_string
  results_df$search_date <- as.character(Sys.Date())
  results_df$domain <- stringr::str_extract(base_url, "https?://[^/]+")
  
  # Summary statistics
  end_time <- Sys.time()
  total_time <- difftime(end_time, start_time, units = "mins")
  
  found_count <- sum(results_df$found, na.rm = TRUE)
  error_count <- sum(!is.na(results_df$error))
  cached_count <- sum(results_df$cached, na.rm = TRUE)
  
  cat("\n=== SEARCH COMPLETE ===\n")
  cat(paste("Total time:", round(total_time, 2), "minutes\n"))
  cat(paste("Pages crawled:", length(all_pages), "\n"))
  cat(paste("Documents found:", length(all_doc_links), "\n"))
  cat(paste("Documents checked:", nrow(results_df), "\n"))
  cat(paste("Documents containing '", search_string, "':", found_count, "\n"))
  cat(paste("Documents from cache:", cached_count, "\n"))
  cat(paste("Errors encountered:", error_count, "\n"))
  
  # Save results if requested
  if (save_results) {
    filename <- paste0("domain_search_", 
                             gsub("[^A-Za-z0-9]", "_", search_string), "_",
                             format(Sys.Date(), "%Y%m%d"), ".csv")
    utils::write.csv(results_df, filename, row.names = FALSE)
    cat(paste("Results saved to:", filename, "\n"))
  }
  
  return(results_df)
}

# Step 6: Advanced filtering and analysis functions
analyze_search_results <- function(results_df) {
  
  cat("=== SEARCH RESULTS ANALYSIS ===\n\n")
  
  # Basic stats
  found_docs <- results_df[results_df$found == TRUE, ]
  
  # By file type
  if (nrow(found_docs) > 0) {
    cat("Documents found by file type:\n")
    type_summary <- table(found_docs$file_type)
    for (i in seq_along(type_summary)) {
      cat(paste(" ", names(type_summary)[i], ":", type_summary[i], "\n"))
    }
    cat("\n")
    
    # Top documents by content length
    cat("Largest documents containing search string:\n")
    top_docs <- found_docs[order(found_docs$content_length, decreasing = TRUE), ]
    for (i in 1:min(5, nrow(top_docs))) {
      cat(paste(i, ".", basename(top_docs$url[i]), 
                      "(", top_docs$content_length[i], "chars )\n"))
    }
    cat("\n")
  }
  
  # Error analysis
  error_docs <- results_df[!is.na(results_df$error), ]
  if (nrow(error_docs) > 0) {
    cat("Common errors:\n")
    error_summary <- table(error_docs$error)
    for (i in seq_along(error_summary)) {
      cat(paste(" ", names(error_summary)[i], ":", error_summary[i], "\n"))
    }
  }
}

# Step 7: Export functions
export_found_documents <- function(results_df, format = "csv") {
  
  found_docs <- results_df[results_df$found == TRUE, ]
  
  if (nrow(found_docs) == 0) {
    cat("No documents found to export.\n")
    return()
  }
  
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  
  if (format == "csv") {
    filename <- paste0("found_documents_", timestamp, ".csv")
    utils::write.csv(found_docs, filename, row.names = FALSE)
  } else if (format == "txt") {
    filename <- paste0("found_documents_", timestamp, ".txt")
    writeLines(found_docs$url, filename)
  }
  
  cat(paste("Exported", nrow(found_docs), "document URLs to", filename, "\n"))
}

# Example usage and setup
setup_domain_search <- function() {
  
  required_packages <- c("rvest", "httr", "stringr", "purrr", "dplyr")
  optional_packages <- c("pdftools", "textreadr", "readxl", "officer", "parallel", "digest")
  
  cat("=== PACKAGE CHECK ===\n")
  
  # Check required packages
  missing_required <- character(0)
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      missing_required <- c(missing_required, pkg)
    }
  }
  
  if (length(missing_required) > 0) {
    cat("Installing required packages...\n")
    utils::install.packages(missing_required)
  }
  
  # Report optional packages
  cat("\nOptional packages for file format support:\n")
  for (pkg in optional_packages) {
    status <- if (requireNamespace(pkg, quietly = TRUE)) "✓" else "✗"
    cat(paste(" ", status, pkg, "\n"))
  }
  
  cat("\n=== USAGE EXAMPLE ===\n")
  cat('results <- search_domain_for_string(\n')
  cat('  base_url = "https://example-university.edu",\n')
  cat('  search_string = "sustainability",\n')
  cat('  max_pages = 50,\n')
  cat('  max_depth = 2\n')
  cat(')\n\n')
  cat('analyze_search_results(results)\n')
  cat('export_found_documents(results)\n')
}

cat("Domain-wide document search functions loaded!\n")
cat("Run setup_domain_search() to check packages and see usage examples\n")

# Usage example
# Setup (install required packages)
setup_domain_search()

# Search entire domain
results <- search_domain_for_string(
  base_url = "https://www.mafmc.org/",
  search_string = "climate change",
  max_pages = 100,        # Maximum pages to crawl; there are 239 in depth 2
  max_depth = 3,          # How deep to crawl; likely needs to be higher
  doc_extensions = c("pdf", "doc", "docx", "txt"),
  parallel = FALSE,        # Use parallel processing--Don't, too many download errors
  cache_dir = "doc_cache", # Cache downloaded content
  delay = 1               # Seconds between requests
)

# Analyze results
analyze_search_results(results)

# Export found documents
export_found_documents(results, format = "csv")
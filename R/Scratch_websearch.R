# Claude's initial attempt at web page search for a string, needs work
# Claude's mistakes
#   * Don't use %>% without loading magrittr, I replaced with native pipe |>
#   * Uses %||% symbol from package infix without loading it

# R Code to Find Documents on Web Page Containing Specific String

# Required packages
library(rvest)
library(httr)
library(stringr)
library(purrr)
library(dplyr)
library(infix)

# Step 1: Extract all document links from webpage
extract_document_links <- function(url, doc_extensions = c("pdf", "doc", "docx", "txt", "xls", "xlsx", "ppt", "pptx")) {
  
  # Read the webpage
  page <- rvest::read_html(url)
  
  # Extract all href attributes
  all_links <- page |>
    rvest::html_nodes("a") |>
    rvest::html_attr("href")
  
  # Remove NA values
  all_links <- all_links[!is.na(all_links)]
  
  # Convert relative URLs to absolute
  all_links <- xml2::url_absolute(all_links, url)
  
  # Filter for document extensions
  doc_pattern <- base::paste0("\\.(", base::paste(doc_extensions, collapse = "|"), ")($|\\?)", collapse = "")
  doc_links <- all_links[stringr::str_detect(all_links, stringr::regex(doc_pattern, ignore_case = TRUE))]
  
  return(doc_links)
}

# Step 2: Download and check document content
check_document_for_string <- function(doc_url, search_string, temp_dir = tempdir()) {
  
  tryCatch({
    # Determine file extension
    file_ext <- stringr::str_extract(doc_url, "\\.[^.\\?]+(?=\\?|$)")
    file_ext <- stringr::str_remove(file_ext, "^\\.")
    
    # Create temporary file path
    temp_file <- base::file.path(temp_dir, base::paste0("temp_doc.", file_ext))
    
    # Download the document
    response <- httr::GET(doc_url, httr::write_disk(temp_file, overwrite = TRUE))
    
    if (httr::status_code(response) != 200) {
      return(list(url = doc_url, found = FALSE, error = "Download failed"))
    }
    
    # Extract text based on file type
    text_content <- extract_text_from_file(temp_file, file_ext)
    
    # Check if string exists in content
    found <- stringr::str_detect(text_content, stringr::fixed(search_string), ignore.case = TRUE)
    
    # Clean up temp file
    base::unlink(temp_file)
    
    return(list(
      url = doc_url,
      found = found,
      file_type = file_ext,
      content_length = base::nchar(text_content),
      error = NULL
    ))
    
  }, error = function(e) {
    return(list(url = doc_url, found = FALSE, error = e$message))
  })
}

# Step 3: Extract text from different file types
extract_text_from_file <- function(file_path, file_ext) {
  
  text_content <- ""
  
  if (file_ext %in% c("txt", "csv", "log")) {
    # Plain text files
    text_content <- base::readLines(file_path, warn = FALSE) |>
      base::paste(collapse = "\n")
    
  } else if (file_ext == "pdf") {
    # PDF files (requires pdftools package)
    if (base::requireNamespace("pdftools", quietly = TRUE)) {
      text_content <- pdftools::pdf_text(file_path) |>
        base::paste(collapse = "\n")
    } else {
      warning("pdftools package required for PDF files")
    }
    
  } else if (file_ext %in% c("doc", "docx")) {
    # Word documents (requires textreadr package)
    if (base::requireNamespace("textreadr", quietly = TRUE)) {
      text_content <- textreadr::read_document(file_path)
    } else if (base::requireNamespace("officer", quietly = TRUE)) {
      # Alternative using officer package
      doc <- officer::read_docx(file_path)
      text_content <- officer::docx_summary(doc)$text |>
        base::paste(collapse = "\n")
    } else {
      warning("textreadr or officer package required for Word documents")
    }
    
  } else if (file_ext %in% c("xls", "xlsx")) {
    # Excel files (requires readxl package)
    if (base::requireNamespace("readxl", quietly = TRUE)) {
      # Read all sheets and combine
      sheets <- readxl::excel_sheets(file_path)
      all_data <- purrr::map(sheets, ~readxl::read_excel(file_path, sheet = .x))
      text_content <- all_data |>
        purrr::map(~base::paste(base::unlist(.x), collapse = " ")) |>
        base::paste(collapse = "\n")
    } else {
      warning("readxl package required for Excel files")
    }
    
  } else if (file_ext %in% c("ppt", "pptx")) {
    # PowerPoint files (requires officer package)
    if (base::requireNamespace("officer", quietly = TRUE)) {
      ppt <- officer::read_pptx(file_path)
      text_content <- officer::pptx_summary(ppt)$text |>
        base::paste(collapse = "\n")
    } else {
      warning("officer package required for PowerPoint files")
    }
  }
  
  return(text_content)
}

# Step 4: Main function to find all documents containing string
find_documents_with_string <- function(webpage_url, search_string,
                                       doc_extensions = c("pdf", "doc", "docx", "txt", "xls", "xlsx"),
                                       parallel = FALSE, max_workers = 4) {
  
  cat("Step 1: Extracting document links from webpage...\n")
  doc_links <- extract_document_links(webpage_url, doc_extensions)
  
  if (base::length(doc_links) == 0) {
    cat("No document links found on the webpage.\n")
    return(data.frame())
  }
  
  cat(base::paste("Found", base::length(doc_links), "document links\n"))
  
  cat("Step 2: Checking documents for search string...\n")
  
  if (parallel && base::requireNamespace("parallel", quietly = TRUE)) {
    # Parallel processing
    cl <- parallel::makeCluster(base::min(max_workers, parallel::detectCores() - 1))
    parallel::clusterEvalQ(cl, {
      library(httr)
      library(stringr)
    })
    
    results <- parallel::parLapply(cl, doc_links, check_document_for_string,
                                   search_string = search_string)
    parallel::stopCluster(cl)
  } else {
    # Sequential processing with progress
    results <- purrr::map(doc_links, ~{
      cat(".")
      check_document_for_string(.x, search_string)
    })
  }
  
  cat("\nStep 3: Compiling results...\n")
  
  # Convert results to data frame
  results_df <- purrr::map_dfr(results, ~{
    data.frame(
      url = .x$url,
      found = .x$found %||% FALSE,
      file_type = .x$file_type %||% "unknown",
      content_length = .x$content_length %||% 0,
      error = .x$error %||% NA,
      stringsAsFactors = FALSE
    )
  })
  
  # Summary
  found_count <- base::sum(results_df$found, na.rm = TRUE)
  error_count <- base::sum(!is.na(results_df$error))
  
  cat(base::paste("Search complete!\n"))
  cat(base::paste("- Total documents checked:", base::nrow(results_df), "\n"))
  cat(base::paste("- Documents containing '", search_string, "':", found_count, "\n"))
  cat(base::paste("- Documents with errors:", error_count, "\n"))
  
  return(results_df)
}

# Step 5: Helper function to display results
display_results <- function(results_df, show_errors = FALSE) {
  
  # Documents containing the string
  found_docs <- results_df[results_df$found == TRUE, ]
  if (base::nrow(found_docs) > 0) {
    cat("\n=== DOCUMENTS CONTAINING SEARCH STRING ===\n")
    for (i in 1:base::nrow(found_docs)) {
      cat(base::paste(i, ".", found_docs$url[i],
                      "(", found_docs$file_type[i], ")\n"))
    }
  }
  
  # Show errors if requested
  if (show_errors) {
    error_docs <- results_df[!is.na(results_df$error), ]
    if (base::nrow(error_docs) > 0) {
      cat("\n=== DOCUMENTS WITH ERRORS ===\n")
      for (i in 1:base::nrow(error_docs)) {
        cat(base::paste(i, ".", error_docs$url[i], "-", error_docs$error[i], "\n"))
      }
    }
  }
}

# Step 6: Advanced search with multiple strings
find_documents_multiple_strings <- function(webpage_url, search_strings,
                                            match_all = FALSE, doc_extensions = c("pdf", "doc", "docx", "txt")) {
  
  doc_links <- extract_document_links(webpage_url, doc_extensions)
  
  results <- purrr::map_dfr(doc_links, function(doc_url) {
    
    # Check each document for all search strings
    matches <- purrr::map_lgl(search_strings, function(search_string) {
      result <- check_document_for_string(doc_url, search_string)
      return(result$found %||% FALSE)
    })
    
    # Determine if document matches criteria
    if (match_all) {
      overall_match <- base::all(matches)
    } else {
      overall_match <- base::any(matches)
    }
    
    data.frame(
      url = doc_url,
      overall_match = overall_match,
      base::setNames(matches, base::paste0("contains_", search_strings)),
      stringsAsFactors = FALSE
    )
  })
  
  return(results)
}

# Example usage function
example_usage <- function() {
  cat("=== EXAMPLE USAGE ===\n\n")
  cat("# Basic usage:\n")
  cat('results <- find_documents_with_string("https://example.com", "climate change")\n')
  cat('display_results(results)\n\n')
  
  cat("# With specific file types:\n")
  cat('results <- find_documents_with_string("https://example.com", "budget", \n')
  cat('                                     doc_extensions = c("pdf", "xlsx"))\n\n')
  
  cat("# Multiple search strings:\n")
  cat('results <- find_documents_multiple_strings("https://example.com", \n')
  cat('                                          c("sustainability", "environment"))\n\n')
  
  cat("# Required packages for full functionality:\n")
  cat("# install.packages(c('pdftools', 'textreadr', 'readxl', 'officer'))\n")
}

# Load required packages check
check_required_packages <- function() {
  required <- c("rvest", "httr", "stringr", "purrr", "dplyr")
  optional <- c("pdftools", "textreadr", "readxl", "officer", "parallel")
  
  cat("Required packages:\n")
  for (pkg in required) {
    status <- if (base::requireNamespace(pkg, quietly = TRUE)) "✓" else "✗"
    cat(base::paste(" ", status, pkg, "\n"))
  }
  
  cat("\nOptional packages (for full file support):\n")
  for (pkg in optional) {
    status <- if (base::requireNamespace(pkg, quietly = TRUE)) "✓" else "✗"
    cat(base::paste(" ", status, pkg, "\n"))
  }
}

# cat("Document search functions loaded!\n")
# cat("Run check_required_packages() to see package status\n")
# cat("Run example_usage() to see usage examples\n")

# Search for documents containing "climate change"
results <- find_documents_with_string(
  webpage_url = "https://www.mafmc.org/",
  search_string = "fisheries",
  doc_extensions = c("pdf", "doc", "docx")
)

# Display results
display_results(results)



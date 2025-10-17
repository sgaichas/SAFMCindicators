# Claude generated code from prompt:
# please write a code snippet for summarizing a pdf that uses prompt caching and also avoids the error HTTP 429 Too Many Requests.
# Can we add one line to the summary that captures the prompt?
# add library::function notation to the script to clarify which libraries are being called


# Install required packages
# install.packages(c("httr2", "jsonlite", "pdftools", "base64enc"))

library(httr2)
library(jsonlite)
library(pdftools)

# Set your API key
api_key <- Sys.getenv("ANTHROPIC_API_KEY")

# Extract text from PDF
extract_pdf_text <- function(pdf_path) {
  text <- pdftools::pdf_text(pdf_path)
  combined_text <- paste(text, collapse = "\n")
  return(combined_text)
}

# Enhanced Claude API call with rate limiting and retry logic
call_claude_with_retry <- function(prompt, api_key, 
                                   model = "claude-sonnet-4-5-20250929",
                                   use_cache = FALSE, 
                                   system_prompt = NULL,
                                   max_retries = 5,
                                   initial_delay = 1) {
  
  url <- "https://api.anthropic.com/v1/messages"
  
  # Prepare request body with optional caching
  if (use_cache && !is.null(system_prompt)) {
    body <- list(
      model = model,
      max_tokens = 4096,
      system = list(
        list(
          type = "text",
          text = system_prompt,
          cache_control = list(type = "ephemeral")
        )
      ),
      messages = list(
        list(
          role = "user",
          content = list(
            list(
              type = "text",
              text = prompt,
              cache_control = list(type = "ephemeral")
            )
          )
        )
      )
    )
  } else {
    body <- list(
      model = model,
      max_tokens = 4096,
      messages = list(
        list(
          role = "user",
          content = prompt
        )
      )
    )
    if (!is.null(system_prompt)) {
      body$system <- system_prompt
    }
  }
  
  # Retry logic with exponential backoff
  for (attempt in 1:max_retries) {
    tryCatch({
      response <- httr2::request(url) %>%
        httr2::req_headers(
          "Content-Type" = "application/json",
          "x-api-key" = api_key,
          "anthropic-version" = "2023-06-01"
        ) %>%
        httr2::req_body_json(body) %>%
        httr2::req_retry(
          max_tries = 3,
          is_transient = function(resp) {
            # Retry on rate limit (429) and server errors (5xx)
            status <- httr2::resp_status(resp)
            status == 429 || (status >= 500 && status < 600)
          },
          backoff = ~ 2^.x  # Exponential backoff: 2, 4, 8 seconds
        ) %>%
        httr2::req_perform()
      
      # Parse successful response
      response_data <- httr2::resp_body_json(response)
      
      # Display cache performance metrics
      if (!is.null(response_data$usage)) {
        usage <- response_data$usage
        cat("\n--- API Usage Stats ---\n")
        cat("Input tokens:", usage$input_tokens %||% 0, "\n")
        cat("Output tokens:", usage$output_tokens %||% 0, "\n")
        
        if (!is.null(usage$cache_creation_input_tokens) || 
            !is.null(usage$cache_read_input_tokens)) {
          cat("Cache creation tokens:", usage$cache_creation_input_tokens %||% 0, "\n")
          cat("Cache read tokens:", usage$cache_read_input_tokens %||% 0, "\n")
          
          # Calculate savings
          if (!is.null(usage$cache_read_input_tokens) && usage$cache_read_input_tokens > 0) {
            savings_pct <- round((1 - 0.1) * 100, 1)
            cat(sprintf("Cache savings: ~%s%% on cached tokens!\n", savings_pct))
          }
        }
        cat("----------------------\n\n")
      }
      
      # Extract and return summary
      summary <- response_data$content[[1]]$text
      return(summary)
      
    }, error = function(e) {
      # Check if it's a rate limit error
      if (grepl("429", e$message, ignore.case = TRUE)) {
        if (attempt < max_retries) {
          delay <- initial_delay * (2 ^ (attempt - 1))  # Exponential backoff
          cat(sprintf("Rate limit hit (429). Waiting %d seconds before retry %d/%d...\n", 
                      delay, attempt, max_retries))
          Sys.sleep(delay)
        } else {
          stop("Max retries reached. Rate limit error persists: ", e$message)
        }
      } else {
        # For other errors, stop immediately
        stop("API error: ", e$message)
      }
    })
  }
  
  stop("Failed to get response after all retries")
}

# Main function to summarize PDF with rate limiting
summarize_pdf_safe <- function(pdf_path, 
                               api_key, 
                               summary_type = "general",
                               use_cache = TRUE,
                               delay_before_request = 0,
                               folder = getwd()) {
  
  # Optional delay before making request (useful for batch processing)
  if (delay_before_request > 0) {
    cat(sprintf("Waiting %d seconds before processing...\n", delay_before_request))
    Sys.sleep(delay_before_request)
  }
  
  # Extract PDF text
  cat("Extracting text from PDF:", basename(pdf_path), "\n")
  pdf_text <- extract_pdf_text(pdf_path)
  
  if (nchar(pdf_text) < 10) {
    stop("Could not extract meaningful text from PDF")
  }
  
  # System prompts for different summary types
  system_prompts <- list(
    ESR = "Please summarize this pdf ecosystem status report in 500 words. After the summary, make a list of the section headings in the report and the ecosystem indicators used in each section. Briefly describe the implications of each ecosystem indicator for fishery management:",
    FEP = "Please summarize this pdf fishery ecosystem plan. Highlight any stated policies, goals, and objectives in the document along with any management approaches and performance metrics. List specific ecosystem indicators identified and how they are aligned with objectives:",
    general = "You are an expert document analyst. Provide comprehensive summaries that capture the main points, key findings, and conclusions. Structure your summaries clearly and highlight the most important information.",
    executive = "You are a business analyst. Create executive summaries focusing on key takeaways, strategic recommendations, and actionable insights. Be concise but comprehensive.",
    academic = "You are an academic researcher. Provide scholarly summaries including methodology, findings, limitations, and theoretical contributions. Maintain academic rigor.",
    bullet_points = "You are a professional summarizer. Create well-organized bullet point summaries capturing all key information in an easy-to-scan format.",
    detailed = "You are a thorough analyst. Provide a detailed summary that covers all major sections, arguments, data points, and conclusions. Include important context and nuance."
  )
  
  system_prompt <- system_prompts[[summary_type]]
  
  # Create document content
  document_prompt <- paste(
    "Please analyze and summarize the following document:\n\n",
    pdf_text
  )
  
  # Check if document is large enough for caching (1024 tokens minimum)
  estimated_tokens <- nchar(document_prompt) / 4
  if (estimated_tokens < 1024) {
    cat("Document too short for caching (< 1024 tokens). Using standard mode.\n")
    use_cache <- FALSE
  } else {
    cat(sprintf("Document size: ~%d tokens. Caching %s.\n", 
                round(estimated_tokens), 
                if(use_cache) "enabled" else "disabled"))
  }
  
  # Call API with retry logic
  cat("Sending request to Claude...\n")
  summary <- call_claude_with_retry(
    prompt = document_prompt,
    api_key = api_key,
    use_cache = use_cache,
    system_prompt = system_prompt
  )
  
  namesum <- basename(pdf_path)
  readr::write_file(summary, here::here(paste0(folder, "/", namesum, ".Rmd")))
  
  return(summary)
}

# Batch process multiple PDFs with intelligent rate limiting
batch_summarize_pdfs_safe <- function(pdf_paths, 
                                      api_key, 
                                      summary_type = "general",
                                      requests_per_minute = 50,
                                      use_cache = TRUE,
                                      folder = getwd()) {
  
  # Calculate delay between requests to stay under rate limit
  delay_seconds <- 60 / requests_per_minute
  
  cat(sprintf("\n=== Batch Processing %d PDFs ===\n", length(pdf_paths)))
  cat(sprintf("Rate limit: %d requests/minute (%.1f second delay between requests)\n", 
              requests_per_minute, delay_seconds))
  cat(sprintf("Caching: %s\n", if(use_cache) "enabled" else "disabled"))
  cat("===============================\n\n")
  
  summaries <- list()
  start_time <- Sys.time()
  
  for (i in seq_along(pdf_paths)) {
    pdf_path <- pdf_paths[i]
    
    cat(sprintf("\n[%d/%d] Processing: %s\n", i, length(pdf_paths), basename(pdf_path)))
    cat(rep("-", 50), "\n", sep = "")
    
    tryCatch({
      # For first request, no delay. For subsequent requests, add delay
      delay <- if(i == 1) 0 else delay_seconds
      
      summary <- summarize_pdf_safe(
        pdf_path = pdf_path,
        api_key = api_key,
        summary_type = summary_type,
        use_cache = use_cache,
        delay_before_request = delay
      )
      
      namesum <- basename(pdf_path)
      readr::write_file(summary, here::here(paste0(folder, "/", namesum, ".Rmd")))
      
      summaries[[basename(pdf_path)]] <- list(
        status = "success",
        summary = summary,
        prompt_used = system_prompt,
        summary_type = summary_type,
        processed_at = Sys.time()
      )
      
      cat("✓ Successfully processed\n")
      
    }, error = function(e) {
      cat("✗ Error:", e$message, "\n")
      summaries[[basename(pdf_path)]] <- list(
        status = "error",
        error_message = e$message,
        processed_at = Sys.time()
      )
    })
  }
  
  # Summary statistics
  end_time <- Sys.time()
  total_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  success_count <- sum(sapply(summaries, function(x) x$status == "success"))
  
  cat("\n\n=== Batch Processing Complete ===\n")
  cat(sprintf("Total time: %.1f seconds\n", total_time))
  cat(sprintf("Successful: %d/%d\n", success_count, length(pdf_paths)))
  cat(sprintf("Failed: %d/%d\n", length(pdf_paths) - success_count, length(pdf_paths)))
  cat("=================================\n")
  
  return(summaries)
}

# =============================================================================
# USAGE EXAMPLES
# =============================================================================

# Set your API key (recommended: use environment variable)
# Sys.setenv(ANTHROPIC_API_KEY = "your-api-key-here")

# Example 1: Summarize a single PDF with caching
# summary <- summarize_pdf_safe(
#   pdf_path = "research_paper.pdf",
#   api_key = api_key,
#   summary_type = "academic",
#   use_cache = TRUE
# )
# cat(summary)

# Example 2: Batch process multiple PDFs (recommended for caching benefits)
# pdf_files <- c(
#   "report1.pdf",
#   "report2.pdf", 
#   "report3.pdf",
#   "report4.pdf"
# )
# 
# results <- batch_summarize_pdfs_safe(
#   pdf_paths = pdf_files,
#   api_key = api_key,
#   summary_type = "executive",
#   requests_per_minute = 50,  # Adjust based on your rate limit tier
#   use_cache = TRUE
# )
# 
# # Access individual summaries
# print(results[["report1.pdf"]]$summary)

# Example 3: Conservative rate limiting for free tier
# results <- batch_summarize_pdfs_safe(
#   pdf_paths = pdf_files,
#   api_key = api_key,
#   summary_type = "general",
#   requests_per_minute = 10,  # Very conservative for free tier
#   use_cache = TRUE
# )

# Example 4: Process without caching (for diverse documents)
# summary <- summarize_pdf_safe(
#   pdf_path = "unique_document.pdf",
#   api_key = api_key,
#   summary_type = "detailed",
#   use_cache = FALSE
# )

# =============================================================================
# RATE LIMIT TIERS (as of 2025)
# =============================================================================
# Free tier: 5 requests/minute
# Build Tier 1: 50 requests/minute
# Build Tier 2: 1,000 requests/minute
# Build Tier 3: 2,000 requests/minute
# Build Tier 4: 4,000 requests/minute
# 
# Adjust requests_per_minute parameter based on your tier
# =============================================================================
#
# Build Tier 1
# results <- batch_summarize_pdfs_safe(
#   pdf_paths = pdf_files,
#   api_key = api_key,
#   requests_per_minute = 45,  # Conservative under 50/min
#   use_cache = TRUE
# )
# # Get the summary
# print(results[["report1.pdf"]]$summary)
# 
# # Get the prompt that was used
# print(results[["report1.pdf"]]$prompt_used)
# 
# # Get the summary type
# print(results[["report1.pdf"]]$summary_type)
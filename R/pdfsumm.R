# pdf summary
# may include AI
# based on Scratch_pdfsumm.R and https://damianoswald.com/blog/pdf-chat-gpt/


extract_pdf_text() <- function(path, numbering = TRUE, references = TRUE){
  text <- pdftools::pdf_text(path) %>%
    paste0(collapse = " ") %>%
    paste0(collapse = " ") %>%
    stringr::str_squish()
  if(numbering) {
    target <- "\\[.*?\\]"
    text <- gsub(target, "", text)
  }
  if(references) {
    target <- "\\bReferences|references|REFERENCES\\b"
    text <- strsplit(text, target) %>%
      unlist %>%
      head(., -1) %>%
      paste(., collapse = " ")
  }
  return(text)
}

# Function to get basic PDF metadata
get_pdf_info <- function(pdf_path) {
  tryCatch({
    info <- pdftools::pdf_info(pdf_path)
    return(list(
      pages = info$pages,
      creation_date = info$created %||% NA,
      modification_date = info$modified %||% NA,
      author = info$keys$Author %||% NA,
      title = info$keys$Title %||% NA,
      subject = info$keys$Subject %||% NA
    ))
  }, error = function(e) {
    return(list(
      pages = NA,
      creation_date = NA,
      modification_date = NA,
      author = NA,
      title = NA,
      subject = NA
    ))
  })
}

# Function to call Claude API
call_claude <- function(prompt, api_key, model = "claude-3-5-sonnet-20241022") {
  
  url <- "https://api.anthropic.com/v1/messages"
  
  # Prepare the request body
  body <- list(
    model = model,
    max_tokens = 4000,  # Adjust based on desired summary length
    messages = list(
      list(
        role = "user",
        content = prompt
      )
    )
  )
  
  # Make the API request
  response <- request(url) %>%
    req_headers(
      "Content-Type" = "application/json",
      "x-api-key" = api_key,
      "anthropic-version" = "2023-06-01"
    ) %>%
    req_body_json(body) %>%
    req_perform()
  
  # Parse response
  response_data <- resp_body_json(response)
  
  # Extract the text content
  summary <- response_data$content[[1]]$text
  
  return(summary)
}

# Function to summarize PDF
summarize_pdf <- function(pdf_path, api_key, summary_type = "general") {
  
  # Extract text from PDF
  cat("Extracting text from PDF...\n")
  pdf_text <- extract_pdf_text(pdf_path)
  
  # Check if text extraction was successful
  if (nchar(pdf_text) < 10) {
    stop("Could not extract meaningful text from PDF")
  }
  
  # Create prompt based on summary type
  prompts <- list(
    general = "Please provide a comprehensive summary of the following document. Include the main points, key findings, and conclusions:",
    executive = "Please provide an executive summary of the following document, focusing on key takeaways, recommendations, and actionable insights:",
    academic = "Please provide an academic summary of the following document, including methodology, findings, limitations, and theoretical contributions:",
    bullet_points = "Please summarize the following document as a bulleted list of key points:"
  )
  
  prompt <- paste(prompts[[summary_type]], "\n\n", pdf_text)
  
  # Check token length (rough estimate: 1 token ≈ 4 characters)
  estimated_tokens <- nchar(prompt) / 4
  if (estimated_tokens > 180000) {  # Leave room for response
    warning("Document may be too long. Consider splitting into chunks.")
  }
  
  cat("Sending request to Claude...\n")
  summary <- call_claude(prompt, api_key)
  
  return(summary)
}

# Example usage:
# Make sure to set your API key first:
# Sys.setenv(ANTHROPIC_API_KEY = "your-api-key-here")

# Summarize a PDF
# summary <- summarize_pdf("path/to/your/document.pdf", api_key)
# cat(summary)

# Different summary types:
# summary <- summarize_pdf("document.pdf", api_key, "executive")
# summary <- summarize_pdf("document.pdf", api_key, "academic")
# summary <- summarize_pdf("document.pdf", api_key, "bullet_points")

# Function to handle large documents by chunking
summarize_large_pdf <- function(pdf_path, api_key, chunk_size = 100000) {
  
  pdf_text <- extract_pdf_text(pdf_path)
  
  # Split into chunks if too large
  if (nchar(pdf_text) > chunk_size) {
    cat("Document is large, processing in chunks...\n")
    
    # Simple chunking by character count
    chunks <- seq(1, nchar(pdf_text), by = chunk_size)
    summaries <- c()
    
    for (i in 1:length(chunks)) {
      start_pos <- chunks[i]
      end_pos <- min(chunks[i] + chunk_size - 1, nchar(pdf_text))
      chunk_text <- substr(pdf_text, start_pos, end_pos)
      
      prompt <- paste("Please summarize this section of a document:", chunk_text)
      chunk_summary <- call_claude(prompt, api_key)
      summaries <- c(summaries, chunk_summary)
      
      cat(sprintf("Processed chunk %d of %d\n", i, length(chunks)))
    }
    
    # Combine chunk summaries
    combined_summaries <- paste(summaries, collapse = "\n\n")
    final_prompt <- paste("Please create a final comprehensive summary from these section summaries:", combined_summaries)
    final_summary <- call_claude(final_prompt, api_key)
    
    return(final_summary)
  } else {
    return(summarize_pdf(pdf_path, api_key))
  }
}
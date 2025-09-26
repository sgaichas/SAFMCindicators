# Initial code from Claude
# prompt "R code that summarizes pdf documents in a directory"

# breaks in so many ways
# not returning words or sentences
# readability score is made up: ignore
# clean up with concepts here
# Oswald, D. Building a PDF summary system based on ChatGPT . damianoswald.com/blog/pdf-chat-gpt (2023).

# R Code to Summarize PDF Documents in a Directory

# Required packages
library(pdftools)
library(stringr)
library(dplyr)
library(tm)
library(SnowballC)
library(wordcloud)

# Function to extract text from a single PDF
extract_pdf_text <- function(pdf_path) {
  tryCatch({
    text <- pdftools::pdf_text(pdf_path)
    # Combine all pages into one string
    full_text <- base::paste(text, collapse = "\n")
    return(full_text)
  }, error = function(e) {
    warning(base::paste("Error reading", pdf_path, ":", e$message))
    return("")
  })
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

# Function to clean and preprocess text
clean_text <- function(text) {
  # Convert to lowercase
  text <- base::tolower(text)
  
  # Remove extra whitespace and line breaks
  text <- stringr::str_replace_all(text, "\\s+", " ")
  
  # Remove special characters but keep periods for sentences
  text <- stringr::str_replace_all(text, "[^a-zA-Z0-9\\s\\.]", " ")
  
  # Remove numbers (optional)
  text <- stringr::str_replace_all(text, "\\b\\d+\\b", "")
  
  # Trim whitespace
  text <- stringr::str_trim(text)
  
  return(text)
}

# Function to extract key sentences (simple extractive summarization)
extract_key_sentences <- function(text, num_sentences = 5) {
  
  # Split into sentences
  sentences <- stringr::str_split(text, "\\.")[[1]]
  sentences <- stringr::str_trim(sentences)
  sentences <- sentences[stringr::str_length(sentences) > 20]  # Filter very short sentences
  
  if (base::length(sentences) <= num_sentences) {
    return(sentences)
  }
  
  # Simple scoring: sentences with more common words get higher scores
  sentence_scores <- numeric(base::length(sentences))
  
  # Create a simple word frequency table
  all_words <- stringr::str_extract_all(base::tolower(text), "\\b\\w+\\b")[[1]]
  word_freq <- base::table(all_words)
  
  # Score each sentence based on word frequencies
  for (i in base::seq_along(sentences)) {
    words_in_sentence <- stringr::str_extract_all(base::tolower(sentences[i]), "\\b\\w+\\b")[[1]]
    sentence_scores[i] <- base::sum(word_freq[words_in_sentence], na.rm = TRUE) / base::length(words_in_sentence)
  }
  
  # Get top sentences
  top_indices <- base::order(sentence_scores, decreasing = TRUE)[1:num_sentences]
  top_sentences <- sentences[base::sort(top_indices)]
  
  return(top_sentences)
}

# Function to extract top keywords
extract_keywords <- function(text, num_keywords = 10) {
  
  # Clean text
  clean <- clean_text(text)
  
  # Create a corpus
  corpus <- tm::Corpus(tm::VectorSource(clean))
  
  # Text preprocessing
  corpus <- tm::tm_map(corpus, tm::content_transformer(base::tolower))
  corpus <- tm::tm_map(corpus, tm::removePunctuation)
  corpus <- tm::tm_map(corpus, tm::removeNumbers)
  corpus <- tm::tm_map(corpus, tm::removeWords, tm::stopwords("english"))
  corpus <- tm::tm_map(corpus, tm::stripWhitespace)
  
  # Stem words (optional)
  if (base::requireNamespace("SnowballC", quietly = TRUE)) {
    corpus <- tm::tm_map(corpus, tm::stemDocument)
  }
  
  # Create document-term matrix
  dtm <- tm::DocumentTermMatrix(corpus)
  
  # Get word frequencies
  word_freq <- base::sort(base::colSums(base::as.matrix(dtm)), decreasing = TRUE)
  
  # Return top keywords
  top_keywords <- base::names(word_freq)[1:base::min(num_keywords, base::length(word_freq))]
  keyword_counts <- word_freq[top_keywords]
  
  return(data.frame(
    keyword = top_keywords,
    frequency = base::as.numeric(keyword_counts),
    stringsAsFactors = FALSE
  ))
}

# Function to generate basic statistics about the text
get_text_stats <- function(text) {
  
  # Basic counts
  char_count <- base::nchar(text)
  word_count <- base::length(stringr::str_extract_all(text, "\\b\\w+\\b")[[1]])
  sentence_count <- base::length(stringr::str_split(text, "[.!?]+")[[1]])
  paragraph_count <- base::length(stringr::str_split(text, "\\n\\s*\\n")[[1]])
  
  # Average words per sentence
  avg_words_per_sentence <- base::round(word_count / base::max(sentence_count, 1), 2)
  
  # Readability (simple measure based on average sentence length)
  readability_score <- base::round(base::max(0, 100 - (avg_words_per_sentence * 2)), 1)
  
  return(list(
    characters = char_count,
    words = word_count,
    sentences = sentence_count,
    paragraphs = paragraph_count,
    avg_words_per_sentence = avg_words_per_sentence,
    readability_score = readability_score
  ))
}

# Main function to summarize a single PDF
summarize_single_pdf <- function(pdf_path, summary_length = 5, keyword_count = 10) {
  
  cat(base::paste("Processing:", base::basename(pdf_path), "\n"))
  
  # Extract text
  text <- extract_pdf_text(pdf_path)
  
  if (base::nchar(text) < 100) {
    return(list(
      filename = base::basename(pdf_path),
      error = "Could not extract sufficient text from PDF"
    ))
  }
  
  # Get PDF metadata
  pdf_info <- get_pdf_info(pdf_path)
  
  # Get text statistics
  text_stats <- get_text_stats(text)
  
  # Extract key sentences
  key_sentences <- extract_key_sentences(text, summary_length)
  
  # Extract keywords
  keywords <- extract_keywords(text, keyword_count)
  
  # Create summary
  summary <- list(
    filename = base::basename(pdf_path),
    file_path = pdf_path,
    
    # Metadata
    pages = pdf_info$pages,
    creation_date = pdf_info$creation_date,
    author = pdf_info$author,
    title = pdf_info$title,
    subject = pdf_info$subject,
    
    # Text statistics
    characters = text_stats$characters,
    words = text_stats$words,
    sentences = text_stats$sentences,
    paragraphs = text_stats$paragraphs,
    avg_words_per_sentence = text_stats$avg_words_per_sentence,
    readability_score = text_stats$readability_score,
    
    # Summary content
    key_sentences = key_sentences,
    top_keywords = keywords,
    
    # Full text (optional, can be large)
    full_text = if (base::nchar(text) < 50000) text else base::substr(text, 1, 50000)
  )
  
  return(summary)
}

# Main function to summarize all PDFs in a directory
summarize_pdf_directory <- function(directory_path, 
                                    pattern = "\\.pdf$",
                                    recursive = TRUE,
                                    summary_length = 5,
                                    keyword_count = 10,
                                    parallel = FALSE,
                                    save_results = TRUE) {
  
  # Find all PDF files
  pdf_files <- base::list.files(directory_path, 
                                pattern = pattern,
                                ignore.case = TRUE,
                                full.names = TRUE,
                                recursive = recursive)
  
  if (base::length(pdf_files) == 0) {
    cat("No PDF files found in the specified directory.\n")
    return(NULL)
  }
  
  cat(base::paste("Found", base::length(pdf_files), "PDF files\n"))
  cat("Starting summarization...\n\n")
  
  start_time <- Sys.time()
  
  # Process PDFs
  if (parallel && base::requireNamespace("parallel", quietly = TRUE)) {
    
    # Parallel processing
    num_cores <- base::min(4, parallel::detectCores() - 1)
    cl <- parallel::makeCluster(num_cores)
    
    # Export required functions and libraries
    parallel::clusterExport(cl, c("summarize_single_pdf", "extract_pdf_text", 
                                  "get_pdf_info", "clean_text", "extract_key_sentences",
                                  "extract_keywords", "get_text_stats",
                                  "summary_length", "keyword_count"))
    
    parallel::clusterEvalQ(cl, {
      library(pdftools)
      library(stringr)
      library(tm)
    })
    
    summaries <- parallel::parLapply(cl, pdf_files, summarize_single_pdf,
                                     summary_length = summary_length,
                                     keyword_count = keyword_count)
    
    parallel::stopCluster(cl)
    
  } else {
    # Sequential processing
    summaries <- base::lapply(pdf_files, function(pdf_file) {
      summarize_single_pdf(pdf_file, summary_length, keyword_count)
    })
  }
  
  end_time <- Sys.time()
  processing_time <- base::difftime(end_time, start_time, units = "mins")
  
  cat(base::paste("\nProcessing complete! Time taken:", base::round(processing_time, 2), "minutes\n"))
  
  # Compile results
  successful_summaries <- summaries[base::sapply(summaries, function(x) is.null(x$error))]
  failed_summaries <- summaries[base::sapply(summaries, function(x) !is.null(x$error))]
  
  cat(base::paste("Successfully processed:", base::length(successful_summaries), "PDFs\n"))
  cat(base::paste("Failed to process:", base::length(failed_summaries), "PDFs\n"))
  
  # Save results if requested
  if (save_results && base::length(successful_summaries) > 0) {
    timestamp <- base::format(Sys.time(), "%Y%m%d_%H%M%S")
    results_file <- base::paste0("pdf_summaries_", timestamp, ".rds")
    base::saveRDS(summaries, results_file)
    cat(base::paste("Results saved to:", results_file, "\n"))
  }
  
  return(list(
    summaries = successful_summaries,
    failed = failed_summaries,
    processing_time = processing_time,
    total_files = base::length(pdf_files)
  ))
}

# Function to create a summary report
create_summary_report <- function(summary_results, output_file = "pdf_summary_report.html") {
  
  if (is.null(summary_results) || base::length(summary_results$summaries) == 0) {
    cat("No summaries to report\n")
    return()
  }
  
  summaries <- summary_results$summaries
  
  # Start HTML report
  html_content <- c(
    "<!DOCTYPE html>",
    "<html><head>",
    "<title>PDF Summary Report</title>",
    "<style>",
    "body { font-family: Arial, sans-serif; margin: 40px; }",
    ".pdf-summary { border: 1px solid #ddd; margin: 20px 0; padding: 20px; }",
    ".filename { font-size: 18px; font-weight: bold; color: #333; }",
    ".metadata { background: #f5f5f5; padding: 10px; margin: 10px 0; }",
    ".keywords { color: #666; font-style: italic; }",
    ".key-sentences { margin: 15px 0; }",
    "</style>",
    "</head><body>",
    base::paste("<h1>PDF Summary Report - ", base::length(summaries), " Documents</h1>"),
    base::paste("<p>Generated on:", Sys.Date(), "</p>")
  )
  
  # Add each PDF summary
  for (summary in summaries) {
    
    keywords_text <- base::paste(summary$top_keywords$keyword[1:5], collapse = ", ")
    sentences_text <- base::paste(summary$key_sentences[1:3], collapse = " ")
    
    pdf_html <- c(
      "<div class='pdf-summary'>",
      base::paste("<div class='filename'>", summary$filename, "</div>"),
      "<div class='metadata'>",
      base::paste("Pages:", summary$pages, "| Words:", summary$words, 
                  "| Readability Score:", summary$readability_score),
      base::ifelse(!is.na(summary$author), base::paste("<br>Author:", summary$author), ""),
      base::ifelse(!is.na(summary$title), base::paste("<br>Title:", summary$title), ""),
      "</div>",
      base::paste("<div class='keywords'><strong>Keywords:</strong>", keywords_text, "</div>"),
      "<div class='key-sentences'>",
      "<strong>Key Sentences:</strong><br>",
      base::paste(sentences_text),
      "</div>",
      "</div>"
    )
    
    html_content <- c(html_content, pdf_html)
  }
  
  # Close HTML
  html_content <- c(html_content, "</body></html>")
  
  # Write to file
  base::writeLines(html_content, output_file)
  cat(base::paste("HTML report saved to:", output_file, "\n"))
}

# Function to create a CSV summary table
create_csv_summary <- function(summary_results, output_file = "pdf_summary_table.csv") {
  
  if (is.null(summary_results) || base::length(summary_results$summaries) == 0) {
    cat("No summaries to export\n")
    return()
  }
  
  summaries <- summary_results$summaries
  
  # Create data frame
  summary_df <- data.frame(
    filename = base::sapply(summaries, function(x) x$filename),
    pages = base::sapply(summaries, function(x) x$pages %||% NA),
    words = base::sapply(summaries, function(x) x$words %||% NA),
    sentences = base::sapply(summaries, function(x) x$sentences %||% NA),
    readability_score = base::sapply(summaries, function(x) x$readability_score %||% NA),
    author = base::sapply(summaries, function(x) x$author %||% ""),
    title = base::sapply(summaries, function(x) x$title %||% ""),
    top_keywords = base::sapply(summaries, function(x) {
      base::paste(x$top_keywords$keyword[1:5], collapse = "; ")
    }),
    first_sentence = base::sapply(summaries, function(x) {
      if (base::length(x$key_sentences) > 0) x$key_sentences[1] else ""
    }),
    stringsAsFactors = FALSE
  )
  
  utils::write.csv(summary_df, output_file, row.names = FALSE)
  cat(base::paste("CSV summary saved to:", output_file, "\n"))
}

# Function to create word cloud from all PDFs
create_wordcloud <- function(summary_results, output_file = "pdf_wordcloud.png") {
  
  if (!base::requireNamespace("wordcloud", quietly = TRUE)) {
    cat("wordcloud package required for this function\n")
    return()
  }
  
  if (is.null(summary_results) || base::length(summary_results$summaries) == 0) {
    cat("No summaries available for word cloud\n")
    return()
  }
  
  # Combine all keywords
  all_keywords <- character(0)
  for (summary in summary_results$summaries) {
    all_keywords <- c(all_keywords, base::rep(summary$top_keywords$keyword, summary$top_keywords$frequency))
  }
  
  # Create word frequency table
  word_freq <- base::table(all_keywords)
  
  # Create word cloud
  grDevices::png(output_file, width = 800, height = 600)
  wordcloud::wordcloud(base::names(word_freq), word_freq, 
                       min.freq = 2, max.words = 100,
                       colors = grDevices::brewer.pal(8, "Dark2"))
  grDevices::dev.off()
  
  cat(base::paste("Word cloud saved to:", output_file, "\n"))
}

# Example usage function
demo_pdf_summarization <- function() {
  cat("=== PDF SUMMARIZATION EXAMPLES ===\n\n")
  
  cat("Example 1: Summarize all PDFs in a directory\n")
  cat("results <- summarize_pdf_directory('./pdfs/', summary_length = 3)\n\n")
  
  cat("Example 2: Create reports\n")
  cat("create_summary_report(results)\n")
  cat("create_csv_summary(results)\n")
  cat("create_wordcloud(results)\n\n")
  
  cat("Example 3: Summarize single PDF\n")
  cat("summary <- summarize_single_pdf('document.pdf', summary_length = 5)\n\n")
  
  cat("Required packages:\n")
  cat("install.packages(c('pdftools', 'stringr', 'dplyr', 'tm', 'SnowballC', 'wordcloud'))\n")
}

# Package check function
check_pdf_packages <- function() {
  required <- c("pdftools", "stringr", "dplyr", "tm")
  optional <- c("SnowballC", "wordcloud", "parallel")
  
  cat("Required packages:\n")
  for (pkg in required) {
    status <- if (base::requireNamespace(pkg, quietly = TRUE)) "✓" else "✗"
    cat(base::paste(" ", status, pkg, "\n"))
  }
  
  cat("\nOptional packages:\n")
  for (pkg in optional) {
    status <- if (base::requireNamespace(pkg, quietly = TRUE)) "✓" else "✗"
    cat(base::paste(" ", status, pkg, "\n"))
  }
}

cat("PDF summarization functions loaded!\n")
cat("Main function: summarize_pdf_directory()\n")
cat("Run check_pdf_packages() to verify required packages\n")
cat("Run demo_pdf_summarization() for usage examples\n")
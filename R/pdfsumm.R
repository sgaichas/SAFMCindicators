# pdf summary
# may include AI
# based on Scratch_pdfsumm.R and https://damianoswald.com/blog/pdf-chat-gpt/

# How to store API key in environment variable
# usethis::edit_r_environ()
# open the .Renviron file, it is in the Home directory on macos
# add this line
# YOUR_API_KEY_NAME=your_actual_api_key_string
# it is namd ANTHROPIC_API_KEY

# Set your API key (get from https://console.anthropic.com/)
api_key <- Sys.getenv("ANTHROPIC_API_KEY")  # Store in environment variable

# # apikey test run in terminal
# curl https://api.anthropic.com/v1/messages \
# --header "x-api-key: XXXXX" \
# --header "anthropic-version: 2023-06-01" \
# --header "content-type: application/json" \
# --data \
# '{
#         "model": "claude-sonnet-4-20250514",
#         "max_tokens": 1024,
#         "messages": [
#             {"role": "user", "content": "Hello, world"}
#         ]
#     }'

# returned
#{"model":"claude-sonnet-4-20250514","id":"msg_01Jobakzxrv8xQe3c84nT9bD","type":"message","role":"assistant","content":[{"type":"text","text":"Hello! Nice to meet you. How are you doing today? Is there anything I can help you with?"}],"stop_reason":"end_turn","stop_sequence":null,"usage":{"input_tokens":10,"cache_creation_input_tokens":0,"cache_read_input_tokens":0,"cache_creation":{"ephemeral_5m_input_tokens":0,"ephemeral_1h_input_tokens":0},"output_tokens":25,"service_tier":"standard"}}Sarahs-MacBook-Air:SAFMCindicators sarahgaichas$ 


library(httr2)
library(jsonlite)
library(pdftools)
library(base64enc)

# Function to extract text from PDF
extract_pdf_text <- function(pdf_path) {
  text <- pdftools::pdf_text(pdf_path)
  # Combine all pages into single string
  combined_text <- paste(text, collapse = "\n")
  return(combined_text)
}

# extract_pdf_text() <- function(path, numbering = TRUE, references = TRUE){
#   text <- pdftools::pdf_text(path) |>
#     paste0(collapse = " ") |>
#     paste0(collapse = " ") |>
#     stringr::str_squish()
#   if(numbering) {
#     target <- "\\[.*?\\]"
#     text <- gsub(target, "", text)
#   }
#   if(references) {
#     target <- "\\bReferences|references|REFERENCES\\b"
#     text <- stringr::strsplit(text, target) |>
#       unlist(.) |>
#       head(., -1) |>
#       paste(., collapse = " ")
#   }
#   return(text)
# }

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
call_claude <- function(prompt, api_key, model = "claude-sonnet-4-20250514") {
  
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
  response <- httr2::request(url) |>
    httr2::req_headers(
      "Content-Type" = "application/json",
      "x-api-key" = api_key,
      "anthropic-version" = "2023-06-01"
    ) |>
    httr2::req_body_json(body) |>
    httr2::req_perform()
  
  # Parse response
  response_data <- httr2::resp_body_json(response)
  
  # Extract the text content
  summary <- response_data$content[[1]]$text
  
  return(summary)
}

# Function to summarize PDF
summarize_pdf <- function(pdf_path, api_key, summary_type = "general", folder = getwd()) {
  
  # Extract text from PDF
  cat("Extracting text from PDF...\n")
  pdf_text <- extract_pdf_text(pdf_path)
  
  # Check if text extraction was successful
  if (nchar(pdf_text) < 10) {
    stop("Could not extract meaningful text from PDF")
  }
  
  # Create prompt based on summary type
  prompts <- list(
    ESR = "Please summarize this pdf ecosystem status report in 500 words. After the summary, make a list of the section headings in the report and the ecosystem indicators used in each section. Briefly describe the implications of each ecosystem indicator for fishery management:",
    FEP = "Please summarize this pdf fishery ecosystem plan in 500 words. After the summary, list stated ecosystem policies, goals, and objectives in the document along with management approaches, performance metrics and specific ecosystem indicators identified for each of the objectives:",
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
  
  summary <- paste(summary, prompts[[summary_type]], sep="\n\nPROMPT: ")
  
  namesum <- basename(pdf_path)
  
  readr::write_file(summary, here::here(paste0(folder, "/", namesum, ".Rmd")))
  
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

# testing
# pdf_path <- "~/Documents/Work/SAFMCindicators/ESRs/MidAtlantic_SOE_2025_noaa_70290_DS1.pdf"
# summary_type <- "ESR"
# 
# summary <- summarize_pdf("~/Documents/Work/SAFMCindicators/ESRs/MidAtlantic_SOE_2025_noaa_70290_DS1.pdf", api_key, "ESR")

#pdf_path <- "~/Documents/Work/SAFMCindicators/FEPs/NPFMCAleutianIslandsFEP.pdf"

pdf_files <- c("~/Documents/Work/SAFMCindicators/FEPs/NPFMCBeringSeaFEP.pdf",
               "~/Documents/Work/SAFMCindicators/FEPs/NPFMCAleutianIslandsFEP.pdf",
               "~/Documents/Work/SAFMCindicators/FEPs/PFMCpacific-coast-fishery-ecosystem-plan-march-2022.pdf",
               "~/Documents/Work/SAFMCindicators/FEPs/WPRFMC Hawaii FEP (2009-09-21).pdf",
               "~/Documents/Work/SAFMCindicators/FEPs/WPRFMC-Pelagic-FEP-2009-09-21.pdf",
               "~/Documents/Work/SAFMCindicators/FEPs/WPRFMC PRIA FEP (2009-09-21).pdf",
               "~/Documents/Work/SAFMCindicators/FEPs/WPRFMC Mariana FEP (2009-09-22).pdf",
               "~/Documents/Work/SAFMCindicators/FEPs/WPRFMC American Samoa FEP (2009-09-22).pdf",
               "~/Documents/Work/SAFMCindicators/FEPs/MAFMCEAFM+Doc+Revised+2019-02-08.pdf",
               "~/Documents/Work/SAFMCindicators/FEPs/GFMC_FEP_Draft.pdf",
               "~/Documents/Work/SAFMCindicators/FEPs/carib_fmp_st_thomasst_john.pdf",
               "~/Documents/Work/SAFMCindicators/FEPs/carib_fmp_st_croix.pdf",
               "~/Documents/Work/SAFMCindicators/FEPs/carib_fmp_puerto_rico.pdf",
               "~/Documents/Work/SAFMCindicators/FEPs/NEFMCGeorgesBankDraft-Example-of-Fishery-Ecosystem-Plan-eFEP--NOTAPPROVED.pdf",
               "~/Documents/Work/SAFMCindicators/FEPs/SAFMCfishery-ecosystem-plan-2-fep-ii.pdf"
)

pdf_path <- pdf_files[13] #11,12,13, 15 break

summary_type <- "FEP"

summary <- summarize_pdf(pdf_path, api_key, "FEP", "FEPsumms")


# knit all rmd in a directory

fepsumms <- list.files(here::here("FEPsumms"), pattern = "\\.Rmd$", full.names = TRUE)

#knitone

knitone <- function(fpath) {
  file_name <- tools::file_path_sans_ext(basename(fpath))
  outdir <- dirname(fpath)
  # Define the output file path for the PDF
  output_file <- file.path(outdir, paste0(file_name, ".pdf"))
  
  # Render the Rmd file to PDF
  # output_format: specifies PDF output
  # output_file: specifies the name and location of the output PDF
  # output_dir: specifies the directory where the output will be saved
  rmarkdown::render(input = fpath, 
                    output_format = "pdf_document", 
                    output_file = output_file,
                    output_dir = outdir)
  
}

purrr::map(fepsumms, knitone)

# now tell Claude to compare objectives across docs in a table


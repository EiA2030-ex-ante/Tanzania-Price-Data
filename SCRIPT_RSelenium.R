setwd("H:\\Tanzania Price data\\Datasets")

# Load libraries
library(RSelenium)
library(rvest)
library(tidyverse)
library(httr)
library(stringr)

# Define path to chromedriver
chromedriver <- "C:\\Users\\LMADAGA\\AppData\\Local\\binman\\binman_chromedriver\\win32\\124.0.6367.78.chromedriver.exe"

# Start RSelenium
driver <- rsDriver(browser = "chrome", chromever = "124.0.6367.78", extraCapabilities = list(chromever = chromedriver))
remDr <- driver[["client"]]

# Initialize list to store PDF links
all_pdf_links <- list()

# Define base URL and find total number of pages
base_url <- "https://www.viwanda.go.tz/documents/product-prices-domestic?page="
total_pages <- 27  # Update with actual total number of pages

# Loop through each page
for (page in 1:total_pages) {
  # Navigate to the page
  page_url <- paste0(base_url, page)
  remDr$navigate(page_url)
  
  # Extract PDF links from the current page
  pdf_links <- remDr$findElements(using = "css selector", value = "a[href$='.pdf']") %>%
    map(~ .x$getElementAttribute("href")[[1]])
  
  # Add PDF links to the list
  all_pdf_links <- c(all_pdf_links, pdf_links)
}

# Create a directory to save the downloaded PDFs
dir.create("downloaded_pdfs", showWarnings = FALSE)

# Initialize a list to store unique filenames
unique_filenames <- list()

# Iterate through each PDF link and download the PDFs
for (link in unlist(all_pdf_links)) {
  # Extract the filename from the URL
  filename <- basename(link)
  
  # Check if the filename already exists
  if (!(filename %in% unique_filenames)) {
    unique_filenames <- c(unique_filenames, filename)
    
    # Download the PDF
    tryCatch({
      GET(link, write_disk(file.path("downloaded_pdfs", filename)))
    }, error = function(e) {
      cat("Error downloading:", filename, "\n")
    })
  }
}

# Close the RSelenium session
remDr$close()

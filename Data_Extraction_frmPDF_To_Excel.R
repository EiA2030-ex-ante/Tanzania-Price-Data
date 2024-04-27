##Processing data from the pdf files to excel formart.

setwd("H:/Tanzania Price data/downloaded_pdfs")

library(pdftools)
library(stringr)
library(writexl)

# Get list of PDF files in the directory
pdf_files <- list.files(pattern = "\\.pdf$", full.names = TRUE)

# Initialize an empty data frame to store the combined data
combined_df <- data.frame()

# Loop through each PDF file
for (pdf_file in pdf_files) {
  # Read the PDF file
  tx <- pdf_text(pdf_file)
  
  # Split the text into lines
  tx2 <- unlist(str_split(tx, "[\\r\\n]+"))
  
  # Split the lines into columns (assuming 5 columns)
  tx3 <- str_split_fixed(str_trim(tx2), "\\s{2,}", 18)
  
  # Convert to data frame
  df <- as.data.frame(tx3, stringsAsFactors = FALSE)
  
  # Add a new column to identify the source PDF
  df$Source_PDF <- pdf_file
  
  # Combine the data with the existing data frame
  combined_df <- rbind(combined_df, df)
}

# Write the combined data frame to an Excel file
write_xlsx(combined_df, "combined_output.xlsx")

# Write the combined data frame to a CSV file
write.csv(combined_df, "combined_output.csv", row.names = FALSE)

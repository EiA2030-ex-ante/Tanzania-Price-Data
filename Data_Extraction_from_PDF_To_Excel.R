setwd("H:/Tanzania Price data/Datasets/Dataset Correction")

# Load necessary libraries
library(pdftools)
library(stringr)
library(dplyr)

# Get list of PDF files in the directory
pdf_files <- list.files(pattern = "\\.pdf$", full.names = TRUE)

# Initialize an empty data frame to store the combined data
combined_df <- data.frame()

# Function to clean and preprocess each line
clean_line <- function(line) {
  # Print the original line for debugging
  cat("Original line: ", line, "\n")
  
  # Remove leading and trailing whitespace in names
  line <- gsub("^\\s+|\\s+$", "", line)
  
  # Replace specific multi-word names with underscores
  # This is done to avoid shifting data to wrong columns during the extraction process
  line <- gsub("Dar es Salaam", "Dar_es_Salaam", line)
  line <- gsub("Dar es salaam", "Dar_es_Salaam", line)
  line <- gsub("Dar	es	saalam", "Dar_es_Salaam", line)
  line <- gsub("Dar	es	Saalam", "Dar_es_Salaam", line)
  line <- gsub("Dar	Es	Salaam", "Dar_es_Salaam", line)
  line <- gsub("Dar	Es	Salaam", "Dar_es_Salaam", line)
  line <- gsub("Dar	Es_Salaam", "Dar_es_Salaam", line)
  line <- gsub("Dar	es	saalam", "Dar_es_Salaam", line)
  line <- gsub("Dar es saalam ", "Dar_es_Salaam", line)
  line <- gsub("Dar Es Salaam", "Dar_es_Salaam", line)
  line <- gsub("Dar -es-Salaam", "Dar_es_Salaam", line)
  line <- gsub("Dar es Saalam", "Dar_es_Salaam", line)
  line <- gsub("Mtwara Dc", "Mtwara_Dc", line)
  line <- gsub("Mtwara	DC", "Mtwara_Dc", line)
  line <- gsub("Mtwara DC", "Mtwara_Dc", line)
  line <- gsub("Lindi	Mc", "Lindi_Mc", line)
  line <- gsub("Lindi Mc", "Lindi_Mc", line)
  line <- gsub("Bariadi	TC", "Bariadi_TC", line)
  line <- gsub("Bariadi TC", "Bariadi_TC", line)
  # Add more here if necessary
  
  # Replace commas within numbers to prevent splitting issues
  line <- gsub("(?<=\\d),(?=\\d)", "", line, perl = TRUE)
  # Replace spaces within numbers to prevent splitting issues
  line <- gsub("(?<=\\d) (?=\\d)", "", line, perl = TRUE)
  # Handle names with parentheses correctly
  line <- gsub("(?<=\\S) (?=\\()", "_", line, perl = TRUE)
  line <- gsub("(?<=\\)) (?=\\S)", "_", line, perl = TRUE)
  # Replace multiple spaces with a single space
  line <- gsub("\\s+", " ", line)
  
  # Print the cleaned line for debugging
  cat("Cleaned line: ", line, "\n")
  
  # Split the line by space
  parts <- unlist(strsplit(line, " "))
  
  # Ensure each line has exactly 18 parts
  if (length(parts) < 18) {
    # Fill missing values with NA to ensure it has 18 columns
    parts <- c(parts, rep(NA, 18 - length(parts)))
  } else if (length(parts) > 18) {
    # If there are more than 18 parts, combine them
    date <- parts[length(parts)]
    parts <- parts[-length(parts)]
    excess <- length(parts) - 17
    parts[17] <- paste(parts[17:(17 + excess)], collapse = " ")
    parts <- parts[1:17]
    parts <- c(parts, date)
  }
  
  return(parts)
}

# Function to extract the date from the PDF file name
extract_date_from_filename <- function(filename) {
  # Match various formats
  date_str <- str_extract(filename, 
                          "\\d{1,2}(st|nd|rd|th)?(%20|\\.|_)?(Of%20)?\\w+,%20\\d{4}|\\w+%20\\d{1,2}(st|nd|rd|th)?(%20|_|\\,)?%20?\\d{4}|\\w+%20\\d{1,2}th?%20\\d{4}|\\d{1,2}(st|nd|rd|th)?%20Of%20\\w+,%20\\d{4}")
  
  if (!is.na(date_str)) {
    # Replace %20, ., and _ with a space if found
    date_str <- gsub("%20|\\.|_", " ", date_str)
    # Remove multiple spaces
    date_str <- gsub("\\s+", " ", date_str)
    # Ensure that ordinal indicators (st, nd, rd, th) are handled properly
    date_str <- gsub("([0-9]+)(st|nd|rd|th)", "\\1", date_str)
    # Remove "Of" in the middle of the date
    date_str <- gsub(" Of ", " ", date_str)
  }
  
  return(date_str)
}

# Main processing loop
for (pdf_file in pdf_files) {
  # Read the PDF file
  tx <- pdf_text(pdf_file)
  
  # Split the text into lines
  tx2 <- unlist(str_split(tx, "[\\r\\n]+"))
  
  # Skip the header lines and the additional parts
  tx2 <- tx2[!grepl("CHANZO|WIZARA YA VIWANDA NA BIASHARA|IDARA YA MAENDELEO YA BIASHARA|BEI ZA JUMLA ZA MAZAO MAKUU YA CHAKULA NCHINI|Region / Mkoa|District / Market|Irish Potatoes \\(Viazi Mviringo\\)|Maize \\(Mahindi\\)|Wheat \\(Ngano Punje\\)|Beans \\(Maharage\\)|Min Price|Max Price|Market Information System- MIT|District\t/\\tMviringo|Market|District|Mviringo)", tx2)]
  
  # Clean each line and convert to a matrix
  tx3 <- t(sapply(tx2, clean_line))
  
  # Convert to data frame
  df <- as.data.frame(tx3, stringsAsFactors = FALSE)
  
  # Add a new column to identify the source PDF
  df$Source_PDF <- pdf_file
  # Add a new column for the extracted date
  df$Date <- extract_date_from_filename(pdf_file)
  
  # Combine the data with the existing data frame
  combined_df <- rbind(combined_df, df)
}

# Assign column names
colnames(combined_df) <- c(
  "Region", "District_Market", "Maize_min", "Maize_max", "Rice_min", "Rice_max", "Sorghum_min", "Sorghum_max", 
  "Bulrush_Millet_min", "Bulrush_Millet_max", "Finger_Millet_min", "Finger_Millet_max", "Wheat_min", "Wheat_max", 
  "Beans_min", "Beans_max", "Irish_Potatoes_min", "Irish_Potatoes_max", "Source_PDF", "Date"
)

# Remove rows where the first column starts with NA
combined_df <- combined_df %>%
  filter(!is.na(Region))

# Write the combined data to a CSV file
write.csv(combined_df, "revised_data2.csv")

#read extracted data CSV File
dta <- read.csv("revised_data2.csv")
head(dta)

#Remove the first column.
#Not well formated when extracting data
dta <- dta[, -1]
# View the modified data
head(dta)

# save csv file
write.csv(dta, "revised_data2.csv")


##Proceed to correct a few other errors in excel.
#For example some prices from 20th-27th July 2022, 5th-14th spt, 1oth-17thoct were in separate pdfs
##Be sure to merge them.
#Correct dates that registered as NA
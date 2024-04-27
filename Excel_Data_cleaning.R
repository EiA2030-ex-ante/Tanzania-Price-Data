setwd("H:/Tanzania Price data/downloaded_pdfs")

# Load the necessary library
library(stringr)

# Read the data
Dt <- read.csv("combined_output2.csv")

# Extract the date part using regular expressions
Dt$Date <- str_extract(Dt$Source_PDF, "\\d{2}[a-z]{2}%20\\w+,%20\\d{4}")

# Remove "%20" and convert the date format
Dt$Date <- gsub("%20", " ", Dt$Date)

# View the first few rows to confirm the extraction
head(Dt$Date)

# Save the dataframe to a CSV file
write.csv(Dt, file = "combined_output_dates.csv", row.names = FALSE)


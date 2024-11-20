setwd("H:\\Tanzania Price data\\Datasets\\downloaded_pdfs\\october")

# Load necessary libraries
library(pdftools)
library(stringr)
library(dplyr)

# Get list of PDF files in the directory
pdf_files <- list.files(pattern = "\\.pdf$", full.names = TRUE)
pdf_files

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
  line <- gsub("Dar	Es	Salaam", "Dar es Salaam", line)
  line <- gsub("Dar	Es_Salaam", "Dar_es_Salaam", line)
  line <- gsub("Dar	es	saalam", "Dar_es_Salaam", line)
  line <- gsub("Dar es saalam ", "Dar_es_Salaam", line)
  line <- gsub("Dar Es Salaam", "Dar_es_Salaam", line)
  line <- gsub("Dar -es-Salaam", "Dar_es_Salaam", line)
  line <- gsub("Dar es Saalam", "Dar_es_Salaam", line)
  line <- gsub("Mtwara Dc", "Mtwara", line)
  line <- gsub("Mtwara	DC", "Mtwara", line)
  line <- gsub("Mtwara DC", "Mtwara", line)
  line <- gsub("Lindi	Mc", "Lindi", line)
  line <- gsub("Lindi Mc", "Lindi", line)
  line <- gsub("Bariadi	TC", "Bariadi", line)
  line <- gsub("Bariadi TC", "Bariadi", line)
  line <- gsub("Lindi mjin", "Lindi", line)
  # Add more here if necessary
  
  # Replace '/' with '_' in market names
  line <- gsub("/", "_", line)
  
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

extract_date_from_filename <- function(filename) {
  # Match the date format in the file names
  date_str <- str_extract(filename, "\\d{1,2}(st|nd|rd|th)?\\s?\\w+\\s?,?\\s?\\d{4}")
  
  # Clean up the matched date string
  if (!is.na(date_str)) {
    # Remove ordinal suffixes (st, nd, rd, th)
    date_str <- gsub("(st|nd|rd|th)", "", date_str)
    # Remove extra spaces
    date_str <- str_trim(date_str)
  }
  
  return(date_str)
}

# Main processing loop
library(pdftools)
library(stringr)

# Initialize an empty data frame to store combined data
combined_df <- data.frame()

for (pdf_file in pdf_files) {
  # Try to read the PDF file and catch any errors
  try({
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
  }, silent = TRUE) # Suppresses error messages from try block
}

# Display the combined data frame
print(combined_df)

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
write.csv(combined_df, "Oct_2024.csv")

#read extracted data CSV File
dta <- read.csv("Oct_2024.csv")
head(dta)

#Remove the first column.
#Not well formated when extracting data
dta <- dta[, -1]
# View the modified data
head(dta)

# save csv file
write.csv(dta, "Oct_2024.csv")


##Proceed to correct a few other errors in excel.
#For example some prices from 20th-27th July 2022, 5th-14th spt, 1oth-17thoct were in separate pdfs
##Be sure to merge them.
#Correct dates that registered as NA

# Load the necessary library
library(stringr)

# Sample data
dta <- read.csv("Oct_2024.csv")
head(dta)

# Update the regular expression to handle dates with or without the comma
dta$Date <- sub(".*?(\\d{1,2}(?:st|nd|rd|th)? \\w+ ?\\w*,? \\d{4}).*", "\\1", dta$Source_PDF)

# Remove the ordinal suffix (st, nd, rd, th)
dta$Date <- gsub("([0-9]+)(st|nd|rd|th)", "\\1", dta$Date)

# Convert to Date format
dta$Date <- gsub("([0-9]+\\s+[A-Za-z]+\\s+\\d{4})", "\\1", dta$Date)

# Check the unique values again
unique(dta$Date)

# Remove the Source_PDF column
dta$Source_PDF <- NULL
head(dta)

library(lubridate)
# Remove any commas from the Date column
dta$Date <- gsub(",", "", dta$Date)
unique(dta$Date)
# Convert to date format
dta$Date <- dmy(dta$Date)
head(dta)

write.csv(dta, "Oct_2024.csv")

#Harmonize the namesof places
# Read the dataset containing market prices
dta <- read.csv("Oct_2024.csv")
head(dta)

unique(dta[, c("Region", "Market")])
#Harmonize the names of places
unique(dta[, c("Region", "Market")])
# it looks like spelling needs to be harmonized
unique(dta[, c("Region")])
dta[dta$Region %in% c("Dar es salaam", "Dar es Saalam", "D'Salaam", "Dar Es Salaam",
                      "Dar -es-Salaam", "Dar es Salaam", "Dar es Salaam ", 
                      "Dar es Saalam ", "Dar es Salaam", "Dar es Salaam Temeke", 
                      "Dar es saalam", "Dar_es_Salaam"), c("Region")] <- "Dar es Salaam"
dta[dta$Region %in% c("AB4:S21rusha", "Arusha"), c("Region")] <- "Arusha"
dta[dta$Region %in% c("Ruvi\\uma" ,"Ruvuma"), c("Region")] <- "Ruvuma"
dta[dta$Region %in% c("Kilimanajaro", "Kilimanajro", "Kilimanjaro", "Kilimanjaro "), c("Region")] <- "Kilimanjaro"
dta[dta$Region %in% c("Nombe"), c("Region")] <- "Njombe"
dta[dta$Region %in% c("Katavi "), c("Region")] <- "Katavi"
dta[dta$Region %in% c("Manyara "), c("Region")] <- "Manyara"
dta[dta$Region %in% c("Mtwara "), c("Region")] <- "Mtwara"
dta[dta$Region %in% c("Rukwa "), c("Region")] <- "Rukwa"
dta[dta$Region %in% c("Mara "), c("Region")] <- "Mara"
dta[dta$Region %in% c("Shinyanga "), c("Region")] <- "Shinyanga"
dta[dta$Region %in% c("Tabora "), c("Region")] <- "Tabora"
dta[dta$Region %in% c("Kagera "), c("Region")] <- "Kagera"
dta[dta$Region %in% c("Mwanza "), c("Region")] <- "Mwanza"
dta[dta$Region %in% c("Dodoma "), c("Region")] <- "Dodoma"
dta[dta$Region %in% c("Mbeya "), c("Region")] <- "Mbeya"
dta[dta$Region %in% c("Sumbawanga ", "Sumbawanga"), c("Region")] <- "Rukwa"

unique(dta[, c("Region")])

unique(dta[, c("Market")])
dta[dta$Market %in% c("Arusha (Urban)", "Arusha", "Arusha_(Urban)", "Arusha "), c("Market")] <- "Arusha"
dta[dta$Market %in% c(" Temeke", "Temeke (Tandika)", "Temeke(Tandika)", "Temeke_(Tandika)",
                      "Temeke"), c("Market")] <- "Temeke"
dta[dta$Market %in% c(" Kinondoni", "Kinondoni (Tandale)", "Kinondoni(Tandale)", "Kinondoni", "Kinondoni_(Tandale)"), c("Market")] <- "Kinondoni"
dta[dta$Market %in% c("mbeya", "Mbeya", "SIDO", "Mbeya "), c("Market")] <- "Mbeya"
dta[dta$Market %in% c("moshi", "Moshi", "Moshi "), c("Market")] <- "Moshi"
dta[dta$Market %in% c("mwanza", "Mwanza"), c("Market")] <- "Mwanza"
dta[dta$Market %in% c("Ilala (Buguruni)", "Ilala(Buguruni)", "ilala (Buguruni)",
                      "Ilala", "ilala", "Ilala_(Buguruni)"), c("Market")] <- "Ilala"
dta[dta$Market %in% c("Manispaa Tabora", "Tabora", "Tabora "), c("Market")] <- "Tabora"
dta[dta$Market %in% c("Lindi Mc", "Lindi", "Lindi_Mc", "Lindi  Mc", "Lindi mjini", "Lindii"), c("Market")] <- "Lindi"
dta[dta$Market %in% c("Bariadi TC", "Bariadi", "Bariadi_TC" ), c("Market")] <- "Bariadi"
dta[dta$Market %in% c("Mtwara Dc", "Mtwara DC", "Mtwara", "Mtwara_Dc"), c("Market")] <- "Mtwara"
dta[dta$Market %in% c("Kilimanajaro", "Kilimanajro", "Kilimanjaro"), c("Market")] <- "Kilimanjaro"
dta[dta$Market %in% c("ubungo", "Ubungo"), c("Market")] <- "Ubungo"
dta[dta$Market %in% c("Kibaigwa", "Kibaigwa"), c("Market")] <- "Kibaigwa"
dta[dta$Market %in% c("Igawilo/Soweto", "Igawilo_Soweto", "Igawilo_Soweto"), c("Market")] <- "Igawilo"
dta[dta$Market %in% c("BabatiTC", "Babati ", "BabatiTC"), c("Market")] <- "Babati"
dta[dta$Market %in% c(" Singida"), c("Market")] <- "Singida"
dta[dta$Market %in% c("Shinyanga "), c("Market")] <- "Shinyanga"
dta[dta$Market %in% c("Sumbawanga "), c("Market")] <- "Sumbawanga"
dta[dta$Market %in% c("Musoma "), c("Market")] <- "Musoma"
dta[dta$Market %in% c("Mpanda "), c("Market")] <- "Mpanda"
dta[dta$Market %in% c(" Majengo"), c("Market")] <- "Majengo"

unique(dta[, c("Market")])

unique(dta[, c("Region", "Market")])

write.csv(dta, "Oct_2024.csv")
# Add coordinates to the price data using market coordinates already prepared

# Load required packages
library(data.table)
library(leaflet)
library(geodata)
library(lubridate)
library(terra)


# Add coodinates to dataset
# Read the dataset containing market names and coordinates
coods <- read.csv("Market_coodinates_TZ2.csv")
coods

dta <- read.csv("Oct_2024.csv")


# Create a new column in the market prices dataset
dta$Latitude <- NA
dta$Longitude <- NA

# Iterate through markets and regions to find matching coordinates
for (i in 1:nrow(dta)) {
  market <- dta[i, "Market"]
  region <- dta[i, "Region"]
  
  # Find matching coordinates based on both Market and Region
  coord <- coods[coods$Market == market & coods$Region == region, c("Latitude", "Longitude")]
  
  # If a match is found, update the Latitude and Longitude in dta
  if (nrow(coord) > 0) {
    dta[i, c("Latitude", "Longitude")] <- coord
  }
}

unique(dta[, c("Region", "Market", "Latitude", "Longitude")])

head(dta)

# Save the modified dataset with coordinates
write.csv(dta, "Oct_2024_coodinates.csv", row.names = FALSE)

dta2 <- read.csv("Oct_2024_coodinates.csv")
dim(dta2)
head(dta2)
table(dta2$Market)
sapply(dta2, class)

unique(dta2[, c("Region", "Market", "Latitude", "Longitude")])

m <- leaflet(data = dta2) %>%
  addTiles() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircleMarkers(
    lng = ~ Longitude, lat = ~ Latitude,
    label = ~paste0(Market, ", ", Region),
    radius = 5,
    color = "blue",
    fillOpacity = 0.7
  )
m


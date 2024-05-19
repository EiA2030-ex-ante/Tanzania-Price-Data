setwd("H:/Tanzania Price data/Datasets")

library(lubridate)
library(data.table)
library(dplyr)

dta <- read.csv("Tanzania_Price_Data_AllCrops1.csv")
dim(dta)
head(dta)
table(dta$Market)
sapply(dta, class)

# fix dates
dta <- dta %>%
  mutate(Date = gsub("Machi", "March", Date))
#Rename months for consistency
# Replace abbreviated month names with full names
replace_months <- function(date_str) {
  date_str <- gsub("\\b20sept\\b", "September", date_str)
  date_str <- gsub("\\bJan\\b", "January", date_str)
  date_str <- gsub("\\bFeb\\b", "February", date_str)
  date_str <- gsub("\\bNov\\b", "November", date_str)
  date_str <- gsub("\\bDec\\b", "December", date_str)
  return(date_str)
}
# Apply the function to the Date column
dta <- dta %>%
  mutate(Date = replace_months(Date))

# Convert to date format
#confirm date formart first
dta$Date <- dmy(dta$Date)
head(dta$Date)


#Harmonize the namesof places
unique(dta[, c("Region", "Market")])
# it looks like spelling needs to be harmonized
unique(dta[, c("Region")])
dta[dta$Region %in% c("Dar es salaam", "Dar es Saalam", "D'Salaam", "Dar Es Salaam",
                            "Dar -es-Salaam", "Dar es Salaam", "Dar es Salaam ", 
                            "Dar es Saalam ", "Dar es Salaam", "Dar es Salaam Temeke", 
                            "Dar es saalam", "Dar_es_Salaam"), c("Region")] <- "Dar es Salaam"
dta[dta$Region %in% c("AB4:S21rusha", "Arusha"), c("Region")] <- "Arusha"
dta[dta$Region %in% c("Ruvi\\uma" ,"Ruvuma"), c("Region")] <- "Ruvuma"
dta[dta$Region %in% c("Kilimanajaro", "Kilimanajro", "Kilimanjaro"), c("Region")] <- "Kilimanjaro"
dta[dta$Region %in% c("Nombe"), c("Region")] <- "Njombe"
unique(dta[, c("Region")])

unique(dta[, c("Market")])
dta[dta$Market %in% c("Arusha (Urban)", "Arusha", "Arusha_(Urban)"), c("Market")] <- "Arusha"
dta[dta$Market %in% c(" Temeke", "Temeke (Tandika)", "Temeke(Tandika)", "Temeke_(Tandika)",
                            "Temeke"), c("Market")] <- "Temeke"
dta[dta$Market %in% c(" Kinondoni", "Kinondoni (Tandale)", "Kinondoni(Tandale)", "Kinondoni", "Kinondoni_(Tandale)"), c("Market")] <- "Kinondoni"
dta[dta$Market %in% c("mbeya", "Mbeya", "SIDO"), c("Market")] <- "Mbeya"
dta[dta$Market %in% c("moshi", "Moshi"), c("Market")] <- "Moshi"
dta[dta$Market %in% c("mwanza", "Mwanza"), c("Market")] <- "Mwanza"
dta[dta$Market %in% c("Ilala (Buguruni)", "Ilala(Buguruni)", "ilala (Buguruni)",
                            "Ilala", "ilala", "Ilala_(Buguruni)"), c("Market")] <- "Ilala"
dta[dta$Market %in% c("Manispaa Tabora", "Tabora"), c("Market")] <- "Tabora"
dta[dta$Market %in% c("Lindi Mc", "Lindi", "Lindi_Mc"), c("Market")] <- "Lindi"
dta[dta$Market %in% c("Bariadi TC", "Bariadi", "Bariadi_TC" ), c("Market")] <- "Bariadi"
dta[dta$Market %in% c("Mtwara Dc", "Mtwara DC", "Mtwara", "Mtwara_Dc"), c("Market")] <- "Mtwara"
dta[dta$Market %in% c("Kilimanajaro", "Kilimanajro", "Kilimanjaro"), c("Market")] <- "Kilimanjaro"
dta[dta$Market %in% c("ubungo", "Ubungo"), c("Market")] <- "Ubungo"
dta[dta$Market %in% c("Kibaigwa", "Kibaigwa"), c("Market")] <- "Kibaigwa"

unique(dta[, c("Market")])
unique(dta[, c("Region", "Market")])

#Check for duplicate dates and clean
table(dta$Date)

# Remove comas between numbers
dta <- read.csv("Tanzania_Price_Data_AllCrops_With_Coordinates2.csv")

# List of columns that need commas removed
price_columns <- c("Maize..min.price.", "Maize..max.price.", "Rice..min.price.", "Rice..max.price.",
                   "Sorghum..min.price.", "Sorghum..max.price.", "Bulrush.Millet..min.price.",
                   "Bulrush.Millet..max.price.", "Finger.Millet..min.price.", "Finger.Millet..max.price.",
                   "Wheat..min.price.", "Wheat..max.price.", "Beans..min.price.", "Beans..max.price.",
                   "Irish.Potatoes..min.price.", "Irish.Potatoes..max.price.")

# Function to remove commas from a string
remove_commas <- function(x) {
  gsub(",", "", x)
}

# Apply the remove_commas function to the relevant columns
dta <- dta %>%
  mutate(across(all_of(price_columns), ~ remove_commas(.)))

write.csv(dta, "Tanzania_Price_Data_AllCrops_With_Coordinates3.csv", row.names = FALSE)





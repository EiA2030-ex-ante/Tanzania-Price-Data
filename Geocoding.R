setwd("H:/Tanzania Price data/Datasets")

library(RSelenium)
library(rvest)
library(xml2)
library(data.table)
library(dplyr)

dta <- read.csv("Tanzania_Price_Data_AllCrops_With_Coordinates.csv")
dim(dta)
head(dta)
table(dta$Market)

unique(dta[, c("Region")])
unique(dta[, c("Market")])
unique(dta[, c("Region", "Market")])

#####Geocode
#Specify the markets
#For some market be more specific, similar names may occur in other parts of the world
a <- c("Arusha", "Temeke dar es salaam", "Majengo Dodoma", "Kibaigwa", "Bukoba", "Babati", "Sumbawanga", "Mpanda Region", 
                       "Mtwara town", "Tabora", "Tanga region", "Kinondoni", "Ilala Dar es Salaam", "Iringa Municipal", "Kigoma municipal", "Morogoro town", 
                       "Mwanza", "Musoma", "Songea", "Shinyanga town", "Moshi", "Mbeya", "Njombe town", "Rukwa", "Lindi Town", 
                       "Ruvuma songea", "Manyara", "Mara Musoma", "Tandika Dar es Salaam ", "Ujiji", "Buguruni", "Kilimanjaro", 
                       "Tandale", "Singida", "Pwani", "Bariadi town", "Mpimbwe", "Geita town", "Songwe town", "Ubungo")

# Define path to chromedriver
chromedriver <- "C:\\Users\\LMADAGA\\AppData\\Local\\binman\\binman_chromedriver\\win32\\124.0.6367.78.chromedriver.exe"

# Start RSelenium
driver <- rsDriver(browser = "chrome", chromever = "124.0.6367.78", extraCapabilities = list(chromever = chromedriver))
remDr <- driver[["client"]]

remDr$navigate("https://www.geoplaner.com/")

Latlist = list() # Creating an empty list to store all the latitudes
Longlist = list() # Creating an empty list to store all the longitudes

web_place = remDr$findElement(using = "class", value = "e80adr") # Finding the css element where the locations will be entered on the geoplaner website

for (val in 1:length(a)) {
  aa = web_place$sendKeysToElement(list(paste(a)[val], key="enter")) # Once the location is entered, enter command is executed 
  source <- remDr$getPageSource() #web page source
  
  locn =  read_html(as.character(source)) %>% html_nodes("#dt") %>% html_text() # reading the webpage as the HTML
  
  
  Latlist[val] = strsplit(strsplit(locn, " +")[[1]][8],"°")[[1]][1] #Extracting the latitudes
  Longlist[val] = strsplit(strsplit(locn, " +")[[1]][9],"°")[[1]][1] #Extracting the longitudes
  
  web_place$clearElement() #clear the input area so that the next location can be entered
  Sys.sleep(5) #Allowing system enough time to reload the page after each execution
}

source <- remDr$getPageSource() #web page source
aa = web_place$sendKeysToElement(list(paste(a)[length(a)], key="enter"))
locn =  read_html(as.character(source)) %>% html_nodes("#dt") %>% html_text() 
Latlist = append(Latlist,strsplit(strsplit(locn, " +")[[1]][8],"°")[[1]][1])
Longlist = append(Longlist,strsplit(strsplit(locn, " +")[[1]][9],"°")[[1]][1])

df = data.frame(Location = c(a[2:length(a)], a[1]), 
                Latitude = as.numeric(c(unlist(Latlist)[3:length(Latlist)],unlist(Latlist)[2])),
                Longitude = as.numeric(c(unlist(Longlist)[3:length(Longlist)],unlist(Longlist)[2])))

df

write.csv(df, "coodinates2.csv", row.names = FALSE)


#Include coodinates in our price dataset
# Read the dataset containing market names and coordinates
coods <- read.csv("Market_Coodinates_TZ.csv")

dta <- read.csv("Tanzania_Price_Data_AllCrops_With_Coordinates.csv")

head(dta)

# Create a new column in the market prices dataset
dta$Latitude <- NA
dta$Longitude <- NA

# Iterate through markets and find matching coordinates
for (i in 1:nrow(dta)) {
  market <- dta[i, "Market"]
  coord <- coods[coods$Market == market, c("Latitude", "Longitude")]
  
  if (nrow(coord) > 0) {
    dta[i, c("Latitude", "Longitude")] <- coord
  }
}

# Save the modified dataset with coordinates
write.csv(dta, "Tanzania_Price_Data_AllCrops_With_Coordinates2.csv", row.names = FALSE)



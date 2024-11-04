# Load Libraries
library(geodata)
library(lubridate)
library(terra)
library(data.table)
library(randomForest)
library(httr)
library(caret)
library(Metrics)
library(pdp)
library(gridExtra)
library(stats)
library(dplyr)
library(stringr)
library(fields)
library(corrplot)
library(ggplot2)
library(tidyr)

setwd("H:/Tanzania Price data/Datasets")

prices <- fread("Tanzania_Price_Data_AllCrops_with_Coordinates4.csv")
dim(prices)
head(prices)
table(prices$Market)
sapply(prices, class)


# Convert to date format
prices$Date <- lubridate::mdy(prices$Date)

## Basic Data preperation
unique(prices[Region=="Arusha",.(Market, Latitude, Longitude)])
unique(prices[Region=="Dar es Salaam",.(Market, Latitude, Longitude)])
unique(prices[Region=="Dodoma",.(Market, Latitude, Longitude)])
unique(prices[Region=="Kagera",.(Market, Latitude, Longitude)])
unique(prices[Region=="Manyara",.(Market, Latitude, Longitude)])
unique(prices[Region=="Rukwa",.(Market, Latitude, Longitude)])
unique(prices[Region=="Mpanda",.(Market, Latitude, Longitude)])
unique(prices[Region=="Mtwara",.(Market, Latitude, Longitude)])
unique(prices[Region=="Tabora",.(Market, Latitude, Longitude)])
unique(prices[Region=="Tanga",.(Market, Latitude, Longitude)])
unique(prices[Region=="Iringa",.(Market, Latitude, Longitude)])
unique(prices[Region=="Kigoma",.(Market, Latitude, Longitude)])
unique(prices[Region=="Morogoro",.(Market, Latitude, Longitude)])
unique(prices[Region=="Mwanza",.(Market, Latitude, Longitude)])
unique(prices[Region=="Mara",.(Market, Latitude, Longitude)])
unique(prices[Region=="Ruvuma",.(Market, Latitude, Longitude)])
unique(prices[Region=="Shinyanga",.(Market, Latitude, Longitude)])
unique(prices[Region=="Kilimanjaro",.(Market, Latitude, Longitude)])
unique(prices[Region=="Mbeya", .(Market, Latitude, Longitude)])
unique(prices[Region=="Katavi",.(Market, Latitude, Longitude)])
unique(prices[Region=="Njombe",.(Market, Latitude, Longitude)])
unique(prices[Region=="Lindi",.(Market, Latitude, Longitude)])
unique(prices[Region=="Singida",.(Market, Latitude, Longitude)])
unique(prices[Region=="Pwani",.(Market, Latitude, Longitude)])
unique(prices[Region=="Simiyu",.(Market, Latitude, Longitude)])
unique(prices[Region=="Geita",.(Market, Latitude, Longitude)])
unique(prices[Region=="Songwe",.(Market, Latitude, Longitude)])


setnames(prices, old = "Maize..min.price.", new = "mai.price.min")
setnames(prices, old = "Rice..min.price.", new = "ric.price.min")
setnames(prices, old = "Sorghum..min.price.", new = "sor.price.min")
setnames(prices, old = "Bulrush.Millet..min.price.", new = "bul.price.min")
setnames(prices, old = "Finger.Millet..min.price.", new = "fin.price.min")
setnames(prices, old = "Wheat..min.price.", new = "whe.price.min")
setnames(prices, old = "Beans..min.price.", new = "bea.price.min")
setnames(prices, old = "Irish.Potatoes..min.price.", new = "pot.price.min")
setnames(prices, old = "Maize..max.price.", new = "mai.price.max")
setnames(prices, old = "Rice..max.price.", new = "ric.price.max")
setnames(prices, old = "Sorghum..max.price.", new = "sor.price.max")
setnames(prices, old = "Bulrush.Millet..max.price.", new = "bul.price.max")
setnames(prices, old = "Finger.Millet..max.price.", new = "fin.price.max")
setnames(prices, old = "Wheat..max.price.", new = "whe.price.max")
setnames(prices, old = "Beans..max.price.", new = "bea.price.max")
setnames(prices, old = "Irish.Potatoes..max.price.", new = "pot.price.max")

sapply(prices, class)

#convert prices to numeric 
prices$mai.price.min <- as.numeric(prices$mai.price.min)
prices$ric.price.min <- as.numeric(prices$ric.price.min)
prices$sor.price.min <- as.numeric(prices$sor.price.min)
prices$bul.price.min <- as.numeric(prices$bul.price.min)
prices$fin.price.min <- as.numeric(prices$fin.price.min)
prices$whe.price.min <- as.numeric(prices$whe.price.min)
prices$bea.price.min <- as.numeric(prices$bea.price.min)
prices$pot.price.min <- as.numeric(prices$pot.price.min)

prices$mai.price.max <- as.numeric(prices$mai.price.max)
prices$ric.price.max <- as.numeric(prices$ric.price.max)
prices$sor.price.max <- as.numeric(prices$sor.price.max)
prices$bul.price.max <- as.numeric(prices$bul.price.max)
prices$fin.price.max <- as.numeric(prices$fin.price.max)
prices$whe.price.max <- as.numeric(prices$whe.price.max)
prices$bea.price.max <- as.numeric(prices$bea.price.max)
prices$pot.price.max <- as.numeric(prices$pot.price.max)

sapply(prices, class)


# convert to price per kg
prices$mai.price.min <- prices$mai.price.min/100
prices$ric.price.min <- prices$ric.price.min/100
prices$sor.price.min <- prices$sor.price.min/100
prices$bul.price.min <- prices$bul.price.min/100
prices$fin.price.min <- prices$fin.price.min/100
prices$whe.price.min <- prices$whe.price.min/100
prices$bea.price.min <- prices$bea.price.min/100
prices$pot.price.min <- prices$pot.price.min/100

prices$mai.price.max <- prices$mai.price.max/100
prices$ric.price.max <- prices$ric.price.max/100
prices$sor.price.max <- prices$sor.price.max/100
prices$bul.price.max <- prices$bul.price.max/100
prices$fin.price.max <- prices$fin.price.max/100
prices$whe.price.max <- prices$whe.price.max/100
prices$bea.price.max <- prices$bea.price.max/100
prices$pot.price.max <- prices$pot.price.max/100

# calculate average of min and max
prices$mai.price <- (prices$mai.price.min + prices$mai.price.max) / 2
prices$ric.price <- (prices$ric.price.min + prices$ric.price.max) / 2
prices$sor.price <- (prices$sor.price.min + prices$sor.price.max) / 2
prices$bul.price <- (prices$bul.price.min + prices$bul.price.max) / 2
prices$fin.price <- (prices$fin.price.min + prices$fin.price.max) / 2
prices$whe.price <- (prices$whe.price.min + prices$whe.price.max) / 2
prices$bea.price <- (prices$bea.price.min + prices$bea.price.max) / 2
prices$pot.price <- (prices$pot.price.min + prices$pot.price.max) / 2


#We can add dates by using the year and the month names
prices$Day   <- day(prices$Date)
prices$Month <- month(prices$Date)
prices$Year  <- year(prices$Date)


# drop unneccessary columns
prices <- prices[,!c("mai.price.min", "mai.price.max",
                     "ric.price.min", "ric.price.max",
                     "sor.price.min", "sor.price.max",
                     "bul.price.min", "bul.price.max",
                     "fin.price.min", "fin.price.max",
                     "whe.price.min", "whe.price.max", 
                     "bea.price.min", "bea.price.max", 
                     "pot.price.min", "pot.price.max")]



# calculate monthly mean prices by market 
prices.monthly <- prices[, .(mai.price = mean(mai.price, na.rm = TRUE), 
           ric.price = mean(ric.price, na.rm = TRUE), 
           sor.price = mean(sor.price, na.rm = TRUE), 
           bul.price = mean(bul.price, na.rm = TRUE),
           fin.price = mean(fin.price, na.rm = TRUE), 
           whe.price = mean(whe.price, na.rm = TRUE), 
           bea.price = mean(bea.price, na.rm = TRUE), 
           pot.price = mean(pot.price, na.rm = TRUE)), 
       by=.(Region, Market, Month, Year, Latitude, Longitude)]


# reshape to long (so that prices for different commodities can be simultaneously estimated)
prices.monthly 
prices.monthly.long <- melt(prices.monthly, id.vars=c('Region', 'Market', 'Month', 'Year', 'Latitude', 'Longitude'),)

# rename columns
setnames(prices.monthly.long, old="variable", new="Crop")
setnames(prices.monthly.long, old="value", new="pkg")

# replace crop names
prices.monthly.long[Crop == "mai.price", Crop := "Maize"]
prices.monthly.long[Crop == "ric.price", Crop := "Rice"]
prices.monthly.long[Crop == "sor.price", Crop := "Sorghum"]
prices.monthly.long[Crop == "bul.price", Crop := "B.Millet"]
prices.monthly.long[Crop == "fin.price", Crop := "F.Millet"]
prices.monthly.long[Crop == "whe.price", Crop := "Wheat"]
prices.monthly.long[Crop == "bea.price", Crop := "Beans"]
prices.monthly.long[Crop == "pot.price", Crop := "Potato"]

#prices.monthly.long <- prices.monthly.long %>%
#  filter(Crop != "Potato")

# Reset the factor levels to updated levels
prices.monthly.long[, Crop := factor(Crop)]
# Check the unique values again
unique(prices.monthly.long$Crop)

# generate dummies to use in place of factors (for later spatial predictions, which are struggling with factors)
prices.monthly.long[, maize   := ifelse(Crop == "Maize",1,0)]
prices.monthly.long[, rice    := ifelse(Crop == "Rice",1,0)]
prices.monthly.long[, sorghum := ifelse(Crop == "Sorghum",1,0)]
prices.monthly.long[, bmillet := ifelse(Crop == "B.Millet",1,0)]
prices.monthly.long[, fmillet := ifelse(Crop == "F.Millet",1,0)]
prices.monthly.long[, wheat   := ifelse(Crop == "Wheat",1,0)]
prices.monthly.long[, beans   := ifelse(Crop == "Beans",1,0)]
prices.monthly.long[, potato  := ifelse(Crop == "Potato",1,0)]

prices.monthly.long[, jan := ifelse(Month == 1  , 1, 0)]
prices.monthly.long[, feb := ifelse(Month == 2  , 1, 0)]
prices.monthly.long[, mar := ifelse(Month == 3  , 1, 0)]
prices.monthly.long[, apr := ifelse(Month == 4  , 1, 0)]
prices.monthly.long[, may := ifelse(Month == 5  , 1, 0)]
prices.monthly.long[, jun := ifelse(Month == 6  , 1, 0)]
prices.monthly.long[, jul := ifelse(Month == 7  , 1, 0)]
prices.monthly.long[, aug := ifelse(Month == 8  , 1, 0)]
prices.monthly.long[, sep := ifelse(Month == 9  , 1, 0)]
prices.monthly.long[, oct := ifelse(Month == 10 , 1, 0)]
prices.monthly.long[, nov := ifelse(Month == 11 , 1, 0)]
prices.monthly.long[, dec := ifelse(Month == 12 , 1, 0)]


# replace NaN with NAs in the price observations
prices.monthly.long[is.nan(pkg), pkg := NA]
# Remove observations with missing observations
prices.monthly.long <- na.omit(prices.monthly.long)

# bring in raster stack as predictors
geodata_path("H:/Tanzania Price data/Datasets/geodata")
list.files("H:/Tanzania Price data/Datasets/geodata", recursive=TRUE)


# tza0 <- gadm(country="TZA", level=0)
# tza1 <- gadm(country="TZA", level=1)
# tza2 <- gadm(country="TZA", level=2)
# tza3 <- gadm(country="TZA", level=3)

tza0 <- readRDS("H:/Tanzania Price data/Datasets/geodata/TRUE/gadm/gadm41_TZA_0_pk.rds")
tza1 <- readRDS("H:/Tanzania Price data/Datasets/geodata/TRUE/gadm/gadm41_TZA_1_pk.rds")
tza2 <- readRDS("H:/Tanzania Price data/Datasets/geodata/TRUE/gadm/gadm41_TZA_2_pk.rds")
tza3 <- readRDS("H:/Tanzania Price data/Datasets/geodata/TRUE/gadm/gadm41_TZA_3_pk.rds")


# convert prices observations to vector for mapping
mypts <- vect(prices.monthly.long, geom=c("Longitude", "Latitude"), crs=crs(tza0), keepgeom=TRUE)
mkt_pts <- unique(mypts[, c("Market", "Latitude", "Longitude")])

# see if these show up correctly
plot(tza1)
plot(mkt_pts, col="Red", add=TRUE)
# text(mypts, label="Market")

# create reference raster
tza_extent <- ext(tza1) |> floor()
r <- crop(rast(res=1/12), tza_extent)


## Interpolate
#xy <- as.matrix(mypts[,c("Longitude", "Latitude")])
xy <- geom(mypts)[,c("y","x")]
#tps <- Tps(xy, p$spatial)
tps <- Tps(xy, mypts$pkg)
sp <- interpolate(r, tps)
sp <- mask(sp, tza1)
plot(sp)
lines(tza1)


## Predict Maize prices with coodinates only
maize_mypts <- mypts[mypts$Crop == "Maize", ]
rf <- randomForest(pkg ~ Longitude + Latitude, data=maize_mypts)
sp3 <- interpolate(r, rf, xyNames=c("Longitude", "Latitude"))
sp3 <- mask(sp3, tza1)
plot(sp3)
lines(tza1)


## Covariates
ttcity <- rast("H:/Tanzania Price data/Datasets/geodata/TRUE/travel/travel_time_to_cities_u5.tif")
ttport <- rast("H:/Tanzania Price data/Datasets/geodata/TRUE/travel/travel_time_to_ports_1.tif")
clm    <- rast("H:/Tanzania Price data/Datasets/geodata/TRUE/wc2.1_country/TZA_wc2.1_30s_bio.tif")
area   <- rast("H:/Tanzania Price data/Datasets/geodata/TRUE/spam/spam2017V2r1_SSA_H_MAIZ_A.tif")
yield  <- rast("H:/Tanzania Price data/Datasets/geodata/TRUE/spam/spam2017V2r1_SSA_Y_MAIZ_R.tif")
popd  <- rast("gpw_v4_population_density_rev11_2020_10m.tif")


names(ttcity) <- c("ttcity_u5") ## travel time cities of 100k or more
names(ttport) <- c("ttport_1") ## travel time to major ports
names(clm) <- gsub("wc2.1_30s_", "", names(clm))
names(area) <- c("MAI_ARE") # SPAM maize area 2010
names(yield)  <- c("MAI_YLD") # SPAM maize yield 2010
names(popd)  <- c("popdens") # GPW4

comment(ttcity) <- "travel time to cities 100k or more"
comment(ttport) <- "travel time to major ports"

comment(popd) <- "population density 2020 (GPW4 @ 10dm)"

comment(clm)[1] <-"BIO1 = Annual Mean Temperature"
comment(clm)[2] <-"BIO2 = Mean Diurnal Range (Mean of monthly (max temp - min temp))"
comment(clm)[3] <-"BIO3 = Isothermality (BIO2/BIO7) (×100)"
comment(clm)[4] <-"BIO4 = Temperature Seasonality (standard deviation ×100)"
comment(clm)[5] <-"BIO5 = Max Temperature of Warmest Month"
comment(clm)[6] <-"BIO6 = Min Temperature of Coldest Month"
comment(clm)[7] <-"BIO7 = Temperature Annual Range (BIO5-BIO6)"
comment(clm)[8] <-"BIO8 = Mean Temperature of Wettest Quarter"
comment(clm)[9] <-"BIO9 = Mean Temperature of Driest Quarter"
comment(clm)[10] <-"BIO10 = Mean Temperature of Warmest Quarter"
comment(clm)[11] <-"BIO11 = Mean Temperature of Coldest Quarter"
comment(clm)[12] <-"BIO12 = Annual Precipitation"
comment(clm)[13] <-"BIO13 = Precipitation of Wettest Month"
comment(clm)[14] <-"BIO14 = Precipitation of Driest Month"
comment(clm)[15] <-"BIO15 = Precipitation Seasonality (Coefficient of Variation)"
comment(clm)[16] <-"BIO16 = Precipitation of Wettest Quarter"
comment(clm)[17] <-"BIO17 = Precipitation of Driest Quarter"
comment(clm)[18] <-"BIO18 = Precipitation of Warmest Quarter"
comment(clm)[19] <-"BIO19 = Precipitation of Coldest Quarter"


## Harmonize rasters to national boundaries and common resolution
ttcity <- resample(ttcity, r)
ttport <- resample(ttport, r)
clm    <- resample(clm, r)
area   <- resample(area, r)
popd   <- resample(popd, r)
freq(is.na(area))
area <- classify(area, cbind(NA,0)) 
yield  <- resample(yield, r)
freq(is.na(yield))
yield <- classify(yield, cbind(NA,0)) 
# check again 
compareGeom(ttcity, ttport, clm, area, yield, popd)


## Generate Latitude and Longitude grid
latgrd <- longrd <- r
latgrd[] <- yFromCell(latgrd, 1:ncell(latgrd))
longrd[] <- xFromCell(longrd, 1:ncell(longrd))
names(latgrd) <- c("latitude")
names(longrd) <- c("longitude")

## Prepare Predictor Stack
rstack <- c(ttcity, ttport, clm, area, yield, popd, latgrd, longrd)
names(rstack)


# create focal mean to extract from (as alternative to using buffers for extraction, which are not supported in terra)
fm <- focalMat(r, d=0.18, type='circle', fillNA=FALSE)
rstack2 <- focal(rstack, w=fm, fun="mean", na.policy="all", fillvalue=NA, # na.rm=TRUE,
                 expand=TRUE, silent=FALSE) #, filename="", overwrite=FALSE) 


# extract values to dataset -- use a 20km buffer
# do a focal sum of 20km radius  - this is about 0.18 of a decimal degree... 0.18*112=20.16
fm <- focalMat(r, d=0.18, type='circle', fillNA=FALSE)
rstack2 <- focal(rstack2, w=fm, fun="sum", na.policy="all", fillvalue=NA, na.rm=TRUE,
                 expand=TRUE, silent=FALSE) #, filename="", overwrite=FALSE) 


#Calculate sum of 6 month Lag Rainfall
# Bring in chirps data
chirps_path <- "H:/Tanzania Price data/chirps_data"

chirps_files <- list.files(chirps_path, pattern = ".tif$", full.names = TRUE)

# Read all CHIRPS data files into a SpatRaster collection
chirps_rasters <- rast(chirps_files)

#crop to Tanzania boundary
Chirps_Tz <- crop(chirps_rasters, tza1)

writeRaster(Chirps_Tz, "Tz_chirps_monthly_croped.tif", overwrite=TRUE)

Tz_chirps_monthly <- terra::rast("Tz_chirps_monthly_croped.tif")
Tz_chirps_monthly

#Replace -9999 with NA
Tz_chirps_monthly <- classify(Tz_chirps_monthly, cbind(-9999,NA))

#Replace -9999 with NA
#Tz_chirps_monthly[Tz_chirps_monthly == -9999.000] <- NA

#extract layer names
layer_names <- names(Tz_chirps_monthly)
layer_names

# We need to create a sequence of dates from the layer names
# Extract year and month from layer names and convert to Date
dates <- as.Date(paste0(sub("chirps-v2.0\\.", "", layer_names), "-01"), format = "%Y.%m-%d")
dates

# Assign these dates to the SpatRaster object
time(Tz_chirps_monthly) <- dates

#rename the layers to the formatted dates
names(Tz_chirps_monthly) <- dates

# Check the SpatRaster object
print(Tz_chirps_monthly)

# do a focal mean of 100km radius - this is about 0.9 of a decimal degree... 0.9009*112=100.9008
# Calculate the focal mean for each layer (month)
fm_r <- focalMat(Tz_chirps_monthly, d=0.9, type='circle', fillNA=FALSE)
Rainfall_focal_sum_100km <- focal(Tz_chirps_monthly, w=fm_r, fun="mean", na.policy="all", fillvalue=NA, na.rm=TRUE,
                                  expand=TRUE, silent=FALSE)
# Check the result
Rainfall_focal_sum_100km
Rainfall <- Rainfall_focal_sum_100km

Rainfall_res <- resample(Rainfall, r)
Rainfall_res
plot(Rainfall_res)

#---------------------------------------------------------------------------
# Define the function to calculate the 6-month lagged sum
calculate_lagged_sum <- function(raster_stack, num_months = 6) {
  # Get the time vector from the raster stack
  time_vector <- time(raster_stack)
  
  # Initialize list to store lagged sum rasters
  lagged_sum_rasters <- vector("list", length(time_vector))
  
  # Loop through each layer in the raster stack
  for (i in seq_along(time_vector)) {
    if (i > num_months) {  # We need at least 'num_months' previous layers to calculate the lagged sum
      # Determine the start and end dates for the lag period
      end_date <- time_vector[i] # Date of the current layer being processed
      start_date <- end_date %m-% months(num_months) #The date num_months before the end_date
      
      # Select the layers that fall within the lag period
      lag_period_layers <- raster_stack[[which(time_vector > start_date & time_vector <= end_date)]]
      
      # Calculate the sum of the selected layers
      if (nlyr(lag_period_layers) == num_months) {
        lagged_sum_rasters[[i]] <- sum(lag_period_layers, na.rm = TRUE)
      } else {
        lagged_sum_rasters[[i]] <- rast(nrow = nrow(raster_stack), ncol = ncol(raster_stack), 
                                        crs = crs(raster_stack), ext = ext(raster_stack), 
                                        vals = NA)  # Use an empty raster with NA values
      }
    } else {
      lagged_sum_rasters[[i]] <- rast(nrow = nrow(raster_stack), ncol = ncol(raster_stack), 
                                      crs = crs(raster_stack), ext = ext(raster_stack), 
                                      vals = NA)  # Use an empty raster with NA values
    }
  }
  
  # Combine the lagged sum rasters into a single raster stack, excluding empty rasters
  lagged_sum_stack <- rast(lagged_sum_rasters)
  
  # Set the names for the layers in the lagged sum stack
  names(lagged_sum_stack) <- names(raster_stack)[!is.na(lagged_sum_rasters)]
  
  return(lagged_sum_stack)
}

# Calculate the 6-month lagged sum for each period in the raster stack
lagged_rainfall_sum <- calculate_lagged_sum(Rainfall_res, num_months = 6)

# Remove the first 6 layers from the raster stack since they are empty
lagged_rainfall_sum_filtered <- lagged_rainfall_sum[[7:nlyr(lagged_rainfall_sum)]]
# check the result
print(lagged_rainfall_sum_filtered)

names(lagged_rainfall_sum_filtered) <- paste0(names(lagged_rainfall_sum_filtered), "_rain.sum.lag")

plot(lagged_rainfall_sum_filtered)


## We'll have to include lagged_rainfall_sum_filtered in the predictor stack.
rstack
names(rstack)
rstack3 <- c(rstack, lagged_rainfall_sum_filtered)
names(rstack3)

#Extract to the point dataset
extr1 <- terra::extract(rstack3, mypts, method = "bilinear")

mypts <- cbind(mypts, extr1)
# Remove the ID column from the dataset
mypts <- mypts[, !names(mypts) %in% "ID"]
head(mypts)

mypts_df <- as.data.frame(mypts)

# Define the function to obtain sum of lag rainfall from corresponding rasters to mypts under rain.sum.lag column (for each row)
# Each extraction has to match the month and year
get_rain_sum_row <- function(current_date, mypts_row) {
  # Extract the rainfall value for the current date
  rain_sum <- mypts_row[[paste0(current_date, "_rain.sum.lag")]]
  return(rain_sum)
}

# Loop through each row and obtain the rainfall sum for each month and year
for (i in 1:nrow(mypts_df)) {
  # Extract relevant data for the current row
  month <- mypts_df$Month[i]
  year <- mypts_df$Year[i]
  current_date <- paste0(year, "-", sprintf("%02d", month), "-01")  # Format date correctly
  # Pass data to the function
  rain_sum <- get_rain_sum_row(current_date, mypts_df[i, ])
  # Update the rain.sum.lag column
  mypts_df$rain.sum.lag[i] <- rain_sum
}

# Update the SpatVector with the new rain.avg column
mypts$rain.sum.lag <- mypts_df$rain.sum.lag
names(mypts)

# I'll drop the dates with rain.sum.lag from mypts, seems redundant
column_indices <- grep("^202[0-4]-", names(mypts))
mypts <- mypts[, -column_indices]
names(mypts)

#----------------------------------------------------------------------------------------------------------------------------------------------------------
#define Month as a factor
#mypts$Month <- as.factor(mypts$Month)
#levels(mypts$Month)

#We'll define month as an interger instead.
# Check to make sure Month is interger
sapply(mypts, class)

# drop levels that don't exist in Crop field
mypts$Crop <- mypts$Crop[,drop=TRUE]
levels(mypts$Crop)


# Linear model price Prediction
# Fit the linear model
lm_model <- lm(pkg ~ maize + rice + sorghum + bmillet + fmillet + wheat + beans +
                 Month +
                 Year + 
                 ttcity_u5 + ttport_1 + 
                 bio_1 + bio_2 + bio_3 + bio_4 + bio_5 + bio_6 + 
                 bio_7 + bio_8 + bio_9 + bio_10 + bio_11 + bio_12 + bio_13 + bio_14 + bio_15 +
                 bio_16 + bio_17 + bio_18 + bio_19 + 
                 MAI_ARE + MAI_YLD + 
                 Longitude + Latitude + 
                 rain.sum.lag,
               data = mypts)
# Extract and print the coefficients
summary(lm_model)


# RandomForest and TPS-------------------------------------------------------------------------------------------------
## Random Forest price prediction
for(column in seq_along(mypts)){
  if(any(is.na(mypts[column]))){
    print(paste0("Column: ", colnames(mypts)[column], " has at least one NA value"))
  }
}
#There are no columns with missing values


## Split data the to be used for Training and validation 
# Filter the data for training (May 2021 - Dec 2023)
training_data <- mypts[mypts$Year %in% c(2021, 2022, 2023), ]
# Check training data
head(training_data)
training_data <- as.data.frame(training_data)

# Filter the data for validation (Jan 2024 - June 2024)
validation_data <- mypts[mypts$Year == 2024, ]
# Check validation data
head(validation_data)
validation_data <- as.data.frame(validation_data)

### Random Forest for generating variable of importance
### Tune The Forest
# Convert training_data data to data frame
mypts_df <- as.data.frame(training_data)

trf <- tuneRF(x=mypts_df[,1:ncol(mypts_df)], # Prediction variables
              y=mypts_df$pkg) # Response variable


(mintree <- trf[which.min(trf[,2]),1])


### Fit The Random Forest Model (1)
#### Random Forest for generating variable of importance
# Create the random forest model
rf1 <- randomForest(pkg ~ maize + rice + sorghum + bmillet + fmillet + wheat + beans +
                      Month + 
                      Year +
                      ttcity_u5 + ttport_1 + 
                      bio_1 + bio_2 + bio_3 + bio_4 + bio_5 + bio_6 + 
                      bio_7 + bio_8 + bio_9 + bio_10 + bio_11 + bio_12 + bio_13 + bio_14 + bio_15 +
                      bio_16 + bio_17 + bio_18 + bio_19 + 
                      MAI_ARE + MAI_YLD + 
                      Longitude + Latitude + 
                      rain.sum.lag,
                    data = training_data, mtry=mintree, importance=TRUE, na.rm=TRUE)

rf1

varImpPlot(rf1)

## evaluate
(oob <- sqrt(rf1$mse[which.min(rf1$mse)]))


importance_metrics <- importance(rf1, type=1)  # %IncMSE
impvar <- rownames(importance_metrics)[order(importance_metrics[, 1], decreasing=TRUE)]
impvar
# Get the top 20 variables
top_20_vars <- impvar[1:20]
top_20_vars

node_purity <- importance(rf1, type=2)  # IncNodePurity
# Sort variables by importance (IncNodePurity)
node_purity_sorted <- sort(node_purity[,1], decreasing = TRUE)
node_purity_sorted
# Select the top 20 important variables
top_vars <- names(node_purity_sorted)[1:20]
print(top_vars)

rf1$importanceSD


### Fit The Random Forest Model (2)
#### Estimate more parsimonious specification
# Estimate more parsimonious specification
rf <- randomForest(pkg ~ maize + rice + sorghum + bmillet + fmillet + wheat + beans +
                     Month +
                     Year + 
                     ttport_1 +
                     popdens +
                     bio_3 + bio_6 + bio_9 +  bio_12 + bio_18 + 
                     rain.sum.lag, 
                   data=training_data, na.rm=TRUE)

rf

# evaluate
varImpPlot(rf)

(oob <- sqrt(rf$mse[which.min(rf$mse)]))

partialPlot(rf, as.data.frame(training_data), "rain.sum.lag")

## spatial prediction
# Define months and year for prediction
# try for 1 month
# note: we must set the rain.sum.lag variables 
#names(rstack3)
#rain.sum.lag <- rstack3["2023-03-01_rain.sum.lag"]
#names(rain.sum.lag) <- c("rain.sum.lag")
#newstack <- c(rstack, rain.sum.lag)
#names(newstack)
#pred1 <- predict(newstack, rf, 
#                 const=data.frame(maize=1, rice=0, sorghum=0, bmillet=0, fmillet=0, wheat=0, beans=0, potato=0,
#                                  Month=3,
#                                  Year=2023), 
#                 na.rm=TRUE)
#pred1 <- mask(pred1, tza1)
#plot(pred1)
#plot(mypts, add=TRUE, pch=20, col="Red")
#text(mypts, labels="Region", pos=1)


### Predicted Maize Prices
#Maize------------------------------------------------------------------------------------
# note: we must set the rain.sum.lag variables for each month
# we'll define a fuction to create prediction for a given month
year =2024

predict_for_month <- function(month){
  rain_sum_lag <- rstack3[paste0("2024-", sprintf("%02d", month), "-01_rain.sum.lag")] # Remember to change depending on year
  names(rain_sum_lag) <- "rain.sum.lag"
  
  newstack <- c(rstack, rain_sum_lag)
  
  const_maize <- data.frame(
    maize = 1, rice = 0, sorghum = 0, bmillet = 0, fmillet = 0, wheat = 0, beans = 0, potato = 0,
    Month = month,
    Year = year
  )
  
  pred <- predict(newstack, rf, const = const_maize, na.rm = TRUE)
  pred <- mask(pred, tza1)
  return(pred)
}

# Create predictions for all months
predictions_maize <- lapply(1:8, predict_for_month)

# Extract pixel values from predictions_maize
maize_values <- unlist(lapply(predictions_maize, values))
# Get min and max values
min_maize <- min(maize_values, na.rm = TRUE)
max_maize <- max(maize_values, na.rm = TRUE)

# Define the continuous color palette and reverse it
color_palette <- rev(terrain.colors(100))

# Set up plot layout with an extra row for the legend
layout_matrix <- matrix(c(1, 2, 3, 4,
                          5, 6, 7, 8,
                          9, 9, 9, 9), nrow = 3, byrow = TRUE)

# Set up layout
layout(layout_matrix, heights = c(1, 1, 0.5))

break_interval <- 150 

# Create a 3x4 matrix of plots
par(mar = c(0, 0, 0, 0))  # Set margins to 0 for inner plots
for (i in 1:8) {
  plot(predictions_maize[[i]], main = paste(month.name[i], year),
       zlim = c(min_maize, max_maize), col = color_palette, breaks = seq(min_maize, max_maize, by = break_interval), legend = FALSE, axes = FALSE)
  
  # Add region boundaries
  plot(tza1, add = TRUE, border = "dimgray", lwd = 0.1)
  
  #points(mkt_pts, pch = 19, col = "red", cex = 0.9)
}

# Generate pretty breaks for the legend
legend_breaks <- pretty(c(min_maize, max_maize), n = 4)

# Reset plot layout for the legend
layout(matrix(1))
par(mar = c(5, 4, 2, 1))

# Plot the legend in the bottom row, centered horizontally
image.plot(zlim = c(min_maize, max_maize), legend.only = TRUE,
           col = color_palette, horizontal = TRUE,
           legend.width = 0.7, legend.shrink = 0.9,
           axis.args = list(at = legend_breaks, labels = legend_breaks, cex.axis = 0.9),
           legend.args = list(text = "Predicted Maize Price (TZS/Kg)", side = 1, line = 2, cex = 0.9))


output_dir_Maize <- "H:/Tanzania Price data/Datasets/Pred-plots/Maize"

for (i in 1:length(predictions_maize)) {
  month_str <- sprintf("%02d", i)
  output_filename <- paste0("maize_price_rf_pred_", month_str, ".tif")
  output_path <- file.path(output_dir, output_filename)
  writeRaster(predictions_maize[[i]], output_path, overwrite = TRUE)
}

### Predicted Beans Prices --------------------------------------------------------
# Beans
# Function to predict beans for a given month
predict_for_beans <- function(month) {
  rain_sum_lag <- rstack3[paste0("2024-", sprintf("%02d", month), "-01_rain.sum.lag")]
  names(rain_sum_lag) <- "rain.sum.lag"

  newstack <- c(rstack, rain_sum_lag)

  const_beans <- data.frame(
    maize = 0, rice = 0, sorghum = 0, bmillet = 0, fmillet = 0, wheat = 0, beans = 1, potato = 0,
    Month = month,
    Year = year
  )

  pred <- predict(newstack, rf, const = const_beans, na.rm = TRUE)
  pred <- mask(pred, tza1)
  return(pred)
}

# Create predictions for all months
predictions_beans <- lapply(1:8, predict_for_beans)

# Extract pixel values from predictions_beans
bean_values <- unlist(lapply(predictions_beans, values))
min_bean <- min(bean_values, na.rm = TRUE)
max_bean <- max(bean_values, na.rm = TRUE)

# Define the continuous color palette and reverse it
color_palette <- rev(terrain.colors(100))

# Set up plot layout with an extra row for the legend
layout_matrix <- matrix(c(1, 2, 3, 4,
                          5, 6, 7, 8,
                          9, 9, 9, 9), nrow = 3, byrow = TRUE)

# Set up layout
layout(layout_matrix, heights = c(1, 1, 0.5))

break_interval <- 210 

# Loop through each month to plot beans prices
par(mar = c(0, 0, 0, 0))
for (i in 1:8) {
  plot(predictions_beans[[i]], main = paste(month.name[i], year),
       zlim = c(min_bean, max_bean), col = color_palette, breaks = seq(min_bean, max_bean, by = break_interval), legend = FALSE, axes = FALSE)
  
  plot(tza1, add = TRUE, border = "dimgray", lwd = 0.1)
  
  points(training_data, pch = 20, col = "red", cex = 0.5)
}

# Generate pretty breaks for the legend
legend_breaks <- pretty(c(min_bean, max_bean), n = 4)

# Reset plot layout to 1x1 for the legend
layout(matrix(1))  

# Set margins for the legend
par(mar = c(5, 4, 2, 1))  

# Plot the legend in the bottom row, centered horizontally
image.plot(zlim = c(min_bean, max_bean), legend.only = TRUE, 
           col = color_palette, horizontal = TRUE,
           legend.width = 0.7, legend.shrink = 0.9, 
           axis.args = list(at = legend_breaks, labels = legend_breaks, cex.axis = 0.9), 
           legend.args = list(text = "Predicted Beans Price (TZS/Kg)", side = 1, line = 2, cex = 0.9))


for (i in 1:length(predictions_beans)) {
  month_str <- sprintf("%02d", i)
  output_filename <- paste0("beans_price_rf_pred_", month_str, ".tif")
  output_path <- file.path(output_dir, output_filename)
  writeRaster(predictions_beans[[i]], output_path, overwrite = TRUE)
}

### Predicted Rice Prices----------------------------------------------------------------------------------------------------
# Rice
# Function to predict rice prices for a given month
predict_for_rice <- function(month) {
  rain_sum_lag <- rstack3[paste0("2024-", sprintf("%02d", month), "-01_rain.sum.lag")]
  names(rain_sum_lag) <- "rain.sum.lag"
  
  newstack <- c(rstack, rain_sum_lag)
  
  const_rice <- data.frame(
    maize = 0, rice = 1, sorghum = 0, bmillet = 0, fmillet = 0, wheat = 0, beans = 0, potato = 0,
    Month = month,
    Year = year
  )
  
  pred <- predict(newstack, rf, const = const_rice, na.rm = TRUE)
  pred <- mask(pred, tza1)
  return(pred)
}

# Create predictions for all months
predictions_rice <- lapply(1:8, predict_for_rice)

# Extract pixel values from predictions_rice
rice_values <- unlist(lapply(predictions_rice, values))
min_rice <- min(rice_values, na.rm = TRUE)
max_rice <- max(rice_values, na.rm = TRUE)

# Define the continuous color palette and reverse it
color_palette <- rev(terrain.colors(100))

layout_matrix <- matrix(c(1, 2, 3, 4,
                          5, 6, 7, 8,
                          9, 9, 9, 9), nrow = 3, byrow = TRUE)

# Set up layout
layout(layout_matrix, heights = c(1, 1, 0.5))

break_interval <- 150 

# Loop through each month to plot rice prices
par(mar = c(0, 0, 0, 0))
for (i in 1:8) {
  plot(predictions_rice[[i]], main = paste(month.name[i], year),
       zlim = c(min_rice, max_rice), col = color_palette, breaks = seq(min_rice, max_rice, by = break_interval), legend = FALSE, axes = FALSE)
  
  plot(tza1, add = TRUE, border = "dimgray", lwd = 0.1)
  
  points(training_data, pch = 20, col = "red", cex = 0.5)
}

# Generate pretty breaks for the legend
legend_breaks <- pretty(c(min_rice, max_rice), n = 5)

# Reset plot layout to 1x1 for the legend
layout(matrix(1)) 

# Set margins for the legend
par(mar = c(5, 4, 2, 1))  

# Plot the legend in the bottom row, centered horizontally
image.plot(zlim = c(min_rice, max_rice), legend.only = TRUE, 
           col = color_palette, horizontal = TRUE,
           legend.width = 0.7, legend.shrink = 0.9, 
           axis.args = list(at = legend_breaks, labels = legend_breaks, cex.axis = 0.9), 
           legend.args = list(text = "Predicted Rice Price (TZS/Kg)", side = 1, line = 2, cex = 0.9))

for (i in 1:length(predictions_rice)) {
  month_str <- sprintf("%02d", i)
  output_filename <- paste0("rice_price_rf_pred_", month_str, ".tif")
  output_path <- file.path(output_dir, output_filename)
  writeRaster(predictions_rice[[i]], output_path, overwrite = TRUE)
}

### Predicted Sorghum Prices---------------------------------------------------------------------------------------------------
# sorghum
# Function to predict sorghum prices for a given month
predict_for_sorghum <- function(month) {
  rain_sum_lag <- rstack3[paste0("2024-", sprintf("%02d", month), "-01_rain.sum.lag")]
  names(rain_sum_lag) <- "rain.sum.lag"
  
  newstack <- c(rstack, rain_sum_lag)
  
  const_sorghum <- data.frame(
    maize = 0, rice = 0, sorghum = 1, bmillet = 0, fmillet = 0, wheat = 0, beans = 0, potato = 0,
    Month = month,
    Year = 2023
  )
  
  pred <- predict(newstack, rf, const = const_sorghum, na.rm = TRUE)
  pred <- mask(pred, tza1)
  return(pred)
}

# Create predictions for all months
predictions_sorghum <- lapply(1:8, predict_for_sorghum)

# Extract pixel values from predictions_sorghum
sorghum_values <- unlist(lapply(predictions_sorghum, values))
min_sorghum <- min(sorghum_values, na.rm = TRUE)
max_sorghum <- max(sorghum_values, na.rm = TRUE)

# Define the continuous color palette and reverse it
color_palette <- rev(terrain.colors(100))

# Set up plot layout with an extra row for the legend
layout_matrix <- matrix(c(1, 2, 3, 4,
                          5, 6, 7, 8,
                          9, 9, 9, 9), nrow = 3, byrow = TRUE)

# Set up layout
layout(layout_matrix, heights = c(1, 1, 0.5))

break_interval <- 250 

# Loop through each month to plot sorghum prices
par(mar = c(0, 0, 0, 0))
for (i in 1:8) {
  plot(predictions_sorghum[[i]], main = paste(month.name[i], year),
       zlim = c(min_sorghum, max_sorghum), col = color_palette, breaks = seq(min_sorghum, max_sorghum, by = break_interval), legend = FALSE, axes = FALSE)
  
  plot(tza1, add = TRUE, border = "dimgray", lwd = 0.1)
  
  points(training_data, pch = 20, col = "red", cex = 0.5)
}

# Generate pretty breaks for the legend
legend_breaks <- pretty(c(min_sorghum, max_sorghum), n = 5)

# Reset plot layout to 1x1 for the legend
layout(matrix(1))  

# Set margins for the legend
par(mar = c(5, 4, 2, 1))  

# Plot the legend in the bottom row, centered horizontally
image.plot(zlim = c(min_sorghum, max_sorghum), legend.only = TRUE, 
           col = color_palette, horizontal = TRUE,
           legend.width = 0.7, legend.shrink = 0.9, 
           axis.args = list(at = legend_breaks, labels = legend_breaks, cex.axis = 0.9), 
           legend.args = list(text = "Predicted Sorghum Price (TZS/Kg)", side = 1, line = 2, cex = 0.9))

for (i in 1:length(predictions_sorghum)) {
  month_str <- sprintf("%02d", i)
  output_filename <- paste0("sorghum_price_rf_pred_", month_str, ".tif")
  output_path <- file.path(output_dir, output_filename)
  writeRaster(predictions_sorghum[[i]], output_path, overwrite = TRUE)
}

### Predicted Bulrush Millet Prices------------------------------------------------------------------------------------
# bmillet
# Function to predict bmillet prices for a given month
predict_for_bmillet <- function(month) {
  rain_sum_lag <- rstack3[paste0("2024-", sprintf("%02d", month), "-01_rain.sum.lag")]
  names(rain_sum_lag) <- "rain.sum.lag"
  
  newstack <- c(rstack, rain_sum_lag)
  
  const_bmillet <- data.frame(
    maize = 0, rice = 0, sorghum = 0, bmillet = 1, fmillet = 0, wheat = 0, beans = 0, potato = 0,
    Month = month,
    Year = year
  )
  
  pred <- predict(newstack, rf, const = const_bmillet, na.rm = TRUE)
  pred <- mask(pred, tza1)
  return(pred)
}

# Create predictions for all months
predictions_bmillet <- lapply(1:8, predict_for_bmillet)


# Extract pixel values from predictions_bmillet
bmillet_values <- unlist(lapply(predictions_bmillet, values))
min_bmillet <- min(bmillet_values, na.rm = TRUE)
max_bmillet <- max(bmillet_values, na.rm = TRUE)

# Define the continuous color palette and reverse it
color_palette <- rev(terrain.colors(100))

# Set up plot layout with an extra row for the legend
layout_matrix <- matrix(c(1, 2, 3, 4,
                          5, 6, 7, 8,
                          9, 9, 9, 9), nrow = 3, byrow = TRUE)

# Set up layout
layout(layout_matrix, heights = c(1, 1, 0.5))

# Loop through each month to plot bmillet prices
break_interval <- 200 
par(mar = c(0, 0, 0, 0)) 
for (i in 1:8) {
  plot(predictions_bmillet[[i]], main = paste(month.name[i], year),
       zlim = c(min_bmillet, max_bmillet), col = color_palette, 
       breaks = seq(min_bmillet, max_bmillet, by = break_interval), 
       legend = FALSE, axes = FALSE)
  
  plot(tza1, add = TRUE, border = "dimgray", lwd = 0.1)
  
  #points(training_data, pch = 20, col = "red", cex = 0.5)
}

# Generate pretty breaks for the legend
legend_breaks <- pretty(c(min_bmillet, max_bmillet), n = 5) 

# Reset plot layout to 1x1 for the legend
layout(matrix(1))  

# Set margins for the legend
par(mar = c(5, 4, 2, 1))

# Plot the legend in the bottom row, centered horizontally
image.plot(zlim = c(min_bmillet, max_bmillet), legend.only = TRUE, 
           col = color_palette, horizontal = TRUE,
           legend.width = 0.7, legend.shrink = 0.9, 
           axis.args = list(at = legend_breaks, labels = legend_breaks, cex.axis = 0.9), 
           legend.args = list(text = "Predicted Bulrush Millet Price (TZS/kg)", side = 1, line = 2, cex = 0.9))

for (i in 1:length(predictions_bmillet)) {
  month_str <- sprintf("%02d", i)
  output_filename <- paste0("bmillet_price_rf_pred_", month_str, ".tif")
  output_path <- file.path(output_dir, output_filename)
  writeRaster(predictions_bmillet[[i]], output_path, overwrite = TRUE)
}

### Predicted Finger Millet Prices-------------------------------------------------------------------------------------------------
#fmillet
# Function to predict fmillet prices for a given month
predict_for_fmillet <- function(month) {
  rain_sum_lag <- rstack3[paste0("2024-", sprintf("%02d", month), "-01_rain.sum.lag")]
  names(rain_sum_lag) <- "rain.sum.lag"
  
  newstack <- c(rstack, rain_sum_lag)
  
  const_fmillet <- data.frame(
    maize = 0, rice = 0, sorghum = 0, bmillet = 0, fmillet = 1, wheat = 0, beans = 0, potato = 0,
    Month = month,
    Year = year
  )
  
  pred <- predict(newstack, rf, const = const_fmillet, na.rm = TRUE)
  pred <- mask(pred, tza1)
  return(pred)
}

# Create predictions for all months
predictions_fmillet <- lapply(1:8, predict_for_fmillet)

# Extract pixel values from predictions_fmillet
fmillet_values <- unlist(lapply(predictions_fmillet, values))
min_fmillet <- min(fmillet_values, na.rm = TRUE)
max_fmillet <- max(fmillet_values, na.rm = TRUE)

# Define the continuous color palette and reverse it
color_palette <- rev(terrain.colors(100))

# Set up plot layout with an extra row for the legend
layout_matrix <- matrix(c(1, 2, 3, 4,
                          5, 6, 7, 8,
                          9, 9, 9, 9), nrow = 3, byrow = TRUE)

# Set up layout
layout(layout_matrix, heights = c(1, 1, 0.5))

# Loop through each month to plot fmillet prices
break_interval <- 250 
par(mar = c(0, 0, 0, 0))
for (i in 1:8) {
  plot(predictions_fmillet[[i]], main = paste(month.name[i], year),
       zlim = c(min_fmillet, max_fmillet), col = color_palette, 
       breaks = seq(min_fmillet, max_fmillet, by = break_interval), 
       legend = FALSE, axes = FALSE)
  
  plot(tza1, add = TRUE, border = "dimgray", lwd = 0.1)
  
  points(training_data, pch = 20, col = "red", cex = 0.5)
}

# Generate pretty breaks for the legend
legend_breaks <- pretty(c(min_fmillet, max_fmillet), n = 3)  

# Reset plot layout to 1x1 for the legend
layout(matrix(1))  

# Set margins for the legend
par(mar = c(5, 4, 2, 1))  

# Plot the legend in the bottom row, centered horizontally
image.plot(zlim = c(min_fmillet, max_fmillet), legend.only = TRUE, 
           col = color_palette, horizontal = TRUE,
           legend.width = 0.7, legend.shrink = 0.9, 
           axis.args = list(at = legend_breaks, labels = legend_breaks, cex.axis = 0.9), 
           legend.args = list(text = "Predicted Finger Millet Price (TZS/kg)", side = 1, line = 2, cex = 0.9))

for (i in 1:length(predictions_fmillet)) {
  month_str <- sprintf("%02d", i)
  output_filename <- paste0("fmillet_price_rf_pred_", month_str, ".tif")
  output_path <- file.path(output_dir, output_filename)
  writeRaster(predictions_fmillet[[i]], output_path, overwrite = TRUE)
}

### Predicted Wheat Prices---------------------------------------------------------------------------------
#Wheat
# Function to predict wheat prices for a given month
predict_for_wheat <- function(month) {
  rain_sum_lag <- rstack3[paste0("2024-", sprintf("%02d", month), "-01_rain.sum.lag")]
  names(rain_sum_lag) <- "rain.sum.lag"
  
  newstack <- c(rstack, rain_sum_lag)
  
  const_wheat <- data.frame(
    maize = 0, rice = 0, sorghum = 0, bmillet = 0, fmillet = 0, wheat = 1, beans = 0, potato = 0,
    Month = month,
    Year = year
  )
  
  pred <- predict(newstack, rf, const = const_wheat, na.rm = TRUE)
  pred <- mask(pred, tza1)
  return(pred)
}

# Create predictions for all months
predictions_wheat <- lapply(1:8, predict_for_wheat)
# Extract pixel values from predictions_wheat
wheat_values <- unlist(lapply(predictions_wheat, values))
min_wheat <- min(wheat_values, na.rm = TRUE)
max_wheat <- max(wheat_values, na.rm = TRUE)

# Define the continuous color palette and reverse it
color_palette <- rev(terrain.colors(100))

# Set up plot layout with an extra row for the legend
layout_matrix <- matrix(c(1, 2, 3, 4,
                          5, 6, 7, 8,
                          9, 9, 9, 9), nrow = 3, byrow = TRUE)

# Set up layout
layout(layout_matrix, heights = c(1, 1, 0.5))
# Define the break interval for both plot and legend
break_interval <- 250

# Loop through each month to plot wheat prices
par(mar = c(0, 0, 0, 0))
for (i in 1:8) {
  plot(predictions_wheat[[i]], main = paste(month.name[i], year),
       zlim = c(min_wheat, max_wheat), col = color_palette, 
       breaks = seq(min_wheat, max_wheat, by = break_interval), 
       legend = FALSE, axes = FALSE)
  
  plot(tza1, add = TRUE, border = "dimgray", lwd = 0.1)
  
  points(training_data, pch = 20, col = "red", cex = 0.5)
}

# Generate pretty breaks for the legend
legend_breaks <- pretty(c(min_wheat, max_wheat), n = 5)

# Reset plot layout to 1x1 for the legend
layout(matrix(1))  

# Set margins for the legend
par(mar = c(5, 4, 2, 1))  

# Plot the legend in the bottom row, centered horizontally
image.plot(zlim = c(min_wheat, max_wheat), legend.only = TRUE, 
           col = color_palette, horizontal = TRUE,
           legend.width = 0.7, legend.shrink = 0.9, 
           axis.args = list(at = legend_breaks, labels = legend_breaks, cex.axis = 0.9), 
           legend.args = list(text = "Predicted Wheat Price (TZS/kg)", side = 1, line = 2, cex = 0.9))

#save
for (i in 1:length(predictions_wheat)) {
  month_str <- sprintf("%02d", i)
  output_filename <- paste0("wheat_price_rf_pred_", month_str, ".tif")
  output_path <- file.path(output_dir, output_filename)
  writeRaster(predictions_wheat[[i]], output_path, overwrite = TRUE)
}

## Predicted potato prices----------------------------------------------------------------------------------------------
#potatoes
# Function to predict potato prices for a given month
predict_for_potato <- function(month) {
  rain_sum_lag <- rstack3[paste0("2024-", sprintf("%02d", month), "-01_rain.sum.lag")]
  names(rain_sum_lag) <- "rain.sum.lag"
  
  newstack <- c(rstack, rain_sum_lag)
  
  const_potato <- data.frame(
    maize = 0, rice = 0, sorghum = 0, bmillet = 0, fmillet = 0, wheat = 0, beans = 0, potato = 1,
    Month = month,
    Year = year
  )
  
  pred <- predict(newstack, rf, const = const_potato, na.rm = TRUE)
  pred <- mask(pred, tza1)
  return(pred)
}

# Create predictions for all months
predictions_potato <- lapply(1:8, predict_for_potato)

# Extract pixel values from predictions_potato
potato_values <- unlist(lapply(predictions_potato, values))
min_potato <- min(potato_values, na.rm = TRUE)
max_potato <- max(potato_values, na.rm = TRUE)

# Define the continuous color palette and reverse it
color_palette <- rev(terrain.colors(100))

layout_matrix <- matrix(c(1, 2, 3, 4,
                          5, 6, 7, 8,
                          9, 9, 9, 9), nrow = 3, byrow = TRUE)

# Set up layout
layout(layout_matrix, heights = c(1, 1, 0.5))

# Loop through each month to plot potato prices
break_interval <- 150
par(mar = c(0, 0, 0, 0))
for (i in 1:8) {
  plot(predictions_potato[[i]],  main = paste(month.name[i], year),
       zlim = c(min_potato, max_potato), col = color_palette, 
       breaks = seq(min_potato, max_potato, by = break_interval), 
       legend = FALSE, axes = FALSE)
  
  plot(tza1, add = TRUE, border = "dimgray", lwd = 0.1)
  
  points(training_data, pch = 20, col = "red", cex = 0.5)
}

# Generate pretty breaks for the legend
legend_breaks <- pretty(c(min_potato, max_potato), n = 5)  # Adjust n as needed

# Reset plot layout to 1x1 for the legend
layout(matrix(1))  

# Set margins for the legend
par(mar = c(5, 4, 2, 1))  
# Plot the legend in the bottom row, centered horizontally
image.plot(zlim = c(min_potato, max_potato), legend.only = TRUE, 
           col = color_palette, horizontal = TRUE,
           legend.width = 0.7, legend.shrink = 0.9, 
           axis.args = list(at = legend_breaks, labels = legend_breaks, cex.axis = 0.9), 
           legend.args = list(text = "Predicted Potato Price (TZS/kg)", side = 1, line = 2, cex = 0.9))
#save
for (i in 1:length(predictions_potato)) {
  month_str <- sprintf("%02d", i)
  output_filename <- paste0("potato_price_rf_pred_", month_str, ".tif")
  output_path <- file.path(output_dir, output_filename)
  writeRaster(predictions_potato[[i]], output_path, overwrite = TRUE)
}

## Prediction Evaluation-------------------------------------------------------------------------------------------------------
### 1. Using 2024 Validation data
pred<-predict(object=rf, newdata=validation_data)
actual<-validation_data$pkg
result<-data.frame(actual=actual, predicted=pred)

mse <- mean((actual - pred)^2, na.rm=TRUE)
paste('Mean Squared Error:', mse)

rmse <- sqrt(mse)
paste('Root Mean Squared error: ',mean(sqrt(rf$mse)))

#Save predicted & observed yield
write.csv(result, "result.csv")

#reading result.csv file (predicted vs observed)
rslt <- read.csv("result.csv", header=T)
print(names(rslt))

#R-square predicting from rf predicited vs observed 
rf.rmse<-round(sqrt(mean( (rslt$actual-rslt$predicted)^2 , na.rm = TRUE )),2)
print(rf.rmse)

#R-square
rf.r2<-round(summary(lm(actual~predicted, rslt))$r.squared,2)
print(rf.r2)

range(actual)

range(pred)

#plotting predicted Vs observed
ggplot(result, aes(x=actual, y=predicted), alpha=0.6) +
  geom_point(colour = "blue", size = 1.4, alpha=0.6) +
  ggtitle('Random Forest "Wholesale Grain Prices in Tanzania"') +
  scale_x_continuous("Observed Price (TZS Per Kg)",
                     limits = c(0, 5000),
                     breaks = seq(0, 5000, 1000)) +
  scale_y_continuous("Predicted Price (TZS Per Kg)",
                     limits = c(0, 5000),
                     breaks = seq(0, 5000, 1000)) +
  theme(axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size = 8, angle = 90, hjust = 0.5, vjust = 1),
        axis.text.x = element_text(size = 8)) +
  geom_abline(intercept = 0, slope = 1, linewidth = 0.5) +
  geom_smooth(aes(x = actual, y = predicted), formula = y ~ x, method = "lm", se = FALSE, colour = "red", linetype = 2, size = 0.9) +
  annotate("text", x = 300, y = 4500, label = paste("RMSE:", rf.rmse)) +
  annotate("text", x = 300, y = 4200, label = paste("R^2: ", rf.r2), parse = TRUE)


### 2. Compare the observed Prices (the training data) with the predicted Prices (predicted using the training data) using stats package
library(stats)

mypts_df$pred <- stats::predict(rf)

rsq <- function (obs, pred) cor(obs, pred, use = 'complete.obs') ^ 2
RMSE <- function(obs, pred){sqrt(mean((pred - obs)^2, na.rm = TRUE))}

fr2_rsq <- rsq(mypts_df$pkg, mypts_df$pred) %>% round(digits = 2)
fr2_rmse <- RMSE(mypts_df$pkg, mypts_df$pred) %>% round(digits = 0)

Price_fit_plot <- ggplot(data = mypts_df, aes(x = pkg, y = pred)) +
  geom_point(colour = "blue", size = 1.4 ,alpha=0.6) + 
  ggtitle('Observed vs Predicted "Wholesale Grain Prices in Tanzania"') +
  geom_abline(slope = 1, alpha=0.3) +
  annotate('text', x = 150, y = 4500, label = paste0("R^{2}==", fr2_rsq), parse = TRUE, size=3)  +
  annotate('text', x = 150, y = 4200, label = paste0("RMSE==", fr2_rmse), parse = TRUE, size=3)  +
  labs(x = "Observed Price (Tsh)", y = "Predicted Price (Tsh)") +
  xlim(0, 5000) + ylim(0, 5000)
Price_fit_plot

# Partial dependence plots
library(caret)

var_importance <- varImp(rf)

impvar <- rownames(var_importance)[order(var_importance[, 1], decreasing=TRUE)]

op <- par(mfrow=c(2, 4))
# exclude food commodities and months
predictors_to_plot <- setdiff(impvar, c("maize", "rice", "sorghum", "bmillet", "fmillet", "wheat", "beans", "potato", "jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec"))

for (i in seq_along(predictors_to_plot)) {
  partialPlot(rf, as.data.frame(training_data), predictors_to_plot[i], xlab=predictors_to_plot[i],
              main="Partial Dependence")
}

#------------------------------------------------------------------------------------------
# Determine the number of observations for each commodity
#After removing NAs
crop_counts <- table(mypts$Crop)
crop_counts

# Create a data frame with crop and count
crop_summary <- data.frame(
  Crop = names(crop_counts),
  Count = as.vector(crop_counts)
)

random# Print the crop summary
print(crop_summary)

#We need to create a Correlation matrix
prices.monthly

# Create a function to determine the season
get_season <- function(month) {
  if (month %in% 5:10) {
    return("Post-Harvest")
  } else {
    return("Lean Season")
  }
}

# Add a 'Season' column to the data
prices.monthly$Season <- sapply(prices.monthly$Month, get_season)

# Post Harvest Data
post_harvest_data <- prices.monthly[prices.monthly$Season == "Post-Harvest", ]
# Remove rows with NaNs from the Post-Harvest data
post_harvest_data <- post_harvest_data[complete.cases(post_harvest_data[, 7:14]), ]


# Lean Season data
lean_season_data <- prices.monthly[prices.monthly$Season == "Lean Season", ]
# Remove rows with NaNs from the Lean Season data
lean_season_data <- lean_season_data[complete.cases(lean_season_data[, 7:14]), ]

# Calculate correlation matrix for Post-Harvest season
post_harvest_corr <- cor(post_harvest_data[, 7:14])
# Calculate correlation matrix for Lean Season
lean_season_corr <- cor(lean_season_data[, 7:14])


# Plot for Post-Harvest season
corrplot(post_harvest_corr, 
         method = "color",          
         title = "",                
         tl.col = "black",          
         tl.cex = 0.5,             
         addCoef.col = "black",     
         number.cex = 0.5,         
         number.digits = 2)  

# Add title to the plot
title(main = "Post-Harvest Correlation Matrix", 
      line = 3,                
      cex.main = 0.9)          


# Plot for Lean Season
corrplot(lean_season_corr, 
         method = "color", 
         title = "",
         tl.col = "black",       
         tl.cex = 0.5, 
         addCoef.col = "black",
         number.cex = 0.5,
         number.digits = 2)


# Add title to the plot
title(main = "Lean Season Correlation Matrix", 
      line = 3,                
      cex.main = 0.9) 

# Crop Specific Price Predictions
## Maize
# Filter data for maize
mypts_df2 <- as.data.frame(mypts)
mypts_maize <- mypts_df2[mypts_df2$Crop == "Maize", ]
unique(mypts_maize$Crop)

# Harmonize the levels
mypts_maize <- mypts_maize %>%
  mutate(Crop = as.character(Crop)) %>%
  mutate(Crop = factor(Crop))
# check again
unique(mypts_maize$Crop)
mypts_maize <- vect(mypts_maize, geom=c("Longitude", "Latitude"), crs=crs(tza0), keepgeom=TRUE)

# Training data for maize
training_data_maize <- mypts_maize[mypts_maize$Year %in% c(2021, 2022, 2023), ]
training_data_maize <- as.data.frame(training_data_maize)

# Filter the data for validation (Jan 2024 - July 2024)
validation_data_maize <- mypts_maize[mypts_maize$Year == 2024, ]
validation_data_maize <- as.data.frame(validation_data_maize)

# Fit the Random Forest Model
rf_maize <- randomForest(pkg ~ maize +
                           Month + Year + 
                           ttport_1 + bio_3 + bio_6 + bio_9 + bio_12 + bio_18 + 
                           rain.sum.lag, 
                         data = training_data_maize, 
                         na.rm = TRUE)

rf_maize

(oob <- sqrt(rf_maize$mse[which.min(rf_maize$mse)]))

# evaluate
varImpPlot(rf_maize)

## Prediction Evaluation Maize Only RF Model-------------------------------------------------------------------------------------------------------
### 1. Using Validation data
pred_maize<-predict(object=rf_maize, newdata=validation_data_maize)
actual_maize<-validation_data_maize$pkg
result_maize<-data.frame(actual=actual_maize, predicted=pred_maize)

#Save predicted & observed yield
write.csv(result_maize, "result_maize.csv")

#reading result_maize.csv file (predicted vs observed)
rslt_m <- read.csv("result_maize.csv", header=T)
print(names(rslt_m))

#R-square predicting from rf_maize predicited vs observed 
rf_maize.rmse<-round(sqrt(mean( (rslt_m$actual-rslt_m$predicted)^2 , na.rm = TRUE )),2)
print(rf_maize.rmse)

#R-square
rf_maize.r2<-round(summary(lm(actual~predicted, rslt_m))$r.squared,2)
print(rf_maize.r2)

range(actual_maize)

range(pred_maize, na.rm = TRUE)

#plotting predicted Vs observed
ggplot(result_maize, aes(x=actual_maize, y=pred_maize), alpha=0.6) +
  geom_point(colour = "blue", size = 1.4, alpha=0.6) +
  ggtitle('Random Forest "Maize Prices in Tanzania"') +
  scale_x_continuous("Observed Price (Tsh)",
                     limits = c(0, 2000),
                     breaks = seq(0, 2000, 500)) +
  scale_y_continuous("Predicted Price (Tsh)",
                     limits = c(0, 2000),
                     breaks = seq(0, 5000, 500)) +
  theme(axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size = 8, angle = 90, hjust = 0.5, vjust = 1),
        axis.text.x = element_text(size = 8)) +
  geom_abline(intercept = 0, slope = 1, linewidth = 0.5) +
  geom_smooth(aes(x = actual, y = predicted), formula = y ~ x, method = "lm", se = FALSE, colour = "red", linetype = 2, size = 0.9) +
  annotate("text", x = 400, y = 2000, label = paste("RMSE:", rf_maize.rmse)) +
  annotate("text", x = 400, y = 1800, label = paste("R^2: ", rf_maize.r2), parse = TRUE)

## Beans Price Prediction
# Filter data for Beans
mypts_beans <- mypts_df2[mypts_df2$Crop == "Beans", ]
unique(mypts_beans$Crop)

# Harmonize the levels
mypts_beans <- mypts_beans %>%
  mutate(Crop = as.character(Crop)) %>%
  mutate(Crop = factor(Crop))
# check again
unique(mypts_beans$Crop)
mypts_beans <- vect(mypts_beans, geom=c("Longitude", "Latitude"), crs=crs(tza0), keepgeom=TRUE)

# Training data for beans
training_data_beans <- mypts_beans[mypts_beans$Year %in% c(2021, 2022, 2023), ]

# Validation Data beans
validation_data_beans <- mypts_beans[mypts_beans$Year == 2024, ]


# Fit the Random Forest Model
rf_beans <- randomForest(pkg ~ Month + Year + ttport_1 + bio_3 + bio_6 + bio_9 + bio_12 + rain.sum.lag, 
                         data = training_data_beans, 
                         na.rm = TRUE)

rf_beans

(oob <- sqrt(rf_beans$mse[which.min(rf_beans$mse)]))

# evaluate
varImpPlot(rf_beans)

## Prediction Evaluation Beans Only RF Model Using Validation data-------------------------------------------------------------------------
pred_beans<-predict(object=rf_beans, newdata=validation_data_beans)
actual_beans<-validation_data_beans$pkg
result_beans<-data.frame(actual=actual_beans, predicted=pred_beans)

#Save predicted & observed beans price
write.csv(result_beans, "result_beans.csv")

#reading result_beans.csv file (predicted vs observed)
rslt_b <- read.csv("result_beans.csv", header=T)
print(names(rslt_b))

#R-square predicting from rf_beans predicited vs observed 
rf_beans.rmse<-round(sqrt(mean( (rslt_b$actual-rslt_b$predicted)^2 , na.rm = TRUE )),2)
print(rf_beans.rmse)

#R-square
rf_beans.r2<-round(summary(lm(actual~predicted, rslt_b))$r.squared,2)
print(rf_beans.r2)

range(actual_beans)

range(pred_beans)

#plotting predicted Vs observed Beans Prices
ggplot(result_beans, aes(x=actual_beans, y=pred_beans), alpha=0.6) +
  geom_point(colour = "blue", size = 1.4, alpha=0.6) +
  ggtitle('Random Forest "Beans Prices in Tanzania"') +
  scale_x_continuous("Observed Price (Tsh)",
                     limits = c(0, 4000),
                     breaks = seq(0, 4000, 1000)) +
  scale_y_continuous("Predicted Price (Tsh)",
                     limits = c(0, 4000),
                     breaks = seq(0, 4000, 1000)) +
  theme(axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size = 8, angle = 90, hjust = 0.5, vjust = 1),
        axis.text.x = element_text(size = 8)) +
  geom_abline(intercept = 0, slope = 1, linewidth = 0.5) +
  geom_smooth(aes(x = actual, y = predicted), formula = y ~ x, method = "lm", se = FALSE, colour = "red", linetype = 2, size = 0.9) +
  annotate("text", x = 1000, y = 3800, label = paste("RMSE:", rf_beans.rmse)) +
  annotate("text", x = 1000, y = 3600, label = paste("R^2: ", rf_beans.r2), parse = TRUE)

## Rice Price Prediction
# Filter data for Rice
mypts_rice <- mypts_df2[mypts_df2$Crop == "Rice", ]
unique(mypts_rice$Crop)

# Harmonize the levels
mypts_rice <- mypts_rice %>%
  mutate(Crop = as.character(Crop)) %>%
  mutate(Crop = factor(Crop))
# check again
unique(mypts_rice$Crop)
mypts_rice <- vect(mypts_rice, geom=c("Longitude", "Latitude"), crs=crs(tza0), keepgeom=TRUE)

# Training data for rice
training_data_rice <- mypts_rice[mypts_rice$Year %in% c(2021, 2022, 2023), ]

# Validation Data rice
validation_data_rice <- mypts_rice[mypts_rice$Year == 2024, ]


# Fit the Random Forest Model
rf_rice <- randomForest(pkg ~ Month + Year + ttport_1 + bio_3 + bio_6 + bio_9 + bio_12 + bio_12 + rain.sum.lag, 
                         data = training_data_rice, 
                         na.rm = TRUE)

rf_rice

(oob <- sqrt(rf_rice$mse[which.min(rf_rice$mse)]))

# evaluate
varImpPlot(rf_rice)

## Prediction Evaluation Rice Only RF Model Using Validation data-------------------------------------------------------------------------
pred_rice<-predict(object=rf_rice, newdata=validation_data_rice)
actual_rice<-validation_data_rice$pkg
result_rice<-data.frame(actual=actual_rice, predicted=pred_rice)

#Save predicted & observed rice price
write.csv(result_rice, "result_rice.csv")

#reading result_rice.csv file (predicted vs observed)
rslt_r <- read.csv("result_rice.csv", header=T)
print(names(rslt_r))

#R-square predicting from rf_rice predicited vs observed 
rf_rice.rmse<-round(sqrt(mean( (rslt_r$actual-rslt_r$predicted)^2 , na.rm = TRUE )),2)
print(rf_rice.rmse)

#R-square
rf_rice.r2<-round(summary(lm(actual~predicted, rslt_r))$r.squared,2)
print(rf_rice.r2)

range(actual_rice)

range(pred_rice)

#plotting predicted Vs observed Rice Prices
ggplot(result_rice, aes(x=actual_rice, y=pred_rice), alpha=0.6) +
  geom_point(colour = "blue", size = 1.4, alpha=0.6) +
  ggtitle('Random Forest "Rice Prices in Tanzania"') +
  scale_x_continuous("Observed Price (Tsh)",
                     limits = c(0, 4000),
                     breaks = seq(0, 4000, 1000)) +
  scale_y_continuous("Predicted Price (Tsh)",
                     limits = c(0, 4000),
                     breaks = seq(0, 4000, 1000)) +
  theme(axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size = 8, angle = 90, hjust = 0.5, vjust = 1),
        axis.text.x = element_text(size = 8)) +
  geom_abline(intercept = 0, slope = 1, linewidth = 0.5) +
  geom_smooth(aes(x = actual, y = predicted), formula = y ~ x, method = "lm", se = FALSE, colour = "red", linetype = 2, size = 0.9) +
  annotate("text", x = 1000, y = 3800, label = paste("RMSE:", rf_rice.rmse)) +
  annotate("text", x = 1000, y = 3600, label = paste("R^2: ", rf_rice.r2), parse = TRUE)

#--------------------------------------------------------------------------------------------------------------------------------------------------------
# Comparison between Pooled model and Crop Specific Model
set.seed(1983)
evaluate_models <- function(crop) {
  # Filter data for the specific crop
  crop_data <- mypts[mypts$Crop == crop, ]
  training_data_crop <- crop_data[crop_data$Year %in% c(2021, 2022, 2023), ]
  training_data_crop <- as.data.frame(training_data_crop)
  
  validation_data_crop <- crop_data[crop_data$Year == 2024, ]
  validation_data_crop <- as.data.frame(validation_data_crop)
  
  # Predictions for pooled model on specific crop data
  pooled_pred_crop <- predict(rf, newdata = validation_data_crop)
  actual_pooled <- validation_data_crop$pkg
  result_pooled <-data.frame(actual=actual_pooled, predicted=pooled_pred_crop)
  
  # Crop-Specific Model
  rf_crop <- randomForest(pkg ~ Month + Year + ttport_1 + bio_3 + bio_6 + bio_9 + bio_12 + bio_18 + rain.sum.lag,
                          data = training_data_crop, na.rm = TRUE)
  
  # Predictions for crop-specific model
  predictions_crop <- predict(rf_crop, newdata = validation_data_crop)
  actual_crop <- validation_data_crop$pkg
  result_crop_specific <- data.frame(actual=actual_crop, predicted=predictions_crop)
  
  # Calculate performance metrics
  rmse_pooled <- round(sqrt(mean((result_pooled$actual-result_pooled$predicted)^2 , na.rm = TRUE )),2)
  r2_pooled <- round(summary(lm(actual~predicted, result_pooled))$r.squared,2)
  rmse_crop <- round(sqrt(mean((result_crop_specific$actual-result_crop_specific$predicted)^2 , na.rm = TRUE )),2)
  r2_crop <- round(summary(lm(actual~predicted, result_crop_specific))$r.squared,2)
  
  return(data.frame(Crop = crop,
                    Model = c("Pooled", "Crop-Specific"),
                    RMSE = c(rmse_pooled, rmse_crop),
                    R_squared = c(r2_pooled, r2_crop)))
}

# Apply the function to all crops
crop <- unique(mypts$Crop)
comparison_df <- do.call(rbind, lapply(crop, evaluate_models))

print(comparison_df)
#write.csv(comparison_df, "model_comparison_df.csv")

comparison_df_wide <- comparison_df %>%
  pivot_wider(names_from = Model, values_from = c(RMSE, R_squared)) %>%
  rename(RMSE_Crop_Specific = `RMSE_Crop-Specific`,
         RMSE_Pooled = `RMSE_Pooled`,
         R_squared_Crop_Specific = `R_squared_Crop-Specific`,
         R_squared_Pooled = `R_squared_Pooled`)
comparison_df_wide

comparison_df_wide <- as.data.frame(comparison_df_wide)
write.csv(comparison_df_wide, "Last-year-validation.csv")

#----------------------------------------------------------------------------
# Cross Validation
mypts <- as.data.frame(mypts)

# Maize
## Set seed for reproducibility
set.seed(123)

# Function to perform in-fold validation for pooled and crop-specific Random Forest models
perform_infold_validation <- function(training_data, outcome, crop_col) {
  
  # Set up training control with k-fold cross-validation
  ctrl <- trainControl(method = "cv", number = 10, savePredictions = TRUE)
  
  # Train the pooled Random Forest model (for all crops)
  rf_model_pooled <- train(
    pkg ~ maize + rice + sorghum + bmillet + fmillet + wheat + beans + Month + Year +
      ttport_1 + bio_3 + bio_6 + bio_9 + bio_12 + bio_18 + rain.sum.lag,
    data = training_data,
    method = "rf",
    trControl = ctrl,
    na.action = na.omit
  )
  
  # Subset the training data for maize only
  training_maize <- training_data[training_data[[crop_col]] == "Maize", ]
  
  # Train the crop-specific Random Forest model for maize
  rf_model_maize <- train(
    pkg ~ Month + Year + ttport_1 + bio_3 + bio_6 + bio_9 + bio_12 + bio_18 + rain.sum.lag,
    data = training_maize,
    method = "rf",
    trControl = ctrl,
    na.action = na.omit
  )
  
  # Extract predictions for in-fold validation
  predictions_pooled <- rf_model_pooled$pred
  predictions_maize <- rf_model_maize$pred
  
  # Subset training_data to include only rows where Crop is Maize
  maize_indices <- training_data[[crop_col]] == "Beans"
  
  # Filter pooled predictions using rowIndex for maize rows only
  predictions_pooled_maize <- predictions_pooled %>%
    filter(rowIndex %in% which(maize_indices))
  
  # Calculate overall RMSE and R-squared for the pooled model (maize only)
  result_pooled_maize <- predictions_pooled_maize %>%
    group_by(Resample) %>%
    summarize(
      RMSE = sqrt(mean((obs - pred)^2, na.rm = TRUE)),
      R_squared = summary(lm(obs ~ pred))$r.squared
    )
  
  result_maize <- predictions_maize %>%
    group_by(Resample) %>%
    summarize(
      RMSE = sqrt(mean((obs - pred)^2, na.rm = TRUE)),
      R_squared = summary(lm(obs ~ pred))$r.squared
    )
  
  list(
    rf_model_pooled = rf_model_pooled,
    rf_model_maize = rf_model_maize,
    results = data.frame(
      model_type = c("Pooled (Maize)", "Maize-specific"),
      RMSE = c(result_pooled_maize$RMSE, result_maize$RMSE),
      R_squared = c(result_pooled_maize$R_squared, result_maize$R_squared)
    )
  )
}

result <- perform_infold_validation(training_data, outcome = "pkg", crop_col = "Crop")

# Extract the results data frame from the list
results_df <- result$results

# Group and summarize the data
avg_results <- results_df %>%
  group_by(model_type) %>%
  summarise(
    avg_RMSE = mean(RMSE),
    avg_R_Squared = mean(R_squared)
  )

avg_results

# In fold validation for all Crops --------------------------------------------------------
# Function to perform in-fold validation for pooled and crop-specific Random Forest models
perform_infold_validation <- function(train, outcome, crop_col, crop_name) {
  
  # Set up training control with 10-fold cross-validation
  ctrl <- trainControl(method = "cv", number = 10, savePredictions = TRUE)
  
  # Train the pooled Random Forest model (for all crops)
  rf_model_pooled <- train(
    pkg ~ maize + rice + sorghum + bmillet + fmillet + wheat + beans + Month + Year +
      ttport_1 + bio_3 + bio_6 + bio_9 + bio_12 + bio_18 + rain.sum.lag,
    data = train,
    method = "rf",
    trControl = ctrl,
    na.action = na.omit
  )
  
  # Subset training data for the specific crop only
  training_crop <- train[train[[crop_col]] == crop_name, ]
  
  # Train the crop-specific Random Forest model for the current crop
  rf_model_crop <- train(
    pkg ~ Month + Year + ttport_1 + bio_3 + bio_6 + bio_9 + bio_12 + bio_18 + rain.sum.lag,
    data = training_crop,
    method = "rf",
    trControl = ctrl,
    na.action = na.omit
  )
  
  # Extract predictions for in-fold validation
  predictions_pooled <- rf_model_pooled$pred
  predictions_crop <- rf_model_crop$pred
  
  # Subset training_data to include only rows where Crop is the current crop
  crop_indices <- training_data[[crop_col]] == crop_name
  
  # Filter pooled predictions using rowIndex for the current crop rows only
  predictions_pooled_crop <- predictions_pooled %>%
    filter(rowIndex %in% which(crop_indices))
  
  # Calculate overall RMSE and R-squared for the pooled model (current crop only)
  result_pooled_crop <- predictions_pooled_crop %>%
    group_by(Resample) %>%
    summarize(
      RMSE = sqrt(mean((obs - pred)^2, na.rm = TRUE)),
      R_squared = summary(lm(obs ~ pred))$r.squared
    )
  
  # Calculate RMSE and R-squared for the crop-specific model
  result_crop <- predictions_crop %>%
    group_by(Resample) %>%
    summarize(
      RMSE = sqrt(mean((obs - pred)^2, na.rm = TRUE)),
      R_squared = summary(lm(obs ~ pred))$r.squared
    )
  
  # Return models and results
  list(
    rf_model_pooled = rf_model_pooled,
    rf_model_crop = rf_model_crop,
    results = data.frame(
      model_type = c(paste(crop_name, "- pooled", sep = ""), paste(crop_name, "- specific", sep = "")),
      RMSE = c(result_pooled_crop$RMSE, result_crop$RMSE),
      R_squared = c(result_pooled_crop$R_squared, result_crop$R_squared)
    )
  )
}

# List of unique crops from your data
crops <- unique(training_data$Crop)

# Initialize a list to store results for each crop
all_results <- list()

# Loop through each crop and apply the validation function
for (crop in crops) {
  result <- perform_infold_validation(train, outcome = "pkg", crop_col = "Crop", crop_name = crop)
  
  # Store the results in the list
  all_results[[crop]] <- result$results
}

# Combine all results into a single data frame
combined_results <- do.call(rbind, all_results)

# Group and summarize the data for each model type across crops
avg_results_all_crops <- combined_results %>%
  group_by(model_type) %>%
  summarise(
    avg_RMSE = mean(RMSE),
    avg_R_Squared = mean(R_squared)
  )

# Print the summarized results
print(avg_results_all_crops)

# First, clean and separate the 'model_type' into 'Crop' and 'Model'
combined_results <- avg_results_all_crops %>%
  mutate(
    Crop = sub("- (pooled|specific)", "", model_type),
    Model = ifelse(grepl("pooled", model_type), "Pooled", "Specific")
  )

# Summarize and pivot the data to wide format
comparison_df_wide2 <- combined_results %>%
  select(Crop, Model, avg_RMSE, avg_R_Squared) %>%
  pivot_wider(
    names_from = Model,
    values_from = c(avg_RMSE, avg_R_Squared),
    names_glue = "{.value}_{Model}"
  ) %>%
  rename(
    RMSE_Pooled = avg_RMSE_Pooled,
    RMSE_Crop_Specific = avg_RMSE_Specific,
    R_squared_Pooled = avg_R_Squared_Pooled,
    R_squared_Crop_Specific = avg_R_Squared_Specific
  )

print(comparison_df_wide2)

#Train - Test Validation
##Split the data into train and test datasets
set.seed(1983)
mypts <- as.data.frame(mypts)
rows <- sample(x=1:nrow(mypts), size = 0.70* nrow(mypts))
train <- mypts[rows, ]
test <- mypts[! rownames(mypts) %in% rows, ]

RF <- randomForest(pkg ~ maize + rice + sorghum + bmillet + fmillet + wheat + beans +
                     Month +
                     Year + 
                     ttport_1 +
                     bio_3 + bio_6 + bio_9 +  bio_12 + bio_18 + 
                     rain.sum.lag, 
                   data=train, na.rm=TRUE)

RF

varImpPlot(RF)

partialPlot(RF, as.data.frame(train), "rain.sum.lag")

### 1. Using Validation data
pred<-predict(object=RF, newdata=test)
actual<-test$pkg
result<-data.frame(actual=actual, predicted=pred)

#R-square predicting from rf predicited vs observed 
rf.rmse<-round(sqrt(mean( (result$actual-result$predicted)^2 , na.rm = TRUE )),2)
print(rf.rmse)

#R-square
rf.r2<-round(summary(lm(actual~predicted, result))$r.squared,2)
print(rf.r2)

evaluate_models <- function(crop) {
  # Filter data for the specific crop
  crop_data <- mypts[mypts$Crop == crop, ]
  set.seed(1983)
  rownames(crop_data)<-1:nrow(crop_data)
  rows <- sample(x=1:nrow(crop_data), size = 0.70* nrow(crop_data))
  
  training_data_crop <- crop_data[rows, ]
  
  validation_data_crop <- crop_data[! rownames(crop_data) %in% rows, ]
  
  # Predictions for pooled model on specific crop data
  pooled_pred_crop <- predict(RF, newdata = validation_data_crop)
  actual_pooled <- validation_data_crop$pkg
  result_pooled <-data.frame(actual=actual_pooled, predicted=pooled_pred_crop)
  
  # Crop-Specific Model
  rf_crop <- randomForest(pkg ~ Month + Year + ttport_1 + bio_3 + bio_6 + bio_9 + bio_12 + bio_18 + rain.sum.lag,
                          data = training_data_crop, na.rm = TRUE)
  
  # Predictions for crop-specific model
  predictions_crop <- predict(rf_crop, newdata = validation_data_crop)
  actual_crop <- validation_data_crop$pkg
  result_crop_specific <- data.frame(actual=actual_crop, predicted=predictions_crop)
  
  # Calculate performance metrics
  rmse_pooled <- round(sqrt(mean((result_pooled$actual-result_pooled$predicted)^2 , na.rm = TRUE )),2)
  r2_pooled <- round(summary(lm(actual~predicted, result_pooled))$r.squared,2)
  rmse_crop <- round(sqrt(mean((result_crop_specific$actual-result_crop_specific$predicted)^2 , na.rm = TRUE )),2)
  r2_crop <- round(summary(lm(actual~predicted, result_crop_specific))$r.squared,2)
  
  return(data.frame(Crop = crop,
                    Model = c("Pooled", "Crop-Specific"),
                    RMSE = c(rmse_pooled, rmse_crop),
                    R_squared = c(r2_pooled, r2_crop)))
}

# Apply the function to all crops
crop <- unique(mypts$Crop)
comparison_df3 <- do.call(rbind, lapply(crop, evaluate_models))

comparison_df3

comparison_df_wide3 <- comparison_df3 %>%
  pivot_wider(names_from = Model, values_from = c(RMSE, R_squared)) %>%
  rename(RMSE_Crop_Specific = `RMSE_Crop-Specific`,
         RMSE_Pooled = `RMSE_Pooled`,
         R_squared_Crop_Specific = `R_squared_Crop-Specific`,
         R_squared_Pooled = `R_squared_Pooled`)
comparison_df_wide3

comparison_df_wide3 <- as.data.frame(comparison_df_wide3)
write.csv(comparison_df_wide3, "Train-test-split-validation.csv")


# Large markets = Markets with a population of 300k or more
# 2022 Population data from Tanzania national beaurau of statistics at district level
# Compiled Markets, coodinates, and population
Mkts_pop <- read.csv("Mkts_population.csv")
head(Mkts_pop, 42)

# Merge population data with the rest of the dataset
# Convert to data.table
setDT(Mkts_pop)
mypts <- as.data.table(mypts)
Mkts_pop[, Population := as.numeric(gsub(",", "", Population))]

price_pop_data <- merge(mypts, Mkts_pop, by = c("Market", "Latitude", "Longitude"))
head(price_pop_data)

# Create a dummy variable for large markets
price_pop_data$Market_Size <- ifelse(price_pop_data$Population > 300000, 1, 0)
price_pop_data

table(price_pop_data$Market)


# Market Validation
# Specify the model 
# Split the data into 70% training and 30% testing
set.seed(1983)
train_index <- createDataPartition(price_pop_data$Region, p = 0.7, list = FALSE)
train_data <- price_pop_data[train_index, ]
test_data <- price_pop_data[-train_index, ]

# Set up training control with 5-fold cross-validation
ctrl <- trainControl(method = "cv", number = 5, savePredictions = TRUE)

# Train a Random Forest model on the training data
rf_model <- train(pkg ~ Market + Crop +
                    Month +
                    Year + 
                    ttport_1 +
                    bio_3 + bio_6 + bio_9 +  bio_12 + bio_18 + 
                    rain.sum.lag, 
                  data=train_data, na.rm=TRUE)
rf_model

var_imp <- varImp(rf_model)
var_imp
plot(var_imp, top = 20) 

# Make predictions on the test data
predictions_mkts <- predict(rf_model, newdata = test_data)
actual_mkts <- test_data$pkg
result_mkts <- data.frame(actual=actual_mkts, predicted=predictions_mkts)

# Calculate performance metrics for the entire test set
r_squared <- round(summary(lm(actual~predicted, result_mkts))$r.squared,2)
r_squared
rmse <- round(sqrt(mean((result_mkts$actual-result_mkts$predicted)^2 , na.rm = TRUE )),2)
rmse

ggplot(result_mkts, aes(x=actual, y=predicted), alpha=0.6) +
  geom_point(colour = "blue", size = 1.4, alpha=0.6) +
  ggtitle('5-Fold Cross-Validated Price Predictions Using Random Forest by Market') +
  scale_x_continuous("Observed Price (Tsh)",
                     limits = c(0, 5000),
                     breaks = seq(0, 5000, 1000)) +
  scale_y_continuous("Predicted Price (Tsh)",
                     limits = c(0, 5000),
                     breaks = seq(0, 5000, 1000)) +
  theme(axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size = 8, angle = 90, hjust = 0.5, vjust = 1),
        axis.text.x = element_text(size = 8)) +
  geom_abline(intercept = 0, slope = 1, linewidth = 0.5) +
  geom_smooth(aes(x = actual, y = predicted), formula = y ~ x, method = "lm", se = FALSE, colour = "red", linetype = 2, size = 0.9) +
  annotate("text", x = 300, y = 4500, label = paste("RMSE:", rmse)) +
  annotate("text", x = 300, y = 4200, label = paste("R^2: ", r_squared), parse = TRUE)

# Filter test data for large and small markets
large_market_test <- subset(test_data, Market_Size == 1)
small_market_test <- subset(test_data, Market_Size == 0)

# Predictions for large markets
large_market_predictions <- predict(rf_model, newdata = large_market_test)
actual_large <- large_market_test$pkg
result_large <- data.frame(actual=actual_large, predicted=large_market_predictions)

r_squared_large <- round(summary(lm(actual~predicted, result_large))$r.squared,2)
r_squared_large
rmse_large <- round(sqrt(mean((result_large$actual-result_large$predicted)^2 , na.rm = TRUE )),2)
rmse_large

# Predictions for small markets
small_market_predictions <- predict(rf_model, newdata = small_market_test)
actual_small <- small_market_test$pkg
result_small <- data.frame(actual=actual_small, predicted=small_market_predictions)

r_squared_small <- round(summary(lm(actual~predicted, result_small))$r.squared,2)
r_squared_small
rmse_small <- round(sqrt(mean((result_small$actual-result_small$predicted)^2 , na.rm = TRUE )),2)
rmse_small

# Merge results for large and small markets
metrics_table <- data.frame(
  Market_Size = c("Large Markets", "Small Markets"),
  R_squared = c(r_squared_large, r_squared_small),
  RMSE = c(rmse_large, rmse_small)
)
metrics_table


# How well does the model predicts for each individual market?
# Create a function to evaluate performance for each market using the test data
evaluate_market <- function(market_name, model, data) {
  # Filter test data for the current market
  market_test_data <- subset(data, Market == market_name)
  
  # Make predictions for the current market
  market_predictions <- predict(rf_model, newdata = market_test_data)
  actual_values <- market_test_data$pkg
  
  # Data frame for the actual and predicted values
  result <- data.frame(actual = actual_values, predicted = market_predictions)
  
  # Calculate RMSE and R-squared for the current market
  r_squared <- round(summary(lm(actual ~ predicted, data = result))$r.squared, 2)
  rmse <- round(sqrt(mean((result$actual - result$predicted)^2, na.rm = TRUE)), 2)
  
  # Return a data frame with the market name, RMSE, and R-squared
  return(data.frame(Market = market_name, R_squared = r_squared, RMSE = rmse))
}

# markets
markets <- unique(test_data$Market)

# Apply function to each market
market_metrics <- do.call(rbind, lapply(markets, evaluate_market, model = rf_model, data = test_data))
market_metrics

market_size_dta <- unique(test_data[, c("Market", "Market_Size")])

# Convert 1 and 0 into "Large Market" and "Small Market"
market_size_dta$Market_Size <- ifelse(market_size_dta$Market_Size == 1, "Large Market", "Small Market")

# Merge the market metrics with Market_Sizes
market_metrics <- merge(market_metrics, market_size_dta, by = "Market")
head(market_metrics)

###---------------------------------------------------------------------------
# Extract Predicted price using OSM Data
# Bring in Tanzania Populated Places from OSM
TZ_populated_places <- vect("H:\\Tanzania Price data\\Datasets\\OSM_Population\\hotosm_tza_populated_places_points_shp.shp")
head(TZ_populated_places)
# Plot All data points
plot(tza1)
plot(TZ_populated_places, col="Red", add=TRUE)

sapply(TZ_populated_places, class)
# Ensure that the population column is numeric
TZ_populated_places$population <- as.numeric(TZ_populated_places$population)

# Filter to remain with rows where population is 10,000 or more
TZ_populated_places_filtered <- TZ_populated_places[!is.na(TZ_populated_places$population) & TZ_populated_places$population >= 10000, ]
head(TZ_populated_places_filtered, 30)


# Plot places where population data is provided
plot(tza1)
plot(TZ_populated_places_filtered, col="Red", add=TRUE)
#writeVector(TZ_populated_places_filtered, "H:/Tanzania Price data/Datasets/Cost_surface data/tz_towns.shp", overwrite = TRUE)

# Extract prices for these points
# Read the predicted price raster data for Maize
output_dir_Maize <- "H:/Tanzania Price data/Datasets/Pred-plots/Maize"

Maize_price_list <- list.files(output_dir_Maize, pattern = ".tif$", full.names = TRUE)
Maize_price_list
Maize_price_rast <- rast(Maize_price_list)
terra::plot(Maize_price_rast[[1]])

# Extract predicted maize prices for OSM points
osm_points_with_prices <- TZ_populated_places_filtered  

# Loop over each month and extract the maize price for OSM points
for (i in 1:nlyr(Maize_price_rast)) {
  # Extract prices for month i
  prices_for_month <- terra::extract(Maize_price_rast[[i]], TZ_populated_places_filtered, method = "bilinear")
  
  # Add the extracted prices to the SpatVector (each month as a separate column)
  osm_points_with_prices[[paste0("maize_price_month_", sprintf("%02d", i))]] <- prices_for_month[,2]
}

# Filter and remain only with necessary columns
osm_points_with_prices_select <- osm_points_with_prices[, c("name", "place", "is_in", "population", "osm_id", 
                                                            "maize_price_month_01", 
                                                            "maize_price_month_02", 
                                                            "maize_price_month_03", 
                                                            "maize_price_month_04", 
                                                            "maize_price_month_05", 
                                                            "maize_price_month_06", 
                                                            "maize_price_month_07", 
                                                            "maize_price_month_08")]


head(osm_points_with_prices_select)

osm_points_with_prices_select <- as.data.table(osm_points_with_prices_select)

# Reshape to long format with a month and price column
osm_points_long <- osm_points_with_prices_select %>%
  pivot_longer(cols = starts_with("maize_price_month_"), 
               names_to = "Month", 
               values_to = "price")

# Clean month column to extract only the month number
osm_points_long$Month <- as.numeric(gsub("maize_price_month_", "", osm_points_long$Month))
# Add a new column crop with the value Maize for now
osm_points_long$crop <- "maize"

head(osm_points_long, 40)


write.csv(osm_points_long, "Predicted_maize_long.csv")

# Access to Markets
library(gdistance)
library(raster)
library(terra)
library(magrittr)

# Set Lambert Azimuthal Equal-Area (LAEA) projection centered on Tanzania
laea_tz <- "+proj=laea +lat_0=-6 +lon_0=35 +datum=WGS84 +units=m +no_defs"


# Slope
# Load and reproject slope layer
slope_radian <- terra::rast("H:/Tanzania Price data/Datasets/Cost_surface data/tz_slope_radian.tif")
# Reproject to LAEA and set resolution to 500m
slope_radian_laea <- terra::project(slope_radian, laea_tz, res = 500, 
                                    filename = "slope_laea_500m.tif", overwrite = TRUE)
terra::plot(slope_radian_laea, main = "Slope of Tanzania (Radians)")


# Use the slope layer obtained above to create a decay coefficient
# We use a decay coefficient of 1.5
# Create slope cost surface
decay <- 1.5
slope_cost <- exp(decay * tan(slope_radian_laea))
names(slope_cost) <- "slope_cost"
terra::plot(slope_cost, main = "Slope cost")

# Roads
# Load and reproject roads
roads <- terra::vect("H:/Tanzania Price data/Datasets/Cost_surface data/tz_roads.shp")
# Reproject the roads vector to LAEA
roads_laea <- terra::project(roads, laea_tz)

terra::plot(slope_radian_laea)
terra::lines(roads_laea, col="black")
terra::lines(roads_laea[roads_laea$highway == "primary", ], lwd=4, col="red")
terra::lines(roads_laea[roads_laea$highway == "secondary", ], lwd=2, col="blue")

# Create road cost surface
cfile <- "rdcost_laea.tif"
roadtypes <- c("primary", "secondary", "tertiary")

if (!file.exists(cfile)) {
  i <- match(roads_laea$highway, roadtypes)
  roads_laea$speed <- c(0.001, 0.0015, 0.002)[i]
  rd_cost <- rasterize(roads_laea, slope_radian, field=roads_laea$speed, filename=cfile, overwrite=TRUE)
} else {
  rd_cost <- terra::rast(cfile)
}

rd_cost_laea <- terra::project(rd_cost, laea_tz,  res = 500, 
                               filename = "rd_cost_laea_500m.tif", overwrite = TRUE)

a <- aggregate(rd_cost_laea, 3, min, na.rm=TRUE)
terra::plot(a, col=c("black", "blue", "red"), main = "Road travel cost (min/m)")

# Land Cover
# Load and reproject land cover layer
tza_lulc <- terra::rast("H:/Tanzania Price data/Datasets/Cost_surface data/TZ_Land_cover_2021.tif")

plot(tza_lulc, main = "WorldCover 10 m 2021 v200 - landcover classes")
terra::lines(tza1) 

Table_class <- data.frame(
  Value = c(10,20,30,40,50,60,70,80,90, 95),
  LandClass = c("Tree cover",
                "Shrubland",
                "Grassland",
                "Cropland",
                "Built-up",
                "Bare/sparse vegetation",
                "Snow and Ice",
                "Permanent water bodies",
                "Herbaceous wetland",
                "Mangroves"),
  travel_speeds=c(0.04, 0.02, 0.02, 0.01, 0.01, 0.02, 0.04, 0.11, 0.04, 0.05)
)

rc <- data.frame(from=unique(tza_lulc)[,1], to=0.02)
rc
# Adjust Reclassification Table Based on the Classes
#rc <- data.frame(from = Table_class$Value, to = Table_class$travel_speeds)
# Assign specific travel speeds based on land cover class values
rc$to[rc$from %in% c(10)] <- 0.04   # Tree cover
rc$to[rc$from %in% c(20, 30)] <- 0.02  # Shrubland and Grassland
rc$to[rc$from == 40] <- 0.01  # Cropland
rc$to[rc$from == 50] <- 0.01  # Built-up
rc$to[rc$from == 60] <- 0.02  # Bare/sparse vegetation
rc$to[rc$from == 70] <- 0.04  # Snow and Ice 
rc$to[rc$from == 80] <- 0.11  # Permanent water bodies
rc$to[rc$from == 90] <- 0.04  # Herbaceous wetland
rc$to[rc$from == 95] <- 0.05  # Mangroves
rc
#reclassifying
tza_lc_reclass <- classify(tza_lulc, rc)

lcfname <- "lc_cost.tif"
if (!file.exists(lcfname)) {
  # first aggregate to about the same spatial resolution
  lc_cost <- aggregate(tza_lc_reclass, 3, mean)
  # then resample
  lc_cost <- resample(lc_cost, slope_radian, filename=lcfname, wopt=list(names="lc_cost"), overwrite=TRUE)
} else {
  lc_cost <- rast(lcfname)
}

lc_cost_laea <- terra::project(lc_cost, laea_tz,  res = 500, 
                               filename = "lc_cost_laea_500m.tif", overwrite = TRUE)
terra::plot(lc_cost_laea, main = "Off-road travel costs (min/m) based on land cover class")


# Combine the cost layers
all_cost <- c(rd_cost_laea, lc_cost_laea)
#getting the minimum value of each grid cell
cost <- min(all_cost, na.rm=TRUE)
cost <- cost * slope_cost
terra::plot(cost, main="Final cost layer (min/m)")

# Combine the cost layers
library(gdistance)
cost <- raster(cost)
conductance <- 1/cost
#Creating a transition object
tran <- transition(conductance, transitionFunction=mean, directions= 8)

tran <- geoCorrection(tran, type="c")

save(trans, file = "H:/Tanzania Price data/Datasets/Cost_surface data/transition.rda")

# Towns of 20,000 people or more from OSM
town <- terra::vect("H:/Tanzania Price data/Datasets/Cost_surface data/tz_towns.shp")
# Project the towns to match the CRS of slope_radian
town_prj <- project(town, crs(slope_cost))
geom(town_prj)
# Extracting coordinates and population from the projected towns
towns <- data.frame(
  x = geom(town_prj)[, 3],  
  y = geom(town_prj)[, 4],  
  population = town_prj$population  # Population from the projected towns
)
towns
#convert to spatial points needed in gdistance
spTowns <- SpatialPoints(cbind(towns$x, towns$y))
spTowns

#Estimating
Ac <- accCost(tran, fromCoords=spTowns)
A <- rast(Ac) / 60
AA <- clamp(A, 0, 24) |> mask(slope_radian_laea)
## Warning: [mask] CRS do not match
terra::plot(AA, main="Access to markets (towns > 20k) in Tanzania (hrs)")
terra::lines(roads_laea)
terra::points(town_prj, col="red", pch=20, cex=1.0)

writeRaster(Ac, "H:/Tanzania Price data/Datasets/Cost_surface data/Access20k.tif", overwrite=TRUE)

# Calculating farmgate prices 1
#Load trans object that was created
load("H:/Tanzania Price data/Datasets/Cost_surface data/transition.rda")
terra::plot(raster(tran), main='Transition matrix (minutes)')
# We'll use towns our >20k from OSM
terra::plot(town_prj, col = "red", cex = 0.5, add=TRUE)

# Extract predicted maize prices for OSM points
# Lets use January Maize price for now
maize_price_jan <- rast("H:\\Tanzania Price data\\Datasets\\Pred-plots\\Maize\\maize_price_rf_pred_01.tif")
maize_price_jan_laea <- terra::project(maize_price_jan, laea_tz, res = 500)
maize_price_jan_osm <- terra::extract(maize_price_jan_laea, town_prj, fun=mean, buffer=2500, small=TRUE)
# the extract function is returning some NA values, complete the list with a mean of the other values
price_values <- maize_price_jan_osm[, 2]  
# Replace NAs with the mean of the non-NA values
maize_price_jan_osm[, 2][is.na(price_values)] <- mean(price_values, na.rm = TRUE)
maize_price_jan_osm

town_prj[["maipkg"]] <- maize_price_jan_osm$lyr1
town_sp <- as(town_prj, "Spatial")

#transportation cost. I converted 0.02 usd/kg/h to TZS/kg/hr = 54.43 
tr_cost <- 54.43 #TZS/kg/hr

for(x in seq_along(town_sp)){
  tmp.acc <- accCost(tran, town_sp[x,])/60
  market_price <-  as.data.frame(town_sp)[x,"maipkg"]
  # farmgate price: market price +/- variable transport cost (60ETB/MT/hr)
  # fgate_price <- market_price * (exp(-1 * alpha * tmp.acc) + (minprsh*(1 - exp(-1 * alpha * tmp.acc))))
  fgate_price <- market_price - (54.43 * tmp.acc)
  fgate_price[fgate_price<0] <- 0
  if (x == 1) {
    mystack <- fgate_price
  } else {
    mystack <- stack(mystack, fgate_price)}
}

# Loop over each layer in the stack to save individually
for (i in seq_len(nlayers(mystack))) {
  # Extract the current layer
  fgate_rast <- mystack[[i]]
  
  # Define the filename for saving the raster
  output_file <- file.path(output_directory, paste0("maize_farmgate_price_", i, ".tif"))
  
  # Save the raster to disk
  terra::writeRaster(fgate_rast, filename = output_file, overwrite = TRUE)
}

mystack_spat <- terra::rast(mystack)

fgate_price <- max(mystack_spat) %>% resample(.,maize_price_jan_laea)  %>% mask(.,maize_price_jan_laea)
maize_price_jan_laea; fgate_price
# Save the output
terra::writeRaster(
  fgate_price, 
  "H:/Tanzania Price data/Datasets/Cost_surface data/max_farmgate_price.tif", 
  overwrite = TRUE
)
terra::compareGeom(maize_price_jan_laea, fgate_price)
names(fgate_price) <- "farm_gate_price"
terra::plot(fgate_price)
names(maize_price_jan_laea) <- "Pred_Maize_price_jan"
terra::plot(maize_price_jan_laea)
# Combine the rasters
stacked_rasters <- c(maize_price_jan_laea, fgate_price)
# Plot the combined rasters
terra::plot(stacked_rasters)


# Calculating farmgate prices 2
#transportation cost. I converted 0.02 usd/kg/h to TZS/kg/hr = 54.43 
tr_cost <- 54.43 #TZS/kg/hr

output_directory <- "H:/Tanzania Price data/Datasets/Cost_surface data/farm_gate_prices"

# Initialize an empty list to store individual farmgate price rasters
mystack_list <- list()

# Loop through all points to calculate farmgate prices
for (x in seq_along(maize_price_jan_osm$ID)) {
  # Get the coordinates for the current point
  coords <- terra::crds(town1)[x, ]
  
  # Calculate accumulated cost from the market to the town using accCost from gdistance
  tmp.acc <- gdistance::accCost(tran, fromCoords = coords)/60
  
  # Get the market price for the current point
  market_price <- maize_price_jan_osm$lyr1[x]
  
  # Calculate farmgate price
  fgate_price <- market_price - (tr_cost * tmp.acc)
  fgate_price[fgate_price < 0] <- 0  # Ensure non-negative prices
  
  # Create a SpatRaster from the farmgate price
  fgate_rast <- terra::rast(fgate_price)
  
  # Store the raster in the list
  mystack_list[[x]] <- fgate_rast
  
  # Define the filename for saving the raster
  output_file <- file.path(output_directory, paste0("maize_farmgate_price_", x, ".tif"))
  
  # Save the raster
  terra::writeRaster(fgate_rast, filename = output_file, overwrite = TRUE)
}

mystack <- do.call(c, mystack_list)
plot(mystack)

## maximum cost between all market prices
fgate_list <- list.files(output_directory, pattern = ".tif$", full.names = TRUE)
#combine all rasters
farmgate_prices <- rast(fgate_list)

# maximum price
farmgate_price <- max(farmgate_prices, na.rm=TRUE)

plot(farmgate_price, main="Predicted Farm gate prices Jan 2024")

#Negative values are changed to 0
values(farmgate_price)[values(farmgate_price)<0] <- 0

writeRaster(farmgate_price, "H:/Tanzania Price data/Datasets/Cost_surface data/maize_jan_farmgate_price.tif", overwrite=TRUE)

#plotting map
plot(farmgate_price, main ="Maximum farm price (TZS/ha)")



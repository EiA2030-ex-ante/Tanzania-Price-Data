library(geodata)
library(lubridate)
library(terra)



setwd("C:/Github/chamb244/EiA2030-ex-ante/Tanzania-Price-Data")
list.files(pattern=".csv$")



prices <- read.csv("Tanzania_Price_Data_Maize_with_Coordinates.csv")
dim(prices)
head(prices)
table(prices$Market)
sapply(prices, class)


# fix dates
# Remove the 'th', 'rd', 'nd', 'st' from the date text
prices$Date <- gsub("([0-9]+)(st|nd|rd|th)", "\\1", prices$Date)
# Convert to date format
prices$Date <- dmy(prices$Date)
# note: 287 failed... see if you can find out why


# fix prices -- raw data is price in TSh per 100kg
names(prices)[names(prices) == "Maize..Min.Price."] <- "Maize.price.min"
names(prices)[names(prices) == "Maize..Max.Price."] <- "Maize.price.max"
head(prices)

prices$Maize.price.min <- gsub(",","", prices$Maize.price.min)
prices$Maize.price.max <- gsub(",","", prices$Maize.price.max)

prices$Maize.price.min <- as.numeric(prices$Maize.price.min)
prices$Maize.price.max <- as.numeric(prices$Maize.price.max)
head(prices)
sapply(prices, class)

# convert to price per kg
prices$Maize.price.min <- prices$Maize.price.min/100
prices$Maize.price.max <- prices$Maize.price.max/100

# calculate average of min and max
prices$Maize.pkg <- (prices$Maize.price.min + prices$Maize.price.max) / 2


# bring in raster stack as predictors
geodata_path("C:/data/geodata")
list.files("C:/data/geodata", recursive=TRUE)


# see places
unique(prices[, c("Region", "Market")])
unique(prices[!is.na(prices$Maize.pkg), c("Region", "Market")])
# it looks like spelling needs to be harmonized
unique(prices[, c("Region")])
prices[prices$Region %in% c("Dar es salaam", "Dar es Saalam", "D'Salaam", "Dar Es Salaam",
                            "Dar -es-Salaam", "Dar es Salaam", "Dar es Salaam ", 
                            "Dar es Saalam ", "Dar es Salaam", "Dar es Salaam Temeke", 
                            "Dar es saalam"), c("Region")] <- "Dar es Salaam"
prices[prices$Region %in% c("AB4:S21rusha", "Arusha"), c("Region")] <- "Arusha"
prices[prices$Region %in% c("Ruvi\\uma", "Ruvuma"), c("Region")] <- "Ruvuma"
prices[prices$Region %in% c("Kilimanajaro", "Kilimanajro", "Kilimanjaro"), c("Region")] <- "Kilimanjaro"
unique(prices[, c("Region")])

unique(prices[, c("Market")])
prices[prices$Market %in% c("Arusha (Urban)", "Arusha"), c("Market")] <- "Arusha"
prices[prices$Market %in% c(" Temeke", "Temeke (Tandika)", "Temeke(Tandika)", 
                            "Temeke"), c("Market")] <- "Temeke"
prices[prices$Market %in% c(" Kinondoni", "Kinondoni (Tandale)", "Kinondoni(Tandale)", "Kinondoni"), c("Market")] <- "Kinondoni"
prices[prices$Market %in% c("mbeya", "Mbeya", "SIDO"), c("Market")] <- "Mbeya"
prices[prices$Market %in% c("moshi", "Moshi"), c("Market")] <- "Moshi"
prices[prices$Market %in% c("mwanza", "Mwanza"), c("Market")] <- "Mwanza"
prices[prices$Market %in% c("Ilala (Buguruni)", "Ilala(Buguruni)", "ilala (Buguruni)",
                             "Ilala", "ilala"), c("Market")] <- "Ilala"
prices[prices$Market %in% c("Manispaa Tabora", "Tabora"), c("Market")] <- "Tabora"
prices[prices$Market %in% c("Lindi Mc", "Lindi"), c("Market")] <- "Lindi"
prices[prices$Market %in% c("Bariadi TC", "Bariadi"), c("Market")] <- "Bariadi"
prices[prices$Market %in% c("Mtwara Dc", "Mtwara DC", "Mtwara"), c("Market")] <- "Mtwara"
prices[prices$Market %in% c("Kilimanajaro", "Kilimanajro", "Kilimanjaro"), c("Market")] <- "Kilimanjaro"
unique(prices[, c("Market")])

unique(prices[, c("Region", "Market")])
unique(prices[, c("Region", "Market", "Longitude", "Latitude")])

#We can add dates bu using the year and the month names
prices$Day   <- day(prices$Date)
prices$Month <- month(prices$Date)
prices$Year  <- year(prices$Date)


head(prices)


# convert to data.table and subset as most useful
library(data.table)
prices <- data.table(prices)

# for now, keep only observations in 2023 
prices_2023avg <- prices[(Date >= "2023-01-01") & (Date < "2024-01-01"),][order(Date)]
sum(is.na(prices_2023avg$Maize.pkg))
# drop rows with missing prices
prices_2023avg <- prices_2023avg[!is.na(prices_2023avg$Maize.pkg)]

# take average price for each market
# prices_2023avg <- prices_2023avg[,.(Maize.pkg.avg = mean(Maize.pkg), Latitude = mean(Latitude), Longitude = mean(Longitude)), 
#            by =.(Region, Market)]
prices_2023avg <- prices_2023avg[,.(Maize.pkg.avg = mean(Maize.pkg)), 
           by =.(Region, Market, Longitude, Latitude)]


#Remove the markets with missing coordinates:

prices_2023avg <- na.omit(prices_2023avg)
plot(tza2)
points(prices_2023avg[, c("Longitude", "Latitude")], col="Red", pch=20)


# tza0 <- gadm(country="TZA", level=0)
# tza1 <- gadm(country="TZA", level=1)
# tza2 <- gadm(country="TZA", level=2)
# tza3 <- gadm(country="TZA", level=3)

tza0 <- readRDS("C:/DATA/geodata/gadm/gadm41_TZA_0_pk.rds")
tza1 <- readRDS("C:/DATA/geodata/gadm/gadm41_TZA_1_pk.rds")
tza2 <- readRDS("C:/DATA/geodata/gadm/gadm41_TZA_2_pk.rds")
tza3 <- readRDS("C:/DATA/geodata/gadm/gadm41_TZA_3_pk.rds")
# eth1 <- readRDS("C:/DATA/geodata/gadm/gadm41_ETH_1_pk.rds")
# eth2 <- readRDS("C:/DATA/geodata/gadm/gadm41_ETH_2_pk.rds")
# eth3 <- readRDS("C:/DATA/geodata/gadm/gadm41_ETH_3_pk.rds")



# convert prices observations to vector for mapping
mypts <- vect(prices_2023avg, geom=c("Longitude", "Latitude"), crs=crs(tza0), keepgeom=TRUE)

# see if these show up correctly
plot(tza1)
plot(mypts, col="Red", add=TRUE)


# 
# 
# ## Temporal patterns
# 
# a <- aggregate(prices[,"Maize.pkg", drop=FALSE], prices[,"Date", drop=FALSE], mean, na.rm=TRUE)
# plot(a)
# lines(a)
# 
# #reshape the price data to the wide format, so that each market has a row, and such that we get missing values for months for which prices were not reported.
# 
# pp <- prices[, c("Market", "Maize.pkg", "Date", "Longitude", "Latitude")]
# pp <- pp[order(pp$Date), ]
# pw <- reshape(pp,  timevar = "Date",  idvar = c("Market", "Longitude", "Latitude"), direction = "wide")
# colnames(pw) <- gsub("maize_price.", "", colnames(pw))
# pw[1:4, 1:6]
# 
# # # checking that the columns in the correct order
# # all(as.Date(gsub("Maize.pkg.", "", colnames(pw) )[-c(1:3)] )  |> order() == 1:69)
# 
# # we need a matrix of price values
# markets <- pw[,1:3]
# pm <- as.matrix(pw[,-c(1:3)])
# rownames(pm) <- markets[,1]
# 
# 
# 
# #Now we can have a look at the time series for a single site
# 
# a <- ts(pm[1, ], start=c(2019,1), end=c(2023,12), frequency=12)
# dc <- decompose(a)  
# plot(dc) 
# dc$seasonal[1:12]
# 
# 
# #We compute the seasonal component for all markets
# 
# fseasonal <- function(x) {
#   notna <- !is.na(x)
#   if (sum(notna) < 24) return(rep(NA, 12))
#   notna <- which(notna)
#   
#   start <- notna[1]
#   s <- names(start)
#   syr <- as.integer(substr(s, 1, 4))
#   smth <-  as.integer(substr(s, 6, 7))
#   
#   end <- notna[length(notna)]
#   e <- names(end)
#   eyr <- as.integer(substr(e, 1, 4))
#   emth <-  as.integer(substr(e, 6, 7))
#   
#   x <- x[start:end]
#   a = ts(x, start=c(syr, smth), end=c(eyr, emth), frequency=12)
#   a = zoo::na.StructTS(a)
#   dc <- decompose(a)  
#   dc$seasonal[1:12]
# }
# 
# out <- apply(pm, 1, fseasonal)
# # apply transposes matrices
# dim(out)
# out <- t(out)
# 
# 
# #Link the seasonal data back to the coordinates, and remove the missing cases
# 
# s <- cbind(markets, out)
# head(s)
# 
# 
# #National average seasonal trend
#  
# na <- colMeans(out, na.rm=TRUE)
# plot(na); lines(na)
# 
# 
# 
# 
# pdar <- pm[markets$Market=="Ilala", ] 
# # note: need to redo market names to merge cases where name is rendered differently
# # example: "Ilala(Buguruni)" "ilala (Buguruni)"  "Ilala" "ilala" 
# 
# # relative price for a location for each month
# m <- pm / pdar
# # relative price for a location
# m <- rowMeans(m, na.rm=TRUE)
# p <- cbind(s, spatial=m)
# p <- na.omit(p)


## Interpolate

tza_extent <- ext(tza1) |> floor()
r <- crop(rast(res=1/12), tza_extent)

library(fields)
#xy <- as.matrix(mypts[,c("Longitude", "Latitude")])
xy <- geom(mypts)[,c("y","x")]
#tps <- Tps(xy, p$spatial)
tps <- Tps(xy, mypts$Maize.pkg.avg)
sp <- interpolate(r, tps)
sp <- mask(sp, tza1)
plot(sp)
lines(tza1)


# 
# # inverse distance weighting
# 
# xyz <- cbind(xy, mypts$Maize.pkg)
# sp2 <- interpIDW(r, xyz, 2)
# sp2 <- mask(sp2, tza1)
# plot(sp2)
# lines(tza2)
# 
# 


# Randomforest

library(randomForest) 
#rf <- randomForest(Maize.pkg.avg ~ geom(mypts)[,"x"] + geom(mypts)[,"y"], data=mypts)
rf <- randomForest(Maize.pkg.avg ~ Longitude + Latitude , data=mypts)
sp3 <- interpolate(r, rf, xyNames=c("Longitude", "Latitude"))
sp3 <- mask(sp3, tza1)
plot(sp3)
lines(tza1)



# covariates


library(terra)
# travel <- geodata::travel_time(to="city", size=5, up=TRUE) 
# clm <- geodata::worldclim_country("TZA", "bio")
# area <- geodata::crop_spam("maize", "area", africa=TRUE)
# yield <- geodata::crop_spam("maize", "yield", africa=TRUE)

travel <- rast("C:/DATA/geodata/travel/travel_time_to_cities_u5.tif")
clm    <- rast("C:/DATA/geodata/TRUE/wc2.1_country/TZA_wc2.1_30s_bio.tif")
area   <- rast("C:/DATA/geodata/spam/spam2017V1r1_SSA_gr_H_MAIZ_A.tif")
yield  <- rast("C:/DATA/geodata/spam/spam2017V1r1_SSA_gr_H_MAIZ_R.tif")

names(travel) <- c("traveltime_u5") ## travel time cities of 100k or more
names(clm) <- gsub("wc2.1_30s_", "", names(clm))
names(area) <- c("MAI_ARE") # SPAM maize area 2010
names(yield)  <- c("MAI_YLD") # SPAM maize yield 2010

comment(travel) <- "travel time to cities 100k or more"

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

#compareGeom(travel, clm, area, yield)
# harmonize to ET boundaries and common resolution
travel <- resample(travel, r)
clm    <- resample(clm, r)
area   <- resample(area, r)
freq(is.na(area))
area <- classify(area, cbind(NA,0)) 
yield  <- resample(yield, r)
freq(is.na(yield))
yield <- classify(yield, cbind(NA,0)) 
# check again 
compareGeom(travel, clm, area, yield)


# RandomForest and TPS
  
library(terra)
library(randomForest)

#generate lat grid, lon grid: 
latgrd <- longrd <- r
latgrd[] <- yFromCell(latgrd, 1:ncell(latgrd))
longrd[] <- xFromCell(longrd, 1:ncell(longrd))
names(latgrd) <- c("latitude")
names(longrd) <- c("longitude")

rstack <- c(travel, clm, area, yield, latgrd, longrd)
names(rstack)

# create focal mean to extract from (as alternative to using buffers for extraction, which are not supported in terra)
fm <- focalMat(r, d=0.18, type='circle', fillNA=FALSE)
rstack2 <- focal(rstack, w=fm, fun="mean", na.policy="all", fillvalue=NA, # na.rm=TRUE,
                 expand=TRUE, silent=FALSE) #, filename="", overwrite=FALSE) 


# create vector for points data
#mypoints <- vect(p, geom=c("longitude", "latitude"), crs="+proj=longlat +datum=WGS84 +no_defs")
plot(tza1, axes=TRUE)
plot(mypts, add=TRUE, pch=20, col="Red")
head(mypts)

# extract values to dataset -- use a 20km buffer
# do a focal sum of 20km radius  - this is about 0.18 of a decimal degree... 0.18*112=20.16
fm <- focalMat(r, d=0.18, type='circle', fillNA=FALSE)
rstack2 <- focal(rstack2, w=fm, fun="sum", na.policy="all", fillvalue=NA, na.rm=TRUE,
                 expand=TRUE, silent=FALSE) #, filename="", overwrite=FALSE) 
# rstack2 <- focal(rstack, w=5, fun="mean", na.policy="all", fillvalue=NA, na.rm=TRUE,
#       expand=TRUE, silent=FALSE) #, filename="", overwrite=FALSE) 

#### !!!! 
#### I need to figure out how to remove NAs from the SPAM data before the focal sum 

extr1 <- extract(rstack, mypts, method="bilinear")

mypts <- cbind(mypts, extr1)
head(mypts)

rf <- randomForest(Maize.pkg.avg ~ traveltime_u5 + bio_1 + bio_2 + bio_3 + 
                     bio_4 + bio_5 + bio_6 + bio_7 + bio_8 + bio_9 + bio_10 + 
                     bio_11 + bio_12 + bio_13 + bio_14 + bio_15 + bio_16 + 
                     bio_17 + bio_18 + bio_19 + MAI_ARE + MAI_YLD + longitude + latitude, 
                   data=mypts, na.rm=TRUE)
# # missing values...
# mypoints$MAI_ARE
## evaluate
rf
varImpPlot(rf)

# estimate more parsimonious specification
rf <- randomForest(Maize.pkg.avg ~ traveltime_u5 + 
                     bio_6 + bio_19 + 
                     longitude + latitude, 
                   data=mypts, na.rm=TRUE)
# evaluate 
rf
varImpPlot(rf)
partialPlot(rf, as.data.frame(mypts), "traveltime_u5")

## spatial prediction
pred1 <- predict(rstack, rf, na.rm=TRUE)

pred1 <- mask(pred1, tza1)
plot(pred1)
plot(mypts, add=TRUE, pch=20, col="Red")




  
  
  
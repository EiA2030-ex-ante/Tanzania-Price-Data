library(geodata)
library(lubridate)
library(terra)
library(data.table)
library(randomForest)



setwd("C:/Github/chamb244/EiA2030-ex-ante/Tanzania-Price-Data")
list.files(pattern=".csv$")



prices <- fread("Tanzania_Price_Data_AllCrops_with_Coordinates.csv")
dim(prices)
head(prices)
table(prices$Market)
sapply(prices, class)

prices <- prices[,!c("V22")]

# TEMPORARY....!!!!
unique(prices[Region=="Lindi",.(Market, Latitude, Longitude)])
# for now replace Lindi values which are in the middle of nowwhere
prices[Region=="Lindi", Latitude:= -9.995]
prices[Region=="Lindi", Longitude:=  39.708]

unique(prices[Region=="Dar es Salaam",.(Market, Latitude, Longitude)])
# Tandika market is in the wrong place!! way down in Lindi
prices[Market=="Tandika", Latitude:= -6.867911575091655]
prices[Market=="Tandika", Longitude:=  39.25480305833879]

unique(prices[Region=="Mwanza",.(Market, Latitude, Longitude)])
# Mwanza Mkoa in wrong place
prices[Region=="Mwanza", Latitude:= -2.51969]
prices[Region=="Mwanza", Longitude:=  32.90144]

unique(prices[Market == "Mkoa",.(Region, Market, Latitude, Longitude)])
unique(prices[Region == "Njombe",.(Region, Market, Latitude, Longitude)])
# Njombe Mkoa in wrong place
prices[Region=="Njombe", Latitude:= -9.12109]
prices[Region=="Njombe", Longitude:=  34.43168]


# fix dates
# Remove the 'th', 'rd', 'nd', 'st' from the date text
#prices$Date <- gsub("([0-9]+)(st|nd|rd|th)", "\\1", prices$Date)
# Convert to date format
prices$Date <- lubridate::mdy(prices$Date)
# note: 287 failed... see if you can find out why


# fix prices -- raw data is price in TSh per 100kg
head(prices)

# for (c in c("Maize","Rice","Sorghum", "Bulrush.Millet", "Finger.Millet", "Wheat", "Beans", "Irish.Potatoes")) {
#   print(c)
#   paste0("prices$",c,"..min.price") <- as.numeric(paste0("prices$",c,"..min.price"))
# }


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

names(prices)
sapply(prices, class)
sapply(prices[,c(3:18)], class)

#convert to numeric .... REDO THIS WITH SAPPLY ON COLS
#not working: sapply(prices[,c(3:18)], function(x) as.numeric(x))

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
# divby100 <- function(x) (x/100)
# prices[,c(3:18)] <- sapply(prices[,c(3:18)], divby100) 
       

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
prices$mai.price <- (prices$mai.price.max + prices$mai.price.max) / 2
prices$ric.price <- (prices$ric.price.max + prices$ric.price.max) / 2
prices$sor.price <- (prices$sor.price.max + prices$sor.price.max) / 2
prices$bul.price <- (prices$bul.price.max + prices$bul.price.max) / 2
prices$fin.price <- (prices$fin.price.max + prices$fin.price.max) / 2
prices$whe.price <- (prices$whe.price.max + prices$whe.price.max) / 2
prices$bea.price <- (prices$bea.price.max + prices$bea.price.max) / 2
prices$pot.price <- (prices$pot.price.max + prices$pot.price.max) / 2




# bring in raster stack as predictors
geodata_path("C:/data/geodata")
list.files("C:/data/geodata", recursive=TRUE)


# see places
unique(prices[, c("Region", "Market")])
# it looks like spelling needs to be harmonized

unique(prices[, c("Region", "Market")])
unique(prices[, c("Region", "Market", "Longitude", "Latitude")])

#We can add dates bu using the year and the month names
prices$Day   <- day(prices$Date)
prices$Month <- month(prices$Date)
prices$Year  <- year(prices$Date)



# drop unneccessary columns
head(prices)
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

unique(prices.monthly.long$Crop)
## something wrong about the levels here.... has both old and new values
# not working: droplevels(prices.monthly.long$Crop)

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

# tza0 <- gadm(country="TZA", level=0)
# tza1 <- gadm(country="TZA", level=1)
# tza2 <- gadm(country="TZA", level=2)
# tza3 <- gadm(country="TZA", level=3)

tza0 <- readRDS("C:/DATA/geodata/gadm/gadm41_TZA_0_pk.rds")
tza1 <- readRDS("C:/DATA/geodata/gadm/gadm41_TZA_1_pk.rds")
tza2 <- readRDS("C:/DATA/geodata/gadm/gadm41_TZA_2_pk.rds")
tza3 <- readRDS("C:/DATA/geodata/gadm/gadm41_TZA_3_pk.rds")


# convert prices observations to vector for mapping
mypts <- vect(prices.monthly.long, geom=c("Longitude", "Latitude"), crs=crs(tza0), keepgeom=TRUE)

# see if these show up correctly
plot(tza1)
plot(mypts, col="Red", add=TRUE)
text(mypts, label="Market")

# Mkoa still in wrong place
unique(as.data.table(mypts)[,.(Region, Market)])

# create reference raster
tza_extent <- ext(tza1) |> floor()
r <- crop(rast(res=1/12), tza_extent)




## Interpolate

library(fields)
#xy <- as.matrix(mypts[,c("Longitude", "Latitude")])
xy <- geom(mypts)[,c("y","x")]
#tps <- Tps(xy, p$spatial)
tps <- Tps(xy, mypts$pkg)
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

#rf <- randomForest(Maize.pkg.avg ~ geom(mypts)[,"x"] + geom(mypts)[,"y"], data=mypts)
rf <- randomForest(pkg ~ Longitude + Latitude , data=mypts)
sp3 <- interpolate(r, rf, xyNames=c("Longitude", "Latitude"))
sp3 <- mask(sp3, tza1)
plot(sp3)
lines(tza1)



# covariates


# ttc_1 <- geodata::travel_time(to="city", size=5, up=TRUE) 
# ttc_u2 <- geodata::travel_time(to="city", size=5, up=TRUE)
# ttc_u3 <- geodata::travel_time(to="city", size=5, up=TRUE)
# ttc_u4 <- geodata::travel_time(to="city", size=5, up=TRUE)
# ttc_u5 <- geodata::travel_time(to="city", size=5, up=TRUE)
# ttp_1 <- geodata::travel_time(to="port", size=1) 
# ttp_u2 <- geodata::travel_time(to="port", size=1, up=TRUE) 

# clm <- geodata::worldclim_country("TZA", "bio")
# area <- geodata::crop_spam("maize", "area", africa=TRUE)
# yield <- geodata::crop_spam("maize", "yield", africa=TRUE)

ttcity <- rast("C:/DATA/geodata/travel/travel_time_to_cities_u5.tif")
ttport <- rast("C:/DATA/geodata/travel/travel_time_to_cities_1.tif")
clm    <- rast("C:/DATA/geodata/TRUE/wc2.1_country/TZA_wc2.1_30s_bio.tif")
area   <- rast("C:/DATA/geodata/spam/spam2017V1r1_SSA_gr_H_MAIZ_A.tif")
yield  <- rast("C:/DATA/geodata/spam/spam2017V1r1_SSA_gr_H_MAIZ_R.tif")
popd  <- rast("C:/DATA/geodata/pop/gpw_v4_population_density_rev11_2020_10m.tif")

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

#compareGeom(travel, clm, area, yield)
# harmonize to national boundaries and common resolution
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


# RandomForest and TPS

library(terra)
library(randomForest)

#generate lat grid, lon grid: 
latgrd <- longrd <- r
latgrd[] <- yFromCell(latgrd, 1:ncell(latgrd))
longrd[] <- xFromCell(longrd, 1:ncell(longrd))
names(latgrd) <- c("latitude")
names(longrd) <- c("longitude")


# predictor stack
rstack <- c(ttcity, ttport, clm, area, yield, popd, latgrd, longrd)
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

#define Month as a factor
mypts$Month <- as.factor(mypts$Month)
is.factor(mypts$Month)
levels(mypts$Month)

# because of problems with using factors as rasters for spatial predictions
# use dummies for Crop and Month instead of factors




# drop levels that don't exist in Crop field
mypts$Crop <- mypts$Crop[,drop=TRUE]
levels(mypts$Crop)

# rf <- randomForest(pkg ~ Crop + Month + Year + 
#                      ttcity_u5 + ttport_1 + bio_1 + bio_2 + bio_3 + 
#                      bio_4 + bio_5 + bio_6 + bio_7 + bio_8 + bio_9 + bio_10 + 
#                      bio_11 + bio_12 + bio_13 + bio_14 + bio_15 + bio_16 + 
#                      bio_17 + bio_18 + bio_19 + MAI_ARE + MAI_YLD + 
#                      Longitude + Latitude, 
#                    data=mypts, na.rm=TRUE)

rf <- randomForest(pkg ~ maize + rice + sorghum + bmillet + fmillet + wheat + beans + potato +
                     jan + feb + mar + apr + may + jun + jul + aug + sep + oct + nov + dec +
                     Year + 
                     ttcity_u5 + ttport_1 + bio_1 + bio_2 + bio_3 + 
                     bio_4 + bio_5 + bio_6 + bio_7 + bio_8 + bio_9 + bio_10 + 
                     bio_11 + bio_12 + bio_13 + bio_14 + bio_15 + bio_16 + 
                     bio_17 + bio_18 + bio_19 + MAI_ARE + MAI_YLD + 
                     Longitude + Latitude, 
                   data=mypts, na.rm=TRUE)

# # missing values...
# mypoints$MAI_ARE
## evaluate
rf
varImpPlot(rf)

# estimate more parsimonious specification
# rf <- randomForest(pkg ~ Crop + Month + Year + 
#                      ttcity_u5 + 
#                      bio_6 + bio_2 + bio_9 + bio_7 + 
#                      longitude + latitude, 
#                    data=mypts, na.rm=TRUE)

rf <- randomForest(pkg ~ maize + rice + sorghum + bmillet + fmillet + wheat + beans + potato +
                     jan + feb + mar + apr + may + jun + jul + aug + sep + oct + nov + dec +
                     Year + 
                     ttcity_u5 + ttport_1 + 
                     bio_3 + bio_6 + bio_9 + 
                     longitude + latitude, 
                   data=mypts, na.rm=TRUE)

# evaluate 
rf
varImpPlot(rf)
partialPlot(rf, as.data.frame(mypts), "ttcity_u5")
partialPlot(rf, as.data.frame(mypts), "ttport_1")
# partialPlot(rf, as.data.frame(mypts), "Crop")
# partialPlot(rf, as.data.frame(mypts), "Month")
# partialPlot(rf, as.data.frame(mypts), "Year")


## spatial prediction
# pred1 <- predict(rstack, rf, const=data.frame(Year=2023), na.rm=TRUE)
#pred1 <- predict(rstack, rf, const=data.frame(Crop=as.factor("Maize"), Month=as.factor(3), Year=2023), na.rm=TRUE)
pred1 <- predict(rstack, rf, 
                 const=data.frame(maize=1, rice=0, sorghum=0, bmillet=0, fmillet=0, wheat=0, beans=0, potato=0,
                                  jan=0, feb=0, mar=0, apr=0, may=0, jun=1, jul=0, aug=0, sep=0, oct=0, nov=0, dec=0,
                                  Year=2023), 
                 na.rm=TRUE)

pred1 <- mask(pred1, tza1)
plot(pred1)
plot(mypts, add=TRUE, pch=20, col="Red")
#text(mypts, labels="Region", pos=1)

# compare with beans price for same month
pred2 <- predict(rstack, rf, 
                 const=data.frame(maize=0, rice=0, sorghum=0, bmillet=0, fmillet=0, wheat=0, beans=1, potato=0,
                                  jan=0, feb=0, mar=0, apr=0, may=0, jun=1, jul=0, aug=0, sep=0, oct=0, nov=0, dec=0,
                                  Year=2023), 
                 na.rm=TRUE)

pred2 <- mask(pred2, tza1)
plot(pred2)
plot(mypts, add=TRUE, pch=20, col="Red")
#text(mypts, labels="Region", pos=1)





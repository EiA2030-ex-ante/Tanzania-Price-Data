* explore price predictions
clear all 
cd "C:\Github\chamb244\EiA2030-ex-ante\Tanzania-Price-Data\"
import delimited "Tanzania_Price_Data_AllCrops_With_Coordinates.csv"
cap drop v22

* remove duplicate rows
duplicates drop

rename date date_string
gen date = date(date_string, "MDY", 2000)
gen month = month(date)
gen year = year(date)
order date month year, after(date_string)

cap drop maipkg 
cap drop ricpkg 
cap drop sorpkg 
cap drop bulpkg 
cap drop finpkg 
cap drop whepkg 
cap drop beapkg 
cap drop potpkg 

egen maipkg = rowmean(maizeminprice   maizemaxprice   )
egen ricpkg = rowmean(riceminprice    ricemaxprice    )
egen sorpkg = rowmean(sorghumminprice sorghummaxprice )
egen bulpkg = rowmean(bulrushm~nprice bulrushm~xprice )
egen finpkg = rowmean(fingermi~nprice fingermi~xprice )
egen whepkg = rowmean(wheatminprice   wheatmaxprice   )
egen beapkg = rowmean(beansminprice   beansmaxprice   )
egen potpkg = rowmean(irishpot~nprice irishpot~xprice )

drop *price

* correlations across prices 
pwcorr *pkg, star(.01)
scatter maipkg beapkg
tw scatter maipkg ricpkg || scatter maipkg sorpkg


rename maipkg pkg1
rename ricpkg pkg2 
rename sorpkg pkg3 
rename bulpkg pkg4
rename finpkg pkg5 
rename whepkg pkg6 
rename beapkg pkg7 
rename potpkg pkg8 


* reshape 
reshape long pkg, i(region market date) j(crop)

label define CROP 1 "maize" 2 "rice" 3 "sorghum" 4 "bulrush millet" 5 "finger millet" 6 "wheat" 7 "beans" 8 "irish potato", replace
la values crop CROP 

* montly means
collapse pkg, by(crop month year region market latitude longitude)

* aspatial model
reg pkg i.crop i.month year c.latitude##c.longitude


/* export market locations for mapping:
preserve
gen obs=1
collapse (sum) obs, by(region market latitude longitude)
export delimited using "C:\Github\chamb244\EiA2030-ex-ante\Tanzania-Price-Data\market_locations_for_mapping.csv"
restore
*/


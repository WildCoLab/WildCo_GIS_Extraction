###################################################################
# NDVI/EV/LAI/whatever you can get your hands on from MODIStools
library(MODISTools)

# list available bands for a product
bands <- mt_bands(product = "MOD15A2H") #MOD13Q1
head(bands)

# https://lpdaac.usgs.gov/products/mod13q1v006/ ndv/evi
# https://lpdaac.usgs.gov/products/mod15a2hv006/ lai
# The algorithm chooses the best available pixel value from all the acquisitions from the 16 day period. 
# The criteria used is low clouds, low view angle, and the highest NDVI/EVI value.

# Import station locations
sta<- read.csv("Station_Covariates_Feb_2020_STANDARD.csv")
tmp <- sta[,which(colnames(sta)%in%c("Deployment.Location.ID", "Longitude", "Latitude"))]
colnames(tmp) <- c("site_name", "lon", "lat")
tmp <- tmp[,c(1,3,2)]
str(tmp)

# list available dates for a prodcut at a location
dates <- mt_dates(product = "MOD15A2H", lat = 56.47384, lon = -112.5752) #MOD13Q1
head(dates)
tail(dates)

#### Get LAI data ##########################################################

algar_lai <- mt_batch_subset(product = "MOD15A2H", #MOD13Q1
                             df=tmp,
                             band = "Lai_500m",
                             start = "2015-01-11",
                             end = "2019-11-30",
                             km_lr = 0,
                             km_ab = 0,
                             internal = TRUE)

# rough plot
par(mar=c(5,4,1,1))
par(mfrow=c(1,1))
hist(algar_lai$value)

algar_lai$calendar_date <- strptime(algar_lai$calendar_date, "%Y-%m-%d")
plot(algar_lai$calendar_date, algar_lai$value, las=1, ylab="LAI", pch=19, type="n")
for(i in 1:length(unique(algar_lai$site))){
  points(algar_lai$calendar_date[algar_lai$site==unique(algar_lai$site)[i]], algar_lai$value[algar_lai$site==unique(algar_lai$site)[i]], pch=19, col=rgb(0,0,0,0.1))
}

######## Get NDVI#######################################################

algar_ndvi <- mt_batch_subset(product = "MOD13Q1",
                              df=tmp,
                              band = "250m_16_days_NDVI",
                              start = "2015-01-11",
                              end = "2019-11-30",
                              km_lr = 0,
                              km_ab = 0,
                              internal = TRUE)
# rough plot
par(mar=c(5,4,1,1))
par(mfrow=c(1,1))
hist(algar_ndvi$value)

# Remove stuff that is below zero, which are indicative of water
algar_ndvi <- algar_ndvi[algar_ndvi$value>0,]

algar_ndvi$calendar_date <- strptime(algar_ndvi$calendar_date, "%Y-%m-%d")
plot(algar_ndvi$calendar_date, algar_ndvi$value, las=1, ylab="NDVI", pch=19, type="n")
for(j in 1:length(unique(algar_ndvi$site)))
{
  points(algar_ndvi$calendar_date[algar_ndvi$site==unique(algar_ndvi$site)[j]], algar_ndvi$value[algar_ndvi$site==unique(algar_ndvi$site)[j]], pch=19, col=rgb(0,0,0,0.1))
}

######## Get EVI###########################################################

algar_evi <- mt_batch_subset(product = "MOD13Q1",
                             df=tmp,
                             band = "250m_16_days_EVI",
                             start = "2015-01-11",
                             end = "2019-11-30",
                             km_lr = 0,
                             km_ab = 0,
                             internal = TRUE)

hist(algar_evi$value)

# Remove stuff that is below zero, which similar to NDVI mean nonveg like water, rock, etc... 
algar_evi <- algar_evi[algar_evi$value>0,]

algar_evi$calendar_date <- strptime(algar_evi$calendar_date, "%Y-%m-%d")

plot(algar_evi$calendar_date, algar_evi$value, las=1, ylab="evi", pch=19, type="n")
for(k in 1:length(unique(algar_evi$site))){
  points(algar_evi$calendar_date[algar_evi$site==unique(algar_evi$site)[k]], algar_evi$value[algar_evi$site==unique(algar_evi$site)[k]], pch=19, col=rgb(0,0,0,0.1))
}


##### plitty plots#####

par(mfrow=c(2,1))
plot(algar_evi$calendar_date, algar_evi$value, las=1, ylab="evi", pch=19, type="n")
for(i in 1:length(unique(algar_evi$site))){
  points(algar_evi$calendar_date[algar_evi$site==unique(algar_evi$site)[i]], algar_evi$value[algar_evi$site==unique(algar_evi$site)[i]], pch=19, col=rgb(0,0,0,0.1))
}

plot(algar_ndvi$calendar_date, algar_ndvi$value, las=1, ylab="NDVI", pch=19, type="n")
for(i in 1:length(unique(algar_ndvi$site))){
  points(algar_ndvi$calendar_date[algar_ndvi$site==unique(algar_ndvi$site)[i]], algar_ndvi$value[algar_ndvi$site==unique(algar_ndvi$site)[i]], pch=19, col=rgb(0,0,0,0.1))
}

## Clustering real data

library(dplyr)
library(tidyr)
library(lubridate)

dt <- tbl_df(data)

summary(dt)

u <- dt %>% filter(Latitude == '60,206611600000002')
## Ulvilantie

s <- dt %>% 
     group_by(postinumero) %>%
     summarize(count = n()) %>%
     arrange(desc(count))
     #mutate (match = agrepl("ulvilantie", katuosoite, max.distance = 0.1, ignore.case = T)) %>%
#filter(dt, agrepl("00370", postinumero, max.distance = 0.01, ignore.case = T) )
     #filter (match == 1)

## 471 riviä
#rm(lassila)
lassila <- filter(dt, postinumero == "00440")
lassila$postinumero <- "00440"
lassila$paikkakunta <- "Helsinki"

huonot <- filter(lassila, asunnon_koko > 2000 | asunnon_koko == 0)
huonot <- rbind(huonot, filter(lassila, rakennusvuosi == 0))

rakvuodet <- filter(lassila, rakennusvuosi != 0) %>%
     group_by(katuosoite) %>% 
     summarise(rakvuosi = min(rakennusvuosi)) %>%
     arrange(rakvuosi)

lassila <- left_join(lassila, rakvuodet, by="katuosoite") 

lassila %>% 
     filter(rakennusvuosi != rakvuosi)

osoitteet <- unique(select(lassila, katuosoite:paikkakunta))

source('E:/Github/SpatialClustering/getGeoDetails.R')

addresses <- paste(osoitteet$katuosoite,
                              osoitteet$postinumero,
                              osoitteet$paikkakunta)
getGeoDetails("Vahtirinne 00370 Helsinki")

geocoded <- data.frame()
startindex <- 1
# Start the geocoding process - address by address. geocode() function takes care of query speed limit.
for (ii in seq(startindex, length(addresses))){
     print(paste("Working on index", ii, "of", length(addresses)))
     #query the google geocoder - this will pause here if we are over the limit.
     result = getGeoDetails(addresses[ii]) 
     print(result$status)     
     result$index <- ii
     #append the answer to the results file.
     geocoded <- rbind(geocoded, result)
     #save temporary results as we are going along
}

library(RgoogleMaps)

map <- GetMap(center=c(lat = 60, lon = 24), zoom=11)
PlotOnStaticMap(map)

60.22944     24.89156
60,22943     24,89163
summary(lassila)

60.23217     24.88451
60,23217     24,88709

rm(reimala)
reimarla <- filter(dt, postinumero == "00370")

reimarla$postinumero <- "00370"
reimarla$paikkakunta <- "Helsinki"

huonot <- filter(reimarla, asunnon_koko > 2000 | asunnon_koko == 0)
huonot <- rbind(huonot, filter(reimarla, rakennusvuosi == 0))

rakvuodet2 <- filter(reimarla, rakennusvuosi != 0) %>%
     group_by(katuosoite) %>% 
     summarise(rakvuosi = min(rakennusvuosi)) %>%
     arrange(rakvuosi)

reimarla <- left_join(reimarla, rakvuodet2, by="katuosoite") 

reimarla %>% 
     filter(rakennusvuosi != rakvuosi)

filter(reimarla, is.na(kauppahinta))

getGeoDetails(addresses[ii])

## Second spatial clustering
library(sp)
lassila$Latitude <- as.numeric(sub(",", ".", lassila$Latitude))
lassila$Longitude <- as.numeric(sub(",", ".", lassila$Longitude))
dt2 <- as.data.frame(filter(lassila, !is.na(Latitude), !is.na(Longitude)))
coordinates(dt2) <- ~Latitude+Longitude
cdat <- data.frame(Latitude=coordinates(dt2)[,1],Longitude=coordinates(dt2)[,2])
rownames(cdat) <- rownames(dt2@data)

chc <- hclust(dist(cdat))
## KNN
chc.n <- cutree(chc, k=6)

chc.d200 <- cutree(chc, h=200)

dt2@data <- data.frame(dt2@data, KNN=as.factor(chc.n), DClust=chc.d200)

par(mfcol=c(1,2))      
plot(dt2, col=factor(dt2@data$KNN), pch=19)
box(col="black")
title(main="KNN Clustering")
legend("topleft", legend=paste("Cluster", 1:6,sep=""), col=palette()[1:6], pch=rep(19,6), bg="white")
pRamp <- colorRampPalette(c("blue","yellow","red"))
Col <- pRamp(20)[as.numeric(cut(dt2@data$DClust, breaks=20))]
plot(dt2, col=Col, pch=19)
box(col="black")
title(main="Distance Clustering k=70")

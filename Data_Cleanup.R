nyc_collisions <- read.csv("NYPD_Motor_Vehicle_Collisions.csv")
#Loading necessary files
acceptable_rng = 0.01

dif <- function(x) return(max(x) - min(x))
#Creating a function to give range of difference

dim(subset(nyc_collisions[!is.na(nyc_collisions$LATITUDE),]))
#identifying how many columns have longtiude and latitude
# 765413 rows wich transaltes to a 79.2%

nyc_collisions$FULL.STREET <- paste(nyc_collisions$ON.STREET.NAME, nyc_collisions$CROSS.STREET.NAME, sep = " ")
#Creating a new column to combine On street Name and cross Street Name

withlocation <- subset(nyc_collisions, !is.na(nyc_collisions$LATITUDE))
#subseting all the rows that have location data to create a lookup table

uniquestreet <- as.data.frame(as.list(aggregate(cbind(LONGITUDE,LATITUDE) ~ FULL.STREET, data = withlocation, FUN = function(x) c(mn = mean(x), dif = dif(x)))))
#creating a lookup table of the unique street and cross street with longtitude and latitude data

uniquestreet <- subset(uniquestreet, uniquestreet$LONGITUDE.dif <= acceptable_rng & uniquestreet$LATITUDE.dif <= acceptable_rng, )
#Removing cases where the range was greater than 0.01

uniquestreet <- subset(uniquestreet, ,-c(LONGITUDE.dif,LATITUDE.dif))

uniquestreet <- subset(uniquestreet, uniquestreet$FULL.STREET != " ")
#Removing the column with blank street name

colnames(uniquestreet) <- c("FULL.STREET","LONGITUDE","LATITUDE")



nyc_collisions <- merge(uniquestreet, nyc_collisions, by = "FULL.STREET", all.y = TRUE)
#looking up values from unique street for nyc_collisions by FULL.STREET and keeping all rows of nyc_collisions

rm(uniquestreet)
#Removing unique street data frame as it is no longer required

nyc_collisions$LATITUDE.y[is.na(nyc_collisions$LATITUDE.y)] <- nyc_collisions$LATITUDE.x[is.na(nyc_collisions$LATITUDE.y)]
nyc_collisions$LONGITUDE.y[is.na(nyc_collisions$LONGITUDE.y)] <- nyc_collisions$LONGITUDE.x[is.na(nyc_collisions$LONGITUDE.y)]
#replacing LATITUDE AND LONGITUDE that are NA with values from the new columns

nyc_collisions <- subset(nyc_collisions, ,-c(FULL.STREET,LATITUDE.x,LONGITUDE.x))
#Removing additional columns

names(nyc_collisions)[names(nyc_collisions) == "LATITUDE.Y"] <- "LATITUDE"
names(nyc_collisions)[names(nyc_collisions) == "LATITUDE.Y"] <- "LATITUDE"
#Renaming columns changed by the merge function

dim(subset(nyc_collisions[!is.na(nyc_collisions$LATITUDE),]))
#Checking how many rows and columns now have location
#807929 rows or 83.59%

#we will now repeat this process with Zipcodes and boroughs
uniquezip <- as.data.frame(as.list(aggregate(cbind(LONGITUDE,LATITUDE) ~ ZIP.CODE, data = withlocation, FUN = function(x) c(mn = mean(x), rng = dif(x)))))
#Getting mean and ranges by zipcode

uniquezip <- subset(uniquezip, uniquezip$LONGITUDE.rng < acceptable_rng & uniquezip$LATITUDE.rng < acceptable_rng)
#Removing all ranges over 0.01

uniquezip <- subset(uniquezip,, select = -c(LONGITUDE.rng,LATITUDE.rng))
#Removing range columns

colnames(uniquezip) <- c("ZIP.CODE","LONGITUDE","LATITUDE")
#renameing columns to remove the .mn

nyc_collisions <- merge(uniquezip,nyc_collisions,by="ZIP.CODE", all.y = TRUE)
#Merging with orignal dataset

nyc_collisions$LONGITUDE.y[is.na(nyc_collisions$LONGITUDE.y)] <- nyc_collisions$LONGITUDE[is.na(nyc_collisions$LONGITUDE.y)]
nyc_collisions$LATITUDE.y[is.na(nyc_collisions$LATITUDE.y)] <- nyc_collisions$LATITUDE[is.na(nyc_collisions$LATITUDE.y)]

nyc_collisions <- subset(nyc_collisions, ,-c(LATITUDE,LONGITUDE))
#Removing additional columns

names(nyc_collisions)[names(nyc_collisions) == "LATITUDE.y"] <- "LATITUDE"
names(nyc_collisions)[names(nyc_collisions) == "LONGITUDE.y"] <- "LONGITUDE"
#Renaming columns changed by the merge function

dim(subset(nyc_collisions[!is.na(nyc_collisions$LATITUDE),]))
#Checking how many rows and columns now have location
#808005 rows or 83.60%

rm(uniquezip)
rm(withlocation)
rm(acceptable_rng)
rm(dif)
#Removing all additional dataframes and variables
nyc_collisions$LOCATION <- as.character(nyc_collisions$LOCATION)
nyc_collisions$LOCATION[!is.na(nyc_collisions$LATITUDE)] <- paste(as.character(nyc_collisions$LATITUDE[!is.na(nyc_collisions$LATITUDE)]),",",as.character(nyc_collisions$LONGITUDE[!is.na(nyc_collisions$LATITUDE)]))

library(ggmap)
library(ggplot2)


# Histograms HourWise and MonthWise

ggplot(nyc , aes(x  = as.numeric(Hour))) + geom_histogram(stat = 'count')
ggplot(nyc , aes(x  = as.numeric(MONTH))) + geom_histogram(stat = 'count', bins = 12)

# Plotting injuries by vehicle count

#create new vehicle count variable
nyc$vehicle.count <- pmax(as.numeric(nyc$VEHICLE.TYPE.CODE.1 != "") + 
                            as.numeric(nyc$VEHICLE.TYPE.CODE.2 != "") +
                            as.numeric(nyc$VEHICLE.TYPE.CODE.3 != "") +
                            as.numeric(nyc$VEHICLE.TYPE.CODE.4 != "") +
                            as.numeric(nyc$VEHICLE.TYPE.CODE.5 != ""),
                          as.numeric(nyc$CONTRIBUTING.FACTOR.VEHICLE.1 != "") + 
                            as.numeric(nyc$CONTRIBUTING.FACTOR.VEHICLE.2 != "") +
                            as.numeric(nyc$CONTRIBUTING.FACTOR.VEHICLE.3 != "") +
                            as.numeric(nyc$CONTRIBUTING.FACTOR.VEHICLE.4 != "") +
                            as.numeric(nyc$CONTRIBUTING.FACTOR.VEHICLE.5 != ""))

# Plot the data
hist(nyc$vehicle.count[nyc$NUMBER.OF.PERSONS.KILLED > 0])

# Plotting accidents yearwise
data_datewise <- nyc %>%
  filter(distraction == 1) %>%
  group_by(YEAR,MONTH) %>%
  summarise(number.of.accidents = n())

ggplot(data_datewise,aes(MONTH,number.of.accidents, group = 1))+
  geom_line(col=rgb(255,0,0,75,maxColorValue=255),pch=19,cex=1.5)+
  xlab("MONTH")+
  ylab("Accidents")+
  facet_wrap(~YEAR)+
  ggtitle("injuries by YEAR")+
  theme_bw()

# Plotting number of accidents over the years with regression line

month_vector <- c("Jan","Feb","Mar","Apr","May","Jun","July","Aug","Sep","Oct","Nov","Dec")
data_datewise$MONTH_char <- month_vector

data_datewise_char <- unite(data_datewise,new_period, MONTH_char, YEAR ,  sep = '-')


factor_level <- unique(data_datewise_char$new_period)
data_datewise_char$new_period <- factor(data_datewise_char$new_period, levels = factor_level)

ggplot(data_datewise_char , aes(x = new_period , y = number.of.accidents, group = 1)) + 
  geom_line(col=rgb(255,0,0,75,maxColorValue=255),pch=19,cex=1.5) +
  geom_smooth(method = 'lm', pch=19,cex=1.5) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1), plot.title = element_text(hjust = 0.5)) +
  xlab("Month-Year") + ylab("Number of Accidents") +
  ggtitle("Number of Accidents over the years")


# Accidents due to driver distraction
month_vector <- c("Jan","Feb","Mar","Apr","May","Jun","July","Aug","Sep","Oct","Nov","Dec")
data_datewise$MONTH_char <- month_vector

data_datewise_char <- unite(data_datewise,new_period, MONTH_char, YEAR ,  sep = '-')


factor_level <- unique(data_datewise_char$new_period)
data_datewise_char$new_period <- factor(data_datewise_char$new_period, levels = factor_level)

ggplot(data_datewise_char , aes(x = new_period , y = number.of.accidents, group = 1)) + 
  geom_line(col=rgb(255,0,0,75,maxColorValue=255),pch=19,cex=1.5) +
  geom_smooth(method = 'lm', pch=19,cex=1.5) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1), plot.title = element_text(hjust = 0.5)) +
  xlab("Month-Year") + ylab("Number of Accidents") +
  ggtitle("Number of Accidents due to driver distraction")

# Density plot of accidents in Manhatten
manhatten.zip <- c(10026, 10027, 10030, 10037, 10039,10001, 10011, 10018, 10019, 10020, 10036,10029, 10035,10010, 10016, 10017, 10022,10012, 10013, 10014,10004, 10005, 10006, 10007, 10038, 10280,10002, 10003, 10009,10021, 10028, 10044, 10065, 10075, 10128,10023, 10024, 10025,10031, 10032, 10033, 10034, 10040)

nyc_manhatten <- nyc %>%
  filter(ZIP.CODE %in% manhatten.zip)

nyc_manhatten$LATITUDE <- round(nyc_manhatten$LATITUDE,2)
nyc_manhatten$LONGITUDE <- round(nyc_manhatten$LONGITUDE,2)

nyc_manhatten_2016 <- subset(nyc_manhatten , YEAR == 2016)
nyc_manhatten_2015 <- subset(nyc_manhatten , YEAR == 2015)
nyc_manhatten_2014 <- subset(nyc_manhatten , YEAR == 2014)
nyc_manhatten_2013 <- subset(nyc_manhatten , YEAR == 2013)


ny_plot=ggmap(get_map(location = "Manhatten", ,zoom = 13, maptype='hybrid'))
plot1=ny_plot+stat_density2d(data= nyc_manhatten, aes(x = LONGITUDE, y = LATITUDE, alpha=0.01,fill = ..level..),bins = 10, geom = 'polygon')+
  guides(fill = guide_colorbar(barwidth = 1, barheight = 8)) +
  scale_alpha(guide = FALSE)+ 
  xlab(' ')+ylab(' ')+
  ggtitle('Manhattan Vehicle Accidents')

freq_manhatted <- aggregate(NUMBER.OF.PERSONS.INJURED ~ LONGITUDE+LATITUDE , data = nyc_manhatten , length)

ny_plot <- ggmap(get_map(location = "Manhatten", ,zoom = 13, maptype='hybrid'))
plot1=ny_plot+ geom_tile(data = nyc_manhatten , aes(x = LONGITUDE , y = LATITUDE , alpha = NUMBER.OF.PERSONS.INJURED) , fill = 'red')


# Plotting word cloud for reason of accidents

reasons <- nyc %>%
  select(CONTRIBUTING.FACTOR.VEHICLE.1,CONTRIBUTING.FACTOR.VEHICLE.2,CONTRIBUTING.FACTOR.VEHICLE.3,
         CONTRIBUTING.FACTOR.VEHICLE.4, CONTRIBUTING.FACTOR.VEHICLE.5)

reasons[] <- lapply(reasons, as.character)

reasons <- gather(reasons, vehicle_number, reason)

reasons <- reasons %>%
  filter(reason != "" & reason != "Unspecified") %>%
  group_by(reason) %>%
  summarize(count = n())

reasons %>%
  arrange((desc(count))) %>%
  head(n= 10)

set.seed(123)
png("wordcloud_packages.png", width=80,height=40, units='in', res=200)
wordcloud(words = reasons$reason, freq = reasons$count, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.15, 
          colors=brewer.pal(8, "Dark2"), scale=c(2,.5))


#Creating heatmap of vehicle collisions by month


nyc_map <- get_map(location = "NYC")
#Geting the New York City Map

nyc_round <- nyc
nyc_round$LATITUDE <- round(nyc_round$LATITUDE,2)
nyc_round$LONGITUDE <- round(nyc_round$LONGITUDE,2)
nyc_round$Hour <- as.numeric(nyc_round$Hour)
#Creating a new dataframe nyc_round, with rounded longitude and latitude to 2 decimal points and a numeric hour

hour_vec <- c("00:00","01:00","02:00","03:00","04:00","05:00","06:00","07:00","08:00","09:00","10:00","11:00","12:00","13:00","14:00","15:00","16:00","17:00","18:00","19:00","20:00","21:00","22:00","23:00")
hour_vec_name <- c("00","01","02","03","04","05","06","07","08","09","10","11","12","13","14","15","16","17","18","19","20","21","22","23")
#Creating 2 hour reference vectors used for building plot titles and Filenames



for (hour in 0:23)
  #Running a for loop for hour from 0:23
{
  
  nyc_hour <- subset(nyc_round, nyc_round$Hour == hour)
  #Creating a new data frame that has the number of accidents by location for that particular hour
  
  title <- hour_vec[hour+1]
  #Creating the title for the ggplot
  
  ggmap(nyc_map) + ggtitle(title) + stat_density2d(data= nyc_hour, aes(x = LONGITUDE, y = LATITUDE, alpha=..level..),bins = 10, geom = 'polygon')+
    scale_alpha(limits = c(0,100))+ xlab(' ')+ylab(' ') + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                                                plot.title = element_text(size = 30,face = "bold", margin = margin(10,0,10,0),hjust = 0.5),
                                                                legend.title = element_text(size = 20))
  
  
  
  file_name <- paste(hour_vec_name[hour+1],".png",sep = "")
  #Adding the .png extension to the filename
  ggsave(filename = file_name,device = "png",scale = 3)    
  #Saving the plots to your present working directory
  
}


#Creating heatmps of vehicle collisions for each month in the 4 years

nyc_round <- nyc
nyc_round$LATITUDE <- round(nyc_round$LATITUDE,2)
nyc_round$LONGITUDE <- round(nyc_round$LONGITUDE,2)
nyc_round$MONTH <- as.numeric(nyc_round$MONTH)
#Creating a new dataframe nyc_round, with rounded longitude and latitude to 2 decimal points and a numeric month

month_vec <- c("January","February","March","April","May","June","July","August","September","October","November","December")
month_vec_numeric <- c("01","02","03","04","05","06","07","08","09","10","11","12")
#Creating 2 month reference vectors used for building plot titles and Filenames


for (year in 2013:2016)
  #Running a for loop for year from 2013:2016
{
  for (month in 1:12)
    #Running a for loop for month from 1:12
    
  {
    freq_accident <- aggregate(NUMBER.OF.PEDESTRIANS.INJURED ~ LONGITUDE + LATITUDE, data = nyc_round[nyc_round$YEAR==year & nyc_round$MONTH == month,],length)
    #Creating a new data frame that has the number of accidents by location for that particular month and year
    
    title <- paste(month_vec[month],year,sep = " - ")
    #Creating the title for the ggplot
    
    ggmap(nyc_map) + ggtitle(title) + scale_alpha_continuous(name = "Accident Frequency",limit =c(0,1800) )  +  geom_tile(data = freq_accident, aes(x = LONGITUDE, y = LATITUDE, alpha = NUMBER.OF.PERSONS.INJURED),
                                                                                                                          fill = 'red') + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                                                                                                                                                legend.position = "bottom",  
                                                                                                                                                plot.title = element_text(size = 30,face = "bold",margin = ggplot2::margin(10,0,10,0),hjust = 0.5),
                                                                                                                                                legend.title = element_text(size = 20))
    #Creating the heat map using geom_tile to create rectangular tiles for each location observation, the frequency of the accidents decides the alpha value(i.e. the transparency of the tile)
    #This will give us a nice output of the heatmap with darker tiles representing higher frequencies
    
    
    file_name <- paste(year,month_vec_numeric[month],sep = "")
    #Creating the file name using the month as a string and year
    file_name <- paste(file_name,".png",sep = "")
    #Adding the .png extension to the filename
    ggsave(filename = file_name,device = "png",scale = 3)    
    #Saving the plots to your present working directory
  }
}


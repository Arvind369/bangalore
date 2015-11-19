# Read the schools.csv file. The 121st line had a problem and I had delete the record manually 
# before loading
schools <- read.csv("C:/Arvind/Bangalore/schools.csv")
# Structure of the dataframe and covert all the strings to characters as R loads the character string
# attributes as factor
str(schools)
schools$district <- as.character(schools$district)
schools$block <- as.character(schools$block)
schools$cluster <- as.character(schools$cluster)
schools$schoolname <- as.character(schools$schoolname)
schools$category <- as.character(schools$category)
schools$gender <- as.character(schools$gender)
schools$medium_of_inst <- as.character(schools$medium_of_inst)
schools$address <- as.character(schools$address)
schools$pincode <- as.character(schools$pincode)
schools$landmark <- as.character(schools$landmark)
schools$identification1 <- as.character(schools$identification1)
schools$busroutes <- as.character(schools$busroutes)
schools$identification2 <- as.character(schools$identification2)
schools$latlong <- as.character(schools$latlong)
str(schools)
# The Geo Positions are available in the last column and they are stored within POINT() term
# using the sub command replace the "POINT" in the latlong attribute
schools$latlong <- sub("POINT", "", schools$latlong)
head(schools$latlong)
#schools$latlong <- sub([\(], "", schools$latlong)
#sub(".*\\(","" , schools$latlong[1]) # worked
#sub("[\\(\\)]","" , schools$latlong[1]) # worked
# Remove the paranthesis in the Geo Positions 
schools$latlong <-sub("[\\(\\)]","" , schools$latlong) # worked
head(schools$latlong)
#strsplit(schools$latlong[1], " ")
#strsplit(schools$latlong[1], " ")[[1]]
#strsplit(schools$latlong[1], " ")[[1]]
# Once the paranthesis is removed now we have a string containing the lat and long
# separated by spaces. using unnlist convert lat and long info into a matrix and then to dataframe
mat  <- matrix(unlist(strsplit(schools$latlong, " ")), ncol=2, byrow=TRUE)
BlrSchGeoPos   <- as.data.frame(mat)
View(BlrSchGeoPos)
colnames(BlrSchGeoPos) <- c("long","lat")
View(BlrSchGeoPos)
BlrSchGeoPos$long <- as.numeric(as.character(BlrSchGeoPos$long))
BlrSchGeoPos$lat <- as.numeric(as.character(BlrSchGeoPos$lat))

# Load maps and ggmaps library to pull Bangalore map from Google map API
library(maps)
library(ggmap)
bangalore <- get_map(location = "bangalore", zoom=11)
ggmap(bangalore)
ggmap(bangalore) + geom_point(data=BlrSchGeoPos,aes(x=long,y=lat),color="red")

# Create a heat map of schoold on the Bangalore map
GeoPosCount <- as.data.frame(table(round(BlrSchGeoPos$long,2),round(BlrSchGeoPos$lat,2)))
str(GeoPosCount)
colnames(GeoPosCount) <- c("long","lat","freq")
GeoPosCount$long <- as.numeric(as.character(GeoPosCount$long))
GeoPosCount$lat <- as.numeric(as.character(GeoPosCount$lat))
str(GeoPosCount)
ggmap(bangalore) + geom_point(data=GeoPosCount,aes(x=long,y=lat,color=freq,size=freq))+scale_color_gradient(low="yellow",high="red")
ggmap(bangalore) + geom_tile(data=GeoPosCount,aes(x=long,y=lat,alpha=freq),fill="red")
#### 
View(schools)
# Number of schools in the block. From the graph we can see that the data is for the South and 
# North regions of Bangalore. Even the Anekal area is an extention of south Bangalore
ggplot(data=schools, aes(x=block, fill=block)) + geom_bar(stat="bin") + coord_flip() +
ggtitle("Schools in different blocks of Bangalore")

# Schools in different blocks of Bangalore with category
ggplot(data=schools, aes(x=block, fill=category)) + geom_bar(stat="bin") + coord_flip() +
  ggtitle("Schools in different blocks of Bangalore")

# Teaching language medium. By looking at the data it looks like the data is related to the 
# BBMP/Govt schools as the number of schools for English medium is listed as only 1, but we know
# there are many English medium schools
table(schools$medium_of_inst)
ggplot(data=schools, aes(x=medium_of_inst, fill=medium_of_inst)) + geom_bar(stat="bin") + 
  ggtitle("Teaching medium")

ggplot(schools, aes(x = factor(1), fill = factor(medium_of_inst))) +
  geom_bar(width = 1) + coord_polar(theta = "y")


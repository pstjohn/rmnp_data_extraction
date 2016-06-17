rm(list = ls())
# Setup libraries/paths
library(rgdal)
library(reshape)
library(ggmap)
options(stringsAsFactors = FALSE)
path <- "~/GitHub/rmnp_data_extraction"  # Rename to your own directory location
setwd(path)
if(!file.exists(paste(path, "campsites", sep = "/"))){
  # Getting the zipped files
  myPathToData <- paste(path, "spatial_data_files", sep = "/")
  allFiles <- list.files(path = myPathToData, full.names = T)
  names <- list.files(path = myPathToData, full.names = F)
  names <- gsub(".zip", "", names)
  # Unzip the files
  for (i in 1:length(allFiles[grepl("zip", allFiles)])){
    unzip(zipfile = allFiles[grepl("zip", allFiles)][i], list = FALSE, 
          junkpaths = TRUE, exdir = paste(path, names[i], sep = "/"), 
          overwrite = TRUE)
  }
}

# Load data
sites <- read.csv(paste(path, "sites.csv", sep = "/"))
available <- as.data.frame(t(read.csv(paste(path, "availability.csv", sep = "/"), 
                                      header = T)))
campsites <- readOGR(dsn = paste(getwd(), "campsites", sep = "/"), 
                     layer = "campsites")
boundary <- readOGR(dsn = paste(getwd(), "nps_boundary", sep = "/"), 
                     layer = "nps_boundary")
romo_tracts <- readOGR(dsn = paste(getwd(), "romo_tracts", sep = "/"), 
                    layer = "romo_tracts")
ROMO_TRANS_Trail_ln <- readOGR(dsn = paste(getwd(), "ROMO_TRANS_Trail_ln", 
                                           sep = "/"), 
                               layer = "ROMO_TRANS_Trail_ln")

# Reformat availability data
colnames(available) <- available[1, ]
available <- available[-1, ]
for (i in 1:dim(available)[2]){ 
  available[, i] <- as.numeric(available[, i])
}
available$Code <- gsub("X", "", unlist(row.names(available)))
rename <- which(!available$Code %in% sites$Code)
available$Code[rename] <- c("044-1", "044-2", "044-3")
row.names(available) <- available$Code
available <- available[sites$Code, ]
as.numeric(available[, 1:147])

# Reformat spatial data
boundary <- boundary[boundary$UNIT_CODE=="ROMO", ]
a <- campsites@data$SITE
a <- unlist(strsplit(a, split = "/"))
campsites@data$Code <- a[!grepl("-", a)][-1]
campsites@data$Code[campsites@data$Code=="A1"] <- "ACC1"
toReplace <- unique(campsites@data$Code[!campsites@data$Code%in%sites$Code])
for (i in toReplace){
  campsites@data$Code[campsites@data$Code==i] <- sites$Code[grepl(i, sites$Code)][1]
}
# Remove non-backcountry sites
campsites <- campsites[campsites$Code%in%sites$Code, ]

# Change easting/northing to lat/lon
latlong = "+init=epsg:4326"
campsites <- spTransform(campsites, CRS(latlong))
boundary <- spTransform(boundary, CRS(latlong))
romo_tracts <- spTransform(romo_tracts, CRS(latlong))
ROMO_TRANS_Trail_ln <- spTransform(ROMO_TRANS_Trail_ln, CRS(latlong))

# Calculated fields
sites$Remaining <- rowSums(available[, 1:147])/(sites$MaxNum * 147)
sites$Reserved <- 1 - sites$Remaining
for (i in campsites$Code){
  campsites@data$Reserved[campsites@data$Code==i] <- sites$Reserved[sites$Code==i]
}


# Analyses
# Which dates are most popular
plot(x = as.Date(colnames(available[, 1:147])), 
     y = 1 - (colSums(available[, 1:147])/sum(sites$MaxNum)),
     ylim = c(0, 1), pch = 21, bg = "forestgreen",
     ylab = "Proportion of backcountry sites reserved @ ROMO",
     xlab = "Date", main = "Reservations as of 6/9/2016 1:17 PM MDT")

# Elevation vs. Snow-free
sites$SnowFreeDate <- as.Date(sites$SnowFreeDate, format = "%m/%d")
plot(x = sites$SnowFreeDate, y = sites$Elevation, ylim = c(8000, 13000),
     ylab = "Elevation (m)", xlab = "First Snow-free Date (20-year average)", 
     main = "Rocky Mountain National Park Backcountry Sites",
     pch = 21, bg = "darksalmon")
# Regression
model <- lm(sites$Elevation ~ sites$SnowFreeDate)
abline(model)
# Elements
value = round(summary(model)$fstatistic[1], 2)
numdf = summary(model)$fstatistic[2]
dendf = summary(model)$fstatistic[3]
rsquare = summary(model)$r.squared
pvalue = summary(model)$coefficients[8]
if(pvalue < 0.0001){pvalue <- "0.0001"}
# Annotation (set for image with 600 x 600 pix)
text(x = as.Date("2016-07-02"), y = 12950, 
     labels =  bquote(paste('ANOVA F'[.(numdf)]*', '[.(dendf)]*
                              ' = ', .(value), ' ')))
text(x = as.Date("2016-07-09"), y = 12750, 
     labels = paste("p =", pvalue, sep = " "))

# Day of the week
a <- data.frame(weekday = format(as.Date(colnames(available[, 1:147])), format = "%a"),
                available = colSums(available[, 1:147]))
a$weekday <- factor(a$weekday,
                            levels = c("Mon", "Tue", "Wed", "Thu",
                                       "Fri", "Sat", "Sun"))
plot(x = a$weekday,
     y = a$available,
     xlab = "Day of the week", ylab = "Number of available sites",
     col = "skyblue", main = "Weekends are desirable")



# Spatial Analyses
# ROMO <- get_map(location = c(lon = mean(boundary@bbox[1, ]), 
#                              lat = mean(boundary@bbox[2, ])))
plot(boundary,
     main = "Rocky Mountain National Park", 
     col = terrain.colors(20)[8])
lines(ROMO_TRANS_Trail_ln)
points(campsites, cex = (3*campsites@data$Reserved), pch = 21, 
       bg = "red")
campsites <- campsites[order(campsites$Reserved, decreasing = T), ]
points(campsites[1:9, ], 
       cex = 2.5, pch = 21, bg = "blue")

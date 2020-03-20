# Clear plots
if(!is.null(dev.list())) dev.off()
# Clear console
cat("\014") 
# Clean workspace
rm(list=ls())
# Set working directory
setwd("C:/Users/seacr/Desktop/Germany/2019SS/Spatial Data Science/Paper/Analysis")

#packages
#install.packages("tidyverse") #for data manipulation in R
library(dplyr)
library(stringr)
library(stringi)
library(ggplot2)
library(GISTools)
library(sf)
library(sp)
library(tmap)
library(grid)
library(ggmap)
library(googleway)
library(RgoogleMaps)
library(rgdal)
library(maps)
library(spdep)

"================================================================================================================================"
"=====================CLEANING THE DATA=========================================================================================="
"================================================================================================================================"
#reading the town center(tc) statistics in 2009 & 2014
tc09 <- read.csv("C:/Users/seacr/Desktop/Germany/2019SS/Spatial Data Science/Paper/Data/Town Centre Statistics/tables/TC_09_%.csv")
tc14 <- read.csv("C:/Users/seacr/Desktop/Germany/2019SS/Spatial Data Science/Paper/Data/Town Centre Statistics/tables/TC_14_%.csv")

#reading the shape file 
dclg_sf <- st_read("C:/Users/seacr/Desktop/Germany/2019SS/Spatial Data Science/Paper/Data/2004 DCLG Retail Centres/shapefile/RetailCentres.shp")

#adding year column for both tc datasets
tc09$year <- 2009
tc14$year <- 2014

#creating columns with differences in each variable by year
tc09 <- tc09 %>%
  arrange(TC2004_ID)
tc14 <- tc14 %>%
  arrange(TC2004_ID)
tc14$vac_diff <- tc14$Vacant_Prop - tc09$Vacant_Prop #difference in vacancy proportion
tc14$ser_diff <- tc14$Services_Prop - tc09$Services_Prop #difference in service premises proportion
tc14$lei_diff <- tc14$Leisure_Prop - tc09$Leisure_Prop #difference in leisure premises proportion
tc14$con_diff <- tc14$Convenience_Prop - tc09$Convenience_Prop #difference in convenience premises proportion

tc09$vac_diff<-tc14$vac_diff
tc09$ser_diff<-tc14$ser_diff
tc09$lei_diff<-tc14$lei_diff
tc09$con_diff<-tc14$con_diff

#merging into one big tc dataset
tc <- rbind(tc09, tc14)

#chaging the name of the ID column for consistency
names(tc)[1] <- "ID"
#names(tc)

#changing both ID columns in tc and dclg_sf into character vectors
tc$ID <- as.character(tc$ID)
dclg_sf$ID <- as.character(dclg_sf$ID)

#creating one big dataset with all the variables
#however, dropped all the observation which doesn't have any geomtry values in dclg_sf(1188 observations)
data <- inner_join(tc, dclg_sf, by = "ID")
namey_idx <- grep("NAME.y",names(data))
data <- data[,-namey_idx]
names(data)[2] <- "NAME"
data <- data %>%
  filter(is.na(vac_diff)==F)
data <- st_as_sf(data)

#just checking once more
#temp <- full_join(tc, dclg_sf, by = "ID")
#num <- 0
#for(i in 1:length(temp$geometry)){
#  if(is.na(temp$geometry[[i]][1])==T){
#    num <- num +1
#  }
#}
#print(num)
class(data)
#checking whether each town center has values in both years
length(unique(data$ID)) #1312 = 2624/2 no problem!

##map background
#retrieving coordinates data
data_sp <- as(data, Class = "Spatial")
data_sp <- spTransform(data_sp, CRS('+proj=longlat +datum=WGS84'))
data_coord <- as.data.frame(coordinates(data_sp))
data$x_google <- data_coord$coords.x1
data$y_google <- data_coord$coords.x2

#setting a background map of UK
background <- get_map(location = c(-2, 54), zoom = 6, maptype = "terrain", source = "google", color = "bw")

##A DATASET FOR Town Centers(TC) & Retail Cores (RC), seperately!
#constructing an index for Retail Cores (RC)!
rc_idx <- grepl("RC", data$ID)
#TC dataset
tc <- data[!rc_idx, ]
tc_sp <- as(tc, Class = "Spatial")
#RC dataset
rc <- data[rc_idx, ]
rc_sp <- as(rc, Class = "Spatial")

#overall map(TC)
tmap_mode("view")
tm_shape(tc) +
  tm_dots("vac_diff", title = "difference in vacancy proportion", palette = "Spectral", style = "quantile") + 
  tm_layout(legend.title.size = 1,
            legend.text.size = 1,
            legend.position = c(0.1, 0.1))
tmap_mode("plot")

#overall map(RC)
tmap_mode("view")
tm_shape(rc) +
  tm_dots("vac_diff", title = "difference in vacancy proportion", palette = "Spectral", style = "quantile") + 
  tm_layout(legend.title.size = 1,
            legend.text.size = 1,
            legend.position = c(0.1, 0.1))
tmap_mode("plot")
"================================================================================================================================"
"==============CREATING NEIGHBORS AND SPATIAL WEIGHTS============================================================================"
"================================================================================================================================"
neighbors <- dnearneigh(data_sp,0,50) #50km arbitrary number
#not_empty_idx <- c(rep(T, length(data$ID)))
#for(i in 1:length(neighbors)){
#  if(neighbors[[i]]==0){
#    print(i)
#    not_empty_idx[i] <- F
#  }
#}
#summary(not_empty_idx==F)
#neighbors <- neighbors[not_empty_idx]
#data_sp <- as(data[not_empty_idx, ], Class = "Spatial")

#double power weights
dists <- nbdists(neighbors, coordinates(data_sp))
double_power_distance_function <- function(x) {if(is.numeric(x) && x > 10000) x * 0 else (1-(x/10000)^1)^1}
dist_weights <- lapply(dists, double_power_distance_function)
weights <- nb2listw(neighbours = neighbors,
                    glist = dist_weights,
                    zero.policy = TRUE)

moran.mc(data$vac_diff, weights, nsim = 1000, zero.policy = T)
plot(moran.mc(data$vac_diff, weights, nsim = 1000, zero.policy = T), 
     xlab = "Vacancy Difference")

"================================================================================================================================"
"==============TOWN CENTERS======================================================================================================"
"================================================================================================================================"
#retrieving coordinates of major cities in UK by population(2011 census)
key <- "GOOGLE_API_KEY"

##creating metropolitan data
#greater london
london_data <- data %>%
  filter(x_google<=0.34 & x_google>=-0.5 & y_google<=51.681 & y_google>=51.28) %>%
  filter(!(x_google<=-0.301 & y_google>=51.6348)) %>%
  filter(!(x_google<=-0.362401 & y_google<=51.411258)) %>%
  filter(!(x_google>=-0.3061 & x_google<=-51.3353 & y_google<=51.379)) %>%
  filter(!(x_google<=-0.326646 & y_google<=51.326)) %>%
  filter(!(x_google>=-0.316408 & x_google<=-0.2422 & y_google>=51.642)) %>%
  filter(!(x_google>=0.021974 & x_google<=0135476 & y_google>=51.60)) %>%
  filter(!(x_google>=0.188353 & y_google<=51.5345)) %>%
  filter(!(x_google>=0.153773 & y_google<=51.418498))
ggmap(london) + geom_point(data = london_data, aes(x_google, y_google, colour = "red"), size = 3)
#west midlands
birmingham_data <- data %>%
  filter(x_google<=-1.43 & x_google>=-2.17 & y_google<=52.65 & y_google>=52.37) %>%
  filter(!(x_google>=-1.74521 & y_google>=52.458))
ggmap(birmingham) + geom_point(data = birmingham_data, aes(x_google, y_google, colour = "red"), size = 3)
#greater manchester
manchester_data <- data %>%
  filter(x_google<=-1.96 & x_google>=-2.72 & y_google<=53.65 & y_google>=53.35) %>%
  filter(!(x_google>=-2.641 & x_google<=-2.47 & y_google<=53.48))
ggmap(manchester) + geom_point(data = manchester_data, aes(x_google, y_google, colour = "red"), size = 3)

#writing shape files for online ESRI
st_write(london_data, "greater_london.shp", delete_dsn=TRUE)
st_write(birmingham_data, "west_midlands.shp", delete_dsn=TRUE)
st_write(manchester_data, "greater_manchester.shp", delete_dsn=TRUE)
st_write(data, "data.shp", delete_dsn=TRUE)

##PLOTTING BY METROPOLITAN AREAS
"LONDON"
london <- get_map(location =  c(-0.5, 51.28, 0.34, 51.7), maptype = "terrain", source = "google", color = "bw")
#ggmap(london)
london_map <- ggmap(london) +
  geom_point(data = data, 
             aes(x = x_google, 
                 y = y_google, 
                 colour = cut(vac_diff, c(-Inf, 0, Inf)))) +
  scale_color_manual(name = "Vacancy difference",
                     values = c("(-Inf,0]" = "#35B4EB",
                                "(0, Inf]" = "#FF1C8E"),
                     labels = c("Negative vacancy difference", "Positive vacancy difference")) +
  labs(title = "Vacancy Difference between 2009 & 2014",
       subtitle = "Greater London",
       x = "Latitude", 
       y = "Longitude") +
  theme(plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5))
plot(london_map)

"BIRMINGHAM"
birmingham <- get_map(location = "West Midlands", maptype = "terrain", source = "google", color = "bw")
#ggmap(birmingham)
birmingham_map <- ggmap(birmingham) +
  geom_point(data = tc, aes(x = x_google, y = y_google,colour = cut(vac_diff, c(-Inf, 0, Inf)))) +
  scale_color_manual(name = "Vacancy difference",
                     values = c("(-Inf,0]" = "#35B4EB",
                                "(0, Inf]" = "#FF1C8E"),
                     labels = c("Negative vacancy difference", "Positive vacancy difference")) +
  labs(title = "Vacancy Difference between 2009 & 2014",
       subtitle = "West Midlands",
       x = "Latitude", 
       y = "Longitude") +
  theme(plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5))
plot(birmingham_map)

"MANCHESTER"
manchester <- get_map(location = c(-2.73, 53.33, -1.9, 53.7), maptype = "terrain", source = "google", color = "bw")
#ggmap(manchester)
manchester_map <- ggmap(manchester) +
  geom_point(data = tc, aes(x = x_google, y = y_google, colour = cut(vac_diff, c(-Inf, 0, Inf)))) +
  scale_color_manual(name = "Vacancy difference",
                     values = c("(-Inf,0]" = "#35B4EB",
                                "(0, Inf]" = "#FF1C8E"),
                     labels = c("Negative vacancy difference", "Positive vacancy difference")) +
  labs(title = "Vacancy Difference between 2009 & 2014",
       subtitle = "Greater Manchester",
       x = "Latitude", 
       y = "Longitude") +
  theme(plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5))
plot(manchester_map)

"================================================================================================================================"
"==============Hot Spots======================================================================================================"
"================================================================================================================================"
dir <- "C:/Users/seacr/Desktop/Germany/2019SS/Spatial Data Science/Paper/Data/ESRI/"

#overall 
hotspot_overall <- st_read(paste0(dir, "overall_hotspot_shape/Hot_Spots_vac_dff.shp"))

tmap_mode("view")
tm_shape(hotspot_overall) +
  tm_dots("Gi_Bin", title = "hot spot map of vacancy proportion", palette = "-RdBu", style = "cat",
          labels = c("Cold Spot with 99% Confidence", "Cold Spot with 95% Confidence", "Cold Spot with 90% Confidence", 
                     "Not significant", 
                     "Hot Spot with 90% Confidence", "Hot Spot with 95% Confidence", "Hot Spot with 99% Confidence")) + 
  tm_layout(legend.title.size = 1,
            legend.text.size = 1,
            legend.position = c(0.1, 0.1))
tmap_mode("plot")

#greater london
hotspot_london <- st_read(paste0(dir, "greater_london_true_shape/Hot_Spots_vac_dff_greater_london_true.shp"))

tmap_mode("view")
tm_shape(hotspot_london) +
  tm_dots("Gi_Bin", title = "hot spot map of vacancy proportion", palette = "-RdBu", style = "cat",
          labels = c("Cold Spot with 99% Confidence", "Cold Spot with 95% Confidence", "Cold Spot with 90% Confidence", 
                     "Not significant", 
                     "Hot Spot with 90% Confidence", "Hot Spot with 95% Confidence", "Hot Spot with 99% Confidence")) + 
  tm_layout(legend.title.size = 1,
            legend.text.size = 1,
            legend.position = c(0.1, 0.1))
tmap_mode("plot")

#west midlands
hotspot_birmingham <- st_read(paste0(dir, "west_midlands_true_shape/Hot_Spots_vac_dff_west_midlands_true.shp"))
tmap_mode("view")
tm_shape(hotspot_birmingham) +
  tm_dots("Gi_Bin", title = "hot spot map of vacancy proportion", palette = "-RdBu", style = "cat",
          labels = c("Cold Spot with 99% Confidence", "Cold Spot with 95% Confidence", "Cold Spot with 90% Confidence", 
                     "Not significant", 
                     "Hot Spot with 90% Confidence", "Hot Spot with 95% Confidence", "Hot Spot with 99% Confidence")) + 
  tm_layout(legend.title.size = 1,
            legend.text.size = 1,
            legend.position = c(0.1, 0.1))
tmap_mode("plot")

#greater manchester
hotspot_manchester <- st_read(paste0(dir, "greater_manchester_true_shape/Hot_Spots_vac_dff_greater_manchester_true.shp"))

tmap_mode("view")
tm_shape(hotspot_manchester) +
  tm_dots("Gi_Bin", title = "hot spot map of vacancy proportion", palette = "-RdBu", style = "cat",
          labels = c("Cold Spot with 99% Confidence", "Cold Spot with 95% Confidence", "Cold Spot with 90% Confidence", 
                     "Not significant", 
                     "Hot Spot with 90% Confidence", "Hot Spot with 95% Confidence", "Hot Spot with 99% Confidence")) + 
  tm_layout(legend.title.size = 1,
            legend.text.size = 1,
            legend.position = c(0.1, 0.1))
tmap_mode("plot")


"*********************************************************************************************************************************"
"********************LONDON AND DEPRIVATION***************************************************************************************"
"*********************************************************************************************************************************"
#reading deprivation shape file
deprivation <- st_read("C:/Users/seacr/Desktop/Germany/2019SS/Spatial Data Science/Paper/Data/deprivation/E12000007/spatial/E12000007_domain.shp")
deprivation <- deprivation %>% arrange(lsoa11cd)

#house price
"This geodata pack provides the annual median transaction values per property type received at the Land Registry 
during the period 1995-2016 for the LSOAs covering the Greater London Area"
house_price <- st_read("C:/Users/seacr/Desktop/Germany/2019SS/Spatial Data Science/Paper/Data/house_price/E12000007/spatial/E12000007_total.shp")
house_price <- house_price %>% arrange(lso11cd)

#retrieving only 2013 data
idx_2013 <- grepl("2013", names(house_price))
idx_2013[1] <- T 
house_price <- house_price[idx_2013]
house_price <- house_price %>%
  arrange(-md_2013, -fr_2013)

#checking correlation between frequency and median price
house_price %>% 
  ggplot(aes(x = md_2013, 
             y = fr_2013)) +
  geom_jitter()+
  geom_smooth(method = "loess")

#attaching median property price to deprivation dataset
deprivation$house_price <- house_price$md_2013
rm(house_price)

#attaching hotspot bin data to london_data dataframe
london_data$hotspot_bin <- hotspot_london$Gi_Bin
names(london_data)
head(london_data)

#making all three datasets to have same espg code
london_data <- st_transform(london_data, 7405)
deprivation <- st_transform(deprivation, 7405)


#mapping index of multiple deprivation with hotspots in vacancy difference
tmap_mode("view")
tm_shape(deprivation) +
  tm_polygons("imd_d", style = "cat", palette = "YlOrBr") +
  tm_shape(london_data) +
  tm_dots("hotspot_bin", title = "hot spot map of vacancy proportion", palette = "-RdBu", style = "cat",
          labels = c("Cold Spot with 99% Confidence", "Cold Spot with 95% Confidence", "Cold Spot with 90% Confidence", 
                     "Not significant", 
                     "Hot Spot with 90% Confidence", "Hot Spot with 95% Confidence", "Hot Spot with 99% Confidence")) +
  tm_layout(legend.title.size = 1,
            legend.text.size = 1,
            legend.position = c(0.1, 0.1))
tmap_mode("plot")

#mapping annual median house price with hotspots in vacancy difference
tmap_mode("view")
tm_shape(deprivation) +
  tm_polygons("house_price", style = "quantile", palette = "YlOrBr") +
  tm_shape(london_data) +
  tm_dots("hotspot_bin", title = "hot spot map of vacancy proportion", palette = "-RdBu", style = "cat",
          labels = c("Cold Spot with 99% Confidence", "Cold Spot with 95% Confidence", "Cold Spot with 90% Confidence", 
                     "Not significant", 
                     "Hot Spot with 90% Confidence", "Hot Spot with 95% Confidence", "Hot Spot with 99% Confidence")) +
  tm_layout(legend.title.size = 1,
            legend.text.size = 1,
            legend.position = c(0.1, 0.1))
tmap_mode("plot")

#plotting between housing decile in deprivation index and median housing price
deprivation %>%
  ggplot(aes(x = house_price, 
             y = housing_d)) +
  geom_jitter()+
  geom_smooth(method = "loess")
cor.test(deprivation$house_price, deprivation$housing_d)

#mimizing the size of the london_data set
london_data <- london_data %>% 
  arrange(ID)

#retrieving the deprivation polygons which contains a retail center.
contains <- st_contains(deprivation, london_data)
contains_id <- list()
row <- list() #for the polygons
points <- list() #for corresponding retail centers
for(i in 1: length(contains)){
  if(length(contains[[i]])>0){
    print(i)
    row <- append(row,i)
    points <- append(points, contains[[i]][1])
  }
}
row <- unlist(row)
points <- unlist(points)
length(row)
length(points)
head(row)
head(points)

#only the polygons with having a point within
sar_points <- london_data[points,]

#creating each deprivation decile column from deprivation data set
sar_points$income_d <- -999 
sar_points$empl_d <- -999
sar_points$educ_d <- -999
sar_points$health_d <- -999
sar_points$crime_d <- -999
sar_points$l_env_d <- -999
sar_points$housing_d <- -999

#retreiving each deprivation decile value from deprivation data set
for(i in 1:length(sar_points$ID)){
  sar_points$imd_d[i] <- deprivation$imd_d[row[i]]
  sar_points$income_d[i] <- deprivation$income_d[row[i]]
  sar_points$empl_d[i] <- deprivation$empl_d[row[i]]
  sar_points$educ_d[i] <- deprivation$educ_d[row[i]]
  sar_points$health_d[i] <- deprivation$health_d[row[i]]
  sar_points$crime_d[i] <- deprivation$crime_d[row[i]]
  sar_points$l_env_d[i] <- deprivation$l_env_d[row[i]]
  sar_points$housing_d[i] <- deprivation$housing_d[row[i]]
  sar_points$house_price[i] <- deprivation$house_price[row[i]]
}


tmap_mode("view")
tm_shape(sar_points) +
  tm_dots("housing_d", style = "cat", palette = "YlOrBr")
tmap_mode("plot")

#deprivation
basic_formula <- as.formula(vac_diff ~ income_d + empl_d + educ_d + health_d + crime_d + l_env_d + housing_d)
simple_formula <- as.formula(vac_diff ~ income_d + health_d + l_env_d)

ols <- lm(basic_formula, sar_points)
summary(ols)
ols_result <- summary(ols)$coefficients
write.csv(ols_result, "ols_result.csv")

simple_ols <- lm(simple_formula, sar_points)
summary(simple_ols)
simple_ols_result <- summary(simple_ols)$coefficients
write.csv(simple_ols_result, "simple_ols_result.csv")

#ols residuals
e <- ols$residuals
simple_e <- simple_ols$residuals

#ols spatial weights
#double power weights
sar_points_sp <- as(sar_points, Class = "Spatial")
ols_neighbors <- dnearneigh(sar_points_sp,0, 5000)  #5 km(5000m) somehow now the unit is in meters
ols_dists <- nbdists(ols_neighbors, coordinates(sar_points_sp)) 
ols_dist_weights <- lapply(ols_dists, double_power_distance_function)
ols_weights <- nb2listw(neighbours = ols_neighbors,
                        glist = ols_dist_weights,
                        zero.policy = TRUE)

#moran's I test
moran.mc(e, ols_weights, nsim = 1000, zero.policy = T)
plot(moran.mc(e, ols_weights, nsim = 1000, zero.policy = T), 
     xlab = "Vacancy Difference")

moran.mc(simple_e, ols_weights, nsim = 1000, zero.policy = T)
plot(moran.mc(simple_e, ols_weights, nsim = 1000, zero.policy = T), 
     xlab = "Vacancy Difference")

#house price  + imd SAR
final_formula <- as.formula(vac_diff ~ income_d + health_d + l_env_d + house_price)
final_ols <- lm(final_formula, sar_points)
summary(final_ols)
final_e <- final_ols$residuals

moran.mc(final_e, ols_weights, nsim = 1000, zero.policy = T)
plot(moran.mc(final_e, ols_weights, nsim = 1000, zero.policy = T), 
     xlab = "Vacancy Difference")

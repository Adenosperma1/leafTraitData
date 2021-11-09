#install.packages("lme4")
#install.packages("arm")
library(lme4)
library(arm) 
library(ggeffects)
library(tidyverse)
library(raster)
library(sjPlot)

citation("maps")
version("ggplot")

#read in data
#dataSyz = read.csv(file="SYZ_MEASURE_LOCATION.csv")
#dataFig = read.csv(file="FIGS_MEASURE_LOCATION.csv")
#dataWri = read.csv(file="wright.csv")


#read in data
dataSyz = read.csv(file="/Users/brendanwilde/HONOURS/DATA/SYZ/SYZ_MEASURE_LOCATION.csv", header = T,    na.strings="NA", dec=".", strip.white=TRUE)
dataFig = read.csv(file="/Users/brendanwilde/HONOURS/DATA/FIGS/FIGS_MEASURE_LOCATION.csv", header = T,  na.strings="NA", dec=".", strip.white=TRUE)
dataWri = read.csv(file="/Users/brendanwilde/HONOURS/DATA/Wright/wright.csv", header = T,  na.strings="NA", dec=".", strip.white=TRUE)



#reduce columns 
dataSyz2 = dataSyz %>% dplyr::select(Image, Scientific.Name..intepreted., Decimal.latitude..WGS84., Decimal.longitude..WGS84., cm.Area.Mask, )
dataFig2 = dataFig %>% dplyr::select(Image, Scientific.Name..intepreted., Decimal.latitude..WGS84., Decimal.longitude..WGS84., cm.Area.Mask, )
dataWri2 = dataWri %>% dplyr::select(Family, Genus.species, Latitude, Longitude, Area)

#rename headings
dataSyz3  = dataSyz2 %>% 
  rename(
    specimen = Image,
    species = Scientific.Name..intepreted.,
    lat = Decimal.latitude..WGS84.,
    long = Decimal.longitude..WGS84.,
    area = cm.Area.Mask,
  )

dataFig3  = dataFig2 %>% 
  rename(
    specimen = Image,
    species = Scientific.Name..intepreted.,
    lat = Decimal.latitude..WGS84.,
    long = Decimal.longitude..WGS84.,
    area = cm.Area.Mask,
  )


dataWri3 = dataWri2 %>% 
  rename(
    family = Family,
    species = Genus.species,
    lat = Latitude,
    long = Longitude,
    area = Area,
  )

#get MAT data
#mean_mean_temp_map <- getData(name="worldclim", res=2.5, var="tmean")
#annual_MAT <- mean(mean_mean_temp_map)/10 #data comes as degrees * 10

dataSyz3$MAT <- raster::extract(annual_MAT, cbind(x=dataSyz3$long, y=dataSyz3$lat))
dataFig3$MAT <- raster::extract(annual_MAT, cbind(x=dataFig3$long, y=dataFig3$lat))
dataWri3$MAT <- raster::extract(annual_MAT, cbind(x=dataWri3$long, y=dataWri3$lat))

#GET MAP data
#annual_map <- getData(name="worldclim", res=2.5, var="prec")
#annual_MAP <- sum(annual_map) #comes as monthly rain

dataSyz3$MAP<- raster::extract(annual_MAP, cbind(x=dataSyz3$long, y=dataSyz3$lat))
dataFig3$MAP <- raster::extract(annual_MAP, cbind(x=dataFig3$long, y=dataFig3$lat))
dataWri3$MAP <- raster::extract(annual_MAP, cbind(x=dataWri3$long, y=dataWri3$lat))



#WHAT AM I TRYING TO FIGURE OUT???
#IS there a link between leaf size and MAT
#IS there a link between leaf size and MAP
#and comparing these links for species, genus, global?
#I NEED TO COMBINE ALL leaf measurements for a specimen?

#Fit the Non-Multilevel Models
test <- lm(cm.Area.Mask ~  MAT, data = dataSyz2)
summary(test)



dataSyz3 %>% filter(!is.na(MAT)) %>% filter(species!="Syzygium") -> dataSyz4

lm1 <- lm(log10(area) ~  MAT , data = dataSyz4)
dataSyz4$area<-set_label(dataSyz4$area,"Syzygium within species")
m1 <- lmer(log10(area) ~  MAT + (1|species) + (1|specimen), data = dataSyz4)
dataSyz4$area<-set_label(dataSyz4$area,"Syzygium among species")
m3 <- lmer(log10(area) ~  MAT  + (1|specimen), data = dataSyz4)

dataWri3$area<-set_label(dataWri3$area,"Wright et al within species")
wm1 <- lmer(log10(area) ~  MAT + (1|species) , data = dataWri3)
dataWri3$area<-set_label(dataWri3$area,"Wright et al among species")
wm3 <- lm(log10(area) ~  MAT  , data = dataWri3)


tab_model(m1, m3,wm1,wm3,title="Mean Annual Temperature",file="mean_annual_temp.html")



dataSyz3 %>% filter(!is.na(MAP)) %>%
  filter(species!="Syzygium") -> dataSyz4



lm1 <- lm(log10(area) ~  log10(MAP) , data = dataSyz4)
dataSyz4$area<-set_label(dataSyz4$area,"Syzygium within species")
m1 <- lmer(log10(area) ~  log10(MAP) + (1|species) + (1|specimen), data = dataSyz4)
dataSyz4$area<-set_label(dataSyz4$area,"Syzygium among species")
m3 <- lmer(log10(area) ~  log10(MAP)  + (1|specimen), data = dataSyz4)

dataWri3$area<-set_label(dataWri3$area,"Wright et al within species")
wm1 <- lmer(log10(area) ~  log10(MAP) + (1|species) , data = dataWri3)
dataWri3$area<-set_label(dataWri3$area,"Wright et al among species")
wm3 <- lm(log10(area) ~  log10(MAP)  , data = dataWri3)


tab_model(m1, m3,wm1,wm3,title="Annual Precip",file="annual_precip.html")

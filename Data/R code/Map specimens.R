
library(ggplot2)
library(raster)
library(tidyverse)
library(maps)

#COLOURS##############################
red = rgb(231, 84, 102, maxColorValue = 255)
dotGrey = rgb(154,154,154, maxColorValue = 255)
mapGrey = rgb(228,228,228, maxColorValue = 255)


#VARIABLES#########################
theWidth = 7
theHeight = 6
pointShape = 1
pointSize = .2 #.4
pointAlpha = .2
quant1 = 0.05
quant2 = 0.95
quantColour = "black"
quantSize = .5
quantSize2 = .2
areaTitle = "Area (log scale, cm²)"

#SAVE PATH #####################
path = "/Users/brendanwilde/HONOURS/_manuscript/graphs/"

#FILE PATHS #####################
#merge the downloaded records from avh with the measurements file... there's a script in the script folder 7_MERGE MEAS AND LOCATION
dataAllSyz = read.csv(file="/Users/brendanwilde/HONOURS/DATA/SYZ/SYZ_MEASURE_LOCATION.csv", sep = ',')
dataAllFigs = read.csv(file="/Users/brendanwilde/HONOURS/DATA/FIGS/FIGS_MEASURE_LOCATION.csv", sep = ',')
dataAllwright = read.csv(file="/Users/brendanwilde/HONOURS/DATA/Wright/wright.csv", sep = ',')


#as tables
my_data_Syz <- as_tibble(dataAllSyz)
my_data_figs <- as_tibble(dataAllFigs)
wright_data <- as_tibble(dataAllwright)


#reduce columns
locations_Syz = my_data_Syz %>% dplyr::select(Image, Scientific.Name..intepreted., Decimal.latitude..WGS84., Decimal.longitude..WGS84., cm.Area.Mask, cm.Length, cm.Width)
locationsShort_Syz = my_data_Syz %>% dplyr::select(Scientific.Name..intepreted., Decimal.latitude..WGS84., Decimal.longitude..WGS84., cm.Area.Mask)
locationsWrightShort = wright_data %>% dplyr::select(Genus.species, Latitude, Longitude, Area)
locationsWright = wright_data %>% dplyr::select(Family, Genus.species, Latitude, Longitude, Area)
locationsShortFigs = my_data_figs %>% dplyr::select(Scientific.Name..intepreted., Decimal.latitude..WGS84., Decimal.longitude..WGS84., cm.Area.Mask)


ausShort = my_data_figs %>% dplyr::select(Image, Scientific.Name..intepreted. )
ausFiltered = ausShort %>% filter(Scientific.Name..intepreted. == "Ficus coronata")
count = count(unique(ausFiltered[c("Image")]))

#rename headings
loc = locations_Syz %>% 
  rename(
    specimen = Image,
    species = Scientific.Name..intepreted.,
    lat = Decimal.latitude..WGS84.,
    long = Decimal.longitude..WGS84.,
    area = cm.Area.Mask,
    length = cm.Length,
    width = cm.Width,
  )
length(loc$specimen)


locW = locationsWright %>% 
  rename(
    family = Family,
    species = Genus.species,
    lat = Latitude,
    long = Longitude,
    area = Area,
  )


#rename headings
locShortSyz = locationsShort_Syz %>% 
  rename(
    species = Scientific.Name..intepreted.,
    lat = Decimal.latitude..WGS84.,
    long = Decimal.longitude..WGS84.,
    area = cm.Area.Mask,
  )
length(locShortSyz$species)


#rename headings
locShortW = locationsWrightShort %>% 
  rename(
    species = Genus.species,
    lat = Latitude,
    long = Longitude,
    area = Area,
  )
length(locShortW$species)

#rename headings
locShortFigs = locationsShortFigs %>% 
    rename(
      species = Scientific.Name..intepreted.,
      lat = Decimal.latitude..WGS84.,
      long = Decimal.longitude..WGS84.,
      area = cm.Area.Mask,
    )
length(locShortFigs$species)


threespeciesFiltered = locShortSyz %>% filter(species == "Syzygium australe" | species == "Syzygium oleosum" | species == "Syzygium smithii")

#change the species names to be numbers for each so they can be coloured and mapped one ontop of the other...
locShortW$species = "1"
locShortFigs$species = "2"
locShortSyz$species = "3"
wrightAndSyz <- rbind(locShortW, locShortFigs, locShortSyz)



#remove missing data
allLeaves = loc[complete.cases(loc), ]
allLeavesW = locW[complete.cases(locW), ]

#should probably use LocShort??? instead of threespecies?
allAustrale = threespeciesFiltered %>% filter(species == "Syzygium australe")
allOleosum = threespeciesFiltered %>% filter(species == "Syzygium oleosum")
allSmithii = threespeciesFiltered %>% filter(species == "Syzygium smithii")


#should probably use LocShort??? instead of threespecies?
allcoronata = locShortFigs %>% filter(species == "Ficus coronata")
allfraseri = locShortFigs %>% filter(species == "Ficus fraseri")
allopposita = locShortFigs %>% filter(species == "Ficus opposita")
locShortFigs$species = "1"
allcoronata$species = "2"
allfraseri$species = "3"
allopposita$species = "4"
#TODO ADD ALL FIG RECORDS TO THIS in Green?
figs3Species <- rbind(allcoronata, allfraseri, allopposita)
AllFigsAnd3Species <- rbind(locShortFigs, figs3Species)

length(allcoronata$species)

#change the species names to be common for each so they can be mapped one ontop of the other...
allAustrale$species = "4"
allOleosum$species  = "5"
allSmithii$species  = "6"
length(allSmithii$species)
wrightAndSyzAND3 <- rbind(wrightAndSyz, allAustrale, allOleosum, allSmithii)
wrightAndSyzAND3 <-wrightAndSyzAND3[order(wrightAndSyzAND3$species),] 


########################
########################


allLeaves3 = threespeciesFiltered[complete.cases(threespeciesFiltered), ]

#USED TO JOIN THE WRIGHT, SYZ, AND THREE SPECIES OF SYZ TOGETHER
threespeciesFiltered1 = threespeciesFiltered[complete.cases(threespeciesFiltered), ]
#wrightAndSyzAND3 <- rbind(wrightAndSyz, threespeciesFiltered1)
#combined <- rbind(allLeaves, allLeavesW)

#combined$species = allLeaves$species
#length(allLeaves$species)
#length(allLeavesW$species )


###################################################
###GET CLIMATE DATA
mean_temp_map <- getData(name="worldclim", res=2.5, var="tmean")
annual_mean_temp <- mean(mean_temp_map)/10 #data comes as degrees * 10

allLeaves$mean_annual_temp <- raster::extract(annual_mean_temp, cbind(x=allLeaves$long, y=allLeaves$lat))
allLeavesW$mean_annual_temp <- raster::extract(annual_mean_temp, cbind(x=allLeavesW$long, y=allLeavesW$lat))
wrightAndSyz$mean_annual_temp <- raster::extract(annual_mean_temp, cbind(x=wrightAndSyz$long, y=wrightAndSyz$lat))
allLeaves3$mean_annual_temp <- raster::extract(annual_mean_temp, cbind(x=allLeaves3$long, y=allLeaves3$lat))
wrightAndSyzAND3$mean_annual_temp <- raster::extract(annual_mean_temp, cbind(x=wrightAndSyzAND3$long, y=wrightAndSyzAND3$lat))
figs3Species$mean_annual_temp <- raster::extract(annual_mean_temp, cbind(x=figs3Species$long, y=figs3Species$lat))
AllFigsAnd3Species$mean_annual_temp <- raster::extract(annual_mean_temp, cbind(x=AllFigsAnd3Species$long, y=AllFigsAnd3Species$lat))
locShortFigs$mean_annual_temp <- raster::extract(annual_mean_temp, cbind(x=locShortFigs$long, y=locShortFigs$lat))

annual_map <- getData(name="worldclim", res=2.5, var="prec")
annual_mean_prec <- sum(annual_map) #comes as monthly rain

allLeaves$annual_mean_prec <- raster::extract(annual_mean_prec, cbind(x=allLeaves$long, y=allLeaves$lat))
allLeavesW$annual_mean_prec <- raster::extract(annual_mean_prec, cbind(x=allLeavesW$long, y=allLeavesW$lat))
wrightAndSyz$annual_mean_prec <- raster::extract(annual_mean_prec, cbind(x=wrightAndSyz$long, y=wrightAndSyz$lat))
allLeaves3$annual_mean_prec <- raster::extract(annual_mean_prec, cbind(x=allLeaves3$long, y=allLeaves3$lat))
wrightAndSyzAND3$annual_mean_prec <- raster::extract(annual_mean_prec, cbind(x=wrightAndSyzAND3$long, y=wrightAndSyzAND3$lat))
figs3Species$annual_mean_prec <- raster::extract(annual_mean_prec, cbind(x=figs3Species$long, y=figs3Species$lat))
AllFigsAnd3Species$annual_mean_prec <- raster::extract(annual_mean_prec, cbind(x=AllFigsAnd3Species$long, y=AllFigsAnd3Species$lat))
locShortFigs$annual_mean_prec <- raster::extract(annual_mean_prec, cbind(x=locShortFigs$long, y=locShortFigs$lat))


theTheme = theme(legend.position = "none",
                 panel.background = element_rect(fill = 'white', colour = 'grey'), 
                 panel.border = element_rect(fill = NA), 
                 axis.title = element_text( size=7), 
                 axis.text = element_text(size=4),
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
)



###################################################
#Map loctions

#Syzygium australe
#Syzygium oleosum
#Syzygium smithii

#"Ficus coronata"
"Ficus fraseri"
#"Ficus opposita"



"Ficus obliqua"
"Ficus rubiginosa f. rubiginosa"



#specieName = "Ficus fraseri"
#onespecies = locShortFigs %>% filter(species == specieName)

specieName = "Syzygium australe"
onespecies = allLeaves %>% filter(species == specieName)
#length(allOleosum$species)

threespecies = allLeaves %>% filter(species == "Syzygium australe" | species == "Syzygium oleosum" | species == "Syzygium smithii")

 

#################################################################
#SWITCH COLOURS
values = c(dotGrey, rPink, rGreen, rYellow, rAqua, rOrange)
#values = c(rPink, rYellow, rAqua, rOrange)
theColours = values
mapColour = mapGrey
#################################################################


#########################################
#USE THIS TO PICK THE SPECIES OR MULTIPLE SPECIES TO MAP

#plotThis = allLeaves #AllSpecies
#plotThis = allLeavesW #Wright data #AllSpecies
#saveName = "Ficus" #name for the files, Globe for all genus, Syzygium or Ficus

#OR
#plotThis = onespecies
#saveName = specieName

#OR
#plotThis = wrightAndSyz
#saveName = "wrightAndSyz"

#OR
#plotThis = allLeaves3
#saveName = "syzygium3"

#OR
plotThis = wrightAndSyzAND3
saveName = "ALLandFigs"

#OR
#plotThis = figs3Species
#saveName = "ficus3"

#OR
#plotThis = AllFigsAnd3Species
#saveName = "AllFigs"

#OR
#plotThis = locShortFigs
#saveName = "AllFigs"


#########################################
#A1#A1#A1#A1#A1#A1#A1#A1#A1#A1
#WORLD

world_map <- map_data("world")

(australasia <- ggplot(data = world_map) + 
    ggtitle("A") +
    xlab("Longitude") + 
    ylab("Latitude") +
    geom_polygon(data=world_map, 
                 aes(x=long, y=lat, group=group),  
                 colour=mapColour, fill=mapColour) +
    #Globe
    coord_sf(xlim = c(-180, 200), ylim = c(-60, 90), expand = FALSE) +
    scale_fill_viridis_d(option = "plasma") +
    theTheme) +
    geom_point(data=plotThis, 
             aes(x=long, y=lat, colour = factor(species)), 
             pch=pointShape, 
             size=.2, 
             alpha=I(1)) + 
  scale_colour_manual(values = theColours)

saveAs = paste(path,  saveName, "Globe_A", ".pdf",  sep = "_")
ggsave(saveAs, width = 17.8, height = 11, units = "cm")




#########################################
#A#A#A#A#A#A#A#A#A#A#A#A#A#A#A#A#A#A#A#A
#AUSTRALIA ONLY#AUSTRALIA ONLY#AUSTRALIA ONLY#AUSTRALIA ONLY#AUSTRALIA ONLY#AUSTRALIA ONLY
world_map <- map_data("world")

(australasia <- ggplot(data = world_map) + 
    ggtitle("A") +
    xlab("Longitude") + 
    ylab("Latitude") +
    geom_polygon(data=world_map, 
                 aes(x=long, y=lat, group=group),  
                 colour=mapColour, fill=mapColour) +
    
    #Aus
    coord_sf(xlim = c(50, 180.12), ylim = c(-50, 10), expand = FALSE) +
    scale_fill_viridis_d(option = "plasma") +
    theTheme) +
  geom_point(data=plotThis, 
             aes(x=long, y=lat, group=species, colour = species), 
             pch=pointShape, 
             size=.2, 
             alpha=I(1)) + 
  scale_colour_manual(values = theColours)




saveAs = paste(path,  saveName, "_au_A", ".pdf",  sep = "_")
ggsave(saveAs, width = theWidth +15 , height  = theHeight + 6, units = "cm")
#USE Patchwork to layout the graphs




###################################################
#BNEW
plotThis %>% group_by(species) %>%
  ggplot(aes(x = abs(lat), y=area, col=species)) + 
  theTheme +
  ggtitle("B") +
  xlab("Latitude") + 
  ylab(areaTitle) +
  geom_point(aes(colour = species), pch=pointShape, size=pointSize, alpha=I(pointAlpha))+
  geom_smooth(method = "lm", se = TRUE) +
  geom_quantile(quantiles = quant1, colour = quantColour, size = quantSize2, alpha = 0.3)+
  geom_quantile(quantiles = quant2, colour = quantColour, size = quantSize2, alpha = 0.3)+
  scale_color_manual(values = theColours) +
  scale_y_log10()

saveAs = paste(path, saveName, "B", ".pdf",  sep = "_")
ggsave(saveAs, width = theWidth , height = theHeight , units = "cm")





###################################################
#C#C#C#C#C#C#C#C#C#C#C#C#C#C#C#C#C#C#C#C#C#C#C#C#C#C#C#C#C#C

plotThis %>% group_by(species) %>%
  ggplot(aes(x = mean_annual_temp, y=area, col=species)) + 
  theTheme +
  ggtitle("C") +
  xlab("Mean Annual Temperature") + 
  ylab(areaTitle) +
  geom_point(aes(colour = species), pch=pointShape, size=pointSize, alpha=I(pointAlpha))+
  geom_smooth(method = "lm", se = TRUE) +
  geom_quantile(quantiles = quant1, colour = quantColour, size = quantSize2, alpha = 0.3)+
  geom_quantile(quantiles = quant2, colour = quantColour, size = quantSize2, alpha = 0.3)+
  scale_color_manual(values = theColours) +
  scale_y_log10()

saveAs = paste(path, saveName, "C", ".pdf",  sep = "_")
ggsave(saveAs, width = theWidth , height = theHeight , units = "cm")

###################################################
#D#D#D#D#D#D#D#D#D#D#D#D#D#D#D#D#D#D#D#D#D#D#D#D

plotThis %>% group_by(species) %>%
  ggplot(aes(x=annual_mean_prec,y=area, col=species)) + 
  theTheme +
  ggtitle("D") +
  xlab("Mean Annual Precipitation") + 
  ylab(areaTitle) +
  geom_point(aes(colour = species), pch=pointShape, size=pointSize, alpha=I(pointAlpha))+
  geom_smooth(method = "lm", se = TRUE) +
  geom_quantile(quantiles = quant1, colour = quantColour, size = quantSize2, alpha = 0.3)+
  geom_quantile(quantiles = quant2, colour = quantColour, size = quantSize2, alpha = 0.3)+
  scale_color_manual(values = theColours) +
  scale_y_log10()

saveAs = paste(path, saveName, "D", ".pdf",  sep = "_")
ggsave(saveAs, width = theWidth , height = theHeight , units = "cm")




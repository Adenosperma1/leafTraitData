
library(tidyverse)
library(ggplot2)



#set paths
pathSelectSet = "/Users/brendanwilde/HONOURS/DATA/SYZ/validation\ data/Log_measurements_select1.csv"
pathRandomSet = "/Users/brendanwilde/HONOURS/DATA/SYZ/validation\ data/Log_measurements_random1.csv"

#read data in
dataAllSelect = read.csv(file=pathSelectSet, sep = ',')
dataAllRandom = read.csv(file=pathRandomSet, sep = ',')

#make tables
dataSelect <- as_tibble(dataAllSelect)
dataRandom <- as_tibble(dataAllRandom)

#reduce columns
dataSelectReduced = dataSelect %>% dplyr::select(cm.Area.Mask, cm.Area.Validaiton)
dataRandomReduced = dataRandom %>% dplyr::select(cm.Area.Mask)


#rename headings select
dataSelectRenamed = dataSelectReduced %>% 
  rename(
    Area = cm.Area.Mask,
    AreaVal = cm.Area.Validaiton
  )


#rename headings random 
dataRandomRenamed = dataRandomReduced %>% 
  rename(
    Area = cm.Area.Mask,
  )


#remove NA's
dataSelectClean <- na.omit(dataSelectRenamed) 
dataRandomClean <- na.omit(dataRandomRenamed) 






##########################################################
#variables for layout

pointColour = "#7cae00"
lightColour = rgb(219, 228, 201, maxColorValue = 255)

pointShape = 20
pointSize = 1
pointAlpha = 1
theWidth = 7
theHeight = 6
quant1 = 0.05
quant2 = 0.95
path = "/Users/brendanwilde/HONOURS/_manuscript/art/workingfiles/workings/graphs/"
quantColour = "black"
quantSize = .5
quantSize2 = .2

theTheme = theme(panel.background = element_rect(fill = 'white', colour = 'white'), 
                 panel.border = element_rect(fill = NA), 
                 axis.title = element_text( size=7), 
                 axis.text = element_text(size=4),
                 panel.grid.minor = element_blank(), 
                 legend.text=element_text(size=6),
                 legend.position="none",
                 legend.title=element_blank(),
                 legend.background = element_blank()
                 )




############################################################################################################################################################################
#A#A#A#A#A#A#A#A#A#A#A#A#A#A#A#A#A#A#A#A#A#A#A#A#A#A#A#A#A#A#A#A#A#A#A#A


#Validation Area vs Predicted Area
ggplot(dataSelectClean,aes(x=Area,y=AreaVal)) + 
  ggtitle("A") + 
  xlab("Predicted Area (log scale cm²)") + 
  ylab("Validation Area (log scale cm²)") +
  geom_point(colour=pointColour, pch=pointShape, size=pointSize, alpha=I(pointAlpha))+
  theTheme +
  geom_abline(colour=quantColour, size = .2) +
  geom_quantile(quantiles = quant1, colour = quantColour, size = quantSize2, alpha = 0.3)+
  geom_quantile(quantiles = quant2, colour = quantColour, size = quantSize2, alpha = 0.3) +
  scale_x_log10() + scale_y_log10()
saveAs = paste(path, "ValidationANEW.pdf",  sep = "_")
ggsave(saveAs, width = theWidth, height = theHeight, units = "cm")

##########################################################
#B#B#B#B#B#B#B#B#B#B#B#B#B#B
n = length(dataSelectClean$AreaVal)

ggplot(dataSelectClean, aes(AreaVal - Area)) +
  ggtitle("B") + 
  xlab("Validation - Predicated Area (cm²)") + 
  ylab("Count") +
  geom_histogram(binwidth = .5,  fill=pointColour) +
  theTheme 

saveAs = paste(path, "ValidationBNEW.pdf",  sep = "_")
ggsave(saveAs, width = theWidth, height = theHeight, units = "cm")






##########################################################
#ROOT MEANS SQUARE calc

sqrt(mean((dataSelectClean$Area - dataSelectClean$AreaVal)^2))



##########################################################
#C#C#C#C#C#C#C#C#C#C#C#C

xVal = log(dataSelectClean$AreaVal)
yVal = (log(dataSelectClean$AreaVal) - log(dataSelectClean$Area)) / log (dataSelectClean$AreaVal)

#validation vs predicated - validation log
ggplot(dataSelectClean,aes(x=xVal, y= yVal)) + 
  ggtitle("C") + 
  xlab("Validation Area (log scale cm²)") + 
  ylab("(Predicted - Validation Area) / validation area (log scale cm²)") +
  geom_point(colour=pointColour, pch=pointShape, size=pointSize)+
  theTheme +
  geom_smooth(method="lm",se = TRUE, colour = quantColour, size = quantSize2) +
  geom_quantile(quantiles = quant1, colour = quantColour, size = quantSize2, alpha = 0.3)+
  geom_quantile(quantiles = quant2, colour = quantColour, size = quantSize2, alpha = 0.3) 
saveAs = paste(path, "ValidationCNEW.pdf",  sep = "_")
ggsave(saveAs, width = theWidth, height = theHeight, units = "cm")



#######################################################################################################################################
#D##D##D##D##D##D##D##D##D##D##D##D##D##D#


#Plot the distribution of leaf areas for the select and random models on top of each other
data <- data.frame(
  type = c( 
    rep("Random", length(dataRandomRenamed$Area)), #don't remove NA's!
    rep("Select", length(dataSelectRenamed$Area)) #don't remove NA's!
  ),
  area = c(
    dataRandomRenamed$Area,
    dataSelectRenamed$Area
  ))



p1 <- ggplot(data=data, aes(x=area, group=type, fill=type)) +
  ggtitle("D") + 
  geom_density(alpha=.7, color=NA) + 
  scale_fill_manual(values=c(lightColour, pointColour)) + 
  xlab("Predicated Area (log scale cm²)") + 
  ylab("Density") +
  theTheme +
  scale_x_log10()



p1
saveAs = paste(path, "ValidationDNEW.pdf",  sep = "_")
ggsave(saveAs, width = theWidth, height = theHeight, units = "cm")


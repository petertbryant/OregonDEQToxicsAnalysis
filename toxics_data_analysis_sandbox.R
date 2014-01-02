library(RODBC)
library(reshape2)
library(doBy)
library(ggplot2)
library(plyr)

#Connect to database
con <- odbcConnectAccess('S:/TOXICS_2012/Data/Toxics_7_2_12.mdb')

Data <- sqlQuery(con, 'SELECT * FROM 2011_Actually', nullstring = "")
Data$PARAMETER_MODIFIER_ABBREVIATION[Data$PARAMETER_MODIFIER_ABBREVIATION == ''] <- NA
Data$Analyte <- ifelse(is.na(Data$PARAMETER_MODIFIER_ABBREVIATION), paste(Data$NAME), 
                       paste(Data$PARAMETER_MODIFIER_ABBREVIATION, Data$NAME, sep=" "))

Data <- Data[,c('SAMPLE_DATE', 'SUBPROJECT_NAME', 'STATION', 'Analyte','RESULT', 'PRV', 'Detect/Nondetect', 'UNIT')]
colnames(Data) <- c("sample_date", "subproject", 'station', 'analyte', 'result', 'prv', 'detect', 'unit')

Data <- Data[Data$result != 'Cancelled',]
Data <- Data[Data$result != 'Void',]

Data$prv <- as.character(Data$prv)
Data$prv <- as.numeric(Data$prv)

Data.subproject.summary <- ddply(Data, .(station, analyte), summarise, 
              N      = sum(!is.na(prv)),
              min    = min(prv, na.rm=T) ,
              max    = max(prv, na.rm=T) ,
              median = median(prv, na.rm=T),
              mean   = mean(prv, na.rm=T),
              sd     = sd(prv, na.rm=T))

library(googleVis)
Walla.Walla <- Data[Data$subproject == 'TMP-Water-Walla Walla',]
Walla.Walla.Detects <- Walla.Walla[Walla.Walla$detect == 1,]
wide <- dcast(Walla.Walla.Detects, station + sample_date ~ analyte + unit, mean, value.var="prv")

wide <- dcast(summaried, name ~ station, value.var="mean")

regulars.list <- c('Conductivity', 'Dissolved Organic Carbon', 'Dissolved Oxygen', 'Hardness as CaCO3', 
               'pH', 'Temperature', 'Total Organic Carbon', 'Total Solids', 'Total Suspended Solids',
               'Turbidity')

#playing with wq package
long <- Data[,c('sample_date', 'station', 'analyte', 'prv')]
long$depth = NA
long$depth <- as.numeric(long$depth)
long <- long[which(long$station != '10000'),]
long <- wqData(long, locus = c('sample_date', 'station','depth'), wqdata = c('analyte', 'prv'), 
       site.order = T, type = 'long')
q <- plot(long, vars = 'beta-Sitosterol')
q + opts(axis.text.x=theme_text(angle=-90, hjust=0))

#isolating qc hits
qc <- subset(Data, Data$station == '10000')
qc <- qc[qc$value != 0.0,]
qc <- qc[-grep('^Field', qc$variable),]

#example dataset line and point displays
d <- expand.grid(event = 1:5, site = 1:10)
d$conc <- rlnorm(50)
d$x <- runif(10)[d$site]
d$y <- runif(10)[d$site]
d$site <- reorder(d$site, d$conc)
d # raw data:  you can add colors for nondetect or not, etc.  
ggplot() + geom_line(aes(x = event, y = conc ), data = d) + facet_wrap(~ site)
mean.d <- ddply(d, .(site, x, y), summarize, mean = mean(conc))
ggplot() + geom_point(aes(x = x, y = y, size = mean), data = mean.d) 

#separating out rogue data
rogue <- subset(Data, Data$subproject == c('TMP-Water-Rogue'))
rogue <- rogue[rogue$station!='10000',]
rogue$result <- sub("<", "", rogue$result, fixed = T)
rogue$result <- trim(sub('est', '', rogue$result, fixed = T))
rogue$result <- trim(sub('Est', '', rogue$result, fixed = T))
#working on a single analyte
rogue.beta = rogue[rogue$analyte == 'beta-Sitosterol',]
rogue.beta$bench <- ifelse(rogue.beta$result > 200, 'Y', 'N')
g <- ggplot(aes(x = sample_date, y = result, group = subproject, colour = bench), 
            data = rogue.beta)
g <- g + facet_wrap(~station)
g <- g + geom_point(shape = 1, size = 4)
g <- g + geom_line(aes(y = 200), colour = 'black')
g <- g + ggtitle(title = "beta-Sitosterol") 
g <- g + xlab("Sample Date") + ylab("Value (ng/L)")
g <- g + labs(values= "Above Benchmark?")
g
ggsave(filename = 'E:/plottest.pdf')
#adding gps coordinates
xvals <- c('42.4968', '42.5253', '42.3975', '42.2244', '42.4554', '42.4047', '42.6594', '42.2384', '42.4486')
yvals <- c("-123.4873", '-122.8416', '-123.4558', '-122.7449', '-122.8550', '-122.9377', '-122.6989', '-123.6868', '-123.0421')
cutty <- as.factor(rogue.beta$station)
#levels(cutty) <- xvals #run this first
levels(cutty) <- yvals #run this second
#rogue.beta$x <- cutty #run this first
rogue.beta$y <- cutty #run this second
mean.rb <- ddply(rogue.beta, .(station, x, y), summarize, mean = mean(result))


for (i in 1:length(Data$detect)) 
  Data$detect <- ifelse(Data$prv != 0, 1, 0)

Data$detect[ is.na(Data$detect), ] <- 0
detects <- subset(Data, Data$detect == '1')
nondetects <- subset(Data, Data$detect == '0')



#fish data table split
fish <- read.csv('S:/TOXICS_2012/Data/Fish/2011_Fish_Data.csv')
names(fish) <- gsub('total.', 'total_', names(fish))
names(fish) <- gsub('Total.', 'total_', names(fish))
names(fish) <- gsub('alpha.', 'alpha_', names(fish))

for (i in 4:length(fish)) {
  fname <- paste('fish', names(fish)[i], '.csv', sep = "_")
  selection <- c(1:3, i)
  tmp <- subset(fish, select = selection)
  write.csv(tmp, file = fname)
}

#chart of average number of detected compound groupings by species 
general <- read.csv('S:/TOXICS_2012/Data/Fish/2011_Fish_ppb.csv')
general <- within(general, rm('lat', 'lon'))
rg <- general[general$Basin == 'Rogue',]
mc <- ddply(rg, "Species", summarise, mc = mean(Count_over_ALR))
ggplot(rg, aes(x=Species, y=Count_over_ALR)) + 
  facet_wrap(~name) +
  geom_bar(fill = c('light blue')) + 
  ggtitle('Number of Detected Analytes over Screening Value') + 
  theme_bw() + 
  ylab("") + 
  geom_text(aes(label = Count_over_ALR, vjust = 1.1))
ggsave(filename = "SV_DetectionbySpecies_Rogue.jpg", width = 6, height = 4.5)


#count of unique chemical detections
data <- read.csv("2011_samples.csv")
data <- data[data$Focus.List.Category != "Standard Parameters",]
data <- data[data$PARAMETER_MODIFIER_ABBREVIATION != "Dissolved",]

basins <- unique(data$SUBPROJECT_NAME)

basin.count <- NULL
for (i in 1:length(basins)) {
  
  tmp <- subset(data, data$SUBPROJECT_NAME == basins[i], 
                select = c("SUBPROJECT_NAME", "STATION", "Name.full", "Detect.Nondetect"))
  detects <- tmp[tmp$Detect.Nondetect == 1,]
  detect.names <- unique(detects$Name.full)
  detect.names <- as.character(detect.names)
  #tmp <- subset(tmp, tmp$Name.full %in% detect.names)  
  #tmp$Name.full <- as.character(tmp$Name.full)
  #tmp$SUBPROJECT_NAME <- as.character(tmp$SUBPROJECT_NAME)
  
  basin.tmp <- c(basin = as.character(basins[i]), count = as.numeric(length(detect.names)))
    
  basin.count <- rbind(basin.count, basin.tmp)
}

basin.count<-as.data.frame(basin.count, optional = T)
rownames(basin.count) <- NULL
write.csv(basin.count, file = "z:/TOXICS_2012/Data/2011_Basin_Count_Unique_Compounds.csv")

#count of unique chemicals by stations
data <- read.csv("2011_samples.csv")
data <- data[data$Focus.List.Category != "Standard Parameters",]
data <- data[data$PARAMETER_MODIFIER_ABBREVIATION != "Dissolved",]

stations <- unique(data$STATION)

station.count <- NULL

for (i in 1:length(stations)) {
  
  tmp <- subset(data, data$STATION == stations[i], 
                select = c("SUBPROJECT_NAME", "STATION", "Name.full", "Detect.Nondetect"))
  detects <- tmp[tmp$Detect.Nondetect == 1,]
  detect.names <- unique(detects$Name.full)
  detect.names <- as.character(detect.names)
  #tmp <- subset(tmp, tmp$Name.full %in% detect.names)  
  #tmp$Name.full <- as.character(tmp$Name.full)
  #tmp$SUBPROJECT_NAME <- as.character(tmp$SUBPROJECT_NAME)
  
  station.tmp <- c(station = as.character(stations[i]), basin = as.character(unique(detects$SUBPROJECT_NAME)), count = as.numeric(length(detect.names)))
  
  station.count <- rbind(station.count, station.tmp)
}

station.count<-as.data.frame(station.count, optional = T)
rownames(station.count) <- NULL
write.csv(station.count, file = "Z:/TOXICS_2012/Data/Station_Count_Unique_Compounds.csv")

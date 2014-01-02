library(plyr)
library(stringr)
library(reshape2)
options(stringsAsFactors = FALSE, 'scipen' = 100)
source('//deqlead01/wqm/TOXICS_2012/Data/hardness_eval_functions.R')
#library(xlsx)
#data <- read.xlsx('//deqlead01/wqm/TOXICS_2012/Data/2011 Access export & basin summaries 4DecKG.xlsx', sheetName = '2011_samples')
data.test <- read.csv('//deqlead01/wqm/TOXICS_2012/Data/2011allBasins.csv', stringsAsFactors = FALSE)

data.wo.void <- data[!data$PRV == 'VOID',]
data.wo.void[data.wo.void$PRV == 'ND','PRV'] <- 0
data.wo.void$METHOD_REPORTING_LIMIT <- as.numeric(data.wo.void$METHOD_REPORTING_LIMIT)
data.wo.void[which(as.numeric(data.wo.void$PRV) < data.wo.void$METHOD_REPORTING_LIMIT),'Detect.Nondetect'] <- 0

#Comparing hardness based criteria 
data.wo.void$ID <- paste(data.wo.void$STATION, data.wo.void$SAMPLE_DATE)

hm.evaluated <- data.frame()
for (j in 1:length(unique(constants$Name.alone))) {
  tmp <- hardnessEval(metal = unique(constants$Name.alone)[j], df = data.wo.void)
  tmp <- as.data.frame(lapply(tmp, as.character), stringsAsFactors = FALSE)
  ifelse(tmp == "", hm.evaluated <- hm.evaluated, ifelse(j == 1, hm.evaluated <- tmp, hm.evaluated <- rbind(hm.evaluated, tmp)))
}

View(arrange(hm.evaluated[,c('SUBPROJECT_NAME' ,'SAMPLE_DATE', 'STATION', 'DESCRIPTION','NAME', 'Name.full', 'PRVmetal', 'PRVhardness','crit.acute', 'crit.chronic', 'exceed.acute', 'exceed.chronic')], SUBPROJECT_NAME))
hm.evaluated$exceed.acute <- as.numeric(hm.evaluated$exceed.acute)
hm.evaluated$exceed.chronic <- as.numeric(hm.evaluated$exceed.chronic)
hm.evaluated$Detect.Nondetect <- as.numeric(hm.evaluated$Detect.Nondetect)

#two roll ups are required. one to subproject with total percent detection and number over screening value and the other to the station
#giving the maximum value from that station
roll.up <- ddply(hm.evaluated, .(SUBPROJECT_NAME, STATION, DESCRIPTION, NAME, Name.full), summarise, acute.exceed = sum(exceed.acute, na.rm = TRUE), 
      chronic.exceed = sum(exceed.chronic, na.rm = TRUE), sample.count = length(exceed.acute), detect.count = sum(Detect.Nondetect, na.rm = TRUE))
roll.up$percent.detect <- round(100*(roll.up$detect.count/roll.up$sample.count))

roll.up.detect <- roll.up[roll.up$percent.detect > 0,]

roll.up.exceed <- roll.up[roll.up$acute.exceed > 0 | roll.up$chronic.exceed > 0,]
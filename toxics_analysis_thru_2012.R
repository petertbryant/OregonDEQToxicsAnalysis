library(plyr)
library(stringr)
library(reshape2)
library(xlsx)
library(ggplot2)

source('//deqlead01/wqm/TOXICS_2012/Data/R/criteria.R')
source('//deqlead01/wqm/TOXICS_2012/Data/R/hardness_eval_functions_Element_Names.R')

options('scipen' = 50, stringsAsFactors = FALSE)

#Pull in the Element data that is final as of 1/3/2014. This data was queried using the SQL Query tool in Element. The text of that query
#is commented at the end of this file for future reference.
data.2012 <- read.csv('//deqlead01/wqm/TOXICS_2012/Data/Element_Final_Data_Qry_on_01062014.csv', stringsAsFactors = FALSE)

#The qualifiers from this query were concatenated into a single field which is kind of a pain to parse so in order to get Status
#I queried the qualifier table directly and will make a status column that can then be merged with the data table.
data.2012.qualifiers <- read.csv('//deqlead01/wqm/TOXICS_2012/Data/Element_Qualifier_Query_for_TMP_on_01032014.csv', stringsAsFactors = FALSE)

data.2012.qualifiers$id <- paste(data.2012.qualifiers$Wrk, data.2012.qualifiers$Sample, data.2012.qualifiers$Analysis, data.2012.qualifiers$Analyte)

data.2012.qualifiers$Status <- substr(data.2012.qualifiers$Qualifier, nchar(data.2012.qualifiers$Qualifier),nchar(data.2012.qualifiers$Qualifier))

data.2012.qualifiers$Status <- as.factor(data.2012.qualifiers$Status)

data.2012.qualifiers$Status <- revalue(data.2012.qualifiers$Status, c('a' = 1, 'A' = 1, 'b' = 2, 'B' = 2, 'c' = 3, 'C' = 3, 'D' = 4, 'Q' = 1))

data.2012.qualifiers$Status <- as.numeric(data.2012.qualifiers$Status)

processed <- ddply(data.2012.qualifiers, .(id), summarise, Status = max(Status))

processed$Status <- as.factor(processed$Status)

processed$Status <- revalue(processed$Status, c('1' = 'A', '2' = 'B', '3' = 'C', '4' = 'D'))

processed$AnalyteStatus <- as.character(processed$Status)

processed <- processed[,c('id','AnalyteStatus')]

#now that the qualifiers have been parsed and the DQL determined we can associate them with the data itself
data.2012$id <- paste(data.2012$Wrk, data.2012$Sample, data.2012$Analysis, data.2012$Analyte)
data.2012.w.qualifiers <- merge(data.2012, processed, by = 'id', all.x = TRUE)

#the above only captures the analyte level qualifiers. the below will also capture the sample level qualifiers
sample.qualifiers <- data.2012.qualifiers[data.2012.qualifiers$Analyte == '',]

sample.qualifiers$sid <- paste(sample.qualifiers$Wrk, sample.qualifiers$Sample, sample.qualifiers$Analysis)

sample.qualifiers$Status <- substr(sample.qualifiers$Qualifier, nchar(sample.qualifiers$Qualifier),nchar(sample.qualifiers$Qualifier))

sample.qualifiers$Status <- as.factor(sample.qualifiers$Status)

sample.qualifiers$Status <- revalue(sample.qualifiers$Status, c('a' = 1, 'A' = 1, 'B' = 2, 'D' = 4, 'Q' = 1))

sample.qualifiers$Status <- as.numeric(sample.qualifiers$Status)

processed <- ddply(sample.qualifiers, .(sid), summarise, Status = max(Status))

processed$Status <- as.factor(processed$Status)

processed$Status <- revalue(processed$Status, c('1' = 'A', '2' = 'B', '3' = 'C', '4' = 'D'))

processed$SampleStatus <- as.character(processed$Status)

processed <- processed[,c('sid','SampleStatus')]

data.2012.w.qualifiers$sid <- paste(data.2012.w.qualifiers$Wrk, data.2012.w.qualifiers$Sample, data.2012.w.qualifiers$Analysis)
data.2012.w.qualifiers <- merge(data.2012.w.qualifiers, processed, by = 'sid', all.x = TRUE)

#This brings the status into a single column
data.2012.w.qualifiers$Status <- ifelse(is.na(data.2012.w.qualifiers$AnalyteStatus),
                                        ifelse(is.na(data.2012.w.qualifiers$SampleStatus),
                                               'A',
                                               data.2012.w.qualifiers$SampleStatus),
                                        data.2012.w.qualifiers$AnalyteStatus)

#i want to check on whether all voided samples should be DQL 'D'. if so, add the code to do that here
data.2012.w.qualifiers$Status <- ifelse(data.2012.w.qualifiers$tResult %in% c('Void', 'Cancelled'),
                                        'D',
                                        data.2012.w.qualifiers$Status)

#this simplifies the qualifier and status columns and writes it back to the dataframe name that is used from here on
data.2012 <- within(data.2012.w.qualifiers, rm('sid','id','AnalyteStatus','SampleStatus'))

#There is a site in the Deschutes basin that was sampled during a John Day basin sampling event and was associated with
#the John Day Project. This puts it in the right Project for consistency.
data.2012[data.2012$SampleRegID == 10411,'Project'] <- 'Deschutes'


#this 2012 data is preliminary so it's possible some of the column names
#will change the next time we get the data
# files.2012 <- list.files('//deqlead01/wqm/toxics_2012/data/2012 data/raw data/')
# 
# for (i in 1:length(files.2012)){
#   file.path <- paste('//deqlead01/wqm/toxics_2012/data/2012 data/raw data/', files.2012[i], sep = '')
#   tmp <- read.xlsx2(file.path, sheetName = 'Data')
#   ifelse(i == 1, ref <- names(tmp), tmp <- tmp[,ref])
#   print(names(tmp))
#   ifelse(i == 1, 
#          data.2012 <- tmp,
#          data.2012 <- rbind(data.2012, tmp))
# }
# 
# data.2012 <- data.2012[data.2012$Analyte != '',]
# 
# data.2012[data.2012$Analyte == 'Inorganic Arsenic, Total','Analyte'] <- 'Arsenic, Total inorganic'

#for this analysis we can leave out blanks and field duplicates (we may want to check the duplicates later and use a detect
#when we don't have a primary detection)
data.2012 <- data.2012[!data.2012$SampleType %in% c('Blank - Equipment::EB', 'Blank - Transfer::TfB', 'Field Duplicate::FD'),]

#this process was initially developed with LASAR data so I will change the element names to lasar names for now
# i want to go the other way eventually but just want to see some results right now
data.2012 <- data.2012[,c('Project', 'SampleRegID', 'SampleAlias','Sampled', 'SampleType','Analyte','tResult', 'tMRL', 'Unit', 'SpecificMethod', 'Status')]
#data.2012 <- rename(data.2012, c('Project' = 'SUBPROJECT_NAME', 'SampleRegID' = 'STATION', 'SampleAlias' = 'DESCRIPTION','Sampled' = 'SAMPLE_DATE', 'Analyte' = 'NAME',
#                                 'tResult' = 'PRV', 'tMRL' = 'METHOD_REPORTING_LIMIT', 'Units' = 'UNIT', 'SpecificMethod' = 'METHOD_CODE'))

#both of these data sets are from the LASAR database and have different column names
data.2011 <- read.csv('//deqlead01/wqm/TOXICS_2012/Data/2011allBasins.csv', stringsAsFactors = FALSE)
willy.data <- read.csv('//deqlead01/wqm/TOXICS_2012/Data/Willamette water data.csv', stringsAsFactors = FALSE)

willy.metals <- read.xlsx2('//deqlead01/wqm/TOXICS_2012/Data/willamette metals_KG.xlsx', sheetName = 'Sheet2')
willy.metals <- rename(willy.metals, c("Sampling.Event.Number" = 'SAMPLING_EVENT_KEY', "Station.Identifier" = 'SampleRegID', 
                                       "Station.Description" = 'SampleAlias', "Sample.Date.Time" = 'Sampled',
                                       "QA.QC.Type" = 'QA_QC_TYPE', 
                                       "Field.Conductivity...µmhos.cm...25..C." = 'Conductivity',
                                       "Field.pH...SU." = 'pH', 
                                       "Field.Turbidity...NTU." = 'Turbidity', 
                                       "TR.Antimony.Result" = 'Antimony, Total recoverable', 
                                       "Total.Recoverable.Arsenic...µg.L." = 'Arsenic, Total recoverable',
                                       "Total.Recoverable.Barium...µg.L." = 'Barium, Total recoverable',
                                       "Total.Recoverable.Beryllium...µg.L." = 'Beryllium, Total recoverable',
                                       "Total.Recoverable.Cadmium...µg.L." = 'Cadmium, Total recoverable',
                                       "Total.Recoverable.Calcium...mg.L." = 'Calcium, Total recoverable',
                                       "Total.Recoverable.Chromium...µg.L." = 'Chromium, Total recoverable',
                                       "Total.Recoverable.Cobalt...µg.L." = 'Cobalt, Total recoverable',
                                       "Total.Recoverable.Copper...µg.L." = 'Copper, Total recoverable',
                                       "Total.Recoverable.Iron...µg.L." = 'Iron, Total recoverable',
                                       "Total.Recoverable.Lead...µg.L." = 'Lead, Total recoverable',
                                       "Total.Recoverable.Magnesium...mg.L." = 'Magnesium, Total recoverable',
                                       "Total.Recoverable.Molybdenum...µg.L." = 'Molybdenum, Total recoverable',
                                       "Total.Recoverable.Nickel...µg.L." = 'Nickel, Total recoverable',
                                       "Total.Recoverable.Selenium...µg.L." = 'Selenium, Total recoverable',
                                       "Total.Recoverable.Silver...µg.L." = 'Silver, Total recoverable',
                                       "Total.Recoverable.Thallium...µg.L." = 'Thallium, Total recoverable',
                                       "Total.Recoverable.Uranium...µg.L." = 'Uranium, Total recoverable',
                                       "Total.Recoverable.Vanadium...µg.L." = 'Vanadium, Total recoverable',
                                       "Total.Recoverable.Zinc...µg.L." = 'Zinc, Total recoverable',
                                       "Total.Suspended.Solids...mg.L." = 'Total Suspended Solids'))
willy.metals <- within(willy.metals, rm('Sample.ID','pH...SU.','Conductivity','pH','Turbidity',
                                        'Total Suspended Solids','QA_QC_TYPE','SAMPLING_EVENT_KEY'))
willy.metals.melted <- melt(willy.metals, id.vars = c('SampleRegID','SampleAlias','Sampled'),variable.name = 'Analyte',value.name='tResult')
willy.metals.melted$Sampled <- as.POSIXct(as.numeric(willy.metals.melted$Sampled)*24*3600 + as.POSIXct("1899-12-30 00:00") )
willy.metals.melted$Sampled <- strftime(willy.metals.melted$Sampled, format = '%d-%b-%y')
willy.metals.melted$SampleRegID <- substr(willy.metals.melted$SampleRegID, 1, 5)
willy.metals.melted$SpecificMethod <- '200.8'
willy.metals.melted$Project <- 'TMP-Water-Willamette'
willy.metals.melted$tMRL<- NA
willy.metals.melted$Unit <- willy.metals.melted$Analyte
willy.metals.melted$Unit <- mapvalues(willy.metals.melted$Unit, 
                                      from = unique(willy.metals.melted$Analyte), 
                                      to = c('µg/L','µg/L','µg/L','µg/L','µg/L','mg/L','µg/L','µg/L','µg/L','µg/L',
                                             'µg/L','mg/L','µg/L','µg/L','µg/L','µg/L','µg/L','µg/L','µg/L','µg/L'))
willy.metals.melted$tResult <- ifelse(substr(willy.metals.melted$tResult,1,1) == '<','ND',willy.metals.melted$tResult)
willy.metals.melted$Status <- 'A'
willy.metals.melted$SampleType <- ''

data.2011 <- data.2011[,names(willy.data)]

#data.2011 <- data.2011[,names(data.2012)]
#willy.data <- willy.data[,names(data.2012)]

lasar <- rbind(data.2011, willy.data)


#This makes the lasar data for metals names equal to the metals names as they are in Element
lasar[lasar$METHOD_CODE == '200.8','NAME'] <- gsub('R', 'r', (paste(lasar[lasar$METHOD_CODE == '200.8','NAME'], 
                                                                          lasar[lasar$METHOD_CODE == '200.8','PARAMETER_MODIFIER_ABBREVIATION'], 
                                                                          sep = ', ')))



lasar <- rename(lasar, c('SUBPROJECT_NAME' = 'Project', 'STATION' = 'SampleRegID', 'DESCRIPTION' = 'SampleAlias','SAMPLE_DATE' = 'Sampled', 
                         'QA_QC_TYPE' = 'SampleType','NAME' = 'Analyte', 'PRV' = 'tResult', 'METHOD_REPORTING_LIMIT' = 'tMRL', 'UNIT' = 'Unit', 
                         'METHOD_CODE' = 'SpecificMethod', 'STATUS' = 'Status'))

#this makes the columns in the LASAR data match the columns in the Element data frame
lasar <- lasar[,names(data.2012)]

#this file is to reconcile the names between lasar and element
lasar.to.element <- read.xlsx('//deqlead01/wqm/TOXICS_2012/Data/lasar_to_element.xlsx', sheetName = 'Lasar to Element All')

lasar.to.element[lasar.to.element$Element.Analyte == '4,4Â´-DDD','Element.Analyte'] <- '4,4´-DDD'
lasar.to.element[lasar.to.element$Element.Analyte == '4,4Â´-DDE','Element.Analyte'] <- '4,4´-DDE'
lasar.to.element[lasar.to.element$Element.Analyte == '4,4Â´-DDT','Element.Analyte'] <- '4,4´-DDT'

lasar.change <- lasar.to.element[!is.na(lasar.to.element$Lasar.Name.full),]

lasar.change <- lasar.change[lasar.change$Lasar.Name.full != '',]

#the name reconciliation occurs in the next four lines of code
lasar.change.vector <- lasar.change$Element.Analyte
names(lasar.change.vector) <- lasar.change$Lasar.Name.full

lasar$Analyte <- as.factor(lasar$Analyte)

lasar$Analyte <- revalue(lasar$Analyte, lasar.change.vector)



#this puts the LASAR and Element data together
data <- rbind(data.2012, lasar, willy.metals.melted)

#There are several stations that have double spaces in their names and several stations that double spaces for some records
#and not others. This removes those double spaces for consistent naming.
data$SampleAlias <- gsub("  "," ",data$SampleAlias)

#This pulls out empty rows
data <- data[!is.na(data$SampleRegID),]

#we also want to add in the Focus List Categories
categories <- read.xlsx2('//deqlead01/wqm/toxics_2012/data/Focus list and 737 categories.xlsx', sheetName = 'Sheet1')
#inside.file <- read.xlsx2('//deqlead01/wqm/toxics_2012/data/2011 Access export & basin summaries 4DecKG.xlsx', sheetName = '737 & Focus List categories')
#category.vector <- lasar.to.element[!is.na(lasar.to.element$categories.Chemical),'Element.Analyte']
#names(category.vector) <- lasar.to.element[!is.na(lasar.to.element$categories.Chemical),'categories.Chemical']
categories.sub <- categories[,c('Chemical','chem.group')]
test <- merge(data, categories.sub, by.x = 'Analyte', by.y = 'Chemical')
sub <- test[!duplicated(test$Analyte),c('Analyte','Analyte')]
sub <- rename(sub, c('Analyte.1' = 'categories.Chemical'))
sub2 <- lasar.to.element[!is.na(lasar.to.element$categories.Chemical),c('Element.Analyte','categories.Chemical')]
sub2 <- rename(sub2, c('Element.Analyte' = 'Analyte'))
categories.mapped <- rbind(sub, sub2)

categories.mapped.vector <- categories.mapped$Analyte
names(categories.mapped.vector) <- categories.mapped$categories.Chemical
categories$Chemical <- as.factor(categories$Chemical)
categories$Chemical <- revalue(categories$Chemical, categories.mapped.vector)
categories$Chemical <- as.character(categories$Chemical)

categories.sub <- categories[,c('Chemical','chem.group')]
categories.sub <- categories.sub[!duplicated(categories.sub$Chemical),]

data.w.categories <- merge(data, categories.sub, by.x = 'Analyte', by.y = 'Chemical', all.x = TRUE)
unmatched.analytes <- unique(data.w.categories[is.na(data.w.categories$chem.group),'Analyte'])

#### Output combined data table ####
#write.xlsx(data, '//deqlead01/wqm/TOXICS_2012/Data/Analysis_through_2012/All_TMP_Water_Data_through_2012.xlsx', sheetName = 'AllDataThrough2012')

#### Continue with data analysis ####
#this eliminates VOIDED samples
data.wo.void <- data.w.categories[!data.w.categories$tResult %in% c('VOID','Void','Cancelled','',NA),]

#this sets the value for all the NDs to 0
data.wo.void[data.wo.void$tResult %in% c('ND','ND*'),'tResult'] <- 0

#This converts the MRL to a numeric field
data.wo.void$tMRL<- as.numeric(data.wo.void$tMRL)

#this converts the tResult to a numeric field
data.wo.void$tResult <- as.numeric(data.wo.void$tResult)

#Apparently the reporting limit for DEET has been raised to 30 per Lori
data.wo.void[data.wo.void$Analyte == 'DEET','tMRL'] <- 30

#populate the detect.nondetect column
data.wo.void[!is.na(data.wo.void$tMRL),'Detect.nondetect'] <- ifelse(data.wo.void[!is.na(data.wo.void$tMRL),'tResult'] 
                                                                     < data.wo.void[!is.na(data.wo.void$tMRL),'tMRL'],
                                                                     0, 1)
data.wo.void[is.na(data.wo.void$tMRL),'Detect.nondetect'] <- ifelse(data.wo.void[is.na(data.wo.void$tMRL),'tResult'] == 0,
                                                                                      0, 1) 


#for comparison we need to exclude recently added methods that don't apply to the entire dataset
data.wo.newmethods <- data.wo.void[!data.wo.void$SpecificMethod %in% c('EPA 1699','EPA 1613', 'EPA 1614A', 'EPA 1668C'),]
dwn.sub <- data.wo.newmethods[!data.wo.newmethods$chem.group %in% c('Standard Parameters','NA','Plant or animal sterols') & !is.na(data.wo.newmethods$chem.group),]
#The writing of this table using write.xlsx is too slow and throws an error with the Java Heap Space so to get it into the same file
#I write it to .csv first, which is fast then use the Move or Copy feature from the right click menu on the tab in Excel to place it 
#into the Data_Summary_DRAFT.xlsx file.
#write.csv(data.wo.newmethods, '//deqlead01/wqm/toxics_2012/data/r/Data_wo_NewMethods.csv', row.names = FALSE)

#list of unique detections with counts of nondetects and detects
detect.counts <- as.data.frame.matrix(table(dwn.sub$Analyte, dwn.sub$Detect.nondetect))
detect.counts$Analyte <- row.names(detect.counts)
detect.counts <- rename(detect.counts, c('0' = 'Nondetect', '1' = 'Detect'))
detect.counts <- merge(detect.counts, categories.sub, by.x = 'Analyte', by.y = 'Chemical', all.x = TRUE)
detect.counts$PercentDetection <- (detect.counts$Detect/(detect.counts$Nondetect + detect.counts$Detect))*100
View(arrange(detect.counts,desc(PercentDetection)))
#This creates the Data_Summary_DRAFT.xlsx file
#write.xlsx(detect.counts,'//deqlead01/wqm/toxics_2012/data/r/Data_Summary_DRAFT.xlsx',sheetName='TotalDetects',row.names = FALSE)

#number of unique compounds detected per station
by.station <- arrange(ddply(dwn.sub, .(Project,SampleRegID,SampleAlias), summarise, sum = sum(Detect.nondetect)),desc(sum))
by.station <- ddply(dwn.sub, .(Project,SampleRegID,SampleAlias), function(x) {length(unique(x[x$Detect.nondetect > 0,'Analyte']))})
by.station <- rename(by.station, c('V1' = 'count'))
by.station <- arrange(by.station, desc(count))
#write.xlsx(by.station,'//deqlead01/wqm/toxics_2012/data/r/Data_Summary_DRAFT.xlsx',sheetName='UniqueCompoundsByStation',row.names = FALSE,append = TRUE)

#number of unique compounds detected per chemical group per station
by.group <- ddply(dwn.sub, .(Project,SampleRegID,SampleAlias,chem.group), function(x) {length(unique(x[x$Detect.nondetect > 0,'Analyte']))})
by.group <- rename(by.group, c('V1' = 'count'))
by.group <- arrange(by.group, SampleRegID, chem.group, desc(count))
#write.xlsx(by.group,'//deqlead01/wqm/toxics_2012/data/r/Data_Summary_DRAFT.xlsx',sheetName='UniqueCompoundsByGroupByStation',row.names = FALSE,append = TRUE)

#compounds detected per chemical group without the new methods
by.group.only <- arrange(ddply(dwn.sub, .(chem.group), summarise, sum = sum(Detect.nondetect)),desc(sum))
by.group.only <- by.group.only[!is.na(by.group.only$chem.group) & by.group.only$chem.group != 'NA',]
#write.xlsx(by.group.only,'//deqlead01/wqm/toxics_2012/data/r/Data_Summary_DRAFT.xlsx',sheetName='UniqueCompoundsByGroup',row.names = FALSE,append = TRUE)

#if you inlcude all the data we have
by.group.only.all <- arrange(ddply(data.wo.void, .(chem.group), summarise, sum = sum(Detect.nondetect)),desc(sum))
by.group.only.all <- by.group.only.all[!is.na(by.group.only.all$chem.group) & by.group.only.all$chem.group != 'NA',]

#look for the compounds consistently detected across the state
#123 total stations sampled in this dataset as of 2014-01-06
detects.only <- dwn.sub[dwn.sub$Detect.nondetect == 1,]

by.analyte <- ddply(detects.only, .(Analyte), function (x) {length(unique(x$SampleRegID))})
by.analyte <- rename(by.analyte, c('V1' = 'CountofStations'))
by.analyte <- arrange(by.analyte, desc(CountofStations))

#to compare to land use we need to pull that file in
lu <- read.xlsx('//deqlead03/gis_wa/project_working_folders/toxics/land use maps/attila_toxics_wshds.xlsx', sheetName = 'attila clip')
data.w.lu <- merge(dwn.sub, lu, by.x = 'SampleRegID', by.y = 'NAME', all.x = TRUE)
data.w.lu <- rename(data.w.lu, c('X.50.' = 'DomLU'))
by.lu <- arrange(ddply(data.w.lu, .(DomLU), summarise, sum = sum(Detect.nondetect)),desc(sum))

#pull out just the metals data
metals <- data.wo.void[data.wo.void$SpecificMethod %in% c('EPA 200.8', '200.8'),]

#separates the parts of the name to make it easier to apply the hardness based criteria
name.split <- strsplit(metals$Analyte, split = ', ')

name.split <- data.frame(matrix(unlist(name.split), nrow=length(name.split), byrow=T))

name.split <- rename(name.split, c('X1' = 'Name.alone', 'X2' = 'PARAMETER_MODIFIER_ABBREVIATION'))

metals <- cbind(metals, name.split)

metals$ID <- paste(metals$SampleRegID, metals$Sampled, metals$PARAMETER_MODIFIER_ABBREVIATION)

#bring in just the metals criteria
metals.criteria <- read.xlsx('//deqlead01/wqm/TOXICS_2012/Data/Criteria_benchmarks_etc/Metals_Table30_Table40.xlsx', sheetName = 'Sheet1')

#change NAME to Analyte for consistency with Element
metals.criteria <- rename(metals.criteria, c('NAME' = 'Analyte'))

#match the metals with the criteria
metals.w.criteria <- merge(metals, metals.criteria, by = 'Analyte', all.x = TRUE)

#doing the hardness evaluation
hm.evaluated <- data.frame()
for (j in 1:length(unique(constants$Name.alone))) {
  tmp <- hardnessEval(metal = unique(constants$Name.alone)[j], df = metals)
  tmp <- as.data.frame(lapply(tmp, as.character), stringsAsFactors = FALSE)
  ifelse(tmp == "", hm.evaluated <- hm.evaluated, ifelse(j == 1, hm.evaluated <- tmp, hm.evaluated <- rbind(hm.evaluated, tmp)))
}

#merge the hardness criteria into the metals.w.criteria table
metals.w.criteria$ID <- paste(metals.w.criteria$ID, metals.w.criteria$Analyte)
hm.evaluated$ID <- paste(hm.evaluated$ID, hm.evaluated$Analyte)
metals.w.criteria.hm <- merge(metals.w.criteria, hm.evaluated[,c('ID','tResulthardness','Table30Acute','Table30Chronic')], by = 'ID', suffixes = c('OG','HM'), all.x = TRUE)

metals.w.criteria.hm$Table30Acute <- ifelse(metals.w.criteria.hm$Table30AcuteOG == 'hardness',
                                              metals.w.criteria.hm$Table30AcuteHM,
                                              metals.w.criteria.hm$Table30AcuteOG)

metals.w.criteria.hm$Table30Chronic <- ifelse(metals.w.criteria.hm$Table30ChronicOG == 'hardness',
                                              metals.w.criteria.hm$Table30ChronicHM,
                                              metals.w.criteria.hm$Table30ChronicOG)

metals.w.criteria.hm <- within(metals.w.criteria.hm, rm('Table30AcuteOG','Table30AcuteHM','Table30ChronicOG','Table30ChronicHM'))

#Now the rest
metals.w.criteria.hm$Table30Acute.Exceed <- ifelse(metals.w.criteria.hm$tResult > suppressWarnings(as.numeric(metals.w.criteria.hm$Table30Acute)), 1, 0)
metals.w.criteria.hm$Table30Acute.Magnitude <- metals.w.criteria.hm$tResult/suppressWarnings(as.numeric(metals.w.criteria.hm$Table30Acute))

metals.w.criteria.hm$Table30Chronic.Exceed <- ifelse(metals.w.criteria.hm$tResult > suppressWarnings(as.numeric(metals.w.criteria.hm$Table30Chronic)), 1, 0)
metals.w.criteria.hm$Table30Chronic.Magnitude <- metals.w.criteria.hm$tResult/suppressWarnings(as.numeric(metals.w.criteria.hm$Table30Chronic))

metals.w.criteria.hm$Table40Water.Organism.Exceed <- ifelse(metals.w.criteria.hm$tResult > suppressWarnings(as.numeric(metals.w.criteria.hm$Table40Water.Organism)), 1, 0)
metals.w.criteria.hm$Table40Water.Organism.Magnitude <- metals.w.criteria.hm$tResult/suppressWarnings(as.numeric(metals.w.criteria.hm$Table40Water.Organism))

metals.w.criteria.hm$Table40OrganismOnly.Exceed <- ifelse(metals.w.criteria.hm$tResult > suppressWarnings(as.numeric(metals.w.criteria.hm$Table40OrganismOnly)), 1, 0)
metals.w.criteria.hm$Table40OrganismOnly.Magnitude <- metals.w.criteria.hm$tResult/suppressWarnings(as.numeric(metals.w.criteria.hm$Table40OrganismOnly))

metals.w.criteria.hm.sub <- metals.w.criteria.hm[,c('Project','SampleRegID','SampleAlias','Sampled','Name.alone','PARAMETER_MODIFIER_ABBREVIATION','Analyte','tResult','Units',
                         'tResulthardness','tMRL','Detect.nondetect','Table30Acute','Table30Acute.Exceed','Table30Acute.Magnitude',
                         'Table30Chronic','Table30Chronic.Exceed','Table30Chronic.Magnitude','Table40Water.Organism','Table40Water.Organism.Exceed',
                         'Table40Water.Organism.Magnitude','Table40OrganismOnly','Table40OrganismOnly.Exceed','Table40OrganismOnly.Magnitude')]

intermediate <- melt(metals.w.criteria.hm, id.vars = c('ID','Project','SampleRegID','SampleAlias','Sampled','Name.alone',
                                                   'PARAMETER_MODIFIER_ABBREVIATION','Analyte','tResult','Units','tResulthardness','tMRL',
                                                   'Detect.nondetect'), 
             measure.vars = c('Table30Acute.Exceed','Table30Chronic.Exceed','Table40Water.Organism.Exceed','Table40OrganismOnly.Exceed'))

roll.up <- ddply(metals.w.criteria.hm.sub, .(Project,SampleRegID,SampleAlias,Name.alone,
                                 PARAMETER_MODIFIER_ABBREVIATION,Analyte), summarise, 
                 Table30Acute.Exceed = sum(Table30Acute.Exceed), 
                 Table30Chronic.Exceed = sum(Table30Chronic.Exceed), 
                 Table40Water.Organism.Exceed = sum(Table40Water.Organism.Exceed),
                 Table40OrganismOnly.Exceed = sum(Table40OrganismOnly.Exceed),
                 sample.count = length(Detect.nondetect), 
                 detect.count = sum(Detect.nondetect, na.rm = TRUE))
roll.up$percent.detect <- round(100*(roll.up$detect.count/roll.up$sample.count))

#metals.exceed.summary <- (dcast(intermediate, formula = Project + SampleRegID + SampleAlias + Analyte ~ variable, fun.aggregate = sum) )
#metals.exceed.summary.n <- (dcast(intermediate, formula = Project + SampleRegID + SampleAlias + Analyte ~ variable, fun.aggregate = length) )
gc()
write.xlsx2(metals.w.criteria.hm.sub, '//deqlead01/wqm/toxics_2012/data/analysis_through_2012/MetalsAnalysisThrough2012_10252013.xlsx',
           sheetName='Data')
gc()
write.xlsx2(roll.up, '//deqlead01/wqm/toxics_2012/data/analysis_through_2012/MetalsAnalysisThrough2012_10252013.xlsx',
           sheetName='ExceedanceSummary',append = TRUE)


#View(arrange(hm.evaluated[,c('Project' ,'Sampled', 'SampleRegID', 'SampleAlias','Analyte', 'Name.full', 'tResultmetal', 'tResulthardness','crit.acute', 'crit.chronic', 'exceed.acute', 'exceed.chronic')], Project))
#hm.evaluated$exceed.acute <- as.numeric(hm.evaluated$exceed.acute)
#hm.evaluated$exceed.chronic <- as.numeric(hm.evaluated$exceed.chronic)
#hm.evaluated$Detect.nondetect <- as.numeric(hm.evaluated$Detect.nondetect)

#for (ii in 1:nrow(hm.evaluated)) {
#  if (is.na(hm.evaluated$crit.acute)[ii]) {
#    hm.evaluated$variable[ii] <- 'Table 20 Toxic Substances - Freshwater Chronic'
#    hm.evaluated$value[ii] <- hm.evaluated$crit.chronic[ii]
#    hm.evaluated$exceed[ii] <- hm.evaluated$exceed.chronic[ii]
#  } else {
#    hm.evaluated$variable[ii] <- 'Table 20 Toxic Substances - Freshwater Acute'
#    hm.evaluated$value[ii] <- hm.evaluated$crit.acute[ii]
#    hm.evaluated$exceed[ii] <- hm.evaluated$exceed.acute[ii]
#  }
#}

#before we can bring in the criteria we need to match names
#unique(data.wo.void$Analyte)[!unique(data.wo.void$Analyte) %in% min.criteria.values$Pollutant]

#these names were referenced and compiled into this file
name.match <- read.csv('//deqlead01/wqm/TOXICS_2012/Data/Criteria_benchmarks_etc/pollutant_corrections.csv', na.strings = c('NA', ''))

name.match.sub <- name.match[!is.na(name.match$criteria_name),]

data.wo.void$Analyte <- as.factor(data.wo.void$Analyte)

data.wo.void$Analyte <- mapvalues(data.wo.void$Analyte, from = name.match.sub$name_in_data, to = name.match.sub$criteria_name)

data.wo.void$Analyte <- as.character(data.wo.void$Analyte)

#brings in the criteria
data.wo.void <- merge(data.wo.void, min.criteria.values, by.x = 'Analyte', by.y = 'Pollutant', all.x = TRUE)

#need to do unit conversion/mapping
data.wo.void[data.wo.void$Units == 'ng/L','tResult2'] <- data.wo.void[data.wo.void$Units == 'ng/L','tResult'] * 1000
data.wo.void[data.wo.void$Units == 'ng/L','Units'] <-  "µg/L"

#marks records that exceed the criteria or benchmark
data.wo.void$exceed <- ifelse(data.wo.void$tResult >= data.wo.void$value, 1, 0)

#magnitude
data.wo.void$magnitude <- data.wo.void$tResult/data.wo.void$value

#makes the ID column to be used in the hardness evaluation
data.wo.void$ID <- paste(data.wo.void$SampleRegID, data.wo.void$Sampled)




non.h.metals <- metals[!metals$NAME %in% constants$NAME,]

non.h.metals <- non.h.metals[!non.h.metals$NAME %in% c("Hardness as CaCO3, Dissolved", "Hardness as CaCO3, Total recoverable"),]

criteria.values.melted.applicable[criteria.values.melted.applicable$Pollutant == 'Chromium (Hex)','Pollutant'] <- 'Chromium' 

non.h.metals.criteria <- criteria.values.melted.applicable[criteria.values.melted.applicable$Pollutant %in% non.h.metals$Name.alone,]

dcast(criteria.values.melted.applicable, formula = Pollutant ~ variable)

#right now this just rolls up the hardness metals -- need to make it work for all toxics
roll.up <- ddply(hm.evaluated, .(Project, SampleRegID, NAME), summarise, acute.exceed = sum(exceed.acute, na.rm = TRUE), 
                 chronic.exceed = sum(exceed.chronic, na.rm = TRUE), sample.count = length(exceed.acute), detect.count = sum(Detect.nondetect, na.rm = TRUE))
roll.up$percent.detect <- round(100*(roll.up$detect.count/roll.up$sample.count))

View(data.wo.void[which(data.wo.void$exceed == 1),])



# Select dbo.REPWRK.Client, dbo.REPWRK.Project, dbo.REPWRK.Wrk,
# dbo.REPSAMPLE.Sample, dbo.REPSAMPLE.SampleAlias, dbo.REPSAMPLE.SampleRegID,
# dbo.REPSAMPLE.SampleType, dbo.REPSAMPLE.ClientMatrix, dbo.REPSAMPLE.Sampled,
# dbo.REPSAMPLEANALYSIS.Analysis, dbo.REPSAMPLEANALYSIS.SpecificMethod,
# dbo.REPSAMPLEANALYTE.Analyte, dbo.REPSAMPLEANALYTE.tMRL,
# dbo.REPSAMPLEANALYTE.tResult, dbo.REPSAMPLEANALYTE.FinalUnits As 'Unit',
# dbo.REPSAMPLEANALYTE.AnalyteNotes as 'AnalyteQualifiers', dbo.REPSAMPLEANALYSIS.SampleNotes as 'SampleQualifiers'
# From dbo.REPWRK Inner Join
# dbo.REPSAMPLE On dbo.REPWRK.Wrk = dbo.REPSAMPLE.Wrk Inner Join
# dbo.REPSAMPLEANALYSIS On dbo.REPSAMPLEANALYSIS.Sample = dbo.REPSAMPLE.Sample
# And dbo.REPSAMPLE.Wrk = dbo.REPSAMPLEANALYSIS.Wrk Inner Join
# dbo.REPSAMPLEANALYTE On dbo.REPSAMPLEANALYSIS.Analysis =
#   dbo.REPSAMPLEANALYTE.Analysis And dbo.REPSAMPLEANALYSIS.Sample =
#   dbo.REPSAMPLEANALYTE.Sample And dbo.REPSAMPLEANALYSIS.Wrk =
#   dbo.REPSAMPLEANALYTE.Wrk
# Where dbo.REPWRK.Wrk In (1212093, 1212094, 1212092, 1212088, 1212105, 1212103,
#                          1212104, 1212106, 1212005, 1212010,
#                          1212009, 1212007, 1212102, 1212099,
#                          1212101, 1212100, 1212108, 1212107,
#                          1212083, 1212089, 1212081, 1212082,
#                          1212002, 1212001, 1211023, 1211024, 1211026)And
#dbo.REPSAMPLEANALYTE.SUR = 'FALSE' And
#dbo.REPSAMPLEANALYTE.Analyte NOT LIKE '%2C%'
# Order By dbo.REPWRK.Wrk, dbo.REPSAMPLE.Sample, dbo.REPSAMPLEANALYTE.Analyte
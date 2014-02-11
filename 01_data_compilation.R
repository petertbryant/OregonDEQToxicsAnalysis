#This file pulls together three files. One with data from Element. One with toxics data from LASAR and the last with metals data in the 
#Willamette basin only. The intention is that this file will always be run first and the R session and workspace from this file will be
#used to feed into the subsequent uses of the data.wo.void composite data frame. The reason for this is to prevent the need to output a file and 
#import it again for the next script to run.

#First import the needed libraries
library(plyr)
library(stringr)
library(reshape2)
library(xlsx)
library(ggplot2)

#This prevents scientific notation from being used and forces all imported fields to be character or numeric
options('scipen' = 50, stringsAsFactors = FALSE)

#Pull in the Element data that is final as of 1/3/2014. This data was queried using the SQL Query tool in Element. The text of that query
#is commented at the end of this file for future reference. NOTE: there may be an updated method for acquiring this data soon 2/10/14
data.2012 <- read.csv('//deqlead01/wqm/TOXICS_2012/Data/Element_Final_Data_Qry_on_01062014.csv', stringsAsFactors = FALSE)

#The qualifiers from this query were concatenated into a single field which is kind of a pain to parse so in order to get Status
#I queried the qualifier table directly and will make a status column that can then be merged with the data table.
#NOTE: the updated query method should provide a dql column already populated
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

#All voided samples should be DQL 'D'
data.2012.w.qualifiers$Status <- ifelse(data.2012.w.qualifiers$tResult %in% c('Void', 'Cancelled'),
                                        'D',
                                        data.2012.w.qualifiers$Status)

#this simplifies the qualifier and status columns and writes it back to the dataframe name that is used from here on
data.2012 <- within(data.2012.w.qualifiers, rm('sid','id','AnalyteStatus','SampleStatus'))

#There is a site in the Deschutes basin that was sampled during a John Day basin sampling event and was associated with
#the John Day Project. This puts it in the right Project for consistency.
data.2012[data.2012$SampleRegID == 10411,'Project'] <- 'Deschutes'

#for this analysis we can leave out blanks and field duplicates (we may want to check the duplicates later and use a detect
#when we don't have a primary detection)
data.2012 <- data.2012[!data.2012$SampleType %in% c('Blank - Equipment::EB', 'Blank - Transfer::TfB', 'Field Duplicate::FD'),]

#we are merging element data with lasar data which means we have to only have the columns that are consistent
#between the systems. This removes all the element columns that don't have a match in lasar.
data.2012 <- data.2012[,c('Project', 'SampleRegID', 'SampleAlias','Sampled', 'SampleType','Analyte','tResult', 'tMRL', 'Unit', 'SpecificMethod', 'Status')]
                     
#both of these data sets are from the LASAR database
data.2011 <- read.csv('//deqlead01/wqm/TOXICS_2012/Data/2011allBasins.csv', stringsAsFactors = FALSE)
willy.data <- read.csv('//deqlead01/wqm/TOXICS_2012/Data/Willamette water data.csv', stringsAsFactors = FALSE)

#First let's make the columns consistent between our two LASAR datasets
data.2011 <- data.2011[,names(willy.data)]

#This brings the LASAR data into a single dataframe
lasar <- rbind(data.2011, willy.data)

#This makes the lasar data for metals names equal to the metals names as they are in Element
lasar[lasar$METHOD_CODE == '200.8','NAME'] <- gsub('R', 'r', (paste(lasar[lasar$METHOD_CODE == '200.8','NAME'], 
                                                                    lasar[lasar$METHOD_CODE == '200.8','PARAMETER_MODIFIER_ABBREVIATION'], 
                                                                    sep = ', ')))

#This makes the LASAR names consistent with Element so we can put them all together into a single dataframe
lasar <- rename(lasar, c('SUBPROJECT_NAME' = 'Project', 'STATION' = 'SampleRegID', 'DESCRIPTION' = 'SampleAlias','SAMPLE_DATE' = 'Sampled', 
                         'QA_QC_TYPE' = 'SampleType','NAME' = 'Analyte', 'PRV' = 'tResult', 'METHOD_REPORTING_LIMIT' = 'tMRL', 'UNIT' = 'Unit', 
                         'METHOD_CODE' = 'SpecificMethod', 'STATUS' = 'Status'))

#this makes the columns in the LASAR data match the columns in the Element data frame
lasar <- lasar[,names(data.2012)]

#this file is to reconcile the names between lasar and element
lasar.to.element <- read.xlsx('//deqlead01/wqm/TOXICS_2012/Data/lasar_to_element.xlsx', sheetName = 'Lasar to Element All')

#There are issues with character encoding between the different software we are using. 
#This should take care of that for the DDT compounds
lasar.to.element[lasar.to.element$Element.Analyte == '4,4´-DDD','Element.Analyte'] <- '4,4`-DDD'
lasar.to.element[lasar.to.element$Element.Analyte == '4,4´-DDE','Element.Analyte'] <- '4,4`-DDE'
lasar.to.element[lasar.to.element$Element.Analyte == '4,4´-DDT','Element.Analyte'] <- '4,4`-DDT'

#This isolates the list to just the compounds we need to change
lasar.change <- lasar.to.element[!is.na(lasar.to.element$Lasar.Name.full),]

#This removes blank columns from the change list
lasar.change <- lasar.change[lasar.change$Lasar.Name.full != '',]

#the name reconciliation occurs in the next four lines of code
lasar.change.vector <- lasar.change$Element.Analyte
names(lasar.change.vector) <- lasar.change$Lasar.Name.full

lasar$Analyte <- as.factor(lasar$Analyte)

lasar$Analyte <- revalue(lasar$Analyte, lasar.change.vector)

#The Willamette metals dataframe is from a LASARWeb query which outputs in wide format so we have to
#fix the names and melt the dataframe and add the necessary columns to make it consistent with the other datasets
willy.metals <- read.xlsx('//deqlead01/wqm/TOXICS_2012/Data/willamette metals_KG.xlsx', sheetName = 'Sheet2')
willy.metals <- rename(willy.metals, c("Sampling.Event.Number" = 'SAMPLING_EVENT_KEY', "Station.Identifier" = 'SampleRegID', 
                                       "Station.Description" = 'SampleAlias', "Sample.Date.Time" = 'Sampled',
                                       "QA.QC.Type" = 'QA_QC_TYPE', 
                                       "Field.Conductivity...Âµmhos.cm...25Â..C." = 'Conductivity',
                                       "Field.pH...SU." = 'pH', 
                                       "Field.Turbidity...NTU." = 'Turbidity', 
                                       "TR.Antimony.Result" = 'Antimony, Total recoverable', 
                                       "Total.Recoverable.Arsenic...Âµg.L." = 'Arsenic, Total recoverable',
                                       "Total.Recoverable.Barium...Âµg.L." = 'Barium, Total recoverable',
                                       "Total.Recoverable.Beryllium...Âµg.L." = 'Beryllium, Total recoverable',
                                       "Total.Recoverable.Cadmium...Âµg.L." = 'Cadmium, Total recoverable',
                                       "Total.Recoverable.Calcium...mg.L." = 'Calcium, Total recoverable',
                                       "Total.Recoverable.Chromium...Âµg.L." = 'Chromium, Total recoverable',
                                       "Total.Recoverable.Cobalt...Âµg.L." = 'Cobalt, Total recoverable',
                                       "Total.Recoverable.Copper...Âµg.L." = 'Copper, Total recoverable',
                                       "Total.Recoverable.Iron...Âµg.L." = 'Iron, Total recoverable',
                                       "Total.Recoverable.Lead...Âµg.L." = 'Lead, Total recoverable',
                                       "Total.Recoverable.Magnesium...mg.L." = 'Magnesium, Total recoverable',
                                       "Total.Recoverable.Molybdenum...Âµg.L." = 'Molybdenum, Total recoverable',
                                       "Total.Recoverable.Nickel...Âµg.L." = 'Nickel, Total recoverable',
                                       "Total.Recoverable.Selenium...Âµg.L." = 'Selenium, Total recoverable',
                                       "Total.Recoverable.Silver...Âµg.L." = 'Silver, Total recoverable',
                                       "Total.Recoverable.Thallium...Âµg.L." = 'Thallium, Total recoverable',
                                       "Total.Recoverable.Uranium...Âµg.L." = 'Uranium, Total recoverable',
                                       "Total.Recoverable.Vanadium...Âµg.L." = 'Vanadium, Total recoverable',
                                       "Total.Recoverable.Zinc...Âµg.L." = 'Zinc, Total recoverable',
                                       "Total.Suspended.Solids...mg.L." = 'Total Suspended Solids',
                                       'CaCO3..mg.L.' = 'Hardness as CaCO3, Total recoverable'))
willy.metals <- within(willy.metals, rm('Sample.ID','pH...SU.','Conductivity','pH','Turbidity',
                                        'Total Suspended Solids','QA_QC_TYPE','SAMPLING_EVENT_KEY'))
willy.metals.melted <- melt(willy.metals, id.vars = c('SampleRegID','SampleAlias','Sampled'),variable.name = 'Analyte',value.name='tResult')
#willy.metals.melted$Sampled <- as.POSIXct(as.numeric(willy.metals.melted$Sampled)*24*3600 + as.POSIXct("1899-12-30 00:00") )
willy.metals.melted$Sampled <- strftime(willy.metals.melted$Sampled, format = '%d-%b-%y')
willy.metals.melted$SampleRegID <- substr(willy.metals.melted$SampleRegID, 1, 5)
willy.metals.melted$SpecificMethod <- '200.8'
willy.metals.melted$Project <- 'TMP-Water-Willamette'
willy.metals.melted$tMRL<- NA
willy.metals.melted$Unit <- willy.metals.melted$Analyte
willy.metals.melted$Unit <- mapvalues(willy.metals.melted$Unit, 
                                      from = unique(willy.metals.melted$Analyte), 
                                      to = c('µg/L','µg/L','µg/L','µg/L','µg/L','mg/L','µg/L','µg/L','µg/L','µg/L',
                                             'µg/L','mg/L','µg/L','µg/L','µg/L','µg/L','µg/L','µg/L','µg/L','µg/L','mg/L'))
willy.metals.melted$tResult <- ifelse(substr(willy.metals.melted$tResult,1,1) == '<','ND',willy.metals.melted$tResult)
willy.metals.melted$Status <- 'A'
willy.metals.melted$SampleType <- ''

#this puts all of the LASAR and Element data together
data <- rbind(data.2012, lasar, willy.metals.melted)

#There are several stations that have double spaces in their names and several stations that double spaces for some records
#and not others. This removes those double spaces for consistent naming.
data$SampleAlias <- gsub("  "," ",data$SampleAlias)

#This pulls out empty rows
data <- data[!is.na(data$SampleRegID),]

#we also want to add in the Focus List Categories
#This brings those categories in and makes the dataframe just have the Analyte and category columns
categories <- read.xlsx2('//deqlead01/wqm/toxics_2012/data/Focus list and 737 categories.xlsx', sheetName = 'Sheet1')
categories.sub <- categories[,c('Chemical','chem.group')]

#This pulls matched names and mapped names from the lasar.to.element naming dataframe to make sure all the
#chemical names in the dataset have a match in the category dataframe
test <- merge(data, categories.sub, by.x = 'Analyte', by.y = 'Chemical')
sub <- test[!duplicated(test$Analyte),c('Analyte','Analyte')]
sub <- rename(sub, c('Analyte.1' = 'categories.Chemical'))
sub2 <- lasar.to.element[!is.na(lasar.to.element$categories.Chemical),c('Element.Analyte','categories.Chemical')]
sub2 <- rename(sub2, c('Element.Analyte' = 'Analyte'))
categories.mapped <- rbind(sub, sub2)

#I create a named vector for the name mapping
categories.mapped.vector <- categories.mapped$Analyte
names(categories.mapped.vector) <- categories.mapped$categories.Chemical

#Then we do the rename by first converting to factor and renaming with the revalue function
categories$Chemical <- as.factor(categories$Chemical)
categories$Chemical <- revalue(categories$Chemical, categories.mapped.vector)
categories$Chemical <- as.character(categories$Chemical)

#Now that the category names are consistent with Element names we can prepare to merge them
#by narrowing the dataframe and removing duplicates
categories.sub <- categories[,c('Chemical','chem.group')]
categories.sub <- categories.sub[!duplicated(categories.sub$Chemical),]

#This adds the category column for those analytes that have a match either by
#exact name or through the lasar.to.element renaming
data.w.categories <- merge(data, categories.sub, by.x = 'Analyte', by.y = 'Chemical', all.x = TRUE)

#Character mapping has been a problem for the DDT compounds and they showed up in the unmatched analytes
#also Inorganic arsenic is giving us trouble. They should be all that is in this list since
#this captures those that didn't have a matched from the categories mapped. There are still character NAs in the
#chem.group column for where we don't have the analyte mapped to a category yet.
unmatched.analytes <- unique(data.w.categories[is.na(data.w.categories$chem.group),'Analyte'])

#Based on the output of the unmatched anlaytes we can correct a few of those here
data.w.categories[data.w.categories$Analyte %in% c('4,4´-DDD','4,4´-DDE','4,4´-DDT'),'chem.group'] <- 'Legacy Pesticides'
data.w.categories[data.w.categories$Analyte == 'Inorganic Arsenic, Total','chem.group'] <- 'Metals'

#### Output combined data table ####
#creates a column that will be used just for numeric representation. this allows an output table to include all the voided and cancelled data
data.w.categories$Result <- data.w.categories$tResult

#this sets the value for all the NDs to 0 and creates a column that can be all numeric
data.w.categories[data.w.categories$tResult %in% c('VOID','Void','Cancelled','',NA,'ND','ND*'),'Result'] <- 0

#This converts the MRL to a numeric field
data.w.categories$tMRL<- as.numeric(data.w.categories$tMRL)

#this converts the Result to a numeric field
data.w.categories$Result <- as.numeric(data.w.categories$Result)

#Apparently the reporting limit for DEET has been raised to 30 per Lori
data.w.categories[data.w.categories$Analyte == 'DEET','tMRL'] <- 30

#populate the detect.nondetect column
data.w.categories[!is.na(data.w.categories$tMRL),'Detect.nondetect'] <- ifelse(data.w.categories[!is.na(data.w.categories$tMRL),'Result'] 
                                                                               < data.w.categories[!is.na(data.w.categories$tMRL),'tMRL'],
                                                                               0, 1)
data.w.categories[is.na(data.w.categories$tMRL),'Detect.nondetect'] <- ifelse(data.w.categories[is.na(data.w.categories$tMRL),'Result'] == 0,
                                                                              0, 1) 

#When you are ready to output the data file uncomment the next line and run it. If you have included
#data from 2013 then you will want to change the file name too.
#write.xlsx(data.w.categories, '//deqlead01/wqm/TOXICS_2012/Data/All_TMP_Water_Data_through_2012.xlsx', sheetName = 'AllDataThrough2012')

#### Continue with data analysis ####
#this eliminates VOIDED samples
data.wo.void <- data.w.categories[!data.w.categories$tResult %in% c('VOID','Void','Cancelled','',NA),]

#We also only want to include A and B data 
data.wo.void <- data.wo.void[data.wo.void$Status %in% c('A','A+','B'),]

#We use the tResult field as numeric in subsequent uses of this dataframe so this populates that
data.wo.void$tResult <- data.wo.void$Result

#remove the separate Result column to avoid confusion
data.wo.void <- within(data.wo.void, rm('Result'))

rm(list = setdiff(ls(), c('data.wo.void','categories.sub')))


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
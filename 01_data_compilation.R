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

#######################################################################################################

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

######################################################################################

#There is a site in the Deschutes basin that was sampled during a John Day basin sampling event and was associated with
#the John Day Project. This puts it in the right Project for consistency.
data.2012[data.2012$SampleRegID == 10411,'Project'] <- 'Deschutes'

#for this analysis we can leave out blanks and field duplicates (we may want to check the duplicates later and use a detect
#when we don't have a primary detection)
data.2012 <- data.2012[!data.2012$SampleType %in% c('Blank - Equipment::EB', 'Blank - Transfer::TfB', 'Field Duplicate::FD'),]

#we are merging element data with lasar data which means we have to only have the columns that are consistent
#between the systems. This removes all the element columns that don't have a match in lasar.
data.2012 <- data.2012[,c('Project', 'SampleRegID', 'SampleAlias','Sampled', 'SampleType','Analyte','tResult', 'tMRL', 'Unit', 'SpecificMethod', 'Status')]

#As of 2/14/2014 there are issues with Arsenic naming in the element dataset. Hoepfully this will not be an issue in the new Element data.
#This is written to only handle it if it is an issue. Otherwise this if block will be skipped.
if (length(unique(data.2012[grep('rsenic',data.2012$Analyte),'Analyte'])) > 0) {
  data.2012[data.2012$Analyte %in% c("Arsenic, total inorganic", "Inorganic Arsenic, Total" ),'Analyte'] <- "Arsenic, Total inorganic"
}

#This is the compilation of the Lasar data from the Willamette, Lasar 2011 and Willamette metals data files
lasar <- read.csv('//deqlead01/wqm/TOXICS_2012/Data/TMP-Lasar-in-Element-Format-02142014.csv', stringsAsFactors = FALSE)

#this puts all of the LASAR and Element data together
data <- rbind(data.2012, lasar)

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
lasar.to.element <- read.xlsx('//deqlead01/wqm/toxics_2012/data/lasar_to_element.xlsx',sheetName = 'Lasar to Element All')
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
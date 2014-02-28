#This file pulls together data from two sources. One with data from Element. One with toxics data from LASAR. The intention is that this file 
#will always be run first and the R session and workspace from this file will be
#used to feed into the subsequent uses of the data.wo.void composite data frame. The reason for this is to prevent the need to output a file and 
#import it again for the next script to run.

#First import the needed libraries
library(plyr)
library(stringr)
library(reshape2)
library(xlsx)
library(ggplot2)
library(RODBC)

#This prevents scientific notation from being used and forces all imported fields to be character or numeric
options('scipen' = 50, stringsAsFactors = FALSE)

#Pull in the Element data that is final as of 1/3/2014. This data was queried using the SQL Query tool in Element. The text of that query
#is commented at the end of this file for future reference. NOTE: there may be an updated method for acquiring this data soon 2/10/14
#element <- read.csv('//deqlead01/wqm/TOXICS_2012/Data/Element_Final_Data_Qry_on_01062014.csv', stringsAsFactors = FALSE)

#Pull in Element data from the Repository directly from R.
con <- odbcConnect('Elm-Repository')
#You can query all of the TMP water data
element <- sqlQuery(con, as.is = TRUE, "SELECT * FROM Repo_Result
                                        WHERE Repo_Result.Client = 'Toxics Monitoring Program - Water' And
                                              Repo_Result.Project != 'Bottle Test'")
odbcCloseAll()
#or you can query by specific work order. Right now this query should be the same as the one above.
#element <- sqlQuery(con, as.is = TRUE, 'SELECT * 
#                                        FROM Repo_Result 
#                                        WHERE Repo_Result.Work_Order In (1212093, 1212094, 1212092, 1212088, 1212105, 1212103,
#                                                                         1212104, 1212106, 1212005, 1212010, 1212022, 1212067,
#                                                                         1212009, 1212007, 1212102, 1212099, 1212068, 1212071,
#                                                                         1212101, 1212100, 1212108, 1212107, 1212109, 1212110,
#                                                                         1212083, 1212089, 1212081, 1212082, 1211025,
#                                                                         1212002, 1212001, 1211023, 1211024, 1211026, 1304007,
#                                                                         1304008, 1309030, 1309031, 1311053, 1311054, 1309093,
#                                                                         1305019, 1305020, 1308050, 1308054, 1312002, 1312003, 
#                                                                         1309090, 1309092, 1309089, 1304071, 1304072, 1304073, 
#                                                                         1308011, 1308013, 1310083, 1310085)')
element <- rename(element, c('Station_ID' = 'SampleRegID','Station_Description' = 'SampleAlias', 'Result' = 'tResult', 'MRL' = 'tMRL', 
                             'Units' = 'Unit'))

#######################################################################################################
#The qualifiers from this query were concatenated into a single field which is kind of a pain to parse so in order to get Status
#we query the qualifier table directly and make a status column that can then be merged with the data table.

#NOTE: As of 2/19/2014 the timeline for getting the DQL column finalized in this output is unknown. Until that time, in order to get the
#qualifiers the section below can be used to populate them. This process relies on running a query in Element first using the menu
#Database Admin -> SQL Query... Then under Query name select Qualifiers_by_WorkOrder. Unless somebody has edited this, the work orders
#from the query above should be in there and all you have to do is click the Query button. Once the query has run you need to export this
#using the Export button on the right just above the result window. Name it whatever you like, you can see below what I called a previous version.
#Then you should open it in excel and save it as a .csv. Wherever you put it make sure to change the name of the file being read in on line 45

element.qualifiers <- read.csv('//deqlead01/wqm/TOXICS_2012/Data/Element_Qualifier_Query_for_TMP_on_01032014.csv', stringsAsFactors = FALSE)

element.qualifiers$id <- paste(element.qualifiers$Wrk, element.qualifiers$Sample, element.qualifiers$Analysis, element.qualifiers$Analyte)

element.qualifiers$Status <- substr(element.qualifiers$Qualifier, nchar(element.qualifiers$Qualifier),nchar(element.qualifiers$Qualifier))

element.qualifiers$Status <- as.factor(element.qualifiers$Status)

element.qualifiers$Status <- revalue(element.qualifiers$Status, c('a' = 1, 'A' = 1, 'b' = 2, 'B' = 2, 'c' = 3, 'C' = 3, 'D' = 4, 'Q' = 1))

element.qualifiers$Status <- as.numeric(element.qualifiers$Status)

processed <- ddply(element.qualifiers, .(id), summarise, Status = max(Status))

processed$Status <- as.factor(processed$Status)

processed$Status <- revalue(processed$Status, c('1' = 'A', '2' = 'B', '3' = 'C', '4' = 'D'))

processed$AnalyteStatus <- as.character(processed$Status)

processed <- processed[,c('id','AnalyteStatus')]

#now that the qualifiers have been parsed and the DQL determined we can associate them with the data itself
element$id <- paste(element$Wrk, element$Sample, element$Analysis, element$Analyte)
element.w.qualifiers <- merge(element, processed, by = 'id', all.x = TRUE)

#the above only captures the analyte level qualifiers. the below will also capture the sample level qualifiers
sample.qualifiers <- element.qualifiers[element.qualifiers$Analyte == '',]

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

element.w.qualifiers$sid <- paste(element.w.qualifiers$Wrk, element.w.qualifiers$Sample, element.w.qualifiers$Analysis)
element.w.qualifiers <- merge(element.w.qualifiers, processed, by = 'sid', all.x = TRUE)

#This brings the status into a single column
element.w.qualifiers$Status <- ifelse(is.na(element.w.qualifiers$AnalyteStatus),
                                        ifelse(is.na(element.w.qualifiers$SampleStatus),
                                               'A',
                                               element.w.qualifiers$SampleStatus),
                                        element.w.qualifiers$AnalyteStatus)

#All voided samples should be DQL 'D'
element.w.qualifiers$Status <- ifelse(element.w.qualifiers$tResult %in% c('Void', 'Cancelled'),
                                        'D',
                                        element.w.qualifiers$Status)

#this simplifies the qualifier and status columns and writes it back to the dataframe name that is used from here on
element <- within(element.w.qualifiers, rm('sid','id','AnalyteStatus','SampleStatus'))

######################################################################################

#There is a site in the Deschutes basin that was sampled during a John Day basin sampling event and was associated with
#the John Day Project. This puts it in the right Project for consistency.
element[element$SampleRegID == 10411,'Project'] <- 'Deschutes'

#for this analysis we can leave out blanks and field duplicates (we may want to check the duplicates later and use a detect
#when we don't have a primary detection)
element <- element[!element$SampleType %in% c('Blank - Equipment::EB', 'Blank - Transfer::TfB', 'Field Duplicate::FD'),]

#We want matrix brought in but as of 2/21/2014 the matrix coming from the repository is based on what it was assigned at login
#NOT what it was assigned when Lori updated the Sites/Analysis information for Report Matrix. So what this means is that I found
#the table with the information Lori updated so we will use that to trump any matrix assignments that have been completed up to this point.
#To recreate the source table here run the TMP-Station-Matrix query in the SQLQuery menu within Element (It pulls all stations with a Client
#of Toxics Monitoring Program - Water).
matrix <- read.csv('//deqlead01/wqm/toxics_2012/sampling_site_information/TMP-SamplingSites-fromElement-02212014.csv')
matrix <- matrix[,c('SampleRegID','ClientMatrix')]
matrix <- rename(matrix, c('ClientMatrix' = 'Matrix'))
element <- within(element, rm('Matrix'))
element <- merge(element, matrix, by = 'SampleRegID', all.x = TRUE)

#we are merging element data with lasar data which means we have to only have the columns that are consistent
#between the systems. This removes all the element columns that don't have a match in lasar.
element <- element[,c('Project', 'SampleRegID', 'SampleAlias','Sampled', 'SampleType','Matrix','Analyte','tResult', 'tMRL', 'Unit', 'SpecificMethod', 'Status')]

#As of 2/14/2014 there are issues with Arsenic naming in the element dataset. Hoepfully this will not be an issue in the new Element data.
#This is written to only handle it if it is an issue. Otherwise this if block will be skipped.
if (length(unique(element[grep('rsenic',element$Analyte),'Analyte'])) > 0) {
  element[element$Analyte %in% c("Arsenic, total inorganic", "Inorganic Arsenic, Total" ),'Analyte'] <- "Arsenic, Total inorganic"
}

#This is the compilation of the Lasar data from the Willamette, Lasar 2011 and Willamette metals data files
lasar <- read.csv('//deqlead01/wqm/TOXICS_2012/Data/TMP-Lasar-in-Element-Format-02142014.csv', stringsAsFactors = FALSE)
lasar <- lasar[lasar$Project != '',]
lasar$Matrix <- 'River/Stream'

#this puts all of the LASAR and Element data together
data <- rbind(element, lasar)

#Resolving differences in naming of the same methods
data$SpecificMethod <- as.factor(data$SpecificMethod)
data$SpecificMethod <- revalue(data$SpecificMethod, c('8270 D' = 'EPA 8270D', '170.1' = 'EPA 170.1', '1698' = 'EPA 1698', '200.8' = 'EPA 200.8',
                                                      '2130 B' = 'SM 2130 B', '2510 B' = 'SM 2510 B', '2540 B' = 'SM 2540 D', '4500-H B' = 'SM 4500-H+ B',
                                                      '4500-O G' = 'SM 4500-O G', '5310 B' = 'SM 5310 B', '6640B' = 'SM 6640', '2540 D' = 'SM 2540 D',
                                                      '300' = 'EPA 300.0', '9060 A' = 'EPA 9060A', '9060' = 'EPA 9060A', '8270 C' = 'EPA 8270D'))
data$SpecificMethod <- as.character(data$SpecificMethod)

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

#The lab checks (i think?) are currently being reported in the repository and in lasar. Either way we have duplicates
#for conductivity, pH and Turbidity. This removes those duplicates where they exist.
#No preference is being made at this point for one result over the other. They use the same method in a lot of cases and I didn't see a consistent
#or significant enough difference to bother making the selection more specific since I can't tell which one is the
#field sample at this point.
to.pare <- data.wo.void[data.wo.void$Analyte %in% c('Conductivity','pH','Turbidity'),]

data.wo.cpt <- data.wo.void[!data.wo.void$Analyte %in% c('Conductivity','pH','Turbidity'),]

to.pare$code <- paste(to.pare$SampleRegID, to.pare$Sampled, to.pare$Analyte, to.pare$Unit)

to.include <- to.pare[!duplicated(to.pare$code),]

to.include <- within(to.include, rm(code))

data.wo.void <- rbind(data.wo.cpt, to.include)

#Now that we have only one method per sample, let's rename them so they group together in future analyses. 
data.wo.void[data.wo.void$Analyte == 'Conductivity','SpecificMethod'] <- 'SM 2510 B'
data.wo.void[data.wo.void$Analyte == 'pH','SpecificMethod'] <- 'SM 4500-H+ B'
data.wo.void[data.wo.void$Analyte == 'Turbidity','SpecificMethod'] <- 'SM 2130 B'

#These methods are essentially the same so let's go with our more up to date naming
data.wo.void[data.wo.void$SpecificMethod == '8321','SpecificMethod'] <- "DEQ 11-LAB-0031-SOP" 

#This cleans up your workspace keeping only the dataframes necessary to move forward
rm(list = setdiff(ls(), c('data.wo.void','categories.sub')))



#The query text that follows is outdated at of 2/27/2014 and is included to preserve documentation of the source for the element file
#currently listed as an option at the top of this file.
#
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
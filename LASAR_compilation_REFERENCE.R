#### This code has already been run and is included as reference to see the decisions made to 
#### transform the lasar data into element data

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

lasar.data <- rbind(lasar, willy.metals.melted)

write.csv(lasar.data, '//deqlead01/wqm/toxics_2012/data/TMP-Lasar-in-Element-Format-02142014.csv', row.names = FALSE)

## Setup Steps
# This creates a data source name driver for R to use when connecting to the database

# 1. Open Windows Explorer and navigate to: C:\Windows\SysWOW64\
# 2. Find and open an exe named "odbcad32.exe"
# 3. Click the Tab "System DSN"
# 4. Click > ADD
# 5. Select the driver "SQL Server"
# 6. Name: LASAR2 DEV
#    Description: Leave Blank
#    Server: DEQSQL2\DEV
#   Click NEXT
# 7. Choose "With Windows NT authentication..."
#   Click NEXT
# 8. Change the default database to: LASAR2
#   Click NEXT
# 9. Don't change anything here > Click FINISH
# 10. Click "Test Data Source" to verify it works - Click OK
# 11. Click OK on ODBC screen

# Make sure to use the 32 bit version of R if you are using a 64 bit computer

# If you use R studio set the defualt to 32 bit 
# 1. In R studio go to Tools > Options
# 2. For the R version, Click CHANGE
# 3. Select "Choose a specfic version of R"
# 4. Select [32-bit] C:\Program Files\R\R-...
# 5. Click OK, Click OK.
# 6. Close R Studio and restart again for changes to take affect.

###########################################################

## Load required R packages
library(RODBC)
library(plyr)

## Make a connection to LASAR
channel <- odbcConnect("LASAR")

## Grab the names of all the tables in the database
TableNames<- sqlTables(channel,errors = FALSE)

##Grab Unit table
Unit <- sqlFetch(channel, 'UNIT')

## Grab all the stations in the database
StationsAll <- sqlFetch(channel, "STATION")

##Restrict stations to types of interest
#UsedUses <- XLU_STATION_USE[XLU_STATION_USE$XLU_STATION_USE_KEY %in% unique_use,]
#XLU_STATION_USE
# XLU_STATION_USE_KEY DESCRIPTION
# 32  Beaches
# 51  Domestic Supply
# 63	Obs/Monitoring
# 71	Air Quality
# 77	Industrial Waste
# 78	Agricultural Waste
# 79	Ambient Water Quality
# 97	Stream surface water
# 98	Lake surface water
# 103	303d analysis Stations General
# 104	303d analysis Saltwater stations
# 105	Subsurface water (well,groundwater,subsurface treatment water)
# 106	Domestic Wastewater treatment (septic tank)
# 107	303d analysis Cascade Lakes above 3000 ft.
# 109	Landfill monitoring well
# 110	Domestic drinking water well
# 111	Landfill surface water
# 113	Domestic Wastewater treatment (STP)
# 114	Umatilla Army WMD Depot well
# 112	Canal, culvert, ditch, drain
# 119	Estuary
# 120	Stream or River
# 121	Land
# 122	Source
# 123	Other
# 124	Lake or Pond
# 125	Air Monitoring
# 126	Reservoir or Quarry
# 127	Ocean
# 128	Wetland
# 129	Spring
# 130	Groundwater

#Relevant use codes based on above description
UseCodes <- c(32, 63, 79, 97, 98, 103, 104, 107, 112, 119, 120, 124, 126, 127, 128, 129)
STATION_USE <- sqlFetch(channel, "STATION_USE")
Stations_with_use <- STATION_USE[STATION_USE$XLU_STATION_USE %in% UseCodes,]
myStations <- StationsAll[StationsAll$STATION_KEY %in% Stations_with_use$STATION,'STATION_KEY']

## Grab the SAMPLE_KEYs associated with these stations in the time period of interest
Samples <- sqlFetch(channel, 'SAMPLE')
SamplesInDateRange <- Samples[Samples$SAMPLE_DATE > as.POSIXct('2003-01-01'),]
mySampleKeys <- SamplesInDateRange[SamplesInDateRange$STATION_KEY %in% myStations,'SAMPLE_KEY']

## Grab a list of all the parameters and numerical codes
AllParameters <- sqlFetch(channel, "PARAMETER")

#Get the Parameter Keys for the parameters needed for the BLM
myParams.s <- AllParameters[AllParameters$NAME %in% c('Temperature', 'pH', 'Dissolved Organic Carbon', 'Calcium', 'Magnesium', 
                                                      'Sodium', 'Potassium', 'Sulfate', 'Chloride', 'Alkalinity as Calcium Carbonate', 
                                                      'Copper', 'Conductivity', 'Total Organic Carbon', 'Total Dissolved Solids', 
                                                      'Hardness as CaCO3'), 'PARAMETER_KEY']

### Set the Date/Time Range
myStartdate <- "2003-01-01"
myEnddate <- "2012-12-31"

myQuery <- c()

# for (station in myStations) {
#   for (param in myParams){
#     qry <- paste("SELECT * FROM Result WHERE (Station =",station,") AND (XLU_LASAR_PARAMETER =",param,") AND (SAMPLE_DATE_TIME >='",myStartdate,"') AND (SAMPLE_DATE_TIME <='",myEnddate,"')", sep="")
#     myQuery <- append(myQuery, qry)
#     }
#   }

#for (station in myStations) {
#  qry <- paste("SELECT * FROM Result WHERE (Station =",station,") AND (XLU_LASAR_PARAMETER IN (",myParams.s,")) AND (SAMPLE_DATE_TIME >='",myStartdate,"') AND (SAMPLE_DATE_TIME <='",myEnddate,"')", sep="")
#  myQuery <- append(myQuery, qry)
#}

chrMySampleKeys <- paste(mySampleKeys, collapse = ', ')
chrMyParams <- paste(myParams.s, collapse = ', ')
qry <- paste("SELECT * FROM PARAMETER_RESULT WHERE (SAMPLE_KEY IN (",chrMySampleKeys,")) AND (PARAMETER_KEY IN (",chrMyParams,"))", sep="")



## Retreive data.
mydata <- sqlQuery(channel, qry, stringsAsFactors = FALSE, na.strings = 'NA', as.is = TRUE)

mydata.sub <- mydata[,c('SAMPLE_KEY','SAMPLE_MATRIX_KEY', 'PARAMETER_PREFIX_1', 'PARAMETER_PREFIX_2', 
                        'PARAMETER_KEY', 'PARAMETER_SUFFIX_1', 'PARAMETER_SUFFIX_2', 'RESULT_TYPE', 
                        'RESULT', 'UNIT_KEY', 'METHOD_DETECTION_LIMIT', 'METHOD_REPORTING_LIMIT', 'QA_QC_TYPE','QA_QC_STATUS')]

#Add station info to result file
toAdd <- SamplesInDateRange[,c('SAMPLE_KEY','SAMPLE_DATE','STATION_KEY')]

mydata.w.Stations <- merge(mydata.sub, toAdd, by = 'SAMPLE_KEY', all.x = TRUE)

#Add parameter names
Parameter <- sqlFetch(channel, 'PARAMETER')

mydata.w.names <- merge(mydata.w.Stations, Parameter[,c('PARAMETER_KEY','NAME')], by = 'PARAMETER_KEY', all.x = TRUE)

#Pare down the data to the primary samples
qaqc <- sqlFetch(channel, 'XLU_QA_QC_TYPE')

toSelect <- qaqc[qaqc$QA_QC_TYPE_KEY %in% mydata.w.names$QA_QC_TYPE,c('QA_QC_TYPE_KEY','QA_QC_TYPE')]

toSelect <- rename(toSelect, c('QA_QC_TYPE' = 'QA_QC_TYPE_NAME'))

mydata.primary <- mydata.w.names[mydata.w.names$QA_QC_TYPE %in% c(1,17,18,33,42),]
mydata.primary <- merge(mydata.primary, toSelect, by.x = 'QA_QC_TYPE', by.y = 'QA_QC_TYPE_KEY', all.x = TRUE)

#Bring in values for all of those silly keys
sample.matrix <- sqlFetch(channel, 'SAMPLE_MATRIX')
sample.matrix.sub <- sample.matrix[,c('SAMPLE_MATRIX_KEY','SAMPLE_MATRIX')]
mydata.w.matrix <- merge(mydata.primary, sample.matrix.sub, by = 'SAMPLE_MATRIX_KEY', all.x = TRUE)

parameter.modifier <- sqlFetch(channel, 'PARAMETER_MODIFIER')
parameter.modifier <- parameter.modifier[,c('MODIFIER_KEY','ABBREVIATION')]
mydata.prefix1 <- merge(mydata.w.matrix, parameter.modifier, by.x = 'PARAMETER_PREFIX_1', by.y = 'MODIFIER_KEY', all.x = TRUE)
mydata.prefix1 <- rename(mydata.prefix1, c('ABBREVIATION' = 'PARAMETER_PREFIX_1_ABBREVIATION'))

mydata.prefix2 <- merge(mydata.prefix1, parameter.modifier, by.x = 'PARAMETER_PREFIX_2', by.y = 'MODIFIER_KEY', all.x = TRUE)
mydata.prefix2 <- rename(mydata.prefix2, c('ABBREVIATION' = 'PARAMETER_PREFIX_2_ABBREVIATION'))

mydata.suffix1 <- merge(mydata.prefix2, parameter.modifier, by.x = 'PARAMETER_SUFFIX_1', by.y = 'MODIFIER_KEY', all.x = TRUE)
mydata.suffix1 <- rename(mydata.suffix1, c('ABBREVIATION' = 'PARAMETER_SUFFIX_1_ABBREVIATION'))

mydata.suffix2 <- merge(mydata.suffix1, parameter.modifier, by.x = 'PARAMETER_SUFFIX_2', by.y = 'MODIFIER_KEY', all.x = TRUE)
mydata.suffix2 <- rename(mydata.suffix2, c('ABBREVIATION' = 'PARAMETER_SUFFIX_2_ABBREVIATION'))

unit <- sqlFetch(channel, 'UNIT')
mydata.unit <- merge(mydata.suffix2, unit[,c('UNIT_KEY','UNIT')], by = 'UNIT_KEY', all.x = TRUE)

status <- sqlFetch(channel, 'XLU_STATUS')
mydata.status <- merge(mydata.unit, status[,c('XLU_STATUS_KEY','STATUS')], by.x = 'QA_QC_STATUS', by.y = 'XLU_STATUS_KEY', all.x = TRUE)

station <- sqlFetch(channel, 'STATION')
mydata.location <- merge(mydata.status, station[,c('STATION_KEY','LOCATION_DESCRIPTION')],by = 'STATION_KEY', all.x = TRUE)

mydata <- mydata.location

close(channel)

#pare down and order the columns how we want
mydata <- mydata[,c('STATION_KEY', 'LOCATION_DESCRIPTION','SAMPLE_DATE', 'SAMPLE_MATRIX', 'QA_QC_TYPE_NAME', 
                    'PARAMETER_PREFIX_1_ABBREVIATION', 'PARAMETER_PREFIX_2_ABBREVIATION',
                    'NAME','PARAMETER_SUFFIX_1_ABBREVIATION', 'PARAMETER_SUFFIX_2_ABBREVIATION',
                    'RESULT', 'UNIT', 'METHOD_DETECTION_LIMIT', 'METHOD_REPORTING_LIMIT', 'STATUS')]

#remove cancelled and void 
mydata <- mydata[!mydata$RESULT %in% c('Void','Cancelled','canceled', 'No sample', 'ELEMENT'),]

#remove air data
mydata <- mydata[mydata$SAMPLE_MATRIX != 'Ambient air',]

## Write ouput to file
# First designate outfile path and name
thedate <-Sys.Date()
outpath <-"//deqlead01/wqm/toxics_2012/data/"
outfile <- paste("LASAR_Query_",substr(myStartdate,1,10),"_to_",substr(myEnddate,1,10),"_on_",thedate,".csv",sep="") 

write.csv(mydata, paste(outpath,outfile,sep=""))

#Then do the writing
#  outfile <- paste("LASAR_DOconc_Query_",substr(myStartdate,1,10),"_to_",substr(myEnddate,1,10),"_on_",thedate,".csv",sep="") 
#  write.csv(DO.mgl, paste(outpath,outfile,sep=""))

#outfile <- paste("LASAR_DOps_Query_",substr(myStartdate,1,10),"_to_",substr(myEnddate,1,10),"_on_",thedate,".csv",sep="") 
#write.csv(DO.ps, paste(outpath,outfile,sep=""))

#outfile <- paste("LASAR_Temp_Query_",substr(myStartdate,1,10),"_to_",substr(myEnddate,1,10),"_on_",thedate,".csv",sep="") 
#write.csv(Temp.match, paste(outpath,outfile,sep=""))






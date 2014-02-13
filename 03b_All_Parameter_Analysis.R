#This script matches the analytes to a corresponding criteria in either a DEQ Water Quality Standard table,
#an Office of Pesticide benchmark or and Office of Water benchmark where applicable.

#This brings in the criteria table and the constants necessary for harndess calculations
source('//deqlead01/wqm/TOXICS_2012/Data/R/criteria.R')

#This acts as a library for the hardness evaluation functions
source('//deqlead01/wqm/TOXICS_2012/Data/R/hardness_eval_functions_Element_Names.R')

#before we can bring in the criteria we need to match names
#unique(data.wo.void$Analyte)[!unique(data.wo.void$Analyte) %in% min.criteria.values$Pollutant]
#these names were referenced and compiled into this file
name.match <- read.csv('//deqlead01/wqm/TOXICS_2012/Data/Criteria_benchmarks_etc/pollutant_corrections.csv', na.strings = c('NA', ''))

#Here we do the name mapping to rename the data we have to match the criteria
name.match.sub <- name.match[!is.na(name.match$criteria_name),]

data.wo.void$Analyte <- as.factor(data.wo.void$Analyte)

data.wo.void$Analyte <- mapvalues(data.wo.void$Analyte, from = name.match.sub$name_in_data, to = name.match.sub$criteria_name)

data.wo.void$Analyte <- as.character(data.wo.void$Analyte)

#Now that the names are consistent we can match using analyte name and bring in the criteria
criteria.for.analytes.we.have <- criteria.values.melted.applicable[criteria.values.melted.applicable$Pollutant %in% data.wo.void$Analyte,]
dvc <- merge(data.wo.void, criteria.for.analytes.we.have, by.x = 'Analyte', by.y = 'Pollutant', all = TRUE)

#Using the hardness evaluation function loaded above we can calculate the hardness based criteria values
#and bring them into the dataframe with the other criteria values. First, though we remove the hardness 
#metals from the dataframe since the output of the function maintains all the columns of the original dataframe
hm <- hardness.crit.calc(data.wo.void)
hm <- hm[,names(dvc)]
dvc.wo.hm <- dvc[!dvc$Analyte %in% hm$Analyte,]
dvc.hm <- rbind(dvc.wo.hm, hm)

#need to do unit conversion/mapping so the criteria and the results are in the same units
dvc.hm[dvc.hm$Unit == 'ng/L','tResult'] <- dvc.hm[dvc.hm$Unit == 'ng/L','tResult'] / 1000
dvc.hm[dvc.hm$Unit == 'ng/L','Unit'] <-  "µg/L"

dvc.hm[dvc.hm$Unit == 'mg/L','tResult'] <- dvc.hm[dvc.hm$Unit == 'mg/L','tResult'] * 1000
dvc.hm[dvc.hm$Unit == 'mg/L','Unit'] <-  "µg/L"

#marks records that exceed the criteria or benchmark
dvc.hm$exceed <- ifelse(dvc.hm$tResult >= dvc.hm$value, 1, 0)

#calculates the magnitude or ratio of the result to the criteria
dvc.hm$magnitude <- dvc.hm$tResult/dvc.hm$value

#make it look pretty for excel
#first make an id column to pull together the columns since each casting can only handle one compiled column
#at a time for the exceed, criteria value and magnitude/ratio
dvc.hm$ID <- paste(dvc.hm$Analyte, dvc.hm$Project, dvc.hm$SampleRegID, dvc.hm$SampleAlias, dvc.hm$Sampled, dvc.hm$SampleType, dvc.hm$tResult, dvc.hm$tMRL, dvc.hm$Unit, dvc.hm$SpecificMethod, dvc.hm$Status, dvc.hm$chem.group, dvc.hm$Detect.nondetect, sep='-')
casted.exceed <- dcast(dvc.hm, ID + Project + SampleRegID + SampleAlias + Sampled + SampleType + Analyte + 
                  tResult + tMRL + Unit + SpecificMethod + Status + chem.group + Detect.nondetect ~ variable, value.var = 'exceed',
                fun.aggregate = function(x){ifelse(length(x) == 0,as.numeric(NA),sum(as.numeric(x)))})

casted.crit <- dcast(dvc.hm, ID ~ variable, value.var = 'value',
                fun.aggregate = function(x){ifelse(length(x) == 0,as.numeric(NA),as.numeric(x))})

casted.magnitude <- dcast(dvc.hm, ID ~ variable, value.var = 'magnitude',
                          fun.aggregate = function(x){ifelse(length(x) == 0,as.numeric(NA),as.numeric(x))})

#Now we need to make sure the column names reflect what was calculated 
casted.exceed <- rename(casted.exceed, sapply(names(casted.exceed)[15:26],FUN = function(x) {paste(x, 'Exceed', sep = ' - ')}))
casted.crit <- rename(casted.crit, sapply(names(casted.crit)[2:13],FUN = function(x) {paste(x, 'Criteria Value', sep = ' - ')}))
casted.magnitude <- rename(casted.magnitude, sapply(names(casted.magnitude)[2:13],FUN = function(x) {paste(x, 'Magnitude', sep = ' - ')}))

#Now we remove the NA column which is an artifact from those analytes that don't associate to a criteria or benchmark
casted.exceed <- casted.exceed[,setdiff(names(casted.exceed),'NA')]
casted.crit <- casted.crit[,setdiff(names(casted.crit),'NA')]
casted.magnitude <- casted.magnitude[,setdiff(names(casted.magnitude),'NA')]

#Now we put them together
cec <- merge(casted.exceed, casted.crit, by = 'ID')
cecm <- merge(cec, casted.magnitude, by = 'ID')

#Now that they've been re-associated we can remove the ID column
cecm <- within(cecm, rm('ID','NA'))

#This groups the criteria exceed,value and magnitude columns so the same sources are grouped together
cecm.ordered <- cecm[,c(names(cecm)[1:13],sort(names(cecm)[14:55]))]

#If you're ready to make this a static file uncomment the next line and run it
#write.csv(cecm.ordered,'//deqlead01/wqm/toxics_2012/data/TMP-Water-Evaluated-Against-Criteria.csv',row.names=FALSE)

#Now in order to roll this up for summary reporting we need to change the column names to names
#that R can work with (i.e. no spaces).
names(casted.exceed) <- make.names(names(casted.exceed))

#Here is where the summary happens. Summing the exceedances for each Standard or Benchmark 
#NOTE: might want to consider adding percent exceed for each criteria or at least the DEQ Tables
#code would look like(untested): Table40.WO.Percent.Exceed = (sum(Table.40.Human.Health.Criteria.for.Toxic.Pollutants...Water...Organism...Exceed)/length(Detect.nondetect))*100
dvc.hm.ru <- ddply(casted.exceed, .(Project,SampleRegID,SampleAlias,chem.group,Analyte,SpecificMethod), summarise, 
                 Table40.WO.Exceed = sum(Table.40.Human.Health.Criteria.for.Toxic.Pollutants...Water...Organism...Exceed), 
                 Table40.OO.Exceed = sum(Table.40.Human.Health.Criteria.for.Toxic.Pollutants...Organism.Only...Exceed), 
                 Table30.Acute.Exceed = sum(Table.30.Toxic.Substances...Freshwater.Acute...Exceed),
                 Table30.Chronic.Exceed = sum(Table.30.Toxic.Substances...Freshwater.Chronic...Exceed),
                 OPP.Acute.Fish = sum(OPP.Aquatic.Life.Benchmarks...Acute.Fish...Exceed),
                 OPP.Chronic.Fish = sum(OPP.Aquatic.Life.Benchmarks...Chronic.Fish...Exceed),
                 OPP.Acute.Invertebrates = sum(OPP.Aquatic.Life.Benchmarks...Acute.Invertebrates...Exceed),
                 OPP.Chronic.Invertebrates = sum(OPP.Aquatic.Life.Benchmarks...Chronic.Invertebrates...Exceed),
                 OPP.Acute.Nonvascular.Plants = sum(OPP.Aquatic.Life.Benchmarks...Acute.Nonvascular.Plants...Exceed),
                 OPP.Acute.Vascular.Plants = sum(OPP.Aquatic.Life.Benchmarks...Acute.Vascular.Plants...Exceed),
                 Office.of.Water.Acute.ALC = sum(Office.of.Water.Aquatic.Life.Criteria...Maximum.Concentration..CMC....Exceed),
                 Office.of.Water.Chronic.ALC = sum(Office.of.Water.Aquatic.Life.Criteria...Continuous.Concentration..CCC....Exceed),
                 sample.count = length(Detect.nondetect), 
                 detect.count = sum(Detect.nondetect, na.rm = TRUE))
dvc.hm.ru$percent.detect <- round(100*(dvc.hm.ru$detect.count/dvc.hm.ru$sample.count))

#This is also ready to be output now
#write.csv(dvc.hm.ru, '//deqlead01/wqm/toxics_2012/data/TMP-Water-Criteria-Evaluation-Summary.csv',row.names=FALSE)

#If you only want to see the Analytes that have a criteria or benchmark run this line
dvc.ru.applicable <- dvc.hm.ru[rowSums(is.na(dvc.hm.ru[,7:18])) != ncol(dvc.hm.ru[,7:18]),]

#If you only want to see the Analytes that exceed any criteria or benchmark run this line
dvc.ru.exceed <- dvc.hm.ru[rowSums(dvc.hm.ru[,7:18],na.rm=TRUE) > 0,]

#By basin summaries
dvc.hm.detect <- dvc.hm[dvc.hm$Detect.nondetect > 0,]

dvc.hm.basin <- ddply(dvc.hm.detect, .(Project,SampleRegID,SampleAlias,Analyte,SpecificMethod,chem.group),
                      function(x){ifelse(is.na(which(x$tResult == max(x$tResult) & x$value == min(x$value))[1]),
                                         df <- x[which.max(x$tResult),],
                                         df <- x[which(x$tResult == max(x$tResult) & x$value == min(x$value))[1],])
                                  return(df)})
dvc.hm.basin[dvc.hm.basin$Anlayte %in% constants$Analyte,'value'] <- 'hardness'
dvc.hm.basin$ID <- paste(dvc.hm.basin$Project, dvc.hm.basin$Analyte, dvc.hm.basin$SpecificMethod)

dvc.ru.sub <- ddply(dvc.hm.ru, .(Project,Analyte,SpecificMethod,chem.group), summarise, 
                    sample.count = sum(sample.count), 
                    detect.count = sum(detect.count), 
                    percent.detect = (sum(detect.count)/sum(sample.count))*100)
dvc.ru.sub$ID <- paste(dvc.ru.sub$Project, dvc.ru.sub$Analyte, dvc.ru.sub$SpecificMethod)
dvc.ru.sub <- dvc.ru.sub[,c('ID','sample.count','detect.count','percent.detect')]

df.list <- list()
for (i in 1:length(unique(dvc.hm.basin$Project))) {
  dvc.hm.basin.sub <- dvc.hm.basin[dvc.hm.basin$Project == unique(dvc.hm.basin$Project)[i],]
  df.list[[i]] <- dcast(dvc.hm.basin.sub, Project + Analyte + chem.group + SpecificMethod + variable + value + ID ~ SampleRegID + SampleAlias, value.var = 'tResult', fill = '')
  df.list[[i]] <- merge(df.list[[i]], dvc.ru.sub, by = 'ID', all.x = TRUE)
  df.list[[i]] <- within(df.list[[i]], rm('ID'))
}

#output the summary tables to excel
for (i in 1:length(df.list)) {
  basinName <- unique(df.list[[i]]$Project)
  write.csv(df.list[[i]],paste('//deqlead01/wqm/toxics_2012/Data/',basinName,'-SummaryTable.csv',sep=''),row.names=FALSE)
}




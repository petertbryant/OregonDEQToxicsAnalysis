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

#Some of the criteria apply to Totals and not individual degradates. Here we do that totalling so comparisons can be done.
#First, we will make a Total DDT
ddt <- data.wo.void[data.wo.void$Analyte %in% c("4,4`-DDD", "4,4`-DDE", "4,4`-DDT"),]
ddt.casted <- dcast(ddt, Project + SampleRegID + SampleAlias + Matrix + 
                         Sampled + SampleType + SpecificMethod + chem.group ~ Analyte, value.var = 'tResult')
ddt.casted$'Total DDT' <- rowSums(ddt.casted[,c("4,4`-DDD", "4,4`-DDE", "4,4`-DDT")],na.rm=TRUE)
ddt.casted.sub <- within(ddt.casted, rm("4,4`-DDD", "4,4`-DDE", "4,4`-DDT"))
ddt.melted <- melt(ddt.casted.sub, 
                   id.vars = c('Project','SampleRegID','SampleAlias','Sampled','Matrix','SampleType','SpecificMethod','chem.group'),
                   variable.name = 'Analyte',
                   value.name = 'tResult')
ddt.melted$Detect.nondetect <- ifelse(ddt.melted$tResult > 0,1,0)
ddt.melted.addons <- data.frame('tMRL' = rep(0,nrow(ddt.melted)), 'Unit' = rep('µg/L',nrow(ddt.melted)), 'Status' = rep('A',nrow(ddt.melted)))
ddt.melted <- cbind(ddt.melted, ddt.melted.addons)
data.wo.void <- rbind(data.wo.void, ddt.melted)

#Now Total Endosulfan
endo <- data.wo.void[data.wo.void$Analyte %in% c("Endosulfan I", "Endosulfan II", "Endosulfan Sulfate"),]
endo.casted <- dcast(endo, Project + SampleRegID + SampleAlias + Matrix +
                      Sampled + SampleType + SpecificMethod + chem.group ~ Analyte, value.var = 'tResult')
endo.casted$Endosulfan <- rowSums(endo.casted[,c("Endosulfan I", "Endosulfan II", "Endosulfan Sulfate")],na.rm=TRUE)
endo.casted.sub <- within(endo.casted, rm("Endosulfan I", "Endosulfan II", "Endosulfan Sulfate"))
endo.melted <- melt(endo.casted.sub, id.vars = c('Project','SampleRegID','SampleAlias','Sampled','Matrix','SampleType','SpecificMethod','chem.group'),variable.name = 'Analyte',value.name = 'tResult')#melt
endo.melted$Detect.nondetect <- ifelse(endo.melted$tResult > 0,1,0)
endo.melted.addons <- data.frame('tMRL' = rep(0,nrow(endo.melted)), 'Unit' = rep('µg/L',nrow(endo.melted)), 'Status' = rep('A',nrow(endo.melted)))
endo.melted <- cbind(endo.melted, endo.melted.addons)
data.wo.void <- rbind(data.wo.void, endo.melted)

#Now Total Chlordane
chlordane <- data.wo.void[data.wo.void$Analyte %in% c("Oxychlordane", "alpha-Chlordane", "cis-Chlordane", 'trans-Chlordane',"gamma-Chlordane+trans-Nonachlor", "trans-Nonachlor", "cis-Nonachlor"),]
chlordane.casted <- dcast(chlordane, Project + SampleRegID + SampleAlias + Matrix +
                       Sampled + SampleType + SpecificMethod + chem.group ~ Analyte, value.var = 'tResult')
chlordane.casted$Chlordane <- rowSums(chlordane.casted[,c("Oxychlordane", "alpha-Chlordane", "cis-Chlordane", 'trans-Chlordane',"gamma-Chlordane+trans-Nonachlor", "trans-Nonachlor", "cis-Nonachlor")],na.rm=TRUE)
chlordane.casted.sub <- within(chlordane.casted, rm("Oxychlordane", "alpha-Chlordane", "cis-Chlordane", 'trans-Chlordane',"gamma-Chlordane+trans-Nonachlor", "trans-Nonachlor", "cis-Nonachlor"))
chlordane.melted <- melt(chlordane.casted.sub, id.vars = c('Project','SampleRegID','SampleAlias','Sampled','Matrix','SampleType','SpecificMethod','chem.group'),variable.name = 'Analyte',value.name = 'tResult')#melt
chlordane.melted$Detect.nondetect <- ifelse(chlordane.melted$tResult > 0,1,0)
chlordane.melted.addons <- data.frame('tMRL' = rep(0,nrow(chlordane.melted)), 'Unit' = rep('µg/L',nrow(chlordane.melted)), 'Status' = rep('A',nrow(chlordane.melted)))
chlordane.melted <- cbind(chlordane.melted, chlordane.melted.addons)
data.wo.void <- rbind(data.wo.void, chlordane.melted)

#Now total PCBs
pcb <- data.wo.void[grep('PCB',data.wo.void$Analyte),]
pcb.casted <- dcast(pcb, Project + SampleRegID + SampleAlias + Matrix + 
                            Sampled + SampleType + SpecificMethod + chem.group ~ Analyte, value.var = 'tResult')
pcb.casted$'Polychlorinated Biphenyls (PCBs)' <- rowSums(pcb.casted[,unique(data.wo.void[grep('PCB',data.wo.void$Analyte),'Analyte'])],na.rm=TRUE)
pcb.casted.sub <- pcb.casted[,!names(pcb.casted) %in% unique(data.wo.void[grep('PCB',data.wo.void$Analyte),'Analyte'])]
pcb.melted <- melt(pcb.casted.sub, id.vars = c('Project','SampleRegID','SampleAlias','Sampled','Matrix','SampleType','SpecificMethod','chem.group'),variable.name = 'Analyte',value.name = 'tResult')#melt
pcb.melted$Detect.nondetect <- ifelse(pcb.melted$tResult > 0,1,0)
pcb.melted.addons <- data.frame('tMRL' = rep(0,nrow(pcb.melted)), 'Unit' = rep('µg/L',nrow(pcb.melted)), 'Status' = rep('A',nrow(pcb.melted)))
pcb.melted <- cbind(pcb.melted, pcb.melted.addons)
data.wo.void <- rbind(data.wo.void, pcb.melted)

#We need an ID to match with the criteria so we'll simplify the Matrix field
data.wo.void$Matrix <- mapvalues(data.wo.void$Matrix, from = c("River/Stream", "Estuary"), to = c('FW','SW'))
data.wo.void$ID <- paste(data.wo.void$Analyte, data.wo.void$Matrix)

#Now that the names are consistent we can match using analyte name and bring in the criteria
criteria.for.analytes.we.have <- criteria.values.melted.applicable[criteria.values.melted.applicable$Pollutant %in% data.wo.void$Analyte,]
dvc <- merge(data.wo.void, criteria.for.analytes.we.have, by = 'ID', all.x = TRUE)

#Using the hardness evaluation function loaded above we can calculate the hardness based criteria values
#and bring them into the dataframe with the other criteria values. First, though we remove the hardness 
#metals from the dataframe since the output of the function maintains all the columns of the original dataframe
hm <- hardness.crit.calc(data.wo.void)
hm <- hm[,names(dvc)]
dvc.wo.hm <- dvc[!dvc$Analyte %in% hm$Analyte,]
dvc.hm <- rbind(dvc.wo.hm, hm)

#Similarly pentachlorophenol is parameter dependent and is handled the same as hardness
penta <- pentachlorophenol.crit.calc(data.wo.void)
penta <- penta[,names(dvc)]
dvc.wo.penta <- dvc.hm[!dvc.hm$Analyte %in% penta$Analyte,]
dvc.penta <- rbind(dvc.wo.penta, penta)

#make a couple sites saltwater for testing
#data.wo.void[data.wo.void$SampleRegID %in% c(28574,13141),'Matrix'] <- 'SW'

#Ammonia is also parameter dependent and is handled similarly
amm <- ammonia.crit.calc(data.wo.void)
amm <- amm[,names(dvc)]
dvc.wo.amm <- dvc.penta[!dvc.penta$Analyte %in% amm$Analyte,]
dvc.amm <- rbind(dvc.wo.amm, amm)
dvc.hm <- dvc.amm

#need to do unit conversion/mapping so the criteria and the results are in the same units
dvc.hm[dvc.hm$Unit == 'ng/L','tResult'] <- dvc.hm[dvc.hm$Unit == 'ng/L','tResult'] / 1000
dvc.hm[dvc.hm$Unit == 'ng/L','Unit'] <-  "µg/L"

dvc.hm[dvc.hm$Unit == 'mg/L','tResult'] <- dvc.hm[dvc.hm$Unit == 'mg/L','tResult'] * 1000
dvc.hm[dvc.hm$Unit == 'mg/L','Unit'] <-  "µg/L"

#marks records that exceed the criteria or benchmark - NEED TO ADD logic to handle SALTWATER/MARINE sites
dvc.hm$exceed <- ifelse(dvc.hm$tResult >= dvc.hm$value, 1, 0)

#calculates the magnitude or ratio of the result to the criteria
dvc.hm$magnitude <- dvc.hm$tResult/dvc.hm$value

#make it look pretty for excel
#first make an id column to pull together the columns since each casting can only handle one compiled column
#at a time for the exceed, criteria value and magnitude/ratio
dvc.hm$ID <- paste(dvc.hm$Analyte, dvc.hm$Project, dvc.hm$SampleRegID, dvc.hm$SampleAlias, dvc.hm$Sampled, dvc.hm$SampleType, dvc.hm$tResult, dvc.hm$tMRL, dvc.hm$Unit, dvc.hm$SpecificMethod, dvc.hm$Status, dvc.hm$chem.group, dvc.hm$Detect.nondetect, sep='-')
casted.exceed <- dcast(dvc.hm, ID + Project + SampleRegID + SampleAlias + Matrix.x + Sampled + SampleType + Analyte + 
                  tResult + tMRL + Unit + SpecificMethod + Status + chem.group + Detect.nondetect ~ variable, value.var = 'exceed',
                fun.aggregate = function(x){ifelse(length(x) == 0,as.numeric(NA),sum(as.numeric(x)))})

casted.crit <- dcast(dvc.hm, ID ~ variable, value.var = 'value',
                fun.aggregate = function(x){ifelse(length(x) == 0,as.numeric(NA),as.numeric(x))})

casted.magnitude <- dcast(dvc.hm, ID ~ variable, value.var = 'magnitude',
                          fun.aggregate = function(x){ifelse(length(x) == 0,as.numeric(NA),as.numeric(x))})

#Now we need to make sure the column names reflect what was calculated 
casted.exceed <- rename(casted.exceed, sapply(names(casted.exceed)[16:length(names(casted.exceed))],FUN = function(x) {paste(x, 'Exceed', sep = ' - ')}))
casted.crit <- rename(casted.crit, sapply(names(casted.crit)[2:length(casted.crit)],FUN = function(x) {paste(x, 'Criteria Value', sep = ' - ')}))
casted.magnitude <- rename(casted.magnitude, sapply(names(casted.magnitude)[2:length(casted.magnitude)],FUN = function(x) {paste(x, 'Magnitude', sep = ' - ')}))

#Now we remove the NA column which is an artifact from those analytes that don't associate to a criteria or benchmark
casted.exceed <- casted.exceed[,setdiff(names(casted.exceed),'NA - Exceed')]
casted.crit <- casted.crit[,setdiff(names(casted.crit),'NA - Criteria Value')]
casted.magnitude <- casted.magnitude[,setdiff(names(casted.magnitude),'NA - Magnitude')]

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
if (any(names(casted.exceed) %in% c('Table.30.Toxic.Substances...Saltwater.Acute...Exceed','Table.30.Toxic.Substances...Saltwater.Chronic...Exceed'))) {
  dvc.hm.ru <- ddply(casted.exceed, .(Project,SampleRegID,SampleAlias,chem.group,Analyte,SpecificMethod), summarise, 
                     Table40.WO.Exceed = sum(Table.40.Human.Health.Criteria.for.Toxic.Pollutants...Water...Organism...Exceed), 
                     Table40.OO.Exceed = sum(Table.40.Human.Health.Criteria.for.Toxic.Pollutants...Organism.Only...Exceed), 
                     Table30.FW.Acute.Exceed = sum(Table.30.Toxic.Substances...Freshwater.Acute...Exceed),
                     Table30.FW.Chronic.Exceed = sum(Table.30.Toxic.Substances...Freshwater.Chronic...Exceed),
                     Table30.SW.Acute.Exceed = sum(Table.30.Toxic.Substances...Saltwater.Acute...Exceed),
                     Table30.SW.Chronic.Exceed = sum(Table.30.Toxic.Substances...Saltwater.Chronic...Exceed),
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
} else {
  dvc.hm.ru <- ddply(casted.exceed, .(Project,SampleRegID,SampleAlias,chem.group,Analyte,SpecificMethod), summarise, 
                     Table40.WO.Exceed = sum(Table.40.Human.Health.Criteria.for.Toxic.Pollutants...Water...Organism...Exceed), 
                     Table40.OO.Exceed = sum(Table.40.Human.Health.Criteria.for.Toxic.Pollutants...Organism.Only...Exceed), 
                     Table30.FW.Acute.Exceed = sum(Table.30.Toxic.Substances...Freshwater.Acute...Exceed),
                     Table30.FW.Chronic.Exceed = sum(Table.30.Toxic.Substances...Freshwater.Chronic...Exceed),
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
}

dvc.hm.ru$percent.detect <- round(100*(dvc.hm.ru$detect.count/dvc.hm.ru$sample.count))

#This is also ready to be output now
#write.csv(dvc.hm.ru, '//deqlead01/wqm/toxics_2012/data/TMP-Water-Criteria-Evaluation-Summary.csv',row.names=FALSE)

#If you only want to see the Analytes that have a criteria or benchmark run this line
dvc.ru.applicable <- dvc.hm.ru[rowSums(is.na(dvc.hm.ru[,7:18])) != ncol(dvc.hm.ru[,7:18]),]

#If you only want to see the Analytes that exceed any criteria or benchmark run this line
dvc.ru.exceed <- dvc.hm.ru[rowSums(dvc.hm.ru[,7:18],na.rm=TRUE) > 0,]

#By basin summaries
dvc.hm.detect <- dvc.hm[dvc.hm$Detect.nondetect > 0,]

#This pulls out the maximum concentration at a site
dvc.hm.basin <- ddply(dvc.hm.detect, .(Project,SampleRegID,SampleAlias,Analyte,SpecificMethod,chem.group),
                      function(x){ifelse(is.na(which(x$tResult == max(x$tResult) & x$value == min(x$value))[1]),
                                         df <- x[which.max(x$tResult),],
                                         df <- x[which(x$tResult == max(x$tResult) & x$value == min(x$value))[1],])
                                  return(df)})

#For the hardness based criteria it doesn't make sense to report an actual value
dvc.hm.basin[dvc.hm.basin$Anlayte %in% constants$Analyte,'value'] <- 'hardness'

#This ID allows us to pull in the percent detect
dvc.hm.basin$ID <- paste(dvc.hm.basin$Project, dvc.hm.basin$Analyte, dvc.hm.basin$SpecificMethod)

#This makes the percent detect information available for joining
dvc.ru.sub <- ddply(dvc.hm.ru, .(Project,Analyte,SpecificMethod,chem.group), summarise, 
                    sample.count = sum(sample.count), 
                    detect.count = sum(detect.count), 
                    percent.detect = (sum(detect.count)/sum(sample.count))*100)

#This ID allows us to merge the percent detect with the concentration table
dvc.ru.sub$ID <- paste(dvc.ru.sub$Project, dvc.ru.sub$Analyte, dvc.ru.sub$SpecificMethod)

#We only need the ID and the percent detect info
dvc.ru.sub <- dvc.ru.sub[,c('ID','sample.count','detect.count','percent.detect')]

#This separates the basins into separate dataframes that are then merged with the percent detect info and then stored in a list
df.list <- list()
for (i in 1:length(unique(dvc.hm.basin$Project))) {
  dvc.hm.basin.sub <- dvc.hm.basin[dvc.hm.basin$Project == unique(dvc.hm.basin$Project)[i],]
  df.list[[i]] <- dcast(dvc.hm.basin.sub, Project + Analyte + chem.group + SpecificMethod + variable + value + ID ~ SampleRegID + SampleAlias, value.var = 'tResult', fill = '')
  df.list[[i]] <- merge(df.list[[i]], dvc.ru.sub, by = 'ID', all.x = TRUE)
  df.list[[i]] <- within(df.list[[i]], rm('ID'))
}

#output the individual basin summary tables to excel for pretty formatting
for (i in 1:length(df.list)) {
  basinName <- unique(df.list[[i]]$Project)
  write.csv(df.list[[i]],paste('//deqlead01/wqm/toxics_2012/Data/',basinName,'-SummaryTable.csv',sep=''),row.names=FALSE)
}




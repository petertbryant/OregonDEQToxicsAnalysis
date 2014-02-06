source('//deqlead01/wqm/TOXICS_2012/Data/R/criteria.R')
source('//deqlead01/wqm/TOXICS_2012/Data/R/hardness_eval_functions_Element_Names.R')

#before we can bring in the criteria we need to match names
#unique(data.wo.void$Analyte)[!unique(data.wo.void$Analyte) %in% min.criteria.values$Pollutant]

#these names were referenced and compiled into this file
name.match <- read.csv('//deqlead01/wqm/TOXICS_2012/Data/Criteria_benchmarks_etc/pollutant_corrections.csv', na.strings = c('NA', ''))

name.match.sub <- name.match[!is.na(name.match$criteria_name),]

data.wo.void$Analyte <- as.factor(data.wo.void$Analyte)

data.wo.void$Analyte <- mapvalues(data.wo.void$Analyte, from = name.match.sub$name_in_data, to = name.match.sub$criteria_name)

data.wo.void$Analyte <- as.character(data.wo.void$Analyte)

#brings in the criteria
criteria.for.analytes.we.have <- criteria.values.melted.applicable[criteria.values.melted.applicable$Pollutant %in% data.wo.void$Analyte,]
dvc <- merge(data.wo.void, criteria.for.analytes.we.have, by.x = 'Analyte', by.y = 'Pollutant', all = TRUE)

hm <- hardness.crit.calc(data.wo.void)
hm <- hm[,names(dvc)]
dvc.wo.hm <- dvc[!dvc$Analyte %in% hm$Analyte,]
dvc.hm <- rbind(dvc.wo.hm, hm)

#need to do unit conversion/mapping
dvc.hm[dvc.hm$Unit == 'ng/L','tResult'] <- dvc.hm[dvc.hm$Unit == 'ng/L','tResult'] / 1000
dvc.hm[dvc.hm$Unit == 'ng/L','Unit'] <-  "µg/L"

dvc.hm[dvc.hm$Unit == 'mg/L','tResult'] <- dvc.hm[dvc.hm$Unit == 'mg/L','tResult'] * 1000
dvc.hm[dvc.hm$Unit == 'mg/L','Unit'] <-  "µg/L"

#marks records that exceed the criteria or benchmark
dvc.hm$exceed <- ifelse(dvc.hm$tResult >= dvc.hm$value, 1, 0)

#magnitude
dvc.hm$magnitude <- dvc.hm$tResult/dvc.hm$value

#make it look pretty for excel
#first make an id column to pull together the columns
dvc.hm$ID <- paste(dvc.hm$Analyte, dvc.hm$Project, dvc.hm$SampleRegID, dvc.hm$SampleAlias, dvc.hm$Sampled, dvc.hm$SampleType, dvc.hm$tResult, dvc.hm$tMRL, dvc.hm$Unit, dvc.hm$SpecificMethod, dvc.hm$Status, dvc.hm$chem.group, dvc.hm$Detect.nondetect, sep='-')
casted.exceed <- dcast(dvc.hm, ID + Project + SampleRegID + SampleAlias + Sampled + SampleType + Analyte + 
                  tResult + tMRL + Unit + SpecificMethod + Status + chem.group + Detect.nondetect ~ variable, value.var = 'exceed',
                fun.aggregate = function(x){ifelse(length(x) == 0,as.numeric(NA),sum(as.numeric(x)))})

casted.crit <- dcast(dvc.hm, ID ~ variable, value.var = 'value',
                fun.aggregate = function(x){ifelse(length(x) == 0,as.numeric(NA),as.numeric(x))})

casted.magnitude <- dcast(dvc.hm, ID ~ variable, value.var = 'magnitude',
                          fun.aggregate = function(x){ifelse(length(x) == 0,as.numeric(NA),as.numeric(x))})

casted.crit <- rename(casted.crit, sapply(names(casted.crit)[2:13],FUN = function(x) {paste(x, 'Criteria Value', sep = ' - ')}))
casted.crit <- casted.crit[,setdiff(names(casted.crit),'NA')]

cec <- merge(casted.exceed, casted.crit, by = 'ID')

casted.magnitude <- rename(casted.magnitude, sapply(names(casted.magnitude)[2:13],FUN = function(x) {paste(x, 'Magnitude', sep = ' - ')}))
casted.magnitude <- casted.magnitude[,setdiff(names(casted.magnitude),'NA')]

cecm <- merge(cec, casted.magnitude, by = 'ID')

cecm <- within(cecm, rm('ID','NA'))

cecm.ordered <- cecm[,c(names(cecm)[1:13],sort(names(cecm)[14:55]))]

names(casted.exceed) <- make.names(names(casted.exceed))

dvc.hm.ru <- ddply(casted.exceed, .(Project,SampleRegID,SampleAlias,chem.group,Analyte), summarise, 
                 Table40.WO.Exceed = sum(Table.40.Human.Health.Criteria.for.Toxic.Pollutants...Water...Organism), 
                 Table40.OO.Exceed = sum(Table.40.Human.Health.Criteria.for.Toxic.Pollutants...Organism.Only), 
                 Table20.Acute.Exceed = sum(Table.30.Toxic.Substances...Freshwater.Acute),
                 Table20.Chronic.Exceed = sum(Table.30.Toxic.Substances...Freshwater.Chronic),
                 OPP.Acute.Fish = sum(OPP.Aquatic.Life.Benchmarks...Acute.Fish),
                 OPP.Chronic.Fish = sum(OPP.Aquatic.Life.Benchmarks...Chronic.Fish),
                 OPP.Acute.Invertebrates = sum(OPP.Aquatic.Life.Benchmarks...Acute.Invertebrates),
                 OPP.Chronic.Invertebrates = sum(OPP.Aquatic.Life.Benchmarks...Chronic.Invertebrates),
                 OPP.Acute.Nonvascular.Plants = sum(OPP.Aquatic.Life.Benchmarks...Acute.Nonvascular.Plants),
                 OPP.Acute.Vascular.Plants = sum(OPP.Aquatic.Life.Benchmarks...Acute.Vascular.Plants),
                 Office.of.Water.Acute.ALC = sum(Office.of.Water.Aquatic.Life.Criteria...Maximum.Concentration..CMC.),
                 Office.of.Water.Chronic.ALC = sum(Office.of.Water.Aquatic.Life.Criteria...Continuous.Concentration..CCC.),
                 sample.count = length(Detect.nondetect), 
                 detect.count = sum(Detect.nondetect, na.rm = TRUE))
dvc.hm.ru$percent.detect <- round(100*(dvc.hm.ru$detect.count/dvc.hm.ru$sample.count))
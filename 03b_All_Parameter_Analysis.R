source('//deqlead01/wqm/TOXICS_2012/Data/R/criteria.R')

#before we can bring in the criteria we need to match names
#unique(data.wo.void$Analyte)[!unique(data.wo.void$Analyte) %in% min.criteria.values$Pollutant]

#these names were referenced and compiled into this file
name.match <- read.csv('//deqlead01/wqm/TOXICS_2012/Data/Criteria_benchmarks_etc/pollutant_corrections.csv', na.strings = c('NA', ''))

name.match.sub <- name.match[!is.na(name.match$criteria_name),]

data.wo.void$Analyte <- as.factor(data.wo.void$Analyte)

data.wo.void$Analyte <- mapvalues(data.wo.void$Analyte, from = name.match.sub$name_in_data, to = name.match.sub$criteria_name)

data.wo.void$Analyte <- as.character(data.wo.void$Analyte)

#brings in the criteria
#data.wo.void <- merge(data.wo.void, min.criteria.values, by.x = 'Analyte', by.y = 'Pollutant', all.x = TRUE)

criteria.for.analytes.we.have <- criteria.values.melted.applicable[criteria.values.melted.applicable$Pollutant %in% data.wo.void$Analyte,]
data.wo.void.w.criteria <- merge(data.wo.void, criteria.for.analytes.we.have, by.x = 'Analyte', by.y = 'Pollutant', all = TRUE)

#need to do unit conversion/mapping
data.wo.void.w.criteria[data.wo.void.w.criteria$Unit == 'ng/L','tResult'] <- data.wo.void.w.criteria[data.wo.void.w.criteria$Unit == 'ng/L','tResult'] / 1000
data.wo.void.w.criteria[data.wo.void.w.criteria$Unit == 'ng/L','Unit'] <-  "µg/L"

data.wo.void.w.criteria[data.wo.void.w.criteria$Unit == 'mg/L','tResult'] <- data.wo.void.w.criteria[data.wo.void.w.criteria$Unit == 'mg/L','tResult'] * 1000
data.wo.void.w.criteria[data.wo.void.w.criteria$Unit == 'mg/L','Unit'] <-  "µg/L"

#marks records that exceed the criteria or benchmark
data.wo.void.w.criteria$exceed <- ifelse(data.wo.void.w.criteria$tResult >= data.wo.void.w.criteria$value, 1, 0)

#magnitude
data.wo.void.w.criteria$magnitude <- data.wo.void.w.criteria$tResult/data.wo.void.w.criteria$value

#make it look pretty for excel
View(dcast(data.wo.void.w.criteria, Project + SampleRegID + SampleAlias + Sampled + SampleType + Analyte + 
             tResult + tMRL + Unit + SpecificMethod + Status + chem.group + Detect.nondetect ~ variable))


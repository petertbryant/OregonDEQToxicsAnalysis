#This is the next script intended to be run after 01_data_compilation.R. This file has summaries of the data in a few different forms, with
#some draft graphs. For each summary table created there is a line write.xlsx that is commented out. The reasoning is that we don't need to 
#save to file every time we test it's creation here in R. When we get the final data we will want to change the name of the output excel file
#to something besides DRAFT. 

#list of all analytes with min, max, median, total samples, total detects, percent detect for each method applied
statewide.summary <- ddply(data.wo.void, .(Analyte, chem.group,SpecificMethod), summarise, 
                           min = min(tResult), 
                           max = max(tResult), 
                           median = median(tResult), 
                           N = length(Detect.nondetect), 
                           detects = sum(Detect.nondetect),
                           percent.detect = (sum(Detect.nondetect)/length(Detect.nondetect)*100))

#This creates the Data_Summary_DRAFT.xlsx file
#write.xlsx(statewide.summary,'//deqlead01/wqm/toxics_2012/data/r/Data_Summary_DRAFT.xlsx',sheetName='StatewideSummary',row.names = FALSE)

#for other comparisons we need to exclude recently added methods that don't apply to the entire dataset
data.wo.newmethods <- data.wo.void[!data.wo.void$SpecificMethod %in% c('EPA 1699','EPA 1613', 'EPA 1614A', 'EPA 1668C'),]

#there are also several samples that have multiple methods but those methods are also the only ones used for other samples
#so we can't just wholesale remove them
#data.wo.newmethods$code <- paste(data.wo.newmethods$Analyte, data.wo.newmethods$SampleRegID, data.wo.newmethods$Sampled)
#View(arrange(data.wo.newmethods[data.wo.newmethods$code %in% names(table(data.wo.newmethods$code)[((table(data.wo.newmethods$code)) > 1)]),],code))

#to compare to land use we need to pull that file in
lu <- read.xlsx('//deqlead03/gis_wa/project_working_folders/toxics/land use maps/attila_toxics_wshds.xlsx', sheetName = 'attila clip')
data.w.lu <- merge(data.wo.newmethods, lu, by.x = 'SampleRegID', by.y = 'NAME', all.x = TRUE)
#data.w.lu <- merge(data.wo.void, lu, by.x = 'SampleRegID', by.y = 'NAME', all.x = TRUE)
data.w.lu <- rename(data.w.lu, c('X.50.' = 'DomLU'))

#there are stations that we colelcted metals data at that didn't get included in Kara's initial Land Use analysis
#here we will take them out for now
data.w.lu <- data.w.lu[!is.na(data.w.lu$DomLU),]

#I think the raw data file I output for everyone to see should have the land use in it so I will output at this point
#The writing of this table using write.xlsx is too slow and throws an error with the Java Heap Space so to get it into the same file
#I write it to .csv first, which is fast then use the Move or Copy feature from the right click menu on the tab in Excel to place it 
#into the Data_Summary_DRAFT.xlsx file.
#write.csv(data.w.lu, '//deqlead01/wqm/toxics_2012/data/r/Data_wo_NewMethods.csv', row.names = FALSE)

#we also don't really want to include standard parameters here, plant or animal sterols and metals without criteria
#The chemicals that don't match to a category
dwn.sub <- data.w.lu[!data.w.lu$chem.group %in% c('Standard Parameters','NA','Plant or animal sterols') & !is.na(data.w.lu$chem.group),]
dwn.sub <- dwn.sub[!dwn.sub$Analyte %in% c('Antimony, Dissolved','Barium, Dissolved', 'Beryllium, Dissolved', 'Beryllium, Total recoverable',
                                           'Calcium, Dissolved', 'Calcium, Total recoverable', 'Chromium, Total recoverable', 'Cobalt, Total recoverable', 
                                           'Copper, Dissolved', 'Iron, Dissolved', 'Lead, Total recoverable', 'Magnesium, Dissolved', 
                                           'Magnesium, Total recoverable', 'Manganese, Dissolved', 'Manganese, Total recoverable',
                                           'Silver, Total recoverable', 'Thallium, Dissolved', 'Molybdenum, Total recoverable'),]

#let's look at chemical group by landuse
by.lu <- ddply(dwn.sub, .(DomLU,chem.group), function(x) {length(unique(x[x$Detect.nondetect > 0,'Analyte']))})
by.lu <- rename(by.lu, c('V1' = 'count'))
by.lu <- arrange(by.lu, DomLU, chem.group, desc(count))
by.lu <- arrange(dcast(by.lu, DomLU ~ chem.group),DomLU)
#write.xlsx(by.lu,'//deqlead01/wqm/toxics_2012/data/r/Data_Summary_DRAFT.xlsx',sheetName='UniqueCompoundsByGroupByLU',row.names = FALSE,append = TRUE)

#number of unique compounds detected per chemical group per station
by.group <- ddply(dwn.sub, .(Project,SampleRegID,SampleAlias,chem.group,DomLU), function(x) {length(unique(x[x$Detect.nondetect > 0,'Analyte']))})
by.group <- rename(by.group, c('V1' = 'count'))
for (i in 1:length(unique(by.group$SampleRegID))) {
  by.group[by.group$SampleRegID == unique(by.group$SampleRegID)[i],'TotalCount'] <- sum(by.group[by.group$SampleRegID == unique(by.group$SampleRegID)[i],'count'])
}
by.group <- arrange(by.group, SampleRegID, chem.group, desc(count))

#Some test graphs
# for (i in 1:length(unique(by.group$Project))) {
#   by.group.sub <- by.group[by.group$Project == unique(by.group$Project)[i],]
#   TitleText <- unique(by.group$Project)[i]
#   ggplot(by.group.sub, 
#          aes(x = reorder(SampleRegID,desc(TotalCount)), 
#              y = count, 
#              fill = chem.group)) + 
#     geom_bar(stat = 'identity') + 
#     theme(axis.text.x = element_text(angle = 90)) + 
#     ggtitle(TitleText)
#   ggsave(paste(TitleText, '-UniqueDetectsbyStation.pdf', sep = ''))
# }

by.group <- arrange(dcast(by.group, Project + SampleRegID + SampleAlias + DomLU + TotalCount ~ chem.group, value.var = 'count'),Project,SampleRegID)
#write.xlsx(by.group,'//deqlead01/wqm/toxics_2012/data/r/Data_Summary_DRAFT.xlsx',sheetName='UniqueCompoundsByGroupByStation',row.names = FALSE,append = TRUE)

#detections per chemical group without the new methods. median and min are calculated on detections only.
by.group.only <- ddply(dwn.sub, .(chem.group), summarise, detect = sum(Detect.nondetect), 
                       nondetect = length(Detect.nondetect) - sum(Detect.nondetect),
                       detection.frequency = (sum(Detect.nondetect)/length(Detect.nondetect))*100,
                       max.conc = max(tResult))
by.group.median <- ddply(dwn.sub, .(chem.group), function(x) {ifelse(all(is.na(x[x$Detect.nondetect > 0,'tResult'])),
                                                                     0,
                                                                     median(x[x$Detect.nondetect > 0,'tResult']))})
by.group.min <- ddply(dwn.sub, .(chem.group), function(x) {ifelse(all(is.na(x[x$Detect.nondetect > 0,'tResult'])),
                                                                  'ND',
                                                                  min(x[x$Detect.nondetect > 0,'tResult']))})
by.group.unique <- ddply(dwn.sub, .(chem.group), function(x) {length(unique(x[x$Detect.nondetect > 0,'Analyte']))})
by.group.total <- ddply(dwn.sub, .(chem.group), function(x) {length(unique(x$Analyte))})
by.group.median <- rename(by.group.median, c('V1' = 'meidan.conc'))
by.group.min <- rename(by.group.min, c('V1' = 'min.conc'))
by.group.unique <- rename(by.group.unique, c('V1' = 'CountOfUniqueCompoundsDetected'))
by.group.total <- rename(by.group.total, c('V1' = 'CountOfCompoundsAnalyzed'))
by.group.only <- merge(by.group.only, by.group.median, by = 'chem.group')
by.group.only <- merge(by.group.only, by.group.min, by = 'chem.group')
by.group.only <- merge(by.group.only, by.group.unique, by = 'chem.group')
by.group.only <- merge(by.group.only, by.group.total, by = 'chem.group')
by.group.only$CompoundDetectionFrequency <- by.group.only$CountOfUniqueCompoundsDetected/by.group.only$CountOfCompoundsAnalyzed*100
#write.xlsx(by.group.only,'//deqlead01/wqm/toxics_2012/data/r/Data_Summary_DRAFT.xlsx',sheetName='UniqueCompoundsByGroup',row.names = FALSE,append = TRUE)

#If you are satisfied with all the outputs as they are and have saved them to the excel file. You can run this line to clear your
#workspace and move ahead with running 03a and 03b which only require data.wo.void as an input.
#rm(list = setdiff(ls(), c('data.wo.void')))

#if you inlcude all the data we have
#by.group.only.all <- arrange(ddply(data.wo.void, .(chem.group), summarise, sum = sum(Detect.nondetect)),desc(sum))
#by.group.only.all <- by.group.only.all[!is.na(by.group.only.all$chem.group) & by.group.only.all$chem.group != 'NA',]

#look for the compounds consistently detected across the state
#123 total stations sampled in this dataset as of 2014-01-06
#detects.only <- dwn.sub[dwn.sub$Detect.nondetect == 1,]

#by.analyte <- ddply(detects.only, .(Analyte), function (x) {length(unique(x$SampleRegID))})
#by.analyte <- rename(by.analyte, c('V1' = 'CountofStations'))
#by.analyte <- arrange(by.analyte, desc(CountofStations))

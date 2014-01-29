#This file evaluates all the metals against their respective criteria including hardness based calculations. 

source('//deqlead01/wqm/TOXICS_2012/Data/R/criteria.R')
source('//deqlead01/wqm/TOXICS_2012/Data/R/hardness_eval_functions_Element_Names.R')

#pull out just the metals data
metals <- data.wo.void[data.wo.void$SpecificMethod %in% c('EPA 200.8', '200.8'),]

#separates the parts of the name to make it easier to apply the hardness based criteria
name.split <- strsplit(metals$Analyte, split = ', ')

name.split <- data.frame(matrix(unlist(name.split), nrow=length(name.split), byrow=T))

name.split <- rename(name.split, c('X1' = 'Name.alone', 'X2' = 'PARAMETER_MODIFIER_ABBREVIATION'))

metals <- cbind(metals, name.split)

metals$ID <- paste(metals$SampleRegID, metals$Sampled, metals$PARAMETER_MODIFIER_ABBREVIATION)

#bring in just the metals criteria
metals.criteria <- read.xlsx('//deqlead01/wqm/TOXICS_2012/Data/Criteria_benchmarks_etc/Metals_Table30_Table40.xlsx', sheetName = 'Sheet1')

#change NAME to Analyte for consistency with Element
metals.criteria <- rename(metals.criteria, c('NAME' = 'Analyte'))

#match the metals with the criteria
metals.w.criteria <- merge(metals, metals.criteria, by = 'Analyte', all.x = TRUE)

#doing the hardness evaluation
hm.evaluated <- data.frame()
for (j in 1:length(unique(constants$Name.alone))) {
  tmp <- hardnessEval(metal = unique(constants$Name.alone)[j], df = metals)
  tmp <- as.data.frame(lapply(tmp, as.character), stringsAsFactors = FALSE)
  ifelse(tmp == "", hm.evaluated <- hm.evaluated, ifelse(j == 1, hm.evaluated <- tmp, hm.evaluated <- rbind(hm.evaluated, tmp)))
}

#merge the hardness criteria into the metals.w.criteria table
metals.w.criteria$ID <- paste(metals.w.criteria$ID, metals.w.criteria$Analyte)
hm.evaluated$ID <- paste(hm.evaluated$ID, hm.evaluated$Analyte)
metals.w.criteria.hm <- merge(metals.w.criteria, hm.evaluated[,c('ID','tResulthardness','Table30Acute','Table30Chronic')], by = 'ID', suffixes = c('OG','HM'), all.x = TRUE)

metals.w.criteria.hm$Table30Acute <- ifelse(metals.w.criteria.hm$Table30AcuteOG == 'hardness',
                                            metals.w.criteria.hm$Table30AcuteHM,
                                            metals.w.criteria.hm$Table30AcuteOG)

metals.w.criteria.hm$Table30Chronic <- ifelse(metals.w.criteria.hm$Table30ChronicOG == 'hardness',
                                              metals.w.criteria.hm$Table30ChronicHM,
                                              metals.w.criteria.hm$Table30ChronicOG)

metals.w.criteria.hm <- within(metals.w.criteria.hm, rm('Table30AcuteOG','Table30AcuteHM','Table30ChronicOG','Table30ChronicHM'))

#Now the rest
metals.w.criteria.hm$Table30Acute.Exceed <- ifelse(metals.w.criteria.hm$tResult > suppressWarnings(as.numeric(metals.w.criteria.hm$Table30Acute)), 1, 0)
metals.w.criteria.hm$Table30Acute.Magnitude <- metals.w.criteria.hm$tResult/suppressWarnings(as.numeric(metals.w.criteria.hm$Table30Acute))

metals.w.criteria.hm$Table30Chronic.Exceed <- ifelse(metals.w.criteria.hm$tResult > suppressWarnings(as.numeric(metals.w.criteria.hm$Table30Chronic)), 1, 0)
metals.w.criteria.hm$Table30Chronic.Magnitude <- metals.w.criteria.hm$tResult/suppressWarnings(as.numeric(metals.w.criteria.hm$Table30Chronic))

metals.w.criteria.hm$Table40Water.Organism.Exceed <- ifelse(metals.w.criteria.hm$tResult > suppressWarnings(as.numeric(metals.w.criteria.hm$Table40Water.Organism)), 1, 0)
metals.w.criteria.hm$Table40Water.Organism.Magnitude <- metals.w.criteria.hm$tResult/suppressWarnings(as.numeric(metals.w.criteria.hm$Table40Water.Organism))

metals.w.criteria.hm$Table40OrganismOnly.Exceed <- ifelse(metals.w.criteria.hm$tResult > suppressWarnings(as.numeric(metals.w.criteria.hm$Table40OrganismOnly)), 1, 0)
metals.w.criteria.hm$Table40OrganismOnly.Magnitude <- metals.w.criteria.hm$tResult/suppressWarnings(as.numeric(metals.w.criteria.hm$Table40OrganismOnly))

metals.w.criteria.hm.sub <- metals.w.criteria.hm[,c('Project','SampleRegID','SampleAlias','Sampled','Analyte','tResult','Unit',
                                                    'tResulthardness','tMRL','Detect.nondetect','Table30Acute','Table30Acute.Exceed','Table30Acute.Magnitude',
                                                    'Table30Chronic','Table30Chronic.Exceed','Table30Chronic.Magnitude','Table40Water.Organism','Table40Water.Organism.Exceed',
                                                    'Table40Water.Organism.Magnitude','Table40OrganismOnly','Table40OrganismOnly.Exceed','Table40OrganismOnly.Magnitude')]

intermediate <- melt(metals.w.criteria.hm, id.vars = c('ID','Project','SampleRegID','SampleAlias','Sampled','Analyte','tResult','Unit','tResulthardness','tMRL',
                                                       'Detect.nondetect'), 
                     measure.vars = c('Table30Acute.Exceed','Table30Chronic.Exceed','Table40Water.Organism.Exceed','Table40OrganismOnly.Exceed'))

roll.up <- ddply(metals.w.criteria.hm.sub, .(Project,SampleRegID,SampleAlias,Analyte), summarise, 
                 Table30Acute.Exceed = sum(Table30Acute.Exceed), 
                 Table30Chronic.Exceed = sum(Table30Chronic.Exceed), 
                 Table40Water.Organism.Exceed = sum(Table40Water.Organism.Exceed),
                 Table40OrganismOnly.Exceed = sum(Table40OrganismOnly.Exceed),
                 sample.count = length(Detect.nondetect), 
                 detect.count = sum(Detect.nondetect, na.rm = TRUE))
roll.up$percent.detect <- round(100*(roll.up$detect.count/roll.up$sample.count))

#metals.exceed.summary <- (dcast(intermediate, formula = Project + SampleRegID + SampleAlias + Analyte ~ variable, fun.aggregate = sum) )
#metals.exceed.summary.n <- (dcast(intermediate, formula = Project + SampleRegID + SampleAlias + Analyte ~ variable, fun.aggregate = length) )
gc()
#write.xlsx2(metals.w.criteria.hm.sub, '//deqlead01/wqm/toxics_2012/data/analysis_through_2012/MetalsAnalysisThrough2012_10252013.xlsx',
#           sheetName='Data')
gc()
#write.xlsx2(roll.up, '//deqlead01/wqm/toxics_2012/data/analysis_through_2012/MetalsAnalysisThrough2012_10252013.xlsx',
#           sheetName='ExceedanceSummary',append = TRUE)

#If you are satisfied with the output from this file you can run the next line to clear out your workspace and move on
#to 3b for the analysis of the rest of the parameters
#rm(list = setdiff(ls(), 'data.wo.void'))
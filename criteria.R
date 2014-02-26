require(plyr)
require(reshape2)
require(stringr)
options('scipen' = 100)

criteria <- read.csv('//deqlead01/wqm/TOXICS_2012/Data/Criteria_benchmarks_etc/Criteria.csv', stringsAsFactors = FALSE)

criteria$Pollutant <- str_trim(criteria$Pollutant)

criteria.values <- within(criteria, rm('Notes','No.', 'index', 'Sort', 'minimum.criteria.benchmark.value','source','alias','CAS.No.',
                                       'Carcinogen','Fish.SV.175.g.d..ppb..','Fish.SV.17.5.g.d..ppb.','Fish.SV.32.g.d..ppb.',
                                       'X', 'OR.MCLs',
                                       'Acute.One.Day.HHBP..ppb.','Chronic..Lifetime.HHBP..ppb.'))
criteria.values <- criteria.values[!duplicated(criteria.values$Pollutant),]
criteria.values <- rename(criteria.values , replace = c('Human.Health.Criteria...................Water...Organism..ug.L.'= 'Table 40 Human Health Criteria for Toxic Pollutants - Water + Organism',
                                                        'Human.Health.Criteria..Organism.Only..ug.L.' = 'Table 40 Human Health Criteria for Toxic Pollutants - Organism Only',
                                                        'Freshwater.Acute.Criteria..CMC..ug.L' = 'Table 30 Toxic Substances - Freshwater Acute',
                                                        'Freshwater.Chronic.Criteria..CCC..ug.L' = 'Table 30 Toxic Substances - Freshwater Chronic',
                                                        "Freshwater.Fish.Acute.1" = 'OPP Aquatic Life Benchmarks - Acute Fish',
                                                        "Freshwater.Fish.Chronic.2" = 'OPP Aquatic Life Benchmarks - Chronic Fish',
                                                        "Freshwater.Invertebrates.Acute.3" = 'OPP Aquatic Life Benchmarks - Acute Invertebrates',
                                                        'Freshwater.Invertebrates.Chronic.4' = 'OPP Aquatic Life Benchmarks - Chronic Invertebrates',
                                                        "Freshwater.Nonvascular.Plants.Acute.5" = 'OPP Aquatic Life Benchmarks - Acute Nonvascular Plants',
                                                        "Freshwater.Vascular.Plants.Acute.6" = 'OPP Aquatic Life Benchmarks - Acute Vascular Plants',
                                                        "Office.of.Water.Aquatic.Life.Criteria.Maximum.Concentration..CMC." = 'Office of Water Aquatic Life Criteria - Maximum Concentration (CMC)',
                                                        "Office.of.Water.Aquatic.Life.Criteria.Continuous.Concentration..CCC." = 'Office of Water Aquatic Life Criteria - Continuous Concentration (CCC)',
                                                        "Marine.Acute.Criteria..CMC..ug.L" = 'Table 30 Toxic Substances - Saltwater Acute',
                                                        "Marine.Chronic.Criteria..CCC..ug.L" = 'Table 30 Toxic Substances - Saltwater Chronic'))
rownames(criteria.values) <- criteria.values[,1]
criteria.values.melted <- melt(criteria.values, id.vars = 'Pollutant')
criteria.values.melted$Matrix <- ifelse(criteria.values.melted$variable %in% c('Table 30 Toxic Substances - Saltwater Acute','Table 30 Toxic Substances - Saltwater Chronic'),
                                      'SW','FW')
t40oo <- criteria.values.melted[criteria.values.melted$variable == 'Table 40 Human Health Criteria for Toxic Pollutants - Organism Only',]
t40oo$Matrix <- 'SW'
criteria.values.melted <- rbind(criteria.values.melted, t40oo)
criteria.values.melted$ID <- paste(criteria.values.melted$Pollutant, criteria.values.melted$Matrix)
criteria.values.melted[criteria.values.melted$ID == 'Arsenic, Total inorganic SW' & criteria.values.melted$variable == 'Table 40 Human Health Criteria for Toxic Pollutants - Organism Only','value'] <- 1
criteria.values.melted[criteria.values.melted$ID == 'Arsenic, Total recoverable SW' & criteria.values.melted$variable == 'Table 40 Human Health Criteria for Toxic Pollutants - Organism Only','value'] <- 1
criteria.values.melted[criteria.values.melted$ID == 'Manganese, Total recoverable SW' & criteria.values.melted$variable == 'Table 40 Human Health Criteria for Toxic Pollutants - Organism Only','value'] <- 100
hardness.pollutants <- criteria.values.melted[criteria.values.melted$value == 'hardness',]
criteria.values.melted$value <- suppressWarnings(as.numeric(criteria.values.melted$value))

criteria.values.melted.applicable <- criteria.values.melted[!is.na(criteria.values.melted$value),]
criteria.values.melted.applicable <- criteria.values.melted.applicable[criteria.values.melted.applicable$value != 0,]

min.criteria.values <- ddply(criteria.values.melted, .(Pollutant), function(m) {
  m <- m[m$value != 0,]
  if (all(is.na(m[m$variable %in% c('Table 40 Human Health Criteria for Toxic Pollutants - Water + Organism', 
                                    'Table 40 Human Health Criteria for Toxic Pollutants - Organism Only', 
                                    'Table 30 Toxic Substances - Freshwater Acute', 
                                    'Table 30 Toxic Substances - Freshwater Chronic'),'value']))) {
    i = which(m$value == min(m$value,na.rm = T))
  } else {
    m <- m[m$variable %in% c('Table 40 Human Health Criteria for Toxic Pollutants - Water + Organism', 
                             'Table 40 Human Health Criteria for Toxic Pollutants - Organism Only', 
                             'Table 30 Toxic Substances - Freshwater Acute', 
                             'Table 30 Toxic Substances - Freshwater Chronic'),]
    i = which(m$value == min(m$value,na.rm = T))
  }
  return (m[i,])
})
min.criteria.values <- min.criteria.values[!duplicated(min.criteria.values$Pollutant),]
min.criteria.values$variable <- factor(min.criteria.values$variable)

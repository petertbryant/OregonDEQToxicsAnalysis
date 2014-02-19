library(plyr)

#establish the constants to use in the criteria calculations and associate them with
constants <- data.frame('Name.alone' = c('Cadmium', 'Copper', 'Cadmium', 'Chromium', 'Lead', 'Nickel', 'Silver', 'Zinc'),
                        'Analyte' = c('Cadmium, Total recoverable', 'Copper, Total recoverable', 'Cadmium, Dissolved', 'Chromium, Dissolved', 
                                   'Lead, Dissolved', 'Nickel, Dissolved', 'Silver, Dissolved', 'Zinc, Dissolved'),
                        'mA' = c(1.128, 0.9422, NA, 0.8190, 1.273, 0.8460, 1.72, 0.8473),
                        'bA' = c(-3.828, -1.464, NA, 3.7256, -1.460, 2.255, -6.59, 0.884),
                        'mC' = c(NA, 0.8545, 0.7409, 0.8190, 1.273, 0.8460, NA, 0.8473),
                        'bC' = c(NA, -1.465, -4.719, 0.6848, -4.705, 0.0584, NA, 0.884),
                        'CFA' = c(1, 1, 1, 0.316, 1, 0.998, 0.85, 0.978),
                        'CFC' = c(1, 1, 1, 0.860, 1, 0.997, 0.85, 0.986))

hardness.crit.calc <- function(df, remove.chromium = TRUE) {
  if(remove.chromium == FALSE) {
    constants <- constants[constants$Name.alone != 'Chromium',]
  }
  
  df$ID <- paste(df$SampleRegID, df$Sampled, substr(df$Analyte, nchar(df$Analyte), nchar(df$Analyte)))
  
  metals <- df[df$Analyte %in% constants$Analyte,]
  
  hardness <- df[grep('Hardness',df$Analyte),c('ID','Analyte','tResult')]
  
  mh <- merge(metals, hardness, by = 'ID', suffixes = c('.metal','.hardness'),all.x = TRUE)
  
  mh$tResult.hardness <- as.numeric(mh$tResult.hardness)
  mh$tResult.hardness <- ifelse(mh$tResult.hardness < 25, 
                                25, 
                                ifelse(mh$tResult.hardness > 400, 
                                       400, 
                                       mh$tResult.hardness))
  
  mhc <- merge(mh, constants, by.x = 'Analyte.metal', by.y = 'Analyte', all.x = TRUE)
  
  for (i in 1:nrow(mhc)) {
    if(mhc$Analyte.metal[i] == 'Cadmium, Dissolved') {
      mhc$CFC[i] <- 1.101672-(log(mhc$tResult.hardness[i])*(0.041838))
    } else if (mhc$Analyte.metal[i] == 'Lead, Dissolved') {
      mhc$CFA[i] <- 1.46203-(log(mhc$tResult.hardness[i])*(0.145712))
      mhc$CFC[i] <- 1.46203-(log(mhc$tResult.hardness[i])*(0.145712))
    }
  }
  
  mhc$'Table 30 Toxic Substances - Freshwater Acute' <- exp(mhc$mA*(log(mhc$tResult.hardness)) + mhc$bA) * mhc$CFA
  mhc$'Table 30 Toxic Substances - Freshwater Chronic' <- exp(mhc$mC*(log(mhc$tResult.hardness)) + mhc$bC) * mhc$CFC
  
  mhc.melted <- melt(mhc, measure.vars = c('Table 30 Toxic Substances - Freshwater Acute', 'Table 30 Toxic Substances - Freshwater Chronic'))
  
  mhcm <- mhc.melted[!is.na(mhc.melted$value),]
  
  mhcm <- rename(mhcm, c('Analyte.metal' = 'Analyte', 'tResult.metal' = 'tResult', 'Type' = 'Type.x'))
  
  mhcm$Type.y <- 'FW'
  
  mhcm$Pollutant <- mhcm$Analyte
  
  return(mhcm)
}


#the general case -- This requires two input dataframes, a detect dataframe and a constants dataframe. the detect dataframe must have
#the detect columns     Name.full: A concatentation of Total Recoverable or Dissolved and the metal name with no space in between
#                            Analyte: The metal name alone
#                             tResult: A clean numeric Result field in micrograms/L
#                              ID: A concatenation of the STATION and SAMPLE_DATE fields
#the constants columns Name.alone: The metal name alone (equivalent to Analyte in the detect dataframe)
#                       Name.full: The metal name preceded by Total Recoverable or Dissolved
#                          mA, bA: The coefficients for acute criteria calculation
#                          mC, bC: The coefficients for chronic criteria calculation
#                        CFA, CFC: The conversion factor for converting between total and dissolved criteria
#The option remove.chromium removes the Chromium III hardness dependent criteria. If you want to include it, set it to FALSE.
hardnessEval <- function(metal, df, remove.chromium = TRUE){
  if(remove.chromium == FALSE) {
    constants <- constants[constants$Name.alone != 'Chromium',]
  }
  
  #name.split <- strsplit(df$Analyte, split = ', ')
  
  #name.split <- data.frame(matrix(unlist(name.split), nrow=length(name.split), byrow=T))
  
  #name.split <- rename(name.split, c('X1' = 'Name.alone', 'X2' = 'PARAMETER_MODIFIER_ABBREVIATION'))
  
  #df <- cbind(df, name.split)
  
  total.string <- paste(metal,'Total recoverable',sep=', ')
  dissolved.string <- paste(metal,'Dissolved',sep=', ')
  
  #metal <- detect[detect$Name.full %in% c(total.string, dissolved.string),]
  metal.df <- df[df$Analyte %in% c(total.string, dissolved.string),]
  
  if (nrow(metal.df) == 0) {
    return('')
  } else {
    metal.df <- merge(metal.df, constants, by = 'Analyte', all.x = TRUE)
    
    #metal.h <- detect[detect$ID %in% metal$ID & detect$Name.full == "DissolvedHardness as CaCO3",c('ID','tResult')]
    metal.h <- df[df$ID %in% metal.df$ID & df$Name.alone == "Hardness as CaCO3",c('ID','tResult','PARAMETER_MODIFIER_ABBREVIATION')]
    metal.h$tResult <- as.numeric(metal.h$tResult)
    metal.h$tResult <- ifelse(metal.h$tResult < 25, 25, metal.h$tResult)
    metal.h$tResult <- ifelse(metal.h$tResult > 400, 400, metal.h$tResult)
        
    metal.df.h <- merge(metal.df, metal.h, by = 'ID', suffixes = c('metal','hardness'), all.x = TRUE)
    
    metal.df.h$PARAMETER_MODIFIER_ABBREVIATION <- metal.df.h$PARAMETER_MODIFIER_ABBREVIATIONmetal
    
    metal.df.h$tResulthardness <- as.numeric(metal.df.h$tResulthardness)
    
    for (i in 1:nrow(metal.df.h)) {
      if (metal.df.h$PARAMETER_MODIFIER_ABBREVIATION[i] == 'Dissolved') {
        if (metal.df.h$Name.alone.x[i] == 'Cadmium') {
          metal.df.h$CFC[i] <- 1.101672-(log(metal.df.h$tResulthardness[i])*(0.041838))
        } else if (metal.df.h$Name.alone.x[i] == 'Lead') {
          metal.df.h$CFA[i] <- 1.46203-(log(metal.df.h$tResulthardness[i])*(0.145712))
          metal.df.h$CFC[i] <- 1.46203-(log(metal.df.h$tResulthardness[i])*(0.145712))
        }
      }
    }
    
    metal.df.h$Table30Acute <- exp(metal.df.h$mA*(log(metal.df.h$tResulthardness)) + metal.df.h$bA) * metal.df.h$CFA
    metal.df.h$Table30Chronic <- exp(metal.df.h$mC*(log(metal.df.h$tResulthardness)) + metal.df.h$bC) * metal.df.h$CFC
        
    metal.df.h$tResultmetal <- as.numeric(metal.df.h$tResultmetal)
    
    metal.df.h$Table30Acute.Exceed <- ifelse(metal.df.h$tResultmetal > metal.df.h$Table30Acute, 1, 0)
    metal.df.h$Table30Acute.Magnitude <- metal.df.h$tResultmetal/metal.df.h$Table30Acute
    
    metal.df.h$Table30Chronic.Exceed <- ifelse(metal.df.h$tResultmetal > metal.df.h$Table30Chronic, 1, 0)
    metal.df.h$Table30Chronic.Magnitude <- metal.df.h$tResultmetal/metal.df.h$Table30Chronic
    
    return(metal.df.h)
  }
  
}

pentachlorophenol.crit.calc <- function(df) {

  df$ID <- paste(df$SampleRegID, df$Sampled)
  
  penta <- df[df$Analyte == 'Pentachlorophenol',]
  
  ph <- df[df$Analyte == 'pH',c('ID','Analyte','tResult')]
  
  pp <- merge(penta, ph, by = 'ID', suffixes = c('.penta','.ph'),all.x = TRUE)
  
  pp$tResult.ph <- as.numeric(pp$tResult.ph)
  
  pp$'Table 30 Toxic Substances - Freshwater Acute' <- exp(1.005*(pp$tResult.ph)-4.869)
  pp$'Table 30 Toxic Substances - Freshwater Chronic' <- exp(1.005*(pp$tResult.ph)-5.134)
  
  pp.melted <- melt(pp, measure.vars = c('Table 30 Toxic Substances - Freshwater Acute', 'Table 30 Toxic Substances - Freshwater Chronic'))
  
  ppm <- pp.melted[!is.na(pp.melted$value),]
  
  ppm <- rename(ppm, c('Analyte.penta' = 'Analyte', 'tResult.penta' = 'tResult', 'Type' = 'Type.x'))
  
  ppm$Type.y <- 'FW'
  
  ppm$Pollutant <- ppm$Analyte
  
  return(ppm)
}
library(plyr)

#establish the constants to use in the criteria calculations and associate them with
constants <- data.frame('Name.alone' = c('Cadmium', 'Copper', 'Cadmium', 'Chromium', 'Lead', 'Nickel', 'Silver', 'Zinc'),
                        'Name.full' = c('Total RecoverableCadmium', 'Total RecoverableCopper', 'DissolvedCadmium', 'DissolvedChromium', 
                                   'DissolvedLead', 'DissolvedNickel', 'DissolvedSilver', 'DissolvedZinc'),
                        'mA' = c(1.128, 0.9422, NA, 0.8190, 1.273, 0.8460, 1.72, 0.8473),
                        'bA' = c(-3.828, -1.464, NA, 3.7256, -1.460, 2.255, -6.59, 0.884),
                        'mC' = c(NA, 0.8545, 0.7409, 0.8190, 1.273, 0.8460, NA, 0.8473),
                        'bC' = c(NA, -1.465, -4.719, 0.6848, -4.705, 0.0584, NA, 0.884),
                        'CFA' = c(1, 1, 1, 0.316, 1, 0.998, 0.85, 0.978),
                        'CFC' = c(1, 1, 1, 0.860, 1, 0.997, 0.85, 0.986))

#the general case -- This requires two input dataframes, a detect dataframe and a constants dataframe. the detect dataframe must have
#the detect columns     Name.full: A concatentation of Total Recoverable or Dissolved and the metal name with no space in between
#                            NAME: The metal name alone
#                             PRV: A clean numeric Result field in micrograms/L
#                              ID: A concatenation of the STATION and SAMPLE_DATE fields
#the constants columns Name.alone: The metal name alone (equivalent to NAME in the detect dataframe)
#                       Name.full: The metal name preceded by Total Recoverable or Dissolved
#                          mA, bA: The coefficients for acute criteria calculation
#                          mC, bC: The coefficients for chronic criteria calculation
#                        CFA, CFC: The conversion factor for converting between total and dissolved criteria
hardnessEval <- function(metal, df){
  total.string <- paste('Total Recoverable',metal,sep='')
  dissolved.string <- paste('Dissolved',metal,sep='')
  
  #metal <- detect[detect$Name.full %in% c(total.string, dissolved.string),]
  metal <- df[df$Name.full %in% c(total.string, dissolved.string),]
  
  if (nrow(metal) == 0) {
    return('')
  } else {
    metal <- merge(metal, constants, by = 'Name.full', all.x = TRUE)
    
    #metal.h <- detect[detect$ID %in% metal$ID & detect$Name.full == "DissolvedHardness as CaCO3",c('ID','PRV')]
    metal.h <- df[df$ID %in% metal$ID & df$Name.full == "DissolvedHardness as CaCO3",c('ID','PRV')]
    metal.h$PRV <- as.numeric(metal.h$PRV)
    metal.h$PRV <- ifelse(metal.h$PRV < 25, 25, metal.h$PRV)
    metal.h$PRV <- ifelse(metal.h$PRV > 400, 400, metal.h$PRV)
    
    metal <- merge(metal, metal.h, by = 'ID', suffixes = c('metal','hardness'))
    
    metal$PRVhardness <- as.numeric(metal$PRVhardness)
    
    for (i in 1:nrow(metal)) {
      if (metal$PARAMETER_MODIFIER_ABBREVIATION[i] == 'Dissolved') {
        if (metal$NAME[i] == 'Cadmium') {
          metal$CFC[i] <- 1.101672-(log(metal$PRVhardness[i])*(0.041838))
        } else if (metal$NAME[i] == 'Lead') {
          metal$CFA[i] <- 1.46203-(log(metal$PRVhardness[i])*(0.145712))
          metal$CFC[i] <- 1.46203-(log(metal$PRVhardness[i])*(0.145712))
        }
      }
    }
    
    metal$crit.acute <- exp(metal$mA*(log(metal$PRVhardness)) + metal$bA) * metal$CFA
    metal$crit.chronic <- exp(metal$mC*(log(metal$PRVhardness)) + metal$bC) * metal$CFC
        
    metal$PRVmetal <- as.numeric(metal$PRVmetal)
    metal$exceed.acute <- ifelse(metal$PRVmetal > metal$crit.acute, 1, 0)
    metal$exceed.chronic <- ifelse(metal$PRVmetal > metal$crit.chronic, 1, 0)
    
    return(metal)
  }
  
}
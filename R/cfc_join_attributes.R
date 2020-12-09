library(tidyverse)
#fol_fc <- neonUtilities::loadByProduct("DP1.10026.001", check.size=F,site='all')
cfc_fieldData = fol_fc$cfc_fieldData
cfc_traits = fol_fc$cfc_carbonNitrogen
cfc_chlorophyll = fol_fc$cfc_chlorophyll
cfc_LMA = fol_fc$cfc_LMA
cfc_elem = fol_fc$cfc_elements
cfc_lig = fol_fc$cfc_lignin

cfc_fieldData = readr::read_delim("~/Documents/Data/Traits/CFC/archive/cfc_field_L0.txt", delim = "\t")
cfc_traits = readr::read_delim("~/Documents/Data/Traits/CFC/archive/cfc_CN_L0.txt", delim = "\t")
cfc_chlorophyll = readr::read_delim("~/Documents/Data/Traits/CFC/archive/cfc_chlorophyll_L0.txt", delim = "\t")
cfc_LMA = readr::read_delim("~/Documents/Data/Traits/CFC/archive/cfc_LMA_L0.txt", delim = "\t")
cfc_lig = readr::read_delim("~/Documents/Data/Traits/CFC/archive/cfc_lignin_L0.txt", delim = "\t")
cfc_elem = readr::read_delim("~/Documents/Data/Traits/CFC/archive/cfc_elements_level0.txt", delim = "\t")

#cfc_chlorophyll = fol_fc$cfc_chlorophyll
#cfc_LMA = fol_fc$cfc_LMA
cfc_chlorophyll = cfc_chlorophyll %>% mutate(chlA_perc = extractChlAConc * solventVolume / freshMass,
                                           chlB_perc = extractChlBConc * solventVolume / freshMass,
                                           carot_perc = extractCarotConc * solventVolume / freshMass)

cfc_chlorophyll = cfc_chlorophyll %>% select(c("chlorophyllSampleID", "chlorophyllSampleCode", "chlA_perc", "chlB_perc", "carot_perc", 
                                               "extractChlAConc", "extractChlBConc", "extractCarotConc",  "solventVolume", "freshMass"))
cfc_fieldData = cfc_fieldData %>% select(individualID, taxonID, plotID, subplotID, sampleID, sampleCode, collectDate, plantStatus, individualFate, 
                                         subsample1Height, subsample2Height, subsample3Height, chlorophyllSampleID, chlorophyllSampleFate, chlorophyllSampleCode)
all <- cfc_fieldData %>%
  inner_join(cfc_LMA, by = c("plotID","collectDate", "sampleID", "sampleCode"))
all = all %>% select(individualID, sampleID, plotID, dataEntryLatitude, dataEntryLongitude, dataEntryAltitude, sampleCode, 
             chlorophyllSampleID, chlorophyllSampleCode, taxonID, plantStatus, 
             lmaSampleID, lmaSampleCode,lmaSampleCondition, dryMass, freshMass, leafArea, dryMassFraction, percentGreen, subsample1Height)
colnames(all)[16] = "lmaFreshMass"
all =inner_join(all, cfc_chlorophyll, by = c("chlorophyllSampleID", "chlorophyllSampleCode")) %>%
  mutate(chlA_perc_dw = chlA_perc / dryMassFraction * .0001,
         chlB_perc_dw = chlB_perc / dryMassFraction * .0001,
         carot_perc_dw = carot_perc/ dryMassFraction * .0001)





# cfc_traits = cfc_traits %>% select(c("nitrogenPercent", "carbonPercent", "CNratio","d15N", "d13C",
#                                    "collectDate", "sampleID", "sampleCode"))
# cfc_lig = cfc_lig %>% select("domainID","siteID","plotID","plotType","sampleID","sampleCode","dryMass","ligninPercent","cellulosePercent",  "ligninSampleID","ligninSampleBarcode")
# colnames(cfc_lig)[7] = "ligninDryMass"
# 
# cfc_elem = cfc_elem %>% select("domainID","siteID","plotID","plotType","sampleID","sampleCode", "ligninSampleID","ligninSampleBarcode", "dryMass",
#                                "foliarPhosphorusConc", "foliarPotassiumConc",  "foliarCalciumConc","foliarMagnesiumConc",  "foliarSulfurConc",
#                                "foliarManganeseConc",  "foliarIronConc","foliarCopperConc","foliarBoronConc","foliarZincConc")
# colnames(cfc_elem)[9]="elemDryMass"
# 
# all =  inner_join(all, cfc_traits)
# all =  inner_join(all, cfc_lig)
# #all =  inner_join(all, cfc_elem)
# 
# all %>% summary
write_csv(all, "~/Documents/Data/Traits/CFC/pigments.csv")

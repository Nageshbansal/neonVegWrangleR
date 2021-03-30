library(tidyverse)
fol_fc <- neonUtilities::loadByProduct("DP1.10026.001", check.size=F,site='all')
cfc_fieldData = fol_fc$cfc_fieldData
cfc_traits = fol_fc$cfc_carbonNitrogen
cfc_chlorophyll = fol_fc$cfc_chlorophyll
cfc_LMA = fol_fc$cfc_LMA
cfc_elem = fol_fc$cfc_elements
cfc_lig = fol_fc$cfc_lignin

# cfc_fieldData = readr::read_delim("~/Documents/Data/Traits/CFC/archive/cfc_field_L0.txt", delim = "\t")
# cfc_traits = readr::read_delim("~/Documents/Data/Traits/CFC/archive/cfc_CN_L0.txt", delim = "\t")
# cfc_chlorophyll = readr::read_delim("~/Documents/Data/Traits/CFC/archive/cfc_chlorophyll_L0.txt", delim = "\t")
# cfc_LMA = readr::read_delim("~/Documents/Data/Traits/CFC/archive/cfc_LMA_L0.txt", delim = "\t")
# cfc_lig = readr::read_delim("~/Documents/Data/Traits/CFC/archive/cfc_lignin_L0.txt", delim = "\t")
# cfc_elem = readr::read_delim("~/Documents/Data/Traits/CFC/archive/cfc_elements_level0.txt", delim = "\t")

#cfc_chlorophyll = fol_fc$cfc_chlorophyll
#cfc_LMA = fol_fc$cfc_LMA
cfc_chlorophyll = cfc_chlorophyll %>% mutate(chlA_perc = extractChlAConc * solventVolume / freshMass,
                                           chlB_perc = extractChlBConc * solventVolume / freshMass,
                                           carot_perc = extractCarotConc * solventVolume / freshMass)

cfc_chlorophyll = cfc_chlorophyll %>% select(c("chlorophyllSampleID", "chlorophyllSampleCode", "chlA_perc", "chlB_perc", "carot_perc",
                                               "extractChlAConc", "extractChlBConc", "extractCarotConc",  "solventVolume", "freshMass"))

cfc_fieldData = cfc_fieldData %>% select(individualID, taxonID, plotID, subplotID, sampleID, sampleCode, collectDate, plantStatus,
                                         subsample1Height, subsample2Height, subsample3Height, chlorophyllSampleID, chlorophyllSampleCode)
all <- cfc_fieldData %>%
  inner_join(cfc_LMA, by = c("collectDate", "sampleID", "sampleCode"))
all = all %>% select(individualID, sampleID, plotID, sampleCode,
             chlorophyllSampleID, chlorophyllSampleCode, taxonID, plantStatus,
             lmaSampleID, lmaSampleCode,lmaSampleCondition, dryMass, freshMass, leafArea, dryMassFraction,leafMassPerArea, percentGreen, subsample1Height)
colnames(all)[colnames(all) == "freshMass"] = "lmaFreshMass"
all =inner_join(all, cfc_chlorophyll, by = c("chlorophyllSampleID", "chlorophyllSampleCode")) %>%
  mutate(chlA_perc_dw = chlA_perc / dryMassFraction * .0001,
         chlB_perc_dw = chlB_perc / dryMassFraction * .0001,
         carot_perc_dw = carot_perc/ dryMassFraction * .0001)

cfc_traits = cfc_traits %>% select(c("nitrogenPercent", "carbonPercent", "CNratio","d15N", "d13C",
                                   "collectDate", "sampleID", "sampleCode"))
cfc_lig = cfc_lig %>% select("domainID","siteID","plotID","plotType","sampleID","sampleCode","dryMass","ligninPercent","cellulosePercent",  "ligninSampleID","ligninSampleBarcode")
colnames(cfc_lig)[colnames(cfc_lig)=="dryMass"] = "ligninDryMass"

cfc_elem = cfc_elem %>% select("domainID","siteID","plotID","plotType","sampleID","sampleCode", "ligninSampleID","ligninSampleBarcode", "dryMass", "digestVolume",
                               "foliarPhosphorusConc", "foliarPotassiumConc",  "foliarCalciumConc","foliarMagnesiumConc",  "foliarSulfurConc",
                               "foliarManganeseConc",  "foliarIronConc","foliarCopperConc","foliarBoronConc","foliarZincConc")

cfc_elem = cfc_elem %>% mutate(foliarPotassium_perc = foliarPotassiumConc  * digestVolume / dryMass,
                               foliarPhosphorus_perc = foliarPhosphorusConc * digestVolume / dryMass,
                               foliarCalcium_perc = foliarCalciumConc * digestVolume / dryMass,
                               foliarMagnesium_perc = foliarMagnesiumConc * digestVolume / dryMass,
                               foliarSulfur_perc = foliarSulfurConc * digestVolume / dryMass,
                               foliarManganese_perc = foliarManganeseConc * digestVolume / dryMass,
                               foliarCopper_perc = foliarCopperConc * digestVolume / dryMass,
                               foliarBoron_perc = foliarBoronConc * digestVolume / dryMass,
                               foliarZinc_perc = foliarZincConc * digestVolume / dryMass)

colnames(cfc_elem)[colnames(cfc_elem)=="dryMass"]="elemDryMass"

all =  inner_join(all, cfc_traits)
all =  inner_join(all, cfc_lig)
all =  inner_join(all, cfc_elem)
all = unique(all)
all %>% summary

vst = readr::read_csv("./outdir/field_data/neon_vst_data_022021.csv")
vst = vst %>% select(-one_of( "plotID", "taxonID", "plantStatus", "domainID", "siteID", "plotType")) %>%
  group_by(individualID) %>% slice(1)
all = left_join(all, vst)
write_csv(all, "~/Documents/Data/2021_neon_traits.csv")


plots = all %>% filter(!is.na(itcEasting)) %>%
                         select(plotID, subplotID, siteID, utmZone, itcEasting, itcNorthing, collectDate)
plots$collectDate = substr(plots$collectDate, 1,4)
plots = plots %>% group_by(plotID, subplotID, siteID, utmZone, collectDate) %>%
  summarize_all(mean, na.rm=T)

write_csv(plots, "/Volumes/Data/NEON_RS/cfc_plots.csv")

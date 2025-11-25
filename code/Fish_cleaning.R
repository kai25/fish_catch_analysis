#_____________Cleaning and processing fish data___________-----
#Calling data file
#Removing unused columns
#Transpose from long to wide and back to long again to get 0 observations
#Transpose to wide again to biomass and density are separate columns

library(readxl)
library(magrittr)
library(dplyr)
library(tidyr)


Fish_abundance_Raw <- read_excel("1. Raw data/Fish_abundance_Raw_2020.xlsx")


#Adding biomass and density columns standardized to  kg/ha and fish/ha respectively

Fish_abundance_Raw <- Fish_abundance_Raw %>% mutate(Biomass = (tot_wt_g*40)/1000,
                                                    Density = Number *40)



#Remove some columns not required at this stage
Fish_abundance_Raw %<>%
  select(-Date,
        -Number,
        -Size_class,
         -L,
         -a,
         -b,
         -weight_g,
         -tot_wt_g)


#Summarise the selected variables and sum by transects

Fish_abundance_Raw %<>% 
  group_by(Location, Sites,Family,Species,Trophic_groups,Trophic_groups2, Transect_No)  %>% 
  summarise(Biomass = Biomass %>% sum(na.rm = TRUE), Density = Density %>% 
              sum(na.rm = TRUE))


#Convert from long to wide

Fish_abundance_Raw %<>%
  tidyr::pivot_wider(
    names_from  = c(Transect_No), 
    values_from = c(Biomass, Density))

#Replace NAs with 0
Fish_abundance_Raw[is.na(Fish_abundance_Raw)] <- 0


#Convert from wide to long
Fish_abundance_Raw %<>% 
  gather(Metric, Value, Biomass_1:Density_5, factor_key=TRUE)


Fish_abundance_Raw %<>%
  mutate(Transect_No = ifelse(Metric == "Biomass_1", "Transect_1",
                       ifelse(Metric == "Biomass_2", "Transect_2",
                       ifelse(Metric == "Biomass_3", "Transect_3",
                       ifelse(Metric == "Biomass_4", "Transect_4",
                       ifelse(Metric == "Biomass_5","Transect_5",
                       ifelse(Metric == "Density_1", "Transect_1",
                       ifelse(Metric == "Density_2", "Transect_2",
                       ifelse(Metric == "Density_3", "Transect_3",
                       ifelse(Metric == "Density_4", "Transect_4",
                       ifelse(Metric == "Density_5","Transect_5",
                        "Density")))))))))))

Fish_abundance_Raw %<>%
  mutate(Metric = ifelse(Metric == "Biomass_1", "Biomass",
                  ifelse(Metric == "Biomass_2", "Biomass",
                  ifelse(Metric == "Biomass_3", "Biomass",
                  ifelse(Metric == "Biomass_4", "Biomass",
                  ifelse(Metric == "Biomass_5","Biomass",
                  "Density"))))))


Fish_abundance_clean <- Fish_abundance_Raw %<>%
  tidyr::pivot_wider(
    names_from  = c(Metric), 
    values_from = c(Value))



write.csv(Fish_abundance_clean, '2. Pre-processing/Fish/Cleaning/Fish_abundance_clean.csv', row.names=T)

#save as rda

save(Fish_abundance_clean,
     file = paste0("2. Pre-processing/Fish/Cleaning/Fish_abundance_clean.rda"))

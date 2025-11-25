## Script Descriptions -------------------------------------------------- 

###  Name:         Script_to_clean_Tanzania_Fish_abundance_raw_data_2024.R
###
### Objective:    This script outlines the process of cleaning and processing raw fish abundance data. 
###                
###               
### Approach:     1. Calling data files
###               2. Station column 
###               3. Adding some columns from Geo file 
###               4. Cleaning species names               
###               5. Adding biomass and density columns   
###               6. Removing some unneccessary columns
###               8. Final columns formatting for output file
###.              9. QAQC step
###               10.Saving the output files
###               
###
### Output files: 1. Object saved to *.rda. - Next script shall use this to produce summaries
###               2. File saved to *.csv - Save clean replicate dataset
###
### Authors:      Abigail Mwalimu & Diana Karan
###
### Date:         08 May 2024
###

###  Notes:


##Read data file ---------------------------------------------------------

Fish_abundance_Raw_2024 <- read.csv("2024/1. Raw_data/Fish_abundance_raw_2024.csv")

#Read metadata file
Metadata<-read.csv("2024/1. Raw_data/Metadata_2024.csv")


Geo_file<- read.csv(file = "../../Template files/Overall_Geo-file_Updated.csv",
                    header = T,stringsAsFactors = F)

Master_species <- read.csv("../../Template files/Fish/Master_Reef_Fish_Species.csv")

# spp_master<- read.csv(file = "../../Template files/Fish/Fish_spp_masterfile.csv",
#                     header = T,stringsAsFactors = F)


##Site standardisation and correction -----------------------------------

## Check for unmatched station names
h<-match(Fish_abundance_Raw_2024$Station,Metadata$Station_code,nomatch = 0)
unique(Fish_abundance_Raw_2024$Station[h==0])

#Renaming Station to station.code
names(Fish_abundance_Raw_2024)[names(Fish_abundance_Raw_2024)=="Station"]<-"Station_code"

Fish_abundance_Raw_2024$Station<-Metadata$Station[match(Fish_abundance_Raw_2024$Station_code,Metadata$Station_code)]


## Adding some columns from geo file file --------------------------------

# Check that the station columns between the data and geo file match

Fish_abundance_Raw_2024$Location<-Geo_file$Proposed_Location[match(Fish_abundance_Raw_2024$Station,Geo_file$GCRMN_Station)] #Add location

Fish_abundance_Raw_2024$Sector<-Geo_file$Proposed_Sector[match(Fish_abundance_Raw_2024$Station,Geo_file$GCRMN_Station)] #Add sector

Fish_abundance_Raw_2024$Country<-Geo_file$Country[match(Fish_abundance_Raw_2024$Station,Geo_file$GCRMN_Station)] #Add Country

Fish_abundance_Raw_2024$Latitude<-Geo_file$Latitude[match(Fish_abundance_Raw_2024$Station,Geo_file$GCRMN_Station)] #Add Latitude

Fish_abundance_Raw_2024$Longitude<-Geo_file$Longitude[match(Fish_abundance_Raw_2024$Station,Geo_file$GCRMN_Station)] #Add Longitude

Fish_abundance_Raw_2024$Site<-Geo_file$Proposed_Site[match(Fish_abundance_Raw_2024$Station,Geo_file$GCRMN_Station)] # Correct site naming

Fish_abundance_Raw_2024$Management_level<-Geo_file$Management_level[match(Fish_abundance_Raw_2024$Station,Geo_file$GCRMN_Station)] # Management level

Fish_abundance_Raw_2024$Management_type<-Geo_file$Management_type[match(Fish_abundance_Raw_2024$Station,Geo_file$GCRMN_Station)] # Management type

Fish_abundance_Raw_2024$Reef_type<-Geo_file$Reef_type[match(Fish_abundance_Raw_2024$Station,Geo_file$GCRMN_Station)] # Reef type

Fish_abundance_Raw_2024$Year<-Geo_file$Year[match(Fish_abundance_Raw_2024$Station,Geo_file$GCRMN_Station)] # Add Year


# Standardizing Trophic group column from the Master_species file

Fish_abundance_Raw_2024$Trophic_groups<-Master_species$Trophic_Group[match(Fish_abundance_Raw_2024$Species,Master_species$Species)]


# ## Clean species name ----------------------------------------------------

## Check for unmatched station names
m<-match(Fish_abundance_Raw_2024$Species,Master_species$Species,nomatch = 0)
unique(Fish_abundance_Raw_2024$Species[m==0])


#Adding biomass and density columns standardized to  kg/ha and fish/ha respectively -----------------------------------
# Change columns from character to numeric formats

Fish_abundance_Raw_2024$weight_g <- as.numeric(as.character(Fish_abundance_Raw_2024$weight_g)) 
Fish_abundance_Raw_2024$total_wt_g <- as.numeric(as.character(Fish_abundance_Raw_2024$total_wt_g)) 

# Replace NAs values with 0's 

Fish_abundance_Raw_2024[is.na(Fish_abundance_Raw_2024)]<-0

Fish_abundance_Raw_2024 <- Fish_abundance_Raw_2024 %>% mutate(Biomass = (total_wt_g*40)/1000,
                                                    Density = Number *40)


# Removing some columns not required at this stage --------------------------
Fish_abundance_Raw_2024 %<>%
  select(-Number,
        -Size_class,
         -L,
         -a,
         -b,
         -weight_g,
         -total_wt_g)


# Variables standardization -----------------------------------------------

#Summarize the selected variables and sum by transects
 
 Fish_abundance_Raw_2024 %<>% 
     group_by(Site,Location,Family,Species,Trophic_groups, Transect_No)  %>% 
     mutate(Biomass = Biomass %>% sum(na.rm = TRUE), Density = Density %>% 
                sum(na.rm = TRUE))

## Delete duplicate data

Fish_abundance_Raw_2024<-Fish_abundance_Raw_2024 [!duplicated(Fish_abundance_Raw_2024[
  c("Location","Site","Family","Species","Trophic_groups","Transect_No")]),]


# Ensure spp names are consistence at each station

spp_cat<-Fish_abundance_Raw_2024 %>%
  group_by(Location,Site,Station_code,Transect_No) %>%
  summarise(n=length(Species))

range(spp_cat$n) # if they are not equal then need to format the dataframe

# Subset the data

master_file<-Fish_abundance_Raw_2024[,5:7]

Fish_abundance_Raw_2024a<-Fish_abundance_Raw_2024[,-c(5,7)]

# convert to long for Density and Biomass

Fish_abundance_Raw_2024a<-gather(Fish_abundance_Raw_2024a, Indicator, Values, Biomass:Density)

Fish_abundance_Raw_2024a<-spread(Fish_abundance_Raw_2024a, Species, Values)

Fish_abundance_Raw_2024b<-gather(Fish_abundance_Raw_2024a, Species, Abundance, 16:ncol(Fish_abundance_Raw_2024a))

Fish_abundance_Raw_2024b$Abundance[is.na(Fish_abundance_Raw_2024b$Abundance)]<-0

Fish_abundance_Raw_2024c<- spread(Fish_abundance_Raw_2024b, Indicator, Abundance)

# Test again

spp_cat2<-Fish_abundance_Raw_2024c %>%
  group_by(Location,Site,Station_code,Transect_No) %>%
  summarise(n=length(Species))

range(spp_cat2$n) # if they are not equal then need to format the dataframe

# # Formatting - ensure standard final columns  -----------------------

# Add other column - family & trophic group

Fish_abundance_Raw_2024c$Family<-master_file$Family[match(Fish_abundance_Raw_2024c$Species,master_file$Species)]
Fish_abundance_Raw_2024c$Trophic_groups<-master_file$Trophic_groups[match(Fish_abundance_Raw_2024c$Species,master_file$Species)]

# Order rows

Fish_abundance_Raw_2024 <- Fish_abundance_Raw_2024c%>%arrange(Site,Station)

# Order columns
Fish_abundance_clean_2024<-Fish_abundance_Raw_2024

colnames(Fish_abundance_clean_2024)

Fish_abundance_clean_2024<-Fish_abundance_clean_2024[,c("Date","Year","Country","Sector","Location","Site","Station","Reef_type","Management_level","Management_type","Latitude","Longitude","Family","Species",
                                              "Trophic_groups","Transect_No","Biomass","Density")]
# QAQC step ---------------------------------------------------------------

# pick a site in the input file (raw data) and cross check with the ouput file if the biomass and density equal to manually calculated value
# for example Popo Island, Popo Island_02 Transect 5 for Siganus sutor density is 160 in output file. Cross checking in the input file
# Number = 4,  and 4*40 = 160. 
# This implies that our steps above to obtain the output file "Fish abundance clean dataset for 2024" are correct.

# Also need to check that there are the correct number of transects for each survey
test<-Fish_abundance_clean_2024 %>% 
  group_by(Sector, Location, Site, Station, Year) %>% 
  reframe(transects= Transect_No %>% unique() %>% list(),
          n = Transect_No %>% max())

test_site<-Fish_abundance_clean_2024 %>% 
  group_by(Sector, Location, Site, Year) %>% 
  reframe(transects= Transect_No %>% unique() %>% list(),
          n = Transect_No %>% max(),
          n2= Transect_No %>% unique() %>% length(),
          diff = n2 - n)

 # need to confirm with Clare about Mwamba Nyama number of transects surveyed (2 or 3?)

# # Save outputs  ---------------------------------------------------------

# write csv
write.csv(Fish_abundance_clean_2024,paste("2024/2. Clean/Fish/Fish_abundance_clean.csv",sep=""),row.names = F)

#save as rda
save(Fish_abundance_clean_2024,
     file = paste0("2024/2. Clean/Fish/Fish_abundance_clean.rda"))


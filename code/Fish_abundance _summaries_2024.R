## Script Descriptions -----------------------------------------------------

##  Name:       Producing_Tanzania_fish_data_summaries_2024.R

##  Objective:  This script details all the steps required to process the summary for the fish abundance data 2024 

##  Author:      Diana Karan
##
##  Date:        17th May 2024
##

##Notes
#1.how to deal with NA on the se & sd 
#2.trophic groups2 family not on initial dataset should it be ommitted or added 
  # - to confirm with CT/MS about the file to use in adding the Trophic groups2 column



# Load data file ---------------------------------------------------------------

##load file
load(file = "2024/2. Clean/Fish/Fish_abundance_clean.rda")

# #remove na for flavissimus in trophic groups- 
    #  - I have added this step in the cleaning script using the master species file so need to remove
# Fish_abundance_clean_2024$Trophic_groups[Fish_abundance_clean_2024$Species == "Forcipiger flavissimus" & Fish_abundance_clean_2024$Trophic_groups == "#N/A"] <- "Corallivores"

# 1. Overall summary ------------------------------------------------------
#compute total abundance and biomass
# Grouping and summarizing fish abundance data by site
Fish_Abund_site_24 <- Fish_abundance_clean_2024 %>% 
  group_by(Sector, 
           Location, 
           Site, 
           Transect_No) %>%
  mutate(
    Total_biomass = sum(Biomass),#calculate Biomass
    Total_density = sum(Density) #calculate density
  )

# Remove duplicate rows from the data 

Fish_Abund_site_24<-Fish_Abund_site_24 [!duplicated(Fish_Abund_site_24[
  c("Sector","Location","Site","Total_biomass","Total_density")]),] # Delete duplicate data

##Trophic group remove NA


### Compute mean, standard deviation, standard error, and no of transects/site
# Compute mean, standard deviation, standard error, and number of transects/site
Fish_Abund_site_mean_24 <- Fish_Abund_site_24%>%
  group_by(Sector, Location, Site) %>%
  mutate(
    Mean_biomass = mean(Total_biomass, na.rm = TRUE),
    sd_b = sd(Total_biomass),
    se_b = sd_b / sqrt(n()),  # Calculate standard error using n()
    Mean_density = mean(Total_density, na.rm = TRUE),
    sd_d = sd(Total_density),
    se_d = sd_d / sqrt(n()),  # Calculate standard error using n()
    number_transect = max(Transect_No)
  )
###Remove duplicate rows
Fish_Abund_site_mean_24<-Fish_Abund_site_mean_24[!duplicated(Fish_Abund_site_mean_24[
  c("Sector","Location","Site","Mean_biomass","Mean_density",
    "number_transect","sd_b","sd_d","se_b","se_d")]),]  # Delete duplicate data

colnames(Fish_Abund_site_mean_24)
###Order columns
Fish_Abund_site_mean_24<-Fish_Abund_site_mean_24[,c("Date","Year","Country",
                                                              "Sector","Location",
                                                             "Site","Management_level",
                                                             "Latitude","Longitude",
                                                             "number_transect",
                                                              "Mean_biomass","sd_b","se_b",
                                                             "Mean_density","sd_d","se_d")] # order columns 


## Save intermediate file
save(Fish_Abund_site_mean_24,
     file = paste0("2024/3. Summarise/Fish/Fishabundance_site_overall_mean_2024.rda"))

## Save csv object
write.csv(Fish_Abund_site_mean_24, "2024/3. Summarise/Fish/Fishabundance_site_overall_mean_2024.csv", row.names=F)



# 3. Location overall summary ---------------------------------------------
#mean biomass, standard deviation, standard error,and number of sites/location
Fish_Abund_Loc_mean_24 <- Fish_Abund_site_24%>%
  group_by(Location) %>%
  mutate(
    Mean_biomass = mean(Total_biomass, na.rm = TRUE),
    sd_b = sd(Total_biomass),
    se_b = sd_b / sqrt(n()),  # Standard error of the mean
    Mean_density = mean(Total_density, na.rm = TRUE),
    sd_d = sd(Total_density),
    se_d = sd_d / sqrt(n()),  # Standard error of the mean
    number_site = n_distinct(Site)  # Number of unique sites
  )
##Remove duplicate rows
Fish_Abund_Loc_mean_24<-Fish_Abund_Loc_mean_24[!duplicated(Fish_Abund_Loc_mean_24[
  c("Location","Mean_biomass","sd_b","se_b","Mean_density","sd_d","se_d","number_site")]),]  # Delete duplicate data


Fish_Abund_Loc_mean_24$Site<- "Overall"

# Order columns

Fish_Abund_Loc_mean_24<-Fish_Abund_Loc_mean_24[,c("Date","Year","Country","Sector","Location","Site","Management_level","Latitude","Longitude","number_site",
                                                                      "Mean_biomass","sd_b","se_b","Mean_density","sd_d","se_d")]
## Save intermediate file
save(Fish_Abund_Loc_mean_24,
     file = paste0("2024/3. Summarise/Fish/Fishabundance_Location_overall_mean_2024.rda"))

## Save csv object
write.csv(Fish_Abund_Loc_mean_24, "2024/3. Summarise/Fish/Fishabundance_Location_overall_mean_2024.csv", row.names=F)


# 4. Site family overall summary -------------------------------------------------
Fish_Abund_sum_site_family_24 <- Fish_abundance_clean_2024 %>% 
  group_by(Location, Site,Family, Transect_No) %>% 
  mutate(Total_biomass = sum(Biomass), Total_density = sum(Density))

Fish_Abund_sum_site_family_24<-Fish_Abund_sum_site_family_24[!duplicated(Fish_Abund_sum_site_family_24[
  c("Location","Site","Family","Transect_No","Total_biomass","Total_density")]),] # Delete duplicate data

#mean biomass,mean density and number of transect

Fish_Abund_sum_site_mean_family_24 <- Fish_Abund_sum_site_family_24 %>%
  group_by(Location, Site, Family) %>% 
  # Add new columns with summary statistics for biomass and density
  mutate(
    Mean_biomass = mean(Total_biomass, na.rm = TRUE),  # mean biomass
    sd_b = sd(Total_biomass),                           # standard deviation of biomass
    se_b = std.error(Total_biomass),                    # standard error of biomass
    Mean_density = mean(Total_density, na.rm = TRUE),   # mean density
    sd_d = sd(Total_density),                           # standard deviation of density
    se_d = std.error(Total_density),                    # standard error of density
    number_transect = max(Transect_No) #maximum transect number for each group
  )

#### Remove duplicate data
Fish_Abund_sum_site_mean_family_24 <- Fish_Abund_sum_site_mean_family_24[
  !duplicated(Fish_Abund_sum_site_mean_family_24[c("Location", "Site", "Family", "Mean_biomass", "Mean_density", 
                                               "sd_b", "sd_d", "se_b", "se_d")]), 
]

###order
Fish_Abund_sum_site_mean_family_24<-Fish_Abund_sum_site_mean_family_24[,c("Date","Year","Country","Location","Site","Management_level","Latitude","Longitude","number_transect",
                                                                            "Family","Mean_biomass","sd_b","se_b","Mean_density","sd_d","se_d")] # order columns 

## Save intermediate file
save(Fish_Abund_sum_site_mean_family_24,
     file = paste0("2024/3. Summarise/Fish/Fishabundance_Site_families_mean_2024.rda"))

## Save csv object
write.csv(Fish_Abund_sum_site_mean_family_24, "2024/3. Summarise/Fish/Fishabundance_Site_families_mean_2024.csv", row.names=F)



# 5. Location family summary ----------------------------------------------

# Compute summary statistics for fish abundance by location and family
Fish_Abund_Loc_mean_family_2024 <- Fish_Abund_sum_site_family_24 %>%
  group_by(Location, Family) %>%
  mutate(
    Mean_biomass = mean(Total_biomass, na.rm = TRUE),  # Calculate mean biomass
    sd_b = sd(Total_biomass),       # standard deviation of biomass
    se_b = std.error(Total_biomass),                    # standard error of biomass
    Mean_density = mean(Total_density, na.rm = TRUE),   #  mean density
    sd_d = sd(Total_density),                           #  standard deviation of density
    se_d = std.error(Total_density),                    #  standard error of density
    number_transect = max(Transect_No)                  # maximum transect number
  )

Fish_Abund_Loc_mean_family_2024_trial <- Fish_Abund_sum_site_family_24 %>%
  group_by(Location, Family) %>% 
  mutate(
    Mean_biomass = round(mean(Total_density), 2),
    sd_b = round(sd(Total_density), 2),
    se_b = round(std.error(Total_density), 2),
    Mean_density = round(mean(Total_density), 2),
    sd_d = round(sd(Total_density), 2),
    se_d = round(std.error(Total_density), 2),  
    number_transect = max(Transect_No)
  )
##duplicate

Fish_Abund_Loc_mean_family_2024<-Fish_Abund_Loc_mean_family_2024 [!duplicated(Fish_Abund_Loc_mean_family_2024[
  c("Location","Family","Mean_biomass","Mean_density","number_transect","sd_b","sd_d","se_b","se_d")]),]  # Delete duplicate data

Fish_Abund_Loc_mean_family_2024$Site<- "Overall"
##order
Fish_Abund_Loc_mean_family_2024<-Fish_Abund_Loc_mean_family_2024[,c("Date","Year","Country","Location","Site","Management_level","Latitude","Longitude","number_transect",
                                                                                    "Family","Mean_biomass","sd_b","se_b","Mean_density","sd_d","se_d")] # order columns 


## Save intermediate file
save(Fish_Abund_Loc_mean_family_2024,
     file = paste0("2024/3. Summarise/Fish/Fishabundance_Location_families_mean_2024.rda"))

## Save csv object
write.csv(Fish_Abund_Loc_mean_family_2024, "2024/3. Summarise/Fish/Fishabundance_Location_families_mean.csv", row.names=F)



# 6. Summary Indicator families - (Acanthuridae,Chaetodontidae,Epi --------



Indicator_families_2024 <- Fish_Abund_sum_site_mean_family_24[Fish_Abund_sum_site_mean_family_24$Family %in% c("Acanthuridae", "Chaetodontidae", "Epinephilidae", "Scarinae"), ]


## Save intermediate file
save(Indicator_families_2024,
     file = paste0("2024/3. Summarise/Fish/Fishabundance_Site_indicator_families_mean_2024.rda"))

## Save csv object
write.csv(Indicator_families_2024, "2024/3. Summarise/Fish/Fishabundance_Site_indicator_families_mean_2024.csv", row.names=F)




# 7. Trophic groups overall summary ------------------------------------------

# Compute total biomass and density for fish abundance data grouped by location, site, trophic groups, and transect number
Fish_Abund_sum_site_Trophic_groups_24 <- Fish_abundance_clean_2024 %>%
  group_by(Location, Site, Trophic_groups, Transect_No) %>%
  mutate(
    Total_biomass = sum(Biomass),    # Calculate total biomass
    Total_density = sum(Density)     # Calculate total density
  )
## remove duplicate data 
Fish_Abund_sum_site_Trophic_groups_24 <-Fish_Abund_sum_site_Trophic_groups_24 [!duplicated(Fish_Abund_sum_site_Trophic_groups_24[
  c("Location","Site","Trophic_groups","Transect_No","Total_biomass","Total_density")]),] # Delete duplicate data

###Fish_Abund_sum_site_Trophic_groups2_24 <- Fish_abundance_clean_2024 %>% 
  #group_by(Location, Site,Trophic_groups2, Transect_No) %>% 
  #mutate(Total_biomass = sum(Biomass), Total_density = sum(Density))

#Fishabundance_sum_site_Trophic_groups2_24<-Fishabundance_sum_site_Trophic_groups2_24 [!duplicated(Fishabundance_sum_site_Trophic_groups2_2022[
  #c("Location","Site","Trophic_groups2","Transect_No","Total_biomass","Total_density")]),] # Delete duplicate data


# 8. # Site - Trophic 1 ---------------------------------------------------
# Compute mean, standard deviation, and standard error of fish abundance by location, site, and trophic groups
Fish_Abund_sum_site_mean_Trophic_groups_24 <- Fish_Abund_sum_site_Trophic_groups_24 %>%
  group_by(Location, Site, Trophic_groups) %>%
  mutate(
    Mean_biomass = mean(Total_biomass, na.rm = TRUE),  #  mean biomass
    sd_b = sd(Total_biomass),                           # standard deviation of biomass
    se_b = std.error(Total_biomass),                    # standard error of biomass
    Mean_density = mean(Total_density, na.rm = TRUE),   # mean density
    sd_d = sd(Total_density),                           # standard deviation of density
    se_d = std.error(Total_density),                    # standard error of density
    number_transect = max(Transect_No)                  # maximum transect number
  )
###delete duplicate data

Fish_Abund_sum_site_mean_Trophic_groups_24<-Fish_Abund_sum_site_mean_Trophic_groups_24 [!duplicated(Fish_Abund_sum_site_mean_Trophic_groups_24[
  c("Location","Site","Trophic_groups","Mean_biomass","Mean_density","number_transect","sd_b","sd_d","se_b","se_d")]),]  # Delete duplicate data
##order columns
Fish_Abund_sum_site_mean_Trophic_groups_24<-Fish_Abund_sum_site_mean_Trophic_groups_24[,c("Date","Year","Country","Location","Site","Management_level","Latitude","Longitude","number_transect",
                                                                                            "Trophic_groups","Mean_biomass","sd_b","se_b","Mean_density","sd_d","se_d")] # order columns
## Save intermediate file
save(Fish_Abund_sum_site_mean_Trophic_groups_24,
     file = paste0("2024/3. Summarise/Fish/Fishabundance_Site_trophic_groups_mean_2024.rda"))

## Save csv object
write.csv(Fish_Abund_sum_site_mean_Trophic_groups_24, "2024/3. Summarise/Fish/Fishabundance_Site_trophic_groups_mean_2024.csv", row.names=F)


# 9. # Location trophic groups summary ----------------------------

Fish_Abund_Loc_mean_trophic_groups_2024 <- Fish_Abund_sum_site_Trophic_groups_24 %>%
  group_by(Location, Trophic_groups) %>%
  mutate(
    Mean_biomass = mean(Total_biomass, na.rm = TRUE),  # Calculate mean biomass
    sd_b = sd(Total_biomass),                           # Calculate standard deviation of biomass
    se_b = std.error(Total_biomass),                    # Calculate standard error of biomass
    Mean_density = mean(Total_density, na.rm = TRUE),   # Calculate mean density
    sd_d = sd(Total_density),                           # Calculate standard deviation of density
    se_d = std.error(Total_density),                    # Calculate standard error of density
    number_transect = max(Transect_No)                  # Determine the maximum transect number
  )
##remove duplicate
Fish_Abund_Loc_mean_trophic_groups_2024<-Fish_Abund_Loc_mean_trophic_groups_2024 [!duplicated(Fish_Abund_Loc_mean_trophic_groups_2024[
  c("Location","Trophic_groups","Mean_biomass","Mean_density","number_transect","sd_b","sd_d","se_b","se_d")]),]  # Delete duplicate data
##re-order
Fish_Abund_Loc_mean_trophic_groups_2024<-Fish_Abund_Loc_mean_trophic_groups_2024[,c("Date","Year","Country","Location","Site","Management_level","Latitude","Longitude","number_transect",
                                                                                                    "Trophic_groups","Mean_biomass","sd_b","se_b","Mean_density","sd_d","se_d")] # order columns

Fish_Abund_Loc_mean_trophic_groups_2024$Site<-"Overall"

## Save intermediate file
save(Fish_Abund_Loc_mean_trophic_groups_2024,
     file = paste0("2024/3. Summarise/Fish/Fishabundance_Location_trophic_groups_mean_2024.rda"))

## Save csv object
write.csv(Fish_Abund_Loc_mean_trophic_groups_2024, "2024/3. Summarise/Fish/Fishabundance_Location_trophic_groups_mean_2024.csv", row.names=F)


# # 10. Site Trophic groups2 summary ----------------------------------------

##Fish_Abund_Site_mean_trophic_groups2_2024 <- Fishabundance_sum_site_Trophic_groups2_2022 %>%
 # group_by(Location, Site,Trophic_groups2) %>% 
  #mutate(Mean_biomass = Total_biomass %>% mean(na.rm = TRUE), sd_b = Total_biomass %>% sd(), se_b = Total_biomass %>% std.error(),
         #Mean_density = Total_density %>% mean(na.rm = TRUE), sd_d = Total_density %>% sd(), se_d = Total_density %>% std.error(),
         #number_transect = max(Transect_No))

###Fishabundance_Site_mean_trophic_groups2_2022<-Fishabundance_Site_mean_trophic_groups2_2022 [!duplicated(Fishabundance_Site_mean_trophic_groups2_2022[
  #c("Location","Site","Trophic_groups2","Mean_biomass","Mean_density","number_transect","sd_b","sd_d","se_b","se_d")]),]  # Delete duplicate data


##Fishabundance_Site_mean_trophic_groups2_2024<-Fishabundance_Site_mean_trophic_groups2_2024[,c("Date","Year","Country","Location","Site","Management_level","Latitude","Longitude","number_transect",
                                                                                             # "Trophic_groups2","Mean_biomass","sd_b","se_b","Mean_density","sd_d","se_d")] # order columns

## Save intermediate file
##save(Fishabundance_Site_mean_trophic_groups2_2024,
     #file = paste0("2022/3. Summarise/Fish/Fishabundance_Site_trophic_groups2_mean_2024.rda"))

## Save csv object
#write.csv(Fishabundance_Site_mean_trophic_groups2_2024, "2024/3. Summarise/Fish/Fishabundance_Site_trophic_groups2_mean_2024.csv", row.names=F)


# # 11. Trophic 2 overall summary -----------------------------------------


#Fishabundance_Location_mean_trophic2_groups_2024 <- Fishabundance_sum_site_Trophic_groups2_2024 %>%
  #group_by(Location, Trophic_groups2) %>% 
  #mutate(Mean_biomass = Total_biomass %>% mean(na.rm = TRUE), sd_b = Total_biomass %>% sd(), se_b = Total_biomass %>% std.error(), 
         #Mean_density = Total_density %>% mean(na.rm = TRUE), sd_d = Total_density %>% sd(), se_d = Total_density %>% std.error(),
         #number_transect = max(Transect_No))

#Fishabundance_Location_mean_trophic2_groups_2024<-Fishabundance_Location_mean_trophic2_groups_2024 [!duplicated(Fishabundance_Location_mean_trophic2_groups_2022[
  #c("Location","Trophic_groups2","Mean_biomass","Mean_density","number_transect","sd_b","sd_d","se_b","se_d")]),]  # Delete duplicate data

#Fishabundance_Location_mean_trophic2_groups_2024$Site<-"Overall"


#Fishabundance_Location_mean_trophic2_groups_2024<-Fishabundance_Location_mean_trophic2_groups_2024[,c("Date","Year","Country","Location","Site","Management_level","Latitude","Longitude","number_transect",
                                                                                                      #"Trophic_groups2","Mean_biomass","sd_b","se_b","Mean_density","sd_d","se_d")] # order columns

## Save intermediate file
#save(Fishabundance_Location_mean_trophic2_groups_2024,
    # file = paste0("2024/3. Summarise/Fish/Fishabundance_Location_trophic_groups2_mean_2022.rda"))

## Save csv object
#write.csv(Fishabundance_Location_mean_trophic2_groups_2022, "2022/3. Summarise/Fish/Fishabundance_Location_trophic_groups2_mean_2022.csv", row.names=F)



# # 12. #Summary Indicator trophic groups (Herbivores- resilience, --------


#Fishabundance_sum_site_Indicator_trophic_groups2_2024 <- Fish_abundance_clean %>% 
  #group_by(Location, Site,Trophic_groups2, Transect_No) %>% 
  #mutate(Total_biomass = sum(Biomass), Total_density = sum(Density))

#Fishabundance_sum_site_Indicator_trophic_groups2_2024<-Fishabundance_sum_site_Indicator_trophic_groups2_2024 [!duplicated(Fishabundance_sum_site_Indicator_trophic_groups2_2022[
 # c("Location","Site","Trophic_groups2","Transect_No","Total_biomass","Total_density")]),]  # Delete duplicate data


#Fishabundance_Site_mean_trophic_groups2_2024 <- Fishabundance_sum_site_Indicator_trophic_groups2_2024 %>%
  #group_by(Location, Site,Trophic_groups2) %>% 
  #mutate(Mean_biomass = Total_biomass %>% mean(na.rm = TRUE), sd_b = Total_biomass %>% sd(),se_b = Total_biomass %>% std.error(), 
         #Mean_density = Total_density %>% mean(na.rm = TRUE), sd_d = Total_density %>% sd(), se_d = Total_density %>% std.error(),
         #number_transect = max(Transect_No))

#Fishabundance_Site_mean_trophic_groups2_2024<-Fishabundance_Site_mean_trophic_groups2_2024 [!duplicated(Fishabundance_Site_mean_trophic_groups2_2024[
  #c("Location","Site","Trophic_groups2","Mean_biomass","Mean_density","number_transect","sd_b","sd_d","se_b","se_d")]),]  # Delete duplicate data

#Fishabundance_Site_mean_trophic_groups2_2024<-Fishabundance_Site_mean_trophic_groups2_2024[,c("Date","Year","Country","Location","Site","Management_level","Latitude","Longitude","number_transect",
                                                                                              #"Trophic_groups2","Mean_biomass","sd_b","se_b","Mean_density","sd_d","se_d")] # order columns

#Indicator_trophicgroups2_2022 <- Fishabundance_Site_mean_trophic_groups2_2022[Fishabundance_Site_mean_trophic_groups2_2022$Trophic_groups2 %in% c("Herbivores","Pisci-Omnivores"),]

## Save intermediate file
#save(Indicator_trophicgroups2_2024,
     #file = paste0("2024/3. Summarise/Fish/Fishabundance_Site_indicator_trophic_groups_mean_2022.rda"))

## Save csv object
#write.csv (Indicator_trophicgroups2_2024, "2024/3. Summarise/Fish/Fishabundance_Site_indicator_trophic_groups_mean_2024.csv", row.names=F)




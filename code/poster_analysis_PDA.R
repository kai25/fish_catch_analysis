#### LIBRARIES ####

require(tidyverse)
require(terra)
require(tidyterra)
require(magrittr)
require(ggforce)
require(data.table)
require(readr)
require(readxl)
require(ggh4x)
require(data.table) 
require(flextable)

#### DATA IMPORT ####

fish.gen = list.files(
  path = "/Users/USER/Documents/fish_catch_analysis/data/checked/gen/",
  pattern = "\\.xlsx$",
  full.names = TRUE) |> 
  map_dfr(~ read_excel(.x, sheet = 1)) |>  
  select(date = "Tarehe",                                                                                               
         global_id = "GlobalID",
         parent_id = "ParentGlobalID",
         data_collector = "Mkusanya takwimu",
         region = "Chagua Mkoa",
         village = "Shehia/BMU",
         lunar_month = "Mwezi na Bamvua",
         landing_site = "Bandari",
         fisher_number = "Namba ya mvuvi",
         fisher_name = "Jina la Mvuvi",
         fisher_age = "Umri wa mvuvi",
         fisher_gender = "Jinsia ya mvuvi",
         number_per_group = "Idadi ya wavuvi kwenye kundi",
         fishing_ground = "Eneo la uvuvi",
         type_of_fishingground = "Aina ya eneo",
         method = "Mbinu ya uvuvi",
         gear = "Zana zinazotumika",
         other_gear = "Andika zana nyingine",
         price = "bei",
         going_time = "Muda wakwenda kuvua",
         return_time = "Muda wakurudi") |>   
  mutate(date = as.Date(date),
         year = lubridate::year(date),
         month = lubridate::month(date, label = TRUE),
         day = lubridate::day(date),
         fishing_ground = as.character(fishing_ground)) 




##### TOTAL CATCH COMPOSITION DATA####

catch.composition = list.files(
  path = "/Users/USER/Documents/fish_catch_analysis/data/checked/gen/",
  pattern = "\\.xlsx$",
  full.names = TRUE) |> 
  map_dfr(~ read_excel(.x, sheet = 2))|>
  select(  global_id = "GlobalID",
           parent_id = "ParentGlobalID",
           creation_date = "CreationDate", 
           scientific_names = "aina ya samaki",
           total_weight_g = "Uzito wa jamii moja ya samaki",
           total_number = "Idadi ya samaki") |>  
  mutate(year_creation = lubridate::year(creation_date)) 


##### SPECIES SPECIFIC LW DATA####

species.specific = list.files(
  path = "/Users/USER/Documents/fish_catch_analysis/data/checked/gen/",
  pattern = "\\.xlsx$",
  full.names = TRUE) |> 
  map_dfr(~ read_excel(.x, sheet = 3))|> 
  select( global_id = "GlobalID",
          parent_id = "ParentGlobalID",
          creation_date = "CreationDate", 
          scientific_names = "Aina ya Samaki",
          weight_g = "Uzito kwa mmoja",
          length_cm = "Urefu kwa mmoja") |>  
  mutate(year_creation = lubridate::year(creation_date)) 



##### FAMILY, GENERAL, TROPHIC GROUP AND SPECIES NAME DATA####

fish.families = read.csv("/Users/USER/Documents/fish_catch_analysis/data/fish_sci_names_families.csv")

# Using data table to fix names

setDT(fish.families)

# Removing duplicates of swahili names

swahili.clean = fish.families[
  , .(swahili_names = paste(unique(swahili_names), collapse = ", ")), 
  by = scientific_names]


family_info_by_species = fish.families[
  , .SD[1], 
  by = scientific_names, 
  .SDcols = c( "family", "common_name", "trophic_groups")
]


# left joining a clean dataset without duplicates

family_info_by_species = swahili.clean[
  family_info_by_species, 
  on = .(scientific_names)
]

# catch_combined = read.csv("../data/Merged data/catch.data_8.25.csv")
# 
# saveRDS(catch_combined, "catch_combined.rds")

#### CLEANING ####

##### Chokocho####

fish.gen %<>% 
  mutate(
    fishing_ground = case_when(
      village == "Chokocho" & fishing_ground %in% c("Fungun", "Funguni  ", " Funguni") ~ "Funguni",
      village == "Chokocho" & fishing_ground %in% c("Mto wapili", "Mtowa pili", "Mtowapili") ~ "Mto wa pili",
      village == "Chokocho" & fishing_ground %in% c("Mtowatatu", "Mto watatu") ~ "Mto wa tatu",
      village == "Chokocho" & fishing_ground %in% c("Vujiwevitatu", "Vijiwevitatu", "Vujiwe vitatu") ~ "Vijiwe vitatu",
      village == "Chokocho" & fishing_ground %in% c("Kwasharif", "Kqasharifu", "Kwashrifu", "Kwasharifu" ) ~ "Kwa sharifu",
      village == "Chokocho" & fishing_ground %in% c("Matumbin", "Matumbuni", "Matumbini", "Matimbini") ~ "Matumbini",
      village == "Chokocho" & fishing_ground %in% c("Ngiwani", "Ngowami", "Ngowano") ~ "Ngowani",
      village == "Chokocho" & fishing_ground %in% c("Mtonyimbi", "Mto nyimbi", "Mtinyimbi", "Mtomyimbi", "Mtonyimbi", "Mto .Nyimbi") ~ "Mto Nyimbi",
      village == "Chokocho" & fishing_ground %in% c("Pandanguli ", "Pandaguli") ~ "Pandanguli",
      village == "Chokocho" & fishing_ground %in% c("Rasin ", "Rasin") ~ "Rasini",
      village == "Chokocho" & fishing_ground %in% c("Mspuku", "Mapukuu") ~ "Mapuku",
      village == "Chokocko" & fishing_ground == "Michechen" ~ "Michecheni",
      village == "Chokocko" & fishing_ground == "Fungu roma" ~ "Roma",
      TRUE ~ fishing_ground))

##### Chongoleani####     
fish.gen %<>%
  mutate(
    fishing_ground = case_when(
      village == "Chongoleani" & fishing_ground %in% c("Kwawa ", "Kwawq", "Kwawa.", "KWAWA", "kwawA", "kwawa") ~ "Kwawa",
      village == "Chongoleani" & fishing_ground %in% c("Mwmbanyama ", "Mwanyanyama", "Mwambanyama", "Mwmbanyama","mwamba nyama","mwambanyama", "mwmbanyama") ~ "Mwamba nyama",
      village == "Chongoleani" & fishing_ground %in% c("Kimii ", "KiMIo", "KIMIO") ~ "Kimio",
      village == "Chongoleani" & fishing_ground %in% c("Wanba", "Wamba", "wamba", "WAMBA") ~ "Wamba",
      village == "Chongoleani" & fishing_ground %in% c("ULENGE", "Ulenge ", " Ulenge") ~ "Ulenge",
      village == "Chongoleani" & fishing_ground %in% c("Ulenge mnarani", "ULENGE MNARANI", "MNARANI ULENGE") ~ "Ulenge mnarani",
      village == "Chongoleani" & fishing_ground %in% c("TAA", "Taa ", " Taa", "taa") ~ "Taa",
      village == "Chongoleani" & fishing_ground %in% c("Fungun", "Funguni  ", " Funguni") ~ "Funguni", 
      village == "Chongoleani" & fishing_ground %in% c("Mnrani", "Mnrani ", "mnarani") ~ "Mnarani",
      village == "Chongoleani" & fishing_ground %in% c( "Kinanzini ", "Kinanzini") ~ "Kinazini",
      village == "Chongoleani" & fishing_ground %in% c( "KIKAANGO", "Kikqango") ~ "Kikaango",
      village == "Chongoleani" & fishing_ground %in% c("MLIMANI", "MLIMANI ") ~ "Mlimani",
      village == "Chongoleani" & fishing_ground %in% c("Gomeni", "GOMENI","MAGOMENI") ~ "Magomeni",
      village == "Chongoleani" & fishing_ground %in% c( "NYULI", "NYULU") ~ "Nyuli",
      village == "Chongoleani" & fishing_ground %in% c( "MAROMBO ", "MAROMBO", "marombo") ~ "Marombo",
      village == "Chongoleani" & fishing_ground == "UFUMA" ~ "Ufuma",
      village == "Chongoleani" & fishing_ground == "KIPWANI" ~ "Kipwani",
      village == "Chongoleani" & fishing_ground == "Mwaduvi" ~ "Kwamaduvi",
      village == "Chongoleani" & fishing_ground == "ningani" ~ "Ningani",
      TRUE ~ fishing_ground))

##### Kangani#####   
fish.gen %<>%
  mutate(
    fishing_ground = case_when(
      village == "Kangani" & fishing_ground %in% c("Baharin", "Bahsrin") ~ "Baharini",
      village == "Kangani" & fishing_ground == "Mkondon" ~ "Mkondoni",
      village == "Kangani" & fishing_ground == "Matumbin" ~ "Matumbini",
      village == "Kangani" & fishing_ground == "chekwe" ~ "Chekwe",
      village == "Kangani" & fishing_ground == "Mnaran" ~ "Mnarani",
      village == "Kangani" & fishing_ground %in% c("Mawarani", "Mawaran") ~ "Mwarani",
      TRUE ~ fishing_ground))

##### Kichalikani####      
fish.gen %<>%
  mutate(
    fishing_ground = case_when(
      village == "Kichalikani" & fishing_ground %in% c("Wambs ", "Wamb", "wamba", "WAMBA", "Wmba", "Wambs", "Jiwe la wamba") ~ "Wamba",
      village == "Kichalikani" & fishing_ground %in% c( "Chund9", "Chndo" ) ~ "Chundo",
      village == "Kichalikani" & fishing_ground %in% c("Uchuei", "Uchuwu") ~ "Uchuwi",
      village == "Kichalikani" & fishing_ground %in% c("Milangoni", "Milsngoni") ~ "Mlangoni",
      village == "Kichalikani" & fishing_ground %in% c( "Maani", "Matsani") ~ "Mataani",
      village == "Kichalikani" & fishing_ground == "Naweni" ~ "Maweni",
      village == "Kichalikani" & fishing_ground == "Kimaa" ~ "Kimaha",
      village == "Kichalikani" & fishing_ground %in% c( "Rssini", ",rasini") ~ "Rasini",
      TRUE ~ fishing_ground))

##### Kisiwa Panza####    
fish.gen %<>%
  mutate(
    fishing_ground = case_when(
      village == "Kisiwa Panza" & fishing_ground %in% c("Nuinzi", "Muinzi", ".uinzi", "Nuinz", "Muinz","Muinzu" ,"Muibz","Muinzi.") ~ "Miuinzi",
      village == "Kisiwa Panza" & fishing_ground %in% c("Nfazi", "Nazi", "Ngazl", "Ngaz","NSngzi", "Zi") ~ "Ngazi",
      village == "Kisiwa Panza" & fishing_ground %in% c("Milangoni", "Mlangon") ~ "Mlangoni",
      village == "Kisiwa Panza" & fishing_ground == "Kiifimbo" ~ "Kifimbo",
      village == "Kisiwa Panza" & fishing_ground %in% c("Kisiwa koongo","Visiwakoongo") ~ "Kisiwa Koongo",
      village == "Kisiwa Panza" & fishing_ground == "Kwajundo" ~ "Kwa Jundo",
      village == "Kisiwa Panza" & fishing_ground == "Kondon" ~ "Kondoni",
      village == "Kisiwa Panza" & fishing_ground == "Kaangale" ~ "Kangale",
      village == "Kisiwa Panza" & fishing_ground == "Mzingan" ~ "Mzingani",
      TRUE ~ fishing_ground))

##### Kiuyu####      
fish.gen %<>%
  mutate(
    fishing_ground = case_when(
      village == "Kiuyu" & fishing_ground %in% c("Sjangaa", "Shangaaa", "Shangaq", "Shangas", "Ahangaa", "Changaa") ~ "Shangaa",
      village == "Kiuyu" & fishing_ground %in% c("Mwachi", "Pwach") ~ "Pwachi",
      village == "Kiuyu" & fishing_ground %in% c("Mdononi", "Mdimon", "Mdomin", "Mdomni","Modomoni","Domoni","Mdomon","Mdonon","Domon", "Mdimoni") ~ "Mdomoni",
      village == "Kiuyu" & fishing_ground == "Mwamba wakwanza" ~ "Mwamba wa kwanza",
      village == "Kiuyu" & fishing_ground == "N'gande" ~ "Ng'ande",
      village == "Kiuyu" & fishing_ground == "Kongoo nchele" ~ "Kongoo mchele",
      village == "Kiuyu" & fishing_ground == "Jamban" ~ "Jambani",
      village == "Kiuyu" & fishing_ground %in% c( "Mwamba watu", "Mwamba watatu") ~ "Mwamba wa tatu",
      village == "Kiuyu" & fishing_ground %in% c("Kogoo", "Kingoo", "Kongooo") ~ "Kongoo",
      TRUE ~ fishing_ground))


##### Kisiwani####      
fish.gen %<>%
  mutate(
    fishing_ground = case_when(
      village == "Kisiwani" & fishing_ground %in% c( "Mkumbu","Mkumbiu", "Mkumbuu,ndagoni") ~ "Mkumbuu",
      village == "Kisiwani" & fishing_ground %in% c("Maguju", "Magujuu", "Mahujju","Mahuju") ~ "Mahujuu",
      village == "Kisiwani" & fishing_ground %in% c("Mithali ", "Mesali") ~ "Misali",
      village == "Kisiwani" & fishing_ground %in% c( "Jamvino", "Javini") ~ "Jamvini",
      village == "Kisiwani" & fishing_ground %in% c("Pandwe", "Pande", "Padwe","Pandewe") ~ "Pangwe",
      village == "Kisiwani" & fishing_ground %in% c("Nakocho", "Mako ho", "Makocho ,funzi", "Makochoo") ~ "Makocho",
      village == "Kisiwani" & fishing_ground %in% c( "Muangwi", "Mungwi", "Muwangwai") ~ "Muwangwi",
      village == "Kisiwani" & fishing_ground == "Dongo" ~ "Dongoni",
      village == "Kisiwani" & fishing_ground %in% c("Mahujui", "Mahjuu","Mahujuuu ,mkumbuu") ~ "Mahujuu",
      village == "Kisiwani" & fishing_ground == "Kwasharifu" ~ "Kwa sharifu",
      TRUE ~ fishing_ground))

##### Kizingani####   

fish.gen %<>%
  mutate(
    fishing_ground = case_when(
      village == "Kizingani" & fishing_ground == "NYULI" ~ "Nyuli",
      village == "Kizingani" & fishing_ground == "Mwamba wamba" ~ "Wamba",
      village == "Kizingani" & fishing_ground %in% c("Mwmba nyama", "MMAMBA NYAMA", "MWAMBA NYAMA","Mwambanyama","MWAMBANYAMA","Mqamba nyama", "Mmwamba nyama") ~ "Mwamba nyama",
      village == "Kizingani" & fishing_ground %in% c("Funguni boma", "FUNGU BOMA") ~ "Fungu boma",
      village == "Kizingani" & fishing_ground %in% c( "BOYA CHUPA", "Boy chupa","Poya chupa","BOYACHUPA") ~ "Boya chupa",
      village == "Kizingani" & fishing_ground %in% c("ULENGE ", "Ulenge ", " Ulenge") ~ "Ulenge",
      village == "Kizingani" & fishing_ground %in% c("Fungun", "FUNGUNI") ~ "Funguni",
      village == "Kizingani" & fishing_ground %in% c("Mnarani ", "MNARANI") ~ "Mnarani",
      TRUE ~ fishing_ground))

##### Kukuu####

fish.gen %<>%
  mutate(
    fishing_ground = case_when(
      village == "Kukuu" & fishing_ground %in% c("Vjjiwe vitatu", "Vijiwe 3") ~ "Vijiwe vitatu",
      village == "Kukuu" & fishing_ground %in% c( "Nyumamji", "Myuma mji", "Nyima mji") ~ "Nyuma mji",
      village == "Kukuu" & fishing_ground %in% c("Nduqni", "Ndiani") ~ "Nduani",
      village == "Kukuu" & fishing_ground == "Cekwe" ~ "Chekwe",
      village == "Kukuu" & fishing_ground == "Muinz" ~ "Muinzi",
      TRUE ~ fishing_ground))

##### Kwale####

fish.gen %<>%
  mutate(
    fishing_ground = case_when(
      village == "Kwale" & fishing_ground %in% c("Jibwe mtu", "Jibwe Ntu", "Jibwemtu", "Jiwe mtu", "J.mtu") ~ "Jiwemtu",
      village == "Kwale" & fishing_ground %in% c(  "Mngogo", "Mgogo") ~ "Mgongo",
      village == "Kwale" & fishing_ground %in% c("Muangwi", "Mwangwi") ~ "Muwangwi",
      village == "Kwale" & fishing_ground == "Makocho" ~ "Makochoo",
      village == "Kwale" & fishing_ground == "Javini" ~ "Jamvini",
      TRUE ~ fishing_ground))


##### Kwale Tanga ####

fish.gen %<>%
  mutate(
    fishing_ground = case_when(
      village == "Kwale Tanga" & fishing_ground %in% c("Kwawa ", "Kwawq", "Kwawa.") ~ "Kwawa",
      village == "Kwale Tanga" & fishing_ground %in% c("Mwamba yama", "Mwamba  Nyanyama", ".wamba nyama","Mwamba nyama","Mwamba Nyamba","Mwamba   Nyama","Mwamba  nyama","Mwamba. Nyama","Mwamba manya","Mwamba Nyana","Mwamba  Nyama","Mwambanyama","Mwamna Nyama","Mwamba Nyam", "Mwamba  Nya","Mwamba" ) ~ "Mwamba nyama",
      village == "Kwale Tanga" & fishing_ground %in% c("Uchaui", "Wambauchui","Uchui","Uchwi") ~ "Uchuwi",
      village == "Kwale Tanga" & fishing_ground %in% c("Wambs", "Wambaa ", "Wambq", "Wamb","Wambaa", "Wamna", "Wamnba") ~ "Wamba",
      village == "Kwale Tanga" & fishing_ground %in% c("ULENGE ", "Ulenge ", "Uienge","Ulenga") ~ "Ulenge",
      village == "Kwale Tanga" & fishing_ground %in% c("Boma", "Funguni boma","Moma", "Bama", "Boms") ~ "Fungu boma",
      village == "Kwale Tanga" & fishing_ground %in% c("Mawemawilu", "Mawenawiki", " Mawemawili","Mawe  mawili","Mawemàwili","Mawemawil","Mawema","Mawe mawil","Mawemawili","Mawe maweli", "Mawemali", "Mawemaili") ~ "Mawe mawili",
      village == "Kwale Tanga" & fishing_ground %in% c("Tawalan", "Tawlani") ~ "Tawalani",
      village == "Kwale Tanga" & fishing_ground == "Kinazina" ~ "Kinazini",
      village == "Kwale Tanga" & fishing_ground == "Mwam,buyu" ~ "Mwambuyu",
      village == "Kwale Tanga" & fishing_ground == "Vitundu Vitatu" ~ "Vitundu  vitatu",
      village == "Kwale Tanga" & fishing_ground == "Mto" ~ "Mtoni",
      TRUE ~ fishing_ground))

##### Mabokweni####

fish.gen %<>%
  mutate(
    fishing_ground = case_when(
      village == "Mabokweni" & fishing_ground %in% c("Mtonj", "Myo", "Mto","Mro","mtoni", "Mtoni" ) ~ "Mto kiongwe",
      village == "Mabokweni " & fishing_ground %in% c("Mikokoni", "MIK") ~ "Mikokotoni",
      village == "Mabokweni " & fishing_ground == "Mchanganyiko,zigi" ~ "Mchanganyiko",
      TRUE ~ fishing_ground))

##### Makoongwe####

fish.gen %<>%
  mutate(
    fishing_ground = case_when( 
      village == "Makoongwe" & fishing_ground %in% c( "Vijiwe 3", "Vijiwe3","Vijiwe vitatu") ~ "Vijiwe Vitatu",
      village == "Makoongwe" & fishing_ground %in% c("Kawta", "Kwat", "Kwsta", "Kwats") ~ "Kwata",
      village == "Makoongwe" & fishing_ground == "Uksi" ~ "Ukai",
      village == "Makoongwe" & fishing_ground == "Matumbin" ~ "Matumbini",
      village == "Makoongwe" & fishing_ground == "Misal" ~ "Misali",
      village == "Makoongwe" & fishing_ground == "K-Mtama" ~ "Kijiwe mtama",
      TRUE ~ fishing_ground))

##### Michenzani####

fish.gen %<>%
  mutate(
    fishing_ground = case_when(
      village == "Michenzani" & fishing_ground %in% c( "Viwe vitatu", "Vijiwe  vitatu", "Vijiqe vitatu","Vijiwe vutatu","Vijiwe vitqtu") ~ "Vijiwe vitatu",
      village == "Michenzani" & fishing_ground %in% c("Jiqe membe", "Jiwe mambe", "Jiwe memba", "Jiwe membe","Jiwe membo","Giwe membe","Jiwe nembe") ~ "Jiwe Membe",
      village == "Michenzani" & fishing_ground %in% c("Kwasharifu", " Kwasharufu","Kwasharufu") ~ "Kwa sharifu",
      village == "Michenzani" & fishing_ground %in% c("Kizambarsuni", "Iizambarauni", "Kizambarauuni","Kizambaruni") ~ "Kizambarauni",
      village == "Michenzani" & fishing_ground %in% c("Nyuma ya fungu", "Nyuma Ya Fungu", "Myuma ya fungu", "Nyuma yq fungu", "Nyuma ya Fungu", "Numa Ya Fungu", "Nyama Ya Fungu", "Nyuma ya  fungu") ~ "Nyuma ya Fungu",
      village == "Michenzani" & fishing_ground == "Kwamwanamauwa" ~ "Kwamwana maua",
      village == "Michenzani" & fishing_ground %in% c("Kipwq kome","Kipwakome", "Kupws kome", "Kipwa Kome") ~ "Kipwa kome" ,
      village == "Michenzani" & fishing_ground == "Kijiqe mtama" ~ "Kijiwe mtama",
      village == "Michenzani" & fishing_ground == "Kipwakidogo" ~ "Kipwa kidogo",
      village == "Michenzani" & fishing_ground == "Nyama matumbini" ~ "Nyuma matumbini",
      village == "Michenzani" & fishing_ground == "Kwats" ~ "Kwata",
      TRUE ~ fishing_ground))

#####Michungwani####

fish.gen %<>%
  mutate(
    fishing_ground = case_when(village == "Michungwani" & fishing_ground %in% c("Dogoni", "Dangoni", "Dongini", "Dongni") ~ "Dongoni",
                               village == "Michungwani" & fishing_ground %in% c("Ukongo mrefu", "Mgongo mrefu") ~ "Ugongo mrefu",
                               village == "Michungwani" & fishing_ground %in% c("Funz", "Fnzi") ~ "Funzi",
                               village == "Michungwani" & fishing_ground == "Pawe papa" ~ "Mawe papa",
                               village == "Michungwani" & fishing_ground == "Kinazni" ~ "Kinazini",
                               village == "Michungwani" & fishing_ground == "Ugongo mrefu" ~ "Mgongo mrefu",
                               village == "Michungwani" & fishing_ground == "Mkubuu" ~ "Fuguni mkumbuu",
                               village == "Michungwani" & fishing_ground == "Iengwe" ~ "Lengwe",
                               village == "Michungwani" & fishing_ground == "Fungu" ~ "Funguni",
                               village == "Michungwani" & fishing_ground == "Kijambni" ~ "Kijambani",
                               village == "Michungwani" & fishing_ground == "PAASI" ~ "Paasi",
                               village == "Michungwani" & fishing_ground == "Ponono" ~ "Pononi",
                               village == "Michungwani" & fishing_ground == "Vijiwevitatu" ~ "Vijiwe vitatu",
                               TRUE ~ fishing_ground))

#####Monga####

fish.gen %<>%
  mutate(
    fishing_ground = case_when(
      village == "Monga Vyeru" & fishing_ground %in% c("MAWE MAWILI", "Mawe mawil", "Mawi Mawili", "Mawe  mawili","Mawemawili","Nawe mawili") ~ "Mawe mawili",
      village == "Monga Vyeru" & fishing_ground == "Kwa mzunuu" ~ "Kwa mzungu",
      village == "Monga Vyeru" & fishing_ground == "CHUNDO" ~ "Chundo",
      village == "Monga Vyeru" & fishing_ground == "Chamba vyery" ~ "chamba vyeru",
      village == "Monga Vyeru" & fishing_ground %in% c("UCHUI","Uchui" ) ~ "Uchuwi",
      village == "Monga Vyeru" & fishing_ground %in% c( "MWAMBANYAMA", "MWAMBA. NYAMA","MWAMBA NYAMA","Mwamba Nyama", "Mwamba  Nyama") ~ "Mwamba nyama",
      village == "Monga Vyeru" & fishing_ground %in% c("Wanba ", "Wamba ", "wamba", "WAMBA","WAMBA", "Wsmba") ~ "Wamba", 
      village == "Monga Vyeru" & fishing_ground %in% c("Miambamitatu ", "Miamba  Mitatu ", " Miamba Mitatu","MIAMBA MITATU","Miambamitatu","Miamba  Mitatu","Miamba Mitatu") ~ "Miamba mitatu",
      village == "Monga Vyeru" & fishing_ground %in% c("FUNGU LA BOMA", "Funguni boma", "Fungu la boma", "FUNGUNI LA BOMA","Boma funguni") ~ "Fungu boma",
      village == "Monga Vyeru" & fishing_ground %in% c("Fungun", "Fununi", "FUNGUNI") ~ "Funguni",
      village == "Monga Vyeru" & fishing_ground == "MATAANI" ~ "Mataani",
      village == "Monga Vyeru" & fishing_ground == "KIMAA" ~ "Kimaa",
      village == "Monga Vyeru" & fishing_ground == "Maboyan" ~ "Maboyani",
      village == "Monga Vyeru" & fishing_ground %in% c("MIAMBA MITANO", "Miamba Mitano","Miamba  mitano", "Miamba  Mitano" ) ~ "Miamba mitano",
      village == "Monga Vyeru" & fishing_ground %in% c("Maweni mtoni", "Maweni/mtoni") ~ "Maweni",
      village == "Monga Vyeru" & fishing_ground == "KITUNGAMWE" ~ "Kitungamwe",
      village == "Monga Vyeru" & fishing_ground == "MWAORE" ~ "Mwaore",
      village == "Monga Vyeru" & fishing_ground == "KINAZINI" ~ "Kinazini",
      village == "Monga Vyeru" & fishing_ground == "Jiqe la wanda" ~ "Jiwe la wanda",
      village == "Monga Vyeru" & fishing_ground == "KIROBA" ~ "Kiroba",
      village == "Monga Vyeru" & fishing_ground == "MILANGONI" ~ "Milangoni",
      village == "Monga Vyeru" & fishing_ground == "Magogoni" ~ "Magongoni",
      village == "Monga Vyeru" & fishing_ground == "Vyongoni" ~ "Viongoni",
      village == "Monga Vyeru" & fishing_ground == "Mlango wa mwaboza" ~ "Milango ya mwaboza",
      TRUE ~ fishing_ground))

#####Mpirani####

fish.gen %<>%
  mutate(
    fishing_ground = case_when(
      village == "Mpirani" & fishing_ground %in% c("Mwambanyama", "Wambanyama") ~ "Mwamba nyama",
      village == "Mpirani" & fishing_ground %in% c("Mnarano", "Marani", "Mnanarani","Mnaran") ~ "Mnarani",
      village == "Mpirani" & fishing_ground %in% c("Kzingani", "Kizingqni","Kizingazi","Kizinani","Kizigani","Ķizigani","Kingani","Kizing,ani","Kizizingani","Kizingan", "Kizinga") ~ "Kizingani",
      village == "Mpirani" & fishing_ground %in% c("Jagwakuu","Jangwakuuya","Jangwakuu") ~ "Jangwa kuu",
      village == "Mpirani" & fishing_ground %in% c("Kmio", "Kimio0") ~ "Kimio",
      village == "Mpirani" & fishing_ground %in% c("Mnyang'","Mnyanga","Mnyng'a","Mnyang,a","Mnyag'a", "Mnyag,a") ~ "Mnyang'a",
      village == "Mpirani" & fishing_ground %in% c("Kironit","Kironi","Krironi","Kirironi","Kiriron") ~ "Kirironi",
      village == "Mpirani" & fishing_ground == "Uzilo" ~ "Uzio",
      TRUE ~ fishing_ground ))

#####Mtambwe####

fish.gen %<>%
  mutate(
    fishing_ground = case_when(
      village == "Mtambwe Kusini" & fishing_ground %in% c( "Kwasharifu", "Kwa Sharifu", "Kwasharifu,", "KWASHARIFU") ~ "Kwa sharifu",
      village == "Mtambwe Kusini" & fishing_ground %in% c( "Vijiwe  Viwili", "VIJIWE VIWILI", "VIJIWE  VIWILI", "Vijiwe  viwili") ~ "Vijiwe  viwili",
      village == "Mtambwe Kusini" & fishing_ground %in% c("PAASI", "Pasi") ~ "Paasi",
      village == "Mtambwe Kusini" & fishing_ground == "Mahujuu65" ~ "Mahujuu",
      village == "Mtambwe Kusini" & fishing_ground %in% c("Kokot", "KOKOTA") ~ "Kokota",
      village == "Mtambwe Kusini" & fishing_ground %in% c("Mpanya", "MAPANYA") ~ "Mapanya",
      village == "Mtambwe Kusini" & fishing_ground %in% c("NJAU", ".NJAU") ~ "Njau",
      village == "Mtambwe Kusini" & fishing_ground == "KADHANI" ~ "Kashani",
      village == "Mtambwe Kusini" & fishing_ground == "UVINJE" ~ "Uvinje",
      village == "Mtambwe Kusini" & fishing_ground == "PANDWE" ~ "Pandwe",
      village == "Mtambwe Kusini" & fishing_ground == "FUNZI" ~ "Funzi",
      village == "Mtambwe Kusini" & fishing_ground == "VIJIWE VITATU" ~ "Vijiwe vitatu",
      village == "Mtambwe Kusini" & fishing_ground == "PEMBE" ~ "Pembe",
      village == "Mtambwe Kusini" & fishing_ground == "KIRETI" ~ "Kireti",
      village == "Mtambwe Kusini" & fishing_ground == "KIONGO" ~ "Kiongo",
      village == "Mtambwe Kusini" & fishing_ground == "KIWANDANI" ~ "Kiwandani",
      TRUE ~ fishing_ground))

#####Mtundani####

fish.gen %<>%
  mutate(
    fishing_ground = case_when(
      village == "Mtundani" & fishing_ground == "Mataan" ~ "Mataani",
      village == "Mtundani" & fishing_ground == "Nyasi  bahari" ~ "Nyasi bahari",
      village == "Mtundani" & fishing_ground == "Bamvua shimo" ~ "Wavu wa shimo",
      TRUE ~ fishing_ground))


#####Mwandusi####

fish.gen %<>%
  mutate(
    fishing_ground = case_when(
      village == "Mwandusi" & fishing_ground == "Mataan" ~ "Mataani",
      village == "Mwandusi" & fishing_ground == "Chamban" ~ "Chambani",
      village == "Mwandusi" & fishing_ground == "Chamba cha mnaz" ~ "Chamba cha mnazini",
      village == "Mwandusi" & fishing_ground == "Kigongon" ~ "Kigongoni",
      village == "Mwandusi" & fishing_ground == "Chamba cha mkumbi" ~ "Chamba cha mnazini",
      village == "Mwandusi" & fishing_ground == "Makoleo" ~ "Koleo",
      village == "Mwandusi" & fishing_ground == "Mwambanyama" ~ "Mwamba nyama",
      village == "Mwandusi" & fishing_ground == "Mnaran" ~ "Mnarani",
      village == "Mwandusi" & fishing_ground == "Milangon" ~ "Milangoni",
      village == "Mwandusi" & fishing_ground == "Maviriz" ~ "Mwavirizi",
      village == "Mwandusi" & fishing_ground == "MKUMBI" ~ "Mkumbi",
      village == "Mwandusi" & fishing_ground %in% c( "Maboyan", "Maboyano", "Maboyab","Mboyani") ~ "Maboyani",
      village == "Mwandusi" & fishing_ground %in% c("Uchwi ", "Uchuw", "Uchuqi", "Uchwi") ~ "Uchuwi",
      village == "Mwandusi" & fishing_ground %in% c("Wamb", "Wsmba", "Qamba", "Wqmba") ~ "Wamba",
      village == "Mwandusi" & fishing_ground %in% c("Funguni boma", "Fungun boma", "Fingu boma") ~ "Fungu boma",
      village == "Mwandusi" & fishing_ground %in% c( "Kidutan", "Jidutani") ~ "Kidutani",
      TRUE ~ fishing_ground))

#####Ndaoya####

fish.gen %<>%
  mutate(
    fishing_ground = case_when(
      village == "Ndaoya" & fishing_ground %in% c("Nyili", ".nyuli", "Nyulu") ~ "Nyuli",
      village == "Ndaoya" & fishing_ground %in% c("Mwamba taa", "Tsa") ~ "Taa",
      village == "Ndaoya" & fishing_ground %in% c("Mwambanyama","M nyama","M nyamam") ~ "Mwamba nyama" ,
      village == "Ndaoya" & fishing_ground == "Ulemge" ~ "Ulenge",
      village == "Ndaoya" & fishing_ground == "Maanda" ~ "Mahanda",
      village == "Ndaoya" & fishing_ground == "Kijamnani" ~ "Kijambani",
      village == "Ndaoya" & fishing_ground %in% c("Kitungu","Litunga","Kitynga","Kutunga","Kitumga","Kitung") ~ "Kitunga" ,
      village == "Ndaoya" & fishing_ground %in% c("Jambe. Rasini", "Ĵambe") ~ "Jambe",
      village == "Ndaoya" & fishing_ground == "Kitinga" ~ "Kitunga",
      TRUE ~ fishing_ground))

#####Putini####

fish.gen %<>%
  mutate(
    fishing_ground = case_when(
      village == "Putini" & fishing_ground %in% c( "Mwabanyama", "Mwambanyama", "MWAMBA NYAMA", "Mwambamyama") ~ "Mwamba nyama",
      village == "Putini" & fishing_ground == "Uchui" ~ "Uchuwi",
      village == "Putini" & fishing_ground %in% c("Njuli","Nyulo","NYULI", "Nyyli", "Nyulk") ~ "Nyuli" ,
      village == "Putini" & fishing_ground %in% c("Roho  yamtu","Roho yamtu") ~ "Roho ya mtu" ,
      village == "Putini" & fishing_ground %in% c("Jutono","Jutloni") ~ "Jutuni" ,
      village == "Putini" & fishing_ground %in% c("Ty", "TAA") ~ "Taa",
      village == "Putini" & fishing_ground == "WAMBA" ~ "Wamba",
      village == "Putini" & fishing_ground == "Jambw" ~ "Jambe",
      village == "Putini" & fishing_ground == "Jutoni." ~ "Jutoni",
      village == "Putini" & fishing_ground == "Hazina" ~ "Azina",
      village == "Putini" & fishing_ground == "UFUMA" ~ "Ufuma",
      village == "Putini" & fishing_ground == "Chatahani" ~ "Chataani",
      village == "Putini" & fishing_ground == "Kwssoka" ~ "Kwasoka",
      village == "Putini" & fishing_ground %in% c("Nambawani", "Namba one") ~ "Namba wani",
      village == "Putini" & fishing_ground %in% c("Hahanda","MAANDA") ~ "Mahanda",
      village == "Putini" & fishing_ground == "Nganyawa i" ~ "Nganyawani",
      TRUE ~ fishing_ground))

#####Shidi####

fish.gen %<>%
  mutate(
    fishing_ground = case_when(
      village == "Shidi" & fishing_ground %in% c("Kwa Shafiru", "Kwa Sharfu", "Kwasharif","Kwasharifu","Kwa sharfu","Kwa shatif","Kwa saharf","Kwa sharif","Kwa Sharifu" , "Kwa sharifu","Kwa Sharif" ,"Kwa sharif","Kwa sharf","Kws sharif" ) ~ "Kwa sharifu",
      village == "Shidi" & fishing_ground %in% c("Mvumon","Mavumoni") ~ "Mvumoni",
      village == "Shidi" & fishing_ground %in% c("Kwaya","Kwat") ~ "Kwata" ,
      village == "Shidi" & fishing_ground == "Mako ongwe" ~ "Makoongwe",
      village == "Shidi" & fishing_ground == "Kiboyani" ~ "Boyani",
      TRUE ~ fishing_ground))

#####Shumba####  

fish.gen %<>%
  mutate(
    fishing_ground = case_when(
      village == "Shumba" & fishing_ground %in% c("Nyuma ya fungu", "Uziqani", "Uziwan") ~ "Uziwani",
      village == "Shumba" & fishing_ground == "Mwmbani" ~ "Mwambani",
      village == "Shumba" & fishing_ground %in% c("Jamban","Jamabni") ~ "Jambani" ,
      village == "Shumba" & fishing_ground == "Mnarano" ~ "Mnarani",
      village == "Shumba" & fishing_ground == "Kongop" ~ "Kongoo",
      TRUE ~ fishing_ground))

#####Stahabu####

fish.gen %<>%
  mutate(
    fishing_ground = case_when( 
      village == "Stahabu" & fishing_ground %in% c("Kwa mwana kondo", "Kwamwana kondo", "Kwamwanakondo","Mwanakondo") ~ "Kwa mwanakondo",
      village == "Stahabu" & fishing_ground == "Vijiwe 3" ~ "Vijiwe vitatu",
      village == "Stahabu" & fishing_ground %in% c("Matumbina","Matumbin") ~ "Matumbini" ,
      village == "Stahabu" & fishing_ground == "Kwasharifu" ~ "Kwa sharifu",
      village == "Stahabu" & fishing_ground == "Fungunu" ~ "Funguni",
      village == "Stahabu" & fishing_ground %in% c("Bwachi parua","Bwachi Parua") ~ "Bwachu parua",
      village == "Stahabu" & fishing_ground %in% c("Kwajundi","Kwajundo") ~ "Kwa jundo",
      village == "Stahabu" & fishing_ground == "Fungu Ndogo" ~ "Fungu ndogo",
      village == "Stahabu" & fishing_ground == "Shikashika" ~ "Shika shika",
      village == "Stahabu" & fishing_ground == "Kipwa chakati" ~ "Kipwa cha kati",
      TRUE ~ fishing_ground))

#####Tawalani####

#####Tumbe####

fish.gen %<>%
  mutate(
    fishing_ground = case_when(
      village == "Tumbe" & fishing_ground == "Raaini" ~ "Rasini",
      village == "Tumbe" & fishing_ground == "IMU" ~ "Imu",
      village == "Tumbe" & fishing_ground == "Fungunu" ~ "Funguni",
      village == "Tumbe" & fishing_ground %in% c("Kongoo ya katii", "Kongoo ya ksti") ~ "Kongoo ya kati",
      village == "Tumbe" & fishing_ground == "Jamba nungwu" ~ "Jamba nungwi",
      TRUE ~ fishing_ground))

#####Ziwani####

fish.gen %<>%
  mutate(
    fishing_ground = case_when(
      village == "Ziwani" & fishing_ground %in% c("Pendwe", "Padwe", "Psndwe") ~ "Pandwe",
      village == "Ziwani" & fishing_ground == "DONGONI" ~ "Dongoni",
      village == "Ziwani" & fishing_ground %in% c("Kokots","Kokoto") ~ "Kokota" ,
      village == "Ziwani" & fishing_ground %in% c("Mkimbuu","Mkubuu", "Mhumbuu") ~ "Mkumbuu" ,
      village == "Ziwani" & fishing_ground == "Pembem" ~ "Pembe",
      village == "Ziwani" & fishing_ground %in% c("Kijamba", "Kijamban") ~ "Kijambani",
      village == "Ziwani" & fishing_ground == "Makoongen" ~ "Makoongeni",
      village == "Ziwani" & fishing_ground %in% c("Mwangwi","Mwangi") ~ "Muwangwi",
      village == "Ziwani" & fishing_ground == "Makocho" ~ "Makochoo",
      TRUE ~ fishing_ground ))

catch.combined = fish.gen |> 
  left_join(catch.composition, by = c("global_id" = "parent_id")) |> 
  select(-global_id, -parent_id, -creation_date, -global_id.y,-data_collector)


catch.combined %<>% 
  left_join(family_info_by_species, by = "scientific_names") 

#write.csv(catch.data, "catch.data_8.25.csv")



catch.combined %<>%
  filter(!fishing_ground %in% c("TASI", "Changu", "Pono", "Pono mchele", "Ngalawa", "Majani bahari", "MIK", "Bodo", "1", "Mwani", "Matumbawe/mwamba", "Tembo janja", "Koana", "Mchanga", "Nyasi  bahari", "Nyasi bahari", "Mchangani", "Kundaji", "Bumra", "Mkundaji", "Hakuna", "Naysi bahari", "Nyasibahari", 1,  "Nyasi bajari"))|> 
  distinct() |> 
  drop_na(number_per_group,fishing_ground,total_weight_g,total_number, swahili_names,village,year,date)

# initial dataset had 62696 entries 

#sort(unique(catch.data$fishing_ground))

catch.combined %<>%
  mutate(swahili_names = case_when(swahili_names == NA ~ scientific_names,
                                    TRUE ~ swahili_names))


#You can use creation dates to cross-check the dates entered

species.lw = fish.gen |>
  left_join(species.specific, by = c("global_id" = "parent_id")) |> 
  select(-global_id, -parent_id, -creation_date, -global_id.y, -fisher_number, -fisher_age, -fisher_name, -fisher_gender,-data_collector, -number_per_group,-price,-going_time,-return_time) 


# left join using data.table
#species.lw <- species.specific[fish.gen, on = .(parent_id = global_id)] 


species.lw %<>% 
  left_join(family_info_by_species, by = "scientific_names")

#species.lw <- family_info_by_species[species.lw, on = .(scientific_names)]

species.lw = species.lw|> 
  filter(!fishing_ground %in% c("TASI", "Changu", "Pono", "Pono mchele", "Ngalawa", "Majani bahari", "MIK", "Bodo", "1", "Mwani", "Matumbawe/mwamba", "Tembo janja", "Koana", "Mchanga", "Nyasi  bahari", "Nyasi bahari", "Mchangani", "Kundaji", "Bumra", "Mkundaji", "Hakuna", "Naysi bahari", "Nyasibahari", 1, "TASI"))|> 
  distinct() |> 
  drop_na(swahili_names,scientific_names,weight_g,length_cm, village,year,date)

# Grouped IQR outlier removal

remove_outliers_grouped <- function(df, cols = c("total_weight_g", "total_number"),
                                    group_vars = c("village", "scientific_names"),
                                    iqr_threshold = 1.5, min_obs = 10) {
  df_clean <- df %>%
    group_by(across(all_of(group_vars))) %>%
    mutate(across(
      all_of(cols),
      ~ {
        if (sum(!is.na(.)) < min_obs) {
          .  # skip small groups
        } else {
          Q1 <- quantile(., 0.25, na.rm = TRUE)
          Q3 <- quantile(., 0.75, na.rm = TRUE)
          IQR <- Q3 - Q1
          lower <- Q1 - iqr_threshold * IQR
          upper <- Q3 + iqr_threshold * IQR
          ifelse(. < lower | . > upper, NA, .)
        }
      },
      .names = "clean_{col}"
    )) %>%
    ungroup()
  
  return(df_clean)
}


clean.catch.combined = remove_outliers_grouped(catch.combined) |> 
  mutate(date = as.Date(date, format = "%m/%d/%Y")) |>
  filter(!is.na(clean_total_weight_g),
         !is.na(clean_total_number))

# after cleaning the dataset had 43093 entries 


clean.catch.combined$month = factor(
  clean.catch.combined$month,
  levels = month.abb,
  ordered = TRUE
)


clean.catch.combined %<>%
  mutate(swahili_names = coalesce(swahili_names, scientific_names))

#### ANALYSIS ####

cpue.general =clean.catch.combined |> 
  group_by(region,village,gear,fishing_ground,year,month,date,scientific_names, swahili_names,family) |> 
  filter(!is.na(clean_total_weight_g),
         !is.na(number_per_group),
         !number_per_group == 0) |> 
  summarise(total_catch = sum(clean_total_weight_g, na.rm = TRUE),
            total_fishers = sum(number_per_group, na.rm = TRUE),
            cpue = (total_catch/total_fishers)) |>
  ungroup() |> 
mutate(family_group = case_when(family %in% c("Lethrinidae", "Mullidae", "Siganidae", "Scaridae","Chaetodontidae") ~ family,
                                TRUE ~ "Others"))

cpue.village = cpue.general |>
  group_by(region,village,year,month,family_group) |> 
  summarise( mean_cpue = mean(cpue,na.rm = TRUE))

cpue = cpue.general |> 
  group_by(village,year,month) |> 
  summarise( mean_cpue = mean(cpue,na.rm = TRUE))


ggplot(data = cpue.village ,  aes(x = interaction(month, year), y  = mean_cpue, fill = family_group)) +
  geom_bar(stat = "identity", color = "black") +
  labs(x = "Month and Year", y = "Average CPUE (g)") +
  scale_x_discrete(guide = "axis_nested")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  facet_wrap(~village, scales = "free")

cpue.gear = cpue.general |> 
  group_by(village, gear) |> 
  summarise( mean_cpue = mean(cpue,na.rm = TRUE), .groups = "drop") 

village.cpue = cpue.general |> 
  filter(
    village == params$village,
    gear %in% c("By_hand","Cast_nets","Diving","Fish_trap","Handline",
                "Monofilament_net","Gillnet","Speargun","Longline",
                "fence_traps","Purse_seines","ring_nets","Seinnet")
  ) |> 
  group_by(gear) |> 
  summarise(mean_cpue = mean(cpue, na.rm = TRUE), .groups = "drop") 


required.gear = cpue.gear |> 
  filter(gear %in% c("By_hand", "Cast_nets" ,"Diving", "Fish_trap", "Handline", "Monofilament_net","Gillnet", "Cast_nets", "Speargun", "Longline", "fence_traps", "Purse_seines", "ring_nets", "Seinenet" ) ) |> 
  distinct(gear) |> 
  pull()

ggplot(data = cpue.gear |> 
         filter(gear %in% c("By_hand", "Cast_nets" ,"Diving", "Fish_trap", "Handline", "Monofilament_net","Gillnet", "Cast_nets", "Speargun", "Longline", "fence_traps", "Purse_seines", "ring_nets", "Seinnet" )),  aes(x = gear, y  = mean_cpue, fill = family_group)) +
  geom_bar(stat = "identity", color = "black") +
  labs(x = "Month and Year", y = "Average CPUE (g)") +
  scale_x_discrete(guide = "axis_nested")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

cpue.fground = cpue.general |> 
  group_by(village,fishing_ground) |> 
  summarise( mean_cpue = mean(cpue,na.rm = TRUE), .groups = "drop") |> 
  group_by(village) |> 
  slice_max(order_by = mean_cpue, n = 10, with_ties = FALSE) |> 
  arrange(village, desc(fishing_ground), desc(mean_cpue))

village.cpuefground = cpue.fground |> 
  filter(village == params$village)

family.composition = cpue.general |> 
  filter(!family == "Unknown") |> 
  group_by(family,village) |> 
  summarise(mean_cpue = mean(cpue), .groups = "drop") |> 
  group_by(village) |> 
  slice_max(order_by = mean_cpue, n = 10, with_ties = FALSE) |> 
  arrange(village, desc(family), desc(mean_cpue))

village.fcomposition = family.composition |> 
  filter(village == params$village)

ggplot(data = family.composition ,  aes(x = family, y  = mean_cpue)) +
  geom_col() +
  coord_flip()+
  labs(x = "Family", y = "Abundance") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  facet_wrap(~village, scales = "free")

species.composition = cpue.general |> 
  filter(!is.na(scientific_names)) |> 
  group_by(swahili_names,scientific_names,village) |> 
  summarise(mean_cpue = mean(cpue),  .groups = "drop") |> 
  group_by(village) |> 
  slice_max(order_by = mean_cpue, n = 10, with_ties = FALSE) |> 
  arrange(village, desc(scientific_names), desc(mean_cpue)) 
  
village.scomposition = species.composition |> 
  filter(village == params$village)

ggplot(data = species.composition ,  aes(x = scientific_names, y  = mean_cpue)) +
  geom_col() +
  coord_flip()+
  labs(x = "Family", y = "Abundance") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  facet_wrap(~village, scales = "free")

average.weight = clean.catch.combined |> 
  group_by(village, scientific_names, swahili_names) |> 
  summarise(total_catch = sum(clean_total_weight_g, na.rm = TRUE),
            total_number = sum(number_per_group, na.rm = TRUE),
            mean_weight = round((total_catch/total_number),2)) #|> 
  #filter(!between(mean_weight, 500, 1000))

village.weight = average.weight |> 
  filter(village == params$village) |>
  arrange(desc(mean_weight)) |> 
  select(swahili_names, scientific_names, mean_weight)

top5 = village.weight |> 
  ungroup() |>
  slice_max(mean_weight, n = 5, with_ties = FALSE) |> 
  select(swahili_names, scientific_names, mean_weight)

bottom5 = village.weight |> 
  ungroup() |>
  slice_min(mean_weight, n = 5, with_ties = FALSE) |> 
  select(swahili_names, scientific_names, mean_weight)


##### Records ####

record.number = fish.gen |> 
  group_by(region, village, month,year) |> 
  filter(!between(date, as.Date("2024-04-01"), as.Date("2024-08-30")),
         !is.na(village)) |> 
  summarise(record_counts = n_distinct(day))

#write_csv(record.number,paste0(file_path,  "/fish_records.csv"))

ggplot(data = record.number |> 
         filter(region == "Pemba",
                !village %in% c("Tumbe", "Shumba", "Kiuyu")), aes(x = interaction(month, year), y = record_counts))+
  geom_col() +
  geom_hline(yintercept = 12, color = "red", size = 1) +
  scale_x_discrete(guide = legendry::guide_axis_nested()) +
  labs(x = "Month and Year",
       y = "Number of Days Worked") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0)) +
  facet_wrap(~ village)

#file_path = "/Users/USER/Documents/fish_catch_analysis/results/"
#ggsave(filename = paste0(file_path,  "/pemba_records.jpg"), width = 10, height = 6, units = "in", dpi = 300)  
  
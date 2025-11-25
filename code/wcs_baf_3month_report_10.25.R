#### Libraries ####

require(tidyverse)
require(sf)
require(rnaturalearth)
require(rnaturalearthdata)
require(marmap)
require(terra)
require(tidyterra)
require(magrittr)
require(ggforce)
require(data.table)
require(readr)
require(readxl)
require(writexl)
#require(summarytools)
require(ggh4x)
require(webr)
require(leaflet)
require(data.table)

#### data import ####

wcs.gen = list.files(
  path = "/Users/USER/Documents/fish_catch_analysis/data/checked/gen/wcs_analysis/",
  pattern = "\\.xlsx$",
  full.names = TRUE)|>  
  map_dfr(~ read_excel(.x, sheet = "Form_1_0")) |> 
  select(date = "Tarehe",                                                                                               
         global_id = "GlobalID",
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
         fishing_ground = as.character(fishing_ground)) |> 
  glimpse()


wcs.catch.composition =list.files(
  path = "/Users/USER/Documents/fish_catch_analysis/data/checked/gen/wcs_analysis/",
  pattern = "\\.xlsx$",
  full.names = TRUE)|>  
  map_dfr(~ read_excel(.x, sheet = "Catch_composition_1")) |>
  select(  global_id = "GlobalID",
           parent_id = "ParentGlobalID",
           creation_date = "CreationDate", 
           scientific_names = "aina ya samaki",
           total_weight_g = "Uzito wa jamii moja ya samaki",
           total_number = "Idadi ya samaki") |>  
  mutate(year_creation = lubridate::year(creation_date)) |>
  glimpse()


##### FAMILY, GENERAL, TROPHIC GROUP AND SPECIES NAME DATA ####

fish.families = read.csv("/Users/USER/Documents/fish_catch_analysis/data/fish_sci_names_families.csv")

# Using data table to fix names

setDT(fish.families)

# Removing duplicates of swahili names

swahili.clean <- fish.families[
  , .(swahili_names = paste(unique(swahili_names), collapse = ", ")), 
  by = scientific_names]


family_info_by_species <- fish.families[
  , .SD[1], 
  by = scientific_names, 
  .SDcols = c( "family", "common_name", "trophic_groups")
]


# left joining a clean dataset without duplicates

family_info_by_species <- swahili.clean[
  family_info_by_species, 
  on = .(scientific_names)
]



#### Data Merging ####

wcs.catch.sep = wcs.gen |> 
  left_join(wcs.catch.composition, by = c("global_id" = "parent_id")) |> 
  select(-global_id, -creation_date, -global_id.y,-data_collector) |> 
  filter(village %in% c("Tumbe", "Shumba", "Kiuyu"),
         month %in% c("Jul","Aug", "Sep")) |> 
  drop_na(total_weight_g, number_per_group)


wcs.catch.sep %<>% 
  left_join(family_info_by_species, by = "scientific_names") 


# REMONING DUPLICATES 

wcs.catch.sep %<>% 
  distinct()

#### cleaning ####

wcs.catch.sep |> 
  filter(is.na(swahili_names)) %$% 
  unique(scientific_names)

wcs.catch.sep %<>% 
  mutate(swahili_names = case_when(is.na(swahili_names)~ scientific_names,
                                   TRUE ~ swahili_names),
         family = case_when(scientific_names %in% c("Pono kifua","Pono bluu","Pono meno", "Pono chore","Papai")~ "Scaridae",
                            scientific_names %in% c("Kundaji mwamba","Kundaji mwekundu","Kundaji mweupe", "Kundaji kidoa")~ "Mullidae",
                            scientific_names %in% c("Kikande mweupe","Kikande mweusi")~ "Balistidae",
                            scientific_names %in% c("Nyamvi","Nyavi","Changu kibaku", "Chengo","Kokwe")~ "Lethrinidae",
                            scientific_names %in% c("Poowe","Mbono/kubu","Numba")~ "Lutjanidae",
                            scientific_names %in% c("Mkubu","Viroho","Kowe", "Mchuanda","Machogodi","Tufe", "Mkizi")~ "Unknown",
                            scientific_names == "Govero/Karabai"~ "Priacanthidae",
                            scientific_names == "Masange"~ "Pomacentridae",
                            scientific_names == "Kolekole mrondoo"~ "Carangidae",
                            scientific_names == "Kifuu"~ "Holocentridae",
                            scientific_names == "chewa_mwekundu"~ "Serranidae",
                            TRUE ~ family
                            ))



# Date range
first_date <- min(wcs.catch.sep$date, na.rm = TRUE)
last_date  <- max(wcs.catch.sep$date, na.rm = TRUE)
first_date_fmt <- format(first_date, "%d %B %Y")
last_date_fmt  <- format(last_date, "%d %B %Y")

# wcs.catch.sep |> 
#   select(-fisher_name) |> 
#   dfSummary() |> 
#   print()


sept.cpue = wcs.catch.sep |> 
  select(date, day,month,year, village, region, fisher_number, fisher_name, number_per_group, 
         fishing_ground, total_weight_g,total_number,family, scientific_names) |> 
  group_by(month,year,  village,fisher_number, fisher_name,fishing_ground,family) |>
  summarise(
    total_catch = sum(total_weight_g, na.rm = TRUE),
    total_fishers = sum(number_per_group, na.rm = TRUE),
    cpue = round((total_catch/total_fishers),2))

##### effort ####
effort.wcs = wcs.catch.sep |> 
  select(date, day,month,year, village,  number_per_group) |> 
  group_by(month,year,  village) |>
  summarise(
    effort_fisher_day = mean(number_per_group))

sept.cpue |> head() 

village.wcs.info = sept.cpue |> 
  select(village, cpue, fishing_ground, family) |> 
  group_by(village) |> 
  summarise(mean_cpue = round(mean(cpue),2),
            number_fground = n_distinct(fishing_ground),
            family_richness = n_distinct(family)) 

village_stats = sept.cpue |>
  group_by(village) |>
  summarise(
    mean_weight = mean(total_catch, na.rm = TRUE),
    min_weight  = min(total_catch, na.rm = TRUE),
    max_weight  = max(total_catch, na.rm = TRUE),
    mean_cpue = mean(cpue, na.rm = TRUE),
    min_cpue  = min(cpue, na.rm = TRUE),
    max_cpue  = max(cpue, na.rm = TRUE),
    .groups = "drop"
  )

village.wcs = sept.cpue |>  
  select(village, cpue) |> 
  group_by(village) |> 
  summarise(mean_cpue = mean(cpue)) |>
  arrange(desc(mean_cpue))  


fish.wcs.cpue = sept.cpue |> 
  select(village, family, cpue) |>
  group_by(village,family)|> 
  summarise(mean_cpue = mean(cpue)) |>
  arrange(desc(mean_cpue)) 


month.cpue.june = sept.cpue |>
  group_by(month, year, village) |> 
  summarise(
    mean_cpue = (mean(cpue)/1000),
    mean_catch = (mean(total_catch)/1000),
    sd_catch = sd(mean_catch))




ggplot(month.cpue.june , aes(x = interaction(month, year))) +
  # Bar for Total Catch
  geom_col(aes(y = mean_catch), fill = "steelblue", alpha = 0.6) +
  
  # Line for CPUE
  geom_line(aes(y = mean_cpue, group = 1), color = "red", size = 1.2) +
  
  # Dual Y-axes
  scale_y_continuous(
    name = "Mean Total Catch (kg)",  # Primary Y axis (Total Catch)
    sec.axis = sec_axis(~ ., name = "Mean CPUE (kg/fisher/day)")  # Secondary Y axis (CPUE)
  ) +
  scale_x_discrete(guide = "axis_nested")+
  
  # Styling
  theme_bw() +
  labs(x = "Month and Year") +
  theme(
    axis.title.y.left = element_text(color = "steelblue"),
    axis.title.y.right = element_text(color = "red"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  
  # Faceting
  facet_grid(~village)


ggplot(effort.wcs, aes(x = interaction(month, year), y = effort_fisher_day, color = village)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Catch Weight Trends Over Time",
    x = "Date",
    y = "Catch Weight (g)"
  )+
  scale_x_discrete(guide = "axis_nested")


# Trend analysis — linear regression of weight over time
trend_model = wcs.catch.sep |>
  group_by(village) |>
  summarise(trend_slope = coef(lm(total_weight_g ~ as.numeric(date)))[2])





#### fishing ground ####

fgrounds.cpue = sept.cpue |> 
  select(fishing_ground, village,  cpue) |> 
  group_by(fishing_ground,village) |> 
  summarize(
    mean_cpue = round(mean(cpue),2)) |>
  arrange(desc(mean_cpue)) 

#### gear ####

gear.use = wcs.catch.sep |> 
  select(gear, village) |> 
  filter(!is.na(gear)) |> 
  group_by(village, gear) |> 
  summarize(
    gear_count = n()
  ) |> 
  arrange(desc(gear_count))

gear.use |> 
  pivot_wider(names_from =village, values_from = gear_count ) 

gear.cpue.june = wcs.catch.sep |> 
  select(date, day,month,year, village,  fisher_number, fisher_name, number_per_group, 
         fishing_ground, total_weight_g,total_number,family, gear) |>
  filter(!is.na(gear)) |> 
  group_by( village, gear) |>
  summarise(
    total_catch = sum(total_weight_g, na.rm = TRUE),
    total_fishers = sum(number_per_group, na.rm = TRUE),
    cpue = round((total_catch/total_fishers),2))

gear.cpue.june = gear.cpue.june |> 
  group_by(village, gear) |> 
  summarize(mean_gear_cpue = mean(cpue)) |> 
  arrange(desc(mean_gear_cpue)) 


#### vessel ####

vessel.use = wcs.catch.sep |> 
  select(method, village) |> 
  filter(!is.na(method)) |> 
  group_by(village, method) |> 
  summarize(
    vessel_count = n()
  ) |> 
  arrange(desc(vessel_count)) 

#### family trends ####

reef.family.composition = sept.cpue |> 
  select(village, family, cpue) |> 
  filter(family %in% c("Siganidae","Mullidae","Lethrinidae","Scaridae","Acanthuridae","Labridae","Lutjanidae","Carangidae","Serranidae","Chaetodontidae", "Haemulidae" )) |>
  group_by(family, village) |>
  summarise(mean_cpue = mean(cpue)) |> 
  arrange(desc(mean_cpue)) 

reef.family.trend = sept.cpue |> 
  select(village, family, month, year, cpue) |> 
  filter(family %in% c("Siganidae","Mullidae","Lethrinidae","Scaridae","Acanthuridae","Labridae","Lutjanidae","Carangidae","Serranidae","Chaetodontidae", "Haemulidae" )) |>
  group_by(month, year, family, village) |>
  summarise(mean_cpue = mean(cpue)) 


#####plot####

ggplot(data = reef.family.trend|> 
         filter(family %in% c("Siganidae","Mullidae","Lethrinidae","Scaridae","Acanthuridae","Labridae","Lutjanidae","Carangidae","Serranidae","Chaetodontidae", "Haemulidae")), 
       aes(x = interaction(month, year), y = family, label = round((mean_cpue/1000),2)  ,fill = round((mean_cpue/1000),2))) +
  geom_tile(color = "grey") +
  scale_x_discrete(guide = "axis_nested")+
  scale_fill_gradient2(low = "#0a7d9a",
                       mid = "orange",
                       high = "#3c742f",
                       name = "Average CPUE")+
  geom_text(color = "white")+
  scale_y_discrete(limit = rev)+
  labs(x = "Month and Year", y = "Family", fill = "Average CPUE (kg)") +
  theme_bw()+
  facet_grid(~village)

#ggsave(filename = paste0(file_path,  "/wcs_heatmap_8.25.jpg"), width = 10, height = 6, units = "in", dpi = 300)

##### fisher gender ####

fisher.gender = wcs.catch.sep |> 
  select(fisher_gender, village) |> 
  filter(!is.na(fisher_gender)) |> 
  group_by(village, fisher_gender) |> 
  summarize(
    gender_count = n()
  ) |> 
  arrange(desc(gender_count))





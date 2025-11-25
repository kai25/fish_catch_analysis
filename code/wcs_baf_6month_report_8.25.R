#### libraries ####

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
require(summarytools)
require(legendry)
require(webr)
require(leaflet)
require(data.table)


#### data import ####

wcs.catch.june = read.csv("/Users/USER/Documents/fish_catch_analysis/data/Merged data/catch.data_6.25.csv") |>
  filter(village %in% c("Tumbe", "Shumba", "Kiuyu")) |> 
  drop_na(total_weight_g, number_per_group) |> 
  select(-1,-day, -month, -year) |> 
  mutate(date = as.POSIXct(date, format="%m/%d/%Y"),
         year = lubridate::year(date),
         month = lubridate::month(date, label = TRUE),
         day = lubridate::day(date)) 

# REMONING DUPLICATES 

wcs.catch.june %<>% 
  distinct()


# Date range
first_date <- min(wcs.catch.june$date, na.rm = TRUE)
last_date  <- max(wcs.catch.june$date, na.rm = TRUE)
first_date_fmt <- format(first_date, "%d %B %Y")
last_date_fmt  <- format(last_date, "%d %B %Y")

wcs.catch.june |> 
  select(-fisher_name) |> 
  dfSummary() |> 
  print()

june.cpue = wcs.catch.june |> 
  select(date, day,month,year, village, region, fisher_number, fisher_name, number_per_group, 
         fishing_ground, total_weight_g,total_number,family, scientific_names) |> 
  group_by(month,year,  village,fisher_number, fisher_name,fishing_ground,family) |>
  summarise(
    total_catch = sum(total_weight_g, na.rm = TRUE),
    total_fishers = sum(number_per_group, na.rm = TRUE),
    cpue = round((total_catch/total_fishers),2))

effort.wcs = wcs.catch.june |> 
  select(date, day,month,year, village,  number_per_group) |> 
  group_by(month,year,  village) |>
  summarise(
    effort_fisher_day = mean(number_per_group))

ggplot(data = effort.wcs ,aes(x = interaction(month, year), y = village, label = effort_fisher_day ,fill = effort_fisher_day)) +
  geom_tile(color = "grey") +
  scale_x_discrete(guide = "axis_nested")+
  scale_fill_gradient2(low = "#0a7d9a",
                       mid = "orange",
                       high = "#3c742f",
                       name = "Effort (Fisher/day)")+
  geom_text(color = "white")+
  scale_y_discrete(limit = rev)+
  labs(x = "Month and Year", y = "Villages", fill = "Effort (Fisher/day)") +
  theme_bw()

june.cpue |> head() 

village.wcs.info = june.cpue |> 
  select(village, cpue, fishing_ground, family) |> 
  group_by(village) |> 
  summarise(mean_cpue = round(mean(cpue),2),
            number_fground = n_distinct(fishing_ground),
            family_richness = n_distinct(family)) 

village_stats = june.cpue |>
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

village.wcs = june.cpue |>  
  select(village, cpue) |> 
  group_by(village) |> 
  summarise(mean_cpue = mean(cpue)) |>
  arrange(desc(mean_cpue))  


fish.wcs.cpue = june.cpue |> 
  select(village, family, cpue) |>
  group_by(village,family)|> 
  summarise(mean_cpue = mean(cpue)) |>
  arrange(desc(mean_cpue)) 


month.cpue.june = june.cpue |>
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
trend_model = wcs.catch.june |>
  group_by(village) |>
  summarise(trend_slope = coef(lm(total_weight_g ~ as.numeric(date)))[2])





#### fishing ground ####

fgrounds.cpue = june.cpue |> 
  select(fishing_ground, village,  cpue) |> 
  group_by(fishing_ground,village) |> 
  summarize(
    mean_cpue = round(mean(cpue),2)) |>
  arrange(desc(mean_cpue)) 

#### gear ####

gear.use = wcs.catch.june |> 
  select(gear, village) |> 
  filter(!is.na(gear)) |> 
  group_by(village, gear) |> 
  summarize(
    gear_count = n()
  ) |> 
  arrange(desc(gear_count))

gear.use |> 
  pivot_wider(names_from =village, values_from = gear_count ) 

gear.cpue.june = wcs.catch.june |> 
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

vessel.use = wcs.catch.june |> 
  select(method, village) |> 
  filter(!is.na(method)) |> 
  group_by(village, method) |> 
  summarize(
    vessel_count = n()
  ) |> 
  arrange(desc(vessel_count)) 

#### family trends ####

reef.family.composition = june.cpue |> 
  select(village, family, cpue) |> 
  filter(family %in% c("Siganidae","Mullidae","Lethrinidae","Scaridae","Acanthuridae","Labridae","Lutjanidae","Carangidae","Serranidae","Chaetodontidae", "Haemulidae" )) |>
  group_by(family, village) |>
  summarise(mean_cpue = mean(cpue)) |> 
  arrange(desc(mean_cpue)) 

reef.family.trend = june.cpue |> 
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


#### map ####
require(leaflet)
require(htmlwidgets)

marine.resource = read.csv("C:/Users/USER/Documents/fish_catch_analysis/data/map_data/Marine_Resources_0.csv") |> 
  select(region = 'Chagua.Mkoa',
         village = 'Jina.la.BMU.SFC.Mtaa',
         fishing_ground = 'Jina.la.eneo.mwamba',
         long = "x",
         lat = "y") |> 
  filter(!is.na(long),
         !is.na(lat)) |> 
  distinct()

#left join with dataset that has cpue values for each year and total catch values to see the change between baseline year and procedding year 


wcs.grounds.maps = marine.resource |> 
  filter(village %in% c("Shumba_mjini","Tumbe","Kiuyu mbuyuni"))

#Michenzani is missing


# 3. Convert to spatial data
wcs.fgrounds.sf <- st_as_sf(
  wcs.grounds.maps,
  coords = c("long", "lat"),
  crs = 4326,
  remove = FALSE
)

wcs.grounds.map = wcs.fgrounds.sf %>%
  leaflet() |> 
  addTiles() |>   
  addCircleMarkers(
    ~long, ~lat,
    popup = ~paste0("<b>Fishing Ground:</b> ", fishing_ground),
    label = ~fishing_ground,
    radius = 6,
    color = "orangered",
    fillOpacity = 0.7
  )

wcs.grounds.map

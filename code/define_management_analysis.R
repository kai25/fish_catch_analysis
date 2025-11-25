

management = catch.data |> 
  filter(fishing_ground %in% c("Taa", "kimio", "Baharini","Muinzi","Jombe", "Chekwe", "Kwata", "Funzi","Mawe mawili","Nganyawani","Mwagisi","Milangoni","Mawe mawili","Mkumbi","Wandaule","Makoongwe","Kwamwana maua", "Wamba", "Kokota", "Kwasharifu"), 
         family %in% c("Siganidae","Mullidae","Lethrinidae","Scaridae","Acanthuridae","Labridae","Lutjanidae","Carangidae","Serranidae","Chaetodontidae", "Haemulidae" ),
         !month == "Jun", 
         !total_weight_g >= 10000 & !total_weight_g <= 10) |> 
  mutate(aina_ya_usimamizi = case_when(fishing_ground %in% c("Baharini","Wamba","Mkumbi","Mwagisi", "Kokota","kimio", "Chekwe","Kwata" ) ~ "Hamna",
                                       fishing_ground %in% c("Muinzi","Jombe","Taa","Wandaule","Mawe mawili","Milangoni","Nganyawani")~ "Uwezekano wa Kufungia" ,
                                       fishing_ground %in% c("Kwasharifu","Makoongwe","Kwamwana maua","Funzi")~ "Ufungiaji" ),
         cmg = case_when(fishing_ground %in% c("Baharini","Muinzi","Chekwe","Jombe") ~ "KUKACHOKI",
                         fishing_ground %in% c("Kwasharifu","Makoongwe","Kwata","Kwamwana maua")~ "STAMISHIMA",
                         fishing_ground %in% c("Funzi","Kokota") ~ "MKIZIKWANI",
                         fishing_ground %in% c("kimio","Nganyawani","Taa","Mwagisi") ~ "MCHOMAPUNDA",
                         fishing_ground %in% c("Mkumbi","Wamba","Milangoni","Mawe mawili","Wandaule")~ "TAWALANI_KIZINGANI"),
         jamii_ya_samaki = case_when(family == "Siganidae"~ "Tasi",
                                     family == "Mullidae"~ "Kundaji",
                                     family == "Lethrinidae"~ "Changu",
                                     family == "Scaridae"~ "Pono",
                                     family == "Acanthuridae"~ "Kangaja",
                                     family == "Labridae"~ "Ndodozi/Vorwe",
                                     family == "Lutjanidae"~ "Tembo",
                                     family == "Carangidae"~ "Kolekole",
                                     family == "Serranidae"~ "Chewa",
                                     family == "Chaetodontidae"~ "Kipepeo",
                                     family == "Haemulidae"~ "Mlea"))



# Data from machomondoni Kukuu, Mwamba wawili monga vyeru, mito miwili mwandusi and kwa thumani kwale tanga is missing

unique(management$family)

colSums(is.na(management))

catch.management = management |> 
  select(region,month, year, total_weight_g, fishing_ground, number_per_group, aina_ya_usimamizi,cmg) |>  
  mutate(cpue = total_weight_g/number_per_group) |> 
  group_by(region, month, year,fishing_ground, aina_ya_usimamizi,cmg) |> 
  summarise(mean_total_catch = round((mean(total_weight_g)),2),
            mean_cpue = round(mean(cpue),2)) |>
  print()


ggplot(
  catch.management |> 
    filter(cmg == "TAWALANI_KIZINGANI"), aes(x = interaction(month, year))) +
  # Bar for Total Catch
  geom_col(aes(y = mean_total_catch), fill = "steelblue", alpha = 0.6) +
  
  # Line for CPUE
  geom_line(aes(y = mean_cpue, group = 1), color = "red", size = 1.2) +
  
  # Dual Y-axes
  scale_y_continuous(
    name = "Wastani wa mavuvi",  # Primary Y axis (Total Catch)
    sec.axis = sec_axis(~ ., name = "Wastani wa pato la Mvuvi")  # Secondary Y axis (CPUE)
  ) +
  scale_x_discrete(guide = "axis_nested")+
  
  # Styling
  theme_bw() +
  labs(x = "Mwezi na Mwaka") +
  theme(
    axis.title.y.left = element_text(color = "steelblue"),
    axis.title.y.right = element_text(color = "red"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  
  # Faceting
  facet_grid(aina_ya_usimamizi ~ fishing_ground)

ggsave(filename = paste0(file_path,  "/TAWALANI_KIZINGANI_cpue.jpg"), width = 10, height = 6, units = "in", dpi = 300)

catch.family = management |> 
  select(month, year, total_weight_g, fishing_ground, number_per_group,jamii_ya_samaki, aina_ya_usimamizi,cmg) |>  
  mutate(cpue = total_weight_g/number_per_group) |> 
  group_by(month, year,fishing_ground, jamii_ya_samaki, aina_ya_usimamizi,cmg) |> 
  summarise(mean_total_catch = round((mean(total_weight_g)),2),
            mean_cpue = round(mean(cpue),2)) |>
  print()

my_color = c("aquamarine", "steelblue", "orangered", "purple", "skyblue3", "firebrick","yellow", "darkgreen", "#0a7d9a", "#b44f18", "#169674", "#d6c300")

# KUKACHOKI, TAWALANI_KIZINGANI, MCHOMAPUNDA, MKIZIKWANI, STAMISHIMA

ggplot(data = catch.family |> 
         filter(cmg == "STAMISHIMA"),aes(x = interaction( fishing_ground, aina_ya_usimamizi), y  = mean_cpue,  fill = jamii_ya_samaki)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(x = "Miamba ya uvuvi", y = "Asilimia ya Jamii ya samaki") +
  scale_x_discrete(guide = "axis_nested")+
  scale_fill_manual(values = c("aquamarine", "steelblue", "orangered", "purple", "skyblue3", "firebrick","yellow", "darkgreen", "#0a7d9a", "#b44f18", "#169674", "#d6c300" ))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(filename = paste0(file_path,  "/STAMISHIMA_family.jpg"), width = 10, height = 6, units = "in", dpi = 300)

gear.use = management |> 
  filter(!is.na(gear)) |>
  mutate(aina_ya_mtego = case_when( 
    gear ==  "Chupa" ~	"Chupa",
    gear == "Seinnet"~	"Juya (Nyavu zenye macho madogo)",
    gear == "Longline"~	"Kaputi/ dhulumati",
    gear == "Diving" ~	"Kuzamia",
    gear == "By_hand" ~	"Kwa mkono",
    gear == "Speargun" ~	"Mkuki",
    gear == "Handline" ~	"Mshipi",
    gear == "Fish_trap" ~	"Mtego/dema",
    gear == "Gillnet" ~	"Nyavu",
    gear == "Cast_nets" ~	"Nyavu za kurusha",
    gear == "Purse_seines/ring_nets" ~	"Nyavu za kuzungusha",
    gear == "Monofilament_net" ~	"Nyavu za utari",
    gear == "Stake/fence_traps"	~ "Uzio",
    gear =="Other" ~	"Nyinginezo",
    gear == "Cast_nets,Monofilament_net" ~"Nyavu za kurusha na Nyavu za utari",
    gear == "Fish_trap,Handline" ~"Mtego/dema na Mshipi",
    gear == "Handline,Stake/fence_traps,Purse_seines/ring_nets" ~"Nyavu za kuzungusha,Uzio na Mshipi",
    gear == "Purse_seines/ring_nets,Fish_trap" ~"Mtego/dema na Nyavu za kuzungusha",
    gear == "Purse_seines/ring_nets,Handline" ~"Nyavu za kuzungusha na Mshipi"
  )) |> 
  group_by(cmg,fishing_ground,aina_ya_usimamizi, aina_ya_mtego) |> 
  summarize(gear_counts = n()) |> 
  print()

# KUKACHOKI, TAWALANI_KIZINGANI, MCHOMAPUNDA, MKIZIKWANI, STAMISHIMA

ggplot(data = gear.use |> 
         filter(cmg == "STAMISHIMA",
                !is.na(aina_ya_mtego)),aes(x =interaction( fishing_ground, aina_ya_usimamizi), y  = gear_counts,  fill = aina_ya_mtego)) +
  geom_bar(stat = "identity", position = "fill", color = "black") +
  labs(x = "Miamba", y = "Zana") +
  scale_x_discrete(guide = "axis_nested")+
  scale_fill_manual(values = c ("steelblue", "orangered", "purple", "skyblue3", "yellow", "darkgreen", "#0a7d9a", "#b44f18", "#169674", "#d6c300" ))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(filename = paste0(file_path,  "/STAMISHIMA_gear.jpg"), width = 10, height = 6, units = "in", dpi = 300)


family.counts = catch.data |> 
  filter(fishing_ground %in% c("Taa", "kimio", "Baharini","Muinzi","Jombe", "Chekwe", "Kwata", "Funzi","Mawe mawili","Nganyawani","Mwagisi","Milangoni","Mawe mawili","Mkumbi","Wandaule","Makoongwe","Kwamwana maua", "Wamba", "Kokota", "Kwasharifu")) |> 
  mutate(aina_ya_usimamizi = case_when(fishing_ground %in% c("Baharini","Wamba","Mkumbi","Mwagisi", "Kokota","kimio", "Chekwe","Kwata" ) ~ "Hamna",
                                       fishing_ground %in% c("Muinzi","Jombe","Taa","Wandaule","Mawe mawili","Milangoni","Nganyawani")~ "Uwezekano wa kufungia" ,
                                       fishing_ground %in% c("Kwasharifu","Makoongwe","Kwamwana maua","Funzi")~ "Ufungiaji" ),
         cmg = case_when(fishing_ground %in% c("Baharini","Muinzi","Chekwe","Jombe") ~ "KUKACHOKI",
                         fishing_ground %in% c("Kwasharifu","Makoongwe","Kwata","Kwamwana maua")~ "STAMISHIMA",
                         fishing_ground %in% c("Funzi","Kokota") ~ "MKIZIKWANI",
                         fishing_ground %in% c("kimio","Nganyawani","Taa","Mwagisi") ~ "MCHOMAPUNDA",
                         fishing_ground %in% c("Mkumbi","Wamba","Milangoni","Mawe mawili","Wandaule")~ "TAWALANI_KIZINGANI")) |> 
  select(fishing_ground, family, aina_ya_usimamizi,cmg) |>
  group_by(cmg, fishing_ground, aina_ya_usimamizi) |> 
  summarize(family_counts = n_distinct(family)) |> 
  print()

write.csv(family.counts, "family counts.csv")



#### FISHCATCH FOR 6MONTH REPORTING ####

#### Import data ####

catch.june = read.csv("data/Merged data/catch.data_6.25.csv") |>
  drop_na(total_weight_g, number_per_group) |> 
  select(-1,-day, -month, -year) |> 
  mutate(date = as.POSIXct(date, format="%m/%d/%Y"),
    year = lubridate::year(date),
         month = lubridate::month(date, label = TRUE),
         day = lubridate::day(date)) |> 
  filter(!month == "Jun") 




catch.june |> 
  summary()

catch.june |> 
  select(-fisher_name) |> 
  dfSummary() |> 
  print()

june.cpue = catch.june |> 
  select(date, day,month,year, village, region, fisher_number, fisher_name, number_per_group, 
         fishing_ground, total_weight_g,total_number,family, scientific_names) |> 
  group_by(day,month,year, region, village,fisher_number, fisher_name,fishing_ground,family) |>
  summarise(
    total_catch = sum(total_weight_g, na.rm = TRUE),
    total_fishers = sum(number_per_group, na.rm = TRUE),
    cpue = round((total_catch/total_fishers),2))

 june.cpue |> head() 
 
 region.info = june.cpue |> 
   select(region, cpue, fishing_ground, family) |> 
   group_by(region) |> 
   summarise(mean_cpue = mean(cpue),
             number_fground = n_distinct(fishing_ground),
             family_richness = n_distinct(family)) |> 
   print()
 
 village.tanga.cpue = june.cpue |> 
   filter(region == "Tanga",
          !is.na(village)) |> 
   select(village, cpue) |> 
   group_by(village) |> 
   summarise(mean_cpue = mean(cpue)) |>
   arrange(desc(mean_cpue)) |> 
   print() 
 
 village.pemba.cpue = june.cpue |> 
   filter(region == "Pemba",
          !is.na(village)) |> 
   select(village, cpue) |> 
   group_by(village) |> 
   summarise(mean_cpue = mean(cpue)) |>
   arrange(desc(mean_cpue)) |> 
   print()
 
 fish.tanga.cpue = june.cpue |> 
   select(region, family, cpue) |> 
   filter(region == "Tanga") |> 
   group_by(family)|> 
   summarise(mean_cpue = mean(cpue)) |>
   arrange(desc(mean_cpue)) |> 
   print()
 
 fish.pemba.cpue = june.cpue |> 
   select(region, family, cpue) |> 
   filter(region == "Pemba") |> 
   group_by(family)|> 
   summarise(mean_cpue = mean(cpue)) |>
   arrange(desc(mean_cpue)) |> 
   print()
 
 month.cpue.june = june.cpue |>
   group_by(month, year, region) |> 
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
   facet_grid(~region)
 
 require(plotly)
 
 plotly(trend) 
 
 ggsave(filename = paste0(file_path,  "/catch_trends_6.jpg"), width = 10, height = 6, units = "in", dpi = 300)
 
 
 #### fishing ground ####
 
 fgrounds_cpue = june.cpue |> 
   select(fishing_ground, village, region, cpue) |> 
   group_by(fishing_ground, region) |> 
   summarize(
     mean_cpue = mean(cpue),
     number_village = n_distinct(village)) |>
   arrange(desc(mean_cpue)) 
 
 
fgrounds_cpue_tanga = fgrounds_cpue|> 
  filter(region == "Tanga") |> 
  top_n(5, wt = mean_cpue) |> 
  arrange(desc(mean_cpue)) |> 
  print()

fgrounds_cpue_pemba = fgrounds_cpue|> 
  filter(region == "Pemba") |> 
  top_n(5, wt = mean_cpue) |> 
  arrange(desc(mean_cpue)) |> 
  print()


fgrounds_use_tanga = fgrounds_cpue|> 
  filter(region == "Tanga") |> 
  top_n(5, wt = number_village) |> 
  arrange(desc(number_village)) |> 
  print() 

fgrounds_use_pemba = fgrounds_cpue|> 
  filter(region == "Pemba") |> 
  top_n(5, wt = number_village) |> 
  arrange(desc(number_village)) |> 
  print()


#### gear ####

gear.use = catch.june |> 
  select(gear, region) |> 
  filter(!is.na(gear)) |> 
  group_by(region, gear) |> 
  summarize(
    gear_count = n()
  ) |> 
  arrange(desc(gear_count)) |> 
  print()

 gear.use |> 
  pivot_wider(names_from =region, values_from = gear_count ) |> 
  print()

gear.cpue.june = catch.june |> 
  select(date, day,month,year, village, region, fisher_number, fisher_name, number_per_group, 
         fishing_ground, total_weight_g,total_number,family, gear) |>
  filter(!is.na(gear)) |> 
  group_by( region, gear) |>
  summarise(
    total_catch = sum(total_weight_g, na.rm = TRUE),
    total_fishers = sum(number_per_group, na.rm = TRUE),
    cpue = round((total_catch/total_fishers),2)) |>
  print()

gear.cpue.june = gear.cpue.june |> 
  group_by(region, gear) |> 
  summarize(mean_gear_cpue = mean(cpue)) |> 
  arrange(desc(mean_gear_cpue)) |> 
  print()


#### vessel ####

vessel.use = catch.june |> 
  select(method, region) |> 
  filter(!is.na(method)) |> 
  group_by(region, method) |> 
  summarize(
    vessel_count = n()
  ) |> 
  arrange(desc(vessel_count)) |> 
  print()  

#### family trends ####

reef.family.composition = june.cpue |> 
  select(region, family, cpue) |> 
  filter(family %in% c("Siganidae","Mullidae","Lethrinidae","Scaridae","Acanthuridae","Labridae","Lutjanidae","Carangidae","Serranidae","Chaetodontidae", "Haemulidae" )) |>
  group_by(family, region) |>
  summarise(mean_cpue = mean(cpue)) |> 
  arrange(desc(mean_cpue)) |> 
  print()

reef.family.trend = june.cpue |> 
  select(region, family, month, year, cpue) |> 
  filter(family %in% c("Siganidae","Mullidae","Lethrinidae","Scaridae","Acanthuridae","Labridae","Lutjanidae","Carangidae","Serranidae","Chaetodontidae", "Haemulidae" )) |>
  group_by(month, year, family, region) |>
  summarise(mean_cpue = mean(cpue)) |>
  print()


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
  theme(axis.text.x = element_text(angle = 90, hjust = 1, face = "bold"),
        axis.text.y = element_text(size = 12, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"))+
  facet_wrap(~region)

ggsave(filename = paste0(file_path,  "/family_heatmap_6.jpg"), width = 10, height = 6, units = "in", dpi = 300)


#### MAP ####

baf.grounds.maps |> 
  mutate(village = case_when(village == "Mtambwe_Kusini" ~ "Mtambwe kusini",
                             village == "Kwale_Tanga" ~ "Kwale Tanga",
                             village == "Kisiwa_Panza" ~ "Kisiwa Panza",
                             TRUE~village))

fgrounds.cpue.map=fgrounds.cpue |>
  left_join(baf.grounds.maps, by = c("fishing_ground", "region"))

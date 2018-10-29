library(tidyverse)
library(googlesheets)

#load drivers worksheet
drivers_gsheet <- gs_title("Physical Drivers of Biogenic Habitat Types for COCA")
drivers <- drivers_gsheet %>% gs_read("Parameters") %>%
  mutate(`Biogenic Habitat/Species` = str_replace(`Biogenic Habitat/Species`, "Clam Bed", "Clams"))


#filter out and create separate substrate table
substrates <- filter(drivers, `Limiting Driver`=="Substrate Type")
drivers_only <- filter(drivers, `Limiting Driver`!="Substrate Type")

subs_translate <- tribble(
  ~`Level or Value`, ~`Substrate Type`,
  "Cobble",   "Gravel",
  "Cobble",   "Gravel with rock",
  "Cobble",   "Gravel with sand",
  "Cobble",   "Rock with gravel",
  "Cobble",   "Sand with gravel",
  "(referring to soft shell clams) sand, coarse gravel, and compact clay substrates", "Gravel with mud",
  "(referring to soft shell clams) sand, coarse gravel, and compact clay substrates", "Mud with gravel",
  "(referring to soft shell clams) sand, coarse gravel, and compact clay substrates", "Mud with sand",
  "(referring to soft shell clams) sand, coarse gravel, and compact clay substrates", "Sand with mud",
  "(referring to soft shell clams) sand, coarse gravel, and compact clay substrates", "Sand with gravel",
  "(referring to soft shell clams) sand, coarse gravel, and compact clay substrates", "Sand",
  "(referring to soft shell clams) sand, coarse gravel, and compact clay substrates", "Mud",
  "(referring to soft shell clams) sand, coarse gravel, and compact clay substrates", "Gravel",
  "can grow equally well on rocky bottoms or on mud capable of supporting their weight.", "Gravel",
  "can grow equally well on rocky bottoms or on mud capable of supporting their weight.", "Mud",
  "can grow equally well on rocky bottoms or on mud capable of supporting their weight.", "Rock",
  "can grow equally well on rocky bottoms or on mud capable of supporting their weight.", "Rock with mud",
  "can grow equally well on rocky bottoms or on mud capable of supporting their weight.", "Rock with gravel",
  "can grow equally well on rocky bottoms or on mud capable of supporting their weight.", "Rock with sand",
  "can grow equally well on rocky bottoms or on mud capable of supporting their weight.", "Mud with sand",
  "can grow equally well on rocky bottoms or on mud capable of supporting their weight.", "Mud with rock",
  "can grow equally well on rocky bottoms or on mud capable of supporting their weight.", "Mud with gravek",
  "50", "Gravel", #refers to 50% silty bottom
  "50", "Sand",
  "50", "Sand with gravel",
  "50", "Sand with rock",
  "50", "Sand with mud",
  "50", "Gravel with mud",
  "50", "Gravel with sand",
  "50", "Gravel with rock",
  "coarse gravel/small cobble to rocky incl boulder", "Gravel",
  "coarse gravel/small cobble to rocky incl boulder", "Rock",
  "coarse gravel/small cobble to rocky incl boulder", "Gravel with mud",
  "coarse gravel/small cobble to rocky incl boulder", "Rock with mud",
  "coarse gravel/small cobble to rocky incl boulder", "Gravel with rock",
  "coarse gravel/small cobble to rocky incl boulder", "Rock with gravel",
  "coarse gravel/small cobble to rocky incl boulder", "Gravel with sand",
  "coarse gravel/small cobble to rocky incl boulder", "Rock with sand",
  "Rocky","Rock",
  "Rocky","Rock with mud",
  "Rocky","Rock with sand",
  "Rocky","Rock with gravel",
)

substrates_joined <- left_join(substrates, subs_translate) %>%
  select(-Units)

#get min/max by driver type for depth, flow, temperature
#first, come cleaning
drivers_minmax <- drivers_only %>%
  filter(`Limiting Driver` %in% c("Temperature", "Depth", 
                                  "Mean Flow Velocity",
                                  "Max Flow Velocity")) %>%
  mutate(`Units` = str_replace(Units, "m MLW", "m"),
         `Units` = str_replace(Units, "Cel[s|c]ius", "C"),
         `Units` = str_replace(Units, "Meters", "m"),
         `Units` = str_replace(Units, "cm/sec", "cm/s"),
         `Units` = str_replace(Units, "meters/second", "m/s"),
         `Level or Value` = str_replace_all(`Level or Value`, " ", ""),
         `Level or Value` = str_replace(`Level or Value`, ".*(\\d+)-(\\d\\d)", "\\2"),
         `Level or Value` = as.numeric(`Level or Value`)) %>%
  mutate(`Limiting Driver` = str_replace(`Limiting Driver`, "(Mean|Max) ", "")) %>%
  mutate(`Level or Value` = ifelse(`Units` == "cm/s", `Level or Value`/100, `Level or Value`)) %>%
  mutate(`Units` = ifelse(Units == "cm/s", "m/s", `Units`)) %>%
  mutate(`Level or Value` = abs(`Level or Value`)) #to deal with weird depth thing

#there can be only one per species
drivers_minmax_one <- drivers_minmax %>%
  group_by(`Biogenic Habitat/Species`, `Limiting Driver`, `Max, Min, or Mean`) %>%
  summarize(Units = Units[1L],
    Value = ifelse(`Max, Min, or Mean` == "Max", 
                            max(`Level or Value`, na.rm=T),
                            min(`Level or Value`, na.rm=T))[1])


write_csv(drivers_minmax_one, "../derived_data/drivers_minmax.csv")
write_csv(substrates_joined, "../derived_data/substrates.csv")

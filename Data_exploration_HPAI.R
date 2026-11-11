#project exploration for env stats

library(readr)
library(tidyverse)
library(tigris)
library(sf)
library(lubridate)
library(dplyr)
library(lwgeom)
library(stringr)
library(ggplot2)

#bring in data 
hpai_mammals <- read_csv("hpai-mammals.csv")
hpai_birds <- read_csv("hpai-wild-birds.csv")

#lets start with some data formating and graphing to explore our data 

options(tigris_use_cache = TRUE)

# 1 Get County Polygons for graphing 
counties <- counties(cb = TRUE, year = 2021, progress_bar = FALSE) %>%
  filter(as.numeric(STATEFP) <= 56) %>%   # drops non US states
  shift_geometry() %>%
  left_join(
    tibble(STATEFP = fips_codes$state_code,
           State   = fips_codes$state_name) %>% distinct(),
    by = "STATEFP"
  ) %>%
  mutate(county_key = str_to_lower(NAME))

# 2 Clean the county names cus I'm getting errors
clean_county <- function(x) {
  x %>% str_to_lower() %>%
    str_remove_all("\\b(county|parish|borough|census area)\\b") %>%
    str_remove_all("\\.") %>%
    str_squish()
}

cases_mammals <- hpai_mammals %>%
  mutate(county_key = clean_county(County),
         State = str_to_title(State)) %>%   
  count(State, county_key, name = "n_cases")

# 3 Join and plot
map_df <- counties %>%
  mutate(county_key = clean_county(NAME)) %>%
  left_join(cases_mammals, by = c("State", "county_key"))

ggplot(map_df) +
  geom_sf(aes(fill = n_cases), color = "grey85", linewidth = 0.1) +
  scale_fill_viridis_c(option = "magma", direction = -1,
                       trans = "log10", na.value = "grey95",
                       name = "Detections\n(log scale)") +
  theme_void() +
  labs(title = "HPAI detections in mammals by county")

# 4 Heyyyy all the counties match with 
anti_join(cases_mammals, st_drop_geometry(map_df) %>% distinct(State, county_key),
          by = c("State", "county_key")) %>%
  arrange(desc(n_cases))

#lets look at time now 

#date formating for the by time graph 

hpai_mammals <- hpai_mammals %>%
  mutate(date_collected = mdy(`Date Collected`),  
         year_month     = floor_date(date_collected, "month"),
         year           = year(date_collected),
         month          = month(date_collected, label = TRUE, abbr = TRUE))

# check for dates that failed to parse
sum(is.na(hpai_mammals$date_collected))

# 1 Detections per month
hpai_mammals %>%
  filter(!is.na(year_month)) %>%
  count(year_month) %>%
  ggplot(aes(year_month, n)) +
  geom_col(fill = "steelblue") +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = NULL, y = "Detections", title = "HPAI mammal detections by month")

# 2 Seasonality
hpai_mammals %>%
  filter(!is.na(date_collected)) %>%
  count(year, month) %>%
  ggplot(aes(month, n, group = factor(year), color = factor(year))) +
  geom_line(linewidth = 1) + geom_point() +
  theme_minimal() +
  labs(x = NULL, y = "Detections", color = "Year",
       title = "Seasonality of mammal HPAI detections")

#lets look at family break down, there are lots of species so will likly need to group in some way for stats

hpai_mammals <- hpai_mammals %>%
  mutate(captive = str_detect(Species, "\\(captive\\)"),
         species_clean = Species %>% str_remove("\\s*\\(captive\\)") %>%
           str_squish() %>% str_to_lower())

hpai_mammals <- hpai_mammals %>%
  mutate(
    group = case_when(
      species_clean == "domestic cat" ~ "Domestic cat",
      species_clean %in% c("bobcat", "mountain lion", "canada lynx", "eurasian lynx",
                           "african lion", "amur leopard", "tiger", "amur tiger",
                           "bengal tiger", "hybrid tiger", "geoffroy's cat", "serval",
                           "savannah cat") ~ "Non-domestic felid",
      species_clean %in% c("red fox", "gray fox", "arctic fox", "catalina island fox",
                           "fox (unidentified)", "coyote") ~ "Canid",
      species_clean %in% c("american black bear", "black bear", "grizzly bear",
                           "kodiak bear", "polar bear") ~ "Bear",
      species_clean %in% c("american mink", "american marten", "fisher", "ermine",
                           "long-tailed weasel", "north american river otter") ~ "Mustelid",
      species_clean %in% c("raccoon", "striped skunk", "skunk (unidentified)") ~ "Raccoon & skunk",
      species_clean %in% c("bottlenose dolphin", "california sea lion", "grey seal",
                           "harbor seal", "northern elephant seal", "sea otter") ~ "Marine mammal",
      species_clean %in% c("house mouse", "deer mouse", "black rat", "norway rat",
                           "prairie vole", "muskrat", "abert's squirrel",
                           "eastern gray squirrel", "round-tailed ground squirrel",
                           "thirteen-lined ground squirrel") ~ "Rodent",
      species_clean == "desert cottontail" ~ "Lagomorph",
      species_clean == "virginia opossum" ~ "Opossum",
      TRUE ~ NA_character_
    ),
    family = case_when(
      group %in% c("Domestic cat", "Non-domestic felid") ~ "Felidae",
      group == "Canid" ~ "Canidae",
      group == "Bear" ~ "Ursidae",
      species_clean == "raccoon" ~ "Procyonidae",
      species_clean %in% c("striped skunk", "skunk (unidentified)") ~ "Mephitidae",
      species_clean %in% c("bottlenose dolphin") ~ "Delphinidae",
      species_clean == "california sea lion" ~ "Otariidae",
      species_clean %in% c("grey seal", "harbor seal", "northern elephant seal") ~ "Phocidae",
      group == "Mustelid" | species_clean == "sea otter" ~ "Mustelidae",
      species_clean %in% c("house mouse", "black rat", "norway rat") ~ "Muridae",
      species_clean %in% c("deer mouse", "prairie vole", "muskrat") ~ "Cricetidae",
      str_detect(species_clean, "squirrel") ~ "Sciuridae",
      group == "Lagomorph" ~ "Leporidae",
      group == "Opossum" ~ "Didelphidae",
      TRUE ~ NA_character_
    )
  )

table(hpai_mammals$family)


#and now time for the birds

cases_birds <- hpai_birds %>%
  mutate(county_key = clean_county(County),
         State = str_to_title(State)) %>%   
  count(State, county_key, name = "n_cases")

map_df <- counties %>%
  mutate(county_key = clean_county(NAME)) %>%
  left_join(cases_birds, by = c("State", "county_key"))

ggplot(map_df) +
  geom_sf(aes(fill = n_cases), color = "grey85", linewidth = 0.1) +
  scale_fill_viridis_c(option = "magma", direction = -1,
                       trans = "log10", na.value = "grey95",
                       name = "Detections\n(log scale)") +
  theme_void() +
  labs(title = "HPAI detections in wild birds by county")

# 4. Are some issues, but can't do anything with the unknowns and i'm not too workies about the Alaska and DC one, can fix if it actually gets picked up as project 
anti_join(cases_birds, st_drop_geometry(map_df) %>% distinct(State, county_key),
          by = c("State", "county_key")) %>%
  arrange(desc(n_cases))

hpai_birds <- hpai_birds %>%
  filter(`Collection Date` != "Unknown") %>%
  mutate(date_collected = mdy(`Collection Date`),   
         year_month     = floor_date(date_collected, "month"),
         year           = year(date_collected),
         month          = month(date_collected, label = TRUE, abbr = TRUE))

hpai_birds %>%
  filter(!is.na(year_month)) %>%
  count(year_month) %>%
  ggplot(aes(year_month, n)) +
  geom_col(fill = "steelblue") +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = NULL, y = "Detections", title = "HPAI bird detections by month")

hpai_birds <- hpai_birds %>%
  mutate(
    species_clean = str_to_lower(str_squish(`Bird Species`)),
    unidentified  = str_detect(species_clean, "unidentified"),
    
    # exotic or domestic birds, probably zoo/farm/pet rather than wild migrants
    likely_captive = species_clean %in% c(
      "muscovy duck","african goose","peafowl (unidentified)","chilean flamingo",
      "lesser flamingo","flamingo (unidentified)","greater rhea","rhea (unidentified)","emu",
      "macaw (unidentified)","african gray parrot","lorikeet (unidentified)","kookaburra (unidentified)",
      "african crowned crane","blue crane","king vulture","scarlet ibis","plush-crested jay",
      "southern screamer","red-breasted goose","swan goose","baer's pochard","baikal teal",
      "scaly-sided merganser","smew","egyptian goose","brazilian teal","puna teal","black swan",
      "black-necked swan","dalmatian pelican","nene","whooper swan"),
    
    family = case_when(
      species_clean %in% c("canada goose","black-bellied whistling duck","mallard","american black duck",
                           "green-winged teal","blue-winged teal","northern shoveler","northern pintail",
                           "muscovy duck","cinnamon teal","wood duck","trumpeter swan","redhead","gadwall",
                           "mute swan","hooded merganser","common eider","snow goose","cackling goose",
                           "greater white-fronted goose","bufflehead","ross's goose","swan (unidentified)",
                           "common merganser","red-breasted merganser","surf scoter","tundra swan","brant",
                           "goose (unidentified)","duck (unidentified)","african goose","american wigeon",
                           "ring-necked duck","whooper swan","lesser scaup","canvasback",
                           "mallard/black duck hybrid","ruddy duck","greater scaup","common goldeneye",
                           "black scoter","white-winged scoter","mottled duck","red-breasted goose",
                           "long-tailed duck","fulvous whistling-duck","merganser (unidentified)",
                           "eider (unidentified)","teal (unidentified)","egyptian goose","brazilian teal",
                           "puna teal","swan goose","scaly-sided merganser","smew","baer's pochard",
                           "baikal teal","black swan","black-necked swan","nene",
                           "wild hybrid duck (anatidae)") ~ "Anatidae",
      species_clean %in% c("california gull","ring-billed gull","american herring gull",
                           "great black-backed gull","caspian tern","glaucous gull","black-legged kittiwake",
                           "common tern","western gull","gull (unidentified)","bonaparte's gull",
                           "lesser black-backed gull","black skimmer","laughing gull","sabine's gull",
                           "arctic tern","short-billed gull","thayer's gull","glaucous-winged gull",
                           "royal tern","sandwich tern","forster's tern") ~ "Laridae",
      species_clean == "parasitic jaeger" ~ "Stercorariidae",
      species_clean %in% c("common murre","razorbill") ~ "Alcidae",
      species_clean %in% c("red-tailed hawk","bald eagle","red-shouldered hawk","golden eagle",
                           "cooper's hawk","swainson's hawk","sharp-shinned hawk","hawk (unidentified)",
                           "rough-legged hawk","northern harrier","ferruginous hawk","harris's hawk",
                           "broad-winged hawk","american goshawk","eagle (unidentified)") ~ "Accipitridae",
      species_clean %in% c("turkey vulture","black vulture","vulture (unidentified)","king vulture",
                           "california condor") ~ "Cathartidae",
      species_clean %in% c("peregrine falcon","american kestrel","merlin","prairie falcon",
                           "falcon (unidentified)","crested caracara") ~ "Falconidae",
      species_clean == "osprey" ~ "Pandionidae",
      species_clean %in% c("great horned owl","barred owl","snowy owl","owl (unidentified)",
                           "eastern screech owl","long-eared owl","western screech owl","burrowing owl",
                           "short-eared owl") ~ "Strigidae",
      species_clean == "american barn owl" ~ "Tytonidae",
      species_clean %in% c("american crow","common raven","fish crow","crow (unidentified)","blue jay",
                           "magpie (unidentified)","black-billed magpie","plush-crested jay") ~ "Corvidae",
      species_clean %in% c("eurasian collared dove","rock pigeon","mourning dove","white-winged dove",
                           "pigeon (unidentified)") ~ "Columbidae",
      species_clean %in% c("sandhill crane","whooping crane","african crowned crane","blue crane",
                           "crane (unidentified)") ~ "Gruidae",
      species_clean %in% c("american white pelican","pelican (unidentified)","brown pelican",
                           "dalmatian pelican") ~ "Pelecanidae",
      species_clean %in% c("brandt's cormorant","cormorant (unidentified)","double-crested cormorant",
                           "neotropic cormorant") ~ "Phalacrocoracidae",
      species_clean == "northern gannet" ~ "Sulidae",
      species_clean %in% c("black-crowned night heron","little blue heron","great blue heron","snowy egret",
                           "great egret","green heron","heron (unidentified)","western cattle-egret") ~ "Ardeidae",
      species_clean %in% c("roseate spoonbill","white-faced ibis","glossy ibis","scarlet ibis") ~ "Threskiornithidae",
      species_clean == "wood stork" ~ "Ciconiidae",
      species_clean %in% c("eared grebe","pied-billed grebe","horned grebe","red-necked grebe",
                           "western grebe") ~ "Podicipedidae",
      species_clean %in% c("common loon","pacific loon") ~ "Gaviidae",
      species_clean %in% c("short-tailed shearwater","northern fulmar") ~ "Procellariidae",
      species_clean %in% c("dunlin","marbled godwit","red-necked phalarope","sanderling","western sandpiper",
                           "greater yellowlegs","lesser yellowlegs","american woodcock","ruddy turnstone",
                           "black turnstone","willet") ~ "Scolopacidae",
      species_clean %in% c("killdeer","snowy plover","black-bellied plover") ~ "Charadriidae",
      species_clean == "black-necked stilt" ~ "Recurvirostridae",
      species_clean == "american coot" ~ "Rallidae",
      species_clean %in% c("chilean flamingo","lesser flamingo","flamingo (unidentified)") ~ "Phoenicopteridae",
      species_clean %in% c("wild turkey","greater sage grouse","ruffed grouse","ring-necked pheasant",
                           "pheasant (unidentified)","peafowl (unidentified)") ~ "Phasianidae",
      species_clean %in% c("gambel's quail","california quail") ~ "Odontophoridae",
      species_clean == "house sparrow" ~ "Passeridae",
      species_clean %in% c("savannah sparrow","lincoln's sparrow","song sparrow","dark-eyed junco",
                           "white-crowned sparrow","lark sparrow") ~ "Passerellidae",
      species_clean == "sparrow (unidentified)" ~ "Passeriformes (unid.)",
      species_clean %in% c("western meadowlark","great-tailed grackle","common grackle","brewer's blackbird",
                           "boat-tailed grackle","red-winged blackbird","grackle (unidentified)",
                           "blackbird (unidentified)") ~ "Icteridae",
      species_clean == "european starling" ~ "Sturnidae",
      species_clean %in% c("bluebird (unidentified)","hermit thrush","american robin") ~ "Turdidae",
      species_clean %in% c("finch (unidentified)","house finch") ~ "Fringillidae",
      species_clean == "horned lark" ~ "Alaudidae",
      species_clean %in% c("barn swallow","swallow (unidentified)","tree swallow") ~ "Hirundinidae",
      species_clean %in% c("say's phoebe","western kingbird") ~ "Tyrannidae",
      species_clean == "warbler (unidentified)" ~ "Parulidae",
      species_clean == "lark bunting" ~ "Calcariidae",
      species_clean == "summer tanager" ~ "Cardinalidae",
      species_clean %in% c("macaw (unidentified)","african gray parrot") ~ "Psittacidae",
      species_clean == "lorikeet (unidentified)" ~ "Psittaculidae",
      species_clean == "kookaburra (unidentified)" ~ "Alcedinidae",
      species_clean %in% c("greater rhea","rhea (unidentified)") ~ "Rheidae",
      species_clean == "emu" ~ "Casuariidae",
      species_clean == "southern screamer" ~ "Anhimidae",
      TRUE ~ NA_character_
    )
  )
table(hpai_birds$family)

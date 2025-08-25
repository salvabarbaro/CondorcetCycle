# Libraries
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(dplyr)

# Example data frame with countries to highlight
load("~/Documents/Research/Elections/AnnaProjects/CondorcetParadox/mogon/d.RData")
## iso3c = IMD1006_UNALPHA3
#df <- d[,1:15] %>% filter(., IMD1006_UNALPHA3 != "RUS")
#rm(d)
cou <- sort(unique(d$IMD1006_UNALPHA3))
rm(d); gc()
#cou

world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  mutate(highlight = ifelse(adm0_a3 %in% cou, "Selected", "Other"))


#world <- world %>%
#  mutate(highlight = ifelse(adm0_a3 %in% cou, "Selected", "Other"))
oceans <- ne_download(scale = "medium", 
                      type = "ocean", category = "physical", 
                      returnclass = "sf")

ggplot() +
  geom_sf(data = oceans, fill = "aliceblue", color = NA) +   # oceans layer
  geom_sf(data = world, aes(fill = highlight), color = "grey30") +
  scale_fill_manual(values = c("Selected" = "#C1002A", "Other" = "lightgrey")) +
  coord_sf(crs = st_crs("+proj=robin"), expand = FALSE, datum = NA) +
  theme_minimal() +
  theme(legend.position = "none")
#ggsave("~/Documents/Research/Elections/AnnaProjects/CondorcetParadox/git/6698fdf61cc3013a205e1eb4/PUCH/Presentation/map.pdf")

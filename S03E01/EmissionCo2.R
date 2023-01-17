# install.packages('maps')
library(dplyr)
library(ggplot2)
library(reshape)
require(maps)

fichierSource <-
  "C:/Users/nath_/Documents/BDD/co2_emissions_tonnes_per_person.csv"

g <- read.csv(fichierSource,
              encoding = "UTF-8")

g2 <-
  g %>% filter(
    country == "Portugal" |
      country == "Spain" |
      country == "France" |
      country == "Switzerland" | country == "Germany" | country ==
      "Austria" |
      country == "Belgium" |
      country == "UK" | country == "Netherlands" | country ==
      "Denmark" |
      country == "Poland" | country == "Italy" | country ==
      "Croatia" |
      country == "Slovenia" |
      country == "Hungary" | country == "Slovakia" | country ==
      "Czech republic"
  )

for (col in 1:ncol(g2)) {
  colnames(g2)[col] <- sub("X", "", colnames(g2)[col])
}

g3 <- melt(as.data.table(g2))

## POINTS 

mid <- mean(g3$value, na.rm = T)

pointsAnimes <- ggplot(data = g3) +
  geom_point(aes(x = value,
                 y = country, color = value)) + scale_color_gradient2(
                   midpoint = mid,
                   low = "blue",
                   high =
                     "red",
                   space = "Lab"
                 ) +
  labs(
    title = "Emissions de CO2",
    subtitle = "En {closest_state}",
    x = "En tonnes par personne",
    y = "Pays",
    caption = "Source : Data by Gapminder"
  ) +
  transition_states(
    states = variable,
    transition_length = 1,
    state_length = 2
  )

## Animation des points 
pointsAnimes

# MAPS

world_map <- map_data("world")
# ggplot(world_map, aes(x = long, y = lat, group = group)) +
#   geom_polygon(fill="lightgray", colour = "white")

# Some Contries
some.countries <- c(
  "Portugal", "Spain", "France", "Switzerland", "Germany",
  "Austria", "Belgium", "UK", "Netherlands",
  "Denmark", "Poland", "Italy", 
  "Croatia", "Slovenia", "Hungary", "Slovakia",
  "Czech republic"
)
# some.countries <- c(
#   "French Polynesia"
# )

some.maps <- map_data("world", region = some.countries)

region.lab.data <- some.maps %>%
  group_by(region) %>%
  summarise(long = mean(long), lat = mean(lat))

# ggplot(some.maps, aes(x = long, y = lat)) +
#   geom_polygon(aes( group = group, fill = region))+
#   geom_text(aes(label = region), data = region.lab.data,  size = 3, hjust = 0.5)+
#   scale_fill_viridis_d()+
#   theme_void()+
#   theme(legend.position = "none")

g4 <- g3 %>% dplyr::mutate(region = country)

jointure <- left_join(g4, world_map, by = "region")

cartes <- ggplot(jointure, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = value), color = "black") +
  scale_fill_viridis_c(option = "C") +
  transition_manual(variable) +
  labs(
    title = "Emissions de CO2",
    caption = "Source : Data by Gapminder"
  )

animate(cartes)




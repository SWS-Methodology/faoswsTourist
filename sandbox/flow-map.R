library(dplyr)
library(rgdal)
library(leaflet)
library(networkD3)
library(RColorBrewer)

set.seed(1983)

df <- data_frame(origins = sample(c('Portugal', 'Romania', 'Nigeria', 'Peru'),
                                  size = 100, replace = TRUE),
                 destinations = sample(c('Texas', 'New Jersey', 'Colorado', 'Minnesota'),
                                       size = 100, replace = TRUE))

head(df)

##
df2 <- df %>%
  group_by(origins, destinations) %>%
  summarize(counts = n()) %>%
  ungroup() %>%
  arrange(desc(counts))

df2

##

# install.packages("networkD3")

name_vec <- c(unique(df2$origins), unique(df2$destinations))

nodes <- data.frame(name = name_vec, id = 0:7)

links <- df2 %>%
  left_join(nodes, by = c('origins' = 'name')) %>%
  rename(origin_id = id) %>%
  left_join(nodes, by = c('destinations' = 'name')) %>%
  rename(dest_id = id)


forceNetwork(Links = links, Nodes = nodes, Source = 'origin_id', Target = 'dest_id',
             Value = 'counts', NodeID = 'name', Group = 'id', zoom = TRUE)

##

sankeyNetwork(Links = links, Nodes = nodes, Source = 'origin_id', Target = 'dest_id',
              Value = 'counts', NodeID = 'name', fontSize = 16)

##

library(parsetR) # devtools::install_github("timelyportfolio/parsetR")

parset(df2, dimensions = c('origins', 'destinations'),
       value = htmlwidgets::JS("function(d){return d.counts}"),
       tension = 0.5)

##

library(rnaturalearth) # devtools::install_github('ropenscilabs/rnaturalearth')
# devtools::install_github("ropenscilabs/rnaturalearthdata")
library(rnaturalearthdata)
countries <- ne_countries()

states <- ne_states(iso_a2 = 'US')

##
# install.packages("rgdal")
library(rgdal)

countries$longitude <- coordinates(countries)[,1]

countries$latitude <- coordinates(countries)[,2]

countries_xy <- countries@data %>%
  select(admin, longitude, latitude)

states_xy <- states@data %>%
  select(name, longitude, latitude)

##

df3 <- df2 %>%
  left_join(countries_xy, by = c('origins' = 'admin')) %>%
  left_join(states_xy, by = c('destinations' = 'name'))

df3$longitude.y <- as.numeric(as.character(df3$longitude.y))

df3$latitude.y <- as.numeric(as.character(df3$latitude.y))

head(df3)

##

library(geosphere)

flows <- gcIntermediate(df3[,4:5], df3[,6:7], sp = TRUE, addStartEnd = TRUE)

flows$counts <- df3$counts

flows$origins <- df3$origins

flows$destinations <- df3$destinations

##
hover <- paste0(flows$origins, " to ",
                flows$destinations, ': ',
                as.character(flows$counts))

?brewer.pal
pal <- colorFactor(brewer.pal(4, 'Set2'), flows$origins)

leaflet() %>%
  addProviderTiles('CartoDB.Positron') %>%
  addPolylines(data = flows, weight = ~counts, label = hover,
               group = ~origins, color = ~pal(origins)) %>%
  addLayersControl(overlayGroups = unique(flows$origins),
                   options = layersControlOptions(collapsed = FALSE))

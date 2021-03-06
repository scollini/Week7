library(sp)
library(rgeos)
library(maptools)
library(ggmap)
library(ggplot2)
library(rgdal)
library(dplyr)
library(tidyr)
library(stringr)
library(historydata)
library(RColorBrewer)
library(classInt)
display.brewer.all()
devtools::install_github("hadley/lazyeval")
devtools::install_github("hadley/dplyr")

pop_1840 <- read.csv("~/Desktop/Clio-3/Week7/nhgis0009_ds7_1840_state.csv")

map_1840 <- readOGR("nhgis-shp/", "state_1840")

head(map_1840)
  
plot(map_1840)   

map_1840_sp <- fortify(map_1840, region = "GISJOIN")

map_1840_df <- fortify(map_1840_sp, region = "GISJOIN")

insane_1840 <- read.csv("~/Desktop/Clio-3/Week7/nhgis0010_ds7_or.csv",
                       stringsAsFactors = FALSE)

glimpse(map_1840_df)
glimpse(insane_1840)

p <- ggplot() + 
  geom_map(data = insane_1840, 
           aes(map_id = GISJOIN, fill = ColoredPublic),
           map = map_1840_sp) +
  expand_limits(x = map_1840_sp$long, y=map_1840_sp$lat) +
  scale_fill_gradient2(low="white", high="red")

print(p)

p <- ggplot() + 
  geom_map(data = insane_1840, 
           aes(map_id = GISJOIN, fill = ColoredPublic + ColoredPrivate),
           map = map_1840_sp) +
   expand_limits(x = map_1840_sp$long, y=map_1840_sp$lat) 
  scale_fill_gradient2(low="white", high="red")

print(p)

p <- ggplot() + 
  geom_map(data = insane_1840, 
           aes(map_id = GISJOIN, fill = WhitePublic + WhitePrivate),
           map = map_1840_sp) +
  expand_limits(x = map_1840_sp$long, y=map_1840_sp$lat) +
  scale_fill_gradient2(low="white", high="red")
print(p)

insane_merged <- map_1840_df %>%
left_join(insane_1840, by = c("id" = "GISJOIN"))

insane_merged_pop <- insane_merged %>%
  left_join(pop_1840, by = c("id" = "GISJOIN")) %>%
  mutate(White = WhitePublic + WhitePrivate) %>%
  mutate(NonWhite = ColoredPublic + ColoredPrivate) %>%
  mutate(Total = White + NonWhite) %>%
  mutate(percentpop = Total / ACD001) %>%
 
insane_total_map <- ggplot() + 
  geom_map(data = insane_merged_pop, 
           aes(map_id = id, fill = percentpop),
           map = map_1840_sp) +
  expand_limits(x = map_1840_sp$long, y=map_1840_sp$lat) +
  scale_fill_gradient2(low="white", high="red") 

print(insane_total_map)


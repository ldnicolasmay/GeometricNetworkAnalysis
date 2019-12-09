# GIS 
# - Vector data model doesn't have topology

library(sp)
library(rgdal)
library(sf)
library(ggplot2)
library(igraph)
library(shp2graph)
library(stplanr)
library(tigris)
library(leaflet)

ls <- st_linestring(x = matrix(c(1,2,3,4), nrow = 2, byrow = TRUE))
plot(ls, col = "red")

ls <- st_linestring(x = matrix(c(1,2,3,4,1,5), nrow = 3, byrow = TRUE))
plot(ls, col = "green")

ls <- st_linestring(x = matrix(c(1,2,3,4,1,1), nrow = 3, byrow = TRUE))
plot(ls, col = "red", add = TRUE)

print(ls)
class(ls)


# 

ls1.coord = matrix(c(-83.73,42.28,-83.74,42.28,-83.74,42.28,-83.74,42.28),
                   ncol = 2, byrow = TRUE)
ls1 = st_linestring(ls1.coord)
class(ls1)

ls1 = st_sfc(ls1)
# st_crs = set coordinate reference system
st_crs(ls1) = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
print(ls1)
# WGS84 = World Geodesic System 84 # Earth map reference system

coord1 = st_coordinates(st_geometry(ls1))
leaflet() %>% 
  addTiles(group = "OSM (default)") %>% 
  addPolylines(lng = coord1[,1], lat = coord1[,2])


# 
p1 = st_cast(ls1, "POINT") # Get each point from the linestring
p1 = st_coordinates(st_geometry(p1))

leaflet() %>% 
  addTiles(group = "OSM (default)") %>% 
  addPolylines(lng = coord1[,1], lat = coord1[,2]) %>% 
  addMarkers() # ...

# ...


# 
library(igraph)

g.empty = graph.empty(n = 10)
plot(g.empty)

g.full = graph.full(n = 10, directed = FALSE, loops = FALSE)
g = graph.formula(1-2, 1-3, 2-3, 2-4, 3-5, 4-5, 4-6, 4-7, 5-6, 6-7)
V(g)
E(g)
plot(g)
print(get.adjacency(g))

# directed graph
dg1 = graph.formula(1-+2, 2-+3, 1++4, 1+-5)
plot(dg1)
print(get.adjacency(dg1))

# different syntax
g1 = graph(edges = c(1,2), n = 10, directed = FALSE)
plot(g1)


# 

library(tigris)
library(tidycensus)

det = roads(state = "26", county = "163")
class(det)
plot(det)

road.detroit = st_read("./data", "detroit_road_edge", stringsAsFactors = FALSE)
class(road.detroit)
colnames(road.detroit)

road.detroit[1:10,]
dplyr::glimpse(road.detroit)

road.detroit <- road.detroit %>% select(OBJECTID, MTFCC, FULLNAME)

st_crs(road.detroit)

sps = "+proj=lcc +lat_1=42.1 +lat_2=43.66666666666666 +lat_0=41.5 +lon_0=-84.36666666666666 +x_0=4000000 +y_0=0 +ellps=GRS80 +units=m +no_defs"
road.detroit = st_transform(road.detroit, crs = sps)
library(shp2graph)
road.detroit.sldf = as(road.detroit, "Spatial")
nt.con = nt.connect(road.detroit.sldf)


# 
library(rgdal)
road.detroit = readOGR("./data", "subset", stringsAsFactors = FALSE)
class(road.detroit)

road.detroit@data = road.detroit@data[c("OBJECTID", "MTFCC", "FULLNAME")]

road.detroit = spTransform(road.detroit, sps)

nt.con = nt.connect(road.detroit)
road.detroit = readshpnw(nt.con, 
                         ELComputed = TRUE, 
                         longlat = FALSE, 
                         Detailed = FALSE)
w.length = road.detroit[[4]]
detroit.g = nel2igraph(road.detroit[[2]],
                       road.detroit[[3]],
                       weight = w.length)
v.g = V(detroit.g)
e.g = E(detroit.g)
print(v.g[1:3])
print(e.g[1:3])
v.attr = vertex_attr(detroit.g)
e.attr = edge_attr(detroit.g)

dt = distance_table(detroit.g, directed = FALSE)
class(dt)
View(dt)
shortest.path = shortest_paths(detroit.g, 
                               from = 1, to = 9, 
                               mode = "all", weights = NULL)

detroit.nw = points2network(nt.con, xy, approach = 2, ELComputed = TRUE,
                            Detailed = FALSE)


# census.var = load_variables(year = 2010, dataset = "sf1", cache = TRUE)
# apiKey = "10f0ddf18477c9831d301b2369a6db5b43c3b50a"
# 
# popu.data = get_decennial(geography = "block",
#                           variable = "P001001",
#                           state = "")












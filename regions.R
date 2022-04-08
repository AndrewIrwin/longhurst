# Make a nice map of selected Longhurst provinces


library(tidyverse)
# library(lubridate)
library(sf)
# Dropbox
# "Mirelle Naud Data cleaning/Longhurst/"
longhurst <- st_read(paste0("/Users/airwin/Dropbox/",
                            "Mirelle Naud Data cleaning/Longhurst/",
                            "longhurst_v4_2010/Longhurst_world_v4_2010.shp"))
longhurst$ProvCode  = as.character(longhurst$ProvCode)
longhurst$ProvCode[longhurst$ProvCode == "PEQD"] = "PEOD"  # typo

longhurst2 <- longhurst %>% 
  filter(ProvCode %in% c("GFST", "ARCT", "SARC", "NADR", "NWCS", "NAST", "NASW", "NASE", "NECS", "BPLR"))

# How many points to draw? 100-200k Lots!
sum(rapply(st_geometry(longhurst), nrow))
sum(rapply(st_geometry(longhurst2), nrow))

# MacOSX Quartz is very slow for these maps, or with a lot of points
# Cairo is OK, PDF is fine
if(!identical(getOption("bitmapType"), "cairo") && isTRUE(capabilities()[["cairo"]])){
  options(bitmapType = "cairo")
}

p1 <- ggplot() + 
  geom_sf(data = longhurst2, size = 0.25, color = "black", aes(fill=ProvCode)) + 
  ggtitle("Longhurst") + 
  coord_sf() +
  xlim(-90, 20) + ylim(27,85)
ggsave("map.pdf", p1)

library(sp)
library(ggmap)
library(rnaturalearth)
library(rnaturalearthdata)

world <- ne_countries(scale = "medium", returnclass = "sf", type="map_units")
# discrete_colours <- RColorBrewer::brewer.pal(12, "Set3")
# coord_sf(crs = "+proj=laea +lat_0=-90 +lon_0=0") # S pole
# coord_sf(crs = "+proj=ortho +lat_0=90") # N pole
# coord_sf(crs = "+proj=moll +lon_0=0") # Mollwiede, globe
#  coord_sf(crs = "+init=epsg:4326")
p2 <- ggplot(data = world) + geom_sf(fill="#A0A0A0", color="#808080") +
  # geom_sf(data = longhurst2 %>% mutate(geometry = st_transform(geometry, "+proj=eck4 +lat_0=+0 +lon_0=0")),
  geom_sf(data = longhurst2,
          size = 0.5, color = NA, aes(fill=ProvCode)) + 
  scale_fill_brewer(palette = "Set3") + 
  # coord_sf(crs = "+init=epsg:4326") +
  coord_sf(crs = 4326) +
  xlim(-80,10) + ylim(25, 75) +
  # coord_sf(crs = "+proj=eck4 +lat_0=+0 +lon_0=0") # slow and fails.
  theme_void()
ggsave("map2.pdf", p2)
## Show map with various boxes for different regions used in previous analyses

# addRectangles(c(-70, -35, -70, -35), c(30, 30, 50, 50),  # for leaflet map
#               c(-35, 0, -35, 0), c(50, 50, 70, 70))

regions <- tribble(~lat, ~lon, ~id, ~ref,
                   30, -70, 1, "Crispin", 50, -70, 1, "Crispin", 50, -35, 1, "Crispin", 30, -35, 1, "Crispin",
                   50, -70, 2, "Crispin", 78, -70, 2, "Crispin", 78, -35, 2, "Crispin", 50, -35, 2, "Crispin",
                   30, -35, 3, "Crispin", 50, -35, 3, "Crispin", 50, -0, 3, "Crispin", 30, -0, 3, "Crispin",
                   50, -35, 4, "Crispin", 78, -35, 4, "Crispin", 78, -0, 4, "Crispin", 50, -0, 4, "Crispin",
                   45, -15, 5, "Hinder", 45, 10, 5, "Hinder", 60, 10, 5, "Hinder", 60, -15, 5, "Hinder",
                   51.5, -79.5, 6, "Leterme", 51.5, -45, 6, "Leterme", 74.5, -45, 6, "Leterme", 74.5, -79.5, 6, "Leterme",
                   51.5, -79.5, 7, "Leterme", 51.5, -45, 7, "Leterme", 21.5, -45, 7, "Leterme", 21.5, -79.5, 7, "Leterme",
                   51.5, -20, 8, "Leterme", 51.5, -45, 8, "Leterme", 74.5, -45, 8, "Leterme", 74.5, -20, 8, "Leterme",
                   51.5, -20, 9, "Leterme", 51.5, -45, 9, "Leterme", 21.5, -45, 9, "Leterme", 21.5, -20, 9, "Leterme",
                   51.5, -20, 10, "Leterme", 51.5, 15, 10, "Leterme", 74.5, 15, 10, "Leterme", 74.5, -20, 10, "Leterme",
                   51.5, -20, 11, "Leterme", 51.5, 15, 11, "Leterme", 21.5, 15, 11, "Leterme", 21.5, -20, 11, "Leterme",
)

# EPSG 4326 - e.g. lat/lon coordinate map
target_crs <- 4326
  
ggplot(data = world ) + 
  geom_sf(fill="#A0A0A0", color="#808080") +
  geom_polygon(data = regions, aes(x=lon, y=lat, group=id, color=ref), fill="transparent") +
  # geom_point(data = bind_rows(diatoms, dinos) %>% select(latitude, longitude) %>% distinct(),
  #           aes(longitude, latitude)) + 
  geom_point(data = temp_table %>% filter(!is.na(cell_mass)) %>%
                      # mutate(year = year(eventDate), month = month(eventDate)) %>%
               mutate(decimalLatitude = round(decimalLatitude, 1), 
                      decimalLongitude = round(decimalLongitude, 1)) %>%
               select(decimalLatitude, decimalLongitude) %>% 
               filter(decimalLatitude >= 20, decimalLongitude >= -80, decimalLongitude <= 17) %>% distinct() ,
             aes(decimalLongitude, decimalLatitude), size = 0.1, alpha = 0.5) + 
  coord_sf(default_crs = "+init=epsg:4326") + 
  xlim(-80,17) + ylim(20, 80) +
  theme_void()

sf_use_s2(TRUE) # default, FALSE fails
target_crs <- "+proj=eck6 +lon_0=-30" 
# target_crs <- "+proj=eck4 +lat_0=50 +lon_0=-50"
display_window <- st_sfc(st_point(c(-80, 17)), st_point(c(20, 80)), crs = 4326) 
disp_win_coord <- st_coordinates(display_window %>% sf::st_transform(crs = target_crs))
north_atlantic <- world %>% 
  # st_crop(xmin = -80, xmax = 17, ymin = 20, ymax = 80) %>% 
  st_make_valid() %>%
  st_intersection(st_sfc(make_poly(-80, 20, 17, 80), crs = 4326) %>% sf::st_transform(crs = "WGS84"))  %>%
  # smoothr::densify(max_distance = 1) %>%
  sf::st_transform(crs = target_crs) 
  # st_intersection(st_sfc(make_poly(-80, 20, 17, 79), crs = 4326) %>% sf::st_transform(crs = target_crs)) 
# north_atlantic$geometry %>% s2::s2_rebuild() %>% sf::st_as_sfc() # fails
g <- st_graticule(north_atlantic)

# Make sf objects for points and polygons
cpr_survey_points <- st_sfc(st_multipoint(
  bind_rows(diatoms, dinos) %>% select(latitude, longitude) %>% distinct() %>% select(longitude, latitude) %>% as.matrix),
  crs = 4326) %>% sf::st_transform(crs = target_crs)
g <- st_graticule(north_atlantic) %>% sf::st_transform(crs = target_crs)
make_poly <- function(l, b, r, t) {
  st_polygon(list(matrix(c(l, b, l, t, r, t, r, b, l, b), ncol=2, byrow = TRUE)))
}
summary_regions <- st_sfc(
  make_poly(-70, 30, -35, 50),
  make_poly(-70, 50, -35, 78),
  make_poly(-35, 30, 0, 50),
  make_poly(-35, 50, 0, 78),
  make_poly(-15, 45, 10, 60),
  make_poly(-79.5, 51.5, -45, 74.5),
  make_poly(-79.5, 21.5, -45, 51.5),
  make_poly(-45, 51.5, -20, 74.5),
  make_poly(-45, 21.5, -20, 51.5),
  make_poly(-20, 51.5, 15, 74.5),
  make_poly(-20, 21.5, 15, 51.5),
  crs = 4326) %>% sf::st_transform(crs = target_crs) %>% 
  st_as_sf() %>% mutate(reference = c(rep("Mutshinda", 4), 
                                      rep("Hinder", 1),
                                      rep("Leterme", 6)))

ggplot(data = north_atlantic ) + 
  geom_sf(fill="#A0A0A0", color="#808080") +
  geom_sf(data = summary_regions, # fill="transparent", 
          aes(color = reference, fill = reference), alpha = 0.2) +
  geom_sf(data = cpr_survey_points) + 
  # coord_sf(default_crs = "+init=epsg:4326") + 
  # coord_sf(crs = "+init=epsg:4326") +  
  # coord_sf(default_crs = sf::st_crs(4326), expand=FALSE) +
  # coord_sf(xlim = c(-80,17), ylim = c(20, 80) ) +
  # coord_sf(crs = "+proj=eck4 +lat_0=+0 +lon_0=0") +  +
  #  coord_sf(xlim = disp_win_coord[,'X'], ylim = disp_win_coord[,'Y'],
  #           datum = target_crs, expand = FALSE) +
  theme_void() 

########################################################

## Read CPR data from Crispin
## Plot counts by region (richness, observations, etc, etc.)

diatoms <- read.delim("~/Dropbox/post-doc folders/Crispin/MS in progress/diatVSdinof/Data/processed_cprData/aggregate_diatData.txt", sep =" ")
dinos <- read.delim("~/Dropbox/post-doc folders/Crispin/MS in progress/diatVSdinof/Data/processed_cprData/aggregate_dinofData.txt", sep =" ")
# diatoms %>% count(Month, latitude, longitude)
# diatoms %>% count(latitude, longitude) %>%
dinos %>% count(latitude, longitude) %>%
  ggplot() + 
  geom_tile(aes(y = latitude, x = longitude, fill= log10(n))) +
  geom_sf(data = world, aes(geometry=geometry), fill="#A0A0A0", color="#808080") + 
  xlim(-80,10) + ylim(25, 75) +
  theme_bw() +
  labs(title="Number of months, years with data")

## these regions are missing observations at 50N in the eastern half (E of 38 W)
## also, can't count richness, when spp first observed, etc


path <- "~/Dropbox/post-doc folders/Crispin/MS in progress/diatVSdinof/Data/"
event <- read_delim(paste0(path, "cpr_event.txt"), delim="\t")
occurrence <- read_delim(paste0(path, "cpr_occurrenceData.txt"), delim="\t")
merged_cpr <- merge(event, occurrence, by="eventID")
dataCPR <- merged_cpr[,c("eventDate", "individualCount", "scientificName", "decimalLatitude", "decimalLongitude", "taxonID")]  

spp_list <- readxl::read_excel(paste0(path, "cell_Mass_species.xlsx"), sheet=1)
sppID <- unique(spp_list$taxonID) ## Extracting the spp IDs

# taxa in dataCPR and not in spp_list
temp_table <- dataCPR %>% select(scientificName, taxonID) %>% distinct() %>%
  full_join(spp_list)
temp_table %>% filter(is.na(cell_mass)) %>% nrow()  # 201 taxa, including some broad items: Cumacea, Sergestidae, Isopoda, Decapoda, Copepoda
temp_table %>% filter(is.na(scientificName)) # no cell size data for species not in dataCPR

# How many observations with no size data?
temp_table <- dataCPR %>% left_join(spp_list) 
temp_table %>% count(is.na(cell_mass))  # 2/3 of rows have no cell size data (zoo, other things)
temp_table %>% mutate(year = year(eventDate)) %>% group_by(year) %>% summarize(missing_size = sum(is.na(cell_mass)),
                                                                               n = n(),
                                                                               proportion_missing_size = missing_size/n)
temp_table %>% mutate(NAtlantic = (decimalLatitude > 20) & (decimalLatitude < 80) & (decimalLongitude < 20) & (decimalLongitude > -80)) %>% 
  group_by(NAtlantic) %>% summarize(missing_size = sum(is.na(cell_mass)),
                                                                               n = n(),
                                                                               proportion_missing_size = missing_size/n)

## Extract raw diatom, dino data from North Atlantic

count_table1 <- temp_table %>% filter(!is.na(cell_mass)) %>%
  mutate(region = case_when(decimalLatitude < 55 & decimalLongitude < -35 ~ "SW",
                            decimalLatitude >= 55 & decimalLongitude < -35 ~ "NW",
                            decimalLatitude < 55 & decimalLongitude >= -35 ~ "SE",
                            decimalLatitude >= 55 & decimalLongitude >= -35 ~ "NE",
                            ),
        year = year(eventDate), month = month(eventDate)) %>%
  count(year, month, region)

count_table1 %>% # group_by(year, region) %>%
  # summarize(sum_n = sum(n)) %>%
  ggplot(aes(year, month, fill = n)) +
  facet_wrap(~ fct_relevel(region, "NW", "NE", "SW", "SE")) + 
  scale_y_continuous(breaks = 1:12) + 
  geom_tile() + 
  theme_bw()

library(ggridges)
count_table1 %>% # group_by(year, region) %>%
  # summarize(sum_n = sum(n)) %>%
  ggplot(aes(year, month, group = month, height=n)) +
  facet_wrap(~ fct_relevel(region, "NW", "NE", "SW", "SE")) + 
  geom_density_ridges(stat="identity") + 
  theme_bw()

## subset data using Longhurst + big rectangle

temp_table %>% filter(!is.na(cell_mass)) %>%
  filter(decimalLatitude > 43.5, decimalLatitude < 80, decimalLongitude < 30.25, decimalLongitude > -82.5)

longhurst3 <- longhurst2 %>% filter(ProvCode %in% c("BPLR", "ARCT", "SARC", "NWCS", "NADR", "NECS"))  
# fixes?: sf::st_make_valid(); project sf::st_transform(crs = 3857) and then back to sf::st_transform(crs = 4326) 
longhurst3 <- st_make_valid(longhurst3)

# sf_use_s2(FALSE) # sf_use_s2(TRUE)
# longhurst3  %>% mutate(area = st_area(geometry) )  # fails

# intersect with overall bounding box used above
bounding_box <- st_linestring(list(st_point(c(-82.5, 43.5)), 
                                st_point(c(-82.5, 80)),
                                st_point(c(30.25, 80)), 
                                st_point(c(30.25, 43.5)), 
                                st_point(c(-82.5, 43.5)))) 
bounding_box <- st_linestring(matrix(c(-82.5, 43.5, -82.5, 80,  30.25, 80,  30.25, 43.5,  -82.5, 43.5), ncol=2, byrow=TRUE)) %>% 
  st_sfc(crs="WGS84")

# st_intersection drops items from argument 1 that are not fully enclosed in argument 2
# st_intersection(longhurst3, bounding_box) # %>% mutate(area = st_area(geometry)) # does not do what I want!?

## Attach Longhurst provinces to data

longhurst_t <- st_transform(longhurst3, 4326)
# points1_t <- st_transform(as.matrix(db[1:10, c("longitude", "latitude")]), 2163)  # doesn't work
tidyLatLong <- function(db) {
  points1 <- db[ , c("decimalLongitude", "decimalLatitude")]
  names(points1) <- c("x", "y")
  points1_sf <- do.call("st_sfc",
                        c(lapply(1:nrow(points1), function(i) {st_point(as.numeric(points1[i, ]))}), list("crs" = 4326))) 
  # points1_t <- st_transform(points1_sf, 4326)
  points1_sf
}
points1_t <- tidyLatLong(temp_table) # convert lat, lon columns to sf geometry
temp_table_longhurst <- temp_table %>% bind_cols(geometry = points1_t) %>%
  mutate(longhurst = apply(  # find which province contains each point
    st_intersects(longhurst_t, geometry, sparse=FALSE),
    2,
    function(col) paste0(longhurst_t$ProvCode[which(col)], collapse=",")
  )) 

## Check regions
temp_table_longhurst %>% filter(!is.na(cell_mass)) %>%
  filter(decimalLatitude > 43.5, decimalLatitude < 80, decimalLongitude < 30.25, decimalLongitude > -82.5) %>%
  select(decimalLatitude, decimalLongitude, longhurst) %>% distinct() %>%
  ggplot(aes(decimalLongitude, decimalLatitude, color=longhurst)) +
  geom_jitter(size=0.2)

## Count number of months, years, regions with data

ttl_count <- temp_table_longhurst %>% filter(!is.na(cell_mass)) %>%
  filter(decimalLatitude > 43.5, decimalLatitude < 80, decimalLongitude < 30.25, decimalLongitude > -82.5) %>%
  count(longhurst, month = month(eventDate), year = year(eventDate))

ttl_count %>% group_by(longhurst, round(year, -1)) %>% summarize(sum = sum(n)) %>% View()
ttl_count %>% group_by(longhurst) %>% summarize(sum = sum(n)) %>% View()

temp_table_longhurst %>% select(-geometry) %>%
  filter(!is.na(cell_mass)) %>%
  filter(decimalLatitude > 43.5, decimalLatitude < 80, decimalLongitude < 30.25, decimalLongitude > -82.5) %>%
  group_by(longhurst, year = year(eventDate), month = month(eventDate), 
           round(decimalLatitude), round(decimalLongitude)) %>% summarize(sum = n()) %>% 
  group_by(longhurst) %>% count()

temp_table_longhurst %>% select(-geometry) %>%
  filter(!is.na(cell_mass)) %>%
  filter(decimalLatitude > 43.5, decimalLatitude < 80, decimalLongitude < 30.25, decimalLongitude > -82.5) %>%
  group_by(longhurst, year = year(eventDate), month = month(eventDate), 
           decimalLatitude, decimalLongitude) %>% summarize(sum = n()) %>% 
  group_by(longhurst) %>% count()
# less than two observations within each 1x1 deg box; maybe don't average to a lat/long box? How many zeros? What is typical diversity
# at each point, aggregated point?
# what lat/lon rounding did Crispin use? 2.5°

save.image(file="2021-10-26.Rdata")
getwd()


## Make some summary tables
# First with unaggregated data
# Then 1°, then 2.5°, monthly

# count richness (diatom, dino) at each site; report median and number of 0s

# data are in
# temp_table_longhurst %>% filter(!is.na(cell_mass)) %>% head()
# NA in individualCount means 0 (or not observed). Not sure why they are there...
# Would be nice to have retained eventID and id codes

# data are in:
# raw data: event, occurrence 
# merged into: merged_cpr , with selected columns: dataCPR
# species, functional types, sizes: spp_list 
# merged: temp_table

# occurrence: "occurrenceStatus" is always "present", but "individualCount" is often (17.5%, 370,000) NA
# not sure if these are diatoms/dinos.

ttl2 <- temp_table_longhurst %>% filter(!is.na(Group)) %>% select(-geometry) # 729,775 rows

# no aggregation
ttl2_s <- ttl2 %>% group_by(eventDate, Group, longhurst) %>%
  summarize(n = n(),
            biomass = sum(individualCount * cell_mass),
            abundance = sum(individualCount)
  ) %>% ungroup()
ttl2_s2 <- ttl2_s %>% pivot_wider(names_from = Group, values_from = c(n, biomass, abundance), values_fill = 0)

# 1°, month aggregation
xy_scale = 1.0; xy_offset <- xy_scale/2 # 1° aggregation
xy_scale = 2.5; xy_offset <- xy_scale/2 # 2.5° aggregation

ttl2_s <- ttl2 %>% mutate(year = lubridate::year(eventDate),
                          month = lubridate::month(eventDate),
                          lat_r = round((decimalLatitude - xy_offset)/xy_scale)*xy_scale + xy_offset,
                          lon_r = round((decimalLongitude - xy_offset)/xy_scale)*xy_scale + xy_offset) %>%
  group_by(year, month, lat_r, lon_r, Group, longhurst) %>%
  summarize(n = n(),
            biomass = sum(individualCount * cell_mass),
            abundance = sum(individualCount)
  ) %>% ungroup()
ttl2_s2 <- ttl2_s %>% pivot_wider(names_from = Group, values_from = c(n, biomass, abundance), values_fill = 0)

# Show histogram of richness of diatoms and dinoflagellates

ttl2_s %>% ggplot(aes(n)) + geom_histogram(binwidth = 1) + facet_wrap(~ Group) + 
  labs(x = "Richness (sample)", 
       y = "Number of samples",
       title = "Richness of samples, 2.5° aggregaation") # no spatial or temporal aggregation")

# Show samples by decade and region
ttl2_s2 %>% mutate(# year = lubridate::year(eventDate),
                  decade = floor(year/10)*10) %>%
  group_by(decade, longhurst) %>%
  summarize(n = n(), 
            n_both = sum((n_diatom * n_dinoflagellate) > 0),
            n_dinoflagellate = sum(n_dinoflagellate>0),
            n_diatom = sum(n_diatom>0),
             ) %>%
  ggplot(aes(decade, longhurst, fill= n_both/n)) + 
  geom_tile() + 
  geom_text(aes(label = round(n_both/n*100, 1)), color = "white") + 
  labs(title = "Fraction of samples with both diatoms and dinoflagellates")

# Show richness ratio (diatoms as % of total)
nB = sum(ttl2_s2$n_diatom == 0)
nD = sum(ttl2_s2$n_dinoflagellate == 0)
nBoth = sum(ttl2_s2$n_diatom * ttl2_s2$n_dinoflagellate > 0)
nT = nrow(ttl2_s2)
ttl2_s2 %>% mutate(rr = n_diatom / (n_diatom + n_dinoflagellate)) %>% 
  ggplot(aes(rr)) +
  geom_histogram(binwidth = 1/50) + theme_bw() + 
  labs(title = paste0("# only diatoms ", nB, ", # only dinos ", nD, ", # both ", nBoth, ", # total ", nT))

### Warning
## The figures above do not account for sampling effort properly
## I need to count the number of regions with no diatoms and no dinoflagellates

# no aggregation
ttl2_s <- temp_table_longhurst %>% 
  mutate(Group = case_when(is.na(Group) ~ "Other",
                           TRUE ~ Group)) %>%
           group_by(eventDate, Group, longhurst) %>%
  summarize(n = n(),
            biomass = sum(individualCount * cell_mass),
            abundance = sum(individualCount)
  ) %>% ungroup()
ttl2_s2 <- ttl2_s %>% pivot_wider(names_from = Group, values_from = c(n, biomass, abundance), values_fill = 0)

# count number of observations with diatoms, dinos, both, neither
ttl2_s2 %>% count(n_diatom > 0, n_dinoflagellate>0) %>%
  mutate(p = n/sum(n)*100)  # 1/3 neither, 1/3 both, 1/6 each for diatoms or dinos (approx.)

# 2.5°, month aggregation
# xy_scale = 1.0; xy_offset <- xy_scale/2 # 1° aggregation
xy_scale = 2.5; xy_offset <- xy_scale/2 # 2.5° aggregation

ttl2_s <-temp_table_longhurst %>% 
  mutate(Group = case_when(is.na(Group) ~ "Other",
                           TRUE ~ Group)) %>%
  mutate(year = lubridate::year(eventDate),
                          month = lubridate::month(eventDate),
                          lat_r = round((decimalLatitude - xy_offset)/xy_scale)*xy_scale + xy_offset,
                          lon_r = round((decimalLongitude - xy_offset)/xy_scale)*xy_scale + xy_offset) %>%
  group_by(year, month, lat_r, lon_r, Group, longhurst) %>%
  summarize(n = n(),
            biomass = sum(individualCount * cell_mass),
            abundance = sum(individualCount)
  ) %>% ungroup()
ttl2_s2 <- ttl2_s %>% pivot_wider(names_from = Group, values_from = c(n, biomass, abundance), values_fill = 0)

# count number of observations with diatoms, dinos, both, neither
ttl2_s2 %>% count(n_diatom > 0, n_dinoflagellate>0) %>%
  mutate(p = n/sum(n)*100)  # 18% neither, 55% both, 10% dino only, 17% diatom only


## 10 levels of abundance (15,000 to 750,000); quite a lot (17-40%) of NAs

temp_table_longhurst %>% filter(Group == "diatom") %>%
  count(individualCount) %>% mutate(p = n/sum(n)*100)
temp_table_longhurst %>% filter(Group == "dinoflagellate") %>%
  count(individualCount, Taxon) %>% mutate(p = n/sum(n)*100) %>%  View()

temp_table_longhurst %>% filter(Group == "dinoflagellate") %>%
  filter(is.na(individualCount)) %>% sample_n(100)

## Thoughts so far:
# There are a lot of NA for abundance for diatoms and dinoflagellates (16-40%) 
#  - try to find out why? Particular transects? How did we get presence but not abundance?
# When aggregating over area, include the zeros (no diatoms and no dinos) to properly average abundance
# What to do with diatom-only and dino-only samples? Maybe need a presence/absence (# seen) ratio analysis?
# Zoe looking at literature including Head and Platt papers

# summarize species - first and last occurrence by province

## Compare abundance of a taxon with its size

## CPR PCI
# https://doi.org/10.17031/1720

# NA means presence when found looking for other kind
# so get diatoms and dinos, group by sample
# count number of times each taxon is NA, numerical, both

tts <- temp_table %>% filter(Group == "dinoflagellate") %>%
  group_by(eventDate, scientificName) %>%
  summarize(count = max(individualCount, na.rm=TRUE),
            count_na = sum(is.na(individualCount)),
            count_num = sum(!is.na(individualCount)),
            count_both = count_na == 1 & count_num > 0)

tts %>% ungroup() %>% count(count_na, count_num, count_both)


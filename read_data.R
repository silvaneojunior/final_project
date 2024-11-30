# setwd('final_project')

library(ncdf4) # package for netcdf manipulation
library(tidyverse) # package for plotting
library(sp) # package for geospatial analysis
library(sf) # package for geospatial analysis
require(geobr)
library(ggspatial)

### Spatio info for the Brazilian Legal Amazon (BLA)
amazon.border=read_amazon(year=2012, simplified = FALSE)
# amazon.border=read_state('AM',year=2020)

# amazon.map <- read_municipality(
#   year = 2022,
#   showProgress = FALSE
# ) %>% 
#   mutate(loc=as.factor(paste0(str_to_upper(name_muni), ' - ',str_to_upper(name_state)))) %>% 
#   select(loc,geom) %>% 
#   filter(st_intersects(geom, amazon.border,sparse = FALSE)[,1])

amazon.map <- read_micro_region(
  year = 2020,
  showProgress = FALSE,
  simplified = FALSE
) %>% 
  mutate(loc=as.factor(paste0(str_to_upper(name_micro), ' - ',str_to_upper(name_state)))) %>% 
  select(loc,geom) %>% 
  filter(st_intersects(geom, amazon.border,sparse = FALSE)[,1]) %>% 
  mutate(area=as.numeric(st_area(geom))) %>% 
  mutate(geom=st_intersection(geom,amazon.border)) %>%
  filter(0.4*area<as.numeric(st_area(geom)))

amazon.muni <- read_municipality(
  year = 2022,
  showProgress = FALSE,
  simplified = TRUE
) %>% 
  mutate(loc=as.factor(paste0(str_to_upper(name_muni), ' - ',str_to_upper(name_state)))) %>% 
  select(loc,geom) %>% 
  filter(st_intersects(geom, amazon.border,sparse = FALSE)[,1]) %>% 
  mutate(area=as.numeric(st_area(geom))) %>% 
  mutate(geom=st_intersection(geom,amazon.border)) %>%
  filter(0.4*area<as.numeric(st_area(geom)))

valid.muni=unique(amazon.muni$loc)

loc.list=unique(amazon.map$loc)
length(loc.list)

### Fire info
file.list=list.files('data') %>% {.[grepl('csv',.)]}

### Municipio = Municipality
### Estado = State
# raw.data=lapply(paste0('data/',file.list),read.csv) %>% 
#   do.call(rbind,.) %>% 
#   filter(Pais=="Brasil") # Filtering other countries
# 
# ### "FORTALEZA DO TABOCÃO" changed its name to "TABOCÃO" during the period of the analysis.
# raw.data$Municipio[raw.data$Municipio=="FORTALEZA DO TABOCÃO"]="TABOCÃO"
# 
# data.point=raw.data %>% 
#   mutate(loc=paste0(str_to_upper(Municipio), ' - ',str_to_upper(Estado))) %>%
#   filter(loc %in% valid.muni) %>% # Filtering only locations in the BLA
#   mutate(date=as.Date(substr(DataHora,1,10))%>% format('%Y-%m-01') %>% as.Date()) %>%
#   rename(lat=Latitude,long=Longitude) %>% 
#   select(date,loc,lat,long)
# write.csv(data.point,file='data_point.csv')
data.point=read.csv('data/data_point.csv')

st.point=st_as_sf(data.point,coords=c('long','lat'),crs=st_crs(amazon.map)) %>% st_transform()

mat.loc=st_within(st.point,amazon.map,sparse=FALSE)

data.point$loc=apply(mat.loc,1,which,simplify=TRUE) %>% 
  as.numeric()

data.point=data.point %>% 
  filter(!is.na(loc)) %>% 
  mutate(loc=factor(loc,levels=1:length(loc.list)))

levels(data.point$loc)=unique(amazon.map$loc)
amazon.map$n=(mat.loc %>% colSums)

# Those two should be the same value
unique(data.point$loc) %>% length()
length(loc.list)

data.muni=data.point %>%
  arrange(loc,date) %>% 
  # filter(format(date,'%Y')>2007) %>% 
  group_by(loc,date) %>% 
  count() %>% 
  ungroup() %>% 
  complete(loc,date,fill=list(n=0))


data.car=data.muni %>%
  arrange(loc,date) %>% 
  pivot_wider(names_from=loc,values_from=n) %>% 
  arrange(date)

ggplot(data.muni)+
  geom_point(aes(x=date,y=n,color=loc))+
  guides(color='none')+
  theme_bw()

map1=amazon.map %>% 
  arrange(loc)

area=st_area(map1)

space=2
map2=data.point %>% filter(format(date,'%Y')<=2005)

color=rainbow(length(unique(map1$loc)))
names(color)=levels(st.point$loc)

(ggplot()+
  geom_sf(data=map1,aes(fill=loc,color=loc),
  color='#00000055',alpha=0.2)+
  geom_sf(data=amazon.border,
          color='#5555ff',fill='#00000000',linetype='dashed',linewidth=1)+
  geom_sf(data=st.point,aes(fill=loc,color=loc),alpha=0.2,linetype='dashed')+
  # stat_density2d(aes(x=long,y=lat,fill = ..level..),
  #                data=data.point,alpha=0.6, geom = "polygon")+
  # scale_fill_gradient(low = "white", high = "#700000") +
  scale_color_manual('',values=color) +
  scale_fill_manual('',values=color) +
  guides(fill='none',color='none')+
  coord_sf()+
  theme_void()+
  theme(legend.position = 'bottom'))
 

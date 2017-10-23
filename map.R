
install.packages("dplyr")
install.packages("maptools")
install.packages("ggplot2")
install.packages("maps")
install.packages("mapdata")
library(dplyr)       
library(maptools)    
library(ggplot2)     
library(sqldf)
#NZL_adm3.shp
map_data=readShapePoly(file.choose())
df_map=map_data@data[, 'NAME_1']

latitude_longitude=fortify(map_data)

df_map=data.frame(id=unique(latitude_longitude$id),NAME_1=as.character(map_data$NAME_1))

latitude_longitude=latitude_longitude[, c('long', 'lat', 'id')]
latitude_longitude=left_join(latitude_longitude,df_map, by ='id')
latitude_longitude=tbl_df(latitude_longitude)

group_id=group_by(.data = latitude_longitude, id)
group_name=group_by(.data = latitude_longitude, NAME_1)

latitude_longitude=filter(latitude_longitude,(long>165 & long<180) & (lat>-47 ) )

center=function(x) mean(range(x))
province_location=summarise(.data = group_name, latitude = center(lat), longitude = center(long))

doc_data=read.csv(file = file.choose())
doc_data_select=rename(.data = doc_data,NAME_1=NAME)

Province_Info=province_location %>% left_join(.,df_map, by = 'NAME_1') %>% left_join(.,doc_data_select, by = 'NAME_1')
Province_Info=filter(Province_Info,(NAME_1!="Northland" & NAME_1!="Chatham Islands" ) )  

latitude_longitude=latitude_longitude %>% left_join(., Province_Info[,c('NAME_1','Median')], by = 'NAME_1')
##########################################################################################################################

ggplot(data = latitude_longitude, mapping = aes(x = long, y = lat, group = id)) +
  geom_polygon(colour = 'black', fill = 'white') +
  scale_fill_manual(values=colours(),guide=FALSE) + 
  geom_text(mapping = aes(x =longitude, y = latitude, label = NAME_1), data = Province_Info, colour = 'steelblue') +
  
  theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank())

ggplot(data = latitude_longitude, mapping = aes(x = long, y = lat, group = id, fill =MEDIAN_INCOME)) + 
  geom_polygon(colour = 'black') +
  scale_fill_gradient(low = 'green', high = 'blue') + 
  labs(title ="Numbers of Medianpers in NEW Land") +
  geom_text(mapping = aes(x =longitude, y = latitude, label = NAME_1), data = Province_Info, colour = 'steelblue') +
  theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks =element_blank(), panel.grid = element_blank())

ggplot(latitude_longitude)+
  theme_minimal()+
  geom_polygon( aes(x = long, y = lat, group = id, fill= Median),color = "grey", alpha = 0.5) +
  coord_map()+
  scale_fill_distiller(name = "Median Personal Income in NEW Zealand", palette = "Spectral")+
  theme(legend.position = "right")

ggplot(data = latitude_longitude, mapping = aes(x = long, y = lat, group = id, fill =Median)) + 
  geom_polygon(colour = 'white') +
  scale_fill_gradient(low = 'green', high = 'blue') + 
  labs(title ="Numbers of Medianpers and population  in NEW Land") +
  geom_text(mapping = aes(x =longitude, y = latitude, label = NAME_1), data = Province_Info, colour = 'steelblue') +
  geom_point(mapping = aes(x = longitude, y = latitude, size = People), data = Province_Info, colour = 'red') + 
  theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks =element_blank(), panel.grid = element_blank())

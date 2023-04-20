
#https://earthenginepartners.appspot.com/science-2013-global-forest/download_v1.7.html
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, sf, tidyverse, opemxlsx)

library(RPostgres)
library(sf)
library(ggplot2)
library(ggspatial)
library(raster)
library(ggnewscale)
dvr         <- RPostgres::Postgres()
db          <- 'postgres'  ##Nombre de la BBDD
host_db     <- 'localhost'
db_port     <- '5432' 
db_user     <- 'postgres'  ##Tu usuario
db_password <- 'gflorezc' ##Tu contraseña 

# 3.0 Conexión
con <- dbConnect(dvr, dbname = db, host=host_db, port=db_port, user=db_user, password=db_password)  
dbListTables(con)

Poligono<- st_read(con, layer = "Chonta")
Poligono<- st_transform(Chonta,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))

Defores = stack("Raster/Deforestacion.tif")
plot(Defores)

Raster_Cortado    <- crop(Defores , Poligono)                           #
Raster_Cortado     <- Raster_Cortado   <- mask(Raster_Cortado  , Poligono)
plot(Raster_Cortado )

Defores_tbl  <-  rasterToPoints(Raster_Cortado)
Defores_df   <-  data.frame(Defores_tbl)
colnames(Defores_df) = c("x", "y", "Año")


library(dplyr)

Defores_df= Defores_df%>%
  subset(Año<= 19 & Año> 0)  %>%
  mutate(Años = 2000 +Año)


Defores_df_2001 = Defores_df%>% subset(Año<= 1 & Año> 0)  %>% mutate(Años = 2000 +Año)
Defores_df_2002 = Defores_df%>% subset(Año<= 2 & Año> 1)  %>% mutate(Años = 2000 +Año)
Defores_df_2003 = Defores_df%>% subset(Año<= 3 & Año> 2)  %>% mutate(Años = 2000 +Año)
Defores_df_2004 = Defores_df%>% subset(Año<= 4 & Año> 3)  %>% mutate(Años = 2000 +Año)
Defores_df_2005 = Defores_df%>% subset(Año<= 5 & Año> 4)  %>% mutate(Años = 2000 +Año)
Defores_df_2006 = Defores_df%>% subset(Año<= 6 & Año> 5)  %>% mutate(Años = 2000 +Año)
Defores_df_2007 = Defores_df%>% subset(Año<= 7 & Año> 6)  %>% mutate(Años = 2000 +Año)
Defores_df_2008 = Defores_df%>% subset(Año<= 8 & Año> 7)  %>% mutate(Años = 2000 +Año)
Defores_df_2009 = Defores_df%>% subset(Año<= 9 & Año> 8)  %>% mutate(Años = 2000 +Año)
Defores_df_2010 = Defores_df%>% subset(Año<= 10 & Año> 9)  %>% mutate(Años = 2000 +Año)
Defores_df_2011 = Defores_df%>% subset(Año<= 11& Año> 10)  %>% mutate(Años = 2000 +Año)
Defores_df_2012 = Defores_df%>% subset(Año<= 12 & Año> 11)  %>% mutate(Años = 2000 +Año)
Defores_df_2013 = Defores_df%>% subset(Año<= 13 & Año> 12)  %>% mutate(Años = 2000 +Año)
Defores_df_2014 = Defores_df%>% subset(Año<= 14 & Año> 13)  %>% mutate(Años = 2000 +Año)
Defores_df_2015 = Defores_df%>% subset(Año<= 15 & Año> 14)  %>% mutate(Años = 2000 +Año)
Defores_df_2016 = Defores_df%>% subset(Año<= 16 & Año> 15)  %>% mutate(Años = 2000 +Año)
Defores_df_2017 = Defores_df%>% subset(Año<= 17 & Año> 16)  %>% mutate(Años = 2000 +Año)
Defores_df_2018 = Defores_df%>% subset(Año<= 18 & Año> 17)  %>% mutate(Años = 2000 +Año)
Defores_df_2019 = Defores_df%>% subset(Año<= 19 & Año> 18)  %>% mutate(Años = 2000 +Año)



library(grid)
library(png)
library(ggimage)
Logo <- readPNG("Logo R.png", FALSE)
Logo_png <- rasterGrob(Logo, x = unit(0.6, "npc"),y = unit(0.15, "npc"), width = unit(0.15, "npc"))



A= ggplot()+
  geom_sf(data = Poligono, fill="black", color="black", size=1)+
  geom_tile(data= Defores_df_2001, aes(x = x, y =  y,  fill=Año), show.legend = F) +
  scale_fill_gradientn(colours = "#008000")+
  ggspatial::annotation_north_arrow(
    location = "br", which_north = "true",
    pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey10", "white"),
      line_col = "grey10", text_family = "ArcherPro Book" , text_col="white"))+
  ggspatial::annotation_scale(location = "bl",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book", text_col="white")+
  labs( x="", y="" )+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "#404040"),
        legend.position = c(0.2,0.2),
        legend.text=element_text(size=8, family="serif"),
        legend.title = element_text(size=8, family="serif", face='bold',hjust=0.5)
  )+
  ggplot2::annotate(geom = "text", x = -69.25, y = -12.59, hjust = 0, vjust = 1, 
                    label = "2001",
                    size = 9, family="serif", color = "white")+
  annotation_custom(Logo_png)+
  ggplot2::annotate(geom = "text", x = -69.6, y =-12.6, hjust = 0, vjust = 1, 
                    label = "Deforestacion de la Microcuenca-Chonta",size = 6, family="serif", color = 
                      "white",  face = "bold")+
                      ggplot2::annotate(geom = "text", x = -69.6, y =-12.62, hjust = 0, vjust = 1, 
                                        label = "Data: High-Resolution Global Maps of 21st-Century Forest Cover Change.",size = 3, family="serif", color = 
                                          "white",  face = "bold")+
                                          ggplot2::annotate(geom = "text", x = -69.6, y =-12.63, hjust = 0, vjust = 1, 
                                                            label = "@Ing. gflorezc",size = 3, family="serif", color = 
                                                              "white",  face = "bold")




B= ggplot()+
  geom_sf(data = Poligono, fill="black", color="black", size=1)+
  geom_tile(data= Defores_df_2002, aes(x = x, y =  y,  fill=Año), show.legend = F) +
  scale_fill_gradientn(colours = "#008000")+
  ggspatial::annotation_north_arrow(
    location = "br", which_north = "true",
    pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey10", "white"),
      line_col = "grey10", text_family = "ArcherPro Book" , text_col="white"))+
  ggspatial::annotation_scale(location = "bl",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book", text_col="white")+
  labs( x="", y="" )+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "#404040"),
        legend.position = c(0.2,0.2),
        legend.text=element_text(size=8, family="serif"),
        legend.title = element_text(size=8, family="serif", face='bold',hjust=0.5)
  )+
  ggplot2::annotate(geom = "text", x = -69.25, y = -12.59, hjust = 0, vjust = 1, 
                    label = "2002",
                    size = 9, family="serif", color = "white")+
  annotation_custom(Logo_png)+
  ggplot2::annotate(geom = "text", x = -69.6, y =-12.6, hjust = 0, vjust = 1, 
                    label = "Deforestacion de la Microcuenca-Chonta",size = 6, family="serif", color = 
                      "white",  face = "bold")+
                      ggplot2::annotate(geom = "text", x = -69.6, y =-12.62, hjust = 0, vjust = 1, 
                                        label = "Data: High-Resolution Global Maps of 21st-Century Forest Cover Change.",size = 3, family="serif", color = 
                                          "white",  face = "bold")+
                                          ggplot2::annotate(geom = "text", x = -69.6, y =-12.63, hjust = 0, vjust = 1, 
                                                            label = "@Ing. gflorezc",size = 3, family="serif", color = 
                                                              "white",  face = "bold")
B                                                            

C= ggplot()+
  geom_sf(data = Poligono, fill="black", color="black", size=1)+
  geom_tile(data= Defores_df_2003, aes(x = x, y =  y,  fill=Año), show.legend = F) +
  scale_fill_gradientn(colours = "#008000")+
  ggspatial::annotation_north_arrow(
    location = "br", which_north = "true",
    pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey10", "white"),
      line_col = "grey10", text_family = "ArcherPro Book" , text_col="white"))+
  ggspatial::annotation_scale(location = "bl",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book", text_col="white")+
  labs( x="", y="" )+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "#404040"),
        legend.position = c(0.2,0.2),
        legend.text=element_text(size=8, family="serif"),
        legend.title = element_text(size=8, family="serif", face='bold',hjust=0.5)
  )+
  ggplot2::annotate(geom = "text", x = -69.25, y = -12.59, hjust = 0, vjust = 1, 
                    label = "2003",
                    size = 9, family="serif", color = "white")+
  annotation_custom(Logo_png)+
  ggplot2::annotate(geom = "text", x = -69.6, y =-12.6, hjust = 0, vjust = 1, 
                    label = "Deforestacion de la Microcuenca-Chonta",size = 6, family="serif", color = 
                      "white",  face = "bold")+
                      ggplot2::annotate(geom = "text", x = -69.6, y =-12.62, hjust = 0, vjust = 1, 
                                        label = "Data: High-Resolution Global Maps of 21st-Century Forest Cover Change.",size = 3, family="serif", color = 
                                          "white",  face = "bold")+
                                          ggplot2::annotate(geom = "text", x = -69.6, y =-12.63, hjust = 0, vjust = 1, 
                                                            label = "@Ing. gflorezc",size = 3, family="serif", color = 
                                                              "white",  face = "bold")
C                                                            


D= ggplot()+
  geom_sf(data = Poligono, fill="black", color="black", size=1)+
  geom_tile(data= Defores_df_2004, aes(x = x, y =  y,  fill=Año), show.legend = F) +
  scale_fill_gradientn(colours = "#008000")+
  ggspatial::annotation_north_arrow(
    location = "br", which_north = "true",
    pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey10", "white"),
      line_col = "grey10", text_family = "ArcherPro Book" , text_col="white"))+
  ggspatial::annotation_scale(location = "bl",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book", text_col="white")+
  labs( x="", y="" )+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "#404040"),
        legend.position = c(0.2,0.2),
        legend.text=element_text(size=8, family="serif"),
        legend.title = element_text(size=8, family="serif", face='bold',hjust=0.5)
  )+
  ggplot2::annotate(geom = "text", x = -69.25, y = -12.59, hjust = 0, vjust = 1, 
                    label = "2004",
                    size = 9, family="serif", color = "white")+
  annotation_custom(Logo_png)+
  ggplot2::annotate(geom = "text", x = -69.6, y =-12.6, hjust = 0, vjust = 1, 
                    label = "Deforestacion de la Microcuenca-Chonta",size = 6, family="serif", color = 
                      "white",  face = "bold")+
                      ggplot2::annotate(geom = "text", x = -69.6, y =-12.62, hjust = 0, vjust = 1, 
                                        label = "Data: High-Resolution Global Maps of 21st-Century Forest Cover Change.",size = 3, family="serif", color = 
                                          "white",  face = "bold")+
                                          ggplot2::annotate(geom = "text", x = -69.6, y =-12.63, hjust = 0, vjust = 1, 
                                                            label = "@Ing. gflorezc",size = 3, family="serif", color = 
                                                              "white",  face = "bold")
                                                            
D

E= ggplot()+
  geom_sf(data = Poligono, fill="black", color="black", size=1)+
  geom_tile(data= Defores_df_2005, aes(x = x, y =  y,  fill=Año), show.legend = F) +
  scale_fill_gradientn(colours = "#008000")+
  ggspatial::annotation_north_arrow(
    location = "br", which_north = "true",
    pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey10", "white"),
      line_col = "grey10", text_family = "ArcherPro Book" , text_col="white"))+
  ggspatial::annotation_scale(location = "bl",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book", text_col="white")+
  labs( x="", y="" )+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "#404040"),
        legend.position = c(0.2,0.2),
        legend.text=element_text(size=8, family="serif"),
        legend.title = element_text(size=8, family="serif", face='bold',hjust=0.5)
  )+
  ggplot2::annotate(geom = "text", x = -69.25, y = -12.59, hjust = 0, vjust = 1, 
                    label = "2005",
                    size = 9, family="serif", color = "white")+
  annotation_custom(Logo_png)+
  ggplot2::annotate(geom = "text", x = -69.6, y =-12.6, hjust = 0, vjust = 1, 
                    label = "Deforestacion de la Microcuenca-Chonta",size = 6, family="serif", color = 
                      "white",  face = "bold")+
                      ggplot2::annotate(geom = "text", x = -69.6, y =-12.62, hjust = 0, vjust = 1, 
                                        label = "Data: High-Resolution Global Maps of 21st-Century Forest Cover Change.",size = 3, family="serif", color = 
                                          "white",  face = "bold")+
                                          ggplot2::annotate(geom = "text", x = -69.6, y =-12.63, hjust = 0, vjust = 1, 
                                                            label = "@Ing. gflorezc",size = 3, family="serif", color = 
                                                              "white",  face = "bold")
                                                            
E

FF= ggplot()+
  geom_sf(data = Poligono, fill="black", color="black", size=1)+
  geom_tile(data= Defores_df_2006, aes(x = x, y =  y,  fill=Año), show.legend = F) +
  scale_fill_gradientn(colours = "#008000")+
  ggspatial::annotation_north_arrow(
    location = "br", which_north = "true",
    pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey10", "white"),
      line_col = "grey10", text_family = "ArcherPro Book" , text_col="white"))+
  ggspatial::annotation_scale(location = "bl",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book", text_col="white")+
  labs( x="", y="" )+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "#404040"),
        legend.position = c(0.2,0.2),
        legend.text=element_text(size=8, family="serif"),
        legend.title = element_text(size=8, family="serif", face='bold',hjust=0.5)
  )+
  ggplot2::annotate(geom = "text", x = -69.25, y = -12.59, hjust = 0, vjust = 1, 
                    label = "2006",
                    size = 9, family="serif", color = "white")+
  annotation_custom(Logo_png)+
  ggplot2::annotate(geom = "text", x = -69.6, y =-12.6, hjust = 0, vjust = 1, 
                    label = "Deforestacion de la Microcuenca-Chonta",size = 6, family="serif", color = 
                      "white",  face = "bold")+
                      ggplot2::annotate(geom = "text", x = -69.6, y =-12.62, hjust = 0, vjust = 1, 
                                        label = "Data: High-Resolution Global Maps of 21st-Century Forest Cover Change.",size = 3, family="serif", color = 
                                          "white",  face = "bold")+
                                          ggplot2::annotate(geom = "text", x = -69.6, y =-12.63, hjust = 0, vjust = 1, 
                                                            label = "@Ing. gflorezc",size = 3, family="serif", color = 
                                                              "white",  face = "bold")
                                                            

FF

G= ggplot()+
  geom_sf(data = Poligono, fill="black", color="black", size=1)+
  geom_tile(data= Defores_df_2007, aes(x = x, y =  y,  fill=Año), show.legend = F) +
  scale_fill_gradientn(colours = "#008000")+
  ggspatial::annotation_north_arrow(
    location = "br", which_north = "true",
    pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey10", "white"),
      line_col = "grey10", text_family = "ArcherPro Book" , text_col="white"))+
  ggspatial::annotation_scale(location = "bl",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book", text_col="white")+
  labs( x="", y="" )+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "#404040"),
        legend.position = c(0.2,0.2),
        legend.text=element_text(size=8, family="serif"),
        legend.title = element_text(size=8, family="serif", face='bold',hjust=0.5)
  )+
  ggplot2::annotate(geom = "text", x = -69.25, y = -12.59, hjust = 0, vjust = 1, 
                    label = "2007",
                    size = 9, family="serif", color = "white")+
  annotation_custom(Logo_png)+
  ggplot2::annotate(geom = "text", x = -69.6, y =-12.6, hjust = 0, vjust = 1, 
                    label = "Deforestacion de la Microcuenca-Chonta",size = 6, family="serif", color = 
                      "white",  face = "bold")+
                      ggplot2::annotate(geom = "text", x = -69.6, y =-12.62, hjust = 0, vjust = 1, 
                                        label = "Data: High-Resolution Global Maps of 21st-Century Forest Cover Change.",size = 3, family="serif", color = 
                                          "white",  face = "bold")+
                                          ggplot2::annotate(geom = "text", x = -69.6, y =-12.63, hjust = 0, vjust = 1, 
                                                            label = "@Ing. gflorezc",size = 3, family="serif", color = 
                                                              "white",  face = "bold")
                                                            
G

H= ggplot()+
  geom_sf(data = Poligono, fill="black", color="black", size=1)+
  geom_tile(data= Defores_df_2008, aes(x = x, y =  y,  fill=Año), show.legend = F) +
  scale_fill_gradientn(colours = "#008000")+
  ggspatial::annotation_north_arrow(
    location = "br", which_north = "true",
    pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey10", "white"),
      line_col = "grey10", text_family = "ArcherPro Book" , text_col="white"))+
  ggspatial::annotation_scale(location = "bl",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book", text_col="white")+
  labs( x="", y="" )+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "#404040"),
        legend.position = c(0.2,0.2),
        legend.text=element_text(size=8, family="serif"),
        legend.title = element_text(size=8, family="serif", face='bold',hjust=0.5)
  )+
  ggplot2::annotate(geom = "text", x = -69.25, y = -12.59, hjust = 0, vjust = 1, 
                    label = "2008",
                    size = 9, family="serif", color = "white")+
  annotation_custom(Logo_png)+
  ggplot2::annotate(geom = "text", x = -69.6, y =-12.6, hjust = 0, vjust = 1, 
                    label = "Deforestacion de la Microcuenca-Chonta",size = 6, family="serif", color = 
                      "white",  face = "bold")+
                      ggplot2::annotate(geom = "text", x = -69.6, y =-12.62, hjust = 0, vjust = 1, 
                                        label = "Data: High-Resolution Global Maps of 21st-Century Forest Cover Change.",size = 3, family="serif", color = 
                                          "white",  face = "bold")+
                                          ggplot2::annotate(geom = "text", x = -69.6, y =-12.63, hjust = 0, vjust = 1, 
                                                            label = "@Ing. gflorezc",size = 3, family="serif", color = 
                                                              "white",  face = "bold")
                                                            
H

I= ggplot()+
  geom_sf(data = Poligono, fill="black", color="black", size=1)+
  geom_tile(data= Defores_df_2009, aes(x = x, y =  y,  fill=Año), show.legend = F) +
  scale_fill_gradientn(colours = "#008000")+
  ggspatial::annotation_north_arrow(
    location = "br", which_north = "true",
    pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey10", "white"),
      line_col = "grey10", text_family = "ArcherPro Book" , text_col="white"))+
  ggspatial::annotation_scale(location = "bl",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book", text_col="white")+
  labs( x="", y="" )+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "#404040"),
        legend.position = c(0.2,0.2),
        legend.text=element_text(size=8, family="serif"),
        legend.title = element_text(size=8, family="serif", face='bold',hjust=0.5)
  )+
  ggplot2::annotate(geom = "text", x = -69.25, y = -12.59, hjust = 0, vjust = 1, 
                    label = "2009",
                    size = 9, family="serif", color = "white")+
  annotation_custom(Logo_png)+
  ggplot2::annotate(geom = "text", x = -69.6, y =-12.6, hjust = 0, vjust = 1, 
                    label = "Deforestacion de la Microcuenca-Chonta",size = 6, family="serif", color = 
                      "white",  face = "bold")+
                      ggplot2::annotate(geom = "text", x = -69.6, y =-12.62, hjust = 0, vjust = 1, 
                                        label = "Data: High-Resolution Global Maps of 21st-Century Forest Cover Change.",size = 3, family="serif", color = 
                                          "white",  face = "bold")+
                                          ggplot2::annotate(geom = "text", x = -69.6, y =-12.63, hjust = 0, vjust = 1, 
                                                            label = "@Ing. gflorezc",size = 3, family="serif", color = 
                                                              "white",  face = "bold")
                                                            
I

J= ggplot()+
  geom_sf(data = Poligono, fill="black", color="black", size=1)+
  geom_tile(data= Defores_df_2010, aes(x = x, y =  y,  fill=Año), show.legend = F) +
  scale_fill_gradientn(colours = "#008000")+
  ggspatial::annotation_north_arrow(
    location = "br", which_north = "true",
    pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey10", "white"),
      line_col = "grey10", text_family = "ArcherPro Book" , text_col="white"))+
  ggspatial::annotation_scale(location = "bl",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book", text_col="white")+
  labs( x="", y="" )+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "#404040"),
        legend.position = c(0.2,0.2),
        legend.text=element_text(size=8, family="serif"),
        legend.title = element_text(size=8, family="serif", face='bold',hjust=0.5)
  )+
  ggplot2::annotate(geom = "text", x = -69.25, y = -12.59, hjust = 0, vjust = 1, 
                    label = "2010",
                    size = 9, family="serif", color = "white")+
  annotation_custom(Logo_png)+
  ggplot2::annotate(geom = "text", x = -69.6, y =-12.6, hjust = 0, vjust = 1, 
                    label = "Deforestacion de la Microcuenca-Chonta",size = 6, family="serif", color = 
                      "white",  face = "bold")+
                      ggplot2::annotate(geom = "text", x = -69.6, y =-12.62, hjust = 0, vjust = 1, 
                                        label = "Data: High-Resolution Global Maps of 21st-Century Forest Cover Change.",size = 3, family="serif", color = 
                                          "white",  face = "bold")+
                                          ggplot2::annotate(geom = "text", x = -69.6, y =-12.63, hjust = 0, vjust = 1, 
                                                            label = "@Ing. gflorezc",size = 3, family="serif", color = 
                                                              "white",  face = "bold")
                                                            
J

K= ggplot()+
  geom_sf(data = Poligono, fill="black", color="black", size=1)+
  geom_tile(data= Defores_df_2011, aes(x = x, y =  y,  fill=Año), show.legend = F) +
  scale_fill_gradientn(colours = "#008000")+
  ggspatial::annotation_north_arrow(
    location = "br", which_north = "true",
    pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey10", "white"),
      line_col = "grey10", text_family = "ArcherPro Book" , text_col="white"))+
  ggspatial::annotation_scale(location = "bl",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book", text_col="white")+
  labs( x="", y="" )+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "#404040"),
        legend.position = c(0.2,0.2),
        legend.text=element_text(size=8, family="serif"),
        legend.title = element_text(size=8, family="serif", face='bold',hjust=0.5)
  )+
  ggplot2::annotate(geom = "text", x = -69.25, y = -12.59, hjust = 0, vjust = 1, 
                    label = "2011",
                    size = 9, family="serif", color = "white")+
  annotation_custom(Logo_png)+
  ggplot2::annotate(geom = "text", x = -69.6, y =-12.6, hjust = 0, vjust = 1, 
                    label = "Deforestacion de la Microcuenca-Chonta",size = 6, family="serif", color = 
                      "white",  face = "bold")+
                      ggplot2::annotate(geom = "text", x = -69.6, y =-12.62, hjust = 0, vjust = 1, 
                                        label = "Data: High-Resolution Global Maps of 21st-Century Forest Cover Change.",size = 3, family="serif", color = 
                                          "white",  face = "bold")+
                                          ggplot2::annotate(geom = "text", x = -69.6, y =-12.63, hjust = 0, vjust = 1, 
                                                            label = "@Ing. gflorezc",size = 3, family="serif", color = 
                                                              "white",  face = "bold")
                                                            
K

L= ggplot()+
  geom_sf(data = Poligono, fill="black", color="black", size=1)+
  geom_tile(data= Defores_df_2012, aes(x = x, y =  y,  fill=Año), show.legend = F) +
  scale_fill_gradientn(colours = "#008000")+
  ggspatial::annotation_north_arrow(
    location = "br", which_north = "true",
    pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey10", "white"),
      line_col = "grey10", text_family = "ArcherPro Book" , text_col="white"))+
  ggspatial::annotation_scale(location = "bl",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book", text_col="white")+
  labs( x="", y="" )+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "#404040"),
        legend.position = c(0.2,0.2),
        legend.text=element_text(size=8, family="serif"),
        legend.title = element_text(size=8, family="serif", face='bold',hjust=0.5)
  )+
  ggplot2::annotate(geom = "text", x = -69.25, y = -12.59, hjust = 0, vjust = 1, 
                    label = "2012",
                    size = 9, family="serif", color = "white")+
  annotation_custom(Logo_png)+
  ggplot2::annotate(geom = "text", x = -69.6, y =-12.6, hjust = 0, vjust = 1, 
                    label = "Deforestacion de la Microcuenca-Chonta",size = 6, family="serif", color = 
                      "white",  face = "bold")+
                      ggplot2::annotate(geom = "text", x = -69.6, y =-12.62, hjust = 0, vjust = 1, 
                                        label = "Data: High-Resolution Global Maps of 21st-Century Forest Cover Change.",size = 3, family="serif", color = 
                                          "white",  face = "bold")+
                                          ggplot2::annotate(geom = "text", x = -69.6, y =-12.63, hjust = 0, vjust = 1, 
                                                            label = "@Ing. gflorezc",size = 3, family="serif", color = 
                                                              "white",  face = "bold")
                                                            

L

LL= ggplot()+
  geom_sf(data = Poligono, fill="black", color="black", size=1)+
  geom_tile(data= Defores_df_2013, aes(x = x, y =  y,  fill=Año), show.legend = F) +
  scale_fill_gradientn(colours = "#008000")+
  ggspatial::annotation_north_arrow(
    location = "br", which_north = "true",
    pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey10", "white"),
      line_col = "grey10", text_family = "ArcherPro Book" , text_col="white"))+
  ggspatial::annotation_scale(location = "bl",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book", text_col="white")+
  labs( x="", y="" )+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "#404040"),
        legend.position = c(0.2,0.2),
        legend.text=element_text(size=8, family="serif"),
        legend.title = element_text(size=8, family="serif", face='bold',hjust=0.5)
  )+
  ggplot2::annotate(geom = "text", x = -69.25, y = -12.59, hjust = 0, vjust = 1, 
                    label = "2013",
                    size = 9, family="serif", color = "white")+
  annotation_custom(Logo_png)+
  ggplot2::annotate(geom = "text", x = -69.6, y =-12.6, hjust = 0, vjust = 1, 
                    label = "Deforestacion de la Microcuenca-Chonta",size = 6, family="serif", color = 
                      "white",  face = "bold")+
                      ggplot2::annotate(geom = "text", x = -69.6, y =-12.62, hjust = 0, vjust = 1, 
                                        label = "Data: High-Resolution Global Maps of 21st-Century Forest Cover Change.",size = 3, family="serif", color = 
                                          "white",  face = "bold")+
                                          ggplot2::annotate(geom = "text", x = -69.6, y =-12.63, hjust = 0, vjust = 1, 
                                                            label = "@Ing. gflorezc",size = 3, family="serif", color = 
                                                              "white",  face = "bold")
                                                            
LL

M= ggplot()+
  geom_sf(data = Poligono, fill="black", color="black", size=1)+
  geom_tile(data= Defores_df_2014, aes(x = x, y =  y,  fill=Año), show.legend = F) +
  scale_fill_gradientn(colours = "#008000")+
  ggspatial::annotation_north_arrow(
    location = "br", which_north = "true",
    pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey10", "white"),
      line_col = "grey10", text_family = "ArcherPro Book" , text_col="white"))+
  ggspatial::annotation_scale(location = "bl",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book", text_col="white")+
  labs( x="", y="" )+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "#404040"),
        legend.position = c(0.2,0.2),
        legend.text=element_text(size=8, family="serif"),
        legend.title = element_text(size=8, family="serif", face='bold',hjust=0.5)
  )+
  ggplot2::annotate(geom = "text", x = -69.25, y = -12.59, hjust = 0, vjust = 1, 
                    label = "2014",
                    size = 9, family="serif", color = "white")+
  annotation_custom(Logo_png)+
  ggplot2::annotate(geom = "text", x = -69.6, y =-12.6, hjust = 0, vjust = 1, 
                    label = "Deforestacion de la Microcuenca-Chonta",size = 6, family="serif", color = 
                      "white",  face = "bold")+
                      ggplot2::annotate(geom = "text", x = -69.6, y =-12.62, hjust = 0, vjust = 1, 
                                        label = "Data: High-Resolution Global Maps of 21st-Century Forest Cover Change.",size = 3, family="serif", color = 
                                          "white",  face = "bold")+
                                          ggplot2::annotate(geom = "text", x = -69.6, y =-12.63, hjust = 0, vjust = 1, 
                                                            label = "@Ing. gflorezc",size = 3, family="serif", color = 
                                                              "white",  face = "bold")
                                                            
M

N= ggplot()+
  geom_sf(data = Poligono, fill="black", color="black", size=1)+
  geom_tile(data= Defores_df_2015, aes(x = x, y =  y,  fill=Año), show.legend = F) +
  scale_fill_gradientn(colours = "#008000")+
  ggspatial::annotation_north_arrow(
    location = "br", which_north = "true",
    pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey10", "white"),
      line_col = "grey10", text_family = "ArcherPro Book" , text_col="white"))+
  ggspatial::annotation_scale(location = "bl",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book", text_col="white")+
  labs( x="", y="" )+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "#404040"),
        legend.position = c(0.2,0.2),
        legend.text=element_text(size=8, family="serif"),
        legend.title = element_text(size=8, family="serif", face='bold',hjust=0.5)
  )+
  ggplot2::annotate(geom = "text", x = -69.25, y = -12.59, hjust = 0, vjust = 1, 
                    label = "2015",
                    size = 9, family="serif", color = "white")+
  annotation_custom(Logo_png)+
  ggplot2::annotate(geom = "text", x = -69.6, y =-12.6, hjust = 0, vjust = 1, 
                    label = "Deforestacion de la Microcuenca-Chonta",size = 6, family="serif", color = 
                      "white",  face = "bold")+
                      ggplot2::annotate(geom = "text", x = -69.6, y =-12.62, hjust = 0, vjust = 1, 
                                        label = "Data: High-Resolution Global Maps of 21st-Century Forest Cover Change.",size = 3, family="serif", color = 
                                          "white",  face = "bold")+
                                          ggplot2::annotate(geom = "text", x = -69.6, y =-12.63, hjust = 0, vjust = 1, 
                                                            label = "@Ing. gflorezc",size = 3, family="serif", color = 
                                                              "white",  face = "bold")
                                                            
N

O= ggplot()+
  geom_sf(data = Poligono, fill="black", color="black", size=1)+
  geom_tile(data= Defores_df_2016, aes(x = x, y =  y,  fill=Año), show.legend = F) +
  scale_fill_gradientn(colours = "#008000")+
  ggspatial::annotation_north_arrow(
    location = "br", which_north = "true",
    pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey10", "white"),
      line_col = "grey10", text_family = "ArcherPro Book" , text_col="white"))+
  ggspatial::annotation_scale(location = "bl",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book", text_col="white")+
  labs( x="", y="" )+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "#404040"),
        legend.position = c(0.2,0.2),
        legend.text=element_text(size=8, family="serif"),
        legend.title = element_text(size=8, family="serif", face='bold',hjust=0.5)
  )+
  ggplot2::annotate(geom = "text", x = -69.25, y = -12.59, hjust = 0, vjust = 1, 
                    label = "2016",
                    size = 9, family="serif", color = "white")+
  annotation_custom(Logo_png)+
  ggplot2::annotate(geom = "text", x = -69.6, y =-12.6, hjust = 0, vjust = 1, 
                    label = "Deforestacion de la Microcuenca-Chonta",size = 6, family="serif", color = 
                      "white",  face = "bold")+
                      ggplot2::annotate(geom = "text", x = -69.6, y =-12.62, hjust = 0, vjust = 1, 
                                        label = "Data: High-Resolution Global Maps of 21st-Century Forest Cover Change.",size = 3, family="serif", color = 
                                          "white",  face = "bold")+
                                          ggplot2::annotate(geom = "text", x = -69.6, y =-12.63, hjust = 0, vjust = 1, 
                                                            label = "@Ing. gflorezc",size = 3, family="serif", color = 
                                                              "white",  face = "bold")
                                                            
O

P= ggplot()+
  geom_sf(data = Poligono, fill="black", color="black", size=1)+
  geom_tile(data= Defores_df_2017, aes(x = x, y =  y,  fill=Año), show.legend = F) +
  scale_fill_gradientn(colours = "#008000")+
  ggspatial::annotation_north_arrow(
    location = "br", which_north = "true",
    pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey10", "white"),
      line_col = "grey10", text_family = "ArcherPro Book" , text_col="white"))+
  ggspatial::annotation_scale(location = "bl",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book", text_col="white")+
  labs( x="", y="" )+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "#404040"),
        legend.position = c(0.2,0.2),
        legend.text=element_text(size=8, family="serif"),
        legend.title = element_text(size=8, family="serif", face='bold',hjust=0.5)
  )+
  ggplot2::annotate(geom = "text", x = -69.25, y = -12.59, hjust = 0, vjust = 1, 
                    label = "2017",
                    size = 9, family="serif", color = "white")+
  annotation_custom(Logo_png)+
  ggplot2::annotate(geom = "text", x = -69.6, y =-12.6, hjust = 0, vjust = 1, 
                    label = "Deforestacion de la Microcuenca-Chonta",size = 6, family="serif", color = 
                      "white",  face = "bold")+
                      ggplot2::annotate(geom = "text", x = -69.6, y =-12.62, hjust = 0, vjust = 1, 
                                        label = "Data: High-Resolution Global Maps of 21st-Century Forest Cover Change.",size = 3, family="serif", color = 
                                          "white",  face = "bold")+
                                          ggplot2::annotate(geom = "text", x = -69.6, y =-12.63, hjust = 0, vjust = 1, 
                                                            label = "@Ing. gflorezc",size = 3, family="serif", color = 
                                                              "white",  face = "bold")
                                                            
P

Q= ggplot()+
  geom_sf(data = Poligono, fill="black", color="black", size=1)+
  geom_tile(data= Defores_df_2018, aes(x = x, y =  y,  fill=Año), show.legend = F) +
  scale_fill_gradientn(colours = "#008000")+
  ggspatial::annotation_north_arrow(
    location = "br", which_north = "true",
    pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey10", "white"),
      line_col = "grey10", text_family = "ArcherPro Book" , text_col="white"))+
  ggspatial::annotation_scale(location = "bl",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book", text_col="white")+
  labs( x="", y="" )+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "#404040"),
        legend.position = c(0.2,0.2),
        legend.text=element_text(size=8, family="serif"),
        legend.title = element_text(size=8, family="serif", face='bold',hjust=0.5)
  )+
  ggplot2::annotate(geom = "text", x = -69.25, y = -12.59, hjust = 0, vjust = 1, 
                    label = "2018",
                    size = 9, family="serif", color = "white")+
  annotation_custom(Logo_png)+
  ggplot2::annotate(geom = "text", x = -69.6, y =-12.6, hjust = 0, vjust = 1, 
                    label = "Deforestacion de la Microcuenca-Chonta",size = 6, family="serif", color = 
                      "white",  face = "bold")+
                      ggplot2::annotate(geom = "text", x = -69.6, y =-12.62, hjust = 0, vjust = 1, 
                                        label = "Data: High-Resolution Global Maps of 21st-Century Forest Cover Change.",size = 3, family="serif", color = 
                                          "white",  face = "bold")+
                                          ggplot2::annotate(geom = "text", x = -69.6, y =-12.63, hjust = 0, vjust = 1, 
                                                            label = "@Ing. gflorezc",size = 3, family="serif", color = 
                                                              "white",  face = "bold")
                                                            
Q

R= ggplot()+
  geom_sf(data = Poligono, fill="black", color="black", size=1)+
  geom_tile(data= Defores_df_2019, aes(x = x, y =  y,  fill=Año), show.legend = F) +
  scale_fill_gradientn(colours = "#008000")+
  ggspatial::annotation_north_arrow(
    location = "br", which_north = "true",
    pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey10", "white"),
      line_col = "grey10", text_family = "ArcherPro Book" , text_col="white"))+
  ggspatial::annotation_scale(location = "bl",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book", text_col="white")+
  labs( x="", y="" )+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "#404040"),
        legend.position = c(0.2,0.2),
        legend.text=element_text(size=8, family="serif"),
        legend.title = element_text(size=8, family="serif", face='bold',hjust=0.5)
  )+
  ggplot2::annotate(geom = "text", x = -69.25, y = -12.59, hjust = 0, vjust = 1, 
                    label = "2019",
                    size = 9, family="serif", color = "white")+
  annotation_custom(Logo_png)+
  ggplot2::annotate(geom = "text", x = -69.6, y =-12.6, hjust = 0, vjust = 1, 
                    label = "Deforestacion de la Microcuenca-Chonta",size = 6, family="serif", color = 
                      "white",  face = "bold")+
                      ggplot2::annotate(geom = "text", x = -69.6, y =-12.62, hjust = 0, vjust = 1, 
                                        label = "Data: High-Resolution Global Maps of 21st-Century Forest Cover Change.",size = 3, family="serif", color = 
                                          "white",  face = "bold")+
                                          ggplot2::annotate(geom = "text", x = -69.6, y =-12.63, hjust = 0, vjust = 1, 
                                                            label = "@Ing. gflorezc",size = 3, family="serif", color = 
                                                              "white",  face = "bold")
                                                            
R


ggsave(plot = A ,"Mapas/Deforesta_2001.png", units = "cm", width = 29,height = 21, dpi = 300)
ggsave(plot = B,"Mapas/Deforesta_2002.png", units = "cm", width = 29,height = 21, dpi = 300)
ggsave(plot = C,"Mapas/Deforesta_2003.png", units = "cm", width = 29,height = 21, dpi = 300)
ggsave(plot = D,"Mapas/Deforesta_2004.png", units = "cm", width = 29,height = 21, dpi =300)
ggsave(plot = E,"Mapas/Deforesta_2005.png", units = "cm", width = 29,height = 21, dpi = 300)
ggsave(plot = FF,"Mapas/Deforesta_2006.png", units = "cm", width = 29,height = 21, dpi = 300)
ggsave(plot = G,"Mapas/Deforesta_2007.png", units = "cm", width = 29,height = 21, dpi = 300)
ggsave(plot = H,"Mapas/Deforesta_2008.png", units = "cm", width = 29,height = 21, dpi = 300)
ggsave(plot = I,"Mapas/Deforesta_2009.png", units = "cm", width = 29,height = 21, dpi = 300)
ggsave(plot = J,"Mapas/Deforesta_2010.png", units = "cm", width = 29,height = 21, dpi = 300)
ggsave(plot = K,"Mapas/Deforesta_2011.png", units = "cm", width = 29,height = 21, dpi = 300)
ggsave(plot = L,"Mapas/Deforesta_2012.png", units = "cm", width = 29,height = 21, dpi = 300)
ggsave(plot = LL,"Mapas/Deforesta_2013.png", units = "cm", width = 29,height = 21, dpi = 300)
ggsave(plot = M,"Mapas/Deforesta_2014.png", units = "cm", width = 29,height = 21, dpi = 300)
ggsave(plot = N,"Mapas/Deforesta_2015.png", units = "cm", width = 29,height = 21, dpi = 300)
ggsave(plot = O,"Mapas/Deforesta_2016.png", units = "cm", width = 29,height = 21, dpi = 300)
ggsave(plot = P,"Mapas/Deforesta_2017.png", units = "cm", width = 29,height = 21, dpi = 300)
ggsave(plot = Q,"Mapas/Deforesta_2018.png", units = "cm", width = 29,height = 21, dpi = 300)
ggsave(plot = R,"Mapas/Deforesta_2019.png", units = "cm", width = 29,height = 21, dpi = 300)

require(pacman)
p_load(terra, sf, tidyverse, gtools, RColorBrewer, rgeos, stringr, rnaturalearth, glue, fs, magick)

img = fs::dir_ls("Mapas/")
img 
img <- map(img, image_read)
jnd <- image_join(img)
anm <- magick::image_animate(jnd, fps = 1)


# To write
image_write(image = anm, path = glue('GIF/Deforestacion2001_2019.gif'))









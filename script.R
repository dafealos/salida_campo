library(googledrive)
library(tidyverse)
library(patchwork)
library(sf)
library(raster)
library(ggspatial)
library(cowplot)

# Cargar datos de Google Drive --------------------------------------------

# Ver datos de google Drive
archivos <- drive_find(n_max = 30)

# Desscargar los datos
drive_download(as_id("1PnJ1ehn-BNtTEGtwuRok4JLhbn6QLr9gBCtQUK3xDWA"),
                     overwrite = TRUE)

# Leer los datos en R
datos <- readxl::read_xlsx("Datos.xlsx")

# Renombrando las variables
names(datos) <- sub("data-", "", names(datos))

# Arreglando las variables
datos <- datos %>% separate("localizacion", c("latitud", "longitud"), sep = ",")


# Graficando con GGPLOT2 --------------------------------------------------

#Crear Graficos
p2 <- ggplot(datos) + geom_point(aes(x=pH, y = produccion, color = plaga), size = 4) +
        labs(x = "pH", y = "Produccion kg/ha", title = "Produccion vs pH") +
        theme_bw()+
        theme(legend.position = "none")
        

p3 <- ggplot(datos) + geom_boxplot(aes(x=plaga, y = produccion, fill = plaga)) +
        labs(x = "Presencia de plaga", y = "Produccion kg/ha", title = "Diagrama de cajas") +
        theme_bw()

#Juntando los graficos


grafica_final <- p2 + p3 +
        plot_annotation(
                title = 'Titulo Graficos', 
                tag_levels = "A",
                caption = 'Fuente: datos recolectados por estudiantes',
                theme = theme(plot.title = element_text(size = 15))) + 
        plot_layout(guides = "collect")
           
     
#Guargar el grafico

ggsave(grafica_final, filename = "grafico.jpg", dpi = 300, units = "cm", width = 15, 
       height = 10)



# Mapa de area de estudio -------------------------------------------------

# Ver los puntos en leafleat
datos_1 <- datos %>% st_as_sf(coords = c(6,5), crs = "+proj=longlat +datum=WGS84")
tmap::tmap_mode("view")

tmap::tm_shape(datos_1) + tmap::tm_bubbles(size = 2, col = "red")

#Descargar datos geograficos

col <- getData(name = "GADM", country = "COL", level = 2)

#covertir a foprmato df

col_sf <- st_as_sf(col)

#Extraer el Magdalena

mag <- col_sf %>% filter(NAME_1 == "Magdalena")

# Extraer El Banco

ban <- mag %>% filter(NAME_2 == "El Banco")

#Crear caja alrededor de El Banco

area_ban <- st_as_sfc(st_bbox((ban)))


# Grafico de macrolocalizacion

magdalena <- ggplot() + geom_sf(data = mag, fill = "white") + 
        geom_sf(data = area_ban, fill = NA, color= "red") + 
        ggtitle("Macrolocalizacion") +
        theme_bw() + theme(panel.grid.major = element_blank(),
                           panel.grid.minor = element_blank(),
                           axis.text.x = element_blank(),
                           axis.text.y = element_blank(),
                           plot.title = element_text(hjust = 0.5, size = 7))

# Grafico de El banco

el_banco <- ggplot() + geom_sf(data = ban, fill = "white") + 
        geom_sf(data = datos_1, color = "red") + 
        ggtitle("Puntos de recolecta") + 
        theme_bw()+ 
        annotation_north_arrow(location ="tr",
                                         which_north ="true",
                                         style = north_arrow_fancy_orienteering()) +
        annotation_scale(location = "br") + 
        theme_bw() + 
        xlab("Longitud") + 
        ylab("Latitud")

#Mapa Final

mapa_final <- ggdraw() + 
        draw_plot(el_banco) +
        draw_plot(colombia, x=0.21, y=0.64, width=0.3, height=0.28)

# Guardar Mapa

ggsave(ggsave(mapa_final, filename = "Mapa.jpg", dpi = 300, 
              units = "cm", width = 20, height = 15)) 





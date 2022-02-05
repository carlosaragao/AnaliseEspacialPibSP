pacotes <- c("rgdal","plotly","tidyverse","knitr","kableExtra","gridExtra",
             "png","grid","magick","rgl","devtools","GISTools","rayshader",
             "tmap","broom")

# Instalando última versão do Rayshader
devtools::install_github("tylermorganwall/rayshader")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

# Carregando shapefile dp estado de São Paulo
shp_sp <- readOGR(dsn = "shapefile_sp",
                  layer = "estado_sp")

# Transformar o objeto shp_sp em um Data Frame:
shp_sp_df <- tidy(shp_sp, region = "CD_GEOCMU") %>%
  rename(CD_GEOCMU = id)

# Carregar base de dados de São Paulo
load("dados_sp.RData")

# Alterando nome id de referência dos dados de dados_sp
dados_sp <- dados_sp %>% rename(CD_GEOCMU = codigo)

# Transformando coluna CD_GEOCMU em character
dados_sp$CD_GEOCMU <- as.character(dados_sp$CD_GEOCMU)

# Juntando as informações da base de dados dados_sp
# ao objeto shp_sp_df:
shp_sp_df <- shp_sp_df %>%
  left_join(dados_sp, by = "CD_GEOCMU")

# Gerando um mapa no ggplot2
shp_sp_df %>%
  ggplot(aes(x = long,
             y = lat,
             group = group,
             fill = pib)) +
  geom_polygon() +
  scale_fill_gradient(limits = range(shp_sp_df$pib),
                      low = "#FFF3B0",
                      high = "#E09F3E") +
  layer(geom = "path",
        stat = "identity",
        position = "identity",
        mapping = aes(x = long, 
                      y = lat, 
                      group = group, 
                      color = I('#FFFFFF'))) +
  theme(legend.position = "none",
        axis.line = element_blank(), 
        axis.text.x = element_blank(), 
        axis.title.x = element_blank(),
        axis.text.y = element_blank(), 
        axis.title.y = element_blank(),
        axis.ticks = element_blank(), 
        panel.background = element_blank())

# Salvar o mapa acima em um objeto
mapa_sp <- shp_sp_df %>%
  ggplot(aes(x = long,
             y = lat,
             group = group,
             fill = pib)) +
  geom_polygon() +
  scale_fill_gradient(limits = range(shp_sp_df$pib),
                      low = "#FFF3B0",
                      high = "#E09F3E") +
  layer(geom = "path",
        stat = "identity",
        position = "identity",
        mapping = aes(x = long, 
                      y = lat, 
                      group = group, 
                      color = I('#FFFFFF'))) +
  theme(legend.position = "none",
        axis.line = element_blank(), 
        axis.text.x = element_blank(), 
        axis.title.x = element_blank(),
        axis.text.y = element_blank(), 
        axis.title.y = element_blank(),
        axis.ticks = element_blank(), 
        panel.background = element_blank())

# Salvar o objeto mapa_sp em um arquiv de extensão *.png
# com uma boa resolução:
xlim <- ggplot_build(mapa_sp)$layout$panel_scales_x[[1]]$range$range
ylim <- ggplot_build(mapa_sp)$layout$panel_scales_y[[1]]$range$range

ggsave(filename = "mapa_sp.png",
       width = diff(xlim) * 4,
       height = diff(ylim) * 4,
       units = "cm")

# Carregando o arquivo mapa_sp.png:
background_mapa <- readPNG("mapa_sp.png")

# Capturando as coordenadas dos centroides de cada município de SP em um 
# Data Frame:
coordinates(shp_sp) %>%
  data.frame() %>%
  rename(longitude = 1,
         latitude = 2) %>%
  mutate(CD_GEOCMU = shp_sp@data$CD_GEOCMU) %>%
  dplyr::select(latitude, everything()) -> coords_sp

# Adicionando as coordenadas dos municpipios de São Paulo
# no objeto shp_sp_df:
shp_sp_df <- shp_sp_df %>%
  left_join(coords_sp, by = "CD_GEOCMU")

# Georeferenciando a imagem PNG e plotando as marcações sobre o PIB
# em SP nos centroides de cada polígono
shp_sp_df %>%
  ggplot() +
  annotation_custom(
    rasterGrob(background_mapa,
               width = unit(1, "npc"),
               height = unit(1, "npc")), -Inf, Inf, -Inf, Inf) +
  xlim(xlim[1],xlim[2]) + # x-axis Mapping
  ylim(ylim[1],ylim[2]) + # y-axis Mapping
  geom_point(aes(x = longitude, y = latitude, color = pib), size = 1.5) +
  scale_colour_gradient(name = "PIB", 
                        limits = range(shp_sp_df$pib), 
                        low = "#FCB9B2", 
                        high = "#B23A48") + 
  theme(axis.line = element_blank(), 
        axis.text.x = element_blank(), 
        axis.title.x = element_blank(),
        axis.text.y = element_blank(), 
        axis.title.y = element_blank(),
        axis.ticks = element_blank(), 
        panel.background = element_blank())

# Salvando resultado em um objeto:
mapa_pib <- shp_sp_df %>%
  ggplot() +
  annotation_custom(
    rasterGrob(background_mapa,
               width = unit(1, "npc"),
               height = unit(1, "npc")), -Inf, Inf, -Inf, Inf) +
  xlim(xlim[1],xlim[2]) + # x-axis Mapping
  ylim(ylim[1],ylim[2]) + # y-axis Mapping
  geom_point(aes(x = longitude, y = latitude, color = pib), size = 1.5) +
  scale_colour_gradient(name = "PIB", 
                        limits = range(shp_sp_df$pib), 
                        low = "#FCB9B2", 
                        high = "#B23A48") + 
  theme(axis.line = element_blank(), 
        axis.text.x = element_blank(), 
        axis.title.x = element_blank(),
        axis.text.y = element_blank(), 
        axis.title.y = element_blank(),
        axis.ticks = element_blank(), 
        panel.background = element_blank())

# Gerando o mapa 3d do PIB de SP:
plot_gg(ggobj = mapa_pib,
        width = 11,
        height = 6,
        scale = 300,
        multicore = TRUE,
        windowsize = c(1000, 800))

# Melhorando o resultado:
render_camera(fov = 70, 
              zoom = 0.5, 
              theta = 130, 
              phi = 35)

# Gerando vídeo do mapa gerado:
azimute_metade <- 30 + 60 * 1/(1 + exp(seq(-7, 20, length.out = 180)/2))
azimute_completo <- c(azimute_metade, rev(azimute_metade))

rotacao <- 0 + 45 * sin(seq(0, 359, length.out = 360) * pi/180)

zoom_metade <- 0.45 + 0.2 * 1/(1 + exp(seq(-5, 20, length.out = 180)))
zoom_completo <- c(zoom_metade, rev(zoom_metade))

render_movie(filename = "video_sp", 
             type = "custom", 
             frames = 360, 
             phi = azimute_completo, 
             zoom = zoom_completo, 
             theta = rotacao)

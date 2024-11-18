#' ---
#' title: "API rNOMADS para acessar previsões NCEP | NOAA"
#' subtitle: "Previsão da precipitação acumulada (mm) para os próximos 15 dias"
#' author: Vinicio Coelho Lima
#' email: viniciovcl@gmail.com
#' date: Novembro, 2024
#' output:
#'    pdf_document:
#'      toc: true
#'      toc_depth: 4
#'      highlight: tango
#'      latex_engine: lualatex
#'    html_document:
#'      toc: true
#'      toc_depth: 4
#'      highlight: tango
#' header-includes:
#'     - \usepackage{float}
#' ---




#' ## rNOMADS
#'
#' NOMADS (NOAA Operational Model Archive and Distribution System) é um
#' sistema de distribuição e arquivamento de dados operacionais gerenciado
#' pela NOAA (National Oceanic and Atmospheric Administration). Ele fornece
#' acesso a uma ampla gama de dados meteorológicos, climáticos e oceanográficos
#' gerados por modelos numéricos de previsão e reanálises. Fonte: <https://nomads.ncep.noaa.gov/>.
#'
#' O rNOMADS é uma interface para o sistema NOMADS que pode recuperar dados binários
#' em formato grib, bem como importar dados ascii diretamente para o R por
#' meio da interface com o sistema GrADS-DODS. Fonte: <https://r-forge.r-project.org/projects/rnomads/>.

#+ pacotes, echo=TRUE, eval=TRUE, warning=FALSE, message= FALSE


library(rNOMADS)
# update.packages(oldPkgs = "rNOMADS")
library(raster)



#' ### Modelos

#+ modelos, echo=TRUE,eval=TRUE, warning=FALSE, message= FALSE

#  ascii
model.list  <-  NOMADSRealTimeList ( "dods" )

modelos <- aggregate(url ~ name,
                     data = model.list, FUN = paste, collapse = ", ")

knitr::kable(modelos, row.names = TRUE)



#' ### Global Forecast System
#'
#' O Global Forecast System (GFS) é um modelo de previsão do tempo produzido pelo
#' National Centers for Environmental Prediction (NCEP). Dezenas de variáveis
#' atmosféricas e de solo estão disponíveis através deste conjunto de dados, desde
#' temperaturas, ventos e precipitação até umidade do solo e concentração de ozônio
#' na atmosfera.
#'
#'
#' Mudanças são feitas regularmente no modelo GFS para melhorar seu desempenho e
#' precisão de previsão. Este conjunto de dados é executado quatro vezes ao dia às
#' 00z, 06z, 12z e 18z até 192 horas com uma resolução horizontal de 0,5 graus e
#' uma resolução temporal de 3 horas.
#'
#'


#' ###  gfs_0p50
#'
#'
#'
#' O modelo "0p50" refere-se a uma grade global com intervalos de 0,50 graus (~55 km entre os pontos de grade na linha do Equador).
#' Oferece um nível intermediário de detalhamento espacial em comparação a versões mais finas (0,25 graus) ou mais grossas (1,00 grau).
#'
#'
#'
#' Fornece previsões de curto a médio prazo (até 384 horas, ou 16 dias). As saídas podem ser em intervalos de 3 ou 6 horas, dependendo da configuração.

#+ 0p_50, echo=TRUE, eval=TRUE, warning= FALSE, message= FALSE

model.urls <- GetDODSDates("gfs_0p50")




#' ## Região de interesse
#'
#+ limite, echo=TRUE,eval=TRUE, warning= FALSE, message= FALSE

lat <- -19.78753
lon <- -51.98899

#' ## Configurando a grade

#+ grade, echo=TRUE, eval=TRUE, warning=FALSE, message= FALSE

# Subset
lons <- seq(0, 359.5, by = 0.5)
lats <- seq(-90, 90, by = 0.5)

lon.diff <- abs(lon + 360 - lons)
lat.diff <- abs(lat - lats)

model.lon.ind <- which(lon.diff == min(lon.diff)) - 1 # Indexado no 0
model.lat.ind <- which(lat.diff == min(lat.diff)) - 1

lon.inds <- c(model.lon.ind - 12, model.lon.ind + 12) # região
lat.inds <- c(model.lat.ind - 14, model.lat.ind + 14)


#' ## Modelo mais recente
#'
#+ modelo_recente, echo=TRUE,eval=TRUE, warning= FALSE, message= FALSE


latest.model <- tail(model.urls$url, 1)
model.runs <- GetDODSModelRuns(latest.model)
model.runs

latest.model.run <- tail(model.runs$model.run, 1)
latest.model.run


#' ## Variáveis disponíveis
#'
#+ var, echo=TRUE, eval=TRUE, warning=FALSE, message= FALSE

model.info <- GetDODSModelRunInfo(latest.model, tail(model.runs$model.run, 1))

model.info.var <- model.info[c(10,11,28,43,141:148,170,180,201:204,216,220,239)]

knitr::kable(as.data.frame(model.info.var), row.names = TRUE)

#' ## Precipitação
#'
#+ precipit, echo=TRUE, eval=TRUE, warning=FALSE, message= FALSE

variables <- "acpcpsfc" # Accumulated precipitation surface (mm)


#' ## Previsão

#' A variável time no DODSGrab() é um vetor de dois componentes, como
#' c(start, end), que define os índices do intervalo de tempo. O modelo GFS fornece
#' previsões a cada 3 horas:
#'
#'    - time = c(0, 0) Previsão para o tempo mais atual.
#'    - time = c(8, 8) 24 horas à frente (8 × 3 horas = 24 horas).
#'    - time = c(116, 116) 116 × 3 horas = 348 horas, ou cerca de 14,5 dias.
#'
#'
#'

#+ forecast, echo=TRUE, eval=TRUE, warning=FALSE, message= FALSE

time <- c(116,116) # Status de inicialização

model.data <- DODSGrab(latest.model, latest.model.run, variables,
                       time, lon.inds , lat.inds)


hoje <- Sys.time()
hoje

forecast <- ModelGrid(model.data, c(0.5, 0.5))
forecast$fcst.date


#' ## Mapa para o modelo

#+ mapa, echo=TRUE,eval=TRUE, warning=FALSE, message= FALSE


prec <- list()
prec$x <- forecast$x
prec$y <- forecast$y
prec$z <- forecast$z[1,1,,]
r <- raster::raster(prec)
r2 <- raster::rotate(r)
crs_target <- CRS("+proj=longlat +datum=WGS84 +no_defs")
r3 <- projectRaster(r2, crs = crs_target)
# Converter raster para data.frame para ggplot
r_df <- as.data.frame(r3, xy = TRUE, na.rm = TRUE)
colnames(r_df) <- c("lon", "lat", "precip")


#+ s2_IBGE, echo=FALSE,eval=TRUE, warning=FALSE, message= FALSE, results = FALSE

library(dplyr)
library(sf)

pts <- st_sf(pt = 1:2,
             geom = st_sfc(st_point(c(
               -61.633383, -33.751178
             )), st_point(c(
               -39.856829, -7.349028
             ))), crs = 4326)

pol <- pts %>% st_bbox() %>% st_as_sfc(., crs = 4326)
mask <- st_as_text(st_geometry(pol))

bc_ibge <- "/home/vinicio/Documentos/bc250_ibge.gpkg"


cidade <- st_read(bc_ibge, layer = "lml_cidade_p",
                  wkt_filter = mask) %>% # Subset limite exemplo
  dplyr::filter(nome %in% c("Bataguassu",#MS
                            "Inocência", #MS
                            "Dourados", # MS
                            "Ortigueira", #PR
                            "Cascavel", # PR
                            "Uberlândia", # MG
                            "Bauru", # SP
                            "Ribeirão Preto", # SP
                            "Rio Verde", # GO
                            "Alto Araguaia", # GO
                            "Passo Fundo") # RS
  )


capital <- st_read(bc_ibge, layer = "lml_capital_p") %>% dplyr::select( - tipocapital)
mun_label <- rbind(cidade, capital)

uf <- st_read(bc_ibge, layer = "lml_unidade_federacao_a")

mun_label_coords <- cbind(mun_label, st_coordinates(mun_label))

#' ## Base Cartográfica IBGE

#+ IBGE, echo=TRUE,eval=FALSE, warning=FALSE, message= FALSE

library(dplyr)
library(sf)

pts <- st_sf(pt = 1:2,
             geom = st_sfc(st_point(c(
               -61.633383, -33.751178
             )), st_point(c(
               -39.856829, -7.349028
             ))), crs = 4326)

pol <- pts %>% st_bbox() %>% st_as_sfc(., crs = 4326)
subset_reg <- st_as_text(st_geometry(pol))

bc_ibge <- "/IBGE/BC_250/bc250_ibge.gpkg"
layers <- st_layers(bc_ibge)

# Sys.setlocale("LC_ALL", "pt_BR.UTF-8")
cidade <- st_read(bc_ibge, layer = "lml_cidade_p",
                  wkt_filter = subset_reg) %>% # Evita carregar todos o municipios
  dplyr::filter(nome %in% c("Bataguassu",#MS
                            "Inocência", #MS
                            "Dourados", # MS
                            "Ortigueira", #PR
                            "Cascavel", # PR
                            "Uberlândia", # MG
                            "Bauru", # SP
                            "Ribeirão Preto", # SP
                            "Rio Verde", # GO
                            "Alto Araguaia", # GO
                            "Passo Fundo")) # RS

capital <- st_read(bc_ibge, layer = "lml_capital_p") %>% dplyr::select( - tipocapital)
mun_label <- rbind(cidade, capital)

uf <- st_read(bc_ibge, layer = "lml_unidade_federacao_a")

mun_label_coords <- cbind(mun_label, st_coordinates(mun_label))


#' ## Plot

#+ Plot, echo=TRUE,eval=FALSE, warning=FALSE, message= FALSE


library(ggplot2)

ext.mapa <- c(xmin = -58.0, xmax = -46.25, ymin = -28, ymax = -13)

# Definir a codificação de caracteres para UTF-8
# Sys.setlocale("LC_ALL", "pt_BR.UTF-8")

 p <- ggplot() +
   geom_raster(data = r_df, aes(x = lon, y = lat, fill = precip)) +
   scale_fill_gradientn(
     colors = c("#FFFFCC", "#41B6C4", "#0C2C84"),
     name = "",
     na.value = "transparent"
   ) +
   geom_sf(
     data = uf,
     fill = "lightblue",
     color = "black",
     alpha = .10
   ) +
   geom_sf(data = mun_label,
           color = "red",
           size = .15) +
   geom_text(
     data = mun_label_coords,
     aes(x = X, y = Y, label = nome),
     size = 2.25,
     nudge_y = .25,
     color = "red",
     fontface = "bold",
     check_overlap = TRUE
   ) +
   coord_sf(xlim = c(ext.mapa["xmin"], ext.mapa["xmax"]),
            ylim = c(ext.mapa["ymin"], ext.mapa["ymax"])) +
   theme_minimal()  +
   labs(
     title = "Modelo GFS 0.5 deg NCEP",
     subtitle = "Precipitação acumulada (mm) em 348 horas
              <2024-12-02 00:00:00 GMT>",
     caption = "gfs_0p50_12z: GFS 0.5 deg starting from 12Z17nov2024, 17:13 UTC"
   ) +
   theme(
     plot.title = element_text(
       size = 22,
       face = "bold",
       family = "serif"
     ),
     plot.subtitle = element_text(
       size = 12,
       face = "plain",
       family = "mono"
     ),
     plot.caption = element_text(
       size = 10,
       face = "plain",
       family = "mono"
     ),
     axis.text = element_text(family = "mono"),
     axis.text.x = element_text(family = "mono"),
     axis.text.y = element_text(family = "mono"),
     legend.key.height = unit(0.10, 'npc'),
     legend.key.width = unit(0.04, 'npc')
   )

 ggsave(
   plot = p,
   filename = "./mapa_modelo_gfs.png",
   width = 9.5,
   height = 12.5,
   units = "cm",
   device = "png",
   dpi = 200,
   bg = "white"
 )


#+ fig_plot, echo=FALSE,eval=TRUE, warning=FALSE, message= FALSE, out.height = '100%',  fig.align='center', fig.pos="H"

knitr::include_graphics("../mapa_modelo_gfs.png")

#'
#' ## Referência:
#'
#' Bowman D (2014). rNOMADS: An interface to the NOAA Operational Model Archive and Distribution System. R package version 2.0.2, https://r-forge.r-project.org/projects/rnomads.

#'
#' ## Apêndice:
#'
#' ### Sessão R
#'

#+ env, echo=TRUE,eval=FALSE, warning=FALSE, message= FALSE

sessionInfo()

#+ env_, echo=FALSE,eval=TRUE, warning=FALSE, message= FALSE

env <- sessionInfo()
env


#+ render, echo=TRUE,eval=FALSE, warning=FALSE, message= FALSE

rmarkdown::render("./R/rNOMADS_gfs_forecast.R", output_format = "pdf_document")




#+ codigo_leaflet, echo=FALSE,eval=FALSE, warning=FALSE, message= FALSE

# leaflet
library(leaflet)
#hortos.latlon <- st_transform(hortos, 4326)

pal <- colorNumeric(c("#FFFFCC", "#41B6C4", "#0C2C84" ), values(r3),
                    na.color = "transparent")

# titulo do mapa ================================
# library(leaflet)
library(htmlwidgets)
library(htmltools)

d.1 <- format(Sys.Date(),"%Y-%b-%d")
d.2 <- format(as.Date(dias.7$fcst.date), "%Y-%b-%d")
t1 <- paste0("nco.ncep.noaa.gov/pmb/products/gfs/", "     ")
t2 <- paste0("De ", format(Sys.Date(),"%Y-%b-%d")," ate ", forecast.7$fcst.date)




tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title {
    transform: translate(-90%,20%);
    position: fixed !important;
    left: 50%;
    text-align: center;
    padding-left: 10px;
    padding-right: 75px;
    background: rgba(255,255,255,0.75);
    font-weight: bold;
    font-size: 12px;
  }
"))
title <- tags$div(
  tag.map.title, HTML( paste(t1, t2, sep="<br/>"))
)


# =====================================

leaflet() %>% addTiles() %>%
  addControl(title, position = "topleft", className="map-title") %>%
  #addPolygons(data = hortos.latlon, fill = FALSE, stroke = TRUE, color = "#03F") %>%
  addRasterImage(r3, colors = pal, opacity = 0.80) %>%
  addLegend(pal = pal, values = values(r3),
            #labFormat = labelFormat(transform = function(r2) sort(r2, decreasing = TRUE)),
            title = "Acumulado (mm)")




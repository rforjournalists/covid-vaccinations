library(tidyverse)
library(sf)
library(shiny)
library(devtools)
#devtools::install_github("yutannihilation/ggsflabel", force = TRUE)
#library(ggsflabel)

#copyright notice for ggsflabel functions
#Copyright (c) 2018 Hiroaki Yutani
#Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
#The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
#THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

function(input, output) {
  
  #' Limits for 'sf'
  #'
  #' Set scale limits based on the 'bbox' of 'sf' object.
  #'
  #' @name lims_bbox
  #' @param x
  #'   An object of \code{sf}.
  #'
  #' @examples
  #' nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
  #' points_sfg <- sf::st_multipoint(as.matrix(expand.grid(x = -90:-70, y = 30:40)))
  #' points_sfc <- sf::st_sfc(points_sfg, crs = sf::st_crs(nc))
  #'
  #' p <- ggplot() +
  #'   geom_sf(data = nc, aes(fill = AREA)) +
  #'   geom_sf(data = points_sfc)
  #'
  #' # too wide
  #' p
  #'
  #' # shrink the limits to the bbox of nc
  #' p + lims_bbox(nc)
  #'
  #' @export
  lims_bbox <- function(x) {
    if (!inherits(x, "sf") && !inherits(x, "sfc")) {
      stop("x is not an sf object!", call. = FALSE)
    }
    
    bbox <- sf::st_bbox(x)
    ggplot2::lims(
      x = unname(bbox[c("xmin", "xmax")]),
      y = unname(bbox[c("ymin", "ymax")])
    )
  }
  
  #' Label and Text 'Geom's
  #'
  #' Provides label and text \code{geom}s that automatically retrieve the sf object's coordinates.
  #'
  #' @name geom_sf_label
  #' @importFrom ggplot2 stat
  #' @inheritParams ggplot2::geom_label
  #' @param ...
  #'   Other arguments passed to the underlying function.
  #' @param fun.geometry
  #'   A function that takes a \code{sfc} object and returns a
  #'   \code{sfc_POINT} with the same length as the input (e.g. \link[sf]{st_point_on_surface}).
  #'
  #' @details
  #' These functions are thin wrappers of usual geoms like \code{geom_label()}, the only difference is that
  #' they use \code{StatSfCoordinates} for \code{stat}. More precisely:
  #'
  #' \itemize{
  #'   \item \code{geom_sf_label()} is the thin wrapper of \link[ggplot2]{geom_label}.
  #'   \item \code{geom_sf_text()} is the thin wrapper of \link[ggplot2]{geom_text}.
  #'   \item \code{geom_sf_label_repel()} is the thin wrapper of \link[ggrepel]{geom_label_repel}.
  #'   \item \code{geom_sf_text_repel()} is the thin wrapper of \link[ggrepel]{geom_text_repel}.
  #' }
  #'
  #' @section Computed variables:
  #'   Depending on the type of given sfc object, some variables between \code{X}, \code{Y}, \code{Z}
  #'   and \code{M} are available. Please read Esamples section how to use these.
  #'
  #' @examples
  #' nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
  #' # st_point_on_surface may not give correct results for longitude/latitude data
  #' nc_3857 <- sf::st_transform(nc, 3857)
  #'
  #' # geom_label() for sf
  #' ggplot(nc_3857[1:3, ]) +
  #'   geom_sf(aes(fill = AREA)) +
  #'   geom_sf_label(aes(label = NAME))
  #'
  #' # ggrepel::geom_label_repel() for sf
  #' ggplot(nc_3857[1:3, ]) +
  #'   geom_sf(aes(fill = AREA)) +
  #'   geom_sf_label_repel(
  #'     aes(label = NAME),
  #'     # additional parameters are passed to geom_label_repel()
  #'     nudge_x = -5, nudge_y = -5, seed = 10
  #'   )
  #'
  #' # Of course, you can use StatSfCoordinates with any geoms.
  #' ggplot(nc_3857[1:3, ]) +
  #'   geom_sf(aes(fill = AREA)) +
  #'   geom_point(aes(geometry = geometry,stat(X), stat(Y)),
  #'              stat = StatSfCoordinates,
  #'              fun.geometry = sf::st_centroid,
  #'              colour = "white", size = 10)
  NULL
  
  #' @rdname geom_sf_label
  #' @export
  StatSfCoordinates <- ggplot2::ggproto(
    "StatSfCoordinates", ggplot2::Stat,
    compute_group = function(data, scales, fun.geometry) {
      points_sfc <- fun.geometry(data$geometry)
      coordinates <- sf::st_coordinates(points_sfc)
      data <- cbind(data, coordinates)
      
      data
    },
    
    default_aes = ggplot2::aes(x = stat(X), y = stat(Y)),
    required_aes = c("geometry")
  )
  
  geom_sf_label_variants <- function(mapping = NULL,
                                     data = NULL,
                                     fun.geometry,
                                     geom_fun,
                                     ...) {
    if (is.null(mapping$geometry)) {
      geometry_col <- attr(data, "sf_column") %||% "geometry"
      mapping$geometry <- as.name(geometry_col)
    }
    
    geom_fun(
      mapping = mapping,
      data = data,
      stat = StatSfCoordinates,
      fun.geometry = fun.geometry,
      ...
    )
  }
  
  #' @name geom_sf_label
  #' @export
  geom_sf_label <- function(mapping = NULL,
                            data = NULL,
                            fun.geometry = sf::st_point_on_surface,
                            ...) {
    geom_sf_label_variants(
      mapping = mapping,
      data = data,
      fun.geometry = fun.geometry,
      geom_fun = ggplot2::geom_label,
      ...
    )
  }
  
  #' @name geom_sf_label
  #' @export
  geom_sf_text <- function(mapping = NULL,
                           data = NULL,
                           fun.geometry = sf::st_point_on_surface,
                           ...) {
    geom_sf_label_variants(
      mapping = mapping,
      data = data,
      fun.geometry = fun.geometry,
      geom_fun = ggplot2::geom_text,
      ...
    )
  }
  
  
  #' @name geom_sf_label
  #' @export
  geom_sf_label_repel <- function(mapping = NULL,
                                  data = NULL,
                                  fun.geometry = sf::st_point_on_surface,
                                  ...) {
    geom_sf_label_variants(
      mapping = mapping,
      data = data,
      fun.geometry = fun.geometry,
      geom_fun = ggrepel::geom_label_repel,
      ...
    )
  }
  
  #' @name geom_sf_label
  #' @export
  geom_sf_text_repel <- function(mapping = NULL,
                                 data = NULL,
                                 fun.geometry = sf::st_point_on_surface,
                                 ...) {
    geom_sf_label_variants(
      mapping = mapping,
      data = data,
      fun.geometry = fun.geometry,
      geom_fun = ggrepel::geom_text_repel,
      ...
    )
  }
  
  #' Labels for 'sf' with 'ggplot2'
  #'
  #' Provides several 'geom's which plot labels generated from 'sf' data.
  #'
  #' @name ggsflabel
  #' @docType package
  #' @importFrom rlang %||%
  "_PACKAGE"
 
  vacc <- read_csv('vaccinations.csv')
  #get shapefile
  msoa_detailed <- read_sf('Middle_Layer_Super_Output_Areas__December_2011__Boundaries_EW_BFE.shp')
  
  #read in population
  msoa_pop <- read_csv('msoa_pop.csv')
  #add up 65+
  msoa_pop_65 <- msoa_pop[,73:98]
  msoa_pop_65$total <- rowSums(msoa_pop_65)
  
  #add to vacc
  msoa_pop_65 <- cbind(msoa_pop[,1:6], msoa_pop_65$total)
  names(msoa_pop_65)[7] <- 'total_over_65'
  
  vacc <- merge(vacc, msoa_pop_65, by.x = 'MSOA_code', by.y = 'MSOA Code')
  
  #calc percentage
  vacc$over_65_vaccinated <- rowSums(vacc[,8:11])
  vacc$pc <- vacc$over_65_vaccinated / vacc$total_over_65 * 100
  
  #merge shapefile in
  
  vacc_shp <- merge(vacc, msoa_detailed, by.x = 'MSOA_code', by.y = 'MSOA11CD')
  vacc_shp <- arrange(vacc_shp, MSOA_code)
  #vacc_shp <- sf::st_transform(vacc_shp, crs = 4326)
  
  #get date period
  vacc_date <- '8th December to 28th February'
  
  
  output$vaccinePlotLA <- renderPlot({
    
    df <- vacc_shp[vacc_shp$LA_name == input$LA,]
    df$MSOA_name <- gsub('&','&\n', df$MSOA_name)
    df$MSOA_name <- gsub(' and ',' and \n', df$MSOA_name)
    df$MSOA_name <- gsub(' South West',' SW.', df$MSOA_name)
    df$MSOA_name <- gsub(' North West',' NW.', df$MSOA_name)
    df$MSOA_name <- gsub(' South East',' SE.', df$MSOA_name)
    df$MSOA_name <- gsub(' North East',' NE', df$MSOA_name)
    df$MSOA_name <- gsub(' West',' W.', df$MSOA_name)
    df$MSOA_name <- gsub(' East',' E.', df$MSOA_name)
    df$MSOA_name <- gsub(' South',' S.', df$MSOA_name)
    df$MSOA_name <- gsub(' North',' N.', df$MSOA_name)
    df$MSOA_name <- gsub(' Central',' Cen.', df$MSOA_name)
    df$MSOA_name <- gsub(' Junction',' Jun.', df$MSOA_name)
    df$MSOA_name <- gsub('Centre','Cen.', df$MSOA_name)
    df$MSOA_name <- gsub(' Road',' Rd', df$MSOA_name)
    df$MSOA_name <- gsub(' Street',' St', df$MSOA_name)
    df$MSOA_name <- gsub(' Lane',' Ln', df$MSOA_name)
    labels <- df
    
    if(nrow(labels) >= 25 && nrow(labels) <= 50) {
      seq <- seq(1,nrow(labels),by=2)
      labels$num <- seq(1,nrow(labels), by = 1)
      labels <- subset(labels, num %in% seq)
    } else if(nrow(labels) >=51) {
        seq <- seq(1,nrow(labels),by=5)
        labels$num <- seq(1,nrow(labels), by = 1)
        labels <- subset(labels, num %in% seq)
    }
    
    if(input$LA != 'Your authority' && input$LA != '') {
      p <- ggplot() + 
        geom_sf(data = df, aes(geometry = geometry, fill = pc), color = NA)   + scale_fill_distiller(palette = "YlOrBr", direction = -1, trans = 'reverse')  + labs( 
          title = paste0(input$LA),
          subtitle ='Aged 65+', 
          fill = "At least one dose (%)", 
          caption = paste0('Source: NIMS\n',vacc_date)) + theme_void(base_size = 18)+ theme(legend.position='bottom', legend.text=element_text(size=8)) + geom_sf_text_repel(data = labels, aes(label = MSOA_name), segment.colour = NA, size = 4, inherit.aes = TRUE, force = 2, ylim = c(-Inf, Inf), lineheight = .75, colour = 'black') 
      p
    }

    
  })
  
}








#win ratio


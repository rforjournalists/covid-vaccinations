library(tidyverse)
library(sf)
library(shiny)
library(devtools)
devtools::install_github("yutannihilation/ggsflabel")
library(ggsflabel)

vacc <- readxl::read_xlsx('COVID-19-weekly-announced-vaccinations-4-March-2021-1.xlsx', sheet = 'MSOA', skip = 14, col_names = FALSE)

#add new col names
names(vacc) <- c('region_code','region_name','LA_code','LA_name','MSOA_code','MSOA_name','Under 65','65-69','70-74','75-79','80+')

#get shapefile
msoa_detailed <- read_sf('Middle_Layer_Super_Output_Areas__December_2011__Boundaries_EW_BFE.shp')

#read in population
msoa_pop <- readxl::read_xlsx('SAPE22DT4-mid-2019-msoa-syoa-estimates-unformatted.xlsx', sheet = 'Mid-2019 Persons', skip = 3)

#print off for shiny app
write_csv(vacc, 'vaccinations.csv')
write_csv(msoa_pop, 'msoa_pop.csv')


#add up 65+
msoa_pop_65 <- msoa_pop[,73:98]
msoa_pop_65$total <- rowSums(msoa_pop_65)

#cut data frame to just 65+
msoa_pop_65 <- cbind(msoa_pop[,1:6], msoa_pop_65$total)
names(msoa_pop_65)[7] <- 'total_over_65'

#add to vacc
vacc <- merge(vacc, msoa_pop_65, by.x = 'MSOA_code', by.y = 'MSOA Code')

#calc percentage
vacc$over_65_vaccinated <- rowSums(vacc[,8:11])
vacc$pc <- vacc$over_65_vaccinated / vacc$total_over_65 * 100

#merge shapefile in

vacc_shp <- merge(vacc, msoa_detailed, by.x = 'MSOA_code', by.y = 'MSOA11CD')
vacc_shp <- arrange(vacc_shp, MSOA_code)
#vacc_shp <- sf::st_transform(vacc_shp, crs = 4326)

#get date period
vacc_date <- readxl::read_xlsx('COVID-19-weekly-announced-vaccinations-4-March-2021-1.xlsx', sheet = 'MSOA')
vacc_date <- vacc_date[2,2] %>% as.character()
vacc_date <- gsub('2020 ','',vacc_date)
vacc_date <- gsub('2021','',vacc_date)
vacc_date <- trimws(vacc_date)


#choose a LA
chosen_LA <- 'Hammersmith and Fulham'

df <- vacc_shp[vacc_shp$LA_name == chosen_LA ,]
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


  p <- ggplot() + 
    geom_sf(data = df, aes(geometry = geometry, fill = pc), color = NA)   + scale_fill_distiller(palette = "YlOrBr", direction = -1, trans = 'reverse')  + labs( 
      title = paste0(chosen_LA ),
      subtitle ='Aged 65+', 
      fill = "At least one dose (%)", 
      caption = paste0('Source: NIMS\n',vacc_date)) + theme_void(base_size = 18)+ theme(legend.position='bottom', legend.text=element_text(size=8)) + geom_sf_text_repel(data = labels, aes(label = MSOA_name), segment.colour = NA, size = 4, inherit.aes = TRUE, force = 2, ylim = c(-Inf, Inf), lineheight = .75, colour = 'black') 
  p




  
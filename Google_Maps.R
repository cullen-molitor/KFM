library(tidyverse)
library(ggmap)
library(sf)

register_google("AIzaSyCfJAK8E0rkv4X3f_iQ-Jg8g5WRNd555P4")

mpa <- st_read("California_Marine_Protected_Areas.shp")

mpa <- st_as_sf(mpa)

marine <- mpa %>%
  filter(Type == "SMR" |
           Type == "FMCA" |
           Type == "SMCA" |
           Type == "FMR") %>%
  mutate(Color = ifelse(Type == "SMR", "red",
                        ifelse(Type == "SMCA", "blue",
                               ifelse(Type == "FMR", "orange", "purple"))))

TransectSites <- siteInfo2 %>% 
  filter(SiteNumber != 1 & SiteNumber != 5 & SiteNumber != 11) 

transects <- st_read("KFM_Transects_SmoothLine5.shp")  %>%
  st_as_sf() %>%
  mutate(geometry = st_transform(geometry, "+proj=longlat +ellps=WGS84 +datum=WGS84"))

Transect_Endpoints <- read_csv('transect_0_100.csv')

NPS_boundary <- st_read("nps_boundary.shp") %>%
  st_as_sf()

CINMS_boundary <- st_read("cinms_py.shp") %>%
  st_as_sf()

{ # CHIS MPA   ----
  CHIS <- get_googlemap(c(-119.8, 33.90037),
                        zoom = 9,
                        maptype = "satellite",
                        style = c(labels = "off"))
  
  png(filename = "CHIS.png",
      width = 1000, height = 1000, type = "cairo")
  
  ggmap(CHIS, extent = "device") +
    scale_fill_manual(values = c('SMR' = "green", 'FMR' = "deepskyblue2", 'SMCA' = "blue2", 'FMCA' = "red2")) +
    scale_color_manual(values = c('SMR' = "green", 'FMR' = "deepskyblue2", 
                                  'SMCA' = "blue2", 'FMCA' = "red2",
                                  'Inside' = "green", 'Outside' = "red2")) +
    labs(title = NULL,
         x = NULL,
         y = NULL) +
    theme_classic() +
    theme(legend.position = "none",
          legend.title = element_text(),
          axis.text = element_blank())
  
  dev.off()
}

{ # South Point SMR   ----
SouthPointSMR <- get_googlemap(c(-120.1357, 33.90037),
                           zoom = 13,
                           maptype = "satellite",
                           style = c(labels = "off"))

png(filename = "SRI.png",
    width = 1000, height = 1000, type = "cairo")

ggmap(SouthPointSMR, extent = "device") +
  geom_sf(data = marine, aes(fill = Type, color = Type),
          inherit.aes = FALSE, alpha = .1) +
  geom_sf_label(data = marine, inherit.aes = FALSE, hjust = .5, vjust = .5, size = 6,
                aes(label = FULLNAME)) +
  geom_sf(data = transects, aes(color = Site_Code),
          inherit.aes = FALSE, alpha = .6, color = "lightblue", size = 1) +
  geom_sf(data = NPS_boundary, inherit.aes = FALSE, color = "green", size = 1, alpha = 0) +
  # geom_sf(data = CINMS_boundary, inherit.aes = FALSE, color = "blue", size = 1, alpha = 0) +
  geom_point(data = siteInfo,
             aes(x = Longitude, y = Latitude, color = ReserveStatus),
             size = 2, inherit.aes = FALSE) +
  geom_label(data = siteInfo, hjust = .1, vjust = 1, size = 6,
             aes(x = Longitude + .0001, y = Latitude - .001, label = SiteName)) +
  scale_fill_manual(values = c('SMR' = "green", 'FMR' = "deepskyblue2", 'SMCA' = "blue2", 'FMCA' = "red2")) +
  scale_color_manual(values = c('SMR' = "green", 'FMR' = "deepskyblue2", 
                                'SMCA' = "blue2", 'FMCA' = "red2",
                                'Inside' = "green", 'Outside' = "red2")) +
  labs(title = "South Point SMR at Santa Rosa Island",
       x = "Longitude",
       y = "Latitude") +
  theme_classic() +
  theme(legend.position = "none",
        legend.title = element_text(),
        plot.title = element_text(hjust = 0.5, size = 28, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 16),
        axis.title = element_text(size = 28, face = "bold"),
        axis.text.y = element_text(size = 20, face = "bold"),
        axis.text.x = element_text(size = 20, face = "bold"))

dev.off()
}

{ # Scorpion SMR   ----
  ScorpionSMR <- get_googlemap(c(-119.5669, 34.05428),
                                 zoom = 13,
                                 maptype = "satellite",
                                 style = c(labels = "off"))
  
  png(filename = "SCI.png",
      width = 1000, height = 1000, type = "cairo")
  
  ggmap(ScorpionSMR, extent = "device") +
    geom_sf(data = marine, aes(fill = Type, color = Type),
            inherit.aes = FALSE, alpha = .1) +
    geom_sf_label(data = marine, inherit.aes = FALSE, hjust = .5, vjust = .5, size = 6,
                  aes(label = FULLNAME)) +
    geom_sf(data = transects, aes(color = Site_Code),
            inherit.aes = FALSE, alpha = .6, color = "lightblue", size = 1) +
    geom_sf(data = NPS_boundary, inherit.aes = FALSE, color = "green", size = 1, alpha = 0) +
    geom_point(data = siteInfo,
               aes(x = Longitude, y = Latitude, color = ReserveStatus),
               size = 2, inherit.aes = FALSE) +
    geom_label(data = siteInfo, hjust = .1, vjust = 0, size = 6,
               aes(x = Longitude + .001, y = Latitude + .001, label = SiteName)) +
    scale_fill_manual(values = c('SMR' = "green", 'FMR' = "deepskyblue2", 'SMCA' = "blue2", 'FMCA' = "red2")) +
    scale_color_manual(values = c('SMR' = "green", 'FMR' = "deepskyblue2", 
                                  'SMCA' = "blue2", 'FMCA' = "red2",
                                  'Inside' = "green", 'Outside' = "red2")) +
    labs(title = "Scorpion SMR at Santa Cruz Island",
         x = "Longitude",
         y = "Latitude") +
    theme_classic() +
    theme(legend.position = "none",
          legend.title = element_text(),
          plot.title = element_text(hjust = 0.5, size = 28, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5, size = 16),
          axis.title = element_text(size = 28, face = "bold"),
          axis.text.y = element_text(size = 20, face = "bold"),
          axis.text.x = element_text(size = 20, face = "bold"))
  
  dev.off()
}

{ # Anacapa Island SMR   ----
  AnacapaSMR <- get_googlemap(c(-119.40, 34.01260),
                               zoom = 13,
                               maptype = "satellite",
                               style = c(labels = "off"))
  
  png(filename = "ANI.png",
      width = 1000, height = 1000, type = "cairo")
  
  ggmap(AnacapaSMR, extent = "device") +
    geom_sf(data = marine, aes(fill = Type, color = Type),
            inherit.aes = FALSE, alpha = .1) +
    geom_sf_label(data = marine, inherit.aes = FALSE, hjust = .5, vjust = .5, size = 6,
                  aes(label = FULLNAME)) +
    geom_sf(data = transects, aes(color = Site_Code),
            inherit.aes = FALSE, alpha = .6, color = "lightblue", size = 1) +
    geom_sf(data = NPS_boundary, inherit.aes = FALSE, color = "green", size = 1, alpha = 0) +
    geom_point(data = siteInfo,
               aes(x = Longitude, y = Latitude, color = ReserveStatus),
               size = 2, inherit.aes = FALSE) +
    geom_label(data = siteInfo, hjust = 1, vjust = 0, size = 4.5,
               aes(x = Longitude + .001, y = Latitude + .0009, label = SiteName)) +
    scale_fill_manual(values = c('SMR' = "green", 'FMR' = "deepskyblue2", 'SMCA' = "blue2", 'FMCA' = "red2")) +
    scale_color_manual(values = c('SMR' = "green", 'FMR' = "deepskyblue2", 
                                  'SMCA' = "blue2", 'FMCA' = "red2",
                                  'Inside' = "green", 'Outside' = "red2")) +
    labs(title = "Anacapa Island SMR",
         x = "Longitude",
         y = "Latitude") +
    theme_classic() +
    theme(legend.position = "none",
          legend.title = element_text(),
          plot.title = element_text(hjust = 0.5, size = 28, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5, size = 16),
          axis.title = element_text(size = 28, face = "bold"),
          axis.text.y = element_text(size = 20, face = "bold"),
          axis.text.x = element_text(size = 20, face = "bold"))
  
  dev.off()
}

{ # Santa Barbara Island SMR   ----
  SantaBarbaraSMR <- get_googlemap(c(-119.0392, 33.475),
                              zoom = 14,
                              maptype = "satellite",
                              style = c(labels = "off"))
  
  png(filename = "SBI.png",
      width = 1000, height = 1000, type = "cairo")
  
  ggmap(SantaBarbaraSMR, extent = "device") +
    geom_sf(data = marine, aes(fill = Type, color = Type),
            inherit.aes = FALSE, alpha = .1) +
    geom_sf_label(data = marine, inherit.aes = FALSE, hjust = .5, vjust = .5, size = 6,
                  aes(label = FULLNAME)) +
    geom_sf(data = transects, aes(color = Site_Code),
            inherit.aes = FALSE, alpha = .6, color = "lightblue", size = 1) +
    geom_sf(data = NPS_boundary, inherit.aes = FALSE, color = "green", size = 1, alpha = 0) +
    geom_point(data = siteInfo,
               aes(x = Longitude, y = Latitude, color = ReserveStatus),
               size = 2, inherit.aes = FALSE) +
    geom_label(data = siteInfo, hjust = 0, vjust = 1, size = 6,
               aes(x = Longitude + .001, y = Latitude + .0001, label = SiteName)) +
    scale_fill_manual(values = c('SMR' = "green", 'FMR' = "deepskyblue2", 'SMCA' = "blue2", 'FMCA' = "red2")) +
    scale_color_manual(values = c('SMR' = "green", 'FMR' = "deepskyblue2", 
                                  'SMCA' = "blue2", 'FMCA' = "red2",
                                  'Inside' = "green", 'Outside' = "red2")) +
    labs(title = "Santa Barbara Island SMR",
         x = "Longitude",
         y = "Latitude") +
    theme_classic() +
    theme(legend.position = "none",
          legend.title = element_text(),
          plot.title = element_text(hjust = 0.5, size = 28, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5, size = 16),
          axis.title = element_text(size = 28, face = "bold"),
          axis.text.y = element_text(size = 20, face = "bold"),
          axis.text.x = element_text(size = 20, face = "bold"))
  
  dev.off()
}
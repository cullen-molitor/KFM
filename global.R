



{ # Library    ----
  
  library(shiny) 
  library(lubridate)
  library(glue)
  # library(plyr)
  library(ggridges)
  library(tidyverse)
  # library(grid)
  # library(gridExtra)
  # library(png)
  # library(rmarkdown)
  library(shinydashboard)
  library(shinyWidgets)
  # library(plotrix)
  library(splitstackshape)
  # library(zoo)
  # library(reshape)
  library(RColorBrewer)
  library(measurements)
  library(sf)
  # library(rsconnect)
  library(leaflet)
  library(DT)
  library(ggnewscale)
  library(cowplot)
  library(ncdf4)
  library(rnoaa)
}

{ # .. SiteInfo_DF   ----
  siteInfo1 <- read_csv("Site_info.csv")
  siteInfo2 <- read_csv("Site_info2.csv")
  
  IslandLevels <- c("San Miguel Island", "Santa Rosa Island", "Santa Cruz Island", "Anacapa Island",  "Santa Barbara Island")
  MPA_Levels <- c("Santa Rosa Island", "Santa Cruz Island", "Anacapa Island",  "Santa Barbara Island")
  MPA_Site_levels <- siteInfo2 %>%
    filter(Reference == TRUE) %>%
    arrange(ReserveStatus, IslandName)
  Survey_Levels <- c("One_Meter", "Five_Meter", "Bands", "NHSF", "RPC", "FSF", "RDFC")
  
  ARMlist <- siteInfo2 %>%
    filter(ARMs == TRUE)
  BATHlist <- siteInfo2 %>%
    filter(Bath == TRUE)
  
  SiteNames <- as.character(siteInfo1$SiteName)
  names(SiteNames) <- glue("{siteInfo1$SiteNumber} - {siteInfo1$SiteName}")
  
  SiteColor <- as.character(siteInfo1$Color)
  names(SiteColor) <- siteInfo1$SiteName
  
  SiteColor2 <- as.character(siteInfo1$Color2)
  names(SiteColor2) <- siteInfo1$SiteName
  
  SiteLine <- c(as.character(siteInfo1$LineType), "dashed", "dashed", "dashed", "dashed", "dashed")
  names(SiteLine) <- c(siteInfo1$SiteName, IslandLevels)
  
  visitDates <- read_csv("VisitDates.csv")
  visitDates$SurveyType <- factor(visitDates$SurveyType, levels = Survey_Levels)
}

{ # .. Species_Info    ----
  
  SpeciesName <- read_csv("SpeciesComplete.csv")
  
  SpeciesFish <- read_csv("Species_Fish.csv")
  
  Indicators <- SpeciesName %>%
    drop_na(Species) 
  Indicators$Species <- as.character(Indicators$Species)
  fish <- Indicators %>%
    filter(Classification == "Fish")
  
  oneM_species <- read_csv('oneM_Species.csv')
  fiveM_species <- read_csv("fiveM_Species.csv")
  bands_species <- read_csv("bands_Species.csv")
  all_species <- read_csv("core_Species.csv")
  core_species <- as.character(all_species$CommonName)
  names(core_species) <- glue("{all_species$Protocol} - {all_species$CommonName}")
  rpcs_species <- read_csv("rpcs_Species.csv")
  
  
  SpeciesColor <- c(as.character(SpeciesName$Color), "blue3", "forestgreen", "gold", "orangered", "red3")
  names(SpeciesColor) <- c(SpeciesName$CommonName, IslandLevels)
  
  FishColor <- SpeciesFish$Color
  names(FishColor) <- SpeciesFish$CommonName
  
}


{ # .. 1m_DF  ----
  
  oneM_DF <- read_csv("oneM_Summary.csv")
  oneM_Levels <- unique(oneM_species$CommonName)
  oneM_DF$IslandName <- factor(oneM_DF$IslandName, levels = IslandLevels)
  oneM_DF$SiteName <- factor(oneM_DF$SiteName, levels = unique(siteInfo2$SiteName))
  oneM_DF$CommonName <- factor(oneM_DF$CommonName, levels = oneM_Levels)
  
  oneM_DFMPA <- read_csv("oneM_MPA.csv")
  oneM_DFMPA$IslandName <- factor(oneM_DFMPA$IslandName, levels = MPA_Levels)
  oneM_DFMPA$SiteName <- factor(oneM_DFMPA$SiteName, levels = as.character(MPA_Site_levels$SiteName))
  oneM_DFMPA$CommonName <- factor(oneM_DFMPA$CommonName, levels = oneM_Levels)
  
  oneM_DFRaw <- read_csv("oneM_RAW.csv")
  oneM_DFRaw$IslandName <- factor(oneM_DFRaw$IslandName, levels = IslandLevels)
  oneM_DFRaw$SiteName <- factor(oneM_DFRaw$SiteName, levels = unique(siteInfo2$SiteName))
  oneM_DFRaw$CommonName <- factor(oneM_DFRaw$CommonName, levels = oneM_Levels)
  

} 

{ # .. 5m_DF  ----
  
  fiveM_DF <- read_csv("fiveM_Summary.csv", col_types = cols(
    QuadratsSampled = col_double(),
    IslandQuads = col_double()))
  fiveM_Levels <- unique(fiveM_species$CommonName)
  fiveM_DF$IslandName <- factor(fiveM_DF$IslandName, levels = IslandLevels)
  fiveM_DF$SiteName <- factor(fiveM_DF$SiteName, levels = unique(siteInfo2$SiteName))
  fiveM_DF$CommonName <- factor(fiveM_DF$CommonName, levels = fiveM_Levels)

  fiveM_DFMPA <- read_csv("fiveM_MPA.csv")
  fiveM_DFMPA$IslandName <- factor(fiveM_DFMPA$IslandName, levels = MPA_Levels)
  fiveM_DFMPA$SiteName <- factor(fiveM_DFMPA$SiteName, levels = as.character(MPA_Site_levels$SiteName))
  fiveM_DFMPA$CommonName <- factor(fiveM_DFMPA$CommonName, levels = fiveM_Levels)

  fiveM_DFRaw <- read_csv("fiveM_Raw.csv")
  fiveM_DFRaw$IslandName <- factor(fiveM_DFRaw$IslandName, levels = IslandLevels)
  fiveM_DFRaw$SiteName <- factor(fiveM_DFRaw$SiteName, levels = unique(siteInfo2$SiteName))
  fiveM_DFRaw$CommonName <- factor(fiveM_DFRaw$CommonName, levels = fiveM_Levels)
  
}

{ # .. Bands_DF   ----
  
  bands_DF <- read_csv("bands_Summary.csv")
  bands_Levels <- unique(bands_species$CommonName)
  bands_DF$IslandName <- factor(bands_DF$IslandName, levels = IslandLevels)
  bands_DF$SiteName <- factor(bands_DF$SiteName, levels = unique(siteInfo2$SiteName))
  bands_DF$CommonName <- factor(bands_DF$CommonName, levels = bands_Levels)
  
  bands_DFMPA <- read_csv("bands_MPA.csv")
  bands_DFMPA$IslandName <- factor(bands_DFMPA$IslandName, levels = MPA_Levels)
  bands_DFMPA$SiteName <- factor(bands_DFMPA$SiteName, levels = as.character(MPA_Site_levels$SiteName))
  bands_DFMPA$CommonName <- factor(bands_DFMPA$CommonName, levels = bands_Levels)
  
  bands_DFRaw <- read_csv("bands_RAW.csv")
  bands_DFRaw$IslandName <- factor(bands_DFRaw$IslandName, levels = IslandLevels)
  bands_DFRaw$SiteName <- factor(bands_DFRaw$SiteName, levels = unique(siteInfo2$SiteName))
  bands_DFRaw$CommonName <- factor(bands_DFRaw$CommonName, levels = bands_Levels)
  
}

{ # .. Core_DF   ----
  
  # core_DF <- rbind(oneM_DF, fiveM_DF, bands_DF)
  
  core_DF <- read_csv("core_Summary.csv")
  core_Levels <- unique(all_species$CommonName)
  core_DF$IslandName <- factor(core_DF$IslandName, levels = IslandLevels)
  core_DF$SiteName <- factor(core_DF$SiteName, levels = unique(siteInfo2$SiteName))
  core_DF$CommonName <- factor(core_DF$CommonName, levels = core_Levels)
  
  core_DFMPA <- read_csv("core_MPA.csv") %>% 
    filter(SiteCode != "KH") 
}

{ # .. NHSF_DF   ----
  
  NHSF_DF <- read_csv("NHSF_Summary.csv")
  NHSF_DF$IslandName <- factor(NHSF_DF$IslandName, levels = IslandLevels)
  
  NHSF_DFRaw<- read_csv("NHSF_Raw.csv") 
  NHSF_DFRaw$IslandName <- factor(NHSF_DFRaw$IslandName, levels = IslandLevels)
  
  NHSF_DFMPA <- read_csv("NHSF_MPA.csv")
  NHSF_DFMPA$IslandName <- factor(NHSF_DFMPA$IslandName, levels = MPA_Levels)
  
  NHSF_DFRawMPA <- read_csv("NHSF_MPA_Raw.csv") 
  NHSF_DFRawMPA$IslandName <- factor(NHSF_DFRawMPA$IslandName, levels = MPA_Levels)
  
} 

{ # .. RPC_DF   ----
  
  rpcs_DF <- read_csv("rpcs_Summary.csv")
  rpcs_Levels <- unique(rpcs_species$CommonName)
  rpcs_DF$IslandName <- factor(rpcs_DF$IslandName, levels = IslandLevels)
  rpcs_DF$SiteName <- factor(rpcs_DF$SiteName, levels = unique(siteInfo2$SiteName))
  rpcs_DF$CommonName <- factor(rpcs_DF$CommonName, levels = rpcs_Levels)
  
  rpcs_DFMPA <- read_csv("rpcs_MPA.csv")
  rpcs_DFMPA$IslandName <- factor(rpcs_DFMPA$IslandName, levels = MPA_Levels)
  rpcs_DFMPA$SiteName <- factor(rpcs_DFMPA$SiteName, levels = as.character(MPA_Site_levels$SiteName))
  rpcs_DFMPA$CommonName <- factor(rpcs_DFMPA$CommonName, levels = rpcs_Levels)

  # rpcs_DFRaw <- read_csv("rpcs_RAW.csv")
  # rpcs_DFRaw$IslandName <- factor(rpcs_DFRaw$IslandName, levels = IslandLevels)
  # rpcs_DFRaw$SiteName <- factor(rpcs_DFRaw$SiteName, levels = unique(siteInfo2$SiteName))
  # rpcs_DFRaw$CommonName <- factor(rpcs_DFRaw$CommonName, levels = rpcs_Levels)
  
}

{ # .. FSF_DF   ----
  
  FSF_DF <- read_csv("FSF_Summary.csv") 
  FSF_DF$IslandName <- factor(FSF_DF$IslandName, levels = IslandLevels)
  
  FSF_DFMPA <- read_csv("FSF_MPA.csv")
  FSF_DFMPA$IslandName <- factor(FSF_DFMPA$IslandName, levels = MPA_Levels)
  
  FSF_DFRaw <- read_csv("FSF_Raw.csv")
  FSF_DFRaw$IslandName <- factor(FSF_DFRaw$IslandName, levels = IslandLevels)
  
  FSF_DFRawMPA <- read_csv("FSF_MPA_Raw.csv")
  FSF_DFRawMPA$IslandName <- factor(FSF_DFRawMPA$IslandName, levels = MPA_Levels)
}

{ # .. RDFC_DF   ----

  RDFC_DF <- read_csv("RDFC_Summary.csv")
  RDFC_DF$IslandName <- factor(RDFC_DF$IslandName, levels = IslandLevels)

}

{ # .. Temperature_DF   ----
  
  temp <- read_csv("Temp_weekly_summary.csv")
  
  tempByIsland <- read_csv("Temp_weekly_summary_byIsl.csv")
  # %>% 
  #   mutate(Date = as.Date(Date, format='%m/%d/%Y')) %>% 
  #   write_csv("Temp_weekly_summary_byIsl.csv")
  
  oni <- read_csv("nino34.csv")
  
  # oni <- read.table("https://origin.cpc.ncep.noaa.gov/products/analysis_monitoring/ensostuff/detrend.nino34.ascii.txt", header = T) %>%
  #   mutate(Date = as.Date(ISOdate(oni$YR, oni$MON, 1))) 
  # oni$DateStart <- as.Date(ISOdate(oni$YR, oni$MON, 1))
  # oni$DateEnd <- ceiling_date(oni$DateStart, "month")
  # write_csv(oni, path = "nino34.csv")
  
  pdo_uw <- read_csv("PDO_UW.csv") 
  pdo_noaa <- read_csv("PDO_NOAA.csv")
  
  # PDO <- read.table("https://www.ncdc.noaa.gov/teleconnections/pdo/data.csv",
  #                   skip = 1, header = T, sep = ",") %>% 
  #   separate(Date, c('Year','Month'), sep = 4) %>% 
  #   mutate(Date = as.Date(ISOdate(PDO$Year, PDO$Month, 1))) %>% 
  #   dplyr::rename(pdoAnom = Value)
  # PDO$DateStart <- as.Date(ISOdate(PDO$Year, PDO$Month, 1))
  # PDO$DateEnd <- ceiling_date(PDO$DateStart, "month") 
  # write_csv(PDO, path = "PDO_NOAA.csv")
  
  scrippsTemp <- read_csv("SIO_Temp_Weekly.csv")
  
  scrippsSalt <- read_csv("SIO_Salt_Weekly.csv")
}

{ # .. Maps_DF   ----
  
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
    filter(SiteNumber != as.numeric(1) &
             SiteNumber != 5 &
             SiteNumber != 11) 
  
  transects <- st_read("KFM_Transects_SmoothLine5.shp")  %>%
    st_as_sf()%>%
    mutate(geometry = st_transform(geometry, "+proj=longlat +ellps=WGS84 +datum=WGS84"))
  
  NPS_boundary <- st_read("nps_boundary.shp") %>%
    st_as_sf()
  
  CINMS_boundary <- st_read("cinms_py.shp") %>%
    st_as_sf()

}

{ # .. Protocol_DF ----
  Protocol_PDFs <- read_csv("Protocols.csv")
}

{ # .. TAGS  ----
  
  {  # ONI Tags   ----
    ONI_tagList <- tagList(   
      tags$h4("The Oceanic Nino Index (ONI) is NOAAs primary metric for monitoring the El Niño-Southern Oscillation (ENSO).
              ONI is based on the running 3-month average of sea surface temperatures captured in the Niño region 3.4 (+/- 5° 
              latitude of the equator and between 120°-170° W longitude). The values are compared to a 30-year average for the relative time 
              periods. ENSO has three phases: La Niña, neutral, and El Niño. The color gradient you see in the background to this
              data represents those three phases. La Niña corresponds to blue colors to represent colder than usual 
              sea surface temperatures. The Neutral phase is represented in white (bleeds into light blue or light red). 
              El Niño corresponds to red colors to represent warmer than usual sea surface temperatures. 
              Darker colors correspond to more extreme anomalies.", 
              tags$br(), tags$br(),
              "La Niña is a 3-month running mean less than - 0.5° (c) below the relative 30-year average for a period of 5 consecutive months",
              tags$br(), 
              "Neutral is a 3-month running mean greater than - 0.5° (c) and less than 0.5° (c) the relative 30-year average for any length of time", 
              tags$br(), 
              "El Niño is a 3-month running mean greater than 0.5° (c) above the relative 30-year average for a period of 5 consecutive months",
              tags$br(), tags$br(), 
              "ENSO effects global weather patterns and is closely correlated to the water temperatures at the Channel Islands,
              despite the distance from where ENSO data is collected. This gradient is meant to illuminate trends in species density 
              by allowing for a visual relationship to water temperatures. For instance, as you move north and west among the parks 
              5 islands, water temperatures decrease and the species that make up the kelp forest community change. We therefore 
              conclude that certain species are 'warm water' species while others may be considered 'cold water' species among 
              the islands. In the years following an El Niño event, one might expect to see certain 'warm water' species recruit to 
              'cold water' islands.",
              tags$br(), tags$br(), 
              "For a video describing ENSO please visit:",
              tags$br(),
              tags$a(href="https://www.youtube.com/watch?v=KpZsoHru9go&list=PLDT7TfWhSREdvJXrqsDJG1fVKmapQ1QPH&index=2", 
                     "ENSO Video", target = "_blank")),
      tags$h4("For more detailed information on ONI please visit:",
              tags$br(),
              tags$a(href="https://www.climate.gov/news-features/understanding-climate/climate-variability-oceanic-ni%C3%B1o-index", 
                     "Climate.Gov", target = "_blank")))
  }  
  
  {  # PDO NOAA Tags   -----
    PDO_NOAA_tagList <- tagList( 
      tags$h4("The Pacific Decadal Oscillation (PDO) is a lesser known metric than the El Niño-Southern Oscillation (ENSO) but acts in a similar way.
              The PDO is a longer-lived pattern of Pacific climate variability than ENSO. The extremes of PDO represent periods of 
              either warm or cool sea surface temperatures. When sea surface temperatures are cool in the interior of the North 
              Pacific, warm along the Pacific Coast, and sea level atmospheric pressure is lower than average over the North Pacific
              then the PDO has a positive value. Conversely when sea surface temperatures are warm in the interior of the North Pacific, 
              cool along the Pacific Coast, and sea level atmospheric pressure is higher than average over the North Pacific then the 
              PDO has a negative value. In this plot, positive PDO values are represented in red (for warm coastal waters), while 
              negative PDO values correspond to blue colors (for cool coastal waters). Darker colors correspond to more extreme anomalies.", 
              tags$br(), tags$br(),
              "For a video describing the PDO please visit:",
              tags$br(),
              tags$a(href="https://www.youtube.com/watch?v=Sc3tOEcM0YE", "PDO Video", target = "_blank")),
      tags$h4("For more detailed information on the PDO and the associated data from NOAA please visit:",
              tags$br(),
              tags$a(href="https://www.ncdc.noaa.gov/teleconnections/pdo/", "NOAA.gov", target = "_blank")))
  }
  
  {  # PDO UW Tags  -----
    PDO_UW_tagList <- tagList( 
      tags$h4("The Pacific Decadal Oscillation (PDO) is a lesser known metric than the El Niño-Southern Oscillation (ENSO) but acts in a similar way.
              The PDO is a longer-lived pattern of Pacific climate variability than ENSO. The extremes of PDO represent periods of 
              either warm or cool sea surface temperatures. When sea surface temperatures are cool in the interior of the North 
              Pacific, warm along the Pacific Coast, and sea level atmospheric pressure is lower than average over the North Pacific,
              then the PDO has a positive value. Conversely when sea surface temperatures are warm in the interior of the North Pacific, 
              cool along the Pacific Coast, and sea level atmospheric pressure is higher than average over the North Pacific, then the 
              PDO has a negative value. In this plot, positive PDO values are represented in red (for warm coastal waters), while 
              negative PDO values correspond to blue colors (for cool coastal waters). Darker colors correspond to more extreme anomalies.", 
              tags$br(), tags$br(),
              "For a video describing the PDO please visit:",
              tags$br(),
              tags$a(href="https://www.youtube.com/watch?v=Sc3tOEcM0YE", "PDO Video", target = "_blank")),
      tags$h4("For more detailed information on the PDO and the associated data from University of Washington (UW) please visit:",
              tags$br(),
              tags$a(href="http://research.jisao.washington.edu/pdo/", "washington.edu", target = "_blank")))
  } 
  
  {  # MPA Tags  ------
    MPA_tagList <- tagList( 
      tags$h4("California’s Marine Life Protection Act (MLPA) established a network of Marine Protected Areas (MPAs) 
                along the California coastline and around California’s islands. This network includes State Marine 
                Reserves (SMRs), State Marine Conservation Areas (SMCAs), and State Marine Parks (SMPs). The waters of 
                Channel Islands National Park (CINP) have 11 MPAs, including 9 SMRs and 2 SMCAs.", 
              tags$br(), tags$br(),
              "In 2005, CINP was awarded funding from the NPS Natural Resources Preservation Program (NRPP) to 
                establish baseline ecological conditions of newly established MPAs in CINP. This project began with 
                the establishment of 16 new permanent monitoring sites. These sites were placed inside or adjacent 
                to the following four newly established SMRs: Santa Barbara Island SMR, Anacapa Island SMR, Scorpion 
                Anchorage SMR at Santa Cruz Island, and South Point SMR at Santa Rosa Island. New sites were established
                to complement existing sites so that three sites were inside and three were adjacent to each of
                the four chosen SMRs. This means that 24 sites are considered reference sites with 6 sites at each of the
                four chosen SMRs.",
              tags$br(), tags$br(),
              "When looking at the graphs plotted by the MPA means, the data being plotted is filtered to only include 
                the 24 SMR reference sites. When the data is broken out by site means, the data being plotted is similarly 
                filtered except when viewing line plots or smooth line plots which will also include data from Keyhole on 
                Anacapa Island. Keyhole lies within the Anacapa Island SMCA which allows for the take of CA spiny lobster 
                and pelagic finfish. Keyhole will appear as a dotted line rather than solid or dashed.",
              tags$br(), tags$br(),
              "To view all sites that as inside an MPA vs outside an MPA please visit the 'One Species by Island' option.
                Please note that different sites were made into MPAs at different times and currently those graphs only 
                reflect their current MPA status. Use caution as some sites have been inside MPAs since KFM's inception, while 
                others (most that are in MPAs today) were designated in 2003. In the future it is intended to make this more clear
                under that option.",
              tags$br(), tags$br(),
              "For a video describing California’s MPA network please visit:",
              tags$br(), tags$br(),
              tags$a(href="https://www.youtube.com/watch?v=xB_yqcfN7DE", "MPA Video", target = "_blank"),
              tags$br(), tags$br(),
              "For more detailed information on California's MPA network from California's Department of Fish and Wildlife (CDFW) please visit:",
              tags$br(), tags$br(),
              tags$a(href="https://wildlife.ca.gov/Conservation/Marine/MPAs", "wildlife.ca.gov", target = "_blank")))
  } 
  
  {  # NPS Tags   ------ 
    NPS_tagList <- tagList( 
      tags$h4("The above pdf is a small section from the KFM Handbook. This handbook is a living document that changes as needed to best
                reflect the history, methods, and data management for the program.",
              tags$br(), tags$br(),
              "To view a lecture on the importance of long-term monitoring at the Channel Islands please visit:",
              tags$br(),
              tags$a(href="https://www.youtube.com/watch?v=86foiSxQmVU", "KFM Lecture Part 1", target = "_blank"),
              tags$br(),
              tags$a(href="https://www.youtube.com/watch?v=pv9N2xDu0y8", "KFM Lecture Part 2", target = "_blank"),
              
              tags$br(), tags$br(),
              "To view the blog posts from the Our World Underwater Scholarship Society's (OWUSS) NPS Interns please visit:",
              tags$br(),
              tags$a(href="http://blog.owuscholarship.org/2011/11/channel-islands-national-park/", 
                     "2011 Blog Post", target = "_blank"),
              tags$br(),
              tags$a(href="http://blog.owuscholarship.org/2012/09/channel-islands-national-park-a-paradise-hidden-in-plain-sight/", 
                     "2012 Blog Post", target = "_blank"),
              tags$br(),
              tags$a(href="http://blog.owuscholarship.org/2013/08/kelp-forest-monitoring-in-the-channel-islands/", 
                     "2013 Blog Post", target = "_blank"),
              tags$br(),
              tags$a(href="http://blog.owuscholarship.org/2014/08/exploring-the-mysteries-of-the-channel-islands/", 
                     "2014  Blog Post", target = "_blank"),
              tags$br(),
              tags$a(href="http://blog.owuscholarship.org/2015/07/cruising-monitoring-at-the-channel-islands/", 
                     "2015 Blog Post", target = "_blank"),
              tags$br(),
              tags$a(href="http://blog.owuscholarship.org/2016/10/ventura-channel-islands-national-park/", 
                     "2016 Blog Post", target = "_blank"),
              tags$br(),
              tags$a(href="https://blog.owuscholarship.org/2018/02/a-kelp-forest-homecoming-at-channel-islands-national-park/", 
                     "2017 Blog Post", target = "_blank"),
              tags$br(),
              tags$a(href="http://blog.owuscholarship.org/2018/06/channel-islands-national-parks-exploring-the-majestic-kelp-forests/", 
                     "2018 Blog Post", target = "_blank"),
              tags$br(),
              tags$a(href="http://blog.owuscholarship.org/2019/10/exploring-submerged-forests-at-channel-islands-national-park/", 
                     "2019 Blog Post", target = "_blank")
      ),
      tags$h4("For more detailed information on KFM or to download the handbook or the annual reports please visit:",
              tags$br(),
              tags$a(href = "https://www.nps.gov/im/medn/kelp-forest-communities.htm", "NPS.gov", taget = "_blank")),
      tags$hr())
  }
  
  { # NPS Tags   ------
    NPSreports_tagList <- tagList(  
      tags$h4("The latest annual report to have been published was the 2012 report. They are currently behind but expect new 
            reports to be published soon.",
              tags$br(),
              "To view a lecture on the importance of long-term monitoring at the Channel Islands please visit:",
              tags$br(),
              tags$a(href="https://www.youtube.com/watch?v=86foiSxQmVU", "KFM Lecture Part 1", target = "_blank"),
              tags$br(),
              tags$a(href="https://www.youtube.com/watch?v=pv9N2xDu0y8", "KFM Lecture Part 2", target = "_blank"),
              
              tags$br(), tags$br(),
              "To view the blog posts from the Our World Underwater Scholarship Society's (OWUSS) NPS Interns please visit:",
              tags$br(),
              tags$a(href="http://blog.owuscholarship.org/2011/11/channel-islands-national-park/", 
                     "2011 Blog Post", target = "_blank"),
              tags$br(),
              tags$a(href="http://blog.owuscholarship.org/2012/09/channel-islands-national-park-a-paradise-hidden-in-plain-sight/", 
                     "2012 Blog Post", target = "_blank"),
              tags$br(),
              tags$a(href="http://blog.owuscholarship.org/2013/08/kelp-forest-monitoring-in-the-channel-islands/", 
                     "2013 Blog Post", target = "_blank"),
              tags$br(),
              tags$a(href="http://blog.owuscholarship.org/2014/08/exploring-the-mysteries-of-the-channel-islands/", 
                     "2014  Blog Post", target = "_blank"),
              tags$br(),
              tags$a(href="http://blog.owuscholarship.org/2015/07/cruising-monitoring-at-the-channel-islands/", 
                     "2015 Blog Post", target = "_blank"),
              tags$br(),
              tags$a(href="http://blog.owuscholarship.org/2016/10/ventura-channel-islands-national-park/", 
                     "2016 Blog Post", target = "_blank"),
              tags$br(),
              tags$a(href="https://blog.owuscholarship.org/2018/02/a-kelp-forest-homecoming-at-channel-islands-national-park/", 
                     "2017 Blog Post", target = "_blank"),
              tags$br(),
              tags$a(href="http://blog.owuscholarship.org/2018/06/channel-islands-national-parks-exploring-the-majestic-kelp-forests/", 
                     "2018 Blog Post", target = "_blank"),
              tags$br(),
              tags$a(href="http://blog.owuscholarship.org/2019/10/exploring-submerged-forests-at-channel-islands-national-park/", 
                     "2019 Blog Post", target = "_blank")
      ),
      tags$h4("For more detailed information on KFM or to download the the annual reports please visit:",
              tags$br(),
              tags$a(href = "https://www.nps.gov/im/medn/kelp-forest-communities.htm", "NPS.gov", taget = "_blank")),
      tags$hr())
  }
}

# { # .. Buoys  ----
#   
  { # Buoy Stations ----
    Buoys_List <- read_csv("Buoy_Stations.csv")
  }
#   
#   { # 46053 EAST SANTA BARBARA  ----
#     Buoy_46053_DF <- read_csv(
#       "Buoy_46053.csv",
#       col_types = cols(
#         Date = col_date(format = ""),
#         Time = col_time(format = ""),
#         lat = col_double(),
#         lon = col_double(),
#         wind_dir = col_double(),
#         wind_spd = col_double(),
#         gust = col_double(),
#         wave_height = col_double(),
#         dominant_wpd = col_double(),
#         average_wpd = col_double(),
#         mean_wave_dir = col_double(),
#         air_pressure = col_double(),
#         air_temperature = col_double(),
#         sea_surface_temperature = col_double(),
#         dewpt_temperature = col_double(),
#         visibility = col_double(),
#         water_level = col_double()
#       )) 
#     # Buoy_46053_DF <- Buoy_46053_DF[Buoy_46053_DF$Date %in% visitDates$Date, ]
#     # write_csv(Buoy_46053_DF, "Buoy_46053.csv")
#     
#   }
#   
#   { # 46054 WEST SANTA BARBARA  ----
#     Buoy_46054_DF <- read_csv(
#       "Buoy_46054.csv",
#       col_types = cols(
#         Date = col_date(format = ""),
#         Time = col_time(format = ""),
#         lat = col_double(),
#         lon = col_double(),
#         wind_dir = col_double(),
#         wind_spd = col_double(),
#         gust = col_double(),
#         wave_height = col_double(),
#         dominant_wpd = col_double(),
#         average_wpd = col_double(),
#         mean_wave_dir = col_double(),
#         air_pressure = col_double(),
#         air_temperature = col_double(),
#         sea_surface_temperature = col_double(),
#         dewpt_temperature = col_double(),
#         visibility = col_double(),
#         water_level = col_double()
#       ))
#     # Buoy_46054_DF <- Buoy_46054_DF[Buoy_46054_DF$Date %in% visitDates$Date, ]
#     # write_csv(Buoy_46054_DF, "Buoy_46054.csv")
#   }
#   
#   { # 46218 (Platform) Harvest SIO 071 ----
#     Buoy_46218_DF <- read_csv(
#       "Buoy_46218.csv",
#       col_types = cols(
#         Date = col_date(format = ""),
#         Time = col_time(format = ""),
#         lat = col_double(),
#         lon = col_double(),
#         wind_dir = col_double(),
#         wind_spd = col_double(),
#         gust = col_double(),
#         wave_height = col_double(),
#         dominant_wpd = col_double(),
#         average_wpd = col_double(),
#         mean_wave_dir = col_double(),
#         air_pressure = col_double(),
#         air_temperature = col_double(),
#         sea_surface_temperature = col_double(),
#         dewpt_temperature = col_double(),
#         visibility = col_double(),
#         water_level = col_double()
#       ))
#     # Buoy_46218_DF <- Buoy_46218_DF[Buoy_46218_DF$Date %in% visitDates$Date, ]
#     # write_csv(Buoy_46218_DF, "Buoy_46218.csv")
#   }
#   
#   { # 46251 Santa Cruz Basin SIO 203   ----
#     Buoy_46251_DF <- read_csv(
#       "Buoy_46251.csv",
#       col_types = cols(
#         Date = col_date(format = ""),
#         Time = col_time(format = ""),
#         lat = col_double(),
#         lon = col_double(),
#         wind_dir = col_double(),
#         wind_spd = col_double(),
#         gust = col_double(),
#         wave_height = col_double(),
#         dominant_wpd = col_double(),
#         average_wpd = col_double(),
#         mean_wave_dir = col_double(),
#         air_pressure = col_double(),
#         air_temperature = col_double(),
#         sea_surface_temperature = col_double(),
#         dewpt_temperature = col_double(),
#         visibility = col_double(),
#         water_level = col_double()
#       ))
#     # Buoy_46251_DF <- Buoy_46251_DF[Buoy_46251_DF$Date %in% visitDates$Date, ]
#     # write_csv(Buoy_46251_DF, "Buoy_46251.csv")
#   }
#  
#   
# }




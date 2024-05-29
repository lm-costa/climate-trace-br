library(tidyverse)
library(ggsci)
library(geobr)
source("R/gafico.R")
source("R/my-function.R")


emissions_sources <- read_rds("data/emissions_sources.rds") %>%
  mutate(source_name_1 = str_to_title(source_name)) %>%
  filter(year >=2015, year<=2023)

emissions_sources$year %>% unique()
#> [1] 2015 2016 2017 2018 2019 2020 2021 2022 2023
states <- read_rds("data/states.rds") %>%
  mutate(name_region = ifelse(name_region == "Centro Oeste","Centro-Oeste",name_region))


dd <- emissions_sources %>%
  filter(
    year == 2022,
    gas == "co2e_100yr",
    sector_name == "transportation",
    !source_name %in% sigla_uf,
    !sub_sector %in% c("forest-land-clearing",
                       "forest-land-degradation",
                       "shrubgrass-fires",
                       "forest-land-fires",
                       "wetland-fires",
                       "removals")
  ) %>%
  group_by(source_id,source_name, sub_sector) %>%
  summarise(
    emission = sum(emissions_quantity, na.rm=TRUE)
  ) %>%
  arrange(emission %>% desc()) %>%
  ungroup() %>%
  mutate(Acumulada = cumsum(emission));dd


states |>
  filter(name_state=="São Paulo") |>
  ggplot()+
  geom_sf(data=conservation,
          color=conservation$code_conservation_unit,fill=NA)+
  geom_sf(fill=NA, color="black",
          size=.15, show.legend = T)+
  lims(x=c(-53,-44.5),y=c(-25,-19.5))+
  geom_point(data =emissions_sources %>%
               filter(flag_conservation,
                      sigla_uf=="SP",
                      year==2022) %>%
               group_by(lat,lon) %>%
               summarise(
                 emission = sum(emissions_quantity,na.rm = T)/1e6
               ) |>
               mutate(
                 emission_class=case_when(
                   emission < 0 ~ "sink",
                   emission < 0 ~ "< 0",
                   emission < 1 ~ "< 1",
                   emission > 1 ~ "> 1"
                 )
               ),
             aes(x=lon,y=lat,color=emission_class))+
  #scale_color_viridis_d()+
  labs(color='Emission (M ton C)')+
  tema_mapa()

emissions_sources %>%
  filter(flag_conservation,
         sigla_uf=="SP",
         year==2022) %>%
  group_by(lat,lon) %>%
  summarise(
    emission = sum(emissions_quantity,na.rm = T)/1e6
  ) |>
  mutate(
    emission_class=case_when(
      emission < -1 ~ "< -1",
      emission < 0 ~ "< 0",
      emission < 1 ~ "< 1",
      emission > 1 ~ "> 1"
    )
  ) |>
  ungroup() |>
  group_by(emission_class) |>
  summarise(e_count = n())
emissions_sources %>%
  filter(flag_conservation,
         sigla_uf=="SP",
         year==2022) %>%
  group_by(lat,lon) %>%
  summarise(
    emission = sum(emissions_quantity,na.rm = T)/1e6
  ) |>
  mutate(
    emission_class=case_when(
      emission < 0 ~ "sink",
      .default = "source",
    )
  ) |>
  ungroup() |>
  group_by(emission_class) |>
  summarise(e_count = n())


library(treemapify)
emissions_sources %>%
  filter(flag_conservation,
         sigla_uf=="SP",
         year==2022,
         gas=='co2e_100yr') %>%
  filter(sector_name!='forestry_and_land_use')%>%
  group_by(sector_name) %>%
  summarise(
    emission = sum(emissions_quantity,na.rm = T)/1e6
  )%>%
  arrange(emission)  %>%
  ungroup() |>
  mutate(emisison_p = emission/sum(emission)*100) |>
  ggplot(aes(area = emisison_p, fill = sector_name)) +
  geom_treemap() +
  geom_treemap_text(
    aes(label = paste(sector_name,
                      paste0(round(emisison_p, 2), "%"), sep = "\n")),
    colour = "white") +
  theme(legend.position = "none")


emissions_sources %>%
  filter(flag_conservation,
         sigla_uf=="SP",
         year==2022,
         gas=='co2e_100yr') %>%
  #filter(sector_name!='forestry_and_land_use')%>%
  group_by(sector_name) %>%
  summarise(
    emission = sum(emissions_quantity,na.rm = T)/1e6
  )%>%
  ggplot()+
  geom_col(aes(x=sector_name,y=emission))+
  labs(x='Setor',y=expression('Emissão (M ton CO'[2]~'eq.)'))+
  theme(axis.text.x = element_text(angle = 45,hjust = 1))


emissions_sources %>%
  filter(flag_conservation,
         sigla_uf=="SP",
         year==2022,
         gas=='co2e_100yr') %>%
  #filter(sector_name!='forestry_and_land_use')%>%
  #group_by(sector_name) %>%
  summarise(
    emission = sum(emissions_quantity,na.rm = T)/1e6
  )


<!-- README.md is generated from README.Rmd. Please edit that file -->

# climate-trace-br

## Aquisição dos dados

![](img/img-01.png) ![](img/img-02.png) ![](img/img-03.png)

![](img/img-04.png) \## Carregando Pacotes

``` r
library(tidyverse)
library(geobr)
source("R/gafico.R")
source("R/my-function.R")
```

## Mesclando base com ids

### Carregando as bases de dados

``` r
brazil_ids <- read_rds("data/df_nome.rds")
emissions_sources <- read_rds("data/emissions_sources.rds")
```

### Lendo o polígono dos estados

``` r
states  %>%
  ggplot() +
  geom_sf(fill="white", color="black",
          size=.15, show.legend = FALSE) +
  geom_point(
    data = emissions_sources %>%
      filter(sigla_uf == "MT",
             #year == 2020
             ),
    aes(lon,lat)) +
  tema_mapa()
```

![](README_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
emissions_sources %>%
      filter(source_name == "Rio Claro") %>% 
  pull(sigla_uf) %>% unique()
#> [1] "Other" "SP"
```

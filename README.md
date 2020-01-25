DC Census Stats
================

## DC Census Stats

Choropleth Maps of DC created from the Census API

## Overview

This repository contains all the code used to generate the maps for
[\#GetCountedDC](https://twitter.com/hashtag/GetCountedDC) at
@[gupta\_hersh](https://twitter.com/gupta_hersh).

Each R script in the
[Code/](https://github.com/hguptadc/DC-Census-Stats/tree/master/Code/)
directory contains the code needed to generate each map.

## The Map-making process:

1.  Specify Census variables from the [API
    documentation](https://api.census.gov/data/2016/acs/acs5/variables.html)
2.  Get DC ward shapefiles using the
    [`tigris`](https://github.com/walkerke/tigris) R package
3.  Get Census data using the
    [`tidycensus`](https://walkerke.github.io/tidycensus/) R package
    which interfaces with the Census API
4.  Merge shapefile data from `tigris` and Census data from `tidycensus`
5.  Plot using ggplot +
    [hrbrthemes](https://github.com/hrbrmstr/hrbrthemes) R package

## Citations

This project wouldn’t have been possible without the following packages:

[`tidycensus`](https://cran.r-project.org/web/packages/tidycensus/)  
[`tigris`](https://cran.r-project.org/web/packages/tigris/)  
[`ggplot`](https://cran.r-project.org/web/packages/ggplot2/index.html)  
[`sf`](https://cran.r-project.org/web/packages/sf/)  
[`sp`](https://cran.r-project.org/web/packages/sp/)  
[`viridis`](https://cran.r-project.org/web/packages/viridis/)  
[`hrbrthemes`](https://cran.r-project.org/web/packages/hrbrthemes/)

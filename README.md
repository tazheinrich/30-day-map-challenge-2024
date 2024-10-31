# 30 Day Map Challenge 2024

This repository documents my contributions to the [https://30daymapchallenge.com/](#30DayMapChallenge 2024).

## Prerequisites for Reproducibility

All plots are done using the statistical programming language [https://cran.r-progject.org](R) in combination with the plotting package [https://ggplot2.tidyverse.org/](ggplot2). The respository is designed with reproducibility in mind and further prerequistes for seamless reproduction of the plots will be added soon to this README.

## 01 Points: Meteorite Landings in Germany

For the first challenge, I plot meteorite landings as points on a map of Germany. The data for the meteorite landings is from [The Meteoritical Society](The Meteoritical Society), accessible at [https://data.nasa.gov/Space-Science/Meteorite-Landings/gh4g-9sfh/about_data](NASA's Open Data Portal). Only those observations are used, which contain information about mass and location of the landing. The shapefile for Germany is retrieved from [https://ec.europa.eu/eurostat/web/gisco](Eurostat - GISCO) by the [https://ropengov.github.io/giscoR/](giscoR-package).

<img src="output/01-points.png" widht="300px"/>
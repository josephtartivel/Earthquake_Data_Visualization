# Earthquake Data Visualization

Interactive R Shiny dashboard for the Global Earthquakes 2023 dataset
(~17k events). Five linked views: regional trends, depth / magnitude
characteristics, temporal frequency, geographic distribution, and seismic
network coverage.

## What it answers

1. How do key earthquake characteristics change across regions?
2. Are there temporal patterns in earthquake frequency and type?
3. What is the geographic distribution of magnitude and depth?
4. How does seismic-station coverage affect detection accuracy?

## Stack

R (Shiny, leaflet for maps, plotly for charts). Single-file app in
`Earthquake_Visualization_Project/app.R`.

## Run it

```r
install.packages(c("shiny", "leaflet", "plotly"))
shiny::runApp("Earthquake_Visualization_Project/app.R")
```

## Data

Source: [Global Earthquakes 2023 on Kaggle](https://www.kaggle.com/datasets/mustafakeser4/earthquakes-2023-global)
— time, lat/lon, depth, magnitude, magnitude type, station coverage.

## Layout

```
Earthquake_Visualization_Project/
  app.R              full Shiny app (UI + server)
  earthquakes.csv    Global Earthquakes 2023 dump
  README.md          original notes
Earthquake_Visualization_Report.pdf   write-up
```

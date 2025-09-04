# Earthquake Analysis

This project presents an interactive Shiny application designed to analyze and visualize global earthquake data from 2023. It helps seismologists and geophysicists uncover patterns in seismic activity through geographic clustering and temporal analysis.

## Features
- **Global Earthquake Trends**: Analyze earthquake frequency, magnitude, and regional trends.
- **Characteristics Analysis**: Explore changes in earthquake depth and magnitude by continent and over time.
- **Frequency Trends**: Identify temporal patterns in earthquake occurrence and intensity.
- **Geographic Distribution**: Visualize the spatial patterns of earthquake magnitude and depth.
- **Seismic Network Coverage & Errors**: Investigate the impact of seismic station coverage on earthquake detection and reporting errors.

## Dataset
The project uses the **Global Earthquakes 2023** dataset from Kaggle.
[Access Dataset](https://www.kaggle.com/datasets/mustafakeser4/earthquakes-2023-global)

The dataset provides valuable information about earthquakes worldwide in the year 2023. This dataset includes various parameters such as time, location (latitude and longitude), depth, magnitude, magnitude type, and more. It can be used to gain insights into the seismic activity observed throughout the year.

## Core Research Questions
1. How do key earthquake characteristics change across different regions?
2. Are there temporal patterns in the types of earthquakes in terms of frequency?
3. What is the geographic distribution of key earthquake characteristics?
4. How does seismic station coverage affect earthquake detection accuracy?

## Installation
1. Clone the repository:
   ```sh
   git clone https://github.com/your-repo-name.git
   ```
2. Install required R packages:
   ```r
   install.packages(c("shiny", "leaflet", "plotly"))
   ```
3. Run the Shiny app:
   ```r
   shiny::runApp("app.R")
   ```



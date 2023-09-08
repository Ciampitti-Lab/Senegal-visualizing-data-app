# Senegal Data Visualization

## Table of Contents
1. [Objective](#objective)
2. [Important](#important)
3. [Description](#description)
    1. [Structure](#structure)
    2. [Libraries](#libraries)
    3. [CSS](#css)
    4. [Data embed](#data-embed)
    5. [Video](#video)
    6. [Visualization](#visualization)
    7. [Data](#data)
    8. [Data Source](#data-source)
    9. [About](#about)
4. [Future Works](#future-works)
5. [Acknowledgements](#acknowledgements)

## Objective
Taking into account the need of a digital tool to provide open access to harmonized and scaled data of different [Sustainable Intensification Assessment Framework (SIAF)](https://sitoolkit.com/assessment-framework/the-five-domains-of-sustainable-intensificatio) domains, this app was created with the objective of:
1. Generate an interactive dashboard so policy-makers and researchers can visualize the data and connect the domains;
2. Provide open access of gathered data of SIAF from Senegal;
3. Provide the sources, methodology of manipulation, temporal and spatial resolution and year of acquisition of data used;

---

## Important

While running this app in an IDE, some warnings will appear. However this warnings are related to the empiness of the dropdown menu and do not affect the app oppening or performance.
The instructions video also will not be available while running in the IDE, but if the user access the [website](https://ciampittilab.shinyapps.io/SIAF), the video can be visualized.

<br>

## Description

The framework chosen to build the app was Shiny using R computational language due to the facilitation to create and use an interactive and free-to-use dashboard.

The design was made in Canva and can be seen on file Senegal App.pdf

<br>

### Structure

The app were made using all into one Rmd file due to facilitating the visualization of the components inside it and the usage of **flexdashboard** library. The CSS code were made inside the Rmd file. There are 3 "son" folders containing files used in the app:
- Data: containing all the csv files used;
- www: all the images and;
- SHAPEFILES: shapefiles for Senegal districts.

The app is divided into 5 sections:
- Instructions, containing a video teaching how to use the app;
- Visualization, where the main part of the app is;
- Data, where a table with all the data that is used in the app is;
- Data source, containing all the source and methodology used to get to manipulate the data, respectively;
- About, with a brief description of SIAF and the Digital Tools, Geospatial and Farming Systems Consortium.

<br>

### Libraries
22 libraries were used in the app:

| Purpose | Libraries | 
| ----------- | ----------- |
| Shiny framework usage | shiny; flexdashboard; shinydashboard; shinythemes; shinyWidgets |
| Map usage | leaflet; shapefiles; sf|
| Data manipulation | dplyr; scales; psych; reshape2; tidyverse; fmsb|
| Graph usage | ggplot2; ggradar; ggcorrplot; plotly |
| App style | htmltools; RColorBrewer; yarrr|
| Table usage | DT; openxlsx|

<br>

### CSS
As said before, the CSS was embedded in the app. Into it are:
- Styles for the bootstrap widgets for Shiny;
- Adaptations for usage in small screens;
- Font and colors as designed before;
- Shiny error messages hiding;

<br>

### Data embed
Data is embedded into the app (not having connection with a DBS). Inside a chunck, there are variables saving shapefiles, xlsx and csv files. The shapefiles are used to render the map used in Map tab, the xlsx file is used in the Data Source tab and the csv files, for data manipulation to display in the map and charts.

<br>

### Video
Due to the complexity of the app (lot of graphs and different analysis and possibilities to integrate data), an instructions video were recorded. It is hosted on YouTube and is in the first page of the app. There are subtitles in English for better comprehension. It demonstrates what each other tab is and how they can be used.

<br>

### Visualization
The visualization tab is the main part of this app. It is divided into 6 other tabs that dynamically change the content of the sideboard when selected. In every one of then, the data will be filtered according to the user selection (Districts, Years and Variables)

| Tab | Description |
| ----------- | ----------- |
| Map | Presents a choropleth map of Senegal made with leaflet. Users select the variable, the year interval and if they want the sum or the mean of selected period. Users can hover with the mouse in the map to visualize the District name, the variable, the amount and the unit. |
| SIAF | This tab was designed to generate the real integration of different domains of SIAF. It generates a radar chart by user's selection of first, if they want to compare between districts or between years in the same district and then, of District(s), Years and Variables|
| Correlation Matrix | A correlation Matrix of variables using years of repetitions. Can be done of one district or the whole country |
| Line Chart | Generates a line chart that can i) display temporal analysis of a variable, comparing different Districts, or ii) display temporal analysis of different variables in one District |
| Bar chart | Presents the possibility to display the mean or sum of variables (alone or stacked) in selected years, and compare it with different Districts |
| Box chart | Displays a box chart using years as repetitions to compare a variable between Districts |

<br>

### Data
This tab provides a view of the whole data used in the app. There is a button on the top, where users can download it as a csv file.

<br>

### Data Source
In this tab, a table shows the where the data were collected from and the sources of the data manipulation. As the previous tab, this one also has a button on the top, providing the possibility to download it as a csv file.

<br>

### About
A brief text wrote in HTML inside the Rmd provides a brief explanation of SIAF and the Consortium and also the links for the respective websites.

---
## Future Works

Future works are aims to keep the data updated. Another challenge is to add more countries in the dataset. Getting more countries improve the impact of the app into the assessment and planning of interventions in these countries, since it can be used to integrate data of SIAF domains. These task can be solved with two options:

- Keep inserting the csv data in the app and uploading in the server or;
- Create a DBS for the app and use a library to retrieve and manipulate the data.

In order to change the countries displayed in the app, a select widget can be inserted somewhere (future work for the designer) to change the retrieving data.

---
## Acknowledgements
The app was made by Gustavo N. Santiago, with feedbacks from Ana J. P. Carcedo (both from Ciampitti Lab at Kansas State University) and the Consortium members during an in-person meeting at Kansas on September 2022.

This study was supported by the Feed the Future Innovation Lab for Collaborative Research on Sustainable Intensification (SIIL) at Kansas State University through funding United States Agency for International Development (USAID) under the Cooperative Agreement (Grant number AIDOAA-L-14–00006).





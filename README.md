
------------------------------------------------------------------------

This repository is a **summary** of the project called "Urban soil sampling for climate change mitigation: prioritizing urban greenspaces by management and urban heat analysis". This README is based on the project [UHI-detector](https://github.com/isatyamks/UHI-Detector) by Satyam Kumar. From start to finish, detailing all aspects including the **workflow**, **technologies**, **data**, and **expected outcomes**:

------------------------------------------------------------------------

## **Project Title**

**Urban soil sampling for climate change mitigation: prioritizing urban greenspaces by management and urban heat analysis**

------------------------------------------------------------------------

## **Project Overview**

Urban heat islands (UHI) raise air and soil temperatures in cities, despite soil’s high heat capacity. Elevated soil temperatures affect processes like microbial activity and evapotranspiration, especially in poorly managed greenspaces. However, reliable soil temperature data are scarce and rarely monitored in urban areas. As a result, studies often rely on land surface temperature models to estimate spatial variability and assess heat effects across different types of urban greenspaces. This project aims to build reproducible workflow to:

1.  Compare high-resolution urban heat maps (SantiagoHOT) with conventional satellite sources (Sentinel and Landsat) to assess their usefulness for monitoring urban soil thermal patterns.

2.  Identify priority zones for urban soil sampling to support climate mitigation, based on thermal variability, greenspace type, and management conditions.

------------------------------------------------------------------------

## **Objectives**

1.  Describe high-resolution urban heat maps (SantiagoHOT).\
2.  Download satellite data (Sentinel and Landstat) at same conditions to SantiagoHOT.\
3.  Search a framework for comparison of the three sources (StgoHOT/Sentinel/Landstat).\
4.  Describe temperatures (StgoHOT) across urban greenspaces.\
5.  Develop visualization dashboard based on StgoHOT and project results

------------------------------------------------------------------------

## **Satellite Context: NDVI Santiago 2024**

This visualization provides a baseline of vegetation vigor across urban Santiago during the 2024 summer peak, serving as a reference for green space health and distribution.

![Urban Santiago NDVI Map](./plots/map_ndvi_santiago.png)

- **Data Source**: Landsat 8 & 9 (Collection 2 Level 2)
- **Period**: January 2024
- **Scale**: 30m resolution; values from 0.2 (Urban/Soil) to 0.8 (Dense Canopy).

------------------------------------------------------------------------

## **Software and data sources**

### **Data Sources**

-   **Google Earth Engine (GEE)**: For satellite imagery (Landsat and Sentinel).\
-   **Agrometeorologia INIA**: For weather data.\
-   **StgoHOT**: Rasters at three moments of a summer day in 2024.\

### **Programming and data analysis rameworks**

-   **R**: Primary language for data processing.\
-   Main libraries:
    -   **Tidyverse**: Data processing.\
    -   **Terra**: Geospatial data processing.\

------------------------------------------------------------------------

## **Workflow**

### **1. Data Collection**

-   **Satellite Data**: Use GEE to access Landsat/Sentinel:
    -   Land Surface Temperature (LST).\
    -   NDVI (Normalized Difference Vegetation Index).\
-   **Weather Data**: Collect temperature, humidity, and wind speed data.\

------------------------------------------------------------------------

### **2. Data Preprocessing**

-   **Step 1**: Ensure all datasets align to the same coordinate reference system (CRS).\
-   **Data Cleaning**: Remove noisy or missing data.\

------------------------------------------------------------------------

### **3. Data Analysis**

-   **Comparison**: Develop a data framework for comparing the three approaches for UHI (StgoHOT, Sentinel and Landsat).\
-   **StgoHOT temperatu**: Develop a data framework for comparing the three approaches for UHI (StgoHOT, Sentinel and Landsat).\
-   Output: Urban greenspaces under local UHI.\

------------------------------------------------------------------------

### **4. Comparison bewteen Stgo HOT/Sentinel/Landstat**

-   **Comparison**: Develop a data framework for comparing the three approaches for UHI (StgoHOT, Sentinel and Landsat).\
-   **Discussion**: Describe potential under or subestimation of UHI based on StgoHOT, Sentinel and Landsat.\

------------------------------------------------------------------------

### **5. Visualization**

-   **Current UHI Map**:
    -   Interactive maps showing temperature using **Leaflet.js**.\
-   **Histogramas and temperature profile of urban greenspaces**:
    -   Heatmaps of urban greenspaces at different time.\


------------------------------------------------------------------------

### **6. Dashboard Development**

1. **Interactive Maps**: Show StgoHOT temperatures with zoom and pan functionality.\
2. **Insights Panel**: Summarize key findings (e.g., average temperature increase, top hotspots).\
3. **Export Options**: Enable users to download results in CSV or GeoJSON format.

------------------------------------------------------------------------

## **Challenges and Solutions**

### **1. Satellite Data Size**

-   **Challenge**: Satellite imagery can be large and computationally expensive to process.\
-   **Solution**: Use GEE for cloud-based processing and only download the results.

------------------------------------------------------------------------

## **Expected Results**

1.  **Result 1**: result 1.\
2.  **Result 2**: result 2.\

------------------------------------------------------------------------

## **Workflow for UGS Structural Characterization**

This specific workflow targets the differentiation of green space structures to prioritize soil sampling sites.

* **Woody Composition**: Strictly defined as the sum of **`trees` + `shrub_and_scrub`** probability bands from Dynamic World.
* **Woody Ratio**: A structural metric calculated as $$Woody\_Ratio = \frac{Woody}{Woody + Grass}$$ to determine dominant management type.
* **Prioritization Values**:
    * **Urban Forest (Cool)**: Top 10 sites with a ratio **> 0.75**.
    * **Open Grass (Hot)**: Bottom 10 sites representing the **relative minimums** of the dataset to ensure maximum contrast for soil analysis.

### **Spatial Patterns**
Regional hexagonal binning maps visualize management hotspots across the urban fabric:

#### **Woody Density (Trees + Shrubs)**
![Woody Hotspots](plots/04_Hex_Woody.png)
*Map showing areas with high probability of woody biomass.*

#### **Herbaceous Density (Grass)**
![Grass Hotspots](plots/04_Hex_Grass.png)
*Map showing areas with high probability of managed grass cover.*

------------------------------------------------------------------------
## **Impact** 

-   **Urban Planners**: Design cities with better cooling strategies.\
-   **Governments**: Prioritize climate adaptation projects.\
-   **Citizens**: Increase awareness of how urbanization impacts local climates.

------------------------------------------------------------------------

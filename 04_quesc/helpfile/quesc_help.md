# Overview

The Quantification of Environmental Services for Carbon (QUES-C) module calculate the amount of green-house gas emission from land use/cover change using stock difference approach. The stock difference approach estimates GHG emissions or removals from land-use change by calculating the change in carbon stocks over time. This involves comparing the carbon stored in different land-use types at two points in time, using Activity Data that quantifies the extent of land-use change, and Emission Factors that represent the carbon stock changes associated with each land-use transition. By multiplying the Activity Data with the corresponding Emission Factors, we can determine the net GHG emissions or removals resulting from land-use change within a given area and time period.

# Key Concepts:

1.  **Carbon stock dynamics analysis** is carried out for carbon stock changes in an area during one period, using *Stock Difference* method. The carbon stock being considered is time-averaged above ground carbon stock.
2.  **Emissions** are calculated as the decrease of carbon stock amount due to land cover change if the amount of the initial carbon stock is higher than that upon land use change.
3.  **Sequestration** is calculated as the amount of carbon stock addition due to land cover change (meaning that the carbon stock in the initial land use is lower than that upon land use change)

# Data Preparation

Before using the QUES-C Analysis tool, prepare the following:

1.  **Land Cover Maps**: Two raster files (GeoTIFF format) representing land use/cover for two different time points.
2.  **Planning Unit Map**: A raster file (GeoTIFF format) or shapefile of administrative or management zones.
3.  **Carbon Stock Lookup Table**: A CSV file describing emission factors with their corresponding land cover classes.

## Using the QUES-C Analysis Tool

Follow these steps to use the tool:

![](quesc.gif){style="max-width:80%;height=auto;"}

1.  **Land Use/Cover T1**
    -   Click "Browse" and select your land cover raster file for the first time point (T1).
2.  **T1 Year**
    -   Enter the year corresponding to the T1 land cover data.
3.  **Land Use/Cover T2**
    -   Click "Browse" and select your land cover raster file for the second time point (T2).
4.  **T2 Year**
    -   Enter the year corresponding to the T2 land cover data.
5.  **Carbon Stock Lookup Table**
    -   Click "Browse" and select your prepared CSV file containing the emission factors with their corresponding land cover classes.
6.  **Planning Units**
    -   Click "Browse" and select your planning unit shapefile.
7.  **Output Directory**
    -   Click "Select output directory" to choose where results will be saved.
8.  **Run Analysis**
    -   Click "Run" to start the process.

# Accessing Outputs

After the analysis completes, you'll find the following in your selected output directory:

1.  **Carbon Map**: A raster map in GeoTIFF format illustrating the distribution and amount of carbon stored in land use/cover data.
2.  **Emission Map**: A raster map in GeoTIFF format derived from a decrease in carbon stock amount from land use/cover T1 to land use/cover T2.
3.  **Sequestration Map**: A raster map in GeoTIFF format derived from an increase in carbon stock amount from land use/cover T1 to land use/cover T2.
4.  **QUES-C Database**: A CSV file containing land cover transition between T1 and T2 in each planning unit with the value of carbon stock, emission, and sequestration.
5.  **QUES-C Report**: An overview of the analysis results in HTML format, providing detailed interpretations and visualizations of these outputs.

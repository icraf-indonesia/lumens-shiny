# Data Preparation

Before using the SCIENDO Train tool, prepare the following:

1.  **Land Cover Maps**: Two raster files (GeoTIFF format) representing land use/cover for two different time points.
2.  **Planning Unit Map**: A raster file (GeoTIFF format) or shapefile of administrative or management zones.
3.  **Factors Map**: A set of raster files describing driver factors or explanatory variables.
7.  **DINAMICA EGO Path (Optional)**: A custom path to the DINAMICA EGO directory installed on your PC if DINAMICA EGO is installed in a custom installation directory. The default path will be used if not specified, assuming the default DINAMICA EGO installation directory.

> <font size="2">Before running the SCIENDO Train module, ensure that you have downloaded and installed the DINAMICA EGO software. DINAMICA EGO is a free and non-commercial platform for environmental modeling with outstanding possibilities for the design. You can download the latest version 8 [**here**](https://csr.ufmg.br/dinamica/dinamica-8/) but it still compatible with older version. For further description, please visit the [DINAMICA EGO Documentation](https://dinamicaego.com/dokuwiki/doku.php?id=guidebook_start).</font>

## Using the SCIENDO Train Analysis Tool

![](sciendo_train_help.gif){style="max-width:80%;height=auto;"}

Follow these steps to use the tool:

1.  **Land Use/Cover T1**
    -   Click "Browse" and select your land cover raster file for the first time point (T1).
2.  **T1 Year**
    -   Enter the year corresponding to the T1 land cover data.
3.  **Land Use/Cover T2**
    -   Click "Browse" and select your land cover raster file for the second time point (T2).
4.  **T2 Year**
    -   Enter the year corresponding to the T2 land cover data.
5.  **Planning Units**
    -   Click "Browse" and select your planning unit shapefile or raster.
6.  **Factor(s) Folder**
    -   Click "Factor(s) Folder Path" to choose where factor maps folder is prepared.
7.  **Output Directory**
    -   Click "Select output directory" to choose where results will be saved.
8.  **Run Analysis**
    -   Click "Run" to start the process.

# Accessing Outputs

After the analysis completes, you'll find the following in your selected output directory:

1.  **Directory of WoE**: This directory provides a collection of coeffient weights in DCF and CSV files, organized by planning unit.
2.  **SCIENDO factor**: A ERS file containing the images and metadata of factor maps used in ER Mapper Raster.
3.  **EGOML file**: A file format containing the model of DINAMICA EGO.
4.  **SCIENDO Train Report**: An overview of the analysis results in HTML format, providing detailed interpretations and visualizations of these outputs.


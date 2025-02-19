# Data Preparation

Before using the SCIENDO Simulate tool, prepare the following:

1.  **Land Cover Map at T1**: A raster files (GeoTIFF format) representing land use/cover at T1.
2.  **Planning Unit Map**: A raster file (GeoTIFF format) or shapefile of administrative or management zones.
3.  **Land Use/Cover Lookup Table**: A CSV file describing raster values with their corresponding land cover classes.
4.  **Raster Cube**: A raster files (GeoTIFF format) or ERS files containing the factor maps produced by SCIENDO Train.
5.  **Transition Matrices**: A list of CSV file containing transition probability matrix of land cover between T1 and T2 in each planning unit produced by SCIENDO Scenario Builder.
6.  **Weight of Evidence**: A list of DCF file containing weight of evidence of factor maps produced by SCIENDO Train.
7.  **DINAMICA EGO Path (Optional)**: A custom path to the DINAMICA EGO directory installed on your PC if DINAMICA EGO is installed in a custom installation directory. The default path will be used if not specified, assuming the default DINAMICA EGO installation directory.

> <font size="2">Before running the SCIENDO Simulate  module, ensure that you have downloaded and installed the DINAMICA EGO software. Dinamica EGO is a free and non-commercial platform for environmental modeling with outstanding possibilities for the design. You can download the version 6 or lower [**here**](https://csr.ufmg.br/dinamica/dinamica-6/) but it still compatible with older version. For further description, please visit the [DINAMICA EGO Documentation](https://dinamicaego.com/dokuwiki/doku.php?id=guidebook_start).</font>

## Using the SCIENDO Simulate Analysis Tool

Follow these steps to use the tool:

<video controls style="max-width:80%;height:auto;">
    <source src="sciendo-sim.mp4" type="video/mp4">
    Your browser does not support the video tag.
</video>

1.  **Land Use/Cover T1**
    -   Click "Browse" and select your land cover raster file for the first time point (T1).
2.  **Planning Units**
    -   Click "Browse" and select your planning unit shapefile.
3.  **Land Use/Cover Lookup Table**
    -   Click "Browse" and select your prepared CSV file containing land cover classes.
4.  **Raster Cube**
    -   Click "Browse" and select sciendo_factors.tif or two related ERS files.
5.  **Repetition**
    -   Enter a number of repetition to specify the number of projection maps to generate.
6.  **Transition Matrix Folder**
    -   Click "Transition Matrix Folder Path" to choose where TPM output folder is saved or obtained from SCIENDO Scenario Builder.
7.  **Weight of Evidence Folder**
    -   Click "Weight of Evidence Path" to choose where DCF output folder is saved or obtained from SCIENDO Train.
8.  **Output Directory**
    -   Click "Select output directory" to choose where results will be saved.
9.  **Run Analysis**
    -   Click "Run Analysis" to start the process.

# Accessing Outputs

After the analysis completes, you'll find the following in your selected output directory:

1.  **Land use/cover projection maps**: List of TIF file containing land use/cover projection.
2.  **Land use/cover projection lookup table**: A CSV file containing the all of area projected land use/cover.
3.  **EGOML file**: A file format containing the model of DINAMICA EGO.
4.  **SCIENDO Simulate Report**: An overview of the analysis results in HTML format, providing detailed interpretations and visualizations of these outputs.


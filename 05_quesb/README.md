# QuES-B
An R implementation of QuES-B (Quantification of Ecosystem Services - Biodiversity) analysis, now featuring a user-friendly Shiny interface.


![Screenshot 2024-08-30 183450](https://github.com/user-attachments/assets/3a52c2de-74d5-4728-b71b-4385472bef8c)


## Repository Structure
- `rscript/`: Directory containing R scripts and functions.
  - `ques_biodiv_functions.R`: Core QuES-B analysis functions.
- `report_template/`: Directory for storing templates of reports and analysis results.
  - `quesb_report_template.Rmd`: R Markdown template for generating QuES-B analysis reports.
- `helpfile/`: Directory for storing helpfiles.
  - `quesb_quick_user_guide.Rmd`: A quick user guide written in Rmarkdown.   

## Getting Started

### Prerequisites
- R (version 4.0.0 or higher is recommended)
- RStudio (optional, but recommended for easy project management)

### Installation
1. Clone this repository to your local machine:
   ```
   git clone https://github.com/icraf-indonesia/lumens-shiny.git
   ```
   Or download the ZIP file and unzip it to your desired location.
2. Open RStudio.
3. In RStudio, click on `File > Open Project`, then navigate to the cloned/unzipped folder and select the `ques_b.Rproj` file.

## Running the Analysis

### Using the Shiny UI (Recommended)
1. In RStudio, create a new R script and paste the following code:
   ```r
   # Load QuES-B analysis functions
   source("05_quesb/rscript/ques_biodiv_functions.R")          
   # Launch the QuES-B Shiny application
   quesb_app()
   ```
2. Run the script. This will check for required packages, install any missing ones, and launch the Shiny app.
3. Use the Shiny interface to upload your data files, set parameters, and run the analysis.

## Usage
The QuES-B tool allows for comprehensive biodiversity analysis at the landscape scale. To use the tool:

1. Prepare your input data files:
   - Land cover map (GeoTIFF format)
   - Land Use/Cover & Focal Area Lookup Table (CSV)
   - Edge Contrast Table (FSQ)
   - FRAGSTATS Configuration file (optional)

2. Set the analysis parameters:
   - Year of the land cover data
   - No Data Land Cover Class (if present)
   - Sampling Points
   - Window Size
   - Window Shape

3. Run the analysis using the "Run QuES-B Analysis" button.

4. Once the analysis is complete, use the "Open Report" button to view the generated report.

Follow the instructions within the Shiny UI to understand the workflow and customize it for your specific needs.

## Key Concepts
- **Focal Area**: Habitats of taxa of interest or areas with high biodiversity.
- **TECI Map**: Illustrates the spatial distribution of focal area fragmentation and integration across the landscape.
- **DIFA Index**: Quantifies how well integrated a focal area is within a landscape, indicating its capacity to conserve biodiversity.

## Contributing
Contributions to improve QuES-B are welcome. Please feel free to submit pull requests or open issues to discuss potential enhancements.

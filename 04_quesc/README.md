# QUES-C

Shiny version of LUMENS QUES-C Module

## Repository Structure

- `rscript/`: Directory containing R scripts and functions.
  - `functions_ques_c.R`: Core QUES-C analysis functions.
  - `app.R`: A single Shiny execution file.
- `report_template/`: Directory for storing templates of reports and analysis results.

## Getting Started

### Prerequisites

- R (version 4.0.0 or higher is recommended)
- RStudio (optional, recommended for easy project management)

### Installation

1. Clone this repository to your local machine:
   ```
   git clone https://github.com/icraf-indonesia/lumens-shiny.git
   ```
   Or download the ZIP file and unzip it to your desired location.

2. Open RStudio.

3. In RStudio, click on `File > Open File`, then navigate to the cloned/unzipped folder and select and open the `04_quesc/rscript/global.ui` file.

## Running the Analysis

### Using the Shiny UI (Recommended)

1. In RStudio, just run the script or type this to the console:

   ```r
   setwd('04_quesc/rscript')
   shiny::shinyApp()
   ```

2. Use the Shiny interface to upload your data files, set parameters, and run the analysis.

## Usage

The QUES-C tool allows for land use/land cover change analysis along with carbone, emission, and sequestration estimation. Whether using the Shiny UI or the R script:

1. Prepare your input data files (land use/cover maps, lookup tables, etc.).
2. Set the analysis parameters (time points, output directory, etc.).
3. Run the analysis.
4. Review the generated reports and visualizations in the output directory.

Follow the comments and instructions within the `functions_ques_c.R` script or the Shiny UI to understand the workflow and customize it for your specific needs.

## Contributing

Contributions to improve QUES-C are welcome. Please feel free to submit pull requests or open issues to discuss potential enhancements.
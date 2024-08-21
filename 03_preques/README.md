# Pre-QuES

A minimal implementation of Pre-QuES land use/land cover analysis, now with a Shiny user interface.

![image](https://github.com/user-attachments/assets/9e3777ca-7baf-4f26-9c30-bb589f982425)


## Repository Structure

- `rscript/`: Directory containing R scripts and functions.
  - `functions_ques_pre.R`: Core Pre-QuES analysis functions.
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

3. In RStudio, click on `File > Open Project`, then navigate to the cloned/unzipped folder and select the `pre_ques.Rproj` file.

## Running the Analysis

### Using the Shiny UI (Recommended)

1. In RStudio, create a new R script and paste the following code:

   ```r
   # Load Pre-QuES analysis functions
   source("03_preques/rscript/functions_ques_pre.R")          
   # Launch the Pre-QuES Shiny application
   preques_app()
   ```

2. Run the script. This will check for required packages, install any missing ones, and launch the Shiny app.

3. Use the Shiny interface to upload your data files, set parameters, and run the analysis.

## Usage

The Pre-QuES tool allows for land use/land cover change analysis. Whether using the Shiny UI or the R script:

1. Prepare your input data files (land use/cover maps, lookup tables, etc.).
2. Set the analysis parameters (time points, output directory, etc.).
3. Run the analysis.
4. Review the generated reports and visualizations in the output directory.

Follow the comments and instructions within the `functions_ques_pre.R` script or the Shiny UI to understand the workflow and customize it for your specific needs.

## Contributing

Contributions to improve Pre-QuES are welcome. Please feel free to submit pull requests or open issues to discuss potential enhancements.

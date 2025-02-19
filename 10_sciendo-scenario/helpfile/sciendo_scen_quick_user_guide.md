# Data Preparation

Before using the QUES-C Analysis tool, prepare the following:

1.  **QUES-C Database**: A CSV file containing land cover transition between T1 and T2 in each planning unit with the value of carbon stock, emission, and sequestration.

## Using the SCIENDO Scenario Builder Tool

Follow these steps to use the tool:

<video controls style="max-width:80%;height:auto;">
    <source src="sciendo-builder.mp4" type="video/mp4">
    Your browser does not support the video tag.
</video>

1.  **QUES-C Database**
    -   Click "Browse" and select QUES-C Database CSV file containing the emission factors with their corresponding land cover classes.
2.  **Output Directory**
    -   Click "Select output directory" to choose where results will be saved.
3.  **Run Analysis**
    -   Click "Run Analysis" to start the process.

# Accessing Outputs

After the analysis completes, you'll find the following in your selected output directory:

1.  **Directory of TPM per Scenario**: This directory provides a collection of transition probability matrices, organized by planning unit and modified scenario. Each matrix quantifies the probabilities of land cover change between time periods T1 and T2 within that specific unit and scenario.
2.  **Baseline TPM**: A CSV file containing baseline of transition probability matrix of land cover between T1 and T2 in each planning unit.
3.  **Scenario Builder Report**: An overview of the analysis results in HTML format, providing detailed interpretations and visualizations of these outputs.

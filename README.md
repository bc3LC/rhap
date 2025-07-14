# rhap

[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![build](https://github.com/bc3LC/rhap/actions/workflows/build.yaml/badge.svg)](https://github.com/bc3LC/rhap/actions/workflows/build.yaml)
[![docs](https://github.com/bc3LC/rhap/actions/workflows/docs.yaml/badge.svg)](https://github.com/bc3LC/rhap/actions/workflows/docs.yaml)
[![pages-build-deployment](https://github.com/bc3LC/rhap/actions/workflows/pages/pages-build-deployment/badge.svg)](https://github.com/bc3LC/rhap/actions/workflows/pages/pages-build-deployment)
[![test_coverage](https://github.com/bc3LC/rhap/actions/workflows/test_coverage.yaml/badge.svg)](https://github.com/bc3LC/rhap/actions/workflows/test_coverage.yaml)
[![codecov](https://codecov.io/gh/bc3LC/rhap/branch/main/graph/badge.svg?token=rC8eIjNwcN)](https://codecov.io/gh/bc3LC/rhap)
[![DOI](https://zenodo.org/badge/721541306.svg)](https://doi.org/10.5281/zenodo.14423225)


<!-- ------------------------>
<!-- ------------------------>
## <a name="Contents"></a>Contents
<!-- ------------------------>
<!-- ------------------------>

- [Key Links](#KeyLinks)
- [Introduction](#Introduction)
- [Citation](#Citation)
- [Installation Guide](#InstallGuide)
- [How-to guide](#howto) 
- [Publications](#Publications)

<!-- ------------------------>
<!-- ------------------------>
## <a name="KeyLinks"></a>Key Links
<!-- ------------------------>
<!-- ------------------------>

[Back to Contents](#Contents)

- Github: https://github.com/bc3LC/rhap
- Webpage: bc3lc.github.io/rhap/

<!-- ------------------------>
<!-- ------------------------>
## <a name="Introduction"></a>Introduction
<!-- ------------------------>
<!-- ------------------------>

[Back to Contents](#Contents)

`rhap` is a package designed to estimate health impacts attributable to household air pollution (HAP) associated with alternative scenarios simulated using the Global Change Analysis Model (GCAM).


<!-- ------------------------>
<!-- ------------------------>
## <a name="Citation"></a>Citation
<!-- ------------------------>
<!-- ------------------------>

[Back to Contents](#Contents)

TBA

<!-- ------------------------>
<!-- ------------------------>
## <a name="InstallGuide"></a>Installation Guide
<!-- ------------------------>
<!-- ------------------------>

[Back to Contents](#Contents)

You can install the **`rhap`** package in two ways:

- **Option 1**: Install directly from GitHub  
- **Option 2**: Clone the repository (recommended for full access to project files)

### Option 1: Direct Installation from GitHub

#### Prerequisites

Make sure the following software is installed:

- **R**: [Download R](https://www.r-project.org/)
- **RStudio** (recommended IDE): [Download RStudio](https://www.rstudio.com/)

#### Install the Package

- Open **RStudio**

- Run the following in the R Console:

   ```r
   install.packages("devtools")  # Skip if already installed
   devtools::install_github("bc3LC/rhap")
   ```

### Option 2: Clone the Repository (Recommended)

Cloning the repository is recommended if you want full access to the project structure, input files, and examples.

#### Prerequisites

Make sure the following software is installed:

- **R**: [Download R](https://www.r-project.org/)
- **RStudio** (recommended IDE): [Download RStudio](https://www.rstudio.com/)
- **Git**: [Download Git](https://git-scm.com/downloads)


#### Step 1: Clone the Repository

Open **Git Bash** (or a terminal) in your desired directory, and run:

```bash
git clone https://github.com/bc3LC/rhap.git
```

This will create a local copy of the repository.

#### Step 2: Open the Project in RStudio

- Open the `rhap.Rproj` file inside the cloned folder using **RStudio**.  
- In RStudio, either go to **Build → Install and Restart** (or press **Ctrl + Shift + B**),  
   or load the package manually by running the following code:

```r
devtools::load_all()
```

###  Run a Baseline Example (Optional)

The `rhap` package includes a built-in `.dat` file that lets you run a test case based on a GCAM baseline scenario. This is a great way to verify that the package is working correctly and to explore its core functionality.

You can run this example with or without cloning the full repository, as the required data is included in the package:

   ```r
test_rhap <- rhap::test_rhap
   ```

For a step-by-step walkthrough of the example, visit the official guide: [Run a baseline scenario](https://bc3lc.github.io/rhap/articles/run_rhap.html#step-by-step-example)


<!-- ------------------------>
<!-- ------------------------>
## <a name="howto"></a> How to guides
<!-- ------------------------>
<!-- ------------------------>

[Back to Contents](#Contents)

The primary function offered by this package is `calc_hap_impacts`, designed to estimate health impacts attributable to household air pollution (HAP) across a range of alternative GCAM scenarios. This function estimates scenario-specific health outcomes by seamlessly extracting and processing relevant data from GCAM databases or project files.
The extracted data encompasses critical socioeconomic and environmental parameters, enabling comprehensive analysis of HAP impacts. These parameters include per capita GDP, emissions of key pollutants such as primary PM2.5 (BC+OC), NOx, and VOCs, and per capita floorspace metrics. By integrating these diverse factors, `calc_hap_impacts` facilitates detailed assessments of how variations in economic development, pollutant emissions, and living conditions influence human health under different policy or technological scenarios.

The package also includes an ancillary function, `calc_ResidEm_grp`, which determines the contribution of each within-region consumer group to household air pollution (HAP), with results categorized by region, year, and pollutant. Users can customize their analysis by specifying the desired region, time period, and pollutant through adjustable parameters when calling the function.
This flexibility enables targeted assessments of how various consumer groups influence HAP across specific contexts, providing valuable insights for research and policy development.


<!-- ------------------------>
<!-- ------------------------>
## <a name="Publications"></a>Publications
<!-- ------------------------>
<!-- ------------------------>

[Back to Contents](#Contents)

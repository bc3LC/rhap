[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![build](https://github.com/bc3LC/rhap/actions/workflows/build.yaml/badge.svg)](https://github.com/bc3LC/rhap/actions/workflows/build.yaml)
[![docs](https://github.com/bc3LC/rhap/actions/workflows/docs.yaml/badge.svg)](https://github.com/bc3LC/rhap/actions/workflows/docs.yaml)
[![pages-build-deployment](https://github.com/bc3LC/rhap/actions/workflows/pages/pages-build-deployment/badge.svg)](https://github.com/bc3LC/rhap/actions/workflows/pages/pages-build-deployment)
[![test_coverage](https://github.com/bc3LC/rhap/actions/workflows/test_coverage.yaml/badge.svg)](https://github.com/bc3LC/rhap/actions/workflows/test_coverage.yaml)
ADD Zenodo Badge


<!-- ------------------------>
<!-- ------------------------>
# <a name="Contents"></a>Contents
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
# <a name="KeyLinks"></a>Key Links
<!-- ------------------------>
<!-- ------------------------>

[Back to Contents](#Contents)

- Github: https://github.com/bc3LC/rhap
- Webpage: bc3lc.github.io/rhap/

<!-- ------------------------>
<!-- ------------------------>
# <a name="Introduction"></a>Introduction
<!-- ------------------------>
<!-- ------------------------>

[Back to Contents](#Contents)

`rhap` is a package designed to estimate health impacts attributable to household air pollution (HAP) associated with alternative scenarios simulated using the Global Change Analysis Model (GCAM).


<!-- ------------------------>
<!-- ------------------------>
# <a name="Citation"></a>Citation
<!-- ------------------------>
<!-- ------------------------>

[Back to Contents](#Contents)

TBA

<!-- ------------------------>
<!-- ------------------------>
# <a name="InstallGuide"></a>Installation Guide
<!-- ------------------------>
<!-- ------------------------>

[Back to Contents](#Contents)

1. Download and install:
    - R (https://www.r-project.org/)
    - R studio (https://www.rstudio.com/)  
    - (For cloning the repo) Git (https://git-scm.com/downloads) 
    
    
2. Open R studio:

```r
install.packages("devtools")
devtools::install_github("bc3LC/rhap")
```

(Optional) 

To clone the repository to the local machine: Git bash in the working directory (right click "Git Bash Here") -> In the Git console type:  

```r
git clone https://github.com/bc3LC/rhap.git
```

Then, open the Rproject (rfasst.Rproj): In the Rstudio menu, click "Build -> Install and restart" (Ctrl+Shift+B)
  

<!-- ------------------------>
<!-- ------------------------>
# <a name="keyfunctions"></a> How to guides
<!-- ------------------------>
<!-- ------------------------>

[Back to Contents](#Contents)

The primary function offered by this package is `calc_hap_impacts`, designed to estimate health impacts attributable to household air pollution (HAP) across a range of alternative GCAMscenarios. This function estimates scenario-specific health outcomes by seamlessly extracting and processing relevant data from GCAM databases or project files.
The extracted data encompasses critical socioeconomic and environmental parameters, enabling comprehensive analysis of HAP impacts. These parameters include per capita GDP, emissions of key pollutants such as primary PM2.5 (BC+OC), NOx, and VOCs, and per capita floorspace metrics. By integrating these diverse factors, `calc_hap_impacts` facilitates detailed assessments of how variations in economic development, pollutant emissions, and living conditions influence human health under different policy or technological scenarios.

The package also includes an ancillary function, `calc_ResidEm_grp`, which determines the contribution of each within-region consumer group to household air pollution (HAP), with results categorized by region, year, and pollutant. Users can customize their analysis by specifying the desired region, time period, and pollutant through adjustable parameters when calling the function.
This flexibility enables targeted assessments of how various consumer groups influence HAP across specific contexts, providing valuable insights for research and policy development.


<!-- ------------------------>
<!-- ------------------------>
# <a name="Publications"></a>Publications
<!-- ------------------------>
<!-- ------------------------>

[Back to Contents](#Contents)

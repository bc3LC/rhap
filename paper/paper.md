---
title: 'rhap: A tool to analyze health impacts attributable to household air pollution'
tags:
  - R
  - Pollution
authors:
  - name: Jon Sampedro
    orcid: 0000-0002-2277-1530
    affiliation: 1
  - name: Clàudia Rodés-Bachs
    orcid: 0000-0001-6696-7685
    affiliation: 1
  - name: Steven J. Smith
    orcid: 0000-0003-3248-5607
    affiliation: 2
  - name: Manuel Tomás
    orcid: 0000-0003-1725-7702
    affiliation: 1
  - name: Ismael Urrutia
    affiliation: 3
  - name: Dirk-Jan Van de Ven
    orcid: 0000-0001-9120-564X
    affiliation: 1
affiliations:
 - name: Basque Centre for Climate Change (BC3), Leioa, Spain
   index: 1
 - name: Joint Global Change Research Institute, Pacific Northwest National Laboratory, College Park, MD, USA
   index: 2
 - name: University of the Basque Country, Bilbao, Spain
   index: 3

date: 08 January 2025
bibliography: paper.bib
---
# Summary
Recent evidence highlights household air pollution as a significant health risk, particularly in the Global South. 
However, its impacts are frequently overlooked in the integrated assessment models widely used for global scenario analyses.
Alternative scenarios with diverse socioeconomic pathways or climate policies could reshape access to affordable clean energy, particularly for low-income groups, directly impacting household air pollution.

`rhap` is an R package developed to estimate health impacts attributable to household air pollution (HAP) under alternative scenarios simulated using the Global Change Analysis Model (GCAM). 
These impacts are derived using an econometric model that links health outcomes from HAP to various air pollutant emissions and socioeconomic variables, all of which can be extracted from scenario-specific GCAM outputs. 
Figure 1 provides an overview of the `rhap` package structure.

![Overview of `rhap`](figure_rhap.png)

The `rhap` package is publicly available on GitHub at https://github.com/bc3LC/rhap. Below is a simplified example demonstrating how to run the package. 
Comprehensive documentation is also provided and can be accessed [here](https://bc3lc.github.io/rhap/index.html).

```r
install.packages("devtools")
library(devtools)
devtools::install_github("bc3LC/rhap")
library(rhap)

db_path <- "path_to_your_gcam_database" # NULL if project file provided
db_name <- "name of the database" # NULL if project file provided
prj_name <- "name of the project file"
scen_name <- "name of the GCAM scenario" # or vector of names
HIA_var  <- "deaths" # or YLLs or DALYs

 
hap_damages <- calc_hap_impacts (db_path,  db_name, 
prj_name,
scen_name, final_db_year = 2100,
HIA_var,
saveOutput = T, 
map = T) 


```


# Statement of need

According to the World Health Organization’s (WHO) [Global Health Observatory]( https://www.who.int/data/gho/data/indicators/indicator-details/GHO/household-air-pollution-attributable-deaths), 
HAP is one of the major risk factors for human health, particularly in the global South, causing around 3.2 million deaths per year in 2020, including over 237,000 deaths of children under the age of 5. 
HAP affects human health from pre-conception to old age, leading to noncommunicable diseases including cataract, chronic obstructive pulmonary disease, ischaemic heart disease, lower respiratory infections, lung cancer, neonatal disorders, stroke, and type 2 diabetes [@bennitt2025global]. 
In addition to its direct health impacts, disproportionately affecting infants and the elderly, household air pollution (HAP) also has broader socioeconomic consequences, 
such as reduced work productivity [@neidell2023air] and the exacerbation of gender inequalities [@krishnapriya2021improved].

Given the magnitude of these effects, various methodologies have been developed to estimate HAP and its associated health implications [@mohajeri2023urban; @das2021benefits]. 
However, HAP and its subsequent impacts on human health are often overlooked in global scenario analysis. 
Alternative socioeconomic and climate pathways, such as those involving high or low GDP growth and the adoption of deep decarbonization strategies, can significantly influence the transition to cleaner energy fuels and technologies. These transitions may improve access to cleaner energy sources for lower-income households, thereby reducing exposure to HAP and mitigating its health impacts. 
Despite this, the dominant tools used in global scenario analysis, namely, integrated assessment models (IAMs), typically estimate future emissions of air pollutants but do not report on air pollution levels or their health consequences. While some external tools can be linked to IAMs to assess ambient air pollution and its impacts [@sampedro2022rfasst], there is currently no such tool available for HAP and its associated effects.
Incorporating HAP dynamics into IAMs, such as the Global Change Analysis Model (GCAM), represents a meaningful contribution to the field. It provides important insights for evaluating alternative future scenarios with greater attention to equity, health, and household-level energy transitions.

GCAM is a multisector integrated assessment model that quantifies human and Earth-system dynamics by examining the interconnections between the economy, energy, water, climate, and agriculture, forestry, and land use (AFOLU) systems. 
Detailed documentation is available [online](https://github.com/JGCRI/gcam-doc). 
Focusing on emissions, GCAM estimates a wide range of greenhouse gases and air pollutants for each future scenario, categorized by sector, region, and time period up to 2100. 
Nevertheless, while GCAM estimates direct air pollutant emissions from the residential sector, it does not account for HAP levels or their associated health impacts. 
The `rhap` package bridges this gap by quantifying HAP impacts for each GCAM scenario. 
Given the well-documented importance of HAP and its health impacts, a tool that automatically estimates these effects within the GCAM modelling framework represents a valuable advancement for the integrated assessment modelling community, 
and it addresses a critical gap in global scenario analysis.



# Functionality
The core function of this package is the `calc_hap_impacts` function, specifically designed to estimate health impacts attributable to HAP across different GCAM scenarios. 
This function extracts and processes the required socioeconomic and emissions data from GCAM databases or project files and employs a fixed-effects econometric regression model to estimate impacts. 
Key covariates in the model include direct emissions from the residential sector (Primary PM2.5, NOx, and VOC), per capita GDP, and per capita floorspace.
The model is calibrated using cross-regional, multi-year panel data aggregated from diverse sources. 
Detailed information about the model’s formulation, underlying assumptions, and data sources is provided in the accompanying [vignette](https://bc3lc.github.io/rhap/articles/fit_model.html). 

The `calc_hap_impacts` function offers flexibility in generating three distinct health impact metrics: premature mortalities, Years of Life Lost (YLLs), and Disability-Adjusted Life Years (DALYs). 
Users can specify the desired metric by setting the `HIA_var` parameter, with premature mortalities selected by default. 
The package also offers an optional feature (`by_gr = TRUE`) to estimate health impacts by income decile within each region. While country-level calculations ensure consistency, this feature helps explore intra-regional disparities.
In terms of outputs, activating the saveOutput parameter enables the function to save results as a Comma-Separated Values (CSV) file in the `output` sub-directory. 
The function can also generate damage maps and animations by enabling the `map` and `anim` parameters, respectively, based on the rmap package [@khan2022rmap].

![Premature deaths per 100.000 inhabitants attributable to household air pollution in 2050](map_base_2050.png)

The package includes an additional function, `calc_ResidEm_grp`, which quantifies the contribution of within-region consumer groups (e.g., income deciles) to emissions of various pollutants driving HAP. 
Users can customize the analysis by specifying the desired region, time period, and pollutant through adjustable parameters. 
A full list of pollutants that can be analyzed is provided in the dedicated [vignette](https://bc3lc.github.io/rhap/articles/ResidEm_grp.html). 
This functionality supports targeted assessments of how different consumer groups contribute to HAP emissions within specific contexts, offering valuable insights for research and policy development. 

![Direct BC emissions from the residential sector in India in 2050 by income decile](example_gr.png){width="42.5%"}

# Acknowledgements
This research is supported by the European Union’s Horizon research program under grant agreement 101060679 (GRAPHICS project); 
by the María de Maeztu Excellence Unit 2023-2027 Ref. CEX2021-001201-M, funded by MCIN/AEI /10.13039/501100011033; 
and by the Basque Government through the BERC 2022-2025 program. The authors also acknowledge financial support from the Horizon Europe European Commission Projects ‘IAM COMPACT’ (grant no. 101056306) and ‘DIAMOND’ (grant no. 101081179). 
The views and opinions expressed in this paper are those of the authors alone.

# References

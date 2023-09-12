# Do actively cycling C and N pools depend ultimately on soil P supply? Across-biome synthesis

## PIs: 

- Craig See, Postdoctoral Scientist, Northern Arizona University
- Ellery Vaughan, Postdoctoral Scientist, Northern Arizona University
- Ruth Yanai, Professor, SUNY College of Environmental Science and Forestry

## Project Summary

- https://lternet.edu/working-groups/soil-p-control-of-c-and-n/

## Script Explanations

- `soil_p_harmonize.R` - Accepts the [GoogleSheet Data Key](https://docs.google.com/spreadsheets/d/1fJswJ876A1LfFbiwJJ9aJp-Bvl0QEdzjMThOWEHkQUU/edit#gid=402780056) and the raw input files and harmonizes them (i.e., synonymizes columns and combines). Then performs generally necessary tidying / wrangling. Ends by exporting "archival" data (i.e., mostly long-format data relevant to a broad suite of potential data re-users)

- `soil_p_wrangle.R` - Accepts the harmonized data (see `soil_p_harmonize.R`) and performs additional wrangling required by the Soil P SPARC group. Exports an analysis-ready CSV.

- `soil_p_stats.R` - Performs desired statistical analysis to test hypotheses of Soil P SPARC group.

## Supplementary Resources

NCEAS Scientific Computing Support Team page [link](https://nceas.github.io/scicomp.github.io)

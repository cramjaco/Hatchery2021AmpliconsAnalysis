# Hatchery2021 Amplicons
Author: Jacob Cram
Date: 2024 April 24

The four main files that run the analysis are as follows

`RScripts/Drain_Log_Tests.R` -- Shows survivorship over time in both all of the samples in the hatchery, and just the samples we are examining.
`Notebooks/ThirdPassCore.Rmd` -- Carries out CCA based tests to determine how microbial community at each taxonomic level varies between crashed and good communities. It also makes ordination plots showing how CCA varies across time and between good and chrashed sampels, at the ASV and family level, and provides associated statistics.
`Notebooks/TTestsTaxa.Rmd` -- Determine which taxa are different between good and crashed samples at each time-point and each taxonomic level.. These tests identified two ASVs and one class that varied between good and crashed samples at the 3-5 day time-point, which preceded the crashes.

A description of the file structure follows:
`Archive` -- Early codes that didn't make it into the final paper.
`Figures` -- Images used in the manuscript (and some that weren't)
`Hatchery_UVexperimet_All` -- Input data from the hatchery
`HatcheryData` -- More input data. 
`IntermediateData` Data products produced by this analysis.
`Notebooks` R markdown lotebooks that carry out the analysis. All files are listed above.
`RScripts` R scripts that carry out analysis. Many are called by other scripts. These include:
  `Drain_Log_Tests.R` See above
  `Initial_Processing.R` Brings in and wrangles data.
  `Oyster Library.R` Functions used by various analysis.
  `Parse_NCBI_Vibrio.R` Some processing of analysis from NCBI about the Vibrio species.
  `Load_In_Drain_Logs.R` Loads the survivorship data.
  `WrangleMetadata.R` Some processing of metadata. Saves out a new metadata file. The data is further modified by `Initial_Processing.R`
  `ExportBroods.R` Summarizes brood information.
  
`renv` Data for package managment.

`.Rprofile`
`.Rproj.user`
`renv.lock` information about package versions used.
`.gitignore` files not tracked through git.
`.README.md` this file.
  
  
  
# HydroATLAS_sampleR
Example workflow to snap sites on rivers and lakes to RiverATLAS and LakeATLAS, respectively, and extract raster data

**Structure**: the overall project directory is structured with the following sub-directories:  
data/ (raw data, read-only, not to be altered)  
results/ (results of the analysis, mostly reproduceable through code execution. However, also includes manually modified results)
src/ (code written for the project)  

**R Workflow**: this project is setup with a [targets workflow](https://docs.ropensci.org/targets/), ensuring reproducibility.
In the `targets` philosophy, every action is a function, and every R object resulting from a workflow step is a "target" with dependencies.
Intermediate targets/objects are stored in a `_targets` directory. 

**Dependency management**: the R library of this project is managed by [renv](https://rstudio.github.io/renv/articles/renv.html).
This makes sure that the exact same package versions are used when recreating the project.
When calling `renv::restore()`, all required packages will be installed with their specific version. 
Please note that this project was built with R version 4.4.0. on a Windows 10 operating system. Installation may not be possible on previous versions.

**Syntax**: this analysis relies on the [data.table](https://rdatatable.gitlab.io/data.table/) syntax, which provides a high-performance version of data.frame. It is concise, faster, and more memory efficient than conventional data.frames and the tidyverse syntax.

**Spatial analysis**: most of the basic spatial analysis in this workflow is conducted using the `terra` package. `terra` replaces the [raster](https://github.com/rspatial/raster) package. The interfaces of `terra` and `raster` are similar, but `terra` is simpler, faster and can do more. There are tutorials at [rspatial.org/terra](https://rspatial.org/terra/index.html). 

## Getting started
### Download the repository for R
In Git Bash, the following commands illustrate the procedure to make a local copy of the Github repository in a newly created directory at C://GeneticScaling/src :

```{r, engine = 'bash', eval = FALSE}
Mathis@DESKTOP MINGW64 /c/GeneticScaling/src
$ git clone https://github.com/messamat/HydroATLAS_sampleR.git
```

In R Studio for Windows, the following procedure can be used:  

* Click on “File” in the menu ribbon  
* Select “New project…”  
* Choose the “Version control” option in the New Project Wizard window.
* Then, select “Git” in the next window.
* In the next window, fill the fields as follows:  
  * Repository URL: https://github.com/messamat/HydroATLAS_sampleR 
  * Project directory name: [will autofill as “HydroATLAS_sampleR ”]  
  * Create project as subdirectory of: [choose the parent directory of src, e.g., C://HydroATLAS_sampleR]  
* Tick “Open in new session” and then click “Create project”.  


### Github repository structure
- [**data/**] - sample sites
- [**renv/**] - code to automatically activate renv
- [**src/**] — core of the analysis
  - [*functions.R*] - all custom functions used in the data formatting and analysis. 
  - [*packages.R*] - all packages used in the workflow.
- [*.Rprofile*] — used to activate renv for new R sessions launched in the project.
- [*HydroATLAS_sampleR.Rproj*] — R project file.
- [*LICENSE*] - Code license.
- [*README.md*] — README for Github (this file)
- [*\_targets.R*] — configuration script for targets workflow,  this specific file name is required by the targets package. Contains the targets “plan”, the high-level catalog of all the steps in the workflow (see the corresponding chapter in the targets user manual). This plan defines the order of functions to use, their inputs and outputs (usually, targets), and the relationship among targets and steps.
- [*renv.lock*] — renv lockfile, describing the state of the project’s library (installed packages and their version).


## Running the analysis
Provided that your were given the necessary data, the entire analysis can simply be re-run with the following code:
```{r rmake, eval = FALSE}
source('_targets.R')
tar_make()
```
`tar_make()` is the central function of the targets approach. It runs all the steps of the workflow in the correct order, skipping any work that is already up to date. Because of how targets tracks global functions and objects as dependencies of targets, the use of `tar_make()`  is needed to run the analysis pipeline in a clean reproducible environment. If all targets are up to date in the caching directory, then nothing will be run.

## Inspecting results
If you were provided intermediate targets (i.e., a `_targets/` directory; or once you have re-run the analysis), you can load individual targets in the environment with the following commands (even if the targets are not up to date due to e.g. a change in source path). 
``` {r loadtarg, eval = FALSE}
tar_load(sites_gai) #Load target in memory (R environment) with original target name as variable name 
sites_gai_new <- tar_read(sites_gai) #Load target in memory with new variable name
```

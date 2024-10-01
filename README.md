<!-- badges: start -->
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![R-CMD-check](https://github.com/pet221/SSNbler/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/pet221/SSNbler/actions/workflows/R-CMD-check.yaml)
[![CRAN](http://www.r-pkg.org/badges/version/SSNbler)](https://cran.r-project.org/package=SSNbler)
[![cran checks](https://badges.cranchecks.info/worst/SSNbler.svg)](https://cran.r-project.org/web/checks/check_results_SSNbler.html)
<!-- badges: end -->

# SSNbler: Assemble 'SSN' objects

'SSNbler' imports, creates and assembles data needed to fit spatial-statistical stream-network models using the [SSN2 R package](https://usepa.github.io/SSN2/). Streams, observations, and prediction locations are represented as simple features and specific tools have been provided to define topological relationships between features; calculate the hydrologic distances (with flow-direction preserved) and the spatial additive function used to weight converging stream segments; and export the topological, spatial, and attribute information to an SSN object, which can be efficiently stored, accessed and analysed in R.  

Learn more about 'SSNbler' on [our website](https://pet221.github.io/SSNbler/).

## Installation Instructions

```r
# install the most recent approved version from CRAN
install.packages("SSNbler")
# load the most recent approved version from CRAN
library(SSNbler)
```

Install and load the most recent version of'SSNbler' from GitHub by running
```r
# Installing from GitHub requires you first install the remotes package
install.packages("remotes")

# install the most recent version from GitHub
remotes::install_github("pet221/SSNbler", ref = "main")
# load the most recent development version from GitHub
library(SSNbler)
```

## Citation

If you use 'SSNbler' in a formal publication or report, please cite it. Citing 'SSNbler' lets us devote more resources to it in the future. View the 'SSNbler' citation by running
```r
citation(package = "SSNbler")
```

```
#> 
#> To cite SSNbler in publications use:
#> 
#>   Peterson E. E., Dumelle, M., Pearse A., Teleki D., and Ver Hoef, J. M.
#>   (2024). SSNbler: Assemble SSN objects in R. R package version 0.1.0
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     title = {{SSNbler}: Assemble SSN objects in {R}},
#>     author = {Erin Peterson and Michael Dumelle and Alan Pearse and Dan Teleki and Jay M. {Ver Hoef}},
#>     year = {2024},
#>     note = {{R} package version 0.1.0},
#>   }
```

## Example Usage

Please see our [introductory vignette](https://pet221.github.io/SSNbler/articles/introduction.html) for a brief example showing how to use 'SSNbler'. 

## License

This project is licensed under the GNU General Public License, [GPL-3](https://cran.r-project.org/web/licenses/GPL-3).


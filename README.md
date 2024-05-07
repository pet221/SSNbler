<!-- badges: start -->
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![R-CMD-check](https://github.com/pet221/SSNbler/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/pet221/SSNbler/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

# SSNbler: Assemble SSN objects in R

SSNbler imports, creates and assembles data needed to fit spatial-statistical stream-network models using the [SSN2 R package]((https://usepa.github.io/SSN2/)). Streams, observations, and prediction locations are represented as simple features and specific tools have been provided to define topological relationships between features; calculate the hydrologic distances (with flow-direction preserved) and the spatial additive function used to weight converging stream segments; and export the topological, spatial, and attribute information to an SSN object, which can be efficiently stored, accessed and analysed in R.  

## Citation

If you use `SSNbler` in a formal publication or report, please cite it. Citing `SSNbler` lets us devote more resources to it in the future. View the `SSNbler` citation by running
```r
citation(package = "SSNbler")
```

```
#> 
#> To cite SSNbler in publications use:
#> 
#>   Peterson EE, Pearse A, Dumelle M. and Teleki D (2024). SSNbler:
#>   Assemble SSN objects in R. R package version 0.1.0
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     title = {{SSNbler}: Assemble SSN objects in {R}},
#>     author = {Erin Peterson and Alan Pearse and Michael Dumelle and Dan. Teleki},
#>     year = {2024},
#>     note = {{R} package version 0.1.0},
#>   }
```

## Statement of Need

More to come.


## Installation Instructions

Install and load the most recent version of`SSNbler` from GitHub by running
```r
# Installing from GitHub requires you first install the remotes package
install.packages("remotes")

# install the most recent version from GitHub
remotes::install_github("pet221/SSNbler", ref = "main")
# load the most recent development version from GitHub
library(SSNbler)
```


## Contributing to `SSN2`

More to come.

## Getting Help

More to come.

## Example Usage

Please see the introductory vignette for a brief example showing how to use `SSNbler`. 

## License

This project is licensed under the GNU General Public License, [GPL-3](https://cran.r-project.org/web/licenses/GPL-3).

## EPA Disclaimer

The United States Environmental Protection Agency (EPA) GitHub project code is provided on an "as is" basis and the user assumes responsibility for its use. EPA has relinquished control of the information and no longer has responsibility to protect the integrity , confidentiality, or availability of the information. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by EPA. The EPA seal and logo shall not be used in any manner to imply endorsement of any commercial product or activity by EPA or the United States Government.


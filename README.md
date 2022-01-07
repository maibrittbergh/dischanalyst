
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dischanalyst

<!-- badges: start -->

<!-- badges: end -->

The goal of dischanalyst is to offer an easy discharge analysis. Uses a
statistical approach to analyse hydrological data. Was created for a low
flow Analysis. Therefore a discharge time series is required. Helps to
create a descriptive statistical and graphical summary of a dataset.

## Installation

You can install the development version of dischanalyst from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("maibrittbergh/dischanalyst")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r

#to be able to run the functions of dischanalyst on your computer

# Library Packages --------------------------------------------------------


#library(Kendall)
#library(dischanalyst)




# Load in data ------------------------------------------------------------
#enter path: where did you save GRDC-Dataset (path to grdc_disc)
#path="/Users/maibrittberghofer/Desktop/Bachelorarbeit/Datafolder/grdc_03_2021/grdc_disc"
#Country="DE" #in which country are you interested?
#metadata_germany=metadata_grdc(Country, path)


# Load in datset of interest (specific river as well as station )
#rivername="MOSELLE RIVER" #rivername must be equal like rivername in metadata
#station= "COCHEM" #stationname must be equal like stationname  in metadata
#mosel=grdc_readr(metadata_germany, rivername, path )
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this. You could also
use GitHub Actions to re-render `README.Rmd` every time you push. An
example workflow can be found here:
<https://github.com/r-lib/actions/tree/v1/examples>.

You can also embed plots, for example:

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.

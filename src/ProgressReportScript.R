## EDIT CODE BELOW THIS LINE

## Each item in "list_to_generate" is a list consisting of:
## 1. vector containing ALL of a single patient's IDs written in all lowercase. Format: c("gest24", "vist34")
## 2. (optional) date of earliest survey and tech data to include. Format: "yyyy-mm-dd"
list_to_generate <- list( 
  list(c("ShWa86"), "2011-01-01")
)

## Setting "rounds" to TRUE will display a patient's demographics, session number, and planned course
## at the top of the report.
## Only do this if "Demographics.csv" AND session numbers in "Tech.csv" are fully updated!
rounds <- TRUE

## DO NOT EDIT CODE BELOW THIS LINE

pkgLoad <- function( packages = "std" ) {

    if( length( packages ) == 1L && packages == "std" ) {
        packages <- c( "data.table", "chron", "plyr", "dplyr", "shiny",
                       "shinyjs", "parallel", "devtools", "doMC", "utils",
                       "stats", "microbenchmark", "ggplot2", "readxl",
                       "feather", "googlesheets4", "readr", "DT", "knitr",
                       "rmarkdown", "Rcpp", "formattable", "ggnewscale",
                       "htmltools", "lubridate", "stringr", "tidyr", "tidyverse"
        )
    }

    packagecheck <- match( packages, utils::installed.packages()[,1] )

    packagestoinstall <- packages[ is.na( packagecheck ) ]

    if( length( packagestoinstall ) > 0L ) {
        utils::install.packages( packagestoinstall,
                             repos = "https://cran.case.edu/"
        )
    } else {
        print( "All requested packages already installed" )
    }

    for( package in packages ) {
        suppressPackageStartupMessages(
            library( package, character.only = TRUE, quietly = TRUE )
        )
    }

}

## Ensure all packages are loaded/installed.
pkgLoad()

for (item in list_to_generate) {
  selected_patient <- item[[1]]
  patient_id <- item[[1]][1]
  if (length(item) > 2) {
    earliest_date <- item[[3]]
  } else {
    earliest_date <- NA
  }
  rmarkdown::render("src/ProgressReportsGenerator.Rmd",
                    output_file = paste0("../output/", str_to_title(substr(selected_patient[[1]], 1, 2)), str_to_title(substr(selected_patient[[1]], 3, 4)), "_Report_", Sys.Date(), ".html"),
                    params = list(selected_patient = selected_patient,
                                  patient_id = patient_id,
                                  earliest_date = earliest_date,
                                  rounds = rounds))
}



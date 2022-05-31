#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
## EDIT CODE BELOW THIS LINE

## Each item in "list_to_generate" is a list consisting of:
## 1. vector containing ALL of a single patient's IDs written in all lowercase. Format: c("gest24", "vist34")
## 2. string of patient's name to display as report title. Format: "Emily Jusuf"
## 3. (optional) date of earliest survey and tech data to include. Format: "yyyy-mm-dd"

## Setting "rounds" to TRUE will display a patient's demographics, session number, and planned course
## at the top of the report.
## Only do this if "Demographics.csv" AND session numbers in "Tech.csv" are fully updated!
rounds <- TRUE

pkgLoad <- function( packages = "std" ) {

    if( length( packages ) == 1L && packages == "std" ) {
        packages <- c( "data.table", "chron", "plyr", "dplyr", "shiny",
                       "shinyjs", "parallel", "devtools", "doMC", "utils",
                       "stats", "microbenchmark", "ggplot2", "readxl",
                       "feather", "googlesheets4", "readr", "DT", "knitr",
                       "rmarkdown", "Rcpp", "formattable", "ggnewscale",
                       "htmltools", "lubridate", "stringr", "tidyr", "assertive"
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
options(gargle_oauth_email = "pierce.g.wang@gmail.com")
## gs4_auth(gargle_oauth_email = "pierce.g.wang@gmail.com")

survey_raw <- read_sheet("1AFGLSMBQyUNUL0p84mZ-xusaHbLrAveUp-uGTxWl3r4", "Sheet2") ## >% select(-"Txt Sum", -"Email", -"Name", -contains(".of.16."),
##                                                 -contains(" of 21 "), -contains(" of 9 "), -contains(" if none "),
##                                                 -starts_with("Have "), -starts_with("If ")) %>%
##   mutate(Date = as.Date("Completion time", "%m/%d/%y"),
##          Patient = tolower(Patient)) %>%
##   rename(SurveyID = "What survey(s) would you like to complete?",
##          GAD7 = GAD7_)
## print(args[1])
numbers_only <- function(x) !grepl("\\D", x)
for (id in args) {
    if (!numbers_only(id))
        printf("%s not valid ID", id)
    else {
        survey_entry <- survey_raw[survey_raw$ID == id,]
        print(class(survey_entry)) ## Debug
        output_file <- sprintf("../output/%s.txt", id)
cat(sprintf("Date: %s
Stimulation Parameters:
Machine: MagVenture MagPro R30 with Cool-B65 coil.
xx total sessions each separated by at least 45 minutes.

Session xx:  xx:xx

%s

Protocol A: %s stimulated with %s at %i%% MT (%i%% MO)
%s
Technician Notes: %s
Technician: %s
Supervision: Supervised by %s. %s.

Course:  Session # %s of %s planned.
/////-----Patient Mood Surveys-----/////
",
survey_entry$'Start time',
if(survey_entry$'Did you re-threshold since the last session?'=="Yes") sprintf("Motor Threshold today was measured to be %s%% MO. It was performed by %s due to %s.",
                                                                               survey_entry$'What is the new motor threshold?',
                                                                               survey_entry$'What is the full name of the person that performed the re-threshold?',
                                                                               survey_entry$'Why did you re-threshold?') else "",
survey_entry$'What is the first target?',
survey_entry$'What is the first protocol?',
survey_entry$'What is the machine output as a percent of the motor threshold for the first target?' * 100,
survey_entry$'What did you set the machine output for the first target?',
if(survey_entry$'Is there a second target?' == "Yes") sprintf("Protocol B: %s stimulated with %s at %i%% MT (%i%% MO)",
                                                              survey_entry$'What is the second target?',
                                                              survey_entry$'What is the second protocol?',
                                                              survey_entry$'What is the amchine output as a percent of hte motor threshold for the second target?' * 100,
                                                              survey_entry$'What did you set the machine output for the second target?') else "",
survey_entry$'Please write your notes for this patient (select all that may apply)',
survey_entry$'Technician\'s Full Name',
survey_entry$'Who is the supervising psychiatrist?',
survey_entry$'What is the supervising level?',
survey_entry$'Tx',
survey_entry$'Planned Course'), file=output_file,fill=FALSE)
        }
}

## EDIT CODE BELOW THIS LINE

## Each item in "list_to_generate" is a list consisting of:
## 1. vector containing ALL of a single patient's IDs written in all lowercase. Format: c("gest24", "vist34")
## 2. (optional) date of earliest survey and tech data to include. Format: "yyyy-mm-dd"
list_to_generate <- list( 
  list("AaSc56")
)

## Setting "rounds" to TRUE will display a patient's demographics, session number, and planned course
## at the top of the report.
## Only do this if "Demographics.csv" AND session numbers in "Tech.csv" are fully updated!
rounds <- TRUE

## DO NOT EDIT CODE BELOW THIS LINE

pkgLoad <- function( packages = "std" ) {

    if( length( packages ) == 1L && packages == "std" ) {
        packages <- c( "data.table", "chron", "plyr", "dplyr", "shiny",
                       "shinyjs", "parallel", "doMC", "utils",
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

demographics <- read_sheet("***REMOVED***", sheet="Patient Demographics") 

gs4_auth(email = "***REMOVED***")
tech_raw_old_1 <- read_sheet("***REMOVED***", sheet="TMS Technician Data Input 2021 (3)", range="B:AF")
# %>%
#     rename(timestamp = 'What is the date of the treatment?',
#            Tx = 'Treatment #',
#            Planned_Course = 'Planned Course') %>%
#     mutate(
#         Date = as.Date(as.character(timestamp), "%m/%d/%Y"),
#         Tx = as.integer(Tx)) %>%
#     unite(pt_id, c('What is the four letter patient ID? (First two letters of FIRST and LAST name)', 'What are the last two digits of the patient\'s cell phone number?'), sep="", remove=TRUE)


tech_raw_old_2 <- read_sheet("***REMOVED***", sheet="TMS Technician Data Input 2021 1 (1)", range="B:AF")
# %>% ## select(-"Txt.Sum", -"Email", -"Name", -"Date") %>%
    # rename(timestamp = 'What is the date of the treatment?',
    #        Tx = 'Treatment #',
    #        Planned_Course = 'Planned Course') %>%
    # mutate(
    #     Date = as.Date(as.character(timestamp), "%m/%d/%Y"),
    #     Tx = as.integer(as.character(Tx))) %>%
    # unite(pt_id, c('What is the four letter patient ID? (First two letters of FIRST and LAST name)', 'What are the last two digits of the patient\'s cell phone number?'), sep="", remove=TRUE)
## sprintf("2: %i", ncol(tech_raw_old_2))

tech_raw_old_3 <- read_sheet("***REMOVED***", sheet="TMS Technician Data Input Summer 2020", range="B:AF")
# %>% ## select(-"Txt.Sum", -"Email", -"Name", -"Date") %>% 
    # rename(timestamp = 'What is the date of the treatment?',
    #        Tx = 'Treatment #',
    #        Planned_Course = 'Planned Course') %>%
    # filter(Tx != "NA") %>%
    # mutate(
    #     Date = as.Date(timestamp, "%m/%d/%Y"),
    #     Tx = as.integer(as.character(Tx))) %>%
    # unite(pt_id, c('What is the four letter patient ID? (First two letters of FIRST and LAST name)', 'What are the last two digits of the patient\'s cell phone number?'), sep="", remove=TRUE)
## sprintf("3: %i", ncol(tech_raw_old_3))

tech_raw_current <- read_sheet("***REMOVED***", sheet="Form Responses 1") # %>% ## select(-"Txt.Sum", -"Email", -"Name", -"Date") %>%
    # rename(timestamp = 'What is the date of the treatment?',
    #        Tx = 'Treatment #',
    #        Planned_Course = 'Planned Course') %>%
    # filter(Tx != "NA") %>%
    # mutate(
    #     Date = as.Date(Timestamp, "%m/%d/%Y"),
    #     Tx = as.integer(Tx)) %>%
    # unite(pt_id, c('What is the four letter patient ID? (First two letters of FIRST and LAST name)', 'What are the last two digits of the patient\'s cell phone number?'), sep="", remove=TRUE)
## sprintf("4: %i", ncol(tech_raw_current))


tech_raw <- rbind(tech_raw_old_1, tech_raw_old_2, tech_raw_old_3, tech_raw_current) %>%
    rename(timestamp = 'What is the date of the treatment?',
           Tx = 'Treatment #',
           Planned_Course = 'Planned Course') %>%
    filter(Tx != "NA") %>%
    unite(pt_id, c('What is the four letter patient ID? (First two letters of FIRST and LAST name)', 'What are the last two digits of the patient\'s cell phone number?'), sep="", remove=TRUE) %>%
    mutate(
        Date = as.Date(timestamp),
        Tx = as.integer(as.character(Tx)),
        pt_id = tolower(pt_id)
        ) %>%
  rename(Protocol = 'What is the ordered protocol?')

## print(tech_raw)

survey_raw_old <- read_sheet("***REMOVED***", sheet="Results") %>%
    rename(pt_id = 'Patient Code') %>%
    mutate(Date = as.Date(Timestamp),
           pt_id = tolower(pt_id))

survey_hourly_raw_old <- read_sheet("***REMOVED***", sheet="Hourly") %>%
  rename(Timestamp = 'Start time') %>%
  unite(pt_id, c('What are the first two letters of your FIRST name?', 'What are the first two letters of your LAST name?', 'What are the LAST two digits of your phone number?'), sep="", remove=TRUE) %>%
    mutate(Date = as.Date(Timestamp),
           pt_id = tolower(pt_id))

survey_raw_current <- read_sheet("***REMOVED***", sheet="Main Sheet") %>%
  rename(pt_id = 'Patient Code') %>%
    mutate(Date = as.Date(Timestamp),
           pt_id = tolower(pt_id))

survey_hourly_raw_old <- read_sheet("***REMOVED***", sheet="Hourly") %>%
  rename(Timestamp = 'Start time') %>%
  unite(pt_id, c('What are the first two letters of your FIRST name?', 'What are the first two letters of your LAST name?', 'What are the LAST two digits of your phone number?'), sep="", remove=TRUE) %>%
    mutate(Date = as.Date(Timestamp),
           pt_id = tolower(pt_id))

survey_raw <- rbind(survey_raw_old, survey_raw_current) %>%
  mutate(PHQ_Q9 = as.integer(PHQ_Q9)) %>%
  rename(MADRS = 'MADRS-SR',
         PreTMS = 'Pre-TMS')



madrs_raw <- read_sheet("https://docs.google.com/spreadsheets/d/***REMOVED***")

fetch_patient_id_aliases <- function(demographics, id) {
  lower_id <- tolower(id)
  aliases <- demographics[tolower(demographics$`Patient ID`) %like% lower_id, ]
  return(str_split(tolower(aliases$`Patient ID`[1]), ", "))
}


for (item in list_to_generate) {
  patient_id <- item[[1]]
  selected_patient <- fetch_patient_id_aliases(demographics, patient_id)[[1]]
  if (length(item) > 1) {
    earliest_date <- item[[2]]
  } else {
    earliest_date <- NA
  }
  rmarkdown::render("src/ProgressReportsGenerator.Rmd",
                    output_file = paste0("../output/", patient_id, "_Report_", Sys.Date(), ".html"),
                    params = list(selected_patient = selected_patient,
                                  patient_id = patient_id,
                                  earliest_date = earliest_date,
                                  rounds = rounds))
}



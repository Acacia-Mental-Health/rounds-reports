#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
error_file <-"../output/error_log.txt"

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
options(gargle_oauth_email = "***REMOVED***")
## gs4_auth(gargle_oauth_email = "pierce.g.wang@gmail.com")

numbers_only <- function(x) !grepl("\\D", x)

generate_output <- function(filtered_data, patient) {
    ## print(class(survey_entry)) ## Debug
    output_file <- sprintf("../output/%s.html", patient)
    file.create(output_file)
    cat(sprintf("<!DOCTYPE html>
<html>
  <head>
    <meta charset=\"UTF-8\">
    <title>Entries for Patient: %s</title>
  </head>
  <body>\n", patient), file=output_file, fill=FALSE)
    notes_template <- "\n<hr>\n\nDate: %s <br />
Stimulation Parameters:<br />
Machine: MagVenture MagPro R30 with Cool-B65 coil.<br />
xx total sessions each separated by at least 45 minutes.<br />
<br />
Session xx:  xx:xx<br />
<br />
%s
<br />
Protocol A: %s stimulated with %s at %s%% MT (%s%% MO)<br />
%s<br />
Technician Notes: %s<br />
Technician: %s<br />
Supervision: Supervised by %s. %s.<br />
<br />
Course:  Session # %s of %s planned.<br />
/////-----Patient Mood Surveys-----/////\n"
    for (i in 1:nrow(filtered_data)) {
      entry <- filtered_data[i,]
      output <- sprintf(notes_template, entry$Timestamp,
                        if(entry$'Did you re-threshold since the last session?'=="Yes") sprintf("Motor Threshold today was measured to be %s%% MO. It was performed by %s due to %s.<br />",
                                                                                 entry$'What is the new motor threshold?',
                                                                                 entry$'What is the full name of the person that performed the re-threshold?',
                                                                                 entry$'Why did you re-threshold?') else "",
                        entry$'What is the first target?',
                        entry$'What is the first protocol?',
                        as.numeric(entry$'What is the machine output as a percent of the motor threshold for the first target?') * 100,
                        entry$'What did you set the machine output for the first target?',
                        if(entry$'Is there a second target?' == "Yes") sprintf("Protocol B: %s stimulated with %s at %s%% MT (%s%% MO)<br />",
                                                                                      entry$'What is the second target?',
                                                                                      entry$'What is the second protocol?',
                                                                                      as.numeric(entry$'What is the machine output as a percent of the motor threshold for the second target?') * 100,
                                                                                      entry$'What did you set the machine output for the second target?') else "",
                        entry$'Please write your notes for this patient (select all that may apply)',
                        ## survey_entry$'Technician\'s Full Name',
                        entry$'Technician\'s Full Name', ## one or the other (above); TODO: Standardize column names
                        entry$'Who is the supervising psychiatrist?',
                        entry$'What is the supervising level?',
                        entry$'Treatment #',
                        entry$'Planned Course')
          if (output != "")
              cat(output, file=output_file, fill=FALSE, append=TRUE)
          else
              cat(sprintf("Error in entry for patient, %i\n", patient), file=error_file, fill=FALSE, append=TRUE)
    }
    cat("</body>
</html>", file=output_file, fill=FALSE, append=TRUE)
}


cat(sprintf("Error log for %s:\n", Sys.time()), file=error_file, fill=FALSE, append=FALSE)
gs4_auth(email = "***REMOVED***")
## gs4_deauth()
## gs4_auth()
## Load data
## Remove, rename, and clean columns
## Remove entries with "xx" in the ID column
## Prevent case sensitivity in patient IDs by changing all IDs to lowercase
tech_raw_old_1 <- read_sheet("***REMOVED***", sheet="TMS Technician Data Input 2021 (3)")


tech_raw_old_2 <- read_sheet("***REMOVED***", sheet="TMS Technician Data Input 2021 1 (1)") 

tech_raw_old_3 <- read_sheet("***REMOVED***", sheet="TMS Technician Data Input Summer 2020") 

tech_raw_current <- read_sheet("***REMOVED***", sheet="Query")

tech_raw <- rbind(tech_raw_old_1, tech_raw_old_2, tech_raw_old_3, tech_raw_current) %>%
  unite(pt_id, c('What is the four letter patient ID? (First two letters of FIRST and LAST name)', 'What are the last two digits of the patient\'s cell phone number?'), sep="", remove=TRUE)

for (patient in args) {
  tech_filtered <- tech_raw %>% filter(tolower(pt_id) == tolower(patient))
    if (nrow(tech_filtered) > 0) {
        print(sprintf("Output sessions for %s", patient))
        generate_output(tech_filtered, patient)
    }
}

cat("End of error log.", file=error_file, fill=FALSE, append=TRUE)

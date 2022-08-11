## Edit the below lines of code accordingly


## Each item in "list_to_generate" is a list consisting of:
## 1. a single patient's ID capitalized as it should be shown on top of the
## report. Format: "AaBb11"
## 2. (optional) date of earliest survey and tech data to include. Format: "yyyy-mm-dd"
list_to_generate <- list(
  list("AaBb11")
)

## Setting "rounds" to TRUE will display a patient's demographics, session number, and planned course
## at the top of the report.
## Only do this if "Demographics" AND session numbers in the sheets are fully updated!
rounds <- TRUE

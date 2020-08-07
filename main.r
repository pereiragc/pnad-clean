## This code is responsible for (1) reading,  (2) cleaning and (3) combining
## PNAD data
## TODO: create an Rmarkdown file instead of this one

library(data.table)
library(readr)
library(glue)


## External data ---------------------------------------------------------------
source("external_data.r")
## Relevant imports:
## - uf_translate :: data.table with state names and codes (for later matching
##                   pnad)
## =============================================================================


## Source relevant functions for this project ----------------------------------
source("functions.r")
## =============================================================================


## Source my generic functions from `Rtoolkit` ---------------------------------
source("rtoolkit/R/base.r")
source("rtoolkit/R/datatable.r")
## =============================================================================


## -----------------------------------------------------------------------------
## USER PARAMETERS
dlpath <- "../PNAD_download"
param <- list(
  dlpath = dlpath,
  dict_filename=guessDictFilename(dlpath), # "Input_PNADC_trimestral.txt"
  startyear = 2012,
  startqtr = 1,
  endyear = 2020,
  endqtr = 1,
  state_varname="uf_name",
  export_directory = "./output",
  export_fullname = paste0("pnadc_treated_",
                           "{param$startyear}q{param$startqtr}-",
                           "{param$endyear}q{param$endqtr}.Rds")
)


## Create groups based on the following list
groups_specification <- list()


groups_specification[[1]] <- list(
  name = "educ_group",
  variable = "VD3004",
  groups = list("No Elementary" = c("1", "2"),
                "Elementary" = c("3", "4"),
                "High School" = c("5","6"),
                "Higher Educ" = c("7"))
)
groups_specification[[2]] <- list(
  name = "formality_status",
  variable = "VD4009",
  groups = list(formal = c("01", "03", "05"),
                informal = c("02", "04", "06"),
                self_employed = "")
)

cols.keep <- c("household", "person_id", "qid","qid_pretty","Ano", "Trimestre",
               "dbirth", "age", "UPA", "uf_name", "V1023", "V2003", "V2005",
               "V1028", "male", "V4041", "V2010", "head", "V20082", "VD3004", "VD3005",
               "VD4002", "VD4009", "VD4015", "VD4016", "VD4017", "VD4019",
               "VD4020", "V4010", "V4013", "V4019", "V4046", "VD4001", "VD4007",
               "VD4010", "VD4011", "VD4012", "formality_status", "educ_group",
               "labor_status")
##        ^  Note that some of the variables are created in the cleaning
##           process and do not show up in the documentation file.
## =============================================================================




## -----------------------------------------------------------------------------
## * Part 1: read data
##
## Assumptions.
## 1. The PNAD data (dictionary + quarter zip files) are downloaded to `param$dlpath`
## 2. The zip file names have the following naming convention:
##        `pnad_YYYY_qX.zip`
##    where `YYYY` is the four digit year and `X` is the quarter (ϵ {1,2,3,4})
## 3. 

pnad_dict_fname <- file.path(param$dlpath, guessDictFilename(param$dlpath))

dt_pnad_dict <- getColDict(pnad_dict_fname) # pnad variable names & description
                                            # important for reading pnad data
                                            # with proper column names

list_pnad <- pnadRead(dt_pnad_dict, param)  # read datasets
## =============================================================================


## -----------------------------------------------------------------------------
## * Part 2: Clean


## Below is the only function which I include here instead of `functions.r`.
## The reason being: this function is responsible for manipulating each
## quarter in `list_pnad`
##
## so anything you want to change in the cleaning process or add to the final
## datasets should be included here
clean_each <- function(dt_quarter, param) {
  ## Create legible state names

  cat(".")

    dt_quarter[uf_translate, (param$state_varname) := uf_name, on="UF"]
    ## `uf_translate` is imported from `external.r`


  ## Create columns
  dt_quarter[, `:=`(
    age  = V2009,
    male = V2007 == 1,
    head = V2005 == "01",
    qid  = interaction(Ano, Trimestre, lex.order=TRUE),
    household = interaction(UPA, V1008, V1014),
    dbirth = as.Date(paste(V20082, V20081, V2008, sep="-"), format="%Y-%m-%d")
  )]

  ## Create groups based on `groups_specification`
  lapply(groups_specification, function(lgroup) {
    setGroup(dt_quarter, lgroup$variable, lgroup$groups, lgroup$name)
  })



  ## `labor_status` group added by "brute force", since it is conditional and I didn't want to
  ## write generic code for this particular case
  ##
  ## Requires `formality_status` to be defined above.
  dt_quarter[VD4002=="2", labor_status := "not_employed"]
  dt_quarter[VD4002=="1", labor_status := as.character(formality_status)]
  dt_quarter[, labor_status := factor(labor_status,
                                     levels=c("not_employed", "formal", "informal"))]

  cat(".\n")
}


invisible(lapply(list_pnad, clean_each, param = param))
## =============================================================================


## -----------------------------------------------------------------------------
## * Part 3: Combine
dt_pnad <- rbindlist(list_pnad)
rm(list_pnad)
gc()
## =============================================================================

## -----------------------------------------------------------------------------
## * Part 4: Cleaning steps after combining


## Create "pretty (date-formatted) quarter variable"
## [note: maintain `qid` because having a factor is efficient
## for subsetting]
dt_pnad[, qid_pretty := qid]
prettyQuarter(dt_pnad, "qid_pretty")


## Order education levels
dt_pnad[, VD3004 := factor(VD3004, levels=sort(levels(VD3004)), ordered=TRUE)]
dt_pnad[, VD3005 := factor(VD3005, levels=sort(levels(VD3005)), ordered=TRUE)]

## Infer individual IDs from (hh, sex, dbirth) or (hh,sex,age)
pnadInferIndividualIDs(dt_pnad)


## * Part 5: Export

if (!dir.exists(param$export_directory)) {
  warning(glue("Specified export directory `{param$export_directory}`",
               "does not exist; creating it now."))
  dir.create(param$export_directory)
}

saveRDS(dt_pnad[, cols.keep, with = FALSE],
        file.path(param$export_directory, glue(param$export_fullname)))

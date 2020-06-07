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
  startyear = 2015,
  startqtr = 1,
  endyear = 2015,
  endqtr = 4,
  state_varname="uf_name"
  export_fname = ""
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
    dbirth = interaction(V20082, V20081, V2008)
  )]

  ## Create groups based on `groups_specification`
  lapply(groups_specification, function(lgroup) {
    set_group(dt_quarter, lgroup$variable, lgroup$groups, lgroup$name)
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


invisible(lapply(list_pnad, clean_each, param=param))


## * Part 3: Combine
dt_pnad <- rbindlist(list_pnad)
rm(list_pnad)
gc()



## * Part 4: Export derivative dataset
cols.keep <- c("household", "qid","Ano", "Trimestre", "dbirth", "age", "UPA",
               "uf_name", "V1023", "V1028", "male", "V2010", "head", "V20082",
               "VD3004", "VD4002", "VD4009", "VD4015", "VD4016", "VD4017",
               "VD4019", "VD4020", "V4010", "V4013", "V4019", "V4046", "VD4001",
               "VD4007", "VD4010", "VD4011", "VD4012", "formality_status",
               "educ_group", "labor_status")


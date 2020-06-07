
getColDict  <- function(dict.filename){
  input.parse <- read_delim(dict.filename, "\n", locale=locale(encoding="ISO-8859-1"), col_names=FALSE, progress=FALSE)$X1

  # Locate variable specification range in Dictionary string
  skip  <- min(grep("input", input.parse))+1
  final  <- max(grep("^;", input.parse)) - 1
  input.select  <- input.parse[skip:final]

  # Use regex to find column description/whether it's numeric/widths
  # (This part of the code is highly idiosyncratic to the dictionary file.)
  col.desc  <- gsub(".*/\\*[[:space:]]*(.*) *\\*/", "\\1",input.select)
  col.numeric  <- !grepl("\\$", input.select)
  input.widths  <- gsub("(.*)[[:space:]]*/\\*.*", "\\1", input.select)
  input.widths  <- gsub("[[:space:]]+", " ", input.widths)
  reasonable.format  <- gsub("\\@([0-9]+)[[:space:]]*([[:alpha:]].*?)[[:space:]]+\\$*([0-9]+)\\.*[[:space:]]*",
                             "\\1;\\2;\\3",
                             input.widths)

  coldict  <- setDT(transpose(strsplit(reasonable.format, ";")))
  coldict[, `:=`(V1 = as.integer(V1), V3=as.integer(V3), V4=col.desc, V5=col.numeric)]

  ## Sanity check: do widths and positions make sense taken together?
  coldict[, check := c(diff(V1), NA) == V3]
  if ( !coldict[, all(check, na.rm=TRUE)] ) {
    stop("Something went wrong reading PNAD column dictionary.")
  }
  coldict[, check := NULL]


  # Give meaningful names to `coldict`
  setnames(coldict, paste0("V", 1:5),
           c("Pos", "Name","Width", "Description", "IsNumeric"))
  return(coldict)
}



guessDictFilename <- function(dlpath) {
  def_fname <- toupper("Input_PNADC_trimestral.txt")
  txtfiles <- list.files(dlpath, pattern="txt$")

  if (length(txtfiles) == 0) {
    stop(glue("No candidate dictionaries in directory {dlpath}"))
  }



  idx_try_def <- which(def_fname == toupper(txtfiles))

  if (length(idx_try_def) > 0) {
    return(txtfiles[idx_try_def[1]])
  } else {

    chosen.txtfile <- txtfiles[1]
    warning(paste0("No reasonable candidate for dictionary filename found in ",
                   glue("'{dlpath}', using '{chosen.txtfile}'. Please make "),
                   "sure the correct dictionary is in the specified directory."))

    return(chosen.txtfile)
  }

}


pnadGetDownloaded <- function(pnad_data_path){
  ## Parse Data directory for downloaded dataset zip files

  grep.fmt  <- paste("pnad_.*\\.zip", sep="")
  all.downloaded  <- list.files(pnad_data_path, pattern=grep.fmt, full.names = TRUE)

  years  <- gsub(pattern=".*/pnad_(.{4})_.*", "\\1", all.downloaded)
  qtrs  <- gsub(pattern=".*/pnad_.{4}_q(..).*", "\\1", all.downloaded)

  DT <- data.table(
    filename = all.downloaded,
    yr = as.integer(years),
    qtr = as.integer(qtrs)
  )
  DT[, lname := paste(yr, qtr, sep="-")]
  return(DT)
}


pnadRead_single <- function(datazip, coldict){
  DT <- read_fwf(datazip, fwf_widths(coldict$Width, coldict$Name),
                 col_types = columnSpecification(coldict),
                 na = c("", "."))
  setDT(DT)


  ## cols.convert.numeric  <-  coldict[IsNumeric == TRUE, Name]
  ## for (col in cols.convert.numeric){
  ##   # cat("Doing ", col, "\n")
  ##   DT[, (col) := as.numeric(get(col))]
  ## }

  return(DT)

}


pnadRead_generic <- function(pnad.data.path, coldict,
                             startyear, startqtr, endyear, endqtr) {
  all.years <- pnadGetDownloaded(pnad.data.path)[
    lexicompare(yr, qtr, vals=c(startyear, startqtr), binop=`>=`) &
    lexicompare(yr, qtr, vals=c(endyear, endqtr), binop=`<=`)
  ]

  lfulldata  <- lapply(all.years[,filename], pnadRead_single,
                       coldict=coldict)
  names(lfulldata) <- all.years[, lname]

  return(lfulldata)

}


pnadRead <- function(coldict, param) {
  pnadRead_generic(param$dlpath, coldict,
                   param$startyear, param$startqtr,
                   param$endyear, param$endqtr)
}


columnSpecification <- function(coldict){
  ## Auxiliary function that allows us to use `coldict` to specify column types
  ## for `read_fwf`. The output of this function is passed as `col_types` for
  ## `read_fwf`.

  ## We operate with the following convention: whatever is flagged by the IBGE
  ## crew as numeric in the dictionary, we read as numeric. Everything else is
  ## read as a factor (may slow down the reading).

  col_numeric <- coldict[, IsNumeric]

  r <- lapply(col_numeric, function(c) {
    if (c) {
      col_number()
    } else {
      col_factor()
    }
  })

  names(r) <- coldict[,Name]

  col_types = do.call(cols, r)

  return(col_types)
}

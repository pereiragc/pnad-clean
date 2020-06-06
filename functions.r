

getcoldict  <- function(dict.filename){
  # dict.filename  <- paste(pnad.data.path, "Input_PNADC_trimestral.txt", sep="/")

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

  coldict  <- as.data.table(transpose(strsplit(reasonable.format, ";")))
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

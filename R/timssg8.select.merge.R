timssg8.select.merge <-
function(folder=getwd(), countries, student=c(), school, math.teacher, science.teacher) {
  
  # Remove leading and trailing whitespaces in var labes  
  if(!missing(student) & !is.null(student)) {
    student = gsub("^[[:space:]]+|[[:space:]]+$", "", student)
  }
  
  if(!missing(school)){
    school = gsub("^[[:space:]]+|[[:space:]]+$", "", school)
  }
  
  if(!missing(math.teacher)){
    math.teacher = gsub("^[[:space:]]+|[[:space:]]+$", "", math.teacher)
  }
  
  if(!missing(science.teacher)){
    science.teacher = gsub("^[[:space:]]+|[[:space:]]+$", "", science.teacher)
  }
  
  # No variables selected (error)
    
  
  if (missing(student) & missing(school) & missing(math.teacher) & missing(science.teacher)) {
    stop("no variables are selected")
  }
  
  if (!missing(math.teacher) && !missing(science.teacher)) 
    stop("not possible to merge science and math teacher data")

  
  # Look for datasets (student, home, school) including student-teacher linkage (bst)
  files.all <- lapply(c("bsg", "bcg", "bst", "btm", "bts"), function(x) list.files(folder, 
  full.names= TRUE, pattern=paste("^", x, ".*.sav$", sep=""), recursive=TRUE))
  
  # Name list for identification later, rather than using numbers
  names(files.all) <- unique(unlist(lapply(files.all, function(x) substr(x, nchar(x)-11, nchar(x)-9))))
    
  # Country abbrevation labels from existing file names (datasets)
  cntlab <- toupper(unique(unlist(lapply(files.all, function(x) substr(x, nchar(x)-8, nchar(x)-6))))) 
  
  # setdiff(cntlab, iea.country$ISO) needs be zero! all elements in data labels are in userguide
  
  # Countries in the datasets and userguide
  country.list <- iea.country[iea.country[["ISO"]] %in% intersect(iea.country[["ISO"]], cntlab), ]
  
  
  # If no countries are selected: seleect all
  if (missing(countries)) { 
    countries=cntlab
  }
  
  # If countries are entered numerically, change to ISO labels for file selection (next)
  if (is.numeric(countries)) {
    countries=country.list[country.list$Code %in% countries, "ISO"]
  }
  
  # Selected files for selected countries (1st/last time countries argument used)
  files.select <- lapply(files.all, function(y) sapply(countries, function(x) y[substr(y, 
  nchar(y)-8, nchar(y)-6)==tolower(x)])) 
  # no blanks, no home instrument, otherwise delete, see 4g function
  
  # Student achievement and background data, needed also if school is non-missing
  
  if (!missing(student) | !missing(school)) {
    
    if (!missing(student) & is.null(files.select[['bsg']])) {
      stop('cannot locate student data files')
    }
    
    suppressWarnings(suppressMessages(student.data <- do.call("rbind",             # Merge [[1]] student
    lapply(files.select[['bsg']], function(y) 
    read.spss(y, to.data.frame=T)
    [c("IDCNTRY", "IDSCHOOL", "IDCLASS", "IDSTUD", "JKREP",         # IDs/Weights/PVs
    "JKZONE", "HOUWGT", "SENWGT", "TOTWGT",      
    "BSMMAT01", "BSMMAT02", "BSMMAT03", "BSMMAT04", "BSMMAT05",                               
    "BSSSCI01", "BSSSCI02", "BSSSCI03", "BSSSCI04", "BSSSCI05", student)]))))    
  }
  
  
  
  # School data
  
  if (!missing(school)) {
    
    if (is.null(files.select[['bcg']])) {
      stop('cannot locate school data files')
    }
    
  
  suppressWarnings(suppressMessages(school.data <- do.call("rbind",                      # Merge [[2]] school
  lapply(files.select[['bcg']], function(y) 
  read.spss(y, to.data.frame=T)[c("IDCNTRY", "IDSCHOOL", "SCHWGT", school)]))))                                                                     # Selected
  
}
  
  
  # Teacher data
  
  if (!missing(math.teacher)) {
    
    if (is.null(files.select[['btm']]) | is.null(files.select[['bst']])) {
      stop('cannot locate teacher data files')
    }
    
    
    suppressWarnings(suppressMessages(teach.l <- do.call("rbind",                              
    lapply(files.select[['bst']], function(y) read.spss(y, to.data.frame=TRUE)))))
    
    suppressWarnings(suppressMessages(teach.i <-  do.call("rbind",                              
    lapply(files.select[['btm']], function(y) read.spss(y, to.data.frame=TRUE)[, c(
    "IDCNTRY", "IDTEALIN", math.teacher)]))))
  
    teacher.data <- merge(teach.l, teach.i, by=c("IDCNTRY", "IDTEALIN"))
  }
  
  if (!missing(science.teacher)) {
    
    if (is.null(files.select[['bts']]) | is.null(files.select[['bst']])) {
      stop('cannot locate teacher data files')
    }
    
    suppressWarnings(suppressMessages(teach.l <- do.call("rbind",                              
    lapply(files.select[['bst']], function(y) read.spss(y, to.data.frame=TRUE)))))
    
    suppressWarnings(suppressMessages(teach.i <-  do.call("rbind",                              
    lapply(files.select[['bts']], function(y) read.spss(y, to.data.frame=TRUE)[, c(
    "IDCNTRY", "IDTEALIN", science.teacher)]))))
    
    teacher.data <- merge(teach.l, teach.i, by=c("IDCNTRY", "IDTEALIN"))
  }
  
  
  
  # Merging data depending on existing datasets/arguments
  
  # Student data available
  
  if (!missing(student) & missing(school) & missing(math.teacher) & missing(science.teacher)) {
    timss.all <- student.data
  }
  
  if (!missing(student) & !missing(school) & missing(math.teacher) & missing(science.teacher)) {
    timss.all <- merge(student.data, school.data, by=c("IDCNTRY", "IDSCHOOL"), suffixes=c(".st", ".sc"))
  }
  

  
  if (!missing(student) & missing(school) & (!missing(math.teacher) | !missing(science.teacher))) {
    student.data <- merge(teacher.data, student.data, by=c("IDCNTRY", "IDSTUD"))
    student.data <- student.data[, -c(grep("*.y", names(student.data)))]
    names(student.data) <- gsub("*.x", "", names(student.data))
    timss.all <- student.data
  }  
  
  
  
  if (!missing(student) & !missing(school) & (!missing(math.teacher) | !missing(science.teacher))) {
    student.data <- merge(teacher.data, student.data, by=c("IDCNTRY", "IDSTUD"))
    student.data <- student.data[, -c(grep("*.y", names(student.data)))]
    names(student.data) <- gsub("*.x", "", names(student.data))
    timss.all <- merge(student.data, school.data, by=c("IDCNTRY", "IDSCHOOL"), suffixes=c(".st", ".sc"))
  }
  
  
    
  # School data available
  
  if (missing(student) & !missing(school) & missing(math.teacher) & missing(science.teacher)) {
    timss.all <- merge(student.data, school.data, by=c("IDCNTRY", "IDSCHOOL"), suffixes=c(".st", ".sc"))
  }
  

  if (missing(student) & !missing(school) & (!missing(math.teacher) | !missing(science.teacher))) {
    student.data <- merge(teacher.data, student.data, by=c("IDCNTRY", "IDSTUD"))
    student.data <- student.data[, -c(grep("*.y", names(student.data)))]
    names(student.data) <- gsub("*.x", "", names(student.data))
    timss.all <- merge(student.data, school.data, by=c("IDCNTRY", "IDSCHOOL"), suffixes=c(".st", ".sc"))
  }


  # Teacher data available
  
  if (missing(student) & missing(school) & (!missing(math.teacher) | !missing(science.teacher))) {
  timss.all <- teacher.data
  }

  
  
  # Create country label variable (not possible to add labels to numeric factor, see to do list)
  timss.all$IDCNTRYL <- factor(timss.all$IDCNTRY,  
                               levels=country.list[country.list$Code %in% unique(timss.all$IDCNTRY), "Code"] ,        
                               labels= country.list[country.list$Code %in% unique(timss.all$IDCNTRY), "Country"])
  
  # table(student.data$IDCNTRYL, student.data$IDCNTRY) test of equality!
  
  
  return(timss.all)
}

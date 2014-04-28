timssg4.select.merge <-
function(folder=getwd(), countries, student=c(), home, school, teacher) {
  
  # Remove leading and trailing whitespaces in var labes  
  
  if(!missing(student) & !is.null(student)) {
    student = gsub("^[[:space:]]+|[[:space:]]+$", "", student)
  }
  
  if(!missing(home)){
    home = gsub("^[[:space:]]+|[[:space:]]+$", "", home)
  }
  
  if(!missing(school)){
    school = gsub("^[[:space:]]+|[[:space:]]+$", "", school)
  }
  
  if(!missing(teacher)){
    teacher = gsub("^[[:space:]]+|[[:space:]]+$", "", teacher)
  }
  
  
  # No variables selected (error)
  if (missing(student) & missing(home) & missing(school) & missing(teacher)) {
    stop("no variables are selected")
  }
  
  # Look for datasets (student, home, school) including student-teacher linkage (ast)
  files.all <- lapply(c("asg", "ash", "acg", "ast", "atg"), function(x) list.files(folder, 
               full.names= T, pattern=paste("^", x, ".*.sav$", sep=""), recursive=T))
  
  
  # Name list for identification later, rather than using numbers
  names(files.all) <- unique(unlist(lapply(files.all, function(x) substr(x, nchar(x)-11, nchar(x)-9))))
  
  # Country abbrevation labels from existing file names (datasets)
  cntlab <- toupper(unique(unlist(lapply(files.all, function(x) substr(x, nchar(x)-8, nchar(x)-6))))) 
  
  # setdiff(cntlab, iea.country$ISO) needs be zero! all elements in data labels are in userguide
  
  # Countries in the datasets and userguide
  country.list <- iea.country[iea.country[["ISO"]] %in% intersect(iea.country[["ISO"]], cntlab), ]
  
  
  # If no countries are selected: select all
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
  
  # Remove cases for no home instruments
  files.select[['ash']] <- files.select[['ash']][lapply(files.select[['ash']], length)!=0]
  
  # Student achievement and background data
  
  
  # Student achievement and background data, needed also if home is non-missing
  
  if (!missing(student) | !missing(home) | !missing(school)) {
    
    if (!missing(student) & is.null(files.select[['asg']])) {
      stop('cannot locate student data files')
    }
    
    suppressWarnings(suppressMessages(student.data <- do.call("rbind",       # Merge [[1]] student
    lapply(files.select[['asg']], function(y) 
    read.spss(y, to.data.frame=T)
    [c("IDCNTRY", "IDSCHOOL", "IDCLASS", "IDSTUD", "JKREP",         # IDs/Weights/PVs
    "JKZONE", "HOUWGT", "SENWGT", "TOTWGT",      
    "ASMMAT01", "ASMMAT02", "ASMMAT03", "ASMMAT04", "ASMMAT05",                               
    "ASSSCI01", "ASSSCI02", "ASSSCI03", "ASSSCI04", "ASSSCI05", student)]))))    
  }
  
  
  
  # Home background data
  if (!missing(home)) {
    
    if (is.null(files.select[['ash']])) {
      stop('cannot locate home background data files')
    }
    
   suppressWarnings(suppressMessages(home.data <- do.call("rbind",                                      # Merge [[2]] home
   lapply(files.select[['ash']], function(y) read.spss(y, to.data.frame=TRUE)[, c(                      # Each dataset
   "IDCNTRY", "IDSTUD", home)]))))                                                   # Selected
  }
  
  
  
  # School data
  
  if (!missing(school)) {
    
    if (is.null(files.select[['acg']])) {
      stop('cannot locate school data files')
    }
    
    suppressWarnings(suppressMessages(school.data <- do.call("rbind",                                                 # Merge [[3]] school
    lapply(files.select[['acg']], function(y) read.spss(y, to.data.frame=TRUE)[,c(              # Each dataset
    "IDCNTRY", "IDSCHOOL", "SCHWGT", school)]))))                                                                     # Selected
  }
  

  
  # Teacher data
  if (!missing(teacher)) {
    
    if (is.null(files.select[['atg']]) | is.null(files.select[['ast']])) {
      stop('cannot locate teacher data files')
    }
    
    suppressWarnings(suppressMessages(teach.l <- do.call("rbind",                              
    lapply(files.select[['ast']], function(y) read.spss(y, to.data.frame=TRUE)))))
    
    suppressWarnings(suppressMessages(teach.i <-  do.call("rbind",                              
    lapply(files.select[['atg']], function(y) read.spss(y, to.data.frame=TRUE)[, c(
    "IDCNTRY", "IDTEALIN", teacher)]))))
    
    teacher.data <- merge(teach.l, teach.i, by=c("IDCNTRY", "IDTEALIN"))
  }
  
  
  
  # Merging data depending on existing datasets/arguments
  
  # Student data available
  
  if (!missing(student) & missing(home) & missing(school) & missing(teacher)) {
    timss.all <- student.data
  }
  
  if (!missing(student) & !missing(home) & missing(school) & missing(teacher)) {
    timss.all <- merge(student.data, home.data, all.x=TRUE, by=c("IDCNTRY", "IDSTUD"), suffixes=c(".st", ".hm"))
  }
  
  if (!missing(student) & missing(home) & !missing(school) & missing(teacher)) {
    timss.all <- merge(student.data, school.data, by=c("IDCNTRY", "IDSCHOOL"), suffixes=c(".st", ".sc"))
  }
  
  
  if (!missing(student) & missing(home) & missing(school) & !missing(teacher)) {
    student.data <- merge(teacher.data, student.data, by=c("IDCNTRY", "IDSTUD"))
    student.data <- student.data[, -c(grep("*.y", names(student.data)))]
    names(student.data) <- gsub("*.x", "", names(student.data))
    timss.all <- student.data
  }  
  
  
  
  if (!missing(student) & !missing(home) & !missing(school) & missing(teacher)) {
    student.data <- merge(student.data, home.data, by=c("IDCNTRY", "IDSTUD"), all.x=TRUE, suffixes=c(".st", ".hm"))
    timss.all <- merge(student.data, school.data, by=c("IDCNTRY", "IDSCHOOL"), suffixes=c(".st", ".sc"))
  }
  
  
  if (!missing(student) & missing(home) & !missing(school) & !missing(teacher)) {
    student.data <- merge(teacher.data, student.data, by=c("IDCNTRY", "IDSTUD"))
    student.data <- student.data[, -c(grep("*.y", names(student.data)))]
    names(student.data) <- gsub("*.x", "", names(student.data))
    timss.all <- merge(student.data, school.data, by=c("IDCNTRY", "IDSCHOOL"), suffixes=c(".st", ".sc") )
  }
  
  if (!missing(student) & !missing(home) & missing(school) & !missing(teacher)) {
    student.data <- merge(student.data, home.data, by=c("IDCNTRY", "IDSTUD"), all.x=TRUE, suffixes=c(".st", ".hm"))
    student.data <- merge(teacher.data, student.data, by=c("IDCNTRY", "IDSTUD"))
    student.data <- student.data[, -c(grep("*.y", names(student.data)))]
    names(student.data) <- gsub("*.x", "", names(student.data))
    timss.all <- student.data
  }
  
  
  if (!missing(student) & !missing(home) & !missing(school) & !missing(teacher)) {
    student.data <- merge(student.data, home.data, by=c("IDCNTRY", "IDSTUD"), all.x=TRUE, suffixes=c(".st", ".hm"))
    student.data <- merge(teacher.data, student.data, by=c("IDCNTRY", "IDSTUD"))
    student.data <- student.data[, -c(grep("*.y", names(student.data)))]
    names(student.data) <- gsub("*.x", "", names(student.data))
    timss.all <- merge(student.data, school.data, by=c("IDCNTRY", "IDSCHOOL"), suffixes=c(".st", ".sc") )
  }
  
  
  
  # Home data available
  
  if (missing(student) & !missing(home) & missing(school) & missing(teacher)) {
    timss.all <- merge(student.data, home.data, by=c("IDCNTRY", "IDSTUD"), all.x=TRUE, suffixes=c(".st", ".hm"))
  }
  
  
  if (missing(student) & !missing(home) & !missing(school) & missing(teacher)) {
    student.data <- merge(student.data, home.data, by=c("IDCNTRY", "IDSTUD"), all.x=TRUE, suffixes=c(".st", ".hm"))
    timss.all <- merge(student.data, school.data, by=c("IDCNTRY", "IDSCHOOL"), suffixes=c(".st", ".sc"))
  }
  
  
  if (missing(student) & !missing(home) & missing(school) & !missing(teacher)) {
    student.data <- merge(student.data, home.data, by=c("IDCNTRY", "IDSTUD"), all.x=TRUE, suffixes=c(".st", ".hm"))
    student.data <- merge(teacher.data, student.data, by=c("IDCNTRY", "IDSTUD"))
    student.data <- student.data[, -c(grep("*.y", names(student.data)))]
    names(student.data) <- gsub("*.x", "", names(student.data))
    timss.all <- student.data
  }
  
  if (missing(student) & !missing(home) & !missing(school) & !missing(teacher)) {
    student.data <- merge(student.data, home.data, by=c("IDCNTRY", "IDSTUD"), all.x=TRUE, suffixes=c(".st", ".hm"))
    student.data <- merge(teacher.data, student.data, by=c("IDCNTRY", "IDSTUD"))
    student.data <- student.data[, -c(grep("*.y", names(student.data)))]
    names(student.data) <- gsub("*.x", "", names(student.data))
    timss.all <- merge(student.data, school.data, by=c("IDCNTRY", "IDSCHOOL"), suffixes=c(".st", ".sc"))
  }
  
  
  
  # School data available
  
  if (missing(student) & missing(home) & !missing(school) & missing(teacher)) {
    timss.all <- merge(student.data, school.data, by=c("IDCNTRY", "IDSCHOOL"), suffixes=c(".st", ".sc") )
  }
  
  
  if (missing(student) & missing(home) & !missing(school) & !missing(teacher)) {
    timss.all <- merge(teacher.data, school.data, by=c("IDCNTRY", "IDSCHOOL"), suffixes=c(".st", ".sc") )
  }
  
  # Teacher data available
  
  if (missing(student) & missing(home) & missing(school) & !missing(teacher)) {
    timss.all <- teacher.data
  }
  
  
  
  # Create country label variable (not possible to add labels to numeric factor, see to do list)
  timss.all$IDCNTRYL <- factor(timss.all$IDCNTRY,  
                       levels=country.list[country.list$Code %in% unique(timss.all$IDCNTRY), "Code"] ,        
                       labels= country.list[country.list$Code %in% unique(timss.all$IDCNTRY), "Country"])
  
  # table(student.data$IDCNTRYL, student.data$IDCNTRY) test of equality!
  
  
  return(timss.all)
  
}

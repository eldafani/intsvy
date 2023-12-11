pisa.var.label <- 
function(folder=getwd(), student.file, parent.file=c(), school.file=c(), name="Variable labels", output=getwd()) {
  
  # Student file required for country labels
  if(missing(student.file)) {
    stop("the student file is required")
  }
  
  if(!missing(folder)) {
    student.file =  file.path(folder, paste(student.file, sep=""))
  }
    
  if(!missing(folder) & !missing(school.file)) {
    school.file =   file.path(folder, paste(school.file, sep=""))
  }

  if(!missing(folder) & !missing(parent.file)) {
    parent.file =  file.path(folder, paste(parent.file, sep=""))
  }
  
  # Retrieve file name
  files.all <- list(student.file, parent.file, school.file)
  names(files.all) <- c('Student', 'Parent', 'School')
  
  # Remove null elements in list
  files.all <- files.all[lapply(files.all, length)>0]
  
  # Retrieve var labels
  
  suppressWarnings({
    file.label <- lapply(files.all, function(x) spss.system.file(x[[1]], to.lower=FALSE))
    var.label <- lapply(file.label, function(x) description(x))
    country <- table(file.label[["Student"]]$CNT)
  })  

  # Read student file and participating countries 
  var.label[[length(files.all)+1]] <- names(country)
  names(var.label)[length(var.label)] <-"Country abbreviations"
  
  # Print labels in list and text file
  capture.output(var.label, file=file.path(output, paste0(name, ".txt")))
  cat('The file "', paste(name, ".txt", sep=""), '" in directory "', output, '" contains the variable labels of the complete dataset', sep=' ', "\n")
  return(var.label)
}

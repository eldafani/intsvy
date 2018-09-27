intsvy.var.label <-
function(folder=getwd(), name="Variable labels", output=getwd(),
         config) {
  if (missing(config)) {
    stop("You should set the configuration object.")
  }
  
  if (config$input$type %in% c("IEA", "OECD")) {
    # data from IEA studies, many small files, different groups
    # data from PIAAC  many small files, one group
  
    # Looks for files (student, home, school, teacher), not student-teacher linkage
    files.all <- lapply(config$input$prefixes, function(x) list.files(folder, 
                   full.names= TRUE, pattern=paste0("^", x,"|", toupper(x), ".*.sav$"), 
                   recursive=TRUE))
    
    if (all(sapply(files.all, length)==0)){
      stop(paste("cannot locate the original `sav` files in", folder))
    }
    
    # Remove empty elements in list
    files.all <- files.all[lapply(files.all, length)>0]
    
    # Files char found in the datasets
    myabv <- lapply(files.all, function(x)  
      substr(x, nchar(x) + config$input$type_part[1], nchar(x) + config$input$type_part[2]))
    
    # include only file names with expected abvs (remove test, for example)
    files.all <- lapply(seq_along(files.all), function(y) files.all[[y]][myabv[[y]] 
                  %in% c(config$input$prefixes, toupper(config$input$prefixes))])
    
    abv <- unique(unlist(lapply(myabv, function(x) x[x  %in% c(config$input$prefixes, toupper(config$input$prefixes))])))
    
    # Name list for existing datasets, will print student-teacher linkage if available
    names(files.all) <- file.names[match(toupper(abv), toupper(file.names[["Abv"]])), "Instrument"]
    
    # Remove null elements (e.g. no teacher datasets)
    #files.all <- files.all[!is.na(names(files.all))]
    files.all<- files.all[lapply(files.all, length)!=0]
    
    # Country abbreviation in datasets
    cntlab <- toupper(unique(unlist(lapply(files.all, function(x) 
      substr(x, nchar(x) + config$input$cnt_part[1], nchar(x) + config$input$cnt_part[2]))))) 
    
    # setdiff(cntlab, iea.country$ISO) needs be zero! all elements in data labels are in userguide
    
    # Countries in the datasets and userguide
    #country.list <- iea.country[iea.country[["ISO"]] %in% intersect(iea.country[["ISO"]], cntlab), ]
    #rownames(country.list)<-NULL # remove subset rownames
    
    # Variable labels
    suppressWarnings(var.label <- lapply(files.all, function(x) description(spss.system.file(x[[1]], to.lower=FALSE))))
    # Country labels
    var.label[[length(files.all)+1]] <- cntlab
    names(var.label)[length(var.label)] <- "Country abbreviations"
    
    # Print labels in list and text file
    capture.output(var.label, file=file.path(output, paste0(name, ".txt")))
    cat('The file "', paste(name, ".txt", sep=""), '" in directory "', output, '" contains the variable labels of the complete dataset', sep=' ', "\n")
    return(var.label)
  }
}
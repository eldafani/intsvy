### Print outcome

print.intsvy.reg <- function(x) {
  if (all(unlist(lapply(x, is.list)))) { 
    out <- lapply(x, function(y) round(y$reg, 2))
  } else {
    out <- round(x$reg, 2)
  }
  print(out)
}

### Summary function

summary.intsvy.reg <- function(x) {
  if (all(unlist(lapply(x, is.list)))) {
    out <- lapply(x, function(y) y$reg)
  } else {
    out <- x$reg  
  }
  return(out)
}
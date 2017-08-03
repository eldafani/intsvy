timssg8.select.merge <-
function(folder=getwd(), countries, student=c(), school, math.teacher, science.teacher) {
  
  if (!missing(math.teacher) && !missing(science.teacher)) 
    stop("not possible to merge science and math teacher data")
  
  if (!missing(math.teacher)) {
    timss8_conf$input$teacher[2] <- "btm"
    intsvy.select.merge(folder=folder, countries=countries, student=student, school=school, teacher = math.teacher, config=timss8_conf)
  } else {
    timss8_conf$input$teacher[2] <- "bts"
    intsvy.select.merge(folder=folder, countries=countries, student=student, school=school, teacher = science.teacher, config=timss8_conf)
  }

}

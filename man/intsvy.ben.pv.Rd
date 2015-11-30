\name{intsvy.ben.pv}
\alias{intsvy.ben.pv}
\title{Performance international benchmarks and proficiency levels}
\description{
intsvy.ben.pv calculates the percentage of students performing at or above the cut-off points (scores) given by the useR. The default are the benchmarks established by official reports.
}
\usage{
intsvy.ben.pv(pvlabel,  by, cutoff, data, export = FALSE, name = "output", 
  folder = getwd(), config)
}

\arguments{
  \item{pvlabel}{
The label corresponding to the achievement variable, for example, "ASRREA", for overall reading performance.
}
  \item{cutoff}{
The cut-off points for the assessment benchmarks (e.g., cutoff= c(357.77, 420.07, 482.38, 544.68, 606.99, 669.30)). }
  \item{by}{
The label for the grouping variable, usually the countries (i.e., by="IDCNTRYL"), but could be any other categorical variable. 	
}
  \item{data}{
An R object, normally a data frame, containing the data from PIRLS.
}
  \item{export}{
A logical value. If TRUE, the output is exported to a file in comma-separated value format (.csv) that can be opened from LibreOffice or Excel. 
}
  \item{name}{
The name of the exported file.
}
  \item{folder}{
The folder where the exported file is located.
}
  \item{config}{
Object with configuration of a given study. Should contain the slot `prefixes` with prefixes of filenames with the student, home, school, and teacher data.
}
}

\value{
pirls.ben.pv returns a data frame with the percentage of students at or above the benchmark and the corresponding standard error.
}

\seealso{
timss.ben.pv, pirls.ben.pv, pisa.ben.pv
}
\examples{
\dontrun{
pisa.ben.pv(pvlabel="MATH", by="CNT", data=student2012)
intsvy.ben.pv(pvlabel="MATH", by="CNT", data=student2012, config=pisa_conf)

piaac.ben.pv(pvlabel="LIT", by="CNTRYID", data=piaac)
intsvy.ben.pv(pvlabel="LIT", by="CNTRYID", data=piaac, config=piaac_conf)

timss.ben.pv(pvlabel="ASMMAT", by="IDCNTRYL", data=timss4)
intsvy.ben.pv(pvlabel="ASMMAT", by="IDCNTRYL", data=timss4, config=timss4_conf)
}
}
\name{intsvy.config}
\alias{intsvy.config}
\title{Config files for intsvy studies}
\description{
intsvy.config set non standard parameters for intsvy functions. It allso allo to apply intsvy functions to new studies that are similar to PIRLS, TIMSS, PISA, PIAAC, ICILS.
}
\usage{
intsvy.config(variables.pvlabelpref,
           variables.pvlabelsuff,
           variables.weight,
           variables.jackknifeZone,
           variables.jackknifeRep,
           parameters.cutoffs,
           parameters.cutoffs2,
           parameters.percentiles,
           parameters.weights,
           parameters.PVreps,
           parameters.varpv1,
           input.type,
           input.prefixes,
           input.student,
           input.student_colnames1,
           input.student_colnames2,
           input.student_pattern,
           input.homeinput,
           input.home_colnames,
           input.school,
           input.school_colnames,
           input.teacher,
           input.teacher_colnames,
           input.student_ids,
           input.school_ids,
           input.type_part,
           input.cnt_part, base.config = pirls_conf)
}

\arguments{
  \item{parameters.weights}{
Weighting scheme. It may be "JK" for studies like PIRLS, ICLS, TIMSS, or "BRR" for studies like PISA or "mixed_piaac" for studies with mixed design like PIAAC.
}
  \item{parameters.cutoffs2, parameters.cutoffs}{
Cut offs for plausible values, either for benchmar or for logistic regression.
}
  \item{parameters.percentiles, parameters.PVreps}{
Other parameters for weighting schemes, like number of PVs. 	
}
  \item{parameters.varpv1}{
Logical value, TRUE if only 1 plausible value for within variance estimation.
}
  \item{variables.pvlabelpref, variables.pvlabelsuff, variables.weight, 
           variables.jackknifeZone, variables.jackknifeRep}{
Names of variables that are used for jack-knife replicates.
}
  \item{input.type,
           input.prefixes,
           input.student,
           input.student_colnames1,
           input.student_colnames2,
           input.student_pattern,
           input.homeinput,
           input.home_colnames,
           input.school,
           input.school_colnames,
           input.teacher,
           input.teacher_colnames,
           input.student_ids,
           input.school_ids,
           input.type_part,
           input.cnt_part}{
Parameters to correctly read data from files downloaded from iea.nl website. 
}
  \item{base.config}{
Base config structure, either pirls_conf, pisa_conf, piaac_conf, timss4_conf, timss8_conf, icils_conf.
}
}

\value{
intsvy.config returns new object with parameters. It is a list with three components - input, variables and parameters.
}

\examples{
\dontrun{
icils_conf <- intsvy.config(input.student_pattern = "^PV[0-5]CIL$" , 
                            parameters.cutoffs2 = 550, intsvy:::pirls_conf)
icils_conf
}}

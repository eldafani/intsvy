#
# Configurations for different studies
# 

# For TIMMS
timms_conf <- list(variables = list(pvlabelpref="BSMMAT", 
                                    pvlabelsuff = "",
                                    weight="TOTWGT",
                                    p1 = "JKZONE",
                                    p2 = "JKREP"),
                   parameters = list(cutoffs5 = c(400, 475, 550, 625),
                                     cutoffs2 = c(550),
                                     percentiles = c(5, 10, 25, 75, 90, 95),
                                     PVreps = 5),
                   input = list(non_student_teacher = filenames=c("asg", "ash", "acg", "atg"),
                                student_teacher = c("bsg", "bcg", "bst", "btm", "bts"))
                   )

# For PISA
pisa_conf <- list(variables = list(pvlabelpref = "PV",
                                  pvlabelsuff = "READ",
                                  weight    = "W_FSTUWT",
                                  BRRweight = "W_FSTR"),
                 parameters = list(cutoffs7 = c(357.77, 420.07, 482.38, 544.68, 606.99, 669.30),
                                   cutoffs2 = c(606.99),
                                   percentiles = c(5, 10, 25, 75, 90, 95),
                                   PVreps = 5,
                                   BRRreps = 80,
                                   replication_scheme = 'pisa')
)



# skeleton for new configs
new_conf <- list(variables = list(pvlabelpref = "",
                                  pvlabelsuff = "",
                                  weight=""),
                 parameters = list(cutoffs5 = c(),
                                   cutoffs2 = c(),
                                   percentiles = c(),
                                   PVreps = 5)
                 )


#
# Configurations for different studies
# 

# For PISA
pisa_conf <- list(variables = list(pvlabelpref = "PV",
                                  pvlabelsuff = "READ",
                                  weight    = "W_FSTUWT",
                                  BRRweight = "W_FSTR"),
                 parameters = list(cutoffs = c(357.77, 420.07, 482.38, 544.68, 606.99, 669.30),
                                   cutoffs2 = c(606.99),
                                   percentiles = c(5, 10, 25, 75, 90, 95),
                                   PVreps = 5,
                                   BRRreps = 80,
                                   replication_scheme = 'pisa')
)

# For PIAAC
piaac_conf <- list(variables = list(pvlabelpref = "PV",
                                  pvlabelsuff = "",
                                  weight    = "SPFWT0",
                                  BRRweight = "SPFWT",
                                  countryID = "CNTRYID"),
                 parameters = list(cutoffs = c(175.99, 225.99, 275.99, 325.99, 375.99),
                                   cutoffs2 = c(),
                                   percentiles = c(5, 10, 25, 75, 90, 95),
                                   PVreps = 10,
                                   BRRreps = 80,
                                   replication_scheme = 'piaac')
)

# For TIMMS
timms_conf <- list(variables = list(pvlabelpref="BSMMAT", 
                                    pvlabelsuff = "",
                                    weight="TOTWGT",
                                    jackknifeZone = "JKZONE",
                                    jackknifeRep = "JKREP"),
                   parameters = list(cutoffs = c(400, 475, 550, 625),
                                     cutoffs2 = c(550),
                                     percentiles = c(5, 10, 25, 75, 90, 95),
                                     PVreps = 5),
                   input = list(non_student_teacher = filenames=c("asg", "ash", "acg", "atg"),
                                student_teacher = c("bsg", "bcg", "bst", "btm", "bts"))
)

# For PIRLS
pirls_conf <- list(variables = list(pvlabelpref = "ASRREA0",
                                  pvlabelsuff = "",
                                  weight="TOTWGT",
                                  jackknifeZone = "JKZONE",
                                  jackknifeRep = "JKREP"),
                 parameters = list(cutoffs = c(400, 475, 550, 625),
                                   cutoffs2 = c(550),
                                   percentiles = c(5, 10, 25, 75, 90, 95),
                                   PVreps = 5),
                 input = list(non_student_teacher = filenames=c("asg", "ash", "acg", "atg"),
                              student_teacher = c("bsg", "bcg", "bst", "btm", "bts"))
)




# skeleton for new configs
new_conf <- list(variables = list(pvlabelpref = "",
                                  pvlabelsuff = "",
                                  weight=""),
                 parameters = list(cutoffs = c(),
                                   cutoffs2 = c(),
                                   percentiles = c(),
                                   PVreps = 5)
                 )



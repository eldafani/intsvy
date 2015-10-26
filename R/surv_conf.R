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
# http://vs-web-fs-1.oecd.org/piaac/puf-data/SPSS/
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
                                   replication_scheme = 'piaac'),
                 input = list(type = "OECD",
                              prefixes = "prg",
                              type_part = c(-11, -9),
                              cnt_part = c(-8, -6))
)

# For TIMMS
timms4_conf <- list(variables = list(pvlabelpref="BSMMAT", 
                                    pvlabelsuff = "",
                                    weight="TOTWGT",
                                    jackknifeZone = "JKZONE",
                                    jackknifeRep = "JKREP"),
                   parameters = list(cutoffs = c(400, 475, 550, 625),
                                     cutoffs2 = c(550),
                                     percentiles = c(5, 10, 25, 75, 90, 95),
                                     PVreps = 5),
                   input = list(type = "IEA",
                                prefixes = c("asg", "ash", "acg", "atg"),
                                type_part = c(-11, -9),
                                cnt_part = c(-8, -6))
)
timms8_conf <- list(variables = list(pvlabelpref="BSMMAT", 
                                     pvlabelsuff = "",
                                     weight="TOTWGT",
                                     jackknifeZone = "JKZONE",
                                     jackknifeRep = "JKREP"),
                    parameters = list(cutoffs = c(400, 475, 550, 625),
                                      cutoffs2 = c(550),
                                      percentiles = c(5, 10, 25, 75, 90, 95),
                                      PVreps = 5),
                    input = list(type = "IEA",
                                 prefixes = c("bsg", "bcg", "bst", "btm", "bts"),
                                 type_part = c(-11, -9),
                                 cnt_part = c(-8, -6))
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
                 input = list(type = "IEA",
                              prefixes = c("asg", "ash", "acg", "atg"),
                              type_part = c(-11, -9),
                              cnt_part = c(-8, -6))
)

# ICILS
icils_conf <- list(variables = list(pvlabelpref="BSMMAT", 
                                     pvlabelsuff = "",
                                     weight="TOTWGT",
                                     jackknifeZone = "JKZONE",
                                     jackknifeRep = "JKREP"),
                    parameters = list(cutoffs = c(400, 475, 550, 625),
                                      cutoffs2 = c(550),
                                      percentiles = c(5, 10, 25, 75, 90, 95),
                                      PVreps = 5),
                    input = list(type = "IEA",
                                 prefixes = c("bsg", "bcg", "bst", "btm", "bts"),
                                 type_part = c(-11, -9),
                                 cnt_part = c(-8, -6))
)


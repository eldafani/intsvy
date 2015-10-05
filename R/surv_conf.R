#
# Configurations for different studies
# 

timms_conf <- list(variables = list(pvlabel="BSMMAT", 
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



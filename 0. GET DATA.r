
assign(".lib.loc", c(.libPaths(), "path_omitted"), envir = environment(.libPaths))

library(RODBC)

setwd("path_omitted")

dbhandle <- odbcDriverConnect("path_omitted")

cin <- sqlQuery(dbhandle, "SELECT * FROM CIN_2009_to_2019")
save(cin, file = "raw/cin.rda")

cin_disabilities <- sqlQuery(dbhandle, "SELECT * FROM CIN_2009_to_2019_Disabilities")
save(cin_disabilities, file = "raw/cin_disabilities.rda")

cin_age <- sqlQuery(dbhandle, "SELECT * FROM CIN_UCL_DR200617_01_Supplied")
save(cin_age, file = "raw/cin_age.rda")

rm(list = ls())
gc()

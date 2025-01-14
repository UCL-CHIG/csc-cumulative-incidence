
# load --------------------------------------------------------------------

lib_base <- "path_omitted"
assign(".lib.loc", lib_base, envir = environment(.libPaths))

# This enables parallel runs
Sys.setenv("R_LIBS_USER" = lib_base)
rm(lib_base)

setwd("path_omitted")
library(data.table)

load("processed/cin_processed.rda")

cin_pmr <- cin[age_yrs_int < 5 | (age_yrs_int >= 5 & id_type == "pupilmatchingrefanonymous")]

cin_pmr <- cin_pmr[cinreferraldate_clean > as.Date("2011-03-31")]
cin_pmr <- cin_pmr[order(id_combined, cinreferraldate_clean)]
cin_pmr[, row_per_child := seq_len(.N), by = rleid(id_combined)]
cin_pmr[, epi_index := frank(cinreferraldate_clean, ties.method = "dense"), by = id_combined]

# Part 1: OBSERVED CUMULATIVE ---------------------------------------------

# 2012/2013
ref_birth_2013 <- cin_pmr[birthfyear == 2013] # get referrals for children born in 2011/2012
ref_birth_2013 <- ref_birth_2013[row_per_child == 1] # count children only once
ref_birth_2013 <- ref_birth_2013[age_yrs_int <= 5]
table(ref_birth_2013$age_yrs_int)

ax_birth_2013 <- cin_pmr[birthfyear == 2013 & referralnfa == 0]
ax_birth_2013[, row_per_child := seq_len(.N), by = rleid(id_combined)]
ax_birth_2013 <- ax_birth_2013[row_per_child == 1]
ax_birth_2013 <- ax_birth_2013[age_yrs_int <= 5]
table(ax_birth_2013$age_yrs_int)

cin_birth_2013 <- cin_pmr[birthfyear == 2013 & referralnfa == 0 & (reasonforclosure != "RC8" | is.na(reasonforclosure))]
cin_birth_2013[, row_per_child := seq_len(.N), by = rleid(id_combined)]
cin_birth_2013 <- cin_birth_2013[row_per_child == 1]
cin_birth_2013 <- cin_birth_2013[age_yrs_int <= 5]
table(cin_birth_2013$age_yrs_int)

cpp_birth_2013 <- cin_pmr[birthfyear == 2013 & !is.na(cppstartdate)]
cpp_birth_2013[, row_per_child := seq_len(.N), by = rleid(id_combined)]
cpp_birth_2013 <- cpp_birth_2013[row_per_child == 1]
cpp_birth_2013 <- cpp_birth_2013[age_yrs_int <= 5]
table(cpp_birth_2013$age_yrs_int)

output <- cbind(table(ref_birth_2013$age_yrs_int),
                table(ax_birth_2013$age_yrs_int),
                table(cin_birth_2013$age_yrs_int),
                table(cpp_birth_2013$age_yrs_int))
colnames(output) <- c("referrals", "assessed", "need", "cpp")
write.csv(output, file = "outputs/cin/main/birth_2013_main.csv")
rm(output)

# 2005/2006
ref_birth_2006 <- cin_pmr[birthfyear == 2006]
ref_birth_2006 <- ref_birth_2006[order(id_combined, cinreferraldate_clean)]
ref_birth_2006[, row_per_child := seq_len(.N), by = rleid(id_combined)]
ref_birth_2006 <- ref_birth_2006[row_per_child == 1]
ref_birth_2006 <- ref_birth_2006[age_yrs_int > 5 & age_yrs_int <= 12]
table(ref_birth_2006$age_yrs_int)

ax_birth_2006 <- cin_pmr[birthfyear == 2006 & referralnfa == 0]
ax_birth_2006 <- ax_birth_2006[order(id_combined, cinreferraldate_clean)]
ax_birth_2006[, row_per_child := seq_len(.N), by = rleid(id_combined)]
ax_birth_2006 <- ax_birth_2006[row_per_child == 1]
ax_birth_2006 <- ax_birth_2006[age_yrs_int > 5 & age_yrs_int <= 12]
table(ax_birth_2006$age_yrs_int)

cin_birth_2006 <- cin_pmr[birthfyear == 2006 & referralnfa == 0 & (reasonforclosure != "RC8" | is.na(reasonforclosure))]
cin_birth_2006[, row_per_child := seq_len(.N), by = rleid(id_combined)]
cin_birth_2006 <- cin_birth_2006[row_per_child == 1]
cin_birth_2006 <- cin_birth_2006[age_yrs_int > 5 & age_yrs_int <= 12]
table(cin_birth_2006$age_yrs_int)

cpp_birth_2006 <- cin_pmr[birthfyear == 2006 & !is.na(cppstartdate)]
cpp_birth_2006[, row_per_child := seq_len(.N), by = rleid(id_combined)]
cpp_birth_2006 <- cpp_birth_2006[row_per_child == 1]
cpp_birth_2006 <- cpp_birth_2006[age_yrs_int > 5 & age_yrs_int <= 12]
table(cpp_birth_2006$age_yrs_int)

output <- cbind(table(ref_birth_2006$age_yrs_int),
                table(ax_birth_2006$age_yrs_int),
                table(cin_birth_2006$age_yrs_int),
                table(cpp_birth_2006$age_yrs_int))
colnames(output) <- c("referrals", "assessed", "need", "cpp")
write.csv(output, file = "outputs/cin/main/birth_2006_main.csv")
rm(output)

# 2000/2001
ref_birth_2001 <- cin_pmr[birthfyear == 2001]
ref_birth_2001 <- ref_birth_2001[row_per_child == 1]
ref_birth_2001 <- ref_birth_2001[age_yrs_int > 12 & age_yrs_int <= 17]
table(ref_birth_2001$age_yrs_int)

ax_birth_2001 <- cin_pmr[birthfyear == 2001 & referralnfa == 0]
ax_birth_2001[, row_per_child := seq_len(.N), by = rleid(id_combined)]
ax_birth_2001 <- ax_birth_2001[row_per_child == 1]
ax_birth_2001 <- ax_birth_2001[age_yrs_int > 12 & age_yrs_int <= 17]
table(ax_birth_2001$age_yrs_int)

cin_birth_2001 <- cin_pmr[birthfyear == 2001 & referralnfa == 0 & (reasonforclosure != "RC8" | is.na(reasonforclosure))]
cin_birth_2001[, row_per_child := seq_len(.N), by = rleid(id_combined)]
cin_birth_2001 <- cin_birth_2001[row_per_child == 1]
cin_birth_2001 <- cin_birth_2001[age_yrs_int > 12 & age_yrs_int <= 17]
table(cin_birth_2001$age_yrs_int)

cpp_birth_2001 <- cin_pmr[birthfyear == 2001 & !is.na(cppstartdate)]
cpp_birth_2001[, row_per_child := seq_len(.N), by = rleid(id_combined)]
cpp_birth_2001 <- cpp_birth_2001[row_per_child == 1]
cpp_birth_2001 <- cpp_birth_2001[age_yrs_int > 12 & age_yrs_int <= 17]
table(cpp_birth_2001$age_yrs_int)

output <- cbind(table(ref_birth_2001$age_yrs_int),
                table(ax_birth_2001$age_yrs_int),
                table(cin_birth_2001$age_yrs_int),
                table(cpp_birth_2001$age_yrs_int))
colnames(output) <- c("referrals", "assessed", "need", "cpp")
write.csv(output, file = "outputs/cin/main/birth_2001_main.csv")

rm(ref_birth_2001, ax_birth_2001, cin_birth_2001, cpp_birth_2001,
   ref_birth_2006, ax_birth_2006, cin_birth_2006, cpp_birth_2006,
   ref_birth_2013, ax_birth_2013, cin_birth_2013, cpp_birth_2013,
   output)

# Part 2: RECURRENCE RATES ----------------------------------------------

# * referrals -------------------------------------------------------------

ref_tmp <- cin_pmr[birthfyear == 2013 & age_yrs_int < 6]

recurrence <- data.table(
  id_combined = ref_tmp[row_per_child == 1 & reffyear == 2013]$id_combined,
  reref = rep(NA, length(ref_tmp[row_per_child == 1 & reffyear == 2013]$id_combined)),
  reref0 = rep(NA, length(ref_tmp[row_per_child == 1 & reffyear == 2013]$id_combined)),
  reref1 = rep(NA, length(ref_tmp[row_per_child == 1 & reffyear == 2013]$id_combined)),
  reref2 = rep(NA, length(ref_tmp[row_per_child == 1 & reffyear == 2013]$id_combined)),
  reref3 = rep(NA, length(ref_tmp[row_per_child == 1 & reffyear == 2013]$id_combined)),
  reref4 = rep(NA, length(ref_tmp[row_per_child == 1 & reffyear == 2013]$id_combined)),
  reref5 = rep(NA, length(ref_tmp[row_per_child == 1 & reffyear == 2013]$id_combined))
)

recurrence$reref <- recurrence$id_combined %in% ref_tmp[epi_index > 1]$id_combined

recurrence$reref0 <- recurrence$id_combined %in% ref_tmp[epi_index > 1 & reffyear == 2013]$id_combined
recurrence$reref1 <- recurrence$id_combined %in% ref_tmp[epi_index > 1 & reffyear <= 2014]$id_combined
recurrence$reref2 <- recurrence$id_combined %in% ref_tmp[epi_index > 1 & reffyear <= 2015]$id_combined
recurrence$reref3 <- recurrence$id_combined %in% ref_tmp[epi_index > 1 & reffyear <= 2016]$id_combined
recurrence$reref4 <- recurrence$id_combined %in% ref_tmp[epi_index > 1 & reffyear <= 2017]$id_combined
recurrence$reref5 <- recurrence$id_combined %in% ref_tmp[epi_index > 1 & reffyear <= 2018]$id_combined

table(recurrence$reref0); prop.table(table(recurrence$reref0))
table(recurrence$reref1); prop.table(table(recurrence$reref1))
table(recurrence$reref2); prop.table(table(recurrence$reref2))
table(recurrence$reref3); prop.table(table(recurrence$reref3))
table(recurrence$reref4); prop.table(table(recurrence$reref4))
table(recurrence$reref5); prop.table(table(recurrence$reref5))

output <- matrix(
  rep(NA, 6 * 4),
  ncol = 4,
  nrow = 6
)

rownames(output) <- paste0("Re-ref ", 2013:2018)
colnames(output) <- 1:4
colnames(output)[1] <- paste0("Ref; n (%); denom = ", nrow(recurrence))

for (i in 1:nrow(output)) {
  
  n <- round(table(recurrence[, i + 2, with = F])[2], -1)
  p <- round(prop.table(table(recurrence[, i + 2, with = F]))[2] * 100, 1)
  output[i, 1] <- paste0(n, " (", p, "%)")
  
}

rm(i, n, p, recurrence, ref_tmp)

# * assessments ----------------------------------------------------------

ax_tmp <- cin_pmr[birthfyear == 2013 & age_yrs_int < 6 & referralnfa == 0]
ax_tmp <- ax_tmp[order(id_combined, cinreferraldate_clean, acadyr, cinclosuredate_clean)]
ax_tmp[, row_per_child := seq_len(.N), by = rleid(id_combined)]
ax_tmp[, epi_index := frank(cinreferraldate_clean, ties.method = "dense"), by = id_combined]

recurrence <- data.table(
  id_combined = ax_tmp[row_per_child == 1 & reffyear == 2013]$id_combined,
  reref = rep(NA, length(ax_tmp[row_per_child == 1 & reffyear == 2013]$id_combined)),
  reref0 = rep(NA, length(ax_tmp[row_per_child == 1 & reffyear == 2013]$id_combined)),
  reref1 = rep(NA, length(ax_tmp[row_per_child == 1 & reffyear == 2013]$id_combined)),
  reref2 = rep(NA, length(ax_tmp[row_per_child == 1 & reffyear == 2013]$id_combined)),
  reref3 = rep(NA, length(ax_tmp[row_per_child == 1 & reffyear == 2013]$id_combined)),
  reref4 = rep(NA, length(ax_tmp[row_per_child == 1 & reffyear == 2013]$id_combined)),
  reref5 = rep(NA, length(ax_tmp[row_per_child == 1 & reffyear == 2013]$id_combined))
)

recurrence$reref <- recurrence$id_combined %in% ax_tmp[epi_index > 1]$id_combined

recurrence$reref0 <- recurrence$id_combined %in% ax_tmp[epi_index > 1 & reffyear == 2013]$id_combined
recurrence$reref1 <- recurrence$id_combined %in% ax_tmp[epi_index > 1 & reffyear <= 2014]$id_combined
recurrence$reref2 <- recurrence$id_combined %in% ax_tmp[epi_index > 1 & reffyear <= 2015]$id_combined
recurrence$reref3 <- recurrence$id_combined %in% ax_tmp[epi_index > 1 & reffyear <= 2016]$id_combined
recurrence$reref4 <- recurrence$id_combined %in% ax_tmp[epi_index > 1 & reffyear <= 2017]$id_combined
recurrence$reref5 <- recurrence$id_combined %in% ax_tmp[epi_index > 1 & reffyear <= 2018]$id_combined

table(recurrence$reref); prop.table(table(recurrence$reref))

table(recurrence$reref0); prop.table(table(recurrence$reref0))
table(recurrence$reref1); prop.table(table(recurrence$reref1))
table(recurrence$reref2); prop.table(table(recurrence$reref2))
table(recurrence$reref3); prop.table(table(recurrence$reref3))
table(recurrence$reref4); prop.table(table(recurrence$reref4))
table(recurrence$reref5); prop.table(table(recurrence$reref5))

colnames(output)[2] <- paste0("Ax; n (%); denom = ", nrow(recurrence))

for (i in 1:nrow(output)) {
  
  n <- round(table(recurrence[, i + 2, with = F])[2], -1)
  p <- round(prop.table(table(recurrence[, i + 2, with = F]))[2] * 100, 1)
  output[i, 2] <- paste0(n, " (", p, "%)")
  
}

rm(i, n, p, recurrence, ax_tmp)

# * need -----------------------------------------------------------------

cin_tmp <- cin_pmr[birthfyear == 2013 & age_yrs_int < 6 & referralnfa == 0 & (reasonforclosure != "RC8" | is.na(reasonforclosure))]
cin_tmp <- cin_tmp[order(id_combined, cinreferraldate_clean, acadyr, cinclosuredate_clean)]
cin_tmp[, row_per_child := seq_len(.N), by = rleid(id_combined)]
cin_tmp[, epi_index := frank(cinreferraldate_clean, ties.method = "dense"), by = id_combined]

recurrence <- data.table(
  id_combined = cin_tmp[row_per_child == 1 & reffyear == 2013]$id_combined,
  reref = rep(NA, length(cin_tmp[row_per_child == 1 & reffyear == 2013]$id_combined)),
  reref0 = rep(NA, length(cin_tmp[row_per_child == 1 & reffyear == 2013]$id_combined)),
  reref1 = rep(NA, length(cin_tmp[row_per_child == 1 & reffyear == 2013]$id_combined)),
  reref2 = rep(NA, length(cin_tmp[row_per_child == 1 & reffyear == 2013]$id_combined)),
  reref3 = rep(NA, length(cin_tmp[row_per_child == 1 & reffyear == 2013]$id_combined)),
  reref4 = rep(NA, length(cin_tmp[row_per_child == 1 & reffyear == 2013]$id_combined)),
  reref5 = rep(NA, length(cin_tmp[row_per_child == 1 & reffyear == 2013]$id_combined))
)

recurrence$reref <- recurrence$id_combined %in% cin_tmp[epi_index > 1 & referralnfa == 0]$id_combined

recurrence$reref0 <- recurrence$id_combined %in% cin_tmp[epi_index > 1 & reffyear == 2013]$id_combined
recurrence$reref1 <- recurrence$id_combined %in% cin_tmp[epi_index > 1 & reffyear <= 2014]$id_combined
recurrence$reref2 <- recurrence$id_combined %in% cin_tmp[epi_index > 1 & reffyear <= 2015]$id_combined
recurrence$reref3 <- recurrence$id_combined %in% cin_tmp[epi_index > 1 & reffyear <= 2016]$id_combined
recurrence$reref4 <- recurrence$id_combined %in% cin_tmp[epi_index > 1 & reffyear <= 2017]$id_combined
recurrence$reref5 <- recurrence$id_combined %in% cin_tmp[epi_index > 1 & reffyear <= 2018]$id_combined

table(recurrence$reref); prop.table(table(recurrence$reref))

table(recurrence$reref0); prop.table(table(recurrence$reref0))
table(recurrence$reref1); prop.table(table(recurrence$reref1))
table(recurrence$reref2); prop.table(table(recurrence$reref2))
table(recurrence$reref3); prop.table(table(recurrence$reref3))
table(recurrence$reref4); prop.table(table(recurrence$reref4))
table(recurrence$reref5); prop.table(table(recurrence$reref5))

colnames(output)[3] <- paste0("Need; n (%); denom = ", nrow(recurrence))

for (i in 1:nrow(output)) {
  
  n <- round(table(recurrence[, i + 2, with = F])[2], -1)
  p <- round(prop.table(table(recurrence[, i + 2, with = F]))[2] * 100, 1)
  output[i, 3] <- paste0(n, " (", p, "%)")
  
}

rm(i, n, p, recurrence, cin_tmp)

# * cpp -------------------------------------------------------------------

cpp_tmp <- cin_pmr[birthfyear == 2013 & age_yrs_int < 6 & !is.na(cppstartdate)]
cpp_tmp <- cpp_tmp[order(id_combined, cinreferraldate_clean, acadyr, cinclosuredate_clean)]
cpp_tmp[, row_per_child := seq_len(.N), by = rleid(id_combined)]
cpp_tmp[, epi_index := frank(cinreferraldate_clean, ties.method = "dense"), by = id_combined]

recurrence <- data.table(
  id_combined = cpp_tmp[row_per_child == 1 & reffyear == 2013]$id_combined,
  reref = rep(NA, length(cpp_tmp[row_per_child == 1 & reffyear == 2013]$id_combined)),
  reref0 = rep(NA, length(cpp_tmp[row_per_child == 1 & reffyear == 2013]$id_combined)),
  reref1 = rep(NA, length(cpp_tmp[row_per_child == 1 & reffyear == 2013]$id_combined)),
  reref2 = rep(NA, length(cpp_tmp[row_per_child == 1 & reffyear == 2013]$id_combined)),
  reref3 = rep(NA, length(cpp_tmp[row_per_child == 1 & reffyear == 2013]$id_combined)),
  reref4 = rep(NA, length(cpp_tmp[row_per_child == 1 & reffyear == 2013]$id_combined)),
  reref5 = rep(NA, length(cpp_tmp[row_per_child == 1 & reffyear == 2013]$id_combined))
)

recurrence$reref <- recurrence$id_combined %in% cpp_tmp[epi_index > 1]$id_combined

recurrence$reref0 <- recurrence$id_combined %in% cpp_tmp[epi_index > 1 & reffyear == 2013]$id_combined
recurrence$reref1 <- recurrence$id_combined %in% cpp_tmp[epi_index > 1 & reffyear <= 2014]$id_combined
recurrence$reref2 <- recurrence$id_combined %in% cpp_tmp[epi_index > 1 & reffyear <= 2015]$id_combined
recurrence$reref3 <- recurrence$id_combined %in% cpp_tmp[epi_index > 1 & reffyear <= 2016]$id_combined
recurrence$reref4 <- recurrence$id_combined %in% cpp_tmp[epi_index > 1 & reffyear <= 2017]$id_combined
recurrence$reref5 <- recurrence$id_combined %in% cpp_tmp[epi_index > 1 & reffyear <= 2018]$id_combined

table(recurrence$reref); prop.table(table(recurrence$reref))

table(recurrence$reref0); prop.table(table(recurrence$reref0))
table(recurrence$reref1); prop.table(table(recurrence$reref1))
table(recurrence$reref2); prop.table(table(recurrence$reref2))
table(recurrence$reref3); prop.table(table(recurrence$reref3))
table(recurrence$reref4); prop.table(table(recurrence$reref4))
table(recurrence$reref5); prop.table(table(recurrence$reref5))

colnames(output)[4] <- paste0("CPP; n (%); denom = ", nrow(recurrence))

for (i in 1:nrow(output)) {
  
  n <- round(table(recurrence[, i + 2, with = F])[2], -1)
  p <- round(prop.table(table(recurrence[, i + 2, with = F]))[2] * 100, 1)
  output[i, 4] <- paste0(n, " (", p, "%)")
  
}

write.csv(output, "outputs/cin/main/recurrence_main_raw.csv")

rm(list = ls())
gc()

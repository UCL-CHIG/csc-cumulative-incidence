
# SETWD -------------------------------------------------------------------

lib_base <- "path_omitted"
assign(".lib.loc", lib_base, envir = environment(.libPaths))

# This enables parallel runs
Sys.setenv("R_LIBS_USER" = lib_base)
rm(lib_base)

setwd("path_omitted")
library(data.table)
library(ggplot2)

load("processed/cin_processed.rda")

# Part 1: INITIAL VALIDATION ------------------------------------------------------

cin[, age_grp_cohort := as.character(NA)]
cin[age_yrs_int <= 6, age_grp_cohort := "0 to 6"]
cin[age_yrs_int >= 7 & age_yrs_int <= 13, age_grp_cohort := "7 to 13"]
cin[age_yrs_int >= 14 & age_yrs_int <= 15, age_grp_cohort := "14 to 15"]
cin[age_yrs_int >= 16 & age_yrs_int <= 17, age_grp_cohort := "16 to 17"]
cin[, age_grp_cohort := factor(age_grp_cohort, levels = c("0 to 6",
                                                          "7 to 13",
                                                          "14 to 15",
                                                          "16 to 17"))]

# * id types used on referrals --------------------------------------------

# how many referrals in total
nrow(cin[row_per_childepi == 1])

table(cin[row_per_childepi == 1]$id_type, useNA = "always")
prop.table(table(cin[row_per_childepi == 1]$id_type, useNA = "always"))

write.csv(round(table(cin[row_per_childepi == 1]$age_grp_at_ref,
                      cin[row_per_childepi == 1]$id_type,
                      useNA = "always"), -1),
          file = "outputs/cin/validation/tab1_id_type_age.csv")

table(cin[row_per_childepi == 1]$age_grp_at_ref,
      cin[row_per_childepi == 1]$id_type,
      cin[row_per_childepi == 1]$acadyr)

write.csv(round(table(cin[row_per_childepi == 1]$age_grp_at_ref,
                      cin[row_per_childepi == 1]$id_type,
                      cin[row_per_childepi == 1]$acadyr), -1),
          file = "outputs/cin/validation/tab1_id_type_age_year.csv")

write.csv(round(table(cin[row_per_childepi == 1]$refcalyear,
                      cin[row_per_childepi == 1]$id_type,
                      useNA = "always"), -1),
          file = "outputs/cin/validation/tab1_id_type_year.csv")

# * by activity type -----------------------------------------------------

# referral only
table(cin[referralnfa == 1 & row_per_childepi == 1]$age_grp_at_ref,
      cin[referralnfa == 1 & row_per_childepi == 1]$id_type)

prop.table(table(cin[referralnfa == 1 & row_per_childepi == 1]$age_grp_at_ref,
                 cin[referralnfa == 1 & row_per_childepi == 1]$id_type), 1)

write.csv(table(cin[referralnfa == 1 & row_per_childepi == 1]$age_grp_at_ref,
                cin[referralnfa == 1 & row_per_childepi == 1]$id_type),
          file = "outputs/cin/validation/tab3_id_type_ref_only.csv")

# assessed but not in need
table(cin[referralnfa == 0 & row_per_childepi == 1 & reasonforclosure == "RC8"]$age_grp_at_ref,
      cin[referralnfa == 0 & row_per_childepi == 1 & reasonforclosure == "RC8"]$id_type)

prop.table(table(cin[referralnfa == 0 & row_per_childepi == 1 & reasonforclosure == "RC8"]$age_grp_at_ref,
                 cin[referralnfa == 0 & row_per_childepi == 1 & reasonforclosure == "RC8"]$id_type), 1)

write.csv(table(cin[referralnfa == 0 & row_per_childepi == 1 & reasonforclosure == "RC8"]$age_grp_at_ref,
                cin[referralnfa == 0 & row_per_childepi == 1 & reasonforclosure == "RC8"]$id_type),
          file = "outputs/cin/validation/tab3_id_type_ax_only.csv")

# found to be in need
table(cin[(reasonforclosure != "RC8" | is.na(reasonforclosure)) & referralnfa == 0 & row_per_childepi == 1]$age_grp_at_ref,
      cin[(reasonforclosure != "RC8" | is.na(reasonforclosure)) & referralnfa == 0 & row_per_childepi == 1]$id_type)

prop.table(table(cin[(reasonforclosure != "RC8" | is.na(reasonforclosure)) & referralnfa == 0 & row_per_childepi == 1]$age_grp_at_ref,
                 cin[(reasonforclosure != "RC8" | is.na(reasonforclosure)) & referralnfa == 0 & row_per_childepi == 1]$id_type), 1)

write.csv(table(cin[(reasonforclosure != "RC8" | is.na(reasonforclosure)) & referralnfa == 0 & row_per_childepi == 1]$age_grp_at_ref,
                cin[(reasonforclosure != "RC8" | is.na(reasonforclosure)) & referralnfa == 0 & row_per_childepi == 1]$id_type),
          file = "outputs/cin/validation/tab3_id_type_cin_only.csv")

# * multiple IDs ----------------------------------------------------------

cin <- cin[order(pupilmatchingrefanonymous, acadyr, lachildid_anon)]
cin[, row_per_child_pmr := as.integer(NA)]
cin[, row_per_child_pmr := seq_len(.N), by = rleid(pupilmatchingrefanonymous)]

cin[, n_lachildid := as.integer(NA)]
cin[!is.na(pupilmatchingrefanonymous), n_lachildid := length(unique(lachildid_anon)), by = pupilmatchingrefanonymous]

table(cin[row_per_child_pmr == 1]$n_lachildid, useNA = "always")
table(cin[row_per_child_pmr == 1]$age_grp_at_ref, cin[row_per_child_pmr == 1]$n_lachildid, useNA = "always")
round(prop.table(table(cin[row_per_child_pmr == 1]$age_grp_at_ref, cin[row_per_child_pmr == 1]$n_lachildid), 1) * 100, 2)

cin[, n_lachildid_grp := "1"]
cin[n_lachildid > 1, n_lachildid_grp := ">1"]
cin[, n_lachildid_grp := factor(n_lachildid_grp, levels = c("1", ">1"))]
table(cin[row_per_child_pmr == 1]$n_lachildid, cin[row_per_child_pmr == 1]$n_lachildid_grp, useNA = "always")

# referrals
table(cin[row_per_child_pmr == 1]$age_grp_at_ref, cin[row_per_child_pmr == 1]$n_lachildid_grp)
round(prop.table(table(cin[row_per_child_pmr == 1]$age_grp_at_ref, cin[row_per_child_pmr == 1]$n_lachildid_grp), 1) * 100, 2)

# assessment
table(cin[referralnfa == 0 & row_per_child_pmr == 1]$age_grp_at_ref, cin[referralnfa == 0 & row_per_child_pmr == 1]$n_lachildid_grp)
round(prop.table(table(cin[referralnfa == 0 & row_per_child_pmr == 1]$age_grp_at_ref, cin[referralnfa == 0 & row_per_child_pmr == 1]$n_lachildid_grp), 1) * 100, 2)

# cin
table(cin[(reasonforclosure != "RC8" | is.na(reasonforclosure)) & referralnfa == 0 & row_per_child_pmr == 1]$age_grp_at_ref, cin[(reasonforclosure != "RC8" | is.na(reasonforclosure)) & referralnfa == 0 & row_per_child_pmr == 1]$n_lachildid_grp)
round(prop.table(table(cin[(reasonforclosure != "RC8" | is.na(reasonforclosure)) & referralnfa == 0 & row_per_child_pmr == 1]$age_grp_at_ref, cin[(reasonforclosure != "RC8" | is.na(reasonforclosure)) & referralnfa == 0 & row_per_child_pmr == 1]$n_lachildid_grp), 1) * 100, 2)


# * referrals received ----------------------------------------------------

write.csv(round(table(cin[row_per_childepi == 1]$reffyear, useNA = "always"), -1),
          file = "outputs/cin/validation/tab2_n_refs_rec_fyr.csv")

# Figure - referrals recived - do facets for all years
# in order to have x axis ovelap, need to set x value to days from beginning of year
# cin[, ref_date_from_cal_year_start := as.integer(cinreferraldate_clean - as.Date(paste0(cin$refcalyear, "-01-01")))]
# cin[, ref_date_from_f_year_start := as.integer(cinreferraldate_clean - as.Date(paste0(cin$reffyear - 1, "-04-01")))]

# p <- ggplot(data = cin[row_per_childepi == 1],
#             aes(x = ref_date_from_f_year_start)) +
#   geom_histogram(fill = "white",
#                  colour = "black") +
#   ylab("Number") +
#   xlab("Day of year") +
#   ggtitle("Referrals by financial year, ages 0-17 yr (our data)") +
#   scale_x_continuous(breaks = c(0, 31, 61, 92, 122, 153, 184, 214, 245, 276, 306, 337),
#                      labels = c("Mar", "Apr", "May", "Jun",
#                                 "Jul", "Aug", "Sep", "Oct",
#                                 "Nov", "Dec", "Jan", "Feb")) +
#   theme(panel.grid = element_blank(),
#         panel.border = element_blank(),
#         panel.background = element_blank(), 
#         axis.line = element_line(colour = "black"),
#         axis.text = element_text(colour = "black")) +
#   facet_wrap(reffyear ~ .)
# 
# tiff("outputs/cin/validation/fig1_refs_fyear.tiff",
#      width = 12, height = 12, units = "in", res = 300)
# p
# dev.off()
# 
# plot_dt <- ggplot_build(p)
# write.csv(plot_dt$data, file = "outputs/cin/validation/fig1_underlying_data.csv", row.names = F)
# rm(plot_dt, p)

# * need episodes starting ------------------------------------------------

table(cin[(reasonforclosure != "RC8" | is.na(reasonforclosure)) & referralnfa == 0 & row_per_childepi == 1]$reffyear,
      useNA = "always")
write.csv(round(table(cin[(reasonforclosure != "RC8" | is.na(reasonforclosure)) & referralnfa == 0 & row_per_childepi == 1]$reffyear, useNA = "always"), -1),
          file = "outputs/cin/validation/tab2_n_need_fyr.csv")

# p <- ggplot(data = cin[referralnfa == 0 & (reasonforclosure != "RC8" | is.na(reasonforclosure)) & row_per_childepi == 1],
#             aes(x = ref_date_from_f_year_start)) +
#   geom_histogram(fill = "white",
#                  colour = "black") +
#   ylab("Number") +
#   xlab("Day of year") +
#   ggtitle("Need episodes by financial year, ages 0-17 yr (our data)") +
#   scale_x_continuous(breaks = c(0, 31, 61, 92, 122, 153, 184, 214, 245, 276, 306, 337),
#                      labels = c("Mar", "Apr", "May", "Jun",
#                                 "Jul", "Aug", "Sep", "Oct",
#                                 "Nov", "Dec", "Jan", "Feb")) +
#   theme(panel.grid = element_blank(),
#         panel.border = element_blank(),
#         panel.background = element_blank(), 
#         axis.line = element_line(colour = "black"),
#         axis.text = element_text(colour = "black")) +
#   facet_wrap(reffyear ~ .)
# 
# tiff("outputs/cin/validation/fig2_need_fyear.tiff",
#      width = 12, height = 12, units = "in", res = 300)
# p
# dev.off()
# 
# plot_dt <- ggplot_build(p)
# write.csv(plot_dt$data, file = "outputs/cin/validation/fig2_underlying_data.csv", row.names = F)
# rm(plot_dt, p)
# 
# # * above plots by calendar year ------------------------------------------
# 
# # referrals
# p <- ggplot(data = cin[row_per_childepi == 1],
#             aes(x = ref_date_from_cal_year_start)) +
#   geom_histogram(fill = "white",
#                  colour = "black") +
#   ylab("Number") +
#   xlab("Day of year") +
#   ggtitle("Referrals by calendar year, ages 0-17 yr (our data)") +
#   scale_x_continuous(breaks = c(0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334),
#                      labels = c("Jan", "Feb", "Mar",
#                                 "Apr", "May", "Jun",
#                                 "Jul", "Aug", "Sep",
#                                 "Oct", "Nov", "Dec")) +
#   theme(panel.grid = element_blank(),
#         panel.border = element_blank(),
#         panel.background = element_blank(), 
#         axis.line = element_line(colour = "black"),
#         axis.text = element_text(colour = "black")) +
#   facet_wrap(refcalyear ~ .)
# 
# tiff("outputs/cin/validation/fig3_refs_cal_year.tiff",
#      width = 12, height = 12, units = "in", res = 300)
# p
# dev.off()
# 
# plot_dt <- ggplot_build(p)
# write.csv(plot_dt$data, file = "outputs/cin/validation/fig3_underlying_data.csv", row.names = F)
# rm(plot_dt, p)
# 
# # need
# p <- ggplot(data = cin[referralnfa == 0 & (reasonforclosure != "RC8" | is.na(reasonforclosure)) & row_per_childepi == 1],
#             aes(x = ref_date_from_cal_year_start)) +
#   geom_histogram(fill = "white",
#                  colour = "black") +
#   ylab("Number") +
#   xlab("Day of year") +
#   ggtitle("Need episodes by calendar year, ages 0-17 yr (our data)") +
#   scale_x_continuous(breaks = c(0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334),
#                      labels = c("Jan", "Feb", "Mar",
#                                 "Apr", "May", "Jun",
#                                 "Jul", "Aug", "Sep",
#                                 "Oct", "Nov", "Dec")) +
#   theme(panel.grid = element_blank(),
#         panel.border = element_blank(),
#         panel.background = element_blank(), 
#         axis.line = element_line(colour = "black"),
#         axis.text = element_text(colour = "black")) +
#   facet_wrap(refcalyear ~ .)
# 
# tiff("outputs/cin/validation/fig4_need_cal_year.tiff",
#      width = 12, height = 12, units = "in", res = 300)
# p
# dev.off()
# 
# plot_dt <- ggplot_build(p)
# write.csv(plot_dt$data, file = "outputs/cin/validation/fig4_underlying_data.csv", row.names = F)
# rm(plot_dt, p)

# * Age ---------------------------------------------------------------------

p <- ggplot(data = cin[row_per_childepi == 1],
            aes(x = age_at_ref_yrs)) +
  geom_histogram(fill = "white",
                 colour = "black") +
  xlab("Age (yr)") +
  ylab("Number") +
  scale_x_continuous(breaks = c(0, 5, 11, 16)) +
  theme(panel.grid = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text = element_text(colour = "black")) +
  facet_wrap(reffyear ~ .)

tiff("outputs/cin/validation/fig1_age_at_ref_fyear.tiff",
     width = 12, height = 12, units = "in", res = 300)
p
dev.off()

plot_dt <- ggplot_build(p)
write.csv(plot_dt$data, file = "outputs/cin/validation/fig1_underlying_data.csv", row.names = F)

# Part 2: demographics ----------------------------------------------------

cin[, primaryneedcode_clean_binary := as.character(NA)]
cin[primaryneedcode_clean == "n1", primaryneedcode_clean_binary := "Abuse/Neglect"]
cin[primaryneedcode_clean %in% paste0("n", 2:9), primaryneedcode_clean_binary := "Other"]
table(cin$primaryneedcode_clean_binary, useNA = "always")

output_rownames <-
  c(
    "Male",
    "Female",
    "Missing",
    "Pre-birth",
    "0-4 yr",
    "5-9 yr",
    "10-15 yr",
    "16+ yr",
    "Asian",
    "Black",
    "Mixed",
    "Other",
    "Traveller/Roma",
    "White",
    "Missing",
    "Abuse/Neglect",
    "Other reasons",
    "Missing"
  )

# * referrals -------------------------------------------------------------

output <- matrix(
  ncol = 11,
  nrow = length(output_rownames)
)

rownames(output) <- output_rownames
colnames(output) <- paste0("fy_", 2009:2019)


t1 <- round(table(cin[row_per_child == 1]$female,
                  cin[row_per_child == 1]$reffyear,
                  useNA = "ifany"), -1)

t2 <- round(prop.table(round(table(cin[row_per_child == 1]$female,
                                   cin[row_per_child == 1]$reffyear,
                                   useNA = "ifany"), -1), 2) * 100, 1)

output[1:3, ] <- paste0(t1, " (", t2, "%)")


t1 <- round(table(cin[row_per_child == 1]$age_grp_at_ref,
                  cin[row_per_child == 1]$reffyear), -1)

t2 <- round(prop.table(round(table(cin[row_per_child == 1]$age_grp_at_ref,
                                   cin[row_per_child == 1]$reffyear), -1), 2) * 100, 1)

output[4:8, ] <- paste0(t1, " (", t2, "%)")


t1 <- round(table(cin[row_per_child == 1]$ethnicity_major,
                  cin[row_per_child == 1]$reffyear,
                  useNA = "ifany"), -1)

t2 <- round(prop.table(round(table(cin[row_per_child == 1]$ethnicity_major,
                                   cin[row_per_child == 1]$reffyear,
                                   useNA = "ifany"), -1), 2) * 100, 1)

output[9:15, ] <- paste0(t1, " (", t2, "%)")

t1 <- round(table(cin[row_per_child == 1]$primaryneedcode_clean_binary,
                  cin[row_per_child == 1]$reffyear,
                  useNA = "ifany"), -1)

t2 <- round(prop.table(round(table(cin[row_per_child == 1]$primaryneedcode_clean_binary,
                                   cin[row_per_child == 1]$reffyear,
                                   useNA = "ifany"), -1), 2) * 100, 1)

output[16:18, ] <- paste0(t1, " (", t2, "%)")

write.csv(output, file = "outputs/cin/validation/DEMOGRAPHICS_1_REF.csv", row.names = T)
rm(output, t1, t2)

# assessments
ax_tmp <- cin[referralnfa == 0]
ax_tmp <- ax_tmp[order(id_combined, cinreferraldate_clean)]
ax_tmp[, row_per_child := seq_len(.N), by = rleid(id_combined)]

output <- matrix(
  ncol = 11,
  nrow = length(output_rownames)
)

rownames(output) <- output_rownames
colnames(output) <- paste0("fy_", 2009:2019)

t1 <- round(table(ax_tmp[row_per_child == 1]$female,
                  ax_tmp[row_per_child == 1]$reffyear,
                  useNA = "ifany"), -1)

t2 <- round(prop.table(round(table(ax_tmp[row_per_child == 1]$female,
                                   ax_tmp[row_per_child == 1]$reffyear,
                                   useNA = "ifany"), -1), 2) * 100, 1)

output[1:3, ] <- paste0(t1, " (", t2, "%)")


t1 <- round(table(ax_tmp[row_per_child == 1]$age_grp_at_ref,
                  ax_tmp[row_per_child == 1]$reffyear), -1)

t2 <- round(prop.table(round(table(ax_tmp[row_per_child == 1]$age_grp_at_ref,
                                   ax_tmp[row_per_child == 1]$reffyear), -1), 2) * 100, 1)

output[4:8, ] <- paste0(t1, " (", t2, "%)")


t1 <- round(table(ax_tmp[row_per_child == 1]$ethnicity_major,
                  ax_tmp[row_per_child == 1]$reffyear,
                  useNA = "ifany"), -1)

t2 <- round(prop.table(round(table(ax_tmp[row_per_child == 1]$ethnicity_major,
                                   ax_tmp[row_per_child == 1]$reffyear,
                                   useNA = "ifany"), -1), 2) * 100, 1)

output[9:15, ] <- paste0(t1, " (", t2, "%)")

t1 <- round(table(ax_tmp[row_per_child == 1]$primaryneedcode_clean_binary,
                  ax_tmp[row_per_child == 1]$reffyear,
                  useNA = "ifany"), -1)

t2 <- round(prop.table(round(table(ax_tmp[row_per_child == 1]$primaryneedcode_clean_binary,
                                   ax_tmp[row_per_child == 1]$reffyear,
                                   useNA = "ifany"), -1), 2) * 100, 1)

output[16:18, ] <- paste0(t1, " (", t2, "%)")

write.csv(output, file = "outputs/cin/validation/DEMOGRAPHICS_2_AX.csv", row.names = T)
rm(output, ax_tmp)

# cin
cin_tmp <- cin[referralnfa == 0 & (reasonforclosure != "RC8" | is.na(reasonforclosure))]
cin_tmp <- cin_tmp[order(id_combined, cinreferraldate_clean)]
cin_tmp[, row_per_child := seq_len(.N), by = rleid(id_combined)]

output <- matrix(
  ncol = 11,
  nrow = length(output_rownames)
)

rownames(output) <- output_rownames
colnames(output) <- paste0("fy_", 2009:2019)

t1 <- round(table(cin_tmp[row_per_child == 1]$female,
                  cin_tmp[row_per_child == 1]$reffyear,
                  useNA = "ifany"), -1)

t2 <- round(prop.table(round(table(cin_tmp[row_per_child == 1]$female,
                                   cin_tmp[row_per_child == 1]$reffyear,
                                   useNA = "ifany"), -1), 2) * 100, 1)

output[1:3, ] <- paste0(t1, " (", t2, "%)")


t1 <- round(table(cin_tmp[row_per_child == 1]$age_grp_at_ref,
                  cin_tmp[row_per_child == 1]$reffyear), -1)

t2 <- round(prop.table(round(table(cin_tmp[row_per_child == 1]$age_grp_at_ref,
                                   cin_tmp[row_per_child == 1]$reffyear), -1), 2) * 100, 1)

output[4:8, ] <- paste0(t1, " (", t2, "%)")


t1 <- round(table(cin_tmp[row_per_child == 1]$ethnicity_major,
                  cin_tmp[row_per_child == 1]$reffyear,
                  useNA = "ifany"), -1)

t2 <- round(prop.table(round(table(cin_tmp[row_per_child == 1]$ethnicity_major,
                                   cin_tmp[row_per_child == 1]$reffyear,
                                   useNA = "ifany"), -1), 2) * 100, 1)

output[9:15, ] <- paste0(t1, " (", t2, "%)")

t1 <- round(table(cin_tmp[row_per_child == 1]$primaryneedcode_clean_binary,
                  cin_tmp[row_per_child == 1]$reffyear,
                  useNA = "ifany"), -1)

t2 <- round(prop.table(round(table(cin_tmp[row_per_child == 1]$primaryneedcode_clean_binary,
                                   cin_tmp[row_per_child == 1]$reffyear,
                                   useNA = "ifany"), -1), 2) * 100, 1)

output[16:18, ] <- paste0(t1, " (", t2, "%)")

write.csv(output, file = "outputs/cin/validation/DEMOGRAPHICS_3_CIN.csv", row.names = T)
rm(output, cin_tmp)

# cpp
cpp_tmp <- cin[!is.na(cppstartdate)]
cpp_tmp <- cpp_tmp[order(id_combined, cinreferraldate_clean)]
cpp_tmp[, row_per_child := seq_len(.N), by = rleid(id_combined)]

output <- matrix(
  ncol = 11,
  nrow = length(output_rownames)
)

rownames(output) <- output_rownames
colnames(output) <- paste0("fy_", 2009:2019)

t1 <- round(table(cpp_tmp[row_per_child == 1]$female,
                  cpp_tmp[row_per_child == 1]$reffyear,
                  useNA = "ifany"), -1)

t2 <- round(prop.table(round(table(cpp_tmp[row_per_child == 1]$female,
                                   cpp_tmp[row_per_child == 1]$reffyear,
                                   useNA = "ifany"), -1), 2) * 100, 1)

output[1:3, ] <- paste0(t1, " (", t2, "%)")


t1 <- round(table(cpp_tmp[row_per_child == 1]$age_grp_at_ref,
                  cpp_tmp[row_per_child == 1]$reffyear), -1)

t2 <- round(prop.table(round(table(cpp_tmp[row_per_child == 1]$age_grp_at_ref,
                                   cpp_tmp[row_per_child == 1]$reffyear), -1), 2) * 100, 1)

output[4:8, ] <- paste0(t1, " (", t2, "%)")


t1 <- round(table(cpp_tmp[row_per_child == 1]$ethnicity_major,
                  cpp_tmp[row_per_child == 1]$reffyear,
                  useNA = "ifany"), -1)

t2 <- round(prop.table(round(table(cpp_tmp[row_per_child == 1]$ethnicity_major,
                                   cpp_tmp[row_per_child == 1]$reffyear,
                                   useNA = "ifany"), -1), 2) * 100, 1)

output[9:15, ] <- paste0(t1, " (", t2, "%)")

t1 <- round(table(cpp_tmp[row_per_child == 1]$primaryneedcode_clean_binary,
                  cpp_tmp[row_per_child == 1]$reffyear,
                  useNA = "ifany"), -1)

t2 <- round(prop.table(round(table(cpp_tmp[row_per_child == 1]$primaryneedcode_clean_binary,
                                   cpp_tmp[row_per_child == 1]$reffyear,
                                   useNA = "ifany"), -1), 2) * 100, 1)

output[16:18, ] <- paste0(t1, " (", t2, "%)")

write.csv(output, file = "outputs/cin/validation/DEMOGRAPHICS_4_CPP.csv", row.names = T)
rm(output, cpp_tmp)



# Written by Matthew Jay
# Reviewed by Rachel Pearson
# Edited by Matthew Jay

# Step 0: set up workspace ------------------------------------------------

assign(".lib.loc", c(.libPaths(), "path_omitted"), envir = environment(.libPaths))

library(data.table)
setwd("path_omitted")

mode.fun <- function(v) {
  v <- v[!is.na(v)]
  ux <- unique(v)
  tab <- tabulate(match(v, ux))
  md <- ux[tab == max(tab)]
  return(md[length(md)])
}

# Step 1: Load data ----------------------------------------------------------

load("raw/cin.rda")
cin <- as.data.table(cin)

# clean LA codes:
cin[is.na(CIN_LA), CIN_LA := CIN_CIN_LA]
cin[, CIN_CIN_LA := NULL]

# clean variable names:
names(cin) <- tolower(gsub("CIN_", "", names(cin)))

# check missingness among key identifiers:
# table(is.na(cin$pupilmatchingrefanonymous))
# table(is.na(cin$lachildid_anon))
# table(pmr_mis = is.na(cin$pupilmatchingrefanonymous),
#       la_id_mis = is.na(cin$lachildid_anon))

# where only lachild_id missing, just use the pmr - makes no difference to linkage and we can retain them
cin[is.na(lachildid_anon) & !is.na(pupilmatchingrefanonymous), lachildid_anon := pupilmatchingrefanonymous]

# now drop small number with missing lachildid & missing pmr - cannot do anything with them
cin <- cin[!is.na(lachildid_anon)]

# create new identifier (lachildid + la) - necessary to identify unique children as diff LAs can use same ID
cin[, lachildid_anon_concat := paste0(lachildid_anon, "-", la)]

length(unique(cin$lachildid_anon_concat))
length(unique(cin$pupilmatchingrefanonymous))

# Step 2: get age (separate file) -----------------------------------------------------

load("raw/cin_age.rda")
cin_age <- as.data.table(cin_age)

# clean variable names:
names(cin_age) <- tolower(gsub("CIN_", "", names(cin_age)))

# reorder variables:
cin_age <- cin_age[, c("lachildid_anon", "la", "pmr", "acadyr", "month_of_birth", "year_of_birth", "referraldate", "referred_within_6_days_of_birth_unborn")]
setnames(cin_age, "referred_within_6_days_of_birth_unborn", "referred_newborn_unborn")

# table(is.na(cin_age$lachildid_anon))
# table(is.na(cin_age$pmr))
# table(is.na(cin_age$month_of_birth))
# table(is.na(cin_age$year_of_birth))
# table(is.na(cin_age$year_of_birth),
#       is.na(cin_age$month_of_birth))
# table(is.na(cin_age$referred_newborn_unborn))

# format referral date:
cin_age[, referraldate := as.Date(referraldate, format = "%Y-%m-%d")]
cin_age[referraldate < as.Date("1930-01-01"), referraldate := NA]

# remove rows with no child id:
cin_age <- cin_age[!is.na(lachildid_anon)]

# create new child id from id + la:
cin_age[, lachildid_anon_concat := paste0(lachildid_anon, "-", la)]

# how many ids from cin_age in cin and vice versa:
# table(cin_age$lachildid_anon %in% cin$lachildid_anon)
# table(cin$lachildid_anon %in% cin_age$lachildid_anon)

# need to clean age here first
cin_age <- cin_age[order(lachildid_anon_concat, acadyr, referraldate)]

# how many children have yr birth before 1990 (i.e turning 18 in 2008)
# table(cin_age$year_of_birth < 1990)

cin_age[, ym_birth := paste0(year_of_birth, "-", sprintf("%02d", month_of_birth))]
# table(cin_age$ym_birth)
cin_age[ym_birth == "NA-NA", ym_birth := NA]

# multiple birth dates per child:
# cin_age[, check := any(length(unique(ym_birth)) > 1), by = .(lachildid_anon_concat)]
# table(cin_age$check)

# View(cin_age[lachildid_anon %in% cin_age[check == T]$lachildid_anon])

# deal with multiple birth dates per child:
cin_age[, ym_birth := mode.fun(ym_birth), by = .(lachildid_anon_concat)]

# cin_age[, check := any(length(unique(ym_birth)) > 1), by = .(lachildid_anon_concat)]
# table(cin_age$check)
# cin_age[, check := NULL]

# clean referred_newborn_unborn flag:
cin_age[, referred_newborn_unborn := max(referred_newborn_unborn, na.rm = TRUE), by = .(lachildid_anon_concat)]

# get deduplicated age file
cin_age <- cin_age[, c("lachildid_anon_concat", "ym_birth", "referred_newborn_unborn")]
cin_age <- cin_age[!duplicated(cin_age)]
# length(unique(cin_age$lachildid_anon_concat))
cin_age <- cin_age[order(lachildid_anon_concat)]

# merge ages on to main data:
cin <- merge(cin,
             cin_age,
             by = "lachildid_anon_concat",
             all.x = T)

# remove cin_age data:
rm(cin_age)

# check merge results:
# table(is.na(cin$ym_birth))
# table(is.na(cin$expecteddob))
# 
# table(dob = is.na(cin$expecteddob),
#       ym = is.na(cin$ym_birth)) # we'll come back to these later.

# Step 3: Validity of start and end dates ---------------------------------------

# use cinreferraldate and cinclosuredate. Where missing use latestreferraldate and latestclosuredate
cin[, latestreferraldate := as.Date(cin$latestreferraldate, format = "%Y-%m-%d")]
cin[, cinreferraldate := as.Date(cin$cinreferraldate, format = "%Y-%m-%d")]

cin[latestreferraldate < as.Date("1930-01-01"), latestreferraldate := NA]
cin[cinreferraldate < as.Date("1930-01-01"), cinreferraldate := NA]

cin[latestreferraldate > as.Date("2019-03-31"), latestreferraldate := NA]
cin[cinreferraldate > as.Date("2019-03-31"), cinreferraldate := NA]

# table(cin$cinreferraldate == cin$latestreferraldate)

# table(is.na(cin$latestreferraldate), cin$acadyr)
# table(is.na(cin$cinreferraldate), cin$acadyr)
# table(is.na(cin$cinreferraldate),
#       is.na(cin$latestreferraldate))

cin[, latestclosuredate := as.Date(cin$latestclosuredate, format = "%Y-%m-%d")]
cin[, cinclosuredate := as.Date(cin$cinclosuredate, format = "%Y-%m-%d")]

cin[latestclosuredate < as.Date("1930-01-01"), latestclosuredate := NA]
cin[cinclosuredate < as.Date("1930-01-01"), cinclosuredate := NA]

cin[latestclosuredate > as.Date("2019-03-31"), latestclosuredate := NA]
cin[cinclosuredate > as.Date("2019-03-31"), cinclosuredate := NA]

# table(cin$latestclosuredate == cin$cinclosuredate)
# 
# table(is.na(cin$latestclosuredate), cin$acadyr)
# table(is.na(cin$cinclosuredate), cin$acadyr)
# table(is.na(cin$cinclosuredate),
#       is.na(cin$latestclosuredate))

# table(!is.na(cin[is.na(cinreferraldate)]$latestreferraldate))
cin[, cinreferraldate_clean := cinreferraldate]
cin[is.na(cinreferraldate_clean) & !is.na(latestreferraldate), cinreferraldate_clean := latestreferraldate]

# table(!is.na(cin[is.na(cinclosuredate)]$latestclosuredate))
cin[, cinclosuredate_clean := cinclosuredate]
cin[is.na(cinclosuredate_clean) & !is.na(latestclosuredate), cinclosuredate_clean := latestclosuredate]

# create ref year variable
cin[, refyear := format(cin$cinreferraldate_clean, format = "%Y")]

# cpp dates
cin[, cppstartdate := as.Date(cin$cppstartdate, format = "%Y-%m-%d")]
cin[, cppenddate := as.Date(cin$cppenddate, format = "%Y-%m-%d")]

cin[cppstartdate < as.Date("1930-01-01"), cppstartdate := NA]
cin[cppenddate < as.Date("1930-01-01"), cppenddate := NA]

# order data by identifiers
cin <- cin[order(lachildid_anon_concat, acadyr, cinreferraldate_clean, cinclosuredate_clean)]

# Step 4: Drop referrals pre Apr 2008 --------------------------------------------------

cin <- cin[!(cinreferraldate_clean < as.Date("2008-04-01")) | is.na(cinreferraldate_clean)] # this retains missing
cin <- cin[!(cinclosuredate_clean < as.Date("2008-04-01")) | is.na(cinclosuredate_clean)] # this retains missing

length(unique(cin$lachildid_anon_concat))
length(unique(cin$pupilmatchingrefanonymous))

# Step 5: validity of PMRs -------------------------------------------------

# table(is.na(cin$lachildid_anon))
# table(is.na(cin$pupilmatchingrefanonymous))

cin <- cin[order(lachildid_anon, la, acadyr)]
# View(cin[lachildid_anon %in% cin[is.na(pupilmatchingrefanonymous)]$lachildid_anon,
#          c("lachildid_anon", "la","ym_birth", "gender", "ethnicity", "lachildid_anon_concat", "pupilmatchingrefanonymous",
#            "acadyr", "cinreferraldate_clean")])

# draw down PMR where child is in same LA and has PMR in some records but not others
cin[, pupilmatchingrefanonymous2 := pupilmatchingrefanonymous[which(!is.na(pupilmatchingrefanonymous))][1],
    by = .(lachildid_anon_concat)]

cin <- cin[order(lachildid_anon, la, acadyr)]
# View(cin[lachildid_anon %in% cin[is.na(pupilmatchingrefanonymous)]$lachildid_anon,
#          c("lachildid_anon", "la","ym_birth", "gender", "ethnicity", "lachildid_anon_concat", "pupilmatchingrefanonymous",
#            "pupilmatchingrefanonymous2", "acadyr", "cinreferraldate_clean")])

# table(is.na(cin$pupilmatchingrefanonymous))
# table(is.na(cin$pupilmatchingrefanonymous2))

cin[, pupilmatchingrefanonymous := pupilmatchingrefanonymous2]
cin[, pupilmatchingrefanonymous2 := NULL]

length(unique(cin$lachildid_anon_concat))
length(unique(cin$pupilmatchingrefanonymous))

# Step 6: missing start date -----------------------------------------------

cin <- cin[order(lachildid_anon_concat)]
cin[, n_mis_start := sum(is.na(cinreferraldate_clean)), by = .(lachildid_anon_concat)]
cin[, record_per_child := seq_len(.N), by = rleid(lachildid_anon_concat)]

# View(cin[lachildid_anon_concat %in% cin[is.na(cinreferraldate_clean)]$lachildid_anon_concat,
#          c("lachildid_anon_concat", "record_per_child", "cinreferraldate_clean", "n_mis_start")])

# first row per child:
# table(cin[record_per_child == 1]$n_mis_start)

# impute missing start date with first day of academic year so functions below work
cin[is.na(cinreferraldate_clean), cinreferraldate_clean := as.Date(paste0(substr(acadyr, 1, 4), "-09-01"))]
# table(is.na(cin$cinreferraldate_clean))

# Step 7: Referral date > closure date --------------------------------------

# table(cin$cinreferraldate_clean > cin$cinclosuredate_clean, useNA = "always")

# View(cin[lachildid_anon_concat %in% cin[cin$cinreferraldate_clean > cin$cinclosuredate_clean]$lachildid_anon_concat,
#          c("lachildid_anon_concat", "pupilmatchingrefanonymous", "cinreferraldate_clean", "cinclosuredate_clean")])

# Just use the referral date as closure date so we can retain the episodes and the dates make sense at least
# the main thing is that the referal is open during the academic year by design, so this method preserves that.

# First, flag those we have imputed this way
cin[, cin_closure_date_imp_fl := F]
cin[cinreferraldate_clean > cinclosuredate_clean, cin_closure_date_imp_fl := T]

cin[cinreferraldate_clean > cinclosuredate_clean]$cinclosuredate_clean <-
  cin[cinreferraldate_clean > cinclosuredate_clean]$cinreferraldate_clean

# table(cin$cinreferraldate_clean > cin$cinclosuredate_clean)
# table(cin$cin_closure_date_imp_fl)

# Step 8: Referral NFA inconsistent --------------------------------------

# table(cin$referralnfa, useNA = "always")

# check if any children have at least 1 nfa=1 and at least 1 nfa==0 for a single referral:
# cin[, check := any(length(unique(referralnfa)) > 1), by = .(lachildid_anon_concat, cinreferraldate_clean)]
# table(cin$check)

cin[, referralnfa := mode.fun(referralnfa), by = .(lachildid_anon_concat, cinreferraldate_clean)]

# cin[, check := any(length(unique(referralnfa)) > 1), by = .(lachildid_anon_concat, cinreferraldate_clean)]
# table(cin$check)
# cin[, check := NULL]

# Step 9: NFA missing ----------------------------------------------------

# table(cin$referralnfa, useNA = "always")
cin[is.na(referralnfa)]$referralnfa <- 0

# Step 10: Closure reason inconsistent ------------------------------------

# table(cin$reasonforclosure, useNA = "always")
cin[reasonforclosure %in% c("NFA", "No", "NON", "NONE"), reasonforclosure := "RC8"]
cin[!(substr(reasonforclosure, 1, 1) %in% c("R", "r")), reasonforclosure := NA]

cin[, reasonforclosure := toupper(reasonforclosure)]
cin[, reasonforclosure := gsub("[[:space:]]", "", reasonforclosure)]
cin[, reasonforclosure := factor(reasonforclosure)]

# table(cin$reasonforclosure, useNA = "always")
# 
# cin[, check := any(length(unique(reasonforclosure)) > 1), by = .(lachildid_anon_concat, cinreferraldate_clean)]
# table(cin$check)

cin[, reasonforclosure := mode.fun(reasonforclosure), by = .(lachildid_anon_concat, cinreferraldate_clean)]

# cin[, check := any(length(unique(reasonforclosure)) > 1), by = .(lachildid_anon_concat, cinreferraldate_clean)]
# table(cin$check)
# cin[, check := NULL]

# Step 11: Ethnicity inconsistent -----------------------------------------

# table(cin$ethnicity, useNA = "always")

cin$ethnicity <- toupper(cin$ethnicity)
cin[substr(ethnicity,1,3) == "WBR", ethnicity := "WBRI"]

na.vals <- c("NA  ", "NOBT", "NOT ", "REFU")
cin[ethnicity %in% na.vals, ethnicity := NA]

# table(cin$ethnicity, useNA = "always")

# check for multiple ethnicities per child:
# cin[, check := any(length(unique(ethnicity)) > 1), by = .(lachildid_anon_concat)]
# table(cin$check)

cin[, ethnicity := mode.fun(ethnicity), by = .(lachildid_anon_concat)]

# cin[, check := any(length(unique(ethnicity)) > 1), by = .(lachildid_anon_concat)]
# table(cin$check)
# cin[, check := NULL]

# now create major category
# table(cin$ethnicity, useNA = "always")

white <- c("WBRI", "WIRI", "WOTH")
mixed <- c("MWBC", "MWBA", "MWAS", "MOTH", "MWAF", "MWBR")
asian <- c("AIND", "APKN", "ABAN", "AOTH", "CHNE")
black <- c("BCRB", "BAFR", "BOTH")
trave <- c("WIRT", "WROM")
other <- c("OOTH")

# clean:
cin$ethnicity_major <- cin$ethnicity

cin[ethnicity %in% white, ethnicity_major := "White"]
cin[ethnicity %in% mixed, ethnicity_major := "Mixed"]
cin[ethnicity %in% asian, ethnicity_major := "Asian (inc Cn)"]
cin[ethnicity %in% black, ethnicity_major := "Black"]
cin[ethnicity %in% trave, ethnicity_major := "Traveller/Roma"]
cin[ethnicity %in% other, ethnicity_major := "Other"]
cin[, ethnicity_major := factor(ethnicity_major)]

# table(cin$ethnicity_major, useNA = "always")
# table(cin$ethnicity_major, cin$ethnicity, useNA = "always")

rm(white, mixed, asian, black, trave, other, na.vals)

# Step 12: Gender inconsitent ---------------------------------------------

# table(cin$gender, useNA = "always")
cin$female <- cin$gender - 1
cin[female %in% c(-1, 8), female := NA]
# table(cin$female, useNA = "always")

# cin[, check := any(length(unique(female)) > 1), by = .(lachildid_anon_concat)]
# table(cin$check)

cin[, female := mode.fun(female), by = .(lachildid_anon_concat)]

# cin[, check := any(length(unique(female)) > 1), by = .(lachildid_anon_concat)]
# table(cin$check)
# cin[, check := NULL]

# Step 13: primary need code ----------------------------------------------

cin[, primaryneedcode_clean := tolower(primaryneedcode)]
cin[, primaryneedcode_clean := gsub("\\s", "", primaryneedcode_clean)]
cin[primaryneedcode_clean == "a2", primaryneedcode_clean := "n2"]
cin[primaryneedcode_clean == "a3", primaryneedcode_clean := "n3"]
cin[primaryneedcode_clean == "a4", primaryneedcode_clean := "n4"]
cin[primaryneedcode_clean == "a5", primaryneedcode_clean := "n5"]
cin[primaryneedcode_clean %in% c("unmappedneedcode", "no", "n0"), primaryneedcode_clean := NA]
# table(cin$primaryneedcode,
#       cin$primaryneedcode_clean,
#       useNA = "always")

# cin[, check := any(length(unique(primaryneedcode_clean)) > 1), by = .(lachildid_anon_concat)]
# table(cin$check)

cin[, primaryneedcode_clean := mode.fun(primaryneedcode_clean), by = .(lachildid_anon_concat)]

# cin[, check := any(length(unique(primaryneedcode_clean)) > 1), by = .(lachildid_anon_concat)]
# table(cin$check)
# cin[, check := NULL]

# Step 14: End dates where referral NFA -----------------------------------

# table(na.end = is.na(cin$cinclosuredate_clean), nfa = cin$referralnfa)
cin[is.na(cinclosuredate_clean) & referralnfa == 1, cinclosuredate_clean := cinreferraldate_clean]

# Step 15: Deduplication -------------------------------------------------------

# multiple rows with same vals per child for following vars:
dups <- duplicated(cin[, c("lachildid_anon_concat", "acadyr" ,"pupilmatchingrefanonymous", "female", "ethnicity_major",
                           "la", "cinreferraldate_clean", "cinclosuredate_clean",
                           "dateofinitialcpc", "referralnfa",
                           "reasonforclosure", "numberofpreviouscpp",
                           "referralsource", "categoryofabuse", "initialcategoryofabuse",
                           "latestcategoryofabuse", "cppstartdate", "cppenddate")])
table(dups)
cin <- cin[!dups]
rm(dups)

length(unique(cin$lachildid_anon_concat))
length(unique(cin$pupilmatchingrefanonymous))

# Step 16: Miscellaneous ------------------------------------------------------

# referral calendar yr:
cin$refcalyear <- format(cin$cinreferraldate_clean, "%Y")

# academic year (Sep-Aug):
lt <- as.POSIXlt(cin$cinreferraldate_clean)
cin$refayear <- lt$year + (lt$mo >= 8) + 1900
rm(lt)

# financial year (Apr-Mar):
lt <- as.POSIXlt(cin$cinreferraldate_clean)
cin$reffyear <- lt$year + (lt$mo >= 3) + 1900
rm(lt)

# use PMR as ID if non-missing, otherwise child id + la id:
cin[, id_combined := lachildid_anon_concat]

cin[, id_type := "lachildid_anon_concat"]

cin[!is.na(pupilmatchingrefanonymous), id_type := "pupilmatchingrefanonymous"]
cin[!is.na(pupilmatchingrefanonymous), id_combined := pupilmatchingrefanonymous]

length(unique(cin$id_combined))

# Step 17: Calculate age -----------------------------------------------------------

# create a notional DOB so we can calculate approx age
cin <- cin[order(id_combined, cinreferraldate_clean)]
cin[, notional_dob := as.Date(paste0(ym_birth, "-01"))]

# now work out approx age at referral
cin[, age_at_ref_days := as.integer(difftime(cinreferraldate_clean, notional_dob, units = "days"))]

# summary(cin$age_at_ref_days)
# table(cin$age_at_ref_days < -6*31)

# Assume that those referred <6 months before birth are pre-birth referrals otherwise it's an error.
# Fix errors by setting notional_dob to referral date and then fix ym_birth accordingly.
cin[, dob_flag := age_at_ref_days < -6*31]
cin[dob_flag == T, notional_dob := cinreferraldate_clean]

# now need to apply to all episodes per child
cin[, notional_dob := min(notional_dob), by = .(id_combined)] 

# now recalculate ages
cin[, age_at_ref_days := as.integer(difftime(cinreferraldate_clean, notional_dob, units = "days"))]
cin[dob_flag == T, ym_birth := substr(notional_dob, 1, 7)]

# summary(cin$age_at_ref_days)
# table(cin$age_at_ref_days < -6*31)

cin[, dob_flag := NULL]

# can now use expecteddob where notional_dob NA
cin[, expecteddob := as.Date(expecteddob, format = "%Y-%m-%d")]
cin[is.na(notional_dob) & !is.na(expecteddob), notional_dob := expecteddob]
cin[!is.na(notional_dob) & is.na(ym_birth), ym_birth := substr(notional_dob, 1, 7)]
cin[is.na(age_at_ref_days) & !is.na(notional_dob), age_at_ref_days := as.integer(difftime(cinreferraldate_clean, notional_dob, units = "days"))]

# summary(cin$age_at_ref_days)
# table(cin$age_at_ref_days < -6*31)

cin[, dob_flag := age_at_ref_days < -6*31]
cin[dob_flag == T, notional_dob := cinreferraldate_clean]
cin[, notional_dob := min(notional_dob), by = .(id_combined)] 
cin[, age_at_ref_days := as.integer(difftime(cinreferraldate_clean, notional_dob, units = "days"))]
cin[dob_flag == T, ym_birth := substr(notional_dob, 1, 7)]

# summary(cin$age_at_ref_days)
# table(cin$age_at_ref_days < -6*31)

cin[, dob_flag := NULL]

# now check how many greater than 25 - should not be in dataset
# table(cin$age_at_ref_days > 365.25 * 25)
# View(cin[id_combined %in% cin[age_at_ref_days > 365.25 * 25]$id_combined,
#          c("id_combined", "cinreferraldate_clean", "notional_dob", "age_at_ref_days")])

# set to NA those whose first contact >= 25 years on dob and other age variables
cin <- cin[order(id_combined, acadyr, cinreferraldate_clean)]
cin[, row_per_child := seq_len(.N), by = rleid(id_combined)]

cin[, dob_flag := F]
cin[age_at_ref_days > 365.25 * 25 & row_per_child == 1, dob_flag := T]
cin[, dob_flag := max(dob_flag), by = .(id_combined)]

# View(cin[id_combined %in% cin[age_at_ref_days > 365.25 * 25]$id_combined,
#          c("id_combined", "cinreferraldate_clean", "notional_dob", "age_at_ref_days", "dob_flag")])

cin[dob_flag == T, notional_dob := NA]
cin[dob_flag == T, age_at_ref_days := NA]
cin[dob_flag == T, expecteddob := NA]
cin[dob_flag == T, ym_birth := NA]
cin[dob_flag == T, referred_newborn_unborn := NA]

cin[, row_per_child := NULL]
cin[, dob_flag := NULL]

# age in years approx
cin[, age_at_ref_yrs := age_at_ref_days / 365.25]
# summary(cin$age_at_ref_yrs)

# now calculate age groups
cin[, age_grp_at_ref := factor(NA, levels = c("Pre-birth",
                                              "0-4",
                                              "5-9",
                                              "10-15",
                                              "16+"))]
cin[age_at_ref_yrs < 0, age_grp_at_ref := "Pre-birth"]
cin[age_at_ref_yrs >= 0 & age_at_ref_yrs < 5, age_grp_at_ref := "0-4"]
cin[age_at_ref_yrs >= 5 & age_at_ref_yrs < 10, age_grp_at_ref := "5-9"]
cin[age_at_ref_yrs >= 10 & age_at_ref_yrs < 16, age_grp_at_ref := "10-15"]
cin[age_at_ref_yrs >= 16, age_grp_at_ref := "16+"]

# table(cin$age_grp_at_ref, useNA = "always")

cin[, age_yrs_int := trunc(age_at_ref_yrs)]
cin[age_at_ref_yrs < 0, age_yrs_int := -1]
#aggregate(age_at_ref_yrs ~ age_yrs_int, data = cin, FUN = summary)

# create Y of birth based on national DOB
cin[, birthcyear := format(notional_dob, format = "%Y")]

lt <- as.POSIXlt(cin$notional_dob)
cin$birthfyear <- lt$year + (lt$mo >= 3) + 1900
rm(lt)

# table(fyear = cin$birthfyear,
#       cyear = cin$birthcyear,
#       useNA = "always")

# Step 18: Drop redundant variables ------------------------------------------------

cin[, n_mis_start := NULL]
cin[, record_per_child := NULL]
cin[, gender := NULL]
cin[, ethnicity := NULL]
cin[, disability := NULL]
cin[, la_09 := NULL]
cin[, cinreferraldate := NULL]
cin[, latestreferraldate := NULL]
cin[, cinclosuredate := NULL]
cin[, latestclosuredate := NULL]
cin[, expecteddob := NULL]
cin[, primaryneedcode := NULL]

# Step 19: drop missing age and >18yr ------------------------------------

# table(is.na(cin$age_at_ref_yrs))
cin <- cin[!is.na(age_at_ref_yrs)]

# table(cin$age_at_ref_yrs >= 18)
cin <- cin[age_at_ref_yrs < 18]

length(unique(cin$lachildid_anon_concat))
length(unique(cin$pupilmatchingrefanonymous))
length(unique(cin$id_combined))

# Step 20: assign indices -------------------------------------------------

cin <- cin[order(id_combined, cinreferraldate_clean, acadyr, cinclosuredate_clean)]
cin[, row_per_child := seq_len(.N), by = rleid(id_combined)]
cin[, row_per_childepi := seq_len(.N), by = rleid(id_combined, cinreferraldate_clean)]
cin[, epi_index := frank(cinreferraldate_clean, ties.method = "dense"), by = id_combined]


# Step 20: Save --------------------------------------------------------------------

save(cin, file = "processed/cin_processed.rda")
rm(list = ls())
gc()

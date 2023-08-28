# clean the environment
rm(list = ls())

# load required packages
library(tidyverse)    # for general data wrangling
library(readxl)       # for importing .xlsx files
library(fixest)       # for regression analysis
library(xtable)       # for export to LaTeX
library(stargazer)    # for summary stats tables
library(ggpubr)       # for grid charts
library(marginaleffects) # for calculating average partial effects
library(texreg)       # for creating regression tables based on alpaca models

# alpaca package must also be installed but is not fully loaded due to conflicting namespace with fixest
if(!"alpaca" %in% rownames(installed.packages())) stop("Please install the alpaca package before running this script")

# execute a helper function that sets variable labels for the fixest package
source(file.path("src", "utils", "set_fixest_dictionary.r"))

# write a helper function to clean technology names
replace_tech_labels <- function(x) {
  x %>% str_replace("^Thermal$", "CSP") %>%
    str_replace("^PV$", "Solar PV")
}

## load files for replication
# 1) deal-level data set used for regression analysis
df <- readRDS(file.path("input", "df.rds"))
# 2) additional information on lenders for each deal in df
df_lenders <- readRDS(file.path("input", "df_lenders.rds"))
# 3) additional information on SIB activity in df
sib_table <- readRDS(file.path("input", "sib_table.rds"))
# 4) processed IRENA data with technology share's in total installed capacity
df_capshare_irena <- readRDS(file.path("input", "df_capshare_irena.rds"))
# 5) inflation-adjusted feed-in tariff data taken from OECD.Stats
df_fit_oecd <- readRDS(file.path("input", "df_fit_oecd.rds"))

# setting overall table style for fixest
my_style_tex = style.tex(main = "base",
                         fixef.title = '\\midrule',
                         fixef.suffix = " FEs",
                         stats.title = '\\midrule',
                         depvar.title = "",
                         model.title = "",
                         var.title = "\\midrule",
                         adjustbox = "width = 1\\textwidth, height = 0.5\\textheight, center",
                         fontsize = "small",
                         signif.code = c("***"=0.001, "**"=0.01, "*"=0.05, "\\dagger"=0.10))

# set fixest defaults for etable
setFixest_etable(
  fitstat = c("n", "pr2", "bic"),
  style.tex = my_style_tex,
  digits = 3,
  digits.stats = 3
)


################################################################################
#################### TABLES IN THE MAIN MANUSCRIPT #############################
################################################################################

### Table 1
table1 <- sib_table %>%
  # as sib_table also features SIB subsidiaries, we group by parent and collect the BNEF Organization IDs as well as unique deal IDs
  # NOTE: we use unique deal IDs instead of a simple sum to avoid double-counting in case both the parent and the subsidiary are active on the same deal
  group_by(main_institution, Country) %>%
  summarise(bnef_ids = paste0(Organization_ID, collapse = ", "),
            no_of_transactions = length(c(unlist(transaction_ids_in_df_reg)) %>% unique())) %>%
  # sort by # of deals and rearrange columns
  arrange(desc(no_of_transactions)) %>%
  select(Organization = "main_institution", Country, bnef_ids, no_of_transactions)

# print out as LaTeX table
table1 %>% xtable()

# clean the environment
rm(table1)


### Table 2

# convert data from tibble to data.frame for stargazer() and drop numerical variables only featuring indirectly in robustness checks
df_stargazer <- as.data.frame(df %>% select(-c(Asset_Finance_ID,
                                               capshare_irena,
                                               countrytech_withdebt_first1,
                                               countrytech_withdebt_first5,
                                               countrytech_withdebt_first10,
                                               countrytech_withdebt_first25,
                                               is_lender_notreported,
                                               sib_lending_debtshare,
                                               no_lenders)))

# replace names in df_stargazer based on my_fixest_dict dictionary for variable names
for(jj in 1:length(my_fixest_dict)) {
  names(df_stargazer)[names(df_stargazer) == names(my_fixest_dict)[jj]] <- my_fixest_dict[jj]
}

# write out the summary stats table using the stargazer() function
stargazer::stargazer(df_stargazer, median = T, omit.summary.stat = c("p25", "p75"),
          type = "latex", label = "tab:summarystats",
          title = "Summary statistics")

# clean up the environment
rm(df_stargazer, jj)


### Table 3
fixest::did_means(fml = . ~ is_sib_len, base = df %>% select(-c(Asset_Finance_ID,
                                                                capshare_irena,
                                                                countrytech_withdebt_first1,
                                                                countrytech_withdebt_first5,
                                                                countrytech_withdebt_first10,
                                                                countrytech_withdebt_first25,
                                                                is_lender_notreported,
                                                                sib_lending_debtshare,
                                                                no_lenders)), tex = T)


### Table 4

## define the regression formula elements
# 1) the core regressors
base_formula <- "is_sib_len ~ countrytech_withdebt_first3 + log(fin_Capacity_total_MW) + fin_capacity_techyear_decile1 + no_lenders_wo_sibs + gdp_growth + oecd_feedintariff + has_public_sponsor + is_fin_termloan"
# 2) the technology dummy and the maturity dummy (which we only use combined with tech FEs)
tech_fe <- "+ i(Sector_final, ref = 'Onshore') + is_mature"
# 3) alternatively, the technology dummy and the maturity dummy interacted with onshore wind and solar PV
tech_fe_heterogeneous <- "+ i(Sector_final, ref = 'Onshore') + i(Sector_final, is_mature, keep = c('Onshore', 'PV'))"
# 4) country and year FEs
country_year_fes <- "| year_of_close_chr + proj_Country"

# combine these formula elements to build the formulas used in Table 4's specifications
table4_formulas <- list("(1)" = paste(base_formula),
                        "(2)" = paste(base_formula, country_year_fes),
                        "(3)" = paste(base_formula, tech_fe, country_year_fes),
                        "(4)" = paste(base_formula, tech_fe_heterogeneous, country_year_fes))

# estimate the logit model for each specification using st.err. clustered at the year level
table4_models <- map(table4_formulas, ~ fixest::feglm(fml = as.formula(.x),
                                     data = df,
                                     family = "binomial",
                                     vcov = ~ year_of_close_chr))

# print out the regression table (uncomment the tex = TRUE part to get LaTeX output)
etable(table4_models, #tex = TRUE,
       title = "Regression results for the main specification"
)



### replicate Table 5

## create alternative specification formulas
# 1) replace log(capacity) by a 2nd-order polynomial of absolute capacity
base_formula_squaredcapacity <- base_formula %>% str_replace("log\\(fin_Capacity_total_MW\\)", "fin_Capacity_total_MW + fin_Capacity_total_MW_sq")
# 2) replace log(capacity) and the first-decile dummy with capacity decile bins, using the 6th decile as the baseline
base_formula_capacitydeciles <- base_formula %>% str_replace("log\\(fin_Capacity_total_MW\\) \\+ fin_capacity_techyear_decile1", "i(fin_capacity_techyear_decilebin, ref = '6')")

# combine these formula elements to build the formulas used in Table 5's specifications
table5_formulas <- list("(1)" = paste(base_formula_squaredcapacity, tech_fe, country_year_fes),
                        "(2)" = paste(base_formula_squaredcapacity, tech_fe_heterogeneous, country_year_fes),
                        "(3)" = paste(base_formula_capacitydeciles, tech_fe, country_year_fes),
                        "(4)" = paste(base_formula_capacitydeciles, tech_fe_heterogeneous, country_year_fes))

# estimate the logit model for each specification using st.err. clustered at the year level
table5_models <- map(table5_formulas, ~ fixest::feglm(fml = as.formula(.x),
                                                      # create the squared capacity term for the 2nd-order polynomial in the formulas
                                                      data = df %>% mutate(fin_Capacity_total_MW_sq = fin_Capacity_total_MW^2),
                                                      family = "binomial",
                                                      vcov = ~ year_of_close_chr))

# print out the regression table (uncomment the tex = TRUE part to get LaTeX output)
etable(table5_models, #tex = TRUE,
       title = "Additional specifications for size effects"
)

# clean up the environment
rm(table5_formulas, table5_models)


### Table 6

# combine formula elements to build the formulas used in Table 6's specifications
table6_formulas <- list("(1)" = paste(base_formula),
                        "(2)" = paste(base_formula, tech_fe, country_year_fes),
                        "(3)" = paste(base_formula, tech_fe_heterogeneous, country_year_fes))

# estimate these models on a subset that does not feature SIB solo-lending, using year level clustering for st.err.
table6_models <- map(table6_formulas, ~ fixest::feglm(fml = as.formula(.x),
                                                      # remove observations with SIB lending & no further lenders (= solo-lending)
                                                      data = df %>% filter(!(is_sib_len & no_lenders_wo_sibs == 0)),
                                                      family = "binomial",
                                                      vcov = ~ year_of_close_chr))

# print out the regression table (uncomment the tex = TRUE part to get LaTeX output)
etable(table6_models, #tex = TRUE,
       title = "Regression results for lender mobilization"
)


################################################################################
#################### TABLES IN THE APPENDIX ####################################
################################################################################

### Table B1
etable(table4_models, #tex = TRUE,
       vcov = ~ proj_Country)


### Table B2

# check which interaction terms are not perfectly separated (once we adjust for reductions in sample sizes due to missing values)
df %>% mutate(is_mature_alltechnologies = capshare_irena >= 0.1) %>%
  # filter out rows with NAs in one of the main regressors that is not available for the entire sample (see Table 2)
  filter(!is.na(fin_Capacity_total_MW) & !is.na(has_public_sponsor) & !is.na(oecd_feedintariff)) %>%
  # count different combinations of the tech dummy, maturity dummy, and SIB lending dummy
  count(Sector_final, is_mature_alltechnologies, is_sib_len) %>%
  # without perfect separation, there should be 4 cases per technology - filter for technologies that have fewer cases
  group_by(Sector_final) %>% mutate(n_cases = n()) %>% filter(n_cases < 4)
# -> CSP aka "Thermal" never reaches maturity, while for small hydro, immaturity perfectly identifies lack of SIB lending

## create additional formula elements for robustness check of the tech maturity dummy
# 1) splitting PV into before and after 2010
tech_fe_pvsplit <- "+ i(Sector_final_pvsplit, ref = 'Onshore') + i(Sector_final_pvsplit, is_mature, keep = c('Onshore', 'PVfrom2010on'))"
# 2) applying the 10% maturity threshold to all technologies, not just onshore & PV
tech_fe_alltechnologies <- "+ i(Sector_final, ref = 'Onshore') + is_mature_alltechnologies"
tech_fe_alltechnologies_heterogeneous <- "+ i(Sector_final, ref = 'Onshore') + i(Sector_final, is_mature_alltechnologies, keep = c('Onshore', 'Biomass & Waste', 'PV', 'Offshore', 'Geothermal'))"
# 3) using the share in national installed capacity directly instead of the threshold version
tech_fe_capshare <- "+ i(Sector_final, ref = 'Onshore') + capshare_irena"
tech_fe_capshare_heterogeneous <- "+ i(Sector_final, ref = 'Onshore') + i(Sector_final, capshare_irena, keep = c('Onshore', 'PV'))"

# create the formula list for Table B2
tableB2_formulas <- list("(1)" = paste(base_formula, tech_fe_heterogeneous, country_year_fes),
                         "(2)" = paste(base_formula, tech_fe_pvsplit, country_year_fes),
                         "(3)" = paste(base_formula, tech_fe_alltechnologies, country_year_fes),
                         "(4)" = paste(base_formula, tech_fe_alltechnologies_heterogeneous, country_year_fes),
                         "(5)" = paste(base_formula, tech_fe_capshare, country_year_fes),
                         "(6)" = paste(base_formula, tech_fe_capshare_heterogeneous, country_year_fes)
                         )

# estimate the models using st.err. clustered at the year level
tableB2_models <- map(tableB2_formulas, ~ fixest::feglm(fml = as.formula(.x),
                                                      # create a technology dummy where PV is split & a maturity dummy that is applied to all RE technologies
                                                      data = df %>% mutate(Sector_final_pvsplit = case_when(Sector_final == "PV" & year_of_close < 2010 ~ "PVbefore2010",
                                                                                                            Sector_final == "PV" & year_of_close >= 2010 ~ "PVfrom2010on",
                                                                                                            TRUE ~ as.character(Sector_final)),
                                                                           is_mature_alltechnologies = capshare_irena >= 0.1),
                                                      family = "binomial",
                                                      vcov = ~ year_of_close_chr))

# print out the regression table (uncomment the tex = TRUE part to get LaTeX output)
etable(tableB2_models, #tex = TRUE,
       title = "Robustness check for alternative measures of market maturity"
)

# clean up the environment
rm(tableB2_formulas, tableB2_models,
   tech_fe_alltechnologies, tech_fe_alltechnologies_heterogeneous, tech_fe_capshare, tech_fe_capshare_heterogeneous, tech_fe_pvsplit)


### Table B3

# create the regression formulas by replacing the First-3 dummy with different First-k variations
tableB3_formulas <- list("(1)" = paste(base_formula %>% str_replace("countrytech_withdebt_first3", "countrytech_withdebt_first1"), tech_fe, country_year_fes),
                         "(2)" = paste(base_formula %>% str_replace("countrytech_withdebt_first3", "countrytech_withdebt_first3"), tech_fe, country_year_fes),
                         "(3)" = paste(base_formula %>% str_replace("countrytech_withdebt_first3", "countrytech_withdebt_first5"), tech_fe, country_year_fes),
                         "(4)" = paste(base_formula %>% str_replace("countrytech_withdebt_first3", "countrytech_withdebt_first10"), tech_fe, country_year_fes),
                         "(5)" = paste(base_formula %>% str_replace("countrytech_withdebt_first3", "countrytech_withdebt_first25"), tech_fe, country_year_fes)
)

# estimate the logit models, using st.err. clustered at the year level
tableB3_models <- map(tableB3_formulas, ~ fixest::feglm(fml = as.formula(.x),
                                                        data = df,
                                                        family = "binomial",
                                                        vcov = ~ year_of_close_chr))

# print out the regression table (uncomment the tex = TRUE part to get LaTeX output)
etable(tableB3_models, #tex = TRUE,
       title = "Main specification - using different # of first deals in the country providing debt to a given technology"
)

# clean up the environment
rm(tableB3_formulas, tableB3_models)


### Table B4

# define the additional control variables included in the specification for Table B4, column 4
additional_controls <- "+ no_sponsors + ccpi_overall_score + oecd_ltinterest_perc + gfd_bankzscore + wdi_govexpend_percgdp + wdi_primarybal_percgdp"

# define the different formulas for the robustness checks in Table B4
tableB4_formulas <- list("(1)" = paste(base_formula, tech_fe_heterogeneous, country_year_fes),
                         "(2)" = paste(base_formula %>% str_remove(" \\+ gdp_growth"), tech_fe_heterogeneous, "| year_of_close_chr^proj_Country"),
                         "(3)" = paste(base_formula, "+ i(Sector_final, is_mature, keep = c('Onshore', 'PV'))", "| proj_Country + year_of_close_chr^Sector_final"),
                         "(4)" = paste(base_formula, additional_controls, tech_fe_heterogeneous, country_year_fes),
                         "(5)" = paste(base_formula, tech_fe_heterogeneous, country_year_fes)
)

# estimate the models for columns 1-4
tableB4_models <- map(tableB4_formulas[1:4], ~ fixest::feglm(fml = as.formula(.x),
                                                        data = df,
                                                        family = "binomial",
                                                        vcov = ~ year_of_close_chr))

# add the model for column 5, which uses a smaller subsample (excluding deals whose lenders BNEF lists as "Not Reported")
tableB4_models[["(5)"]] <- fixest::feglm(fml = as.formula(tableB4_formulas[["(5)"]]),
                                         data = df %>% filter(!is_lender_notreported),
                                         family = "binomial",
                                         vcov = ~ year_of_close_chr)

# print out the regression table (uncomment the tex = TRUE part to get LaTeX output)
etable(tableB4_models, #tex = TRUE,
       title = "Robustness check for the main specification (market maturity interacted with individual technologies)"
)

# clean up the environment
rm(tableB4_models)


### Table B5

# estimate the same specifications as in Table B4 but for the subsample excluding deals with SIBs as the sole lender
tableB5_models <- map(tableB4_formulas[1:4], ~ fixest::feglm(fml = as.formula(.x),
                                                             data = df %>% filter(!(is_sib_len & no_lenders_wo_sibs == 0)),
                                                             family = "binomial",
                                                             vcov = ~ year_of_close_chr))
tableB5_models[["(5)"]] <- fixest::feglm(fml = as.formula(tableB4_formulas[["(5)"]]),
                                         data = df %>% filter(!(is_sib_len & no_lenders_wo_sibs == 0)) %>%
                                                       filter(!is_lender_notreported),
                                         family = "binomial",
                                         vcov = ~ year_of_close_chr)

# print out the regression table (uncomment the tex = TRUE part to get LaTeX output)
etable(tableB5_models, #tex = TRUE,
       title = "Robustness check for the main specification (market maturity interacted with individual technologies) with deals with SIBs as the only lender(s) omitted"
)

rm(tableB5_models, tableB4_formulas)


### Table B6 

# NOTE: for the bias-corrected estimator, we use the alpaca package's feglm function for estimating models paired with the biasCorr function

# estimate the main specification (Table 4, column 3), replacing the fixest package's i() function in the formula, and apply bias correction
tableB6_models <- list()
tableB6_models[["(1)"]] <- alpaca::biasCorr(alpaca::feglm(formula = table4_formulas[3] %>% str_replace("i\\(Sector_final.+\\)", "Sector_final") %>%
                                                            as.formula(),
                                          data = df,
                                          family = binomial("logit"))
)

# repeat for the specification in Table 4, column 4
# NOTE: we create interaction terms between is_mature & the RE technology dummies manually
tableB6_models[["(2)"]] <- alpaca::biasCorr(alpaca::feglm(formula = table4_formulas[3] %>% str_replace("i\\(Sector_final.+\\)", "Sector_final") %>%
                                                            str_replace("is_mature", "is_mature_pv + is_mature_onshore") %>% as.formula(),
                                                          data = df %>% mutate(is_mature_pv = is_mature & Sector_final == "PV",
                                                                               is_mature_onshore = is_mature & Sector_final == "Onshore"),
                                                          family = binomial("logit"))
)
# estimate the same two specifications with bias correction on the subsample where deals with SIB being the sole lender are excluded
tableB6_models[["(3)"]] <- alpaca::biasCorr(alpaca::feglm(formula = table4_formulas[3] %>% str_replace("i\\(Sector_final.+\\)", "Sector_final") %>% 
                                                            as.formula(),
                                                          data = df %>% filter(!(is_sib_len & no_lenders_wo_sibs == 0)),
                                                          family = binomial("logit"))
)
tableB6_models[["(4)"]] <- alpaca::biasCorr(alpaca::feglm(formula = table4_formulas[3] %>% str_replace("i\\(Sector_final.+\\)", "Sector_final") %>%
                                                            str_replace("is_mature", "is_mature_pv + is_mature_onshore") %>% as.formula(),
                                                          data = df %>% mutate(is_mature_pv = is_mature & Sector_final == "PV",
                                                                               is_mature_onshore = is_mature & Sector_final == "Onshore") %>%
                                                            filter(!(is_sib_len & no_lenders_wo_sibs == 0)),
                                                          family = binomial("logit"))
)

# print out the regression table
texreg::texreg(# extract the regression models using the texreg package and specify the clustering for standard errors
               map(tableB6_models, ~ texreg::extract(.x, cluster = ~ year_of_close_chr, type = "clustered",
                                                      include.groups = F, include.deviance = F)),
               caption = "Main specification results using the bias-corrected FE estimator by Fernández-Val & Weidner (2016)",
               fontsize = "small",
               # include rows for FEs
               custom.gof.rows = list("Country FEs" = rep("Yes", 4),
                                      "Closing year FEs" = rep("Yes", 4)
               ),
               # define the significance stars
               stars = c(0.001, 0.01, 0.05, 0.1),
               custom.model.names = c("Main", "Main", "Excl. solo-lending", "Excl. solo-lending"),
               digits = 3)

# clean up the environment
rm(tableB6_models)


### Table B7

# define the two specifications used in Table B7
tableB7_formulas <- list("(1)" = paste(base_formula, tech_fe, country_year_fes),
                         "(2)" = paste(base_formula, tech_fe_heterogeneous, country_year_fes))

# identify all countries that have less than 25 deals in our full sammple
countries_with_below_25_deals <- df %>% count(proj_Country) %>% filter(n < 25) %>% pull(proj_Country)

# estimate the two specifications on the sample excluding all countries with < 25 obs.
tableB7_models <- map(tableB7_formulas, ~ fixest::feglm(fml = as.formula(.x),
                                                     data = df %>% filter(!proj_Country %in% countries_with_below_25_deals),
                                                     family = "binomial",
                                                     vcov = ~ year_of_close_chr))

# estimate the same two specifications on a subsample that also excludes deals with SIBs as the sole lender
tableB7_models[["(3)"]] <- fixest::feglm(fml = as.formula(tableB7_formulas[["(1)"]]),
                                         data = df %>% filter(!proj_Country %in% countries_with_below_25_deals) %>%
                                           filter(!(is_sib_len & no_lenders_wo_sibs == 0)),
                                         family = "binomial",
                                         vcov = ~ year_of_close_chr)
tableB7_models[["(4)"]] <- fixest::feglm(fml = as.formula(tableB7_formulas[["(2)"]]),
                                         data = df %>% filter(!proj_Country %in% countries_with_below_25_deals) %>%
                                           filter(!(is_sib_len & no_lenders_wo_sibs == 0)),
                                         family = "binomial",
                                         vcov = ~ year_of_close_chr)

# print out the regression table (uncomment the tex = TRUE part to get LaTeX output)
etable(tableB7_models, #tex = TRUE,
       title = "Main specification results excl. countries with less than 25 obs. (Colombia, Costa Rica, Denmark, Estonia, Iceland, Israel, Latvia, Lithuania, New Zealand, Norway, Slovakia, Slovenia, Switzerland)"
)

# clean up the environment
rm(tableB7_formulas, tableB7_models)


### Table C1

# regress real GDP growth in the sample on closing year dummies, with 2008 as the baseline
tableC1_models <- fixest::feglm(fml = gdp_growth ~ i(year_of_close_chr, ref = "2008") - 1,
              # select all unique year-growth combinations in the data as the sample for the regression
              data = df %>% select(proj_Country, gdp_growth, year_of_close_chr) %>% distinct(),
              family = "gaussian",
              vcov = "iid")

# print out the regression table (uncomment the tex = TRUE part to get LaTeX output)
etable(tableC1_models, #tex = TRUE,
       title = "Regressing GDP growth on year FEs")

# clean up the environment
rm(tableC1_models)


### Table C2

# regress the dep. var. on a dummy indicating missingness in the main regressors
tableC2_models <- fixest::feglm(fml = is_sib_len ~ has_missingness,
                                # create the missingness dummy
                                data = df %>% mutate(has_missingness = is.na(oecd_feedintariff) | is.na(fin_Capacity_total_MW) | is.na(has_public_sponsor)),
                                family = "binomial",
                                vcov = ~ year_of_close_chr)

# print out the regression table (uncomment the tex = TRUE part to get LaTeX output)
etable(tableC2_models, #tex = TRUE,
       title = "Regressing SIB lending on dummy indicating missing values in key regressors (FiT, financed generation capacity, public sector sponsor)")

# clean up the environment
rm(tableC2_models)


### Table D1

# replace the dep.var. in the formulas for Table 4 with the share of SIBs in the overall debt ('sib_lending_debtshare')
tableD1_formulas <- map(table4_formulas, ~ str_replace(.x, "^is_sib_len", "sib_lending_debtshare"))

# estimate the logit models, with st.err. clustered at the year level
tableD1_models <- map(tableD1_formulas, ~ fixest::feglm(fml = as.formula(.x),
                                                      # impute unknown debt shares of SIB with 1/# of lenders
                                                      data = df %>% mutate(sib_lending_debtshare = if_else(is.na(sib_lending_debtshare),
                                                                                                           1/no_lenders,
                                                                                                           sib_lending_debtshare)),
                                                      family = "binomial",
                                                      vcov = ~ year_of_close_chr))

# print out the regression table (uncomment the tex = TRUE part to get LaTeX output)
etable(tableD1_models, #tex = TRUE,
       title = "Regression results for the share of SIB lending in total debt (fractional response), imputing unknown shares with 1/(# of lenders)")

# clean up the environment
rm(tableD1_models)


### Table D2

# estimate the same logit models as in Table D1 but without imputing unknown debt shares
tableD2_models <- map(tableD1_formulas, ~ fixest::feglm(fml = as.formula(.x),
                                                        data = df %>% filter(SIB_lending_certainty %in% c("Known", "Unclear allocation across SIBs, known total") | !is_sib_len),
                                                        family = "binomial",
                                                        vcov = ~ year_of_close_chr))

# print out the regression table (uncomment the tex = TRUE part to get LaTeX output)
etable(tableD2_models, #tex = TRUE,
       title = "Regression results for the share of SIB lending in total debt (fractional response), omitting deals with unknown SIB share")


### clean up the environment before proceeding to the figures
rm(tableD2_models, tableD1_formulas,
   base_formula, base_formula_capacitydeciles, base_formula_squaredcapacity, additional_controls,
   country_year_fes, tech_fe, tech_fe_heterogeneous,
   countries_with_below_25_deals)


################################################################################
########################### FIGURES ############################################
################################################################################

# set the overall ggplot theme to theme_classic()
theme_set(theme_classic())

# define the colour code for SIB lending across all figures
sib_colour <- "#0057A6"


### Figure 1

# plot # of observations with & without SIB involvement by each closing year
figure1_1 <- df %>%
  # count all deals with and without SIB involvement for each closing year
  count(is_sib_len, year_of_close) %>%
  # for each year, calculate the total # of obs and the share of deals with SIBs in it
  group_by(year_of_close) %>% mutate(n_year = sum(n),
                                     n_sib = sum(n[is_sib_len]),
                                     sib_share = n_sib/n_year,
                                     # create a clean label for the share of deals with SIBs being involved
                                     sib_share_label = if_else(is_sib_len,
                                                               paste0(100*round(sib_share, 2), "%"),
                                                               "")) %>% ungroup() %>%
  # make a bar chart with year on the x-axis, # of deals on the y-axis, and bars coloured to represent with/without SIB involvement
  ggplot(aes(year_of_close, n)) + geom_col(aes(fill = is_sib_len)) +
  # label chart elements manually
  labs(x = "Closing year", y = "No. of transactions", fill = "SIB lending?") +
  # add value labels for the share of deals with SIBs being involved
  geom_text(aes(label = sib_share_label), nudge_y = 25, colour = sib_colour, size = 3) +
  # position the legend & omit its title
  theme(legend.position = c(0.25, 0.65), legend.title = element_blank()) +
  # set bar colours manually
  scale_fill_manual(values = c("grey", sib_colour)) +
  # omit the legend (which will feature in a different element of the overall grid chart)
  theme(legend.position = "none")
figure1_1

# plot # of observations with & without SIB involvement for each project location country
figure1_2 <- df %>%
  # count all deals with and without SIB involvement for each project country
  count(is_sib_len, proj_Country) %>%
  # for each country, calculate the total # of obs and the share of deals with SIBs in it
  group_by(proj_Country) %>% mutate(proj_Country = proj_Country %>% str_replace("Korea \\(Republic\\)", "Korea"),
                                    n_country = sum(n),
                                     n_sib = sum(n[is_sib_len]),
                                     sib_share = n_sib/n_country,
                                    # create a clean label for the share of deals with SIBs being involved
                                     sib_share_label = if_else(is_sib_len,
                                                               paste0(100*round(sib_share, 2), "%"),
                                                               "")) %>% ungroup() %>%
  # if there are no deals with SIB involvement in a country, add a manual "0%" label
  mutate(sib_share_label = if_else(n_sib == 0, "0%", sib_share_label)) %>%
  # make a bar chart with country on the x-axis, # of deals on the y-axis, and bars coloured for with/without SIB involvement
  ggplot(aes(reorder(proj_Country, n_country), n)) + geom_col(aes(fill = is_sib_len)) +
  # label chart elements manually
  labs(x = NULL, y = "No. of transactions", fill = "SIB lending?") +
  # add value labels for the share of deals in which SIBs are involved
  # NOTE: for countries with small N, we print the label next to the total bar instead of next to the bar for deals with SIB involvement
  geom_text(aes(y = ifelse(n_country < 161, n_country + 75, n + 75), label = sib_share_label), nudge_y = 25, colour = sib_colour, size = 3) +
  # position the legend & omit its title
  theme(legend.position = c(0.25, 0.65), legend.title = element_blank()) +
  # set bar colours manually
  scale_fill_manual(values = c("grey", sib_colour)) +
  # omit the legend (which will feature in a different element of the overall grid chart)
  theme(legend.position = "none") +
  # flip x- & y-axis of the chart to improve readability of country names
  coord_flip()
figure1_2

# plot # of observations with & without SIB involvement for each RE technology
figure1_3 <- df %>%
  # count all deals with and without SIB involvement for each technology
  count(is_sib_len, Sector_final) %>%
  # for each technology, calculate the total # of observations and the share of deals with SIBs in them
  group_by(Sector_final) %>% mutate(Sector_final = replace_tech_labels(Sector_final),
                                    n_tech = sum(n),
                                    n_sib = sum(n[is_sib_len]),
                                    sib_share = n_sib/n_tech,
                                    # create a clean label for the share of deals with SIBs being involved
                                    sib_share_label = if_else(is_sib_len,
                                                              paste0(100*round(sib_share, 2), "%"),
                                                              ""),
                                    # create a clean label for I(SIB lending) dummy for the legend
                                    is_sib_len = if_else(is_sib_len, "with SIB lending -\n% = share of deals involving SIBs", "no SIB lending"),
  ) %>% ungroup() %>%
  # make a bar chart with technology on the x-axis, # of deals on the y-axis, and bars coloured to represent with/without SIB involvement
  ggplot(aes(reorder(Sector_final, n_tech), n)) + geom_col(aes(fill = is_sib_len)) +
  # label chart elements manually
  labs(x = NULL, y = "No. of transactions", fill = "SIB lending?") +
  # add value labels for the share of deals with SIBs being involved
  # NOTE: for technologies with small N, we print the label next to the total bar instead of next to the bar for deals with SIB involvement
  geom_text(aes(label = sib_share_label,
                y = if_else(Sector_final %in% c("Solar PV", "Onshore", "Biomass & Waste"),
                            n_sib + 90,
                            n_tech + 90)), colour = sib_colour, size = 3) +
  # set bar colours manually & reverse their order in the legend
  scale_fill_manual(values = c("grey", sib_colour), guide = guide_legend(reverse = T)) +
  # position the legend & omit its title
  theme(legend.position = c(0.7, 0.25), legend.title = element_blank()) +
  # flip x- & y-axis of the chart to improve readability of technology names
  coord_flip()

# combine the three charts into a single grid chart
ggarrange(
  # left panel: 2x1 combination of year & technology chart
  ggarrange(figure1_1,
            figure1_3,
            nrow = 2, ncol = 1, align = "hv",
            labels = c("a", "c")),
  # right panel: the project country chart
  figure1_2,
  nrow = 1, ncol = 2,
  # set the relative widths of the two panels manually
  widths = c(0.65, 0.35),
  # left panel is already labeled above, so no further label required here
  labels = c("", "b")
)

# save out as PDF file with a date stamp
ggsave(file.path("figures", paste0(Sys.Date(), " Figure1.pdf")), width = 10, height = 6)

# clean up the environment
rm(figure1_1, figure1_2, figure1_3)


### Figure 2

# identify the 6 countries with the highest # of PV deals in-sample, conditional on having reached maturity over the sample period
top6_countries_pv <- df %>%
  # subset to PV deals
  filter(Sector_final == "PV") %>%
  # for each country, indicate if it has reached maturity before the end of our sample
  group_by(proj_Country) %>% mutate(reaches_maturity = sum(is_mature) > 0) %>% ungroup() %>%
  # subset to countries that have reached maturity
  filter(reaches_maturity) %>%
  # count the # of deals by country and slice to the top6
  count(proj_Country) %>% slice_max(order_by = n, n = 6) %>%
  # extract the country names only as a character vector
  pull(proj_Country)
top6_countries_pv

# plot the # of PV deals by country as a bar chart over time, with bar colours indicating deals with/without SIB involvement
figure2_1 <- df %>%
  # subset to PV & the top6 countries
  filter(Sector_final == "PV", proj_Country %in% top6_countries_pv) %>%
  # count the # of deals with and without SIB involvement by year and country
  count(is_sib_len, proj_Country, year_of_close)  %>%
  # create a clean label for the I(SIB lending) dummy variable for the legend
  mutate(is_sib_len = if_else(is_sib_len, "with SIB lending", "no SIB lending")) %>%
  # make a bar chart with year on the x-axis, # of deals on the y-axis, and bars coloured for with/without SIB involvement
  ggplot(aes(year_of_close, n)) + geom_col(aes(fill = is_sib_len)) +
  # for each top6 country, extract the earliest year with market maturity and insert a vertical line right before
  geom_vline(data =  df %>% filter(Sector_final == "PV", proj_Country %in% top6_countries_pv, is_mature) %>%
               group_by(proj_Country) %>% slice_min(year_of_close, n = 1, with_ties = F),
             aes(xintercept = year_of_close - 0.5),
             colour = "darkred",
             linetype = "dashed") +
  # add an invisible line (via alpha = 0) that will prompt a legend item for the maturity line
  geom_hline(aes(yintercept = 1, linetype = "Reaching solar PV market maturity (based on IRENA, 2023)"),
             colour = "darkred", alpha = 0) +
  # set bar colours manually & set their order in the legend
  scale_fill_manual(values = c("grey", sib_colour), guide = guide_legend(reverse = T, order = 1)) +
  # set the number of y-axis breaks
  scale_y_continuous(n.breaks = 3) +
  # set the linetype manually & set their order in the legend
  scale_linetype_manual(values = c("dashed"),
                        guide = guide_legend(override.aes = list(alpha = 1), order = 2)) +
  # create a facet for each country with a 3x2 format and without holding the y-axis breaks constant across facets
  facet_wrap(~proj_Country, nrow = 3, scales = "free_y") +
  # label chart elements manually & omit legend titles for fill & linetype
  labs(x = "Closing year", y = "No. of transactions", fill = NULL, linetype = NULL) +
  # set additional aesthetics of the legend
  theme(legend.position = "bottom",
        legend.box = "vertical",
        strip.background = element_blank(),
        legend.margin = margin(-0.25, -0.5, 0, 0, unit = "cm"),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7 + 1)) +
  # make x-axis always visible even when using facet_wrap by creating a segment manually
  annotate("segment", x=-Inf, xend = Inf, y = -Inf, yend = -Inf)
figure2_1

# identify the most active SIB for each country's PV sector
sib_table %>%
  # for each SIB, create a data frame with 1 row per deal by unnesting the 'transaction_ids_in_df_reg' column that stores deal IDs
  # NOTE: we use distinct() to avoid double counts if both the main SIB & one of its subsidiaries are involved in the same deal
  select(main_institution, transaction_ids_in_df_reg) %>% unnest(transaction_ids_in_df_reg) %>% distinct() %>%
  # for each deal, merge in the RE technology and the project country
  left_join(df %>% select(Asset_Finance_ID, Sector_final, proj_Country), by = c("transaction_ids_in_df_reg" = "Asset_Finance_ID")) %>%
  # subset to the top6 countries & the solar PV sector
  filter(proj_Country %in% top6_countries_pv, Sector_final == "PV") %>%
  # count the lending activity of each SIB & for each country, identify the most active SIB
  count(proj_Country, main_institution) %>% group_by(proj_Country) %>% slice_max(order_by = n, n = 1, with_ties = T)

# compile additional information on the main SIBs in the top6 countries' PV sector
top6_countries_pv_sib_activity <- sib_table %>%
  # subset to the institutions identified above
  filter(main_institution %in% c("Kreditanstalt fuer Wiederaufbau", "Korea Development Bank/The",
                                 "European Investment Bank", "Development Bank of Japan Inc",
                                 "BPIFrance SA"),
         ) %>%
  # for each SIB, create a data frame with 1 row per deal by unnesting the 'transaction_ids_in_df_reg' column that stores deal IDs
  # NOTE: we use distinct() to avoid double counts if both the main SIB & one of its subsidiaries are involved in the same deal
  select(main_institution, transaction_ids_in_df_reg) %>% unnest(transaction_ids_in_df_reg) %>% distinct() %>%
  # for each deal, merge in the RE technology, the project country & the closing year
  left_join(df %>% select(Asset_Finance_ID, Sector_final, proj_Country, year_of_close), by = c("transaction_ids_in_df_reg" = "Asset_Finance_ID")) %>%
  # for each SIB, merge in the organization's country
  left_join(sib_table %>% filter(Organization_Name %in% c("Kreditanstalt fuer Wiederaufbau", "Korea Development Bank/The",
                                                                "European Investment Bank", "Development Bank of Japan Inc",
                                                                "BPIFrance SA")) %>%
              select(Organization_Name, Country), by = c("main_institution" = "Organization_Name")) %>%
  # classify deals as domestic if the project country equals the SIB country
  mutate(is_domestic = proj_Country == Country)

# show that the EIB's PV activities in the sample are EU+UK only, i.e., domestic
top6_countries_pv_sib_activity %>% filter(main_institution == "European Investment Bank", Sector_final == "PV") %>%
  count(proj_Country)

# create a subset of top6_countries_pv_sib_activity with only solar PV deals & additional clean label variables
df_figure2_2 <- top6_countries_pv_sib_activity %>%
  # subset to solar PV deals
  filter(Sector_final == "PV") %>%
  # include the respective SIB's country in the 'main_institution' variable, setting it manually to 'EU' for the EIB
  mutate(main_institution = if_else(main_institution == "European Investment Bank",
                                    paste0(main_institution, "\n(EU)"),
                                    paste0(main_institution, "\n(", Country, ")") %>% str_remove(" \\(Republic\\)") %>%
                                      str_remove("\\/The")),
         # create a clean version of the is_domestic dummy for the chart
         is_domestic = factor(if_else(is_domestic | main_institution == "European Investment Bank\n(EU)", "Domestic", "Non-domestic"),
                              levels = c("Non-domestic", "Domestic"))) %>%
  # for each SIB, count the # of deals by closing year & domestic/foreign location
  count(main_institution, is_domestic, year_of_close)
  
# create a point chart for SIB PV activity by closing year, for each SIB & separately for domestic & foreign deals
figure2_2 <- df_figure2_2 %>%
  # display SIB organization labels on the y-axis
  ggplot(aes(y = main_institution)) +
  # include a vertical line segment marking the year when the solar PV market reaches maturity according to the IRENA 2023 threshold
  geom_errorbar(data = df %>%
                        # subset to solar PV, the top-6 countries & markets classified as mature
                        filter(Sector_final == "PV", proj_Country %in% top6_countries_pv, is_mature) %>%
                        # remove Spain & Italy where EIB is largest institution such that there is no single year for maturity
                        filter(!proj_Country %in% c("Italy", "Spain")) %>%
                        # extract the earliest closing year in our sample
                        # NOTE: this works because there are no years without any PV deals right before/after the moment of reaching maturity in our sample, see Figure 2a
                        group_by(proj_Country) %>% slice_min(year_of_close, n = 1, with_ties = F) %>%
                  # recreate the 'main_institution' variable such that the vertical line will differ by SIB/home market (on the y-axis)
                  mutate(main_institution = case_when(proj_Country == "Germany" ~ "Kreditanstalt fuer Wiederaufbau\n(Germany)",
                                                      proj_Country == "France" ~ "BPIFrance SA\n(France)",
                                                      proj_Country == "Japan" ~ "Development Bank of Japan Inc\n(Japan)",
                                                      proj_Country == "Korea (Republic)" ~ "Korea Development Bank\n(Korea)")),
                # set x-axis to closing year, y-axis to SIB/home market, and the exact value of the vertical line to the moment of reaching maturity
                aes(x = year_of_close, y = main_institution, xmin = year_of_close - 0.5, xmax = year_of_close - 0.5,
                    linetype = "Reaching solar PV market maturity (based on IRENA, 2023)"),
                colour = "darkred") +
  # include a horizontal line segment from the 1st to the last closing year with PV lending activity for each SIB
  geom_errorbar(data = df_figure2_2 %>%
                  # for each SIB & deal type (domestic/foreign), determine the 1st & last year with PV lending activity
                  # NOTE: df_figure2_2 is already subset to PV deals, so no filter() required here
                  group_by(main_institution, is_domestic) %>% summarise(start_year = min(year_of_close), end_year = max(year_of_close)),
                # set start/end of the line to start_year & end_year & colour by deal type
                aes(xmin = start_year, xmax = end_year, colour = is_domestic),
                # set transparency to 50%, omit the whiskers of the error bar and shift it by deal type via position_dodge()
                alpha = 0.5, width = 0, position = position_dodge(width = 0.75)) +
  # create a point for each closing year & deal type, with the size set by the # of PV deals for each SIB; shift position by deal type
  geom_point(aes(x = year_of_close, colour = is_domestic, size = n), position = position_dodge(width = 0.75), alpha = 0.5) +
  # label chart elements manually, omitting the legend title for linetype
  labs(x = "Closing year", y = "SIB & home market", colour = "Deal location", size = "# of solar PV transactions", linetype = NULL) +
  # set the linetype manually
  scale_linetype_manual(values = "dashed") +
  # set the legend breaks for the point size manually
  scale_size_continuous(breaks = c(1, 4, 7)) +
  # set the legend order manually
  guides(size = guide_legend(override.aes = list(colour = "lightgrey"), order = 2),
         colour = guide_legend(order = 1, reverse = T),
         linetype = guide_legend(order = 3)) +
  # set further legend aesthetics manually
  theme(legend.position = "bottom",
        legend.box = "vertical",
        legend.spacing.y = unit(0.01, "cm"),
        legend.margin = margin(0, -0.5, 0, 0, unit = "cm"),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 9)
        )

# combine the two charts into a 2x1 grid chart
ggarrange(figure2_1,
          figure2_2,
          # set relative heights manually & label automatically as 'a', 'b'
          nrow = 2, ncol = 1, heights = c(1.3, 1), labels = "auto")

# save out as PDF file with a date stamp
ggsave(file.path("figures", paste0(Sys.Date(), " Figure2.pdf")),
       width = 6, height = 7.25)

# clean up the environment
rm(figure2_1, figure2_2, df_figure2_2, top6_countries_pv, top6_countries_pv_sib_activity)


### Figure 3

# create a bar chart that shows lender shares of different groups separately for:
# i) RE technology (PV, onshore, other), and ii) first-3 deals/subsequent deals
df_lenders %>%
  # merge RE technology & the I(First-3) dummy into df_lenders
  left_join(df %>% select(Asset_Finance_ID, Sector_final, countrytech_withdebt_first3), by = "Asset_Finance_ID") %>%
  # collapse all technologies other than solar PV and onshore to "other technologies" by creating a Sector_coarse column
  mutate(Sector_coarse = case_when(Sector_final %in% c("PV", "Onshore") ~ as.character(Sector_final),
                                   TRUE ~ "Other technologies")) %>%
  # merge in sample sizes calculated based on the df object
  left_join(df %>% mutate(Sector_coarse = case_when(Sector_final %in% c("PV", "Onshore") ~ as.character(Sector_final),
                                                    TRUE ~ "Other technologies")) %>%
              count(Sector_coarse), by = "Sector_coarse") %>%
  # create a RE technology label column that features the technology name & the # of deals in parentheses
  mutate(Sector_label = paste0(replace_tech_labels(Sector_coarse), " (N=", n, ")"),
         # collapse the categories 'Private banks' & 'Other financial companies' in df_lenders into one category
         lender_classification_coarse = case_when(lender_classification %in% c("Private banks", "Other financial companies") ~ "Banks & other financial companies",
                                                  TRUE ~ lender_classification)) %>%
  # count the # of lender appearances by category, RE technology & I(First-3)
  count(lender_classification_coarse, Sector_label, countrytech_withdebt_first3) %>%
  # calculate shares based on the counts
  group_by(Sector_label, countrytech_withdebt_first3) %>% mutate(share = n/sum(n)) %>% ungroup() %>%
  # convert categorical variables to factor to determine level order in the chart
  mutate(lender_classification_coarse = factor(lender_classification_coarse,
                                        levels = c("SIBs", "Other public sector", "Banks & other financial companies", "Other private sector", 
                                                   "Unknown")),
         Sector_label = factor(Sector_label, levels = c("Solar PV (N=3010)", "Onshore (N=1531)", "Other technologies (N=458)")),
         countrytech_withdebt_first3 = factor(if_else(countrytech_withdebt_first3, "First-3 deals", "Subsequent deals"),
                                              levels = c("Subsequent deals", "First-3 deals"))) %>%
  # create a bar chart with I(First-3) on the x-axis and the shares on the y-axis
  ggplot(aes(countrytech_withdebt_first3, share)) +
  # colour the bars by lender category & flip the order of the categories for visual purposes
  geom_col(aes(fill = lender_classification_coarse), position = position_fill(reverse = T)) +
  # label chart elements manually & omit legend titles
  labs(x = NULL, y = "Share among lenders", fill = NULL) +
  # set the bar colours manually
  scale_fill_manual(values = c(sib_colour, "#b0c9e3",  "#78a352", "#AECA95", "grey")) +
  # convert y-axis value labels to %
  scale_y_continuous(labels = scales::percent) +
  # create separate facets for each RE technology in a 3x1 format
  facet_wrap(~ Sector_label, nrow = 3, ncol = 1) +
  # flip x-axis & y-axis
  coord_flip() +
  # use 2 rows for the elements of the bar colour legend
  guides(fill = guide_legend(nrow = 2, byrow = T)) +
  # set further legend aesthetics
  theme(legend.position = "bottom",
        strip.background = element_blank())

# save out as PDF file with a date stamp
ggsave(file.path("figures", paste0(Sys.Date(), " Figure3.pdf")), width = 6.5, height = 4.5)


### Figure 4

# estimate the main specification model with year dummy variables instead of absorbed fixed effects
figure4_models <- fixest::feglm(fml = as.formula(table4_formulas[["(3)"]] %>%
                                                   str_replace("\\| year_of_close_chr \\+ ", "+ i(year_of_close_chr, ref = '2008') | ")),
                                data = df,
                                family = "binomial",
                                vcov = ~ year_of_close_chr)

# plot the dummy coefficients & the 95% CIs, save out as PDF file with a date stamp
pdf(file.path("figures", paste0(Sys.Date(), " Figure4.pdf")),
    width = 6.5, height = 3.5)
fixest::iplot(figure4_models, i.select = 2,
      main = "Effect on __depvar__ log-odds")
dev.off()

# clean up the environment
rm(figure4_models)


### Figure B1

# write a function to plot average partial effects for a given fixest model
plot_APEs <- function(model = NULL,
                      # set the regressors for which there will be a zoom-in version due to small absolute effects
                      regressors_for_zoomin = c("Capacity (MW)",
                                                "Real GDP PPP growth (%)",
                                                "# of non-SIB lenders")) {
  
  # calculate APEs for the 95% CI on the outcome scale (= probs) using dY/dX for numerical regressors
  partialeffects <- marginaleffects::avg_slopes(model, conf_level = 0.95, type = "response", slope = "dydx") %>%
    broom::tidy()
  
  # clean up the names in the partialeffects object using the my_fixest_dict dictionary
  for(jj in 1:nrow(partialeffects)) {
    if(partialeffects$term[jj] %in% names(my_fixest_dict)) partialeffects$term[jj] <- my_fixest_dict[names(my_fixest_dict) == partialeffects$term[jj]]
    if(partialeffects$term[jj] == "Tech") partialeffects$term[jj] <- paste0("Tech == ", partialeffects$contrast[jj] %>% str_remove(" \\- Onshore") %>% str_trim())
  } 
  
  # clean up variable names, define 5% significance in a column & order regressors manually
  partialeffects <- partialeffects %>%
    mutate(term = term %>% str_replace("Thermal", "CSP") %>% str_replace("PV", "Solar PV"),
           significance = case_when(p.value < 0.05 & estimate > 0 ~ "positive",
                                    p.value < 0.05 & estimate < 0 ~ "negative",
                                    TRUE ~ "not significant"),
           # order is hard-coded based on i) order of hypotheses and ii) tech risk (high to low)
           order = case_when(str_detect(term, "Offshore") ~ 1,
                             str_detect(term, "CSP") ~ 2,
                             str_detect(term, "Geothermal") ~ 3,
                             str_detect(term, "Biomass") ~ 4,
                             str_detect(term, "Hydro") ~ 5,
                             str_detect(term, "Tech == Solar PV") ~ 6,
                             str_detect(term, "Tech matured") ~ 7,
                             str_detect(term, "First-3") ~ 8,
                             str_detect(term, "Cap\\. in") ~ 9,
                             str_detect(term, "Capacity") ~ 10,
                             str_detect(term, "non-SIB lenders") ~ 11,
                             str_detect(term, "GDP") ~ 12,
                             str_detect(term, "Term loan") ~ 13,
                             str_detect(term, "public sponsor") ~ 14,
                             str_detect(term, "Feed-in") ~ 15))
  
    # plot APEs and 95% CIs for all regressors
    p1 <- partialeffects %>%
      # display the regressor on the x-axis, ordered by the manually set 'order' variable
      ggplot(aes(reorder(term, -order))) +
      # include a horizontal dashed line at 0
      geom_hline(yintercept = 0, linetype = "dashed", colour = "black") +
      # display point estimates as points & colour by their 5% significance
      geom_point(aes(y = estimate, colour = significance)) +
      # display 95% CIs as errorbars, again coloured by 5% significance. Omit whiskers by setting width to zero
      geom_errorbar(aes(ymin = conf.low, ymax = conf.high, colour = significance, width = 0)) +
      # label chart elements manually
      labs(y = "Average partial effect (in pp) - 95% CI", colour = "Significance (5% level)") +
      # convert y-axis value labels to %
      scale_y_continuous(labels = scales::percent) +
      # set the colour scale manually
      scale_colour_manual(values = c("grey", "#F8766D", "#00BA38"),
                          breaks = c("not significant", "negative", "positive")) +
      # flip x-axis & y-axis
      coord_flip() +
      # omit y-axis titles & set the legend position
      theme(axis.title.y = element_blank(), legend.position = c(0.75, 0.25))
    
    # plot a zoom-in version for all regressors set in regressors_for_zoomin
    p2 <- partialeffects %>% filter(term %in% regressors_for_zoomin) %>%
      ggplot(aes(reorder(term, -order))) +
      geom_hline(yintercept = 0, linetype = "dashed", colour = "black") +
      geom_point(aes(y = estimate, colour = significance)) +
      geom_errorbar(aes(ymin = conf.low, ymax = conf.high, colour = significance, width = 0)) +
      labs(y = "Average partial effect (in pp) - 95% CI", colour = "Significance (5% level)") +
      scale_y_continuous(labels = scales::percent) +
      scale_colour_manual(values = c("grey", "#F8766D", "#00BA38"),
                          breaks = c("not significant", "negative", "positive")) +
      coord_flip() +
      # omit the legend for 5% significance, which is already featuring in p1
      theme(axis.title.y = element_blank(), legend.position = "none")
    
    # combine the two charts into a 2x1 grid chart, with additional chart titles
    ggarrange(p1 + labs(subtitle = "All regressors"),
              p2 + labs(subtitle = "Zoom-in for small absolute effects"),
              nrow = 2, ncol = 1, align = "hv", heights = c(3, 1))
    
    print(partialeffects)
}

# apply plot_APEs() to the model in Table 4, column 3
plot_APEs(table4_models[["(3)"]])

# save out as PDF file with a date stamp
ggsave(file.path("figures", paste0(Sys.Date(), " FigureB1.pdf")), width = 6, height = 5.5)


### Figure B2

# NOTE: avg_slopes() by default collapses interaction terms, so we define them manually here and re-estimate the model
figureB2_models <- fixest::feglm(fml = as.formula(table4_formulas[["(4)"]] %>% str_replace("i\\(Sector_final, is_mature, keep = c\\('Onshore', 'PV'\\)\\)",
                                                                                           "is_mature_onshore + is_mature_pv")),
                                data = df %>% mutate(is_mature_onshore = is_mature & Sector_final == "Onshore",
                                                     is_mature_pv = is_mature & Sector_final == "PV"),
                                family = "binomial",
                                vcov = ~ year_of_close_chr)

# ensure that estimates are indeed identical to Table 4, column 4
etable(figureB2_models, table4_models[["(4)"]])

# apply plot_APEs() to the model in Table 4, column 4
plot_APEs(figureB2_models)

# save out as PDF file with a date stamp
ggsave(file.path("figures", paste0(Sys.Date(), " FigureB2.pdf")), width = 6, height = 5.5)

# clean up the environment
rm(figureB2_models)


### Figure B3

# apply plot_APEs() to the model in Table 6, column 2
plot_APEs(table6_models[["(2)"]])

# save out as PDF file with a date stamp
ggsave(file.path("figures", paste0(Sys.Date(), " FigureB3.pdf")), width = 6, height = 5.5)


### Figure B4

# estimate model with manually defined interaction terms
figureB4_models <- fixest::feglm(fml = as.formula(table6_formulas[["(3)"]] %>% str_replace("i\\(Sector_final, is_mature, keep = c\\('Onshore', 'PV'\\)\\)",
                                                                                           "is_mature_onshore + is_mature_pv")),
                                 data = df %>% mutate(is_mature_onshore = is_mature & Sector_final == "Onshore",
                                                      is_mature_pv = is_mature & Sector_final == "PV") %>%
                                               filter(!(is_sib_len & no_lenders_wo_sibs == 0)),
                                 family = "binomial",
                                 vcov = ~ year_of_close_chr)

# ensure that the two models are identical
etable(figureB4_models, table6_models[["(3)"]])

# apply plot_APEs()
plot_APEs(figureB4_models)

# save out as PDF file with a date stamp
ggsave(file.path("figures", paste0(Sys.Date(), " FigureB4.pdf")), width = 6, height = 5.5)

# clean up the environment
rm(figureB4_models)


### Figure B5

# identify all countries for which the technology maturity dummy does not lead to perfect separation
df %>% filter(!is.na(fin_Capacity_total_MW) & !is.na(oecd_feedintariff) & !is.na(has_public_sponsor),
              Sector_final %in% c("PV", "Onshore")) %>%
  count(proj_Country, is_mature, is_sib_len) %>% count(proj_Country) %>%
  filter(n == 4) %>% pull(proj_Country)

# throw a warning to indicate that countries are hard-pasted in the code line below
warning("Countries to keep in interacting the maturity dummy with the country FE are hard-coded. Please revise the country selection if required")

# estimate the main specification (Table 4, column 3) for interacting is_mature with the project country for the countries identified above 
figureB5_models <- fixest::feglm(fml = table4_formulas[["(3)"]] %>%
                                                    str_replace("is_mature", "i(proj_Country, is_mature, keep = c('Australia', 'Austria',  'Chile',  'Finland',  'France', 'Germany', 'Italy',  'Japan', 'Korea (Republic)', 'Lithuania', 'Netherlands', 'Poland', 'Portugal', 'Spain', 'Sweden', 'United Kingdom'))") %>%
                                   as.formula(),
                                data = df,
                                family = binomial(link = "logit"), 
                                vcov = ~ year_of_close_chr)
etable(figureB5_models)

# plot the dummy coefficients & the 95% CIs, save out as PDF file with a date stamp
pdf(file.path("figures", paste0(Sys.Date(), " FigureB5.pdf")),
    width = 5, height = 5)
fixest::coefplot(figureB5_models, keep = "is_mature",
                 zero.par = list(lwd = 2, col = "grey"),
                 main = "Effect on __depvar__ log-odds", horiz = T)
dev.off()

# clean up the environment
rm(figureB5_models)


### Figure B6

# estimate the main specification (Table 4, column 3) for interacting the FiT with the RE technology dummy
figureB6_models <- fixest::feglm(fml = table4_formulas[["(3)"]] %>%
                                   str_replace("oecd_feedintariff", "i(Sector_final, oecd_feedintariff)") %>% as.formula(),
                                 data = df %>% mutate(Sector_final = replace_tech_labels(Sector_final)),
                                 family = binomial(link = "logit"), 
                                 vcov = ~ year_of_close_chr)
etable(figureB6_models)

# plot the dummy coefficients & the 95% CIs, save out as PDF file with a date stamp
pdf(file.path("figures", paste0(Sys.Date(), " FigureB6.pdf")),
    width = 5, height = 5)
fixest::coefplot(figureB6_models, keep = "oecd_feedintariff",
                 zero.par = list(lwd = 2, col = "grey"),
                 main = "Effect on __depvar__ log-odds", horiz = T)
dev.off()

# clean up the environment
rm(figureB6_models)

  
### Figure B7

# estimate the main specification (Table 4, column 3) for interacting the first-kind dummy with the RE technology dummy
figureB7_models <- fixest::feglm(fml = table4_formulas[["(3)"]] %>%
                                   str_replace("countrytech_withdebt_first3", "i(Sector_final, countrytech_withdebt_first3)") %>% as.formula(),
                                 data = df %>% mutate(Sector_final = replace_tech_labels(Sector_final)),
                                 family = binomial(link = "logit"), 
                                 vcov = ~ year_of_close_chr)
etable(figureB7_models)

# plot the dummy coefficients & the 95% CIs, save out as PDF file with a date stamp
pdf(file.path("figures", paste0(Sys.Date(), " FigureB7.pdf")),
    width = 5, height = 5)
fixest::coefplot(figureB7_models, keep = "countrytech_withdebt_first3",
                 zero.par = list(lwd = 2, col = "grey"),
                 main = "Effect on __depvar__ log-odds", horiz = T)
dev.off()

# clean up the environment
rm(figureB7_models)


### Figure B8

# identify all countries for which the first-kind dummy does not lead to perfect separation
df %>% filter(!is.na(fin_Capacity_total_MW) & !is.na(oecd_feedintariff) & !is.na(has_public_sponsor)) %>%
  count(proj_Country, countrytech_withdebt_first3, is_sib_len) %>% count(proj_Country) %>%
  filter(n == 4) %>% pull(proj_Country)

# throw a warning to indicate that countries are hard-pasted in the code line below
warning("Countries to keep in interacting the first-kind dummy with the country FE are hard-coded. Please revise the country selection if required")

# estimate the main specification (Table 4, column 3) for interacting I(First-3) with the project country for the countries identified above 
figureB8_models <- fixest::feglm(fml = table4_formulas[["(3)"]] %>%
                                   str_replace("countrytech_withdebt_first3", "i(proj_Country, countrytech_withdebt_first3, keep = c('Australia', 'Austria', 'Belgium', 'Canada',  'Chile', 'Denmark', 'Estonia',  'Finland',  'France', 'Germany', 'Greece', 'Hungary', 'Italy', 'Japan', 'Lithuania', 'Mexico', 'Netherlands', 'Norway', 'Poland', 'Portugal', 'Spain', 'Sweden', 'Turkey', 'United Kingdom', 'United States'))") %>%
                                   as.formula(),
                                 data = df,
                                 family = binomial(link = "logit"), 
                                 vcov = ~ year_of_close_chr)
etable(figureB8_models)

# plot the dummy coefficients & the 95% CIs, save out as PDF file with a date stamp
pdf(file.path("figures", paste0(Sys.Date(), " FigureB8.pdf")),
    width = 5, height = 8)
fixest::coefplot(figureB8_models, keep = "countrytech_withdebt_first3",
                 zero.par = list(lwd = 2, col = "grey"),
                 main = "Effect on __depvar__ log-odds", horiz = T)
dev.off()

# clean up the environment
rm(figureB8_models)


### Figure B9

# estimate the main specification (Table 4, column 3) for interacting the # of non-SIB lenders with the RE technology dummy
figureB9_models <- fixest::feglm(fml = table4_formulas[["(3)"]] %>%
                                   str_replace("no_lenders_wo_sibs", "i(Sector_final, no_lenders_wo_sibs)") %>% as.formula(),
                                 data = df %>% mutate(Sector_final = replace_tech_labels(Sector_final)) %>%
                                   filter(!(is_sib_len & no_lenders_wo_sibs == 0)),
                                 family = binomial(link = "logit"), 
                                 vcov = ~ year_of_close_chr)
etable(figureB9_models)

# plot the dummy coefficients & the 95% CIs, save out as PDF file with a date stamp
pdf(file.path("figures", paste0(Sys.Date(), " FigureB9.pdf")),
    width = 5, height = 5)
fixest::coefplot(figureB9_models, keep = "no_lenders_wo_sibs",
                 zero.par = list(lwd = 2, col = "grey"),
                 main = "Effect on __depvar__ log-odds", horiz = T)
dev.off()

# clean up the environment
rm(figureB9_models)


### Figure C1

# create a bar chart of in-sample lending activity by lender category
df_lenders %>% count(lender_classification) %>%
  # create a more coarse category for public sector/private sector/Unknown
  mutate(category = case_when(lender_classification == "Unknown" ~ NA_character_,
                              lender_classification %in% c("SIBs", "Other public sector") ~ "Public sector",
                              TRUE ~ "Private sector")) %>%
  # lender categories, ordered by lending activity, on the x-axis, # of deals on the x-axis
  ggplot(aes(reorder(lender_classification, n), n)) +
  # bar colours based on lender category
  geom_col(aes(fill = category)) +
  # # of deals displayed as value labels above the bars
  geom_text(aes(label = n), nudge_y = 500) +
  # label chart elements manually and omit x-axis title & fill title in the legend
  labs(fill = NULL, y = "# of in-sample lending activity", x = NULL) +
  # position the legend manually
  theme(legend.position = c(0.6, 0.3)) +
  # flip x-axis & y-axis
  coord_flip()

# save out as PDF file with a date stamp
ggsave(file.path("figures", paste0(Sys.Date(), " FigureC1.pdf")), width = 5.2, height = 3)


### Figure C2

# plot shares over time for each country, with a horizontal line at the 10% threshold
df_capshare_irena %>%
  # subset to solar PV & onshore deals
  filter(Sector_final %in% c("PV", "Onshore")) %>%
  # clean up the 'Sector_final' variable using replace_tech_labels(), which is defined above
  mutate(Sector_final = replace_tech_labels(Sector_final)) %>%
  # chart the year on the x-axis, the share in national installed capacity on the y-axis
  ggplot(aes(Year, capshare_irena)) +
  # include a horizontal line at 10%
  geom_hline(yintercept = 0.1, linetype = "dashed", colour = "black") +
  # add an invisible line (via alpha = 0) that will prompt a legend item for the maturity line
  geom_hline(aes(yintercept = 0, linetype = "Maturity threshold (IRENA, 2023)"), colour = "black", alpha = 0) +
  # add a line for actual capacity shares, coloured by technology
  geom_line(aes(colour = Sector_final)) +
  # create a facet for each country, in a Nx4 format
  facet_wrap(~ Country, ncol = 4) +
  # set linetype values manually
  scale_linetype_manual(values = "dashed",
                        guide = guide_legend(override.aes = list(alpha = 1))) +
  # define x-axis breaks manually
  scale_x_continuous(breaks = c(2000, 2010, 2020)) +
  # set y-axis labels to % & the number of axis breaks
  scale_y_continuous(labels = scales::percent, n.breaks = 3) + 
  # label chart elements manually & omit legend titles 
  labs(y = "Share in national installed capacity", colour = NULL, linetype = NULL) + 
  # omit the legend for the transparency aesthetic
  guides(alpha = NULL) +
  # set further legend aesthetics, drop x-axis titles & rotate x-axis labels for readability
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 0.5),
        legend.position = "bottom",
        strip.background = element_blank()) +
  # ensure that x-axis & y-axis are displayed for all facets by including manual segments
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf) +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf)

# save out as PDF file with a date stamp
ggsave(file.path("figures", paste0(Sys.Date(), " FigureC2.pdf")), width = 5, height = 8)

# clean up the environment
rm(df_capshare_irena)


### Figure C3

# plot the FiT data by the OECD for the countries in our sample
df_fit_oecd %>%
  # drop FiTs that are not used in our analysis (NOTE: we use the Biomass FiT for 'Biomass & Waste')
  filter(!Sector_final %in% c("Waste", "Marine")) %>%
  # plot the year on the x-axis, the FiT on the y-axis, and a line coloured by technology
  ggplot(aes(Year, `Mean feed-in tariff`)) + geom_line(aes(colour = Sector_final)) +
  # ensure that x-axis & y-axis are displayed for all facets by including manual segments
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf) +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf) +
  # create a facet for each country, using the names used by BNEF, in an Nx4 format
  facet_wrap(~ bnef_name, ncol = 4) +
  # set the # of rows in the colour legend to two
  guides(colour = guide_legend(nrow = 2)) +
  # label chart elements manually & omit the legend title for colour
  labs(x = "Year", y = "Mean FiT in 2010 USD/kWh", colour = NULL) +
  # set further legend aesthetics & rotate x-axis labels for readability
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        strip.background = element_blank())

# save out as PDF file with a date stamp
ggsave(file.path("figures", paste0(Sys.Date(), " FigureC3.pdf")), width = 5, height = 8)

# clean up the environment
rm(df_fit, df_fit_oecd, df_us_inflation)


### Figure D1

# create a bar chart for the # of deals based on whether SIB volumes are known (column 'SIB_lending_certainty')
figureD1_1 <- df %>%
  # subset to deals with SIB involvement
  filter(is_sib_len) %>%
  # collapse the one deal where SIB financing in total is known but contributions of individual SIBs are not into the 'Known' category
  mutate(SIB_lending_certainty = case_when(SIB_lending_certainty == "Unclear allocation across SIBs, known total" ~ "Known",
                                           TRUE ~ SIB_lending_certainty)) %>%
  # count # of deals by each certainty category
  count(SIB_lending_certainty) %>%
  # create the bar chart
  ggplot(aes(SIB_lending_certainty, n)) + geom_col(aes(fill = SIB_lending_certainty))  +
  labs(x = "Exact SIB financing volume is...", y = "# of in-sample deals") +
  theme(legend.position = "none")
figureD1_1                                

# for each SIB with >20 deals, plot the share of certainty on SIB financing volumes
figureD1_2 <- sib_table %>%
  # for each SIB, create a data frame with 1 row per deal by unnesting the 'transaction_ids_in_df_reg' column that stores deal IDs
  # NOTE: we use distinct() to avoid double counts if both the main SIB & one of its subsidiaries are involved in the same deal
  select(main_institution, transaction_ids_in_df_reg) %>% unnest(transaction_ids_in_df_reg) %>% distinct() %>%
  # merge in the SIB financing volume certainty from df
  left_join(df %>% select(Asset_Finance_ID, SIB_lending_certainty), by = c("transaction_ids_in_df_reg" = "Asset_Finance_ID")) %>%
  # collapse the one deal where SIB financing in total is known but contributions of individual SIBs are not into the 'Known' category
  mutate(SIB_lending_certainty = case_when(SIB_lending_certainty == "Unclear allocation across SIBs, known total" ~ "Known",
                                           TRUE ~ SIB_lending_certainty)) %>%
  # count the # of deals by SIB & lending certainty
  count(main_institution, SIB_lending_certainty) %>%
  # calculate shares for each SIB
  group_by(main_institution) %>% mutate(n_total = sum(n),
                                        share = n/n_total,
                                        # create a clean label for each SIB with the total # of deals in parentheses
                                        sib_label = paste0(main_institution, "\n(N=", n_total, ")")) %>% ungroup() %>%
  # subset to SIBs that have more than 20 deals in our sample
  filter(n_total > 20) %>%
  # plot the SIB on the x-axis, ordered by sample size, shares on the y-axis, and add bars coloured by certainty category
  ggplot(aes(reorder(sib_label, n_total), share)) + geom_col(aes(fill = SIB_lending_certainty)) +
  # label chart elements manually & omit x-axis titles
  labs(y = "% of deals by availability \nof SIB financing volume", x = NULL) +
  # flip x-axis & y-axis
  coord_flip() +
  # convert y-axis value labels to %
  scale_y_continuous(labels = scales::percent) +
  # omit the legend
  theme(legend.position = "none")

# combine the two charts into a 1x2 grid chart
ggarrange(figureD1_1, figureD1_2,
          nrow = 1, ncol = 2, widths = c(0.4, 0.6), align = "h", labels = "auto"
)

# save out as PDF file with a date stamp
ggsave(file.path("figures", paste0(Sys.Date(), " FigureD1.pdf")),
       width = 6.5, height = 4)

# clean up the environment
rm(figureD1_1, figureD1_2)


### Figure D2

# identify all deal IDs that feature the EIB as a lender
deal_ids_with_eib <- sib_table %>% filter(main_institution == "European Investment Bank") %>% unnest(transaction_ids_in_df_reg) %>%
  pull(transaction_ids_in_df_reg)

# create a scatter plot with actual share of SIB lending in total debt vs. imputed share (= 1/# of lenders)
df %>%
  # subset to deals with multiple lenders where the (1/# of lenders) imputation makes sense
  # NOTE: in rare cases, the SIB volume is known but the share of SIB in total lending is NA because total debt is missing. Omit these deals
  filter(no_lenders > 1, SIB_lending_certainty %in% c("Known", "Unclear allocation across SIBs, known total") & !is.na(sib_lending_debtshare)) %>%
  # create columns with the imputed share & a character variable indicating EIB involvement
  mutate(hypothetical_share = 1/no_lenders,
         is_eib_involved = if_else(Asset_Finance_ID %in% deal_ids_with_eib, "with EIB involvement", "no EIB involvement")) %>%
  # plot imputed share & actual share on x-axis & y-axis respectively
  ggplot(aes(hypothetical_share, sib_lending_debtshare)) +
  # include a dashed 45deg line
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  # colour scatter points by EIB involvement
  geom_point(aes(colour = is_eib_involved)) +
  # convert axes value labels to %
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  # flip the order of the colour legend items for visual purposes
  guides(colour = guide_legend(reverse = T)) +
  # label chart elements manually
  labs(x = "Hypothetical share under equal allocation\n(= 1/# of lenders)",
       y = "Actual share of SIB lending",
       subtitle = "Black dashed line = 45deg line") +
  # set further legend aesthetics
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.background = element_blank())

# save out as PDF file with a date stamp
ggsave(file.path("figures", paste0(Sys.Date(), " FigureD2.pdf")),
       width = 5.75, height = 5.25)

# clean up the environment
rm(deal_ids_with_eib)


################################################################################
#################### STATISTICS IN THE MAIN MANUSCRIPT's TEXT ##################
################################################################################

# NOTE: this section only features calculations for statistics that cannot be
# inferred from one of the tables or figures produced above


### Section 5.1

# share of deals located in G7, Spain, South Korea, and the Netherlands
mean(df$proj_Country %in% c(# G7 members
                            "Canada", "United States", "Japan", "Germany", "France", "United Kingdom", "Italy",
                            # other countries
                            "Spain", "Korea (Republic)", "Netherlands"))

# technology shares in the overall sample
df %>% count(Sector_final) %>% mutate(share = n/sum(n)) %>% arrange(desc(share))


### Section 5.2

# compare mean financed capacity & # of non-SIB lenders for offshore & non-offshore
df %>% mutate(is_offshore = Sector_final == "Offshore") %>%
  group_by(is_offshore) %>% summarise_at(vars(no_lenders_wo_sibs, fin_Capacity_total_MW), ~ mean(.x, na.rm = T))

# explore the in-sample SIB activity in the Japanese PV sector after 2015
df %>% filter(Sector_final == "PV", proj_Country == "Japan", is_sib_len, year_of_close > 2015) %>%
  # merge in the SIB on the respective transaction
  left_join(sib_table %>% select(main_institution, transaction_ids_in_df_reg) %>% unnest(transaction_ids_in_df_reg) %>% distinct(),
            by = c("Asset_Finance_ID" = "transaction_ids_in_df_reg")) %>%
  select(main_institution, everything())

# no. of first-3 deals for CSP in our sample
df %>% filter(countrytech_withdebt_first3, Sector_final == "Thermal") %>% nrow()

# no. of deals with SIB as the sole lender
df %>% filter(is_sib_len & no_lenders_wo_sibs == 0) %>% nrow()

# share of SIB solo-lending in deals involving SIB lending by RE technology
df %>% filter(is_sib_len, Sector_final %in% c("Offshore", "CSP", "Small Hydro")) %>%
  # create a dummy indicating solo-lending
  mutate(is_sib_solo_len = is_sib_len & no_lenders_wo_sibs == 0) %>%
  # subset to deals with SIB involvement
  filter(is_sib_len) %>%
  # count no. of deals & shares by technology
  count(is_sib_solo_len) %>% mutate(share = n/sum(n))
  
# total number of deals for Offshore, CSP & Small Hydro in the sample
sum(df$Sector_final %in% c("Offshore", "CSP", "Small Hydro"))

# share of deals with public sponsor among deals with SIB as the sole lender
mean(df$has_public_sponsor[df$is_sib_len & df$no_lenders_wo_sibs == 0], na.rm = T)

# share of deals with public sponsor among deals with SIB being a co-lender
mean(df$has_public_sponsor[df$is_sib_len & df$no_lenders_wo_sibs > 0], na.rm = T)


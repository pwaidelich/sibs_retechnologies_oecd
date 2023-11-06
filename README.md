# The role of state investment banks for renewable energy technologies in OECD countries
Replication materials for Waidelich and Steffen (2023). If you have questions about the code or find any errors/bugs, please contact Paul Waidelich at paul.waidelich [at] gess.ethz.ch (corresponding author).

## Organization of the overall project repository
The repository features the following elements:
1. The `input` folder, which hosts all data objects (in RDS format) required to replicate all figures, tables, and analyses in the manuscript. 
2. The `src` folder for scripts, which hosts the script replicating all figures, tables and analyses in the manuscript (`src/replication_main.R`). Note that for some tables, our manuscript applies some additional formatting in LaTeX to the raw output of the script. Before running `src/replication_main.R`, please set the working directory to the highest folder level of this repository, which can be easily done by opening the `sibs_retechnologies_oecd.Rproj` via RStudio on your local machine.
3. The `figures` folder, where all figures produced by `src/replication_main.R` are stored as PDF files with a date stamp in the file name. In its original state, the repository features all figures as returned by the script at the time of submission. By executing `src/replication_main.R`, new versions with the current date stamp will be added to the folder.

## Data objects in the `input` folder
The  `input` features the following RDS files, which are loaded and processed by the `src/replication_main.R` script:
1. `input/df.rds`: deal-level data frame/tibble object including the dependent variable indicating SIB lending (`is_sib_len`), all regressors used throughout the manuscript, the BNEF transaction identifier (`Asset_Finance_ID`), as well as some additional variables required for some of the figures. The latter includes a dummy indicating if the lender information in the BNEF Asset Finance database is listed as "Not Reported" (`is_lender_notreported`), the overall number of lenders including SIBs (`no_lenders`), the share of SIB lending volumes in the deal's total debt as per BNEF information (`sib_lending_debtshare`) and the certainty regarding the SIB lending volume which can be Known, Unknown or Imputable using the no. of lenders (`SIB_lending_certainty`). All other variables are described in more detail in Appendix A of the manuscript.
2. `input/df_lenders.rds`: data frame/tibble object with one row per deal-lender combination that features each lender's name and organization ID in the BNEF Organizations database, as well as the role on the respective deal. In addition, it features our lender classification using the BICS framework and, in case of missing Bloomberg Tickers, additional desk research into "Private banks", "Other financial companies", "Other private sector", "SIBs", "Other public sector" and "Unknown".
3. `input/sib_table.rds`: an organization-level data frame/tibble object with one row per SIB institution/subsidiary (column `Organization_Name`). Subsidiaries are connected to the parent (`main_institution`). Aside from additional organization-level information from the BNEF Organizations database, the object features all deal IDs on which the respective SIB institution/subsidiary is listed as a lender in the BNEF Asset Finance database (`transaction_ids_in_df_reg`) as well as the total number of such transactions (`No_transactions_in_df_reg`)
4. `input/df_capshare_irena.rds`: data frame/tibble object with one row per country-year-RE technology combination that features the respective technology's share in the country's installed capacity as per IRENA data. Details on how the data is obtained can be found in Appendix A of the manuscript.
5. `input/df_fit_oecd.rds`: data frame/tibble object with one row per country-year-RE technology combination that features the inflation-adjusted feed-in tariff. Details on how the data is obtained can be found in Appendix A of the manuscript.

## Censored commercial data from Bloomberg New Energy Finance (BNEF)
Please note that `input/df.rds`, `input/sib_table.rds`, and `input/df_lenders.rds` feature commercial and proprietary data from BNEF that can only be shared with license holders. Therefore, this repository features only censored versions of data files that do not contain proprietary information from the underlying BNEF databases and instead feature "-999" placeholder values (and FALSE for logical variables). However, these files have the same structure and feature original identifier variables, enabling BNEF license holders to merge the actual information. For access to the uncensored data, please contact the corresponding author with proof that you are a current BNEF license holder.
Unless uncensored data has been made available and placed in the `input` directory, the lines in `src/replication_main.R` that load the (uncensored) data objects will result in errors. 

## Variable names in the data frames
Please note that extensive variable descriptions and information on underlying data sources are listed in Appendix A of the Online Appendix. Therefore, the following list primarily serves to match the columns in the data objects with the variables described in Appendix A and provides more information only for identifier or helper columns not listed in Appendix A.

### Variables in `input/df.rds`
1. `Asset_Finance_ID`: Financial transaction ID from the BNEF Asset Finance database
2. `is_sib_len`: I(SIB lending) - see Appendix A
3. `year_of_close`: Closing year - see Appendix A
4. `year_of_close_chr`: Closing year, coded as a character instead of an integer
5. `proj_Country`: Project country - see Appendix A
6. `Sector_final`: Technology - see Appendix A
7. `fin_Capacity_total_MW`: Capacity (MW) - see Appendix A
8. `fin_capacity_techyear_decile1`: I(Cap. in 1st decile) - see Appendix A
9. `no_lenders_wo_sibs`: # of non-SIB lenders - see Appendix A
10. `countrytech_withdebt_first...`: I(First-k deal) - see Appendix A 
11. `is_fin_termloan`: I(Term loan) - see Appendix A
12. `has_public_sponsor`: I(Any public sponsor) - see Appendix A
13. `fin_capacity_techyear_decile1`: I(Cap. in 1st decile) - see Appendix A
14. `fin_capacity_techyear_decilebin`: The decile in which the transaction falls based on the methodology underlying I(Cap. in 1st decile) in Appendix A
15. `is_lender_notreported`: A dummy variable indicating whether lenders for the transaction in the BNEF Asset Finance database are listed as "Not Reported". This is used for robustness checks excluding these deals.
16. `no_lenders`: # of lenders on the transaction, incl. both SIB and non-SIB lenders
17. `is_mature`: I(Tech matured) - see Appendix A
18. `oecd_feedintariff`: Feed-in tariff (2010 USD/kWh) - see Appendix A
19. `capshare_irena`: Share of the transaction's technology in national installed capacity used to calculate I(Tech matured) as outlined in Appendix A
20. `gdp_growth`: Real GDP PPP growth (in %) - see Appendix A
21. `ccpi_overall_score`: CCPI Overall Score (0-100) - see Appendix A
22. `oecd_ltinterest_perc`: Long-term interest rate (%) - see Appendix A
23. `gfd_bankzscore`: Country Bank Z-score - see Appendix A
24. `wdi_govexpend_percgdp`: Gov. expenditures (% of GDP) - see Appendix A
25. `wdi_primarybal_percgdp`: Primary balance (% of GDP) - see Appendix A
26. `sib_lending_debtshare`: The share of total debt of the transaction that is (jointly) provided by SIBs listed in `input/sib_table.rds`, ranging from zero (= no SIB debt financing) to one (= only SIB debt financing). This involves both known and imputed values following the methodology outlined in Appendix D of the Online Appendix. 
27. `SIB_lending_certainty`: Indicates if the value in `sib_lending_debtshare` is `Known`, `Imputable` (= imputed), or `Unknown` (in which case `sib_lending_debtshare` is `NA`). The variable is `NA` for all deals w/o SIB lenders. Note that for one transaction, BNEF information does not inform about the exact allocation of debt across different SIBs, but their combined financing is known. This is marked by a value of `Unclear allocation across SIBs, known total`

### Variables in `input/df_lenders.rds`
1. `Asset_Finance_ID`: see above
2. `Organization_Name`: Organization name from the BNEF Organizations database
3. `Organization_ID`: Organization ID from the BNEF Organizations database
4. `Role`: The organization's role in the transaction, as per the BNEF Asset Finance database. Since we only consider lenders, this can be either `Lead arranger` or `Syndicated lender`
5. `lender_classification`: Classifies each lender into one of the following categories: `SIBs`, `Other public sector`, `Private banks`, `Other financial companies`, `Other private sector` or `Unknown`. Classification is obtained based on the BICS classification scheme or, if this information is not available, based on the Subactivity variable of the organization in the BNEF Organizations database. For SIBs included in `input/sib_table.rds`, the value is always `SIBs` regardless of their BICS or Subactivity information

### Variables in `input/sib_table.rds`
1. `main_institution`: The ultimate parent company of SIB subsidiaries, as per the BNEF Organizations database. For the parent SIB, this is equal to the `Organization_Name`
2. `Organization_Name`: See above
3. `No_transactions_in_df_reg`: The number of transactions in `input/df.rds`, in which the organization appears as a lender
4. `transaction_ids_in_df_reg`: The financial transaction IDs of all transactions, in which the organization appears as a lender. This is a listed column where each row stores as many IDs as the value of `No_transactions_in_df_reg` indicates
5. `Organization_ID`: See above
6. `Organization_ID`: The parent company as per the BNEF Organizations database
7. `Country`: The organization's country as per the BNEF Organizations database
8. `Subactivity`: The organization's subactivity as per the BNEF Organizations database
9. `Website`: The organization's website as per the BNEF Organizations database
10. `is_main`: A dummy variable indicating whether the organization is the ultimate parent SIB (`TRUE`) or a subsidiary (`FALSE`)

### Variables in `input/df_capshare_irena.rds`:
NOTE: For more detailed information on how IRENA data on national installed capacity is obtained, see Appendix A of the Online Appendix
1. `Year`: Year
2. `Country`: Country
3. `Sector_final`: Technology - see Appendix A
4. `capshare_irena`: See above

### Variable names in `input/df_fit_oecd.rds`:
NOTE: For more detailed information on how FiT data is obtained, see Appendix A of the Online Appendix
1. `COU`: Three-character country code (e.g., `AUS`)
2. `Year`: Year
3. `Sector_final`: See above
4. `Mean feed-in tariff`: Feed-in tariff (2010 USD/kWh) - see Appendix A
5. `Length of power purchase agreement`: An additional variable from the OECD data on feed-in tariffs that is not used in our paper
6. `bnef_name`: The country name as it appears in the BNEF database

## System requirements
The script can be executed on an ordinary computer and requires neither substantial computational resources nor parallel processing. Total runtime is less than 2min using a computer with an i7-1185G7 @ 3GHz processor and 32 GB RAM.

## sessionInfo()
```
R version 4.2.1 (2022-06-23 ucrt)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 10 x64 (build 19045)

Matrix products: default

locale:
[1] LC_COLLATE=English_United States.utf8  LC_CTYPE=English_United States.utf8    LC_MONETARY=English_United States.utf8
[4] LC_NUMERIC=C                           LC_TIME=English_United States.utf8    

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] texreg_1.38.6               marginaleffects_0.10.0.9008 ggpubr_0.4.0                stargazer_5.2.3             xtable_1.8-4               
 [6] fixest_0.11.1               readxl_1.4.0                forcats_0.5.1               stringr_1.4.0               dplyr_1.0.9                
[11] purrr_0.3.4                 readr_2.1.2                 tidyr_1.2.0                 tibble_3.1.7                ggplot2_3.3.6              
[16] tidyverse_1.3.2            

loaded via a namespace (and not attached):
 [1] httr_1.4.3          jsonlite_1.8.0      carData_3.0-5       modelr_0.1.9        Formula_1.2-4       assertthat_0.2.1    googlesheets4_1.0.1
 [8] cellranger_1.1.0    numDeriv_2016.8-1.1 pillar_1.7.0        backports_1.4.1     lattice_0.20-45     glue_1.6.2          digest_0.6.29      
[15] checkmate_2.1.0     ggsignif_0.6.3      rvest_1.0.3         colorspace_2.0-3    sandwich_3.0-2      cowplot_1.1.1       pkgconfig_2.0.3    
[22] broom_1.0.1         haven_2.5.0         scales_1.2.0        alpaca_0.3.4        tzdb_0.3.0          googledrive_2.0.0   generics_0.1.2     
[29] farver_2.1.0        car_3.1-0           ellipsis_0.3.2      withr_2.5.0         cli_3.4.1           magrittr_2.0.3      crayon_1.5.1       
[36] fs_1.5.2            fansi_1.0.3         nlme_3.1-157        MASS_7.3-57         rstatix_0.7.0       xml2_1.3.3          textshaping_0.3.6  
[43] dreamerr_1.2.3      tools_4.2.1         data.table_1.14.2   hms_1.1.1           gargle_1.2.1        lifecycle_1.0.1     munsell_0.5.0      
[50] reprex_2.0.2        compiler_4.2.1      systemfonts_1.0.4   rlang_1.0.6         grid_4.2.1          rstudioapi_0.13     labeling_0.4.2     
[57] gtable_0.3.0        abind_1.4-5         DBI_1.1.3           R6_2.5.1            zoo_1.8-10          lubridate_1.8.0     utf8_1.2.2         
[64] insight_0.19.0.2    ragg_1.2.2          stringi_1.7.6       Rcpp_1.0.8.3        vctrs_0.4.1         dbplyr_2.2.0        tidyselect_1.1.2
```

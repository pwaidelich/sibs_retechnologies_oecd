# The role of state investment banks for renewable energy technologies in OECD countries
Replication materials for Waidelich and Steffen (2023). If you have questions about the code or find any errors/bugs, please contact Paul Waidelich at paul.waidelich@gess.ethz.ch (corresponding author).

NOTE: THIS CLEAN REPOSITORY SERVES FOR THE REVIEW PROCESS ONLY TO ALLOW FOR TRANSPARENCY REGARDING THE SCRIPTS & ANALYSES UNDERLYING THE SUBMITTED MANUSCRIPT. AS SOME OF THE DATA OBJECTS IN THE `input` FOLDER FEATURE PROPRIETARY BNEF INFORMATION, THEY CANNOT BE INCLUDED IN THIS PUBLISHED VERSION OF THE REPOSITORY.

## Organization of the overall project repository
The repository features the following elements:
1. The `input` folder, which hosts all data objects (in RDS format) required to replicate all figures, tables, and analyses in the manuscript. Note that for all public repositories, we upload censored versions of data files that do not contain proprietary information from the underlying Bloomberg New Energy Finance databases and instead feature "-999" placeholder values (and FALSE for logical variables). However, these files have the same structure and feature identifier variables, enabling BNEF license holders to merge in the actual information.
2. The `src` folder for scripts, which hosts the script replicating all figures, tables and analyses in the manuscript (`src/replication_main.R`). Note that for some tables, our manuscript applies some additional formatting in LaTeX to the raw output of the script.
3. The `figures` folder, where all figures produced by `src/replication_main.R` are stored as PDF files with a date stamp in the file name. In its original state, the repository features all figures as returned by the script at the time of submission.

## Data objects in the `input` folder
The  `input` features the following RDS files, which are loaded and processed by the `src/replication_main.R` script:
1. `input/df.rds`: deal-level data frame/tibble object including the dependent variable indicating SIB lending (`is_sib_len`), all regressors used throughout the manuscript, the BNEF transaction identifier (`Asset_Finance_ID`), as well as some additional variables required for some of the figures. The latter includes a dummy indicating if the lender information in the BNEF Asset Finance database is listed as "Not Reported" (`is_lender_notreported`), the overall number of lenders including SIBs (`no_lenders`), the share of SIB lending volumes in the deal's total debt as per BNEF information (`sib_lending_debtshare`) and the certainty regarding the SIB lending volume which can be Known, Unknown or Imputable using the no. of lenders (`SIB_lending_certainty`). All other variables are described in more detail in Appendix A of the manuscript.
2. `input/df_lenders.rds`: data frame/tibble object with one row per deal-lender combination that features each lender's name and organization ID in the BNEF Organizations database, as well as the role on the respective deal. In addition, it features our lender classification using the BICS framework and, in case of missing Bloomberg Tickers, additional desk research into "Private banks", "Other financial companies", "Other private sector", "SIBs", "Other public sector" and "Unknown".
3. `input/sib_table.rds`: an organization-level data frame/tibble object with one row per SIB institution/subsidiary (column `Organization_Name`). Subsidiaries are connected to the parent (`main_institution`). Aside from additional organization-level information from the BNEF Organizations database, the object features all deal IDs on which the respective SIB institution/subsidiary is listed as a lender in the BNEF Asset Finance database (`transaction_ids_in_df_reg`) as well as the total number of such transactions (`No_transactions_in_df_reg`)
4. `input/df_capshare_irena.rds`: data frame/tibble object with one row per country-year-RE technology combination that features the respective technology's share in the country's installed capacity as per IRENA data. Details on how the data is obtained can be found in Appendix A of the manuscript.
5. `input/df_fit_oecd.rds`: data frame/tibble object with one row per country-year-RE technology combination that features the inflation-adjusted feed-in tariff. Details on how the data is obtained can be found in Appendix A of the manuscript.

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

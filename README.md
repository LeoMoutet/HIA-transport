Article: Different pathways toward net-zero emissions imply diverging health impacts: a health impact assessment study for France

DOI: https://www.medrxiv.org/content/10.1101/2023.10.03.23296478v2


FILES:
"Data brut": Raw datasets necessary to run the codes. All datasets are publicly available and where extracted before January 2024
            - Insee_data.drs : Population projection (2021-2050), including size, sexe, ages (0-100), deaths, mortality rate and life expectancy
            - data ademe vae.xlsx : Projection of Gpkm made by ademe for walking, cycling, e-cycling
            - den.csv : Age distribution of transport volume for Denmark (sensitivity analysis 4)
            - distri_velo_EMT19_inf50km_jour.csv : Age distribution of cycling volume for France
            - distrib_marche_EMP2019.xlsx : Age distribution of walking volume for France
            - monetarization_yll.csv : Projection of VSLY for France
            
"figures": Figures that have been produced for the submission and revision of the publication. They are named as they appear in the publication.

"sensitivity analysis": All codes to run each sensitivity analysis. They are numbered as they appear in the text and table in the publication

"0_data_management_main_analysis.R": R code to run main analysis and necessary to produce the figures and tables (Only figure S6 is display in the R code : sensitivity analysis/4_data_management_evol_distrib.R)

"figures_tables.R": Figures and tables produced for the publication 

"functions.R": All functions used across the data management codes

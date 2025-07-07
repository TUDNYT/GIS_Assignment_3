# GIS_Assignments
GIS Tutorial Assignment 3
 Research Question-
 Does provinces with high temperature in Spain result in low income?

Data source- 

(i)Temperature data from Copernicus Climate Data Store (CDS)

(ii)Income data from INE (Instituto Nacional de Estadística), which is Spain’s official statistics agency.
Link: https://www.ine.es
The data represents net household income per capita (2020) aggregated at the provincial level.


(iii)Population Density data Population density raster data (1km resolution) for Spain was obtained from 
the WorldPop Project via the Humanitarian Data Exchange (HDX), maintained by the United Nations Office for the Coordination of Humanitarian Affairs (OCHA).
Link: https://data.humdata.org/dataset/worldpop-population-density-for-spain

We focussed on the year 2020 for analysis because this was the year for which we got data from all 3 data sources mentioned above.

(iv)Administrative boundaries data for Spain was obtained from the GADM database of Global Administrative Areas.

Link: https://gadm.org/download_country_v3.html

Points to be noted-
(i)We focussed on the year 2020 for analysis because this was the year for which we got data from all 3 data sources mentioned above.
(ii) We left out the Canary Islands and the Balearic Islands from our analysis because they are not part of mainland Spain.
(iii) We used the province level data for analysis because it is the most granular level of data available for all three datasets.
(iv)The number of unit of Observations came up to be 38, after we dropped the NA values from the dataset.

Plots-
(i)We made simple scatter plots to visualize the relationship between temperature and income, and temperature and population density.
(ii)We tried to exploit the Tmap package to its maximum to make the plots pretty.

Analysis-
(i)We performed a simple linear regression analysis to see if there is a significant relationship between temperature(x) and income(y).
(ii) We added pop density variable to the regression model which acts as a control variable.

Results-
(i)The results came up to be statistically significant for both the regression.
In the simple OLS model, mean temperature was negatively associated with income (β = -165.2, p < 0.01), suggesting that provinces with higher average temperatures tend to have lower mean incomes.

After adding population density as a control, the negative effect of temperature remained significant and even slightly stronger (β = -187.1, p < 0.01).



(ii)We would not like to conclude that the relationship is causal, as there could be other confounding variables that we did not account for in our analysis(education, wealth,etc).
Also, the number of observations is small, which could affect the results.We need more granular data and better econometric method to establish causality.
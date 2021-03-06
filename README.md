# DUIA: the Database on Urban Inequality and Amenities

The Database on Urban Inequality and Amenities (DUIA) includes data on the socio-economic development and amenities of 86 cities. The database especially covers cities outside the West, providing new opportunities for comparative research in fields suffering from a dearth of data. We draw upon remote sensing data from the Atlas of Urban Expansion to more accurately define city boundaries. Second, we draw upon survey data stored in IPUMS to include extensive, harmonized, and disaggregated data. Third, we use open source software and share our scripts to ensure transparency and replicability. DUIA includes information on dwelling and household characteristics, educational attainment, ownership of assets and appliances, and access to amenities. 

![workflow](https://user-images.githubusercontent.com/29236246/106577883-3024df80-653f-11eb-8cc8-57c1c3d08dc8.png)

We adopted the Sample of Cities as defined by the UN-Habitat and the AUE as the starting point to the sampling strategy. Among the variables in IPUMS is a geographical code indicating the second-level subnational administrative unit in which the household was enumerated. To match the data from IPUMS and the AUE, we use an overlay procedure to see whether the second-level administrative unit is part of the city as defined by the AEU.

The second step in the workflow consists in the compilation of all the household samples available for each country and the selection of the samples that correspond to the second-level EA within the boundaries of the study area as defined by the AUE. This generates a series of household-level, multi-temporal sample data frames that are then merged into a unique repository, an integrated, household-level database.

The final step comprises the calculation of the aggregated city-level statistics from the household samples. This generates a data frame with the city as the unit of observation, which can then be integrated with the variables on urban morphology derived from the AUE.

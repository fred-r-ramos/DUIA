# Data selection and extraction

The first step consists in the extraction and integration of the datasets in the IPUMS international and the Atlas of Urban Expansion. 
The folder **Description_IPUMS_Extractions** contains the description of the data extracted form the IPUMS International. 

These files contain the description of the data extraction in IPUMS International. After creating a user account in IPUMS international, replicate the list of variables and samples as described to generate a Fixed-width text (.dat) that can be read in R using the read_ipums_micro( ) function.

https://international.ipums.org/international-action/variables/group

Each extraction generates a multitemporal country wide census sample which includes Second Level Administrative code (GEO2_) used to match the geodataset that can be downloaded directly in IPUMS supplemental data.

https://international.ipums.org/international/gis_yrspecific_2nd.shtml 

The GIS layers corresponding to the study area from the Atlas of Urban Expansion are available at:

http://atlasofurbanexpansion.org/data

# ========================================================================
# The World Bank was founded in 1944 with the goal of reducing poverty in the world. 
# One of its functions is to keep track of the economic development of many countries.
# The world bank provides country-wise data about several indicators via this interface: https://databank.worldbank.org/
# ========================================================================

# ========================================================================
# To access the WDI database directly from R, let's install the WDI package.
# ========================================================================
install.packages("WDI")

# ========================================================================
# Load the WDI package
# ========================================================================
library(WDI)

# ========================================================================
# The WDI package
#    downloads the requested data by using the World Bank's API,
#    parses the resulting XML file,
#    and formats it in long country-year format.
# ========================================================================

# ========================================================================
# Set parameters to define the queries to the WDI package
# ========================================================================

# =======================================
# List of countries.
# Let us consider all European countries.
# For now, I have searched the ISO country codes from here: https://www.iban.com/country-codes
# =======================================
countries <- c("AT", # Austria
               "BE", # Belgium
               "BG", # Bulgaria
               "CH", # Switzerland
               "CY", # Cyprus
               "CZ", # Czech Republic
               "DE", # Germany
               "DK", # Denmark
               "ES", # Spain
               "EE", # Estonia
               "FI", # Finland
               "FR", # France
               "GB", # United Kingdom
               "GE", # Georgia
               "GR", # Greece
               "HR", # Croatia
               "HU", # Hungary
               "IS", # Iceland
               "IT", # Italy
               "LT", # Lithuania
               "LU", # Luxembourg
               "LV", # Latvia
               "NL", # Netherlands 
               "NO", # Norway
               "PL", # Poland
               "PT", # Portugal
               "RO", # Romania
               "SI", # Slovenia
               "SE" # Sweden
               )

# =======================================
# Indicators
# =======================================
indicator_gdp_growth <- "NY.GDP.MKTP.KD.ZG"
indicator_unemployment_total <- "SL.UEM.TOTL.NE.ZS"

# =======================================
# Start and end year
# =======================================
start_year <- 1995
end_year <- 2019
num_years <- end_year - start_year + 1

# ========================================================================
# Get the unemployment at percentage of total labour force
# ========================================================================
unemployment <-  WDI(indicator = indicator_unemployment_total,
                     country = countries,
                     start = start_year,
                     end = end_year)
# The data is read from end_year to start_year for country[1], followed by end_year to start_year for country[2], and so on..
# Also, the countries are arranged in  the alphabetical order of their ISO codes! 
# For now, I have set ordered the values in the vector countries such that it remains the same after being read by WDI.

# ========================================================================
# Impute missing values
# ========================================================================
country_index <- 1
for (country in countries)
{
  tmp_idx1 <- (country_index-1)*num_years+1
  tmp_idx2 <- country_index*num_years
  num_na <- length(which(unemployment$SL.UEM.TOTL.NE.ZS[tmp_idx1:tmp_idx2] %in% NA))
  
  # For this country, replace the NAs with the values of nearby years
  if (num_na > 0)
  {
    print(paste("Number of data points unavailable for ", countries[country_index], ": ", num_na, sep=''))
    print("Imputing missing values... ")
    print("Values before imputation: ")
    print(signif(unemployment$SL.UEM.TOTL.NE.ZS[tmp_idx1:tmp_idx2],2))
    
    tmp <- WDI(indicator = indicator_unemployment_total,
               country = country,
               start = start_year,
               end = end_year)
    
    # set the value of the unavailable data point to the value of the next year.
    # this Works for now, as the data is available for the latest year for all countries.
    na_indices <- which(tmp$SL.UEM.TOTL.NE.ZS %in% NA)
    for (na_index in na_indices)
      tmp$SL.UEM.TOTL.NE.ZS[na_index] <- tmp$SL.UEM.TOTL.NE.ZS[na_index - 1]

    unemployment$SL.UEM.TOTL.NE.ZS[tmp_idx1:tmp_idx2] <- tmp$SL.UEM.TOTL.NE.ZS
    print("Values afteriImputation: ")
    print(signif(unemployment$SL.UEM.TOTL.NE.ZS[tmp_idx1:tmp_idx2],2))
  } 
  country_index <- country_index + 1
}

# ========================================================================
# Get the GDP growth 
# ========================================================================
gdp_growth <-  WDI(indicator = indicator_gdp_growth,
                   country = countries,
                   start = start_year,
                   end = end_year)

# ========================================================================
# Impute missing values
# ========================================================================
country_index <- 1
for (country in countries)
{
  tmp_idx1 <- (country_index-1)*num_years+1
  tmp_idx2 <- country_index*num_years
  num_na <- length(which(gdp_growth$NY.GDP.MKTP.KD.ZG[tmp_idx1:tmp_idx2] %in% NA))
  
  # For this country, replace the NAs with the values of nearby years
  if (num_na > 0)
  {
    print(paste("Number of data points unavailable for ", countries[country_index], ": ", num_na, sep=''))
    print("Imputing missing values... ")
    print("Values before imputation: ")
    print(signif(gdp_growth$NY.GDP.MKTP.KD.ZG[tmp_idx1:tmp_idx2],2))
    
    tmp <- WDI(indicator = indicator_gdp_growth,
               country = country,
               start = start_year,
               end = end_year)
    
    # set the value of the unavailable data point to the value of the next year.
    # this Works for now, as the data is available for the latest year for all countries.
    na_indices <- which(tmp$NY.GDP.MKTP.KD.ZG %in% NA)
    for (na_index in na_indices)
      tmp$NY.GDP.MKTP.KD.ZG[na_index] <- tmp$NY.GDP.MKTP.KD.ZG[na_index - 1]
    
    gdp_growth$NY.GDP.MKTP.KD.ZG[tmp_idx1:tmp_idx2] <- tmp$NY.GDP.MKTP.KD.ZG
    print("Values after imputation: ")
    print(signif(gdp_growth$NY.GDP.MKTP.KD.ZG[tmp_idx1:tmp_idx2],2))
  } 
  country_index <- country_index + 1
}

# ========================================================================
# Plot the Unemployment rate vs the GDP growth (for all countries and all years together)
# ========================================================================
plot(unemployment$SL.UEM.TOTL.NE.ZS, gdp_growth$NY.GDP.MKTP.KD.ZG)

# ========================================================================
# Compute the correlation between the unemployment and the GDP growth
# ========================================================================
corr_gdp_unemployment <- cor(unemployment$SL.UEM.TOTL.NE.ZS, gdp_growth$NY.GDP.MKTP.KD.ZG)
print(paste("Pearson correlation coeeficient between the GDP growth and the Unemplyment rate (all countries and years): ", signif(corr_gdp_unemployment,2), sep=''))
print("As expected, we get a negative correlation between the GDP and unemployment.")

# ========================================================================
# Next step: Find the uncertainty in the correlation value, using bootstrapping
# ========================================================================

# ========================================================================
# Next step: Use permutation test to compute the statistical significance of the correlation
# ========================================================================

# ========================================================================
# Repeat the analysis for 
#   1. GDP growth vs Youth unemployment
#   2. GDP growth vs Male unemployment
#   3. GDP growth vs Female unemployment
#   4. GDP growth vs % of employment in industry sector 1
#   5. GDP growth vs % of employment in industry sector 2
# ========================================================================
# ========================================================================
# The World Bank was founded in 1944 with the goal of reducing poverty in the world. 
# One of its functions is to keep track of the economic development of many countries.
# The world bank provides country-wise data about several indicators via this interface: https://databank.worldbank.org/
# ========================================================================

# ========================================================================
# To access the WDI database directly from R, let's install the WDI package.
# ========================================================================
# install.packages("WDI")

# ========================================================================
# Load the WDI package
# ========================================================================
# library(WDI)

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
               "HR", # Croatia
               "CY", # Cyprus
               "CZ", # Czech Republic
               "DK", # Denmark
               "EE", # Estonia
               "FI", # Finland
               "FR", # France
               "GE", # Georgia
               "DE", # Germany
               "GR", # Greece
               "GL", # Greenland
               "HU", # Hungary
               "IS", # Iceland
               "IT", # Italy
               "LV", # Latvia
               "LI", # Liechtenstein
               "LT", # Lithuania
               "LU", # Luxembourg
               "ME", # Montenegro
               "NL", # Netherlands 
               "NO", # Norway
               "PL", # Poland
               "PT", # Portugal
               "RO", # Romania
               "SI", # Slovenia
               "ES", # Spain
               "SE", # Sweden
               "CH", # Switzerland
               "GB" # United Kingdom
               )

# =======================================
# Indicators
# =======================================
indicator_gdp_growth <- "NY.GDP.MKTP.KD.ZG"
indicator_unemployment_total <- "SL.UEM.TOTL.NE.ZS"

# =======================================
# Start and end year
# =======================================
start_year <- 1990
end_year <- 2019

# ========================================================================
# Get the unemployment at percentage of total labour force
# ========================================================================
unemployment <-  WDI(indicator = indicator_unemployment_total,
                     country = countries,
                     start = start_year,
                     end = end_year)

# ========================================================================
# Get the GDP growth 
# ========================================================================
gdp_growth <-  WDI(indicator = indicator_gdp_growth,
                   country = countries,
                   start = start_year,
                   end = end_year)

# ========================================================================
# Plot the Unemployment rate vs the GDP growth (for all countries and all years together)
# ========================================================================
plot(unemployment$SL.UEM.TOTL.NE.ZS, gdp_growth$NY.GDP.MKTP.KD.ZG)

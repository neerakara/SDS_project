# ========================================================================
# The World Bank was founded in 1944 with the goal of reducing poverty in the world. 
# One of its functions is to keep track of the economic development of many countries.
# The world bank provides country-wise data about several indicators via this interface: https://databank.worldbank.org/
# ========================================================================

# ========================================================================
# If not already done, install and load the WDI package.
# ========================================================================
if(!require(WDI)){
  install.packages("WDI")
  library(WDI)
}

# ========================================================================
# The WDI package
#    downloads the requested data by using the World Bank's API,
#    parses the resulting XML file,
#    and formats it in long country-year format.
# ========================================================================

# ========================================================================
# Set parameters to define the queries to the WDI package
# ========================================================================

# ==========================================
# List of countries.
# Let us consider all European countries.
# Searched the ISO country codes from here: https://www.iban.com/country-codes
# ==========================================
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

# ==========================================
# To do the analysis for a subset of the countries, edit and uncomment the following line 
# ==========================================
# countries <- c("NL")
num_countries <- length(countries)

# ==========================================
# Indicators
# ==========================================
indicator_gdp_growth <- "NY.GDP.MKTP.KD.ZG"
indicator_unemployment_total <- "SL.UEM.TOTL.NE.ZS"

# ==========================================
# Start and end year
# ==========================================
start_year <- 1999
end_year <- 2019
num_years <- end_year - start_year + 1
num_datapoints <- num_countries * num_years

# ========================================================================
# Get the GDP growth 
# ========================================================================
gdp_growth <- WDI(indicator = indicator_gdp_growth,
                  country = countries,
                  start = start_year,
                  end = end_year)

# ==========================================
# Impute missing values
# ==========================================
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
# Get the unemployment at percentage of total labour force
# ========================================================================
unemployment <-  WDI(indicator = indicator_unemployment_total,
                     country = countries,
                     start = start_year,
                     end = end_year)
# The data is read from end_year to start_year for country[1], followed by end_year to start_year for country[2], and so on..
# Also, the countries are arranged in  the alphabetical order of their ISO codes! 
# For now, I have set ordered the values in the vector countries such that it remains the same after being read by WDI.

# ==========================================
# Impute missing values
# ==========================================
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

# ==========================================
# Compute the correlation between the unemployment and the GDP growth
# ==========================================
corr_gdp_unemployment <- cor(unemployment$SL.UEM.TOTL.NE.ZS,
                             gdp_growth$NY.GDP.MKTP.KD.ZG)
print(paste("Pearson correlation coeeficient between the GDP growth and the Unemplyment rate (all countries and years): ",
            signif(corr_gdp_unemployment, 3), sep=''))
print("We get a (slightly) negative correlation between the GDP and unemployment.")

# ==========================================
# Plot the Unemployment rate vs the GDP growth (for all countries and all years together)
# ==========================================
plot(unemployment$SL.UEM.TOTL.NE.ZS,
     gdp_growth$NY.GDP.MKTP.KD.ZG,
     main=paste("All countries and years. Correlation: ", signif(corr_gdp_unemployment, 3), sep=''),
     xlab="Umemployment %",
     ylab="GDP Growth")

# ==========================================
# Plot country-wise (each country in different colours)
# This seems to be very hard to see. If required, plot this as a separate subplot for each country.
# ==========================================
country_index <- 1
for (country in countries)
{
  tmp_idx1 <- (country_index-1)*num_years+1
  tmp_idx2 <- country_index*num_years

  unemployment_this_country <- unemployment$SL.UEM.TOTL.NE.ZS[tmp_idx1:tmp_idx2]
  gdp_growth_this_country <- gdp_growth$NY.GDP.MKTP.KD.ZG[tmp_idx1:tmp_idx2]
  
  
  if(country == 'PT')
  {
    print(country)
    plot(unemployment_this_country,
         gdp_growth_this_country,
         type = "l",
         col = rgb(0.01*sample(100,1),0.01*sample(100,1),0.01*sample(100,1), alpha=0.75),
         lwd=2,
         xlim=c(0,30),
         ylim=c(-15,15),
         ann=FALSE)
  }
  else if(country == 'PL')
  {
    print(country)
    plot(unemployment_this_country,
         gdp_growth_this_country,
         type = "l",
         col = rgb(0.01*sample(100,1),0.01*sample(100,1),0.01*sample(100,1), alpha=0.75),
         lwd=2,
         xlim=c(0,30),
         ylim=c(-15,15),
         ann=FALSE)
  }
  else if(country == 'NL')
  {
    print(country)
    plot(unemployment_this_country,
         gdp_growth_this_country,
         type = "l",
         col = rgb(0.01*sample(100,1),0.01*sample(100,1),0.01*sample(100,1), alpha=0.75),
         lwd=2,
         xlim=c(0,30),
         ylim=c(-15,15),
         ann=FALSE)
  }
  else if(country == 'IS')
  {
    print(country)
    plot(unemployment_this_country,
         gdp_growth_this_country,
         type = "l",
         col = rgb(0.01*sample(100,1),0.01*sample(100,1),0.01*sample(100,1), alpha=0.75),
         lwd=2,
         xlim=c(0,30),
         ylim=c(-15,15),
         ann=FALSE)
  }
  else
  {
    plot(unemployment_this_country,
         gdp_growth_this_country,
         type = "l",
         col = rgb(0.01*sample(100,1),0.01*sample(100,1),0.01*sample(100,1), alpha=0.15),
         lwd=2,
         xlim=c(0,30),
         ylim=c(-15,15),
         ann=FALSE,
         axes=FALSE)
  }
  
  par(new=T)

  country_index <- country_index + 1
}
par(new=F)

# ========================================================================
# Bootstrapping to obtain an uncertainty on the correlation coefficient
# ========================================================================
# obtain M bootstrap samples and calculate the correlation coefficient every time
M <- 10000
corr_gdp_unemployment_bootstrap <- numeric(M)
for(i in 1:M)
{
  sampling_indices <- sample(seq(1, length(unemployment$SL.UEM.TOTL.NE.ZS), by=1), size=num_datapoints, replace=TRUE)

  corr_gdp_unemployment_bootstrap[i] <- cor(unemployment$SL.UEM.TOTL.NE.ZS[sampling_indices],
                                            gdp_growth$NY.GDP.MKTP.KD.ZG[sampling_indices])
}

bt_mean <- signif(mean(corr_gdp_unemployment_bootstrap), 3)
bt_std <- signif(sd(corr_gdp_unemployment_bootstrap), 3)
bt_perc97 <- signif(quantile(corr_gdp_unemployment_bootstrap, probs=c(0.975)), 3)
bt_perc2 <- signif(quantile(corr_gdp_unemployment_bootstrap, probs=c(0.025)), 3)

hist(corr_gdp_unemployment_bootstrap,
     main=paste("Bootstrapped values of the correlation coefficient: mean", bt_mean, ", std.dev.: ", bt_std, sep=''),
     sub = "Red line shows the measured value.",
     xlab="Correlation (GDP, Unemployment)")
abline(v=corr_gdp_unemployment, col="red")

print(paste("The measured coefficient is ", signif(corr_gdp_unemployment, 3), sep=''))
print(paste("And the mean of the bootstrapped coefficient is ", bt_mean, sep=''))
print("Thus, the measurement is unbiased but there is quite a variance in the results:")
print(paste("The standard deviation is ", bt_std, sep=''))
paste("The 95% density interval goes from ", bt_perc97, " to ", bt_perc2, sep='')
print("Thus, the real correlation coefficient value for a larger sample could vary quite a bit.")

# ========================================================================
# Permutation test to compute the statistical significance of the correlation
# ========================================================================
# repeat shuffling for N times
N <- 10000 
# vector with the results of the test statistic under each permutation
corr_gdp_unemployment_permutation_test <- numeric(length = N) 
for(i in 1:N)
{
  unemployment_shufled <- unemployment[sample(nrow(unemployment)),]
  corr_gdp_unemployment_permutation_test[i] <- cor(unemployment_shufled$SL.UEM.TOTL.NE.ZS,
                                                   gdp_growth$NY.GDP.MKTP.KD.ZG)
}

# ==========================================
# Computing the p-value of the permutation test
# A nice explanation of permutation test can be read here: https://www.jwilber.me/permutationtest/
# ==========================================
p_value_Cor <- (sum(corr_gdp_unemployment_permutation_test<=corr_gdp_unemployment)+1)/length(corr_gdp_unemployment_permutation_test)
print(paste("p-value: ", signif(p_value_Cor, 3), sep=''))
print("As the p-value is smaller than 0.05, we can say that the observed correlation is statistically significant.")

# ==========================================
# Plotting the histogram of the computed correlation coefficients.
# Showing as a red line, the correlation coefficient between the unshuffled variables.
# ==========================================
hist(corr_gdp_unemployment_permutation_test,
     xlim=range(c(corr_gdp_unemployment_permutation_test, corr_gdp_unemployment)),
     main=paste("Permutation test. P-value: ", signif(p_value_Cor, 3), sep=''),
     sub = "Red line shows the measured value.",
     xlab="Correlation (GDP, Unemployment)")
abline(v=corr_gdp_unemployment, col="red")

# ========================================================================
# Repeat the analysis for 
#   1. GDP growth vs Youth unemployment
#   2. GDP growth vs Male unemployment
#   3. GDP growth vs Female unemployment
#   4. GDP growth vs % of employment in industry sector 1
#   5. GDP growth vs % of employment in industry sector 2
# ========================================================================

# ========================================================================
# Other available indicators related to employment
# ========================================================================
# Some indicators are available in two formats: 1. national estimate and 2. modeled ILO estimate
# ========================================================================
# Unemployment, total (% of total labor force) (national estimate)(SL.UEM.TOTL.NE.ZS)
# Unemployment, total (% of total labor force) (modeled ILO estimate)(SL.UEM.TOTL.ZS)
# ===================
# Unemployment, youth total (% of total labor force ages 15-24) (national estimate)(SL.UEM.1524.NE.ZS)
# Unemployment, youth total (% of total labor force ages 15-24) (modeled ILO estimate)(SL.UEM.1524.ZS)
# ===================
# Unemployment, male (% of male labor force) (national estimate)(SL.UEM.TOTL.MA.NE.ZS)
# Unemployment, male (% of male labor force) (modeled ILO estimate)(SL.UEM.TOTL.MA.ZS)
# ===================
# Unemployment, female (% of female labor force) (national estimate)(SL.UEM.TOTL.FE.NE.ZS)
# Unemployment, female (% of female labor force) (modeled ILO estimate)(SL.UEM.TOTL.FE.ZS)
# ===================
# Employment in agriculture (% of total employment) (modeled ILO estimate) (SL.AGR.EMPL.ZS)
# The agriculture sector consists of activities in agriculture, hunting, forestry and fishing.
# ===================
# Employment in industry (% of total employment) (modeled ILO estimate) (SL.IND.EMPL.ZS)
# The industry sector consists of mining and quarrying, manufacturing, construction, and public utilities (electricity, gas, and water)
# ===================
# Employment in services (% of total employment) (SL.SRV.EMPL.ZS)
# The services sector consists of wholesale and retail trade and restaurants and hotels;
# transport, storage, and communications; financing, insurance, real estate, and business services; and community, social, and personal services
# ===================
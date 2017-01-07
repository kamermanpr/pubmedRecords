############################################################
#                                                          #
#       Generate sysdata.rda of country and US state       #
#         names for filtering country affiliations         #
#                                                          #
############################################################

#-- Country codes --------------------------------------------------------#

# Clean data
country_data <- countrycode::countrycode_data
country_data <- country_data[complete.cases(country_data), ]

# Extract regex name/ISO3 codes from 'countrycode::countrycode_data'
country <- country_data$country.name
regex <- country_data$regex
iso3 <- country_data$iso3c

# Collapse extracted country names/codes into one string, separated by '|'
country_names <- paste(country,
                       collapse = '|')

country_regex <- paste(regex,
                       collapse = '|')

country_iso3 <- paste(iso3,
                      collapse = '|')

country_iso3 <- paste0(country_iso3, '|UK') # Add UK to ISO-3

#-- US states ------------------------------------------------------------#

 # Extract US state names and abbreviations from 'maps::state.fips'
 code <- unique(
     as.character(maps::state.fips$abb))
 state <- stringr::str_to_title(
     unique(
         stringr::str_extract(
             as.character(maps::state.fips$polyname),
             pattern = '^.+(?=:.)|^.+')))

 # Collapse extracted state names/codes into one string, separated by '|'
 us_state_codes <- paste(code,
                         collapse = '|')

 us_state_names <- paste(state,
                         collapse = '|')

#-- Save (internal use only) ---------------------------------------------#

# Compile into list
affiliation_filters <- list(country_names = country_names,
                            country_regex = country_regex,
                            country_iso3 = country_iso3,
                            us_state_names = us_state_names,
                            us_state_codes = us_state_codes)

# Save
devtools::use_data(affiliation_filters,
                   internal = TRUE,
                   overwrite = TRUE)

############################################################
#                                                          #
#             Generate sysdata.rda of country              #
#         names for filtering country affiliations         #
#                                                          #
############################################################

# Extract country names, World Bank/ISO2/ISO3 codes from
# 'countrycode::countrycode_data'
country_name <- countrycode::countrycode_data$country.name
country_wb <- countrycode::countrycode_data$wb
country_iso3 <- countrycode::countrycode_data$iso3c
country_iso2 <- countrycode::countrycode_data$iso2c

# Extract US state names and abbreviations from 'maps::state.fips'
states_abbr <- unique(
    as.character(maps::state.fips$abb))
states_names <- stringr::str_to_title(
    unique(
        stringr::str_extract(
            as.character(maps::state.fips$polyname),
                         pattern = '^.+(?=:.)|^.+')))

# Collapse extracted names/codes into one string, separated by '|'
country_filter <- paste(c(states_abbr, states_names,
                          country_name, country_iso2,
                          country_iso3, country_wb),
                        collapse = '|')

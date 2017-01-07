#' @title Extract the name of the country (along with its geographical region and continent) listed in the primary affiliation of the first-author of each unique PubMed record.
#
#' @description \code{parse_affiliation} is used primarily as a utility function within \code{\link{get_records}}. It takes the xml output from an \href{http://www.ncbi.nlm.nih.gov/pubmed}{PubMed} database query and returns a dataframe with the name of the country (and state code if the USA) listed under the primary affiliation of the first-author of each PubMed entry returned by the query. The extraction of country names is based on full country names and country ISO-3 short name codes (and UK). In addition, the continent and region assignment of the country, based on \href{http://data.worldbank.org/data-catalog/world-development-indicators}{World Bank Development Indicators}, is also returned. The region and continent data are generated using the \code{\link[countrycode]{countrycode}} function from the \pkg{countrycode} package. The country/continental/regional distribution data are useful for assessing the geographical distribution of scientific outputs.
#'
#' @param record The xml output from an PubMed database query. Typically the database query is specified in the \emph{terms} parameter of the \code{\link{get_records}} function, and the downloaded xml output returned by the query is passed to \code{parse_affiliation} for parsing. The parsed affiliation data are then incorporated into the dataframe output of \code{\link{get_records}}. However, PubMed query data in the form of an xml file downloaded from PubMed and results returned by the \code{\link[rentrez]{entrez_fetch}} function from the \pkg{rentrez} package may be passed to the function independently of \code{\link{get_records}}.
#'
#' @return Returns a long-format dataframe with the following columns:
#' \describe{
#' \item{pmid}{PubMed unique identification code for each entry \emph{(class: integer)}.}
#' \item{authors}{Character string specifying author name \emph{format: surname initials}.}
#' \item{country_name}{Character string specifying the name of the country listed under the primary affiliation of the first-author. Returns \code{NA} if no country name is included in the affiliation data.}
#' \item{state_code}{Character string specifying the two-letter state identification code listed under the primary affiliation of the first-author, when the country is the USA. Returns \code{NA} for countries other than the USA, and when no state is included in US-based affiliations.}
#' \item{continent}{Character string specifying the continent a country belongs to as defined in the World Bank Development Indicators. Returns \code{NA} if no country name is included in the affiliation data.}
#' \item{region}{Character string specifying the geographical region a country belongs to as defined in the World Bank Development Indicators. Returns \code{NA} if no country name is included in the affiliation data.}}
#'
#' @family related functions
#'
#' @examples
#' # Get publication record of Prof RD Treede from a downloaded PubMed xml file
#' foo <- treede()
#' # Get first-author primary affiliation information
#' parse_affiliations(foo)
#'
#' \donttest{
#' # Get publication record of Prof RD Treede using rentrez and xml2 packages
#' # Return query PMIDs
#' bar <- rentrez::entrez_search(db = 'pubmed', term = '(Treede RD[AUTH]) AND
#' hasabstract AND (journal article[PTYP]) AND (1980/01/01:2017/01/01[PDAT])',
#' retmode = 'xml', retmax = 248)$ids
#' # Fetch results
#' baz <- xml2::read_xml(rentrez::entrez_fetch(db = 'pubmed', id = bar,
#' rettype = 'xml', retmode = 'xml', parsed = FALSE))
#' # Get first-author primary affiliation data
#' parse_affiliations(baz)}
#'
#' @export
parse_affiliations <- function(record) {

    ############################################################
    #                                                          #
    #                        Set xpaths                        #
    #                                                          #
    ############################################################

    #-- Affliiations -----------------------------------------------------#

    affil_path <- xml2::xml_path(
            xml2::xml_find_all(record,
                           './/Affiliation'))

    #-- Author info for each affiliation ---------------------------------#

    auth_path <- stringr::str_extract(affil_path,
                                      '/PubmedArticleSet/PubmedArticle\\[[0-9][0-9]?[0-9]?\\]/MedlineCitation/Article/AuthorList/Author\\[[0-9][0-9]?[0-9]?\\]')

    #-- Pmid for each record ---------------------------------------------#

    pmid_path <- xml2::xml_path(
        xml2::xml_find_all(record,
                           ".//ArticleId[@IdType = 'pubmed']"))

    #-- Article type (for final dataframe filtering) ---------------------#

    # Set path for articles classified as 'journal articles'
    type_path <- xml2::xml_path(
        xml2::xml_find_all(record,
                           ".//PublicationType"))

    # Trim to /PubmedArticleSet/PubmedArticle[???]
    type_path <- stringr::str_extract(type_path,
                                      '/PubmedArticleSet/PubmedArticle\\[[0-9][0-9]?[0-9]?\\]')


    ############################################################
    #                                                          #
    #                   Extract information                    #
    #                                                          #
    ############################################################

    #-- Affiliation ------------------------------------------------------#

    affil <- c() # empty vector for affiliation

    for(i in 1:length(affil_path)) {
        affil[[i]] <- xml2::xml_text(
            xml2::xml_find_first(record,
                                 affil_path[[i]]))
    }

    #-- Author surname ---------------------------------------------------#

    auth_1 <- c() # empty vector for surname

    for(j in 1:length(auth_path)) {
        auth_1[[j]] <- stringr::str_to_lower(
            xml2::xml_text(
                xml2::xml_find_first(record,
                                     paste0(auth_path[[j]],
                                            '/LastName'))))
    }

    #-- Author initials --------------------------------------------------#

    auth_2 <- c() # empty vector for initials

    for(k in 1:length(auth_path)) {
        auth_2[[k]] <- stringr::str_to_lower(
            xml2::xml_text(
                xml2::xml_find_first(record,
                                     paste0(auth_path[[k]],
                                            '/Initials'))))
    }

    ############################################################
    #                                                          #
    #       Construct parse_bibliographics joining info        #
    #                                                          #
    ############################################################

    #-- Total affiliation count for each record --------------------------#

    counter <- stringr::str_extract(affil_path,
                                    '/PubmedArticleSet/PubmedArticle\\[[0-9][0-9]?[0-9]?\\]') %>%
        dplyr::tbl_df() %>%
        tibble::rownames_to_column() %>%
        dplyr::group_by(value) %>%
        dplyr::summarise(count = n()) %>%
        dplyr::mutate(order = as.numeric(
            stringr::str_extract(value, '[0-9][0-9]?[0-9]?'))) %>%
        dplyr::arrange(order)

    #-- Pmid for each record ---------------------------------------------#

    pmid <- c() # empty vector for pmid (for join: parse_records)

    for(l in 1:length(pmid_path)) {
        pmid[[l]] <- xml2::xml_text(
            xml2::xml_find_first(record,
                               pmid_path[[l]]))
    }

    # Add node path to PubmedArticle[???] to join with 'counter'
    pmid_count <- pmid %>%
        dplyr::tbl_df() %>%
        dplyr::rename(pmid = value) %>%
        dplyr::mutate(value = pmid_path) %>%
        dplyr::mutate(value = stringr::str_extract(value,
                                                  '/PubmedArticleSet/PubmedArticle\\[[0-9][0-9]?[0-9]?\\]'))

    # Join counter to length 'pmid'
    counter <- counter %>%
        dplyr::right_join(pmid_count) %>%
        tibble::rownames_to_column(var = 'order2') %>%
        dplyr::mutate(order = order2) %>%
        dplyr::rename(article_node = value) %>%
        dplyr::select(article_node, pmid, count) %>%
        dplyr::mutate(count = stringr::str_replace_na(count),
                      count = stringr::str_replace(count,
                                                   'NA',
                                                   0))

    # Expand pmid to length 'affil'
    pmid <- purrr::map2(.x = counter$pmid,
                        .y = counter$count,
                        .f = rep) %>%
        unlist()

    # Expand article_node to length 'affil'
    node <- purrr::map2(.x = counter$article_node,
                        .y = counter$count,
                        .f = rep) %>%
        unlist()

    ############################################################
    #                                                          #
    #                    Put it all together                   #
    #                                                          #
    ############################################################

    #-- Make into dataframe ----------------------------------------------#

    affil_df <- dplyr::data_frame(author_node = affil_path,
                                  surname = auth_1,
                                  initials = auth_2,
                                  affiliation = affil,
                                  pmid = pmid) %>%
        # Merge surname and initials for joins with output from parse_record
        tidyr::unite(authors, surname, initials, sep = ' ') %>%
        dplyr::select(author_node, pmid, authors, affiliation) %>%
        # ID primary affiliation of each first author
        dplyr::mutate(author_node = stringr::str_extract(author_node, 'Author\\[1\\]/AffiliationInfo/Affiliation|Author\\[1\\]/AffiliationInfo\\[1\\]/Affiliation')) %>%
        dplyr::filter(!is.na(author_node)) %>%
        dplyr::select(-author_node)

    #-- Country/state/region/continent -----------------------------------#

    # State name vs state code mapping
    states <- strsplit(affiliation_filters$us_state_names,
                       split = '[|]')
    code <- strsplit(affiliation_filters$us_state_codes,
                     split = '[|]')
    state_filter <- dplyr::data_frame(states = states[[1]],
                                      codes = code[[1]])

    # Cleaning
    affil_country <- affil_df %>%
        dplyr::mutate(country_name =
                          stringr::str_extract(affiliation,
                                               affiliation_filters$country_names),
                      # In case regex-based search needed in the future
                      # country_regex =
                          # stringr::str_extract(affiliation,
                                               # affiliation_filters$country_regex),
                      country_iso3 = ifelse(is.na(country_name),
                                            yes = stringr::str_extract(affiliation,
                                                affiliation_filters$country_iso3),
                                            no = NA),
                      state_name = ifelse(country_iso3 == 'USA',
                                          yes = stringr::str_extract(affiliation,
                                                affiliation_filters$us_state_names),
                                          no = NA),
                      state_code = ifelse(country_iso3 == 'USA',
                                          yes = stringr::str_extract(affiliation,
                                                affiliation_filters$us_state_codes),
                                          no = NA)) %>%
        # Country name and region/continent conversions
        dplyr::mutate(country_iso3 = stringr::str_replace(country_iso3,
                                                          pattern = 'UK',
                                                          replacement = 'GBR'),
                      country_name = ifelse(!is.na(country_iso3),
                                            yes = countrycode::countrycode(
                                                sourcevar = country_iso3,
                                                origin = 'iso3c',
                                                destination = 'country.name'),
                                            no = country_name),
                      country_iso3 = ifelse(!is.na(country_name),
                                            yes = countrycode::countrycode(
                                                sourcevar = country_name,
                                                origin = 'country.name',
                                                destination = 'iso3c'),
                                            no = country_iso3),
                      region = ifelse(!is.na(country_name),
                                      yes = countrycode::countrycode(
                                          sourcevar = country_name,
                                          origin = 'country.name',
                                          destination = 'region'),
                                      no = NA),
                      continent = ifelse(!is.na(country_name),
                                                yes = countrycode::countrycode(
                                                    sourcevar = country_name,
                                                    origin = 'country.name',
                                                    destination = 'continent'),
                                                no = NA)) %>%
        dplyr::select(-affiliation)

    # Add missing US state codes using 'state_filter'
    for(i in 1:nrow(affil_country)) {
        if(!is.na(affil_country$state_name[[i]]) &
           is.na(affil_country$state_code[[i]])) {
            affil_country$state_code[[i]] <-
                state_filter[state_filter$states ==
                                 affil_country$state_name[[i]], 'codes'][[1]]
        }
    }

    #-- Output ----------------------------------------------------------------#

   affil_out <- affil_country %>%
        dplyr::select(-state_name)

   return(affil_out)

}

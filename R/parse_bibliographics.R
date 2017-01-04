#' @title Extract bibliographic information from an PubMed xml record.
#
#' @description \code{parse_bibliographics} is used primarily as a utility function within \code{\link{get_records}}. It takes the xml output from an \href{http://www.ncbi.nlm.nih.gov/pubmed}{PubMed} database query and returns a dataframe of bibliographics information for each \href{http://www.ncbi.nlm.nih.gov/pubmed}{PubMed} journal article entry returned by the query.
#'
#' @param record The xml output from an \href{http://www.ncbi.nlm.nih.gov/pubmed}{PubMed} database query. Typically the database query is specified in the \emph{terms} parameter of the \code{\link{get_records}} function, and the downloaded xml output returned by the query is passed to \code{parse_bibliographics} for parsing. The parsed bibliographic data are then incorporated into the dataframe output of \code{\link{get_records}}. However, any xml PubMed query may be passed to the function independently of \code{\link{get_records}}, including xml files downloaded from \href{http://www.ncbi.nlm.nih.gov/pubmed}{PubMed} and unparsed results returned by \code{\link[rentrez]{entrez_fetch}}.
#'
#' @importFrom magrittr %>%
#'
#' @family related functions
#'
#' @seealso \code{\link[rentrez]{enterez_search}} and \code{\link[rentrez]{enterez_fetch}}
#'
#' @examples
#' # Parse an xml file downloaded from PubMed
#' ## Get PMIDs
#' pmid_string <- xml2::read_xml(
#' paste0('https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pubmed&term=Treede+RD[AUTH]&rettype=xml&retmax=1000')) %>%
#' xml2::xml_find_all(., xpath = './/Id') %>%
#' xml2::xml_integer(.)
#' ## Construct PubMed eFetch query
#' url <- paste0('https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=pubmed&id=', paste(pmid_string, collapse = ','), '&retmode=xml')
#' ## Download xml record
#' xml_record <- xml2::read_xml(url)
#' ## Parse record
#' parse_bibliographics(record = xml_record)
#'
#' # Parse an xml file returned by rentrez::entrez_fetch
#' ## Get PMIDs
#' pmid_string <- rentrez::entrez_search(db = 'pubmed', term = 'Treede RD[AUTH]', retmod = 'xml', retmax = 1000)$ids
#' ## Fetch records
#' rentrez_record <- paste0('https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=pubmed&id=', paste(pmid_string, collapse = ','), '&retmode=xml')
#' ## Parse record
#' parse_bibliographics(record = rentrez_record)
#'
#' @export
parse_bibliographics <- function(record) {

    ############################################################
    #                                                          #
    #                        Set xpaths                        #
    #                                                          #
    ############################################################

    #-- Article type (for final dataframe filtering) ---------------------#

    # Set path for articles classified as 'journal articles'
    type_path <- xml2::xml_path(
        xml2::xml_find_all(record,
                           ".//PublicationType"))

    # Trim to /PubmedArticleSet/PubmedArticle[???]
    type_path <- stringr::str_extract(type_path,
                                      '/PubmedArticleSet/PubmedArticle\\[[0-9][0-9]?[0-9]?\\]')

    #-- Surname ----------------------------------------------------------#

    surname_path <- xml2::xml_path(
            xml2::xml_find_all(record,
                               './/LastName'))

    #-- Forename ---------------------------------------------------------#

    forename_path <- xml2::xml_path(
            xml2::xml_find_all(record,
                               './/ForeName'))

    #-- Initials ---------------------------------------------------------#

    initials_path <- xml2::xml_path(
            xml2::xml_find_all(record,
                               './/Initials'))

    #-- Title ------------------------------------------------------------#

    title_path <- xml2::xml_path(
            xml2::xml_find_all(record,
                               './/ArticleTitle'))

    #-- Journal ----------------------------------------------------------#

    journal_path <- xml2::xml_path(
            xml2::xml_find_all(record,
                               './/ISOAbbreviation'))

    #-- Volume -----------------------------------------------------------#

    volume_path <- xml2::xml_path(
        xml2::xml_find_all(record,
                           './/Volume'))

    #-- Issue ------------------------------------------------------------#

    issue_path <- xml2::xml_path(
        xml2::xml_find_all(record,
                           './/Issue'))

    #-- Year -------------------------------------------------------------#

    year_path <- xml2::xml_path(
        xml2::xml_find_all(record,
                           './/PubDate/Year'))

    #-- Pages ------------------------------------------------------------#

    pages_path <- xml2::xml_path(
        xml2::xml_find_all(record,
                           './/MedlinePgn'))

    #-- PMID -------------------------------------------------------------#

    pmid_path <- xml2::xml_path(
        xml2::xml_find_all(record,
                           ".//ArticleId[@IdType = 'pubmed']"))

    #-- DOI --------------------------------------------------------------#

    doi_path <- xml2::xml_path(
        xml2::xml_find_all(record,
                           ".//ArticleId[@IdType = 'doi']"))

    #-- Abstract ---------------------------------------------------------#

    abstract_path <- xml2::xml_path(
        xml2::xml_find_all(record,
                           './/Abstract'))

    ############################################################
    #                                                          #
    #                   Extract information                    #
    #                                                          #
    ############################################################

    #-- Surname ----------------------------------------------------------#

    surname <- c() # empty vector for type

    for(i in 1:length(surname_path)) {
        surname[[i]] <- stringr::str_to_lower(
            xml2::xml_text(
                xml2::xml_find_first(record,
                                     surname_path[[i]])))
    }

    #-- Initials ---------------------------------------------------------#

    initials <- c() # empty vector for initials

    for(i in 1:length(initials_path)) {
        initials[[i]] <- stringr::str_to_lower(
            xml2::xml_text(
                xml2::xml_find_first(record,
                                     initials_path[[i]])))
    }

    # For joins with output from parse_affiliation
    authors <- paste(surname, initials)

    #-- Forename ---------------------------------------------------------#

    forename <- c() # empty vector for forename

    for(i in 1:length(forename_path)) {
        forename[[i]] <- stringr::str_to_lower(
            xml2::xml_text(
                xml2::xml_find_first(record,
                                     forename_path[[i]])))
    }

    # Retain first name only
    forename <- stringr::str_extract(forename, '^\\w+')

    #-- Title ------------------------------------------------------------#

    title <- c() # empty vector for title

    for(i in 1:length(title_path)) {
        title[[i]] <- stringr::str_to_lower(
            xml2::xml_text(
                xml2::xml_find_first(record,
                                     title_path[[i]])))
    }

    # Make article marker for joins
    title_path2 <- c() # empty vector for 'trimmed' title path

    for(i in 1:length(title_path)) {
        title_path2[[i]] <- stringr::str_extract(title_path[[i]],
                                            '/PubmedArticleSet/PubmedArticle\\[[0-9][0-9]?[0-9]?\\]')
    }

    # Make dataframe
    title2 <- dplyr::data_frame(article_node = title_path2,
                                title = title)

    #-- Journal ----------------------------------------------------------#

    journal <- c() # empty vector for journal

    for(i in 1:length(journal_path)) {
        journal[[i]] <- stringr::str_to_lower(
            xml2::xml_text(
                xml2::xml_find_first(record,
                                     journal_path[[i]])))
    }

    # Make article marker for joins
    journal_path2 <- c() # empty vector for 'trimmed' journal path

    for(i in 1:length(journal_path)) {
        journal_path2[[i]] <- stringr::str_extract(journal_path[[i]],
                                                 '/PubmedArticleSet/PubmedArticle\\[[0-9][0-9]?[0-9]?\\]')
    }

    # Make dataframe
    journal2 <- dplyr::data_frame(article_node = journal_path2,
                               journal = journal) %>%
        dplyr::mutate(journal = stringr::str_replace_all(journal,
                                                         pattern = '[.]',
                                                         replacement = ''))

    #-- volume ---------------------------------------------------------#

    volume <- c() # empty vector for volume

    for(i in 1:length(volume_path)) {
        volume[[i]] <- stringr::str_to_lower(
            xml2::xml_text(
                xml2::xml_find_first(record,
                                     volume_path[[i]])))
    }

    # Make article marker for joins
    volume_path2 <- c() # empty vector for 'trimmed' volume path

    for(i in 1:length(volume_path)) {
        volume_path2[[i]] <- stringr::str_extract(volume_path[[i]],
                                                 '/PubmedArticleSet/PubmedArticle\\[[0-9][0-9]?[0-9]?\\]')
    }

    # Make dataframe
    volume2 <- dplyr::data_frame(article_node = volume_path2,
                                 volume = volume)

    #-- issue ---------------------------------------------------------#

    issue <- c() # empty vector for issue

    for(i in 1:length(issue_path)) {
        issue[[i]] <- stringr::str_to_lower(
            xml2::xml_text(
                xml2::xml_find_first(record,
                                     issue_path[[i]])))
    }

    # Make article marker for joins
    issue_path2 <- c() # empty vector for 'trimmed' issue path

    for(i in 1:length(issue_path)) {
        issue_path2[[i]] <- stringr::str_extract(issue_path[[i]],
                                                 '/PubmedArticleSet/PubmedArticle\\[[0-9][0-9]?[0-9]?\\]')
    }

    # Make dataframe
    issue2 <- dplyr::data_frame(article_node = issue_path2,
                                issue = issue)

    #-- year ---------------------------------------------------------#

    year <- c() # empty vector for year

    for(i in 1:length(year_path)) {
        year[[i]] <- stringr::str_to_lower(
            xml2::xml_text(
                xml2::xml_find_first(record,
                                     year_path[[i]])))
    }

    # Make article marker for joins
    year_path2 <- c() # empty vector for 'trimmed' year path

    for(i in 1:length(year_path)) {
        year_path2[[i]] <- stringr::str_extract(year_path[[i]],
                                                 '/PubmedArticleSet/PubmedArticle\\[[0-9][0-9]?[0-9]?\\]')
    }

    # Make dataframe
    year2 <- dplyr::data_frame(article_node = year_path2,
                               year = year)

    #-- pages ---------------------------------------------------------#

    pages <- c() # empty vector for pages

    for(i in 1:length(pages_path)) {
        pages[[i]] <- stringr::str_to_lower(
            xml2::xml_text(
                xml2::xml_find_first(record,
                                     pages_path[[i]])))
    }

    # Make article marker for joins
    pages_path2 <- c() # empty vector for 'trimmed' pages path

    for(i in 1:length(pages_path)) {
        pages_path2[[i]] <- stringr::str_extract(pages_path[[i]],
                                                '/PubmedArticleSet/PubmedArticle\\[[0-9][0-9]?[0-9]?\\]')
    }

    # Make dataframe
    pages2 <- dplyr::data_frame(article_node = pages_path2,
                                pages = pages)

    #-- pmid ---------------------------------------------------------#

    pmid <- c() # empty vector for pmid

    for(i in 1:length(pmid_path)) {
        pmid[[i]] <- stringr::str_to_lower(
            xml2::xml_text(
                xml2::xml_find_first(record,
                                     pmid_path[[i]])))
    }

    # Make article marker for joins
    pmid_path2 <- c() # empty vector for 'trimmed' pmid path

    for(i in 1:length(pmid_path)) {
        pmid_path2[[i]] <- stringr::str_extract(pmid_path[[i]],
                                                '/PubmedArticleSet/PubmedArticle\\[[0-9][0-9]?[0-9]?\\]')
    }

    # Make dataframe
    pmid2 <- dplyr::data_frame(article_node = pmid_path2,
                               pmid = pmid)

    #-- doi ---------------------------------------------------------#

    doi <- c() # empty vector for doi

    for(i in 1:length(doi_path)) {
        doi[[i]] <- stringr::str_to_lower(
            xml2::xml_text(
                xml2::xml_find_first(record,
                                     doi_path[[i]])))
    }

    # Make article marker for joins
    doi_path2 <- c() # empty vector for 'trimmed' doi path

    for(i in 1:length(doi_path)) {
        doi_path2[[i]] <- stringr::str_extract(doi_path[[i]],
                                                '/PubmedArticleSet/PubmedArticle\\[[0-9][0-9]?[0-9]?\\]')
    }

    # Make dataframe
    doi2 <- dplyr::data_frame(article_node = doi_path2,
                              doi = doi)

    #-- Abstract ---------------------------------------------------------#

    abstract <- c() # empty vector for abstract

    for(i in 1:length(abstract_path)) {
        abstract[[i]] <- stringr::str_to_lower(
            xml2::xml_text(
                xml2::xml_find_first(record,
                                     abstract_path[[i]])))
    }

    # Make article marker for joins
    abstract_path2 <- c() # empty vector for 'trimmed' abstract path

    for(i in 1:length(abstract_path)) {
        abstract_path2[[i]] <- stringr::str_extract(abstract_path[[i]],
                                                '/PubmedArticleSet/PubmedArticle\\[[0-9][0-9]?[0-9]?\\]')
    }

    # Make dataframe
    abstract2 <- dplyr::data_frame(article_node = abstract_path2,
                                   abstract = abstract)

    ############################################################
    #                                                          #
    #        Construct parse_affiliations joining info         #
    #                                                          #
    ############################################################

    #-- Total author count for each record -------------------------------#

    counter <- stringr::str_extract(surname_path,
                                    '/PubmedArticleSet/PubmedArticle\\[[0-9][0-9]?[0-9]?\\]') %>%
        dplyr::tbl_df() %>%
        tibble::rownames_to_column() %>%
        dplyr::group_by(value) %>%
        dplyr::summarise(count = n()) %>%
        dplyr::mutate(order = as.numeric(
            stringr::str_extract(value, '[0-9][0-9]?[0-9]?'))) %>%
        dplyr::arrange(order)


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

    # Expand pmid to length 'surname'
    pmid <- purrr::map2(.x = counter$pmid,
                        .y = counter$count,
                        .f = rep) %>%
        unlist()

    # Expand article_node to length 'surname'
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

    # Join 'short' dataframes (<=100 entries)
    biblio_short <- pmid2 %>%
        dplyr::left_join(title2,
                         by = 'article_node') %>%
        dplyr::left_join(journal2,
                         by = 'article_node') %>%
        dplyr::left_join(volume2,
                         by = 'article_node') %>%
        dplyr::left_join(issue2,
                         by = 'article_node') %>%
        dplyr::left_join(year2,
                         by = 'article_node') %>%
        dplyr::left_join(doi2,
                         by = 'article_node') %>%
        dplyr::left_join(abstract2,
                         by = 'article_node')

    # Join 'long' dataframes
    biblio_long <-


    # Join 'long' and 'short' dataframes
    biblio_temp <- biblio_long %>%
        dplyr::left_join(,
                         by = 'pmid')

    #-- Output -----------------------------------------------------------#

    biblio_temp

}
#' @title Extract author affiliation information from an NCBI PubMed xml record.
#
#' @description \code{parse_affiliation} is used primarily as a utility function within \code{\link{get_records}}. It takes the xml output from an \href{http://www.ncbi.nlm.nih.gov/pubmed}{NCBI PubMed} database query and returns a dataframe of author affiliations for each \href{http://www.ncbi.nlm.nih.gov/pubmed}{NCBI PubMed} entry returned by the query.
#'
#' @param record The xml output from an \href{http://www.ncbi.nlm.nih.gov/pubmed}{NCBI PubMed} database query. Typically the database query is specified in the \emph{terms} parameter of the \code{\link{get_records}} function, and the downloaded xml output returned by the query is passed to \code{parse_affiliation} for parsing. The parsed affiliation data are then incorporated into the dataframe output of \code{\link{get_records}}. However, any xml NCBI PubMed query may be passed to the function independently of \code{\link{get_records}}, including xml files downloaded from \href{http://www.ncbi.nlm.nih.gov/pubmed}{NCBI PubMed} and unparsed results returned by \code{\link[rentrez]{entrez_fetch}}.
#'
#' @importFrom magrittr %>%
#'
#' @return Returns a long-format dataframe with the following columns:
#' \describe{
#' \item{author_node}{Character string specifying the XPath for each entry.}
#' \item{pmid}{PubMed unique identification code for each entry \emph{(class: integer)}.}
#' \item{authors}{Character string specifying author name \emph{format: surname initials}.}
#' \item{affiliation}{Character string specifying the affiliation(s) associated with each author for each entry.}
#'}
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
#' ## Construct NCBI eFetch query
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
#' parse_affiliation(record = rentrez_record)
#'
#' @export
parse_affiliation <- function(record) {

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

    affil_temp <- dplyr::data_frame(surname = auth_1,
                                    initials = auth_2,
                                    affiliation = affil,
                                    pmid = pmid) %>%
        # Merge surname and initials for joins with output from parse_record
        tidyr::unite(authors, surname, initials, sep = ' ') %>%
        dplyr::select(pmid, authors, affiliation)

    #-- Output -----------------------------------------------------------#

    affil_temp

}

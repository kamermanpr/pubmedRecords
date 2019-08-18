#' @title Get PubMed records.
#
#' @description \code{get_records} fetches \href{http://www.ncbi.nlm.nih.gov/pubmed}{PubMed} records and stores the records in a tidy data format suitable for processing using tools from the \emph{tidyverse}.
#'
#' @param search_terms A character string of terms that define the scope of the PubMed database query. Boolean operators \emph{(AND, OR, NOT)} and search field tags may be used to create more complex search criteria. Commonly used search fields tags include:
#' \describe{
#' \item{TI}{Word in title}
#' \item{TIAB}{Word in title or abstract}
#' \item{MH}{Medical Subject Heading (MeSH)}
#' \item{AU}{Author name (e.g., Doe J)}
#' \item{AD}{Author institutional affiliation}
#' \item{TA}{Journal title (e.g., J Pain)}
#' }
#'
#' For a full set of search fields tags: \href{https://www.ncbi.nlm.nih.gov/books/NBK3827/#_pubmedhelp_Search_Field_Descriptions_and_}{PubMed search field tags}. Note that the article publication type, date type, and date range are modified using the \code{pub_type}, \code{date_type}, \code{min_date} and \code{max_date} arguments below.
#'
#' @param has_abstract Logical specifying whether the returned records should be limited to those records that have an abstract. The default value is \emph{TRUE}.
#'
#' @param pub_type A character string specifying the type of publication the search must return. The default value is \emph{journal article}. For more information: \href{https://www.ncbi.nlm.nih.gov/books/NBK3827/#_pubmedhelp_Search_Field_Descriptions_and_}{PubMed search field tags}.
#'
#' @param api_key An API character string obtained from the users PubMed account.
#'
#' @param date_type A character string specifying the publication date type that is being specified in the search. Available values are:
#' \describe{
#' \item{PDAT}{Date the article was published \emph{(default)}.}
#' \item{MDAT}{Date the PubMed entry was modified.}
#' \item{EDAT}{Date the entry was added to PubMed.}}
#'
#' @param min_date A character string in the format \emph{'YYYY/MM/DD', 'YYYY/MM' or 'YYYY'} specifying the starting date of the search. The default value is \emph{1966/01/01'}.
#'
#' @param max_date A character string in the format \emph{'YYYY/MM/DD', 'YYYY/MM' or 'YYYY'} specifying the end date of the search. The default value is \code{Sys.Date()}.
#'
#' @importFrom magrittr %>%
#'
#' @return A long-format dataframe with the following columns:
#' \describe{
#' \item{author_node}{Character string specifying the XPath for each entry.}
#' \item{pmid}{PubMed unique identification code for each entry \emph{(class: integer)}.}
#' \item{authors}{Character string specifying author name \emph{format: surname initials}.}
#' \item{affiliation}{Character string specifying the affiliation(s) associated with each author for each entry.}}
#'
#' @family related functions
#'
#' @seealso \code{\link[rentrez]{entrez_search}} and \code{\link[rentrez]{entrez_fetch}}
#'
#' @export
get_records <- function(search_terms = '',
                        has_abstract = TRUE,
                        pub_type = 'journal article',
                        api_key = '',
                        date_type = 'PDAT',
                        min_date = '1966/01/01',
                        max_date = format(Sys.Date(), '%Y/%m/%d')) {

    ############################################################
    #                                                          #
    #                       Query PubMed                       #
    #                                                          #
    ############################################################

    #-- Determine how many articles are returned by the search terms -----#

    # Add has_abstract logical
    if(has_abstract == TRUE) {
        search_terms <- paste(search_terms, 'hasabstract')
    }

    # Search string
    search_string <- paste0('https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pubmed&term=',
                            pub_type, '[PT] AND ',
                            search_terms, '&datetype=',
                            date_type, '&mindate=',
                            min_date, '&maxdate=',
                            max_date, '&api_key=',
                            api_key, '&retmode=xml&rettype=Count')

    # Remove spaces from search terms
    search_string <- stringr::str_replace_all(search_string,
                                             pattern = ' ',
                                             replacement = '+')

    # Get record count
    record_count <- xml2::read_xml(search_string) %>%
        xml2::xml_find_all(.,
                           xpath = './/Count') %>%
        xml2::xml_text(.) %>%
        as.numeric()

    # Throw a warning if number of records is > 200
    if(record_count > 200) {
        print(paste(record_count, 'records will be retrieved. Downloading so many full records will take a long time, so go play some foosball.'))
    }

    #-- Get PMIDs --------------------------------------------------------#

    # Set the fetch string using 'record_count' to set the 'retmax'
    fetch_string <- paste0('https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pubmed&term=',
                           pub_type, '[PT] AND ',
                           search_terms, '&datetype=',
                           date_type, '&mindate=',
                           min_date, '&maxdate=',
                           max_date, '&api_key=',
                           api_key, '&rettype=xml&retmax=',
                           record_count)

    # Remove spaces from fetch terms
    fetch_string <- stringr::str_replace_all(fetch_string,
                                              pattern = ' ',
                                              replacement = '+')

    record_ID <- xml2::read_xml(fetch_string) %>%
        xml2::xml_find_all(.,
                             xpath = './/Id') %>%
        xml2::xml_integer(.)

    #-- Split the PMIDs into manageble chunks ----------------------------#

    # Split the 'record_ID' vector into n = 100 sized chunks
    splitter <- seq(from = 1,
                    to = record_count,
                    by = 100)

    # Create an empty list of length 'splitter'
    splitter_list <- vector(mode = 'list',
                            length = length(splitter))

    # Split the list of PMIDs, and paste each into a single string
    for(i in seq_along(splitter)) {
        splitter_list[[i]] <- record_ID[splitter[[i]]:(splitter[[i]] + 99)]
        splitter_list[[i]] <- paste(splitter_list[[i]],
                                    collapse = ',')
    }

    # Remove NA
    splitter_list <- purrr::map(.x = splitter_list,
                    ~stringr::str_replace_all(.x,
                                              pattern = ',NA',
                                              replacement = ''))

    #-- Set api pubmed query strings -------------------------------------#

    # Create empty list of length 'record_ID'
    pubmed_query <- vector(mode = 'list',
                           length = length(splitter_list))

    # Populate empty list with pubmed query calls
    for(i in seq_along(splitter_list)) {
        pubmed_query[[i]] <- paste0('https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=pubmed&id=',
                                    splitter_list[[i]],
                                    '&api_key=', api_key,
                                    '&retmode=xml')
    }

    #-- Download pubmed xml record ---------------------------------------#

    xml_record <- purrr::map(pubmed_query,
                             xml2::read_xml)

    names(xml_record) <- rep('xml_record',
                             length(pubmed_query))

    #-- Close connections ------------------------------------------------#

    closeAllConnections()

    ############################################################
    #                                                          #
    #                Parse bibliographic data                  #
    #                                                          #
    ############################################################


    record_out <- purrr::map(xml_record,
                             pubmedRecords::parse_bibliographics)

    record_out <- bind_rows(record_out)

    #-- Output ----------------------------------------------------------------#

    return(record_out)

}


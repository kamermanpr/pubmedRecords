#' @title Retrieve the number of records returned by a query.
#
#' @description \code{count_records} returns a count of \href{http://www.ncbi.nlm.nih.gov/pubmed}{PubMed} records returned by a search.
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
#
#' @param pub_type A character string specifying the type of publication the search must return. The default value is \emph{journal article}. For more information: \href{https://www.ncbi.nlm.nih.gov/books/NBK3827/table/pubmedhelp.T.publication_types/?report=objectonly}{PubMed article types}.
#'
#' @param api_key An API character string obtained from the users NCBI account. The key is not essential, but it specifying a key gives substantially faster record query rates.
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
#' @seealso \code{\link[rentrez]{entrez_search}}
#'
#' @export
count_records <- function(search_terms,
                          pub_type = 'journal article',
                          api_key = NULL,
                          date_type = 'PDAT',
                          min_date = '1966/01/01',
                          max_date = format(Sys.Date(), '%Y/%m/%d')) {
    
    ############################################################
    #                                                          #
    #                       Query PubMed                       #
    #                                                          #
    ############################################################
    
    #-- Determine how many articles are returned by the search terms -----#
    
    if(!is.null(api_key)){
        # Search string
        search_string <- paste0('https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pubmed&term=',
                                pub_type, '[PT] AND ',
                                search_terms, '&datetype=',
                                date_type, '&mindate=',
                                min_date, '&maxdate=',
                                max_date, '&api_key=',
                                api_key, '&retmode=xml&rettype=Count')
    } else {
        search_string <- paste0('https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pubmed&term=',
                                pub_type, '[PT] AND ',
                                search_terms, '&datetype=',
                                date_type, '&mindate=',
                                min_date, '&maxdate=',
                                max_date, '&retmode=xml&rettype=Count')
    }
    
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
    if(record_count < 1000) {
        cat(paste(record_count, 'records will be retrieved.'))
    } else {
        cat(paste(record_count, 'records will be retrieved. This will take a long time to download, and I recommend that you refine your search'))
    }
}
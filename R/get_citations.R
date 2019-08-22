#' @title Fetch CrossRef bibliometrics for PubMed records.
#
#' @description \code{get_citations} adds CrossRef citation counts to the records included in the dataframe returned by \code{\link{get_records}}.
#' @param df The dataframe object returned by \code{\link{get_records}}.
#'
#' @return Returns a dataframe with all the columns of the input dataframe, plus:
#' \describe{
#' \item{crossref_citations}{Numeric value specifying the CrossRef citation count for an article.}}
#'
#' @seealso \code{\link{get_records}} for generating input dataframe.
#'
#' @export
get_citations <- function(df) {

    ############################################################
    #                                                          #
    #                  check for pmid column                   #
    #                                                          #
    ############################################################

    if('pmid' %in% colnames(df)) {

        #-- In case pmid column is imported as class factor or character ------#
        df <- df %>%
            dplyr::mutate(pmid = as.numeric(as.character(pmid)),
                          doi = as.character(doi))

        #-- Get variables required for retrieving bibliometrics ---------------#

        df2 <- df %>%
            # Select required columns only
            dplyr::select(pmid, doi) %>%
            # Remove duplicates
            unique(.) %>%
            # Insert dummy doi if NA to avoid error in cr_citation_count
            dplyr::mutate(doi = dplyr::case_when(
                is.na(.$doi) ~ paste0('10.1010/101010'),
                !is.na(.$doi) ~paste0(.$doi)))

        #-- Pass to CrossRef --------------------------------------------------#

           suppressMessages(data_crossref <- df2$doi %>%
                purrr::map(rcrossref::cr_citation_count) %>%
                dplyr::bind_rows() %>%
                dplyr::left_join(df2) %>%
                dplyr::rename(crossref_citations = count) %>%
                dplyr::select(pmid, crossref_citations))

        #-- Join data_crossref with input dataframe -------#

        df <- df %>%
            dplyr::left_join(data_crossref)

        df <- dplyr::as_tibble(df)

        #-- output ------------------------------------------------------------#

        return(df)

    } else {

        return(stop("The function requires a dataframe returned by the 'pubmedRecords::get_records' function."))

    }
}

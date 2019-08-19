#' @title Fetch CrossRef bibliometrics for PubMed records.
#
#' @description \code{citation_metrics} adds CrossRef citation counts to the records included in the dataframe returned by \code{\link{get_records}}.
#' @param df The dataframe object returned by \code{\link{get_records}}. Alternatively, any dataframe that includes a column named \emph{pmid}, which contains the PMIDs of each PubMed record bibliometrics are required for.
#'
#' @return Returns a dataframe with all the columns of the input dataframe, plus:
#' \describe{
#' \item{crossref_citations}{CrossRef citation count \emph{(class: integer)}.}}
#'
#' @seealso \code{\link{get_records}} for generating input dataframe.
#'
#' @export
citation_metrics <- function(df) {

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

            data_crossref <- df2$doi %>%
                purrr::map(rcrossref::cr_citation_count) %>%
                dplyr::bind_rows() %>%
                dplyr::left_join(df2) %>%
                dplyr::rename(crossref_citations = count) %>%
                dplyr::select(pmid, crossref_citations)

        #-- Join data_crossref with input dataframe -------#

        df <- df %>%
            dplyr::left_join(data_crossref)

        #-- output ------------------------------------------------------------#

        return(df)

    } else {

        return(stop("The function requires a dataframe with a column labelled 'pmid', and which contains PubMed IDs"))

    }
}

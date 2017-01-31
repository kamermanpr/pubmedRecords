#' @title Fetch bibliometrics for PubMed records.
#
#' @description \code{citation_metrics} adds bibliometric data to the records included in the dataframe returned by \code{\link{get_records}}. Metrics include: i) CrossRef article citation count, \href{https://elsevier.altmetric.com}{Altmetric} score, Altmetric Twitter, Facebook, and blog post mentions, and Altmetric Mendeley reader counts. The function provides a wrapper around the \code{\link[rcrossref]{cr_citation_count}} from the package \pkg{rcrossref}, and the \code{\link[rAltmetric]{altmetrics}} and \code{\link[rAltmetric]{altmetric_data}} functions from the package \pkg{rAltmetric}, both of which are from the \href{https://ropensci.org/}{rOpenSci} stable of packages.
#'
#' @param df The dataframe object returned by \code{\link{get_records}}. Alternatively, any dataframe that includes a column named \emph{pmid}, which contains the PMIDs of each PubMed record bibliometrics are required for.
#'
#' @param crossref Logical indicating whether CrossRef citation counts should be fetched. Default value is \emph{TRUE}.
#'
#' @param altmetric Logical indicating whether Altmetric data should be fetched. Default value is \emph{TRUE}. Note that if there are no Altmetric data for a record, a warning is printed.
#'
#' @return Returns a dataframe with all the columns of the input dataframe, plus the following columns:
#' \describe{
#' \item{crossref_citations}{CrossRef citation count \emph{(class: integer)}.}
#' \item{altmetric_score}{Altmetric summary score \emph{(class: integer)}.}
#' \item{altmetric_tweets}{Number of mentions in tweets \emph{(class: integer)}.}
#' \item{altmetric_facebook}{Number of Facebook mentions \emph{(class: integer)}.}
#' \item{altmetric_posts}{Number of blog post mentions \emph{(class: integer)}.}
#' \item{altmetric_mendeley}{Number of times read on Mendeley \emph{(class: integer)}.}}
#'
#' @seealso \code{\link{get_records}} for generating input dataframe.
#'
#' @examples
#'
#' \donttest{}
#'
#' @export
citation_metrics <- function(df,
                             crossref = TRUE,
                             altmetric = TRUE) {

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
            # Create pmid formatted for altmetrics
            dplyr::mutate(pmid2 = paste0('pmid/', pmid)) %>%
            # Select required columns only
            dplyr::select(pmid, pmid2, doi) %>%
            # Remove duplicates
            unique(.) %>%
            # Insert dummy doi if NA to avoid error in cr_citation_count
            dplyr::mutate(doi = dplyr::case_when(
                is.na(.$doi) ~ paste0('10.1010/101010'),
                !is.na(.$doi) ~paste0(.$doi)))

        #-- Pass to altmetrics ------------------------------------------------#

        if(altmetric == TRUE) {
            data_altmetrics <- df2$pmid2 %>%
                purrr::map(.f = rAltmetric::altmetrics) %>%
                purrr::map(.f = rAltmetric::altmetric_data)
            # Remove NULL items in the list
            data_altmetrics <- data_altmetrics[sapply(data_altmetrics,
                                                      is.data.frame)]

            data_altmetrics <- data_altmetrics %>%
                purrr::map(.f = dplyr::select,
                           pmid,
                           score,
                           cited_by_tweeters_count,
                           cited_by_fbwalls_count,
                           cited_by_posts_count,
                           mendeley) %>%
                purrr::map(.f = dplyr::rename,
                           altmetric_score = score,
                           altmetric_tweets = cited_by_tweeters_count,
                           altmetric_facebook = cited_by_fbwalls_count,
                           altmetric_posts = cited_by_posts_count,
                           altmetric_mendeley = mendeley) %>%
                purrr::map(.f = dplyr::mutate,
                           pmid = as.numeric(as.character(pmid)),
                           altmetric_score = as.numeric(altmetric_score),
                           altmetric_tweets = as.numeric(altmetric_tweets),
                           altmetric_facebook = as.numeric(altmetric_facebook),
                           altmetric_posts = as.numeric(altmetric_posts),
                           altmetric_mendeley = as.numeric(altmetric_mendeley)) %>%
                purrr::map_df(.f = dplyr::tbl_df)
        }

        #-- Pass to CrossRef --------------------------------------------------#

        if(crossref == TRUE) {
            data_crossref <- df2$doi %>%
                purrr::map(rcrossref::cr_citation_count) %>%
                purrr::map_df(dplyr::as_data_frame) %>%
                dplyr::bind_cols(df2) %>%
                dplyr::rename(crossref_citations = value) %>%
                dplyr::select(pmid, crossref_citations)
        }

        #-- Join data_crossref and data_altmetrics with input dataframe -------#

        if(exists('data_crossref') & exists('data_altmetrics')) {
            df <- df %>%
                dplyr::left_join(data_crossref) %>%
                dplyr::left_join(data_altmetrics)

        } else if(exists('data_crossref') & !exists('data_altmetrics')) {
            df <- df %>%
                dplyr::left_join(data_crossref)

        } else {
            df <- df %>%
                dplyr::left_join(data_altmetrics)

        }

        #-- output ------------------------------------------------------------#

        return(df)

    } else {

        return(stop("The function requires a dataframe with a column labelled 'pmid', and which contains PubMed IDs"))

    }
}

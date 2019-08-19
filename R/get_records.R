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
#' @seealso \code{\link[rentrez]{entrez_search}} and \code{\link[rentrez]{entrez_fetch}}
#'
#' @export
get_records <- function(search_terms,
                        has_abstract = TRUE,
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

    # Add has_abstract logical
    if(has_abstract == TRUE) {
        search_terms <- paste(search_terms, 'hasabstract')
    }

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
    if(record_count > 200) {
        print(paste(record_count, 'records will be retrieved. Downloading so many full records will take a long time, so go play some foosball.'))
    }

    #-- Get PMIDs --------------------------------------------------------#

    # Set the fetch string using 'record_count' to set the 'retmax'
    if(!is.null(api_key)){
        fetch_string <- paste0('https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pubmed&term=',
                               pub_type, '[PT] AND ',
                               search_terms, '&datetype=',
                               date_type, '&mindate=',
                               min_date, '&maxdate=',
                               max_date, '&api_key=',
                               api_key, '&rettype=xml&retmax=',
                               record_count)
    } else {
        fetch_string <- paste0('https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pubmed&term=',
                               pub_type, '[PT] AND ',
                               search_terms, '&datetype=',
                               date_type, '&mindate=',
                               min_date, '&maxdate=',
                               max_date, '&rettype=xml&retmax=',
                               record_count)
    }

    # Remove spaces from fetch terms
    fetch_string <- stringr::str_replace_all(fetch_string,
                                              pattern = ' ',
                                              replacement = '+')

    record_ID <- xml2::read_xml(fetch_string) %>%
        xml2::xml_find_all(.,
                             xpath = './/Id') %>%
        xml2::xml_integer(.)

    # Collapse into a single string
    record_pmid <- paste(record_ID,
                         collapse = ',')

    # Generate search string
    if(!is.null(api_key)){
        retrieve_xml <- paste0('https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=pubmed&id=',
                               record_pmid,
                               '&api_key=', api_key,
                               '&retmode=xml')
    } else {
        retrieve_xml <- paste0('https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=pubmed&id=',
                               record_pmid, '&retmode=xml')
    }


    #-- Download pubmed xml record ---------------------------------------#

    suppressWarnings(
    record <- xml2::read_xml(retrieve_xml)
    )

    #-- Close connections ------------------------------------------------#

    # Messes up knitting
    # closeAllConnections()

    ############################################################
    #                                                          #
    #                Parse bibliographic data                  #
    #                                                          #
    ############################################################

    ############################################################
    #                                                          #
    #                        Set xpaths                        #
    #                                                          #
    ###########################################################

    #-- Surname ----------------------------------------------------------#

    surname_path <- xml2::xml_path(
        xml2::xml_find_all(record,
                           './/LastName'))

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

    #-- Publication status -----------------------------------------------#

    status_path <- xml2::xml_path(
        xml2::xml_find_all(record,
                           './/PublicationStatus'))

    #-- Volume -----------------------------------------------------------#

    volume_path <- xml2::xml_path(
        xml2::xml_find_all(record,
                           './/Volume'))

    #-- Year published ---------------------------------------------------#

    year_published_path <- xml2::xml_path(
        xml2::xml_find_all(record,
                           './/PubDate/Year|.//PubDate/MedlineDate'))

    #-- Year online ------------------------------------------------------#

    year_online_path <- xml2::xml_path(
        xml2::xml_find_all(record,
                           ".//PubMedPubDate[@PubStatus = 'entrez']/Year"))

    #-- Pages ------------------------------------------------------------#

    pages_path <- xml2::xml_path(
        xml2::xml_find_all(record,
                           './/MedlinePgn'))

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

    # Define vector for author surnames
    surname <- vector(mode = 'character',
                      length = length(surname_path))

    suppressWarnings(
    for(i in 1:length(surname_path)) {
        surname[[i]] <- stringr::str_to_lower(
            xml2::xml_text(
                xml2::xml_find_first(record,
                                     surname_path[[i]])))
    })

    #-- Initials ---------------------------------------------------------#

    # Define vector for author initials
    initials <- vector(mode = 'character',
                       length = length(initials_path))

    suppressWarnings(
    for(i in 1:length(initials_path)) {
        initials[[i]] <- stringr::str_to_lower(
            xml2::xml_text(
                xml2::xml_find_first(record,
                                     initials_path[[i]])))
    })

    # For joins with output from parse_affiliation
    authors <- paste(surname, initials)

    #-- Title ------------------------------------------------------------#

    # Define vector for article title
    title <- vector(mode = 'character',
                    length = length(title_path))

    suppressWarnings(
    for(i in 1:length(title_path)) {
        title[[i]] <- stringr::str_to_lower(
            xml2::xml_text(
                xml2::xml_find_first(record,
                                     title_path[[i]])))
    })

    # Make article marker for joins
    ## Define vector of 'trimmed' title path
    title_path2 <- vector(mode = 'character',
                          length = length(title_path))

    for(i in 1:length(title_path)) {
        title_path2[[i]] <- stringr::str_extract(title_path[[i]],
                                                 '/PubmedArticleSet/PubmedArticle\\[[0-9][0-9]?[0-9]?\\]')
    }

    # Make dataframe
    title2 <- data.frame(article_node = title_path2,
                         title = title) %>%
        dplyr::mutate_all(as.character)

    #-- Journal ----------------------------------------------------------#

    # Define vector for journal name
    journal <- vector(mode = 'character',
                      length = length(journal_path))

    suppressWarnings(
    for(i in 1:length(journal_path)) {
        journal[[i]] <- stringr::str_to_lower(
            xml2::xml_text(
                xml2::xml_find_first(record,
                                     journal_path[[i]])))
    })

    # Make article marker for joins
    ## Define vector for 'trimmed' journal path
    journal_path2 <- vector(mode = 'character',
                            length = length(journal_path))

    for(i in 1:length(journal_path)) {
        journal_path2[[i]] <- stringr::str_extract(journal_path[[i]],
                                                   '/PubmedArticleSet/PubmedArticle\\[[0-9][0-9]?[0-9]?\\]')
    }

    # Make dataframe
    journal2 <- data.frame(article_node = journal_path2,
                           journal = journal) %>%
        dplyr::mutate(journal = stringr::str_replace_all(journal,
                                                         pattern = '[.]',
                                                         replacement = '')) %>%
        dplyr::mutate_all(as.character)

    #-- Publication status -------------------------------------------#

    # Define vector for publication status
    status <- vector(mode = 'character',
                     length = length(status_path))

    suppressWarnings(
    for(i in 1:length(status_path)) {
        status[[i]] <- xml2::xml_text(
            xml2::xml_find_first(record,
                                 status_path[[i]]))
    })

    # Make article marker for joins
    ## Define vector for 'trimmed' year path
    status_path2 <- vector(mode = 'character',
                           length = length(status_path))

    for(i in 1:length(status_path)) {
        status_path2[[i]] <- stringr::str_extract(status_path[[i]],
                                                  '/PubmedArticleSet/PubmedArticle\\[[0-9][0-9]?[0-9]?\\]')
    }

    # Make dataframe
    status2 <- data.frame(article_node = status_path2,
                          publication_status = status) %>%
        # Edit text
        dplyr::mutate(publication_status = ifelse(is.na(publication_status),
                                                  yes = NA,
                                                  no = ifelse(
                                                      publication_status
                                                      == 'ppublish',
                                                      yes = 'print',
                                                      no = 'ahead of print'))) %>%
        dplyr::mutate_all(as.character)

    #-- Volume ---------------------------------------------------------#

    # Define vector for journal volume
    volume <- vector(mode = 'numeric',
                     length = length(volume_path))

    suppressWarnings(
    for(i in 1:length(volume_path)) {
        volume[[i]] <- xml2::xml_integer(
            xml2::xml_find_first(record,
                                 volume_path[[i]]))
    })

    # Make article marker for joins
    ## Define vector for 'trimmed' volume path
    volume_path2 <- vector(mode = 'character',
                           length = length(volume_path))

    for(i in 1:length(volume_path)) {
        volume_path2[[i]] <- stringr::str_extract(volume_path[[i]],
                                                  '/PubmedArticleSet/PubmedArticle\\[[0-9][0-9]?[0-9]?\\]')
    }

    # Make dataframe
    volume2 <- data.frame(article_node = volume_path2,
                          volume = volume) %>%
        dplyr::mutate_all(as.character)

    #-- Year published -------------------------------------------------#

    if(length(year_published_path) > 0) {
        # Define vector for publication year
        year_published <- vector(mode = 'character',
                                 length = length(year_published_path))

        suppressWarnings(
        for(i in 1:length(year_published_path)) {
            year_published[[i]] <- xml2::xml_text(
                xml2::xml_find_first(record,
                                     year_published_path[[i]]))
        })

        # Make article marker for joins
        ## Define vector for 'trimmed' year path
        year_published_path2 <- vector(mode = 'character',
                                       length = length(year_published_path))

        for(i in 1:length(year_published_path)) {
            year_published_path2[[i]] <- stringr::str_extract(year_published_path[[i]],
                                                              '/PubmedArticleSet/PubmedArticle\\[[0-9][0-9]?[0-9]?\\]')
        }

        # Make dataframe
        year_published2 <- data.frame(article_node = year_published_path2,
                                      year_published = year_published) %>%
            # Extract year
            dplyr::mutate(year_published = stringr::str_extract(year_published,
                                                                pattern = '[0-9][0-9][0-9][0-9]')) %>%
            dplyr::mutate_all(as.character)
    } else {
        # Make empty dataframe
        year_published2 <- data.frame(article_node = as.character(),
                                      year_published = as.numeric()) %>%
            dplyr::mutate_all(as.character)
    }

    #-- Year online ---------------------------------------------------#

    if(length(year_online_path) > 0) {
        # Define vector for online publication year
        year_online <- vector(mode = 'character',
                              length = length(year_online_path))

        suppressWarnings(
        for(i in 1:length(year_online_path)) {
            year_online[[i]] <- xml2::xml_text(
                xml2::xml_find_first(record,
                                     year_online_path[[i]]))
        })

        # Make article marker for joins
        ## Define vector for 'trimmed' year path
        year_online_path2 <- vector(mode = 'character',
                                    length = length(year_online_path))

        for(i in 1:length(year_online_path)) {
            year_online_path2[[i]] <- stringr::str_extract(year_online_path[[i]],
                                                           '/PubmedArticleSet/PubmedArticle\\[[0-9][0-9]?[0-9]?\\]')
        }

        # Make dataframe
        year_online2 <- data.frame(article_node = year_online_path2,
                                   year_online = year_online) %>%
            dplyr::mutate_all(as.character)
    } else {
        # Make empty dataframe
        year_online2 <- data.frame(article_node = as.character(),
                                   year_online = as.numeric()) %>%
            dplyr::mutate_all(as.character)
    }

    #-- Pages ---------------------------------------------------------#

    if(length(pages_path) > 0) {
        # Define vector for aricle page numbers
        pages <- vector(mode = 'character',
                        length = length(pages_path))

        suppressWarnings(
        for(i in 1:length(pages_path)) {
            pages[[i]] <- stringr::str_to_lower(
                xml2::xml_text(
                    xml2::xml_find_first(record,
                                         pages_path[[i]])))
        })

        # Make article marker for joins
        ## Define vector for 'trimmed' page-number path
        pages_path2 <- vector(mode = 'character',
                              length = length(pages_path))

        for(i in 1:length(pages_path)) {
            pages_path2[[i]] <- stringr::str_extract(pages_path[[i]],
                                                     '/PubmedArticleSet/PubmedArticle\\[[0-9][0-9]?[0-9]?\\]')
        }

        # Make dataframe
        pages2 <- data.frame(article_node = pages_path2,
                             pages = pages) %>%
            dplyr::mutate_all(as.character)
    } else {
        # Make empty dataframe
        pages2 <- data.frame(article_node = as.character(),
                             pages = as.character()) %>%
            dplyr::mutate_all(as.character)
    }

    #-- PMID --------------------------------------------------------#

    pmid <- data.frame(pmid = record_ID)

    #-- DOI ---------------------------------------------------------#

    if(length(doi_path) > 0) {
        # Define vector for doi
        doi <- vector(mode = 'character',
                      length = length(doi_path))

        suppressWarnings(
        for(i in 1:length(doi_path)) {
            doi[[i]] <- xml2::xml_text(
                xml2::xml_find_first(record,
                                     doi_path[[i]]))
        })

        # Make article marker for joins
        ## Define vector for 'trimmed' doi path
        doi_path2 <- vector(mode = 'character',
                            length = length(doi_path))

        for(i in 1:length(doi_path)) {
            doi_path2[[i]] <- stringr::str_extract(doi_path[[i]],
                                                   '/PubmedArticleSet/PubmedArticle\\[[0-9][0-9]?[0-9]?\\]')
        }

        # Make dataframe
        doi2 <- data.frame(article_node = doi_path2,
                           doi = doi) %>%
            dplyr::mutate_all(as.character)
    } else {
        # Make empty dataframe
        doi2 <- data.frame(article_node = as.character(),
                           doi = as.character()) %>%
            dplyr::mutate_all(as.character)
    }

    #-- Abstract ---------------------------------------------------------#

    # Define vector for abstracts
    abstract <- vector(mode = 'character',
                       length = length(abstract_path))

    suppressWarnings(
    for(i in 1:length(abstract_path)) {
        abstract[[i]] <- stringr::str_to_lower(
            xml2::xml_text(
                xml2::xml_find_first(record,
                                     abstract_path[[i]])))
    })

    # Make article marker for joins
    ## Define vector for 'trimmed' abstract path
    abstract_path2 <- vector(mode = 'character',
                             length = length(abstract_path))

    for(i in 1:length(abstract_path)) {
        abstract_path2[[i]] <- stringr::str_extract(abstract_path[[i]],
                                                    '/PubmedArticleSet/PubmedArticle\\[[0-9][0-9]?[0-9]?\\]')
    }

    # Make dataframe
    abstract2 <- data.frame(article_node = abstract_path2,
                            abstract = abstract) %>%
        dplyr::mutate_all(as.character)

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

    # Expand article_node to length 'surname'
    node <- purrr::map2(.x = counter$value,
                        .y = counter$count,
                        .f = rep) %>%
        unlist()

    pmid_expanded <- purrr::map2(.x = pmid$pmid,
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
    biblio_short <- title2 %>%
        dplyr::left_join(journal2,
                         by = 'article_node') %>%
        dplyr::left_join(status2,
                         by = 'article_node') %>%
        dplyr::left_join(volume2,
                         by = 'article_node') %>%
        dplyr::left_join(pages2,
                         by = 'article_node') %>%
        dplyr::left_join(year_published2,
                         by = 'article_node') %>%
        dplyr::left_join(year_online2,
                         by = 'article_node') %>%
        dplyr::left_join(doi2,
                         by = 'article_node') %>%
        dplyr::left_join(abstract2,
                         by = 'article_node') %>%
        # If publication status is 'ahead of print' make 'year published' NA
        dplyr::mutate(year_published = ifelse(publication_status == 'ahead of print',
                                              yes = NA,
                                              no = year_published))

    # Join 'long' dataframes
    biblio_long <- data.frame(article_node = node,
                              authors = authors,
                              pmid = pmid_expanded) %>%
        dplyr::mutate_all(as.character)

    # Join 'long' and 'short' dataframes

    empty_as_na <- function(x){
        ifelse(x == '', yes = NA, no = x)
    }

    biblio_out <- biblio_long %>%
        dplyr::left_join(biblio_short) %>%
        dplyr::select(authors,
                      title,
                      journal,
                      publication_status,
                      volume,
                      pages,
                      year_published,
                      year_online,
                      pmid,
                      doi,
                      abstract) %>%
        dplyr::mutate(authors = as.character(authors),
                      title = as.character(title),
                      journal = as.character(journal),
                      publication_status = as.character(publication_status),
                      volume = as.character(volume),
                      pages = as.character(pages),
                      year_published = as.character(year_published),
                      year_online = as.character(year_online),
                      pmid = as.character(pmid),
                      doi = as.character(doi),
                      abstract = as.character(abstract)) %>%
        dplyr::mutate_all(.funs = trimws) %>%
        dplyr::mutate_all(.funs = empty_as_na) %>%
        dplyr::mutate(year_published = as.numeric(year_published),
                      year_online = as.numeric(year_online))

    biblio_out <- dplyr::as_tibble(biblio_out)
    #-- Output ----------------------------------------------------------------#

    return(biblio_out)

}


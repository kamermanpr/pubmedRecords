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
    
    # Split record_ID into a series of lists of about 100 items long each
    splits <- ceiling(record_count/100)
    
    record_split <- split(record_ID, 
                          rep_len(1:splits, length(record_ID)))
    
    # Collapse into a single string
    record_pmid <- purrr::map(record_split,
                              ~ paste(.x, collapse = ','))
    
    #-- Download pubmed xml record ---------------------------------------#
    record <- purrr::map(record_pmid, 
                         ~ rentrez::entrez_fetch(db = 'pubmed', 
                                                 id = .x, 
                                                 rettype = 'xml', 
                                                 retmode = 'xml'))
    
    record <- purrr::map(record, 
                        ~ xml2::read_xml(.x))

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

    surname_path <- purrr::map(record, 
                               ~ xml2::xml_path(xml2::xml_find_all(.x, './/LastName')))

    #-- Initials ---------------------------------------------------------#

    initials_path <- purrr::map(record,
                                ~ xml2::xml_path(xml2::xml_find_all(.x, './/Initials')))

    #-- Title ------------------------------------------------------------#

    title_path <- purrr::map(record,
                             ~ xml2::xml_path(xml2::xml_find_all(.x, './/ArticleTitle')))

    #-- Journal ----------------------------------------------------------#

    journal_path <- purrr::map(record,
                               ~ xml2::xml_path( xml2::xml_find_all(.x, './/ISOAbbreviation')))

    #-- Publication status -----------------------------------------------#

    status_path <- purrr::map(record, 
                              ~ xml2::xml_path(xml2::xml_find_all(.x, './/PublicationStatus')))

    #-- Volume -----------------------------------------------------------#

    volume_path <- purrr::map(record, 
                              ~ xml2::xml_path(xml2::xml_find_all(.x, './/Volume')))

    #-- Year published ---------------------------------------------------#

    year_published_path <- purrr::map(record,
                                      ~ xml2::xml_path(xml2::xml_find_all(.x, './/PubDate/Year|.//PubDate/MedlineDate')))

    #-- Year online ------------------------------------------------------#

    year_online_path <- purrr::map(record, 
                                   ~ xml2::xml_path(xml2::xml_find_all(.x, ".//PubMedPubDate[@PubStatus = 'entrez']/Year")))

    #-- Pages ------------------------------------------------------------#

    pages_path <- purrr::map(record, 
                             ~ xml2::xml_path(xml2::xml_find_all(.x,'.//MedlinePgn')))
    
    #-- PMID --------------------------------------------------------------#
    
    pmid_path <- purrr::map(record,
                           ~ xml2::xml_path(xml2::xml_find_all(.x, ".//ArticleId[@IdType = 'pubmed']")))
    
    #-- DOI --------------------------------------------------------------#

    doi_path <- purrr::map(record,
                           ~ xml2::xml_path(xml2::xml_find_all(.x, ".//ArticleId[@IdType = 'doi']")))

    #-- Abstract ---------------------------------------------------------#

    abstract_path <- purrr::map(record,
                                ~ xml2::xml_path(xml2::xml_find_all(.x, './/Abstract')))

    ############################################################
    #                                                          #
    #                   Extract information                    #
    #                                                          #
    ############################################################

    #-- Surname ----------------------------------------------------------#

    # Define vector for author surnames
    surname <- purrr::map(surname_path,
                          ~ vector(mode = 'character'))

    suppressWarnings(
        for(j in 1:length(surname)) {
            for(i in 1:length(surname_path[[j]])) {
                surname[[j]][[i]] <- stringr::str_to_lower(
                    xml2::xml_text(
                        xml2::xml_find_first(record[[j]],
                                             surname_path[[j]][[i]])))
                }})
    
    surname <- purrr::map2(.x = surname, 
                           .y = surname_path,
                           ~ data.frame(surname = .x,
                                        article_node = stringr::str_extract(.y,
                                                                            '/PubmedArticleSet/PubmedArticle\\[[0-9][0-9]?[0-9]?\\]')) %>% 
                               dplyr::mutate(article_node = as.character(article_node),
                                             surname = as.character(surname)) %>% 
                               dplyr::mutate(counter = dplyr::row_number()))

    #-- Initials ---------------------------------------------------------#

    # Define vector for author initials
    initials <- purrr::map(initials_path,
                           ~ vector(mode = 'character'))

    suppressWarnings(
        for(j in 1:length(initials)){
            for(i in 1:length(initials_path[[j]])) {
                initials[[j]][[i]] <- stringr::str_to_lower(
                    xml2::xml_text(
                        xml2::xml_find_first(record[[j]],
                                             initials_path[[j]][[i]])))
                }})
    
    initials <- purrr::map2(.x = initials, 
                            .y = initials_path, 
                            ~ data.frame(initials = .x, 
                                         article_node = stringr::str_extract(.y,
                                                                             '/PubmedArticleSet/PubmedArticle\\[[0-9][0-9]?[0-9]?\\]')) %>% 
                                dplyr::mutate(article_node = as.character(article_node),
                                              initials = as.character(initials)) %>% 
                                dplyr::mutate(counter = dplyr::row_number()))

    #-- Title ------------------------------------------------------------#

    # Define vector for article title
    title <- purrr::map(title_path,
                        ~ vector(mode = 'character'))

    suppressWarnings(
        for(j in 1:length(title_path)) {
            for(i in 1:length(title_path[[j]])) {
                title[[j]][[i]] <- stringr::str_to_lower(
                    xml2::xml_text(
                        xml2::xml_find_first(record[[j]],
                                             title_path[[j]][[i]])))
    }})
    
    title <- purrr::map2(.x = title, 
                         .y = title_path, 
                         ~ data.frame(title = .x,
                                      article_node = stringr::str_extract(.y,
                                                                          '/PubmedArticleSet/PubmedArticle\\[[0-9][0-9]?[0-9]?\\]')) %>% 
                             dplyr::mutate(article_node = as.character(article_node),
                                           title = as.character(title)))

    #-- Journal ----------------------------------------------------------#

    # Define vector for journal name
    journal <- purrr::map(journal_path,
                          ~ vector(mode = 'character'))
    
    suppressWarnings(
        for(j in 1:length(journal_path)) {
            for(i in 1:length(journal_path[[j]])) {
                journal[[j]][[i]] <- stringr::str_to_lower(
                    xml2::xml_text(
                        xml2::xml_find_first(record[[j]],
                                             journal_path[[j]][[i]])))
            }})
    
    journal <- purrr::map2(.x = journal, 
                           .y = journal_path, 
                           ~ data.frame(journal = .x,
                                        article_node = stringr::str_extract(.y,
                                                                            '/PubmedArticleSet/PubmedArticle\\[[0-9][0-9]?[0-9]?\\]')) %>% 
                               dplyr::mutate(article_node = as.character(article_node),
                                             journal = as.character(journal)))

    #-- Publication status -------------------------------------------#

    # Define vector for publication status
    status <- purrr::map(status_path, 
                         vector(mode = 'character'))
    
    suppressWarnings(
        for(j in 1:length(status_path)) {
            for(i in 1:length(status_path[[j]])) {
                status[[j]][[i]] <- stringr::str_to_lower(
                    xml2::xml_text(
                        xml2::xml_find_first(record[[j]],
                                             status_path[[j]][[i]])))
            }})
    
    status <- purrr::map2(.x = status, 
                          .y = status_path, 
                          ~ data.frame(status = .x,
                                       article_node = stringr::str_extract(.y,
                                                                           '/PubmedArticleSet/PubmedArticle\\[[0-9][0-9]?[0-9]?\\]')) %>% 
                              dplyr::mutate(article_node = as.character(article_node),
                                            status = as.character(status)))

    #-- Volume ---------------------------------------------------------#

    # Define vector for journal volume
    volume <- purrr::map(volume_path,
                        vector(mode = 'character'))

    suppressWarnings(
        for(j in 1:length(volume_path)) {
            for(i in 1:length(volume_path[[j]])) {
                volume[[j]][[i]] <- stringr::str_to_lower(
                    xml2::xml_text(
                        xml2::xml_find_first(record[[j]],
                                             volume_path[[j]][[i]])))
            }})
    
    volume <- purrr::map2(.x = volume, 
                          .y = volume_path, 
                          ~ data.frame(volume = .x,
                                       article_node = stringr::str_extract(.y,
                                                                           '/PubmedArticleSet/PubmedArticle\\[[0-9][0-9]?[0-9]?\\]')) %>% 
                              dplyr::mutate(article_node = as.character(article_node),
                                            volume = as.character(volume)))

    #-- Year published -------------------------------------------------#

    if(length(year_published_path) > 0) {
        # Define vector for publication year
        year_published <- purrr::map(year_published_path,
                                     vector(mode = 'character'))
        
        suppressWarnings(
            for(j in 1:length(year_published_path)) {
                for(i in 1:length(year_published_path[[j]])) {
                    year_published[[j]][[i]] <- stringr::str_to_lower(
                        xml2::xml_text(
                            xml2::xml_find_first(record[[j]],
                                                 year_published_path[[j]][[i]])))
                }})
        
        year_published <- purrr::map2(.x = year_published, 
                                      .y = year_published_path, 
                                      ~ data.frame(year_published = .x,
                                                   article_node = stringr::str_extract(.y,
                                                                                       '/PubmedArticleSet/PubmedArticle\\[[0-9][0-9]?[0-9]?\\]')) %>% 
                                          dplyr::mutate(article_node = as.character(article_node),
                                                        year_published = as.character(year_published)))
    } else {
        # Make empty dataframe
        year_published <- purrr::map(1:length(title), 
                                    ~ data.frame(article_node = as.character(),
                                                 year_published = as.character()))
    }

    #-- Year online ---------------------------------------------------#

    if(length(year_online_path) > 0) {
        # Define vector for year online
        year_online <- purrr::map(year_online_path,
                                  vector(mode = 'character'))
        
        suppressWarnings(
            for(j in 1:length(year_online_path)) {
                for(i in 1:length(year_online_path[[j]])) {
                    year_online[[j]][[i]] <- stringr::str_to_lower(
                        xml2::xml_text(
                            xml2::xml_find_first(record[[j]],
                                                 year_online_path[[j]][[i]])))
                }})
        
        year_online <- purrr::map2(.x = year_online, 
                                   .y = year_online_path,
                                   ~ data.frame(year_online = .x,
                                                article_node = stringr::str_extract(.y,
                                                                                    '/PubmedArticleSet/PubmedArticle\\[[0-9][0-9]?[0-9]?\\]')) %>% 
                                       dplyr::mutate(article_node = as.character(article_node),
                                                     year_online = as.character(year_online)))
    } else {
        # Make empty dataframe
        year_online <- purrr::map(1:length(title), 
                                  ~ data.frame(article_node = as.character(),
                                               year_online = as.character()))
    }
    #-- Pages ---------------------------------------------------------#

    if(length(pages_path) > 0) {
        # Define vector for publication year
        pages <- purrr::map(pages_path,
                            vector(mode = 'character'))
        
        suppressWarnings(
            for(j in 1:length(pages_path)) {
                for(i in 1:length(pages_path[[j]])) {
                    pages[[j]][[i]] <- stringr::str_to_lower(
                        xml2::xml_text(
                            xml2::xml_find_first(record[[j]],
                                                 pages_path[[j]][[i]])))
                }})
        
        pages <- purrr::map2(.x = pages, 
                             .y = pages_path, 
                             ~ data.frame(pages = .x,
                                          article_node = stringr::str_extract(.y,
                                                                              '/PubmedArticleSet/PubmedArticle\\[[0-9][0-9]?[0-9]?\\]')) %>% 
                                 dplyr::mutate(article_node = as.character(article_node),
                                               pages = as.character(pages)))
    } else {
        # Make empty dataframe
        pages <- purrr::map(1:length(title), 
                            ~ data.frame(article_node = as.character(),
                                         pages = as.character()))
    }
    

    #-- PMID --------------------------------------------------------#

    # Define vector for journal volume
    pmid <- purrr::map(pmid_path,
                       vector(mode = 'character'))
    
    suppressWarnings(
        for(j in 1:length(pmid_path)) {
            for(i in 1:length(pmid_path[[j]])) {
                pmid[[j]][[i]] <- stringr::str_to_lower(
                    xml2::xml_text(
                        xml2::xml_find_first(record[[j]],
                                             pmid_path[[j]][[i]])))
            }})
    
    pmid <- purrr::map2(.x = pmid, 
                        .y = pmid_path, 
                        ~ data.frame(pmid = .x,
                                     article_node = stringr::str_extract(.y,
                                                                         '/PubmedArticleSet/PubmedArticle\\[[0-9][0-9]?[0-9]?\\]')) %>% 
                            dplyr::mutate(article_node = as.character(article_node),
                                          pmid = as.character(pmid)))

    #-- DOI ---------------------------------------------------------#

    if(length(doi_path) > 0) {
        # Define vector for publication year
        doi <- purrr::map(doi_path,
                          vector(mode = 'character'))
        
        suppressWarnings(
            for(j in 1:length(doi_path)) {
                for(i in 1:length(doi_path[[j]])) {
                    doi[[j]][[i]] <- stringr::str_to_lower(
                        xml2::xml_text(
                            xml2::xml_find_first(record[[j]],
                                                 doi_path[[j]][[i]])))
                }})
        
        doi <- purrr::map2(.x = doi, 
                           .y = doi_path,
                           ~ data.frame(doi = .x,
                                        article_node = stringr::str_extract(.y,
                                                                            '/PubmedArticleSet/PubmedArticle\\[[0-9][0-9]?[0-9]?\\]')) %>% 
                               dplyr::mutate(article_node = as.character(article_node),
                                             doi = as.character(doi)))
    } else {
        # Make empty dataframe
        doi <- purrr::map(1:length(title),
                          ~ data.frame(article_node = as.character(),
                                       doi = as.character()))
    }
    
    #-- Abstract ---------------------------------------------------------#

    # Define vector for journal volume
    abstract <- purrr::map(abstract_path,
                         vector(mode = 'character'))
    
    suppressWarnings(
        for(j in 1:length(abstract_path)) {
            for(i in 1:length(abstract_path[[j]])) {
                abstract[[j]][[i]] <- stringr::str_to_lower(
                    xml2::xml_text(
                        xml2::xml_find_first(record[[j]],
                                             abstract_path[[j]][[i]])))
            }})
    
    abstract <- purrr::map2(.x = abstract, 
                            .y = abstract_path, 
                            ~ data.frame(abstract = .x,
                                         article_node = stringr::str_extract(.y,
                                                                             '/PubmedArticleSet/PubmedArticle\\[[0-9][0-9]?[0-9]?\\]')) %>% 
                                dplyr::mutate(article_node = as.character(article_node),
                                              abstract = as.character(abstract)))
    
    ############################################################
    #                                                          #
    #                    Put it all together                   #
    #                                                          #
    ############################################################

    #-- Make into dataframe ----------------------------------------------#
    
    suppressMessages(joined <- purrr::map2(.x = surname,
                          .y = initials,
                          ~ left_join(.x, .y)) %>% 
        purrr::map2(.x = .,
                    .y = title,
                    ~ left_join(.x, .y)) %>% 
        purrr::map2(.x = .,
                    .y = journal,
                    ~ left_join(.x, .y)) %>%
        purrr::map2(.x = .,
                    .y = status,
                    ~ left_join(.x, .y)) %>%
        purrr::map2(.x = .,
                    .y = volume,
                    ~ left_join(.x, .y)) %>%
        purrr::map2(.x = .,
                    .y = pages,
                    ~ left_join(.x, .y)) %>%
        purrr::map2(.x = .,
                    .y = year_published,
                    ~ left_join(.x, .y)) %>%
        purrr::map2(.x = .,
                    .y = year_online,
                    ~ left_join(.x, .y)) %>%
        purrr::map2(.x = .,
                    .y = pmid,
                    ~ left_join(.x, .y)) %>%
        purrr::map2(.x = .,
                    .y = doi,
                    ~ left_join(.x, .y)) %>% 
        purrr::map2(.x = .,
                    .y = abstract,
                    ~ left_join(.x, .y)))
    
    joined <- dplyr::bind_rows(joined) %>% 
        dplyr::select(-counter, -article_node)
        
    empty_as_na <- function(x){
        ifelse(x == '', yes = NA, no = x)
    }

    joined <- joined %>%
        dplyr::mutate(surname = as.character(surname),
                      initials = as.character(initials),
                      title = as.character(title),
                      journal = as.character(journal),
                      status = as.character(status),
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
                      year_online = as.numeric(year_online)) %>% 
        dplyr::as_tibble()
    
    #-- Output ----------------------------------------------------------------#

    return(joined)
}


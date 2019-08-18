#' @title Extract bibliographic information from PubMed records.
#
#' @description \code{parse_bibliographics} is typically called by \code{\link{get_records}}. It takes the xml output from an \href{http://www.ncbi.nlm.nih.gov/pubmed}{PubMed} database query and returns a dataframe of bibliographic information for each \href{http://www.ncbi.nlm.nih.gov/pubmed}{PubMed} record returned by the query.
#'
#' @param record The xml output from an \href{http://www.ncbi.nlm.nih.gov/pubmed}{PubMed} database query. Typically the database query is specified in the \emph{terms} parameter of the \code{\link{get_records}} function, and the downloaded xml output returned by the query is passed to \code{parse_bibliographics} for parsing. The parsed bibliographic data are then incorporated into the dataframe output of \code{\link{get_records}}. However, any xml-format PubMed query may be passed to the function independently of \code{\link{get_records}}, including xml files downloaded from \href{http://www.ncbi.nlm.nih.gov/pubmed}{PubMed} and unparsed results returned by \code{\link[rentrez]{entrez_fetch}}.
#'
#' @family related functions
#'
#' @export
parse_bibliographics <- function(record) {

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

    #-- PMID -------------------------------------------------------------#

    # pmid_path <- xml2::xml_path(
    #     xml2::xml_find_all(record,
    #                        ".//ArticleId[@IdType = 'pubmed']"))

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

    for(i in 1:length(surname_path)) {
        surname[[i]] <- stringr::str_to_lower(
            xml2::xml_text(
                xml2::xml_find_first(record,
                                     surname_path[[i]])))
    }

    #-- Initials ---------------------------------------------------------#

    # Define vector for author initials
    initials <- vector(mode = 'character',
                       length = length(initials_path))

    for(i in 1:length(initials_path)) {
        initials[[i]] <- stringr::str_to_lower(
            xml2::xml_text(
                xml2::xml_find_first(record,
                                     initials_path[[i]])))
    }

    # For joins with output from parse_affiliation
    authors <- paste(surname, initials)

    #-- Title ------------------------------------------------------------#

    # Define vector for article title
    title <- vector(mode = 'character',
                    length = length(title_path))

    for(i in 1:length(title_path)) {
        title[[i]] <- stringr::str_to_lower(
            xml2::xml_text(
                xml2::xml_find_first(record,
                                     title_path[[i]])))
    }

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
                         title = title)

    #-- Journal ----------------------------------------------------------#

    # Define vector for journal name
    journal <- vector(mode = 'character',
                      length = length(journal_path))

    for(i in 1:length(journal_path)) {
        journal[[i]] <- stringr::str_to_lower(
            xml2::xml_text(
                xml2::xml_find_first(record,
                                     journal_path[[i]])))
    }

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
                                                         replacement = ''))

    #-- Publication status -------------------------------------------#

    # Define vector for publication status
    status <- vector(mode = 'character',
                     length = length(status_path))

    for(i in 1:length(status_path)) {
        status[[i]] <- xml2::xml_text(
            xml2::xml_find_first(record,
                                 status_path[[i]]))
    }

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
                                                  no = 'ahead of print')))

    #-- Volume ---------------------------------------------------------#

    # Define vector for journal volume
    volume <- vector(mode = 'numeric',
                     length = length(volume_path))

    for(i in 1:length(volume_path)) {
        volume[[i]] <- xml2::xml_integer(
            xml2::xml_find_first(record,
                                 volume_path[[i]]))
    }

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
                          volume = volume)

    #-- Year published -------------------------------------------------#

    if(length(year_published_path) > 0) {
        # Define vector for publication year
        year_published <- vector(mode = 'character',
                                 length = length(year_published_path))

        for(i in 1:length(year_published_path)) {
            year_published[[i]] <- xml2::xml_text(
                xml2::xml_find_first(record,
                                     year_published_path[[i]]))
        }

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
                                               pattern = '[0-9][0-9][0-9][0-9]'))
    } else {
        # Make empty dataframe
        year_published2 <- data.frame(article_node = as.character(),
                                      year_published = as.numeric())
    }

    #-- Year online ---------------------------------------------------#

    if(length(year_online_path) > 0) {
        # Define vector for online publication year
        year_online <- vector(mode = 'character',
                              length = length(year_online_path))

        for(i in 1:length(year_online_path)) {
            year_online[[i]] <- xml2::xml_text(
                xml2::xml_find_first(record,
                                     year_online_path[[i]]))
        }

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
                                   year_online = year_online)
    } else {
        # Make empty dataframe
        year_online2 <- data.frame(article_node = as.character(),
                                   year_online = as.numeric())
    }

    #-- Pages ---------------------------------------------------------#

    if(length(pages_path) >0 ) {
        # Define vector for aricle page numbers
        pages <- vector(mode = 'character',
                        length = length(pages_path))

        for(i in 1:length(pages_path)) {
            pages[[i]] <- stringr::str_to_lower(
                xml2::xml_text(
                    xml2::xml_find_first(record,
                                         pages_path[[i]])))
        }

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
                             pages = pages)
    } else {
        # Make empty dataframe
        pages2 <- data.frame(article_node = as.character(),
                             pages = as.character())
    }

    #-- PMID ---------------------------------------------------------#

    # if(length(pmid_path) > 0) {
    #     # Define vector for pmid
    #     pmid <- vector(mode = 'numeric',
    #                    length = length(pmid_path))
    #
    #     for(i in 1:length(pmid_path)) {
    #         pmid[[i]] <- xml2::xml_text(
    #             xml2::xml_find_first(record,
    #                                  pmid_path[[i]]))
    #     }
    #
    #     # Make article marker for joins
    #     ## Define vector for 'trimmed' pmid path
    #     pmid_path2 <- vector(mode = 'character',
    #                          length = length(pmid_path))
    #
    #     for(i in 1:length(pmid_path)) {
    #         pmid_path2[[i]] <- stringr::str_extract(pmid_path[[i]],
    #                                                 '/PubmedArticleSet/PubmedArticle\\[[0-9][0-9]?[0-9]?\\]')
    #     }
    #
    #     # Make dataframe
    #     pmid2 <- data.frame(article_node = pmid_path2,
    #                         pmid = pmid)
    # } else {
    #     # Make empty dataframe
    #     pmid2 <- data.frame(article_node = as.character(),
    #                         pmid = as.numeric())
    # }

    #-- DOI ---------------------------------------------------------#

    if(length(doi_path) > 0) {
        # Define vector for doi
        doi <- vector(mode = 'character',
                      length = length(doi_path))

        for(i in 1:length(doi_path)) {
            doi[[i]] <- xml2::xml_text(
                xml2::xml_find_first(record,
                                     doi_path[[i]]))
        }

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
                           doi = doi)
    } else {
        # Make empty dataframe
        doi2 <- data.frame(article_node = as.character(),
                           doi = as.character())
    }

    #-- Abstract ---------------------------------------------------------#

    # Define vector for abstracts
    abstract <- vector(mode = 'character',
                       length = length(abstract_path))

    for(i in 1:length(abstract_path)) {
        abstract[[i]] <- stringr::str_to_lower(
            xml2::xml_text(
                xml2::xml_find_first(record,
                                     abstract_path[[i]])))
    }

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

    # Expand article_node to length 'surname'
    node <- purrr::map2(.x = counter$value,
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
        # dplyr::left_join(title2,
        #                  by = 'article_node') %>%
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
                              authors = authors)

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
                      doi = as.character(doi),
                      abstract = as.character(abstract)) %>%
        dplyr::mutate_all(.funs = trimws) %>%
        dplyr::mutate_all(.funs = empty_as_na) %>%
        dplyr::mutate(year_published = as.numeric(year_published),
                      year_online = as.numeric(year_online))

    #-- Output -----------------------------------------------------------#

    return(biblio_out)

}

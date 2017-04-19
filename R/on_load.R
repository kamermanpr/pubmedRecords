# This user-invisible function defines global variables when the package loads,
# thus avoiding 'no visible binding for global variable' notes when
# running R CMD check.

.onLoad <- function(libname = find.package('pubmedRecords'),
                    pkgname = 'pubmedRecords') {

    if(getRversion() >= "2.15.1")
        utils::globalVariables(
            # Based on initial R CMD check
            c('.', 'affiliation', 'article_node', 'author_node', 'authors',
              'count', 'country_iso3', 'country_name', 'initials', 'n',
              'order2', 'state_name', 'surname', 'value', 'pmid', 'doi',
              'pmid2', 'score', 'cited_by_tweeters_count',
              'cited_by_fbwalls_count', 'cited_by_posts_count',
              'mendeley', 'crossref_citations', 'publication_status'))

    invisible()
}
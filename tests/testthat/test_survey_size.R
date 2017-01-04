library(PubMineR)
context('dataframe structure')

terms <- c('((Pain[JOUR] NOT Pain Suppl[JOUR])
           AND 2015[PDAT] AND Kamerman[AUTH]')

full_record <- get_records(terms = terms)

as.numeric(full_record[1,7])

test_that("get_records returns a dataframe with 12 columns and n rows", {

    expect_match(names(full_record[1]), 'title')

    expect_match(names(full_record[11]), 'last_author')

    expect_output(str(full_record), "19 obs. of  12 variables")

    expect_equal(as.numeric(full_record[1, 7]), 26469320)
})

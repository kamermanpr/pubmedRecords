############################################################
#                                                          #
#    Generate xml document of PubMed records of A Fuller   #
#                                                          #
############################################################
library(magrittr)
library(xml2)

# Get PMIDs
pmid_string <- read_xml(
    paste0('https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pubmed&term=Treede+RD[AUTH]&jornal+article[PTYP]&rettype=xml&retmax=1000')) %>%
    xml_find_all(., xpath = './/Id') %>%
    xml_integer(.)

# Construct NCBI eFetch query
url <- paste0('https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=pubmed&id=', paste(pmid_string, collapse = ','), '&retmode=xml')

# Download xml record
xml_record <- read_xml(url)

# Write to file
write_xml(xml_record, './data-raw/treede_xml.rda')
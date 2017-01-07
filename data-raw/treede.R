############################################################
#                                                          #
#   Generate xml document of PubMed records of RD Treede   #
#                                                          #
############################################################

#-- Get PMIDs ------------------------------------------------------------#

# Basic query string
pmid_string <- 'https://eutils.ncbi.nlm.nih.gov/entrez/eutils/'

# Search criteria string
pmid_search <- xml2::read_xml(paste0(pmid_string,
                                     'esearch.fcgi?db=pubmed',
                                     '&term=journal+article[PT]+AND+Treede+RD[AU]+hasabstract&datetype=PDAT&mindate=1980/01/01&maxdate=2017/01/01&rettype=xml&retmax=300'))

# Extract PMID xpath
pmid_search <- xml2::xml_find_all(pmid_search, xpath = './/Id')

# Use xpath to extract PMIDs
pmid_search <- xml2::xml_integer(pmid_search)

#-- Fetch records --------------------------------------------------------#

# Construct fetch string
url <- paste0(pmid_string,
              'efetch.fcgi?db=pubmed&id=',
              paste(pmid_search, collapse = ','),
              '&retmode=xml&retmax=248')

# Download xml record
treede <- xml2::read_xml(url)

#-- Save (external) ------------------------------------------------------#

xml2::write_xml(treede,
                file = './inst/extdata/treede.xml')


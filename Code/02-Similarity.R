# This script evaluates the similarity between species

# Libraries
library(magrittr)
library(iEat)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# 1. IMPORT SPECIES LIST AND TAXONOMY
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
load('./Data/Taxonomy/TaxonomyEGSL.RData')
load('./Data/Taxonomy/TaxonomyPhytoplankton.RData')
load('./Data/Taxonomy/TaxonomyZooplankton.RData')
taxo <- rbind(taxonomy, taxoPhyto, taxoZoo)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# 2. IMPORT INTERACTION CATALOGUE
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
load('./Data/InteractionCatalogue/S0_catalog.RData')


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# 3. INSERT MISSING TAXA IN CATALOGUE
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Missing taxa
uid <- !taxo$taxon %in% S0_catalog[, 'taxon']
S0_add <- data.frame(taxon = taxo$taxon[uid],
                     taxonomy = taxo$taxonomy[uid],
                     resource = '',
                     nonresource = '',
                     consumer = '',
                     nonconsumer = '',
                     row.names = taxo$taxon[uid],
                     stringsAsFactors = F) %>%
          as.matrix()

# Add to catalogue
S0 <- rbind(S0_catalog, S0_add)

# Export catalog
save(S0, file = './Data/InteractionCatalogue/S0.RData')

# Weight values for 2-way similarity measurements
wt <- 0.5

# 1st is for similarity measured from set of resources and taxonomy, for consumers
similarity.consumers <- similarity_taxon(S0 = S0, wt = wt, taxa = 'consumer')
save(similarity.consumers, file = "./Data/Similarity/Similarity_consumers.RData")

# 2nd is for similarity measured from set of consumers and taxonomy, for resources
similarity.resources <- similarity_taxon(S0 = S0, wt = wt, taxa = 'resource')
save(similarity.resources, file = "./Data/Similarity/Similarity_resources.RData")

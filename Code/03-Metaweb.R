# This script predicts the metaweb of the Estuary and Gulf of St. Lawrence

# Libraries
library(iEat)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# 1. IMPORT SPECIES LIST AND TAXONOMY
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
load('./Data/Taxonomy/TaxonomyEGSL.RData')
load('./Data/Taxonomy/TaxonomyPhytoplankton.RData')
load('./Data/Taxonomy/TaxonomyZooplankton.RData')
taxo <- rbind(taxonomy, taxoPhyto, taxoZoo)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# 2. IMPORT FULL INTERACTION CATALOGUE
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
load('./Data/InteractionCatalogue/S0.RData')
S0 <- S0[, c('taxon','resource','consumer')]
colnames(S0) <- c('taxon','target','source')


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# 3. IMPORT SIMILARITY MATRICES
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
load('./Data/Similarity/Similarity_consumers.RData')
load('./Data/Similarity/Similarity_resources.RData')


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# 4. PREDICT BINARY INTERACTIONS
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
metaweb <- iEat(S0 = S0,
                S1 = taxo$taxon,
                S2 = taxonomy$taxon,
                sourceSim = similarity.consumers,
                targetSim = similarity.resources,
                K = 7,
                minSim = 0.3,
                minWt = 1,
                predict = 'full algorithm')


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# 5. CREATE FOOD WEB MATRIX
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
metaweb <- iEat_to_foodWeb(metaweb)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# 5. COMBINE phytoplankton and zooplankton species
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=
#         Phytoplankton
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Taxa
phyto <- taxoPhyto$taxon

# Consumers
consPhyto <- colSums(metaweb[phyto, ])

# Resources (theoretically this should all be 0s)
resPhyto <- rowSums(metaweb[, phyto])

# Binaries
consPhyto <- ifelse(consPhyto > 0, 1, 0)
resPhyto <- ifelse(resPhyto > 0, 1, 0)

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=
#          Zooplankton
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Taxa
zoo <- taxoZoo$taxon

# Consumers
consZoo <- colSums(metaweb[zoo, ])

# Resources (theoretically this should all be 0s)
resZoo <- rowSums(metaweb[, zoo])

# Binaries
consZoo <- ifelse(consZoo > 0, 1, 0)
resZoo <- ifelse(resZoo > 0, 1, 0)

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=
#        Add to metaweb
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Taxa names
sp <- colnames(metaweb)
sp <- c(sp, 'Zooplankton','Phytoplankton')

# Add phytoplankton and zooplankton
metaweb <- cbind(metaweb, resZoo, resPhyto)
metaweb <- rbind(metaweb, c(consZoo,0,0), c(consPhyto,1,0))

# Change colnames
colnames(metaweb) <- rownames(metaweb) <- sp

# Remove phytoplankton and zooplankton taxa
uid <- !sp %in% c(phyto, zoo)
metaweb <- metaweb[uid,uid]


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# 6. EXPORT METAWEB
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
save(metaweb, file = './Data/Metaweb/metaweb.RData')

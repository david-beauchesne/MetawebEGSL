# This script imports and validates the list of species and their taxonomical
# information for use in the analyses
#
#   Steps:
#     1. Import list of observed species considered for inclusion in the metaweb
#     2. Import interaction catalogue
#     3. Species taxonomy from catalogue
#     4. Missing species taxonomy from WoRMS
#     5. Combine taxonomy
#     6. Identify and manually get any missing taxonomical information
#     7. Export species list

# Libraries
library(magrittr)

# # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# # 1. IMPORT ST. LAWRENCE FULL SPECIES LIST
# # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# carms <- read.table(file = './Data/SpeciesLists/CaRMS_checklist_20160516.txt', sep = '\t', header = TRUE, stringsAsFactors = F)
#
# # Keep only necessary information
# carms <- carms[, c('ScientificName','Kingdom','Phylum','Class','Order','Family','Genus','Species')]
#
# # Remove duplicates
# carms <- carms[!duplicated(carms), ]
#
# # Add formatted taxonomy
# carms$taxo <- paste(carms$Kingdom,
#                     carms$Phylum,
#                     carms$Class,
#                     carms$Order,
#                     carms$Family,
#                     carms$Genus,
#                     carms$ScientificName,
#                     sep = ' | ')


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# 1. IMPORT LIST OF SPECIES CONSIDERED FOR METAWEB
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
load('./Data/SpeciesLists/SpeciesList.RData')

# Format species names
sp$sp <- gsub('\\b sp.\\b', '', sp$species) # Remove sp. from species names
sp$sp <- stringr::str_trim(sp$sp, side = 'both') # Remove white spaces



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# 2. IMPORT INTERACTION CATALOGUE
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
load('./Data/InteractionCatalogue/S0_catalog.RData')


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# 3. SPECIES TAXONOMY FROM CATALOGUE
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Extract taxonomy of taxa that are already described in the catalog
# Start here to make sure that there is no discrepancy with information already
# available through the catalogue.
spTaxo <- S0_catalog[S0_catalog[, 'taxon'] %in% sp$sp, c('taxon','taxonomy')] %>%
          as.data.frame(stringsAsFactors = F)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# 4. SPECIES TAXONOMY FROM WoRMS
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Identify which taxa are not already included in the catalog
missCatalog <- sp$sp[!sp$sp %in% spTaxo$taxon]

# Get classification from WoRMS using 'taxize::classification()'
spClass <- taxize::classification(missCatalog, db = 'worms')

# Empty data.frame to store taxonomy
taxo <- c('Kingdom','Phylum','Class','Order','Family','Genus','Species')
taxoClass <- data.frame(rank = taxo, stringsAsFactors = F)

# Extract taxonomy
for(i in 1:length(missCatalog)) {
  # Run only if there is taxonomic information available
  if (all(!is.na(spClass[[i]]))) {
    # Taxonomic data
    dat <- spClass[[i]]

    # Keep only required taxonomic ranks
    dat <- dat[dat$rank %in% taxo, c('name','rank')]

    # Change name of column
    colnames(dat)[1] <- names(spClass)[i]

    # Fill in taxonomy matrix
    taxoClass <- taxoClass %>%
                 dplyr::left_join(dat, by = 'rank')
  }
}

# Transpose dataset
taxoClass <- t(taxoClass)

# Add column names and remove first row with ranks
colnames(taxoClass) <- taxoClass[1, ]
taxoClass <- taxoClass[-1,]

# Transform as data.frame
taxoClass <- as.data.frame(taxoClass, stringsAsFactors = F)

# Add taxon name from species list
taxoClass$taxon <- rownames(taxoClass)

# Taxonomic classification as single column
taxonomy <- apply(taxoClass[, taxo], 1, paste, collapse = ' | ')
taxoClass$taxonomy <- taxonomy


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# 5. COMBINE TAXONOMY FROM CATALOGUE & WoRMS
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
uid <- c('taxon','taxonomy')
taxonomy <- rbind(spTaxo[, uid], taxoClass[, uid])


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# 6. IDENTIFY AND MANUALLY RETRIEVE ANY MISSING TAXONOMICAL INFORMATION
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Identify still missing
uid <- missCatalog[!missCatalog %in% taxonomy$taxon]
taxo <- data.frame(taxon = uid,
                   taxonomy = 'Animalia | Cnidaria | Hydrozoa | Leptothecata | Laodiceidae | Staurostoma | Staurostoma mertensii',
                   row.names = 'Staurostoma mertensii',
                   stringsAsFactors = F)

# Combine with rest of information
taxonomy <- rbind(taxonomy, taxo)

# Sort data
taxonomy <- taxonomy[order(taxonomy$taxon), ]

save(taxonomy, file = './Data/Taxonomy/TaxonomyEGSL.RData')

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# 6. Add zooplankton and phytoplankton as additional species
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=
#         Zooplankton
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=
euphausiids <- c('Meganyctiphanes norvegica', 'Thysanoessa raschii',
                 'Thysanoessa inermis')

macrozooplankton <- c('Sagitta elegans','Pseudosagitta maxima','Eukrohnia hamata',
                      'Boreomysis artica','Mysis mixta','Erythrops erythrophthalma',
                      'Themisto gaudichaudi','Themisto abyssorum','Themisto compressa',
                      'Aglantha digitalis','Dimophyes arctica','Obelia','Beroe',
                      'Clione limacina','Limacina helicina','Decapoda','Tomopteris',
                      'Euphausiacea','Sagittoidea','Hyperiidea','Mysida','Ctenophora')

mesozooplankton <- c('Calanus finmarchicus','Calanus hyperboreus','Oithona similis',
                     'Temora longicornis','Pseudocalanus')

zoo <- c(euphausiids, macrozooplankton, mesozooplankton)

# Extract taxonomy of taxa that are already described in the catalog
# Start here to make sure that there is no discrepancy with information already
# available through the catalogue.
spTaxo <- S0_catalog[S0_catalog[, 'taxon'] %in% zoo, c('taxon','taxonomy')] %>%
          as.data.frame(stringsAsFactors = F)

# Identify which taxa are not already included in the catalog
missCatalog <- zoo[!zoo %in% spTaxo$taxon]

# Get classification from WoRMS using 'taxize::classification()'
spClass <- taxize::classification(missCatalog, db = 'worms')

# Empty data.frame to store taxonomy
taxo <- c('Kingdom','Phylum','Class','Order','Family','Genus','Species')
taxoClass <- data.frame(rank = taxo, stringsAsFactors = F)

# Extract taxonomy
for(i in 1:length(missCatalog)) {
  # Run only if there is taxonomic information available
  if (all(!is.na(spClass[[i]]))) {
    # Taxonomic data
    dat <- spClass[[i]]

    # Keep only required taxonomic ranks
    dat <- dat[dat$rank %in% taxo, c('name','rank')]

    # Change name of column
    colnames(dat)[1] <- names(spClass)[i]

    # Fill in taxonomy matrix
    taxoClass <- taxoClass %>%
                 dplyr::left_join(dat, by = 'rank')
  }
}

# Transpose dataset
taxoClass <- t(taxoClass)

# Add column names and remove first row with ranks
colnames(taxoClass) <- taxoClass[1, ]
taxoClass <- taxoClass[-1,]

# Transform as data.frame
taxoClass <- as.data.frame(taxoClass, stringsAsFactors = F)

# Add taxon name from species list
taxoClass$taxon <- rownames(taxoClass)

# Taxonomic classification as single column
taxoZoo <- apply(taxoClass[, taxo], 1, paste, collapse = ' | ')
taxoClass$taxonomy <- taxoZoo

# Combine taxonomy from catalogue & worms
uid <- c('taxon','taxonomy')
taxoZoo <- rbind(spTaxo[, uid], taxoClass[, uid])

# Export
save(taxoZoo, file = './Data/Taxonomy/TaxonomyZooplankton.RData')

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=
#        Phytoplankton
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=
phyto <- c('Chaetoceros affinis','Chaetoceros','Fragilariopsis oceanica',
           'Fragilariopsis cylindrus', 'Leptocylindrus minimus',
           'Thalassiosira bioculata','Thalassiosira nordenskioeldii',
           'Thalassiosira pacifica','Thalassiosira punctigera','Dinoflagellata',
           'Cryptophyta','Prasinophyceae','Prymnesiophyceae','Strombidium')

# Extract taxonomy of taxa that are already described in the catalog
# Start here to make sure that there is no discrepancy with information already
# available through the catalogue.
spTaxo <- S0_catalog[S0_catalog[, 'taxon'] %in% phyto, c('taxon','taxonomy')] %>%
          as.data.frame(stringsAsFactors = F)

# Identify which taxa are not already included in the catalog
missCatalog <- phyto[!phyto %in% spTaxo$taxon]

# Get classification from WoRMS using 'taxize::classification()'
spClass <- taxize::classification(missCatalog, db = 'worms')

# Empty data.frame to store taxonomy
taxo <- c('Kingdom','Phylum','Class','Order','Family','Genus','Species')
taxoClass <- data.frame(rank = taxo, stringsAsFactors = F)

# Extract taxonomy
for(i in 1:length(missCatalog)) {
  # Run only if there is taxonomic information available
  if (all(!is.na(spClass[[i]]))) {
    # Taxonomic data
    dat <- spClass[[i]]

    # Keep only required taxonomic ranks
    dat <- dat[dat$rank %in% taxo, c('name','rank')]

    # Change name of column
    colnames(dat)[1] <- names(spClass)[i]

    # Fill in taxonomy matrix
    taxoClass <- taxoClass %>%
                 dplyr::left_join(dat, by = 'rank')
  }
}

# Transpose dataset
taxoClass <- t(taxoClass)

# Add column names and remove first row with ranks
colnames(taxoClass) <- taxoClass[1, ]
taxoClass <- taxoClass[-1,]

# Transform as data.frame
taxoClass <- as.data.frame(taxoClass, stringsAsFactors = F)

# Add taxon name from species list
taxoClass$taxon <- rownames(taxoClass)

# Taxonomic classification as single column
taxoPhyto <- apply(taxoClass[, taxo], 1, paste, collapse = ' | ')
taxoClass$taxonomy <- taxoPhyto

# Combine taxonomy from catalogue & worms
uid <- c('taxon','taxonomy')
taxoPhyto <- rbind(spTaxo[, uid], taxoClass[, uid])

# Export
save(taxoPhyto, file = './Data/Taxonomy/TaxonomyPhytoplankton.RData')

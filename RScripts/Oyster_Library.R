fortify_oyster <- function(counts, tax){
  countLong <- counts %>% 
    pivot_longer(-ASV, names_to = "Sample0", values_to = "count") %>%
    #filter(count > 0) %>%
    identity()
  
  countTax <- left_join(countLong, tax, by = "ASV")
  
  oysterSummary <- countTax %>% filter(Order == "Ostreoida") %>% group_by(Sample0) %>% 
    summarize(n = n(), OysterCount = sum(count))
  
  nonOyster <- countTax %>% filter(Order != "Ostreoida", Kingdom != "Spike")
  nonOyster <- nonOyster %>%
    left_join(oysterSummary, by = "Sample0", suffix = c("Bact", "Oyster"))
  nonOyster <- nonOyster %>%
    mutate(ratio = count/OysterCount)
  
  # # causes problems
  # nonOyster <- nonOyster %>%
    # filter(count > 0)
  
  return(nonOyster)
}

# with metadata
fortify_oyster_wm <- function(counts, tax, metadata){

  fort <- fortify_oyster(counts, tax)
  
  wm <- left_join(fort, metadata, by = "Sample0")
  
  
}

add_tag_to_tax <- function(tax){
  TaxResTab <- tibble(
  TagLevel = c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus"),
  TaxRes = 1:6
)
  
  taxN <- tax %>%
    mutate(nASV = extract_numeric(ASV))
  
  TagDf <- taxN %>%
  select(-nASV) %>%
  pivot_longer(Kingdom:Genus, names_to = "TagLevel", values_to = "Tag") %>%
  left_join(TaxResTab) %>%
  filter(!is.na(Tag)) %>%
  group_by(ASV) %>%
  slice_max(order_by = TaxRes, n = 1) %>%
  select(-TaxRes) %>%
  #mutate(ASV1 = str_extract(ASV, "\\d+")) %>%
  mutate(Tag_ASV = paste(Tag, str_extract(ASV, "\\d+"), sep = ";"))
  
  taxa <- taxN %>%
   left_join(TagDf, by = "ASV")
  
  taxa
}

# group_taxa <- function(fort, taxLevel){
#   taxLevel_eq <- enquo(taxLevel)
#   fort %>%
#     group_by(!!taxLevel_eq, Sample0) %>%
#     summarise(across(.cols = everything(), .fns = first),
#               across(.cols = c(count, ratio), .fns = sum)
#     ) %>%
#     mutate(GlomLevel = !!taxLevel_eq)
# }

group_taxa <- function(fort, taxLevel){
  taxLevel_eq <- enquo(taxLevel)
  fort %>%
    group_by(!!taxLevel_eq, Sample0) %>%
    summarise(across(.cols = c(count, ratio), .fns = sum),
              across(.cols = everything(), .fns = first) # Switch order
    ) %>%
    mutate(GlomLevel = !!taxLevel_eq)
}

# I was unable to ensym taxLevel because it instead ensyms the "." operator from map
# I went back to the "depreciated" `group_by`
group_taxa_2 <- function(fort, taxLevel){
  #taxLevel_ens <- ensym(taxLevel)
  fort %>%
    group_by_(taxLevel, "Sample0") %>% # was group_by_
    summarise(
              across(.cols = c(count, ratio), .fns = sum), # reordered here too
              across(.cols = everything(), .fns = first),
              .groups = "keep"
    ) %>%
    #rename(GTax = 1) %>%
    identity()
}

keep_common <- function(fort, fraction = 0.2, minhits = 3){
  nSamples <- fort$Sample0 %>% unique() %>% length()
  
  hitFracDf <- fort %>%
  select(ASV, count, ratio) %>%
  mutate(enoughHits = count>= minhits) %>%
  group_by(ASV) %>%
  summarise(hits = sum(enoughHits)) %>%
  mutate(hitfrac = hits/nSamples) %>%
  identity()

hitFracOk <- hitFracDf %>%
  filter(hitfrac > fraction)

fort20 <- fort %>%
  filter(ASV %in% hitFracOk$ASV)

fort20
}

library(inbodb)
library(tidyverse)

con <- connect_inbo_dbase("D0156_00_Taxonlijsten")

All_lists <- get_taxonlijsten_lists(con,
                                    list = "%",
                                    version = c("latest",
                                                "old",
                                                "all"),
                                    collect = TRUE)
All_lists

################
# Rode Lijsten #
################
rodelijst_status <- get_taxonlijsten_lists(con,
                                           #list = "%rode%lijst%dagvlinders%",
                                           list = "%rode%lijst%",
                                           version = "all",
                                           collect = TRUE) %>%
  select("Taxonlijst",
         "PublicatieJaar",
         "Criteria",
         "Validering",
         "Vaststelling")
head(rodelijst_status)

get_taxonlijsten_features(con,
                          list = "%",
                          version = c("latest",
                                      "old",
                                      "all"),
                          collect = FALSE)

###########################
# Habitattypische soorten #
###########################
habitat_fauna <- get_taxonlijsten_features(con,
                                           list = "%Habitattyp%fauna%")

get_taxonlijsten_items(con,
                       list = "%",
                       taxon = "%",
                       feature = "%",
                       version = c("latest",
                                   "old",
                                   "all"),
                       original = FALSE,
                       collect = FALSE)

typ91E0 <- get_taxonlijsten_items(con,
                                  list = "Hab%typ%",
                                  feature = "91E0",
                                  collect = TRUE)
head(typ91E0)

# Vraag de Rode-Lijststatus op van de vier bestuiversgroepen
RL_dagvlinders <- get_taxonlijsten_items(con, version = "latest",
                                         list = "%rode%dagvlinders%",
                                         collect = TRUE)
RL_nachtvlinders <- get_taxonlijsten_items(con, version = "latest",
                                           list = "%rode%nachtvlinders%",
                                           collect = TRUE)
RL_zweefvliegen <- get_taxonlijsten_items(con, version = "latest",
                                          list = "%rode%zweefvliegen%",
                                          collect = TRUE)
RL_wildebijen <- get_taxonlijsten_items(con, version = "latest",
                                        list = "%rode%bijen%",
                                        collect = TRUE)

RL_bestuivers <- rbind(RL_dagvlinders,
                       RL_nachtvlinders,
                       RL_zweefvliegen,
                       RL_wildebijen) %>% 
  rename(RodeLijststatus = KenmerkwaardeCode,
         WetenschappelijkeNaam = Naamwet_interpretatie,
         NederlandseNaam = NaamNed_interpretatie) %>% 
  select(Taxongroep,
         WetenschappelijkeNaam,
         NederlandseNaam,
         RodeLijststatus)
head(RL_bestuivers)

ReCrEnVu <- RL_bestuivers %>% 
  filter(RodeLijststatus == "RE" | RodeLijststatus == "CR" | RodeLijststatus == "EN" | RodeLijststatus == "VU" | RodeLijststatus == "NT")
ReCrEnVu

# Hoe evolueert de Rode-Lijststatus van een soortengroep?
red_list_evolution_nachtvlinders <- get_taxonlijsten_items(
  con,
  version = "all",
  #list = "rode lijst van de dagvlinders",
  list = "Rode lijst van de Macro-nachtvlinders",
  collect = TRUE
) %>% 
  select(
    "Lijst",
    "Publicatiejaar",
    "Naamwet_interpretatie",
    "NaamNed_interpretatie",
    "KenmerkwaardeCode"
  ) %>% 
  pivot_wider(
    names_from = "Publicatiejaar",
    values_from = "KenmerkwaardeCode"
  )
head(red_list_evolution_nachtvlinders)
nrow(red_list_evolution_nachtvlinders)

red_list_evolution_dagvlinders <- get_taxonlijsten_items(
  con,
  version = "all",
  list = "rode lijst van de dagvlinders",
  collect = TRUE
) %>% 
  select(
    "Lijst",
    "Publicatiejaar",
    "Naamwet_interpretatie",
    "NaamNed_interpretatie",
    "KenmerkwaardeCode"
  ) %>% 
  pivot_wider(
    names_from = "Publicatiejaar",
    values_from = "KenmerkwaardeCode"
  )
head(red_list_evolution_dagvlinders)
nrow(red_list_evolution_dagvlinders)

###########################
# Habitatrichtlijnsoorten #
###########################
HRL_list <- get_taxonlijsten_lists(con,
                       list = "Habitatricht%",
                       version = c("latest",
                                   "old",
                                   "all"),
                       collect = TRUE)
HRL_list

HRL <- get_taxonlijsten_items(con,
                              list = "%Habitatrichtlijn%",
                              taxon = "%",
                              feature = "%",
                              original = FALSE,
                              collect = TRUE)
head(HRL)

##############################
# Vlaams prioritaire soorten # lukt niet!!
##############################
VPS_list <- get_taxonlijsten_lists(con,
                                   list = "%Vlaams%prioritair%",
                                   version = c("latest"),
                                   collect = TRUE)
VPS_list

VPS <- get_taxonlijsten_items(con,
                              list = "%Vlaams%prioritair%",
                              version = c("latest"),
                              taxon = "%",
                              feature = "%",
                              original = FALSE,
                              collect = TRUE)
head(VPS)

library(tidyverse)
library(readxl)
library(inbodb)
library(rprojroot)
library(assertthat)

get_criteria <-
  function(soorten) {
    con <- connect_inbo_dbase("D0156_00_Taxonlijsten")
    All_lists <- get_taxonlijsten_lists(con,
                                        list = "%",
                                        version = c("latest",
                                                    "old",
                                                    "all"),
                                        collect = TRUE)

    #rode lijst
    rl_status <- get_taxonlijsten_items(con, version = "latest",
                                        list = "%rode%lijst%",
                                        collect = TRUE) %>%
      group_by(Naamwet_interpretatie) %>% #4 soorten vleermuis hebben een dubbele status
      slice(1) %>%
      ungroup %>% #behoud enkel gevalideerde rode lijsten
      dplyr::filter(Lijst %in%
                      (All_lists %>%
                         dplyr::filter(is.na(Validering) |
                                         Validering == "Gevalideerd") %>%
                         dplyr::pull(Taxonlijst)))
    #alleen deze willen we houden
    rl_statussen <- c("Ernstig bedreigd", "Bedreigd", "Kwetsbaar")
    criteria <- soorten %>%
      left_join(rl_status %>%
                  dplyr::select(Naamwet_interpretatie, Kenmerkwaarde) %>%
                  rename("rl_status" = "Kenmerkwaarde"),
                by = join_by(naam_wetenschappelijk ==
                               Naamwet_interpretatie)) %>%
      mutate(rl_status = ifelse(rl_status %in% rl_statussen,
                                rl_status,
                                "Andere"))
    #Europees beschermd - Habitatrichtlijn & Vogelrichtlijn (HRL2 > HRL4 = VRL1 > geen VRL > geen HRL2 of 4 = HRL5)
    RL <- get_taxonlijsten_items(con,
                                 list = "%Habitatrichtlijn%",
                                 taxon = "%",
                                 feature = "%",
                                 original = FALSE,
                                 collect = TRUE) %>%
      mutate(KenmerkwaardeCode = str_c("HRL", KenmerkwaardeCode)) %>%
      rbind(get_taxonlijsten_items(con,
                                   list = "%Vogelrichtlijn%",
                                   taxon = "%",
                                   feature = "%",
                                   original = FALSE,
                                   collect = TRUE) %>%
              mutate(KenmerkwaardeCode = str_c("VRL", KenmerkwaardeCode))) %>%
      mutate(KenmerkwaardeCode = factor(as.factor(KenmerkwaardeCode),
                                        levels = c("HRLII", "HRLIV", "VRLI",
                                                   "HRLV"),
                                        ordered = TRUE)) %>%
      dplyr::filter(KenmerkwaardeCode != "HRLV") %>% #HRL5 zit op hetzelfde niveau als geen HRL2 of 4
      arrange(KenmerkwaardeCode) %>%
      group_by(Naamwet_interpretatie) %>%
      slice_head(n = 1) %>%#hou enkel Kenmerkwaardecode met hoogste belang -> indien zowel HRL2 als 4, houden we enkel HRL2
      ungroup() %>%
      mutate(KenmerkwaardeCode = as.character(KenmerkwaardeCode))
    criteria <- criteria %>%
      left_join(RL %>%
                  rename("EB" = "KenmerkwaardeCode") %>% #EB = "Europees beschermd"
                  dplyr::select(Naamwet_interpretatie, EB),
                by = join_by(naam_wetenschappelijk == Naamwet_interpretatie)) %>%
      mutate(EB =
               ifelse(str_detect(taxonomische_groep, "vogels") &
                        is.na(EB),
                      "VRL0", # geen VRL is belangrijker dan geen HRL
                      EB)) %>%
      mutate(EB = factor(as.factor(EB),
                                         levels = c("HRLII", "HRLIV", "VRLI",
                                                    "VRL0"),
                                         ordered = TRUE))
    #Habitattypische soorten (habitattypische soort annex 1-habitat >
    #habitattypische soort > niet-habitattypes soorten)
    HTS <- get_taxonlijsten_items(con,
                                  list = "%Habitattypische%faunasoorten%",
                                  taxon = "%",
                                  feature = "%",
                                  original = FALSE,
                                  collect = TRUE) %>%
      mutate(HTS = TRUE)
    criteria <- criteria %>%
      left_join(HTS %>% distinct(Naamwet_interpretatie, HTS),
                by = join_by(naam_wetenschappelijk ==
                               Naamwet_interpretatie)) %>%
      mutate(HTS = ifelse(is.na(HTS), FALSE, HTS))

    #IHD  (IHD-doelen > zonder IHD-doelen)
    #
   IHD_file <-
     find_root_file(
       "data",
       "INBO.A.4862 - Appendix 3 - Overzichtstabel Europees te beschermen soorten.xlsx",#nolint
       criterion = has_file("prioritering-soorten-monitoring.Rproj"))
   IHD <- readxl::read_excel(IHD_file, sheet = "HRL-soorten") %>%
     dplyr::select(`Wet soortnaam`, `G-IHD`, `S-IHD`) %>%
     rbind(readxl::read_excel(IHD_file, sheet = "Vogels") %>%
             dplyr::select(`Wet soortnaam`, `G-IHD`, `S-IHD`)) %>%
     mutate(IHD = (str_detect(`G-IHD`, "[0-9]") |
                     str_detect(`S-IHD`, "[0-9]"))) %>%
     group_by(`Wet soortnaam`) %>% #sommige soorten zoals Ardea alba komen meerdere keren voor in de lijst.
     summarize(IHD = any(IHD)) %>%
     ungroup()

    criteria <- criteria %>%
      left_join(IHD %>%
                  dplyr::select(`Wet soortnaam`, IHD) %>%
                  distinct(),
                by = join_by(naam_wetenschappelijk ==
                               `Wet soortnaam`)) %>%
      mutate(IHD = ifelse(is.na(IHD), FALSE, IHD))

    #Soortenmeetnetten
    SMN <- get_taxonlijsten_items(con,
                                  list = "%Soortenmeetnetten%",
                                  taxon = "%",
                                  feature = "%",
                                  original = FALSE,
                                  collect = TRUE) %>%
      mutate(meetnet = str_detect(Kenmerkwaarde, "Meetnet")) %>%
      dplyr::select(Naamwet_interpretatie,meetnet) %>%
      mutate(type_meetnet = "traditioneel")
    criteria <- criteria %>%
      left_join(SMN,
                by = join_by(naam_wetenschappelijk ==
                               Naamwet_interpretatie)) %>%
      mutate(meetnet = ifelse(is.na(meetnet), FALSE, meetnet))
    return(criteria)
  }

simulate_costs <- function(total_budget, n, prop) {
  #We samplen zodat, gemiddeld gezien, er budget is om een proportie "prop" van
  #meetnetten uit te voeren
  assert_that(prop > 0 & prop <= 1,
              msg = "Kies een prop tussen 0 en 1")
  samples <- rnorm(n,
                   mean = (total_budget * prop / n),
                   sd = (total_budget * prop / 10 / n))
  # Replace negative values with new samples
  while (any(samples < 0)) {
    neg <- samples < 0
    samples[neg] <- rnorm(sum(neg),
                          mean = (total_budget * prop / n),
                          sd = (total_budget * prop / 10 / n))
  }
  return(samples)
}

lees_data <-
  function(
    excel_file =
      find_root_file("data", "oefendata.xlsx",
                     criterion =
                       has_file("prioritering-soorten-monitoring.Rproj"))
  ) {
    soorten <- readxl::read_excel(excel_file, sheet = "soort")
    meetnetten <- readxl::read_excel(excel_file, sheet = "meetnet", skip = 1)
    soort_scores <- readxl::read_excel(excel_file, sheet = "soort_scores",
                                       skip = 1)
    groepen <-  readxl::read_excel(excel_file, sheet = "monitoringgroepen",
                                   skip = 1) %>%
      dplyr::filter(!is.na(groep)) %>%
      mutate(across(starts_with("kost"), function(x) as.numeric(x)))
    if (nrow(soort_scores) != nrow(soorten)) {
      #Als we deze data nog niet hebben opgehaald, doen we dat nu.
      soort_scores <- get_criteria(soorten)
      openxlsx::write.xlsx(soort_scores,
                           file = paste0(str_remove(
                             excel_file, ".xlsx"),
                             "-kopie.xlsx"),
                           sheetName = "soort_scores", startRow = 2)
    }
    if (sum(is.na(meetnetten$kost_indiv)) >= 1) {#als er ontbrekende kosten zijn in de oefendata
      meetnetten[is.na(meetnetten$kost_indiv), ] <-
                                   simulate_costs(params$total_budget,
                                      n = nrow(meetnetten),
                                      prop = 1/4) %>%
        sample(size = sum(is.na(meetnetten$kost_indiv)))
    }
    return(list(soorten, meetnetten, soort_scores, groepen))
  }


library(AHPWR)
library(tidyverse)
library(rprojroot)
# excel_file <-
#   find_root_file("data", "oefendata.xlsx",
#                  criterion =
#                    has_file("prioritering-soorten-monitoring.Rproj"))
forall <- tibble(criterium = c("EB", "RL", "HTS", "IHD", "meetnet"),
       range = c("B9:F14", "B17:E21", "B25:C27", "B30:C32", "B35:C37"))
calculate_priority_vector <- function(criterium = "EB", range = "B9:F14") {
  m <- readxl::read_excel(datafolder, sheet = "criterium_belang",
                          range = range) %>%
    as.matrix()
  rownames(m) <- colnames(m)
  priority_vector <- tibble(
    priority = unlist(calcula_prioridades(list(m))),
    criterium = criterium,
    level = colnames(m)
    )
  return(priority_vector)
}

priorities <- lapply(X = seq_len(nrow(forall)),
          FUN = function(x) {calculate_priority_vector(
            criterium = as.character(forall[x, "criterium"]),
            range = as.character(forall[x, "range"]))}) %>%
  data.table::rbindlist()


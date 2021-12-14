library(revtools)
library(purrr)
library(dplyr)
library(readr)
library(stringr)
library(ggplot2)

# read in the references as a data frame
list.files("REFS", full.names = T) -> all_ref_files

all_ref_files %>% 
  map_dfr(read_bibliography) %>%
  as_tibble() %>% 
  select(id = unique_id, doi, type, author, title, journal, year) ->
  all_refs

# SCREEN 1: remove any duplicates
all_refs %>% unique() -> all_refs_1

# SCREEN 2: remove low-impact journals

# read in journal rankings
read_csv("journal_reports.csv", skip = 1,
         col_types = cols(
           Rank = col_double(),
           `Full Journal Title` = col_character(),
           `Total Cites` = col_integer(),
           `Journal Impact Factor` = col_double(),
           `Eigenfactor Score` = col_double()
         )) %>% 
  select(journal = `Full Journal Title`,
         IF = `Journal Impact Factor`,
         ES = `Eigenfactor Score`) %>% 
  mutate(journal = str_to_lower(journal),
         journal = str_replace_all(journal, "[[:punct:]]", " "),
         journal = str_squish(journal)) -> all_journal_scores

# add impact factors
all_refs_1 %>% 
  select(id, journal) %>% 
  mutate(journal = str_to_lower(journal),
         journal = str_replace_all(journal, "[[:punct:]]", " "),
         journal = str_squish(journal)) %>% 
  left_join(all_journal_scores, by = "journal") ->
  all_refs_with_IF

# screen for IF > 4 and ES > 0.005
all_refs_with_IF %>% 
  filter(ES > 0.005 & IF > 4.0) %>%
  # remove undesired journals by excluding the following strings
  filter(!grepl("geology", journal),
         !grepl("bio", journal),
         !grepl("ecol", journal),
         !grepl("geomatics", journal),
         !grepl("control", journal),
         !grepl("chemical", journal),
         !grepl("mechanics", journal),
         !grepl("rock", journal),
         !grepl("tunnel", journal),
         !grepl("hydrogen", journal),
         !grepl("life cycle", journal),
         !grepl("biological", journal),
         !grepl("pollution", journal),
         !grepl("chemo", journal),
         !grepl("landscape", journal),
         !grepl("landslide", journal),
         !grepl("soft computing", journal),
         !grepl("applied thermal engineering", journal),
         !grepl("aqua", journal),
         !grepl("avian", journal),
         !grepl("comput", journal),
         !grepl("ecolog", journal),
         !grepl("transport", journal),
         !grepl("space", journal),
         !grepl("agricult", journal),
         !grepl("buildings", journal),
         !grepl("safety", journal),
         !grepl("failure", journal),
         !grepl("circuits", journal),
         !grepl("recycling", journal)
         ) -> 
  all_refs_2

# ID keepers from those with < 3 counts
keepers <- c("wiley interdisciplinary reviews water",
             "water research",
             "proceedings of the ieee", "sustainability science",
             "bulletin of the american meteorological society",
             "energy environmental science",
             "global and planetary change",
             "iet renewable power generation",
             "journal of advances in modeling earth systems",
             "nature", "scientific data", "journal of modern power systems and clean energy",
             "mitigation and adaptation strategies for global change",
             "critical reviews in environmental science and technology", "current opinion in environmental sustainability",
             "environmental modelling software", "geophysical research letters", "ieee transactions on sustainable energy",
             "ieee systems journal", "journal of asian earth sciences", "national science review")

all_refs_2 %>% 
  group_by(journal) %>% summarise(count = n()) %>%
  arrange(-count) %>% filter(count<3) %>% 
  filter(!journal %in% keepers) %>% .[["journal"]] -> journals_to_drop

all_refs_2 %>% 
  filter(!journal %in% journals_to_drop) %>% 
  .[["id"]] -> final_cut

all_refs %>% filter(id %in% final_cut) %>%
  filter(year >= 2005) %>% 
  filter(!grepl("case study", title)) %>% 
  filter(!grepl("Case Study", title)) %>% unique() %>% 
  readr::write_csv("hydro_impact_refs.csv")







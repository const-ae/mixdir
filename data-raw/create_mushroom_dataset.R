library(stringr)
library(tidyverse)

data <- read_csv("data-raw/agaricus-lepiota.data.txt")


text <- read_file("data-raw/reference_names.txt")
translator <- str_split(text, "\\d+\\.") %>%
  flatten_chr() %>%
  as.list() %>%
  purrr::discard(~ str_length(.x) == 0) %>%
  map_chr(str_trim) %>%
  tibble(text=.) %>%
  separate(text, sep=":", into=c("label", "values")) %>%
  mutate(values=str_split(values, "\\,")) %>%
  unnest() %>%
  mutate(long=str_match(values, "^\\s*([a-z]+)=[a-z?]\\s*$")[,2],
         short=str_match(values, "^\\s*[a-z]+=([a-z?])\\s*$")[,2]) %>%
  select(-values)


mushroom <- data %>%
  mutate(id=1:n()) %>%
  gather(column, value, -id) %>%
  left_join(translator, by=c("column"="label", "value"="short")) %>%
  select(id, column, long) %>%
  mutate(long=if_else(str_detect(long, "missing"), NA_character_, long)) %>%
  spread(column, long) %>%
  select(- id) %>%
  as.data.frame()


devtools::use_data(mushroom, overwrite=TRUE)


# Helper to generate part of the description of the mushroom dataset.
apply(mushroom, 2, function(col) unique(col)) %>%
  map2_chr(., names(.), ~ paste0("\\item{", .y, "}{",
                                 paste0("\\code{", .x, "}", collapse=" "),
                                 "}")) %>%
  paste0(collapse="\n") %>%
  cat()

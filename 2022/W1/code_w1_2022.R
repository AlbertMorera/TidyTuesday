library(tidyverse)
library(gt)
library(gtExtras)

scripts <- read.csv("C:/Users/Albert/Desktop/TidyTuesday/2022/W1/lotr_scripts.csv", sep = ",")

fellowship <- data %>% tibble() %>% filter(char %in% c("FRODO", "SAM", "PIPPIN", "MERRY", "GANDALF", "ARAGORN", "LEGOLAS", "GIMLI", "BOROMIR"))

fellowship_by_movie <-
  fellowship %>%
  group_by(char, movie) %>%
  tally() %>%
  spread(movie, value=n) %>%
  rename("I"=`The Fellowship of the Ring `, "II"=`The Two Towers `, "III"=`The Return of the King `) %>%
  relocate(char, I, II, III) %>%
  replace(is.na(.), 0) %>%
  rowwise() %>%
  mutate(chart = list(c_across())) %>%
  mutate(char = stringi::stri_trans_totitle(char)) %>%
  select(char, chart)

fellowship_img <- paste0("https://github.com/AlbertMorera/TidyTuesday/blob/main/2022/W1/fellowship/",
                         fellowship_by_movie$char,".jpg?raw=true",sep="")

fellowship_total <-
  fellowship %>%
  group_by(char) %>%
  tally() %>%
  mutate(race = case_when(
    char == "ARAGORN" ~ "Man",
    char == "BOROMIR" ~ "Man",
    char == "GANDALF" ~ "Maia",
    char == "GIMLI" ~ "Dwarf",
    char == "LEGOLAS" ~ "Elf",
    char == "FRODO" ~ "Hobbit",
    char == "SAM" ~ "Hobbit",
    char == "PIPPIN" ~ "Hobbit",
    char == "MERRY" ~ "Hobbit"
  )) %>%
  mutate(char = stringi::stri_trans_totitle(char)) %>%
  mutate(img = fellowship_img) %>%
  relocate(img, char, race, n) %>%
  left_join(fellowship_by_movie)

table <- fellowship_total %>%
  gt() %>%
  gt_img_rows(img, height = 50) %>%
  gt_sparkline(chart,  
               type = "sparkline") %>%
  gtExtras::gt_color_rows(n, 
                          palette = "ggsci::light_green_material", 
                          direction=1,
                          type="continuous") %>%
  cols_label(img = "",
             char = "Character", 
             n = "Nº of dialogs",
             chart = "Dialogs by film") %>%
  tab_header(title = md("The Fellowship of the Ring Dialogs"),
             subtitle= md("Based on the trilogy of The Lord of the Rings")) %>%
  tab_source_note(md("#TidyTuesday week 1 - 2022 | Data from kaggle | Table by @_AlbertMorera")) %>%
  tab_style(
    style = cell_text(
      font = c(
        google_font(name = "IBM Plex Mono")
      ),
      align = "right"
    ),
    locations = cells_source_notes()
  ) %>%
  gt_merge_stack(col1 = char, col2 = race) %>%
  cols_width(
    img ~ px(75),
    char ~ px(250),
    n ~ px(200),
    chart ~ px(200)
  ) %>%
  cols_align(
    align = "center",
    columns = c(n, img)
  )

table
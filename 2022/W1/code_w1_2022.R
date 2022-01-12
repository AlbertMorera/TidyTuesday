
remotes::install_github("jthomasmock/gtExtras")

scripts <- read.csv("C:/Users/Albert/Desktop/TidyTuesday/2022/W1/lotr_scripts.csv", sep = ",")
characters <- read.csv("C:/Users/Albert/Desktop/TidyTuesday/2022/W1/lotr_characters.csv", sep = ",")

scripts %>% tibble()
characters %>% tibble()


data$char %>% unique() %>% sort()

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

fellowship_total <-
  fellowship %>%
  group_by(char) %>%
  tally() %>%
  mutate(race = case_when(
    char == "ARAGORN" ~ "Men",
    char == "BOROMIR" ~ "Men",
    char == "GANDALF" ~ "Maiar",
    char == "GIMLI" ~ "Dwarves",
    char == "LEGOLAS" ~ "Elves",
    char == "FRODO" ~ "Hobbits",
    char == "SAM" ~ "Hobbits",
    char == "PIPPIN" ~ "Hobbits",
    char == "MERRY" ~ "Hobbits"
  )) %>%
  mutate(char = stringi::stri_trans_totitle(char)) %>%
  relocate(char, race, n) %>%
  left_join(fellowship_by_movie)

fellowship_total
  

fellowship_total %>%
  gt::gt() %>%
  gtExtras::gt_color_rows(n, 
                palette = "ggsci::amber_material", 
                direction=1,
                type="continuous") %>%
  gt::cols_label(char = "Character", 
                 n = "Nº of dialogs",
                 chart = "Dialogs by film") %>%
  tab_header(title = md("MY PELOTON INSTRUCTORS 2021"),
             subtitle= md("Personal Workout Summary by Instructor")) %>%
  tab_source_note("Data from Peloton API | Table by @tanya_shapiro") %>%
  gtExtras::gt_sparkline(chart,  
                         type="sparkline") %>%
  gt_merge_stack(col1 = char, col2 = race)





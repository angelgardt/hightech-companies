library(tidyverse)
theme_set(theme_bw())

### Reading -----
files <- dir("db", full.names = TRUE)
length(files)

raw <- tibble()

# readxl::read_xlsx(files[2],
#                   skip = 3) %>% 
#   filter(str_detect(`№`, "^\\d+")) %>% nrow()

for (file in files) {
  print(file)
  readxl::read_xlsx(file,
                    skip = 3) %>% 
    filter(str_detect(`№`, "^\\d+")) %>% 
    bind_rows(raw) -> raw
  print("done")
}

raw %>% colnames()

raw %>% 
  distinct(Наименование, `Регистрационный номер`) %>% nrow()

raw %>% 
  select(-1) %>% 
  distinct() %>% # nrow()
  summarise(n = n(),
            .by = c(Наименование, `Регистрационный номер`)) %>% 
  arrange(desc(n)) %>% 
  filter(n != 1)
  # distinct(Наименование, `Регистрационный номер`) %>% nrow()


raw %>% 
  # filter(`Регистрационный номер` == "1037789063476") %>% View()
  filter(`Регистрационный номер` == "1087746571065") %>% View()

raw$Статус %>% unique()


raw %>% 
  mutate(`ОКВЭД основной` = str_extract(`Коды всех видов деятельности`, "^\\d{2}\\.\\d+\\.\\d+|^\\d{2}\\.\\d+|^\\d{2}")) %>% 
  # select(`ОКВЭД основной`, `Коды всех видов деятельности`) #%>% # sapply(is.na) %>% apply(2, sum)
  # filter(is.na(`Код основного вида деятельности`))
  mutate(`ОКВЭДы дополнительные` = str_remove(`Коды всех видов деятельности`, "^\\d{2}\\.\\d+\\.\\d+\\s|^\\d{2}\\.\\d+\\s|^\\d{2}\\s") %>% 
           str_remove("\\(.+\\),\\s")) %>% 
  # select(`ОКВЭД основной`, `ОКВЭДы дополнительные`, `Коды всех видов деятельности`) %>% View()
  select(-`Коды всех видов деятельности`) %>% 
  filter(Статус == "Действующая") %>% 
  select(-1) %>% 
  # select(-matches("НДПИ")) %>% 
  distinct() %>% # nrow()
  summarise(n = n(),
            .by = c(Наименование, `Регистрационный номер`)) %>% 
  arrange(desc(n)) %>% 
  filter(n != 1) -> doubles

nrow(doubles)

colnames(raw)

raw %>% 
  mutate(nrow = 1:nrow(raw)) %>% 
  right_join(doubles) %>% 
  arrange(`Регистрационный номер`) %>% 
  write_excel_csv("doubles.xlsx")
  # select(nrow, Наименование, `Регистрационный номер`, matches("НДПИ")) %>% View()


## nrows delete ----
# nrows_del <- c(50256,
# 72520,
# 132504,
# 48842,
# 108321,
# 131029,
# 2717,
# 38804,
# 100375,
# 31285,
# 108055,
# 2141,
# 37065,
# 35215,
# 52322,
# 60982,
# 74551,
# 104035,
# 134477,
# 610,
# 31931,
# 154323,
# 31157,
# 48395,
# 70896,
# 139434,
# 1424,
# 34870,
# 118431,
# 143207,
# 33415,
# 59528,
# 86205,
# 98047,
# 55927,
# 93297,
# 96039,
# 100528,
# 106319,
# 115968,
# 148878,
# 119651,
# 136602,
# 37596,
# 76743,
# 963,
# 33240,
# 50581,
# 97960,
# 123589,
# 132757,
# 37414,
# 99742,
# 1143,
# 6841,
# 33943,
# 9967,
# 99757,
# 114186,
# 136258,
# 6955,
# 51369,
# 98373,
# 11431,
# 137594,
# 7679,
# 125193,
# 143208,
# 5492,
# 49877,
# 58751,
# 89318,
# 97621,
# 122904,
# 132167,
# 140824,
# 9921,
# 136163,
# 7723,
# 103896,
# 125242,
# 134256,
# 11868,
# 56158,
# 78379,
# 4634,
# 101927,
# 108557,
# 122048,
# 9825,
# 99695,
# 136087,
# 145451,
# 150389,
# 6329,
# 15705,
# 50754,
# 94674,
# 123755,
# 132916,
# 6101,
# 15490,
# 59335,
# 72762,
# 89681,
# 102862,
# 7516,
# 16837,
# 51923,
# 60591,
# 74110,
# 11457,
# 20550,
# 137617,
# 19275,
# 136392,
# 9715,
# 18961,
# 113915,
# 127270,
# 145361,
# 5586,
# 14925,
# 102519,
# 109547,
# 8047,
# 17347,
# 52421,
# 57807,
# 71023,
# 101628,
# 108090,
# 121620,
# 7225,
# 16596,
# 51646,
# 60289,
# 73767,
# 86434,
# 98516,
# 103566,
# 111254,
# 133818,
# 142680,
# 19743,
# 63337,
# 77163,
# 87454,
# 105686,
# 114846,
# 128134,
# 19167,
# 99738,
# 145634,
# 16844,
# 60599,
# 74117,
# 90619,
# 18772,
# 27366,
# 53820,
# 76070,
# 83235,
# 119288,
# 127032,
# 71805,
# 80417,
# 89124,
# 140471,
# 15680,
# 24489,
# 50723,
# 59527,
# 66679,
# 72954,
# 81165,
# 28586,
# 55153,
# 69065,
# 84111,
# 87518,
# 115126,
# 128400,
# 126955,
# 25964,
# 98864,
# 27703,
# 54230,
# 99729,
# 145589,
# 50440,
# 66510,
# 80962,
# 26696,
# 44496,
# 118940,
# 135225,
# 11236,
# 38793,
# 55558,
# 34983,
# 52129,
# 98752,
# 134303,
# 143314,
# 35236,
# 52334,
# 112032,
# 5181,
# 14505,
# 131794,
# 11960,
# 21020,
# 100684,
# 147768,
# 148565,
# 14198,
# 71660,
# 117394,
# 122353,
# 154265)

## Preprocess ----


raw %>% 
  mutate(nrow = 1:nrow(raw)) %>% 
  slice(-nrows_del) %>% 
  select(-nrow) %>% 
  mutate(`ОКВЭД основной` = str_extract(`Коды всех видов деятельности`, "^\\d{2}\\.\\d+\\.\\d+|^\\d{2}\\.\\d+|^\\d{2}")) %>% 
  mutate(`ОКВЭДы дополнительные` = str_remove(`Коды всех видов деятельности`, "^\\d{2}\\.\\d+\\.\\d+\\s|^\\d{2}\\.\\d+\\s|^\\d{2}\\s") %>% 
           str_remove("\\(.+\\),\\s")) %>% 
  select(-`Коды всех видов деятельности`) %>% #nrow()
  filter(Статус == "Действующая") %>% # nrow()
  select(-1) %>% 
  distinct() -> db

db %>% 
  summarise(n = n(),
            .by = c(`Регистрационный номер`)) %>% 
  filter(n != 1)

db %>% write_excel_csv("database_merged.xlsx")

# db %>% pull(`2023, Выручка, млн. RUB`) %>% `<`(100) %>% sum()


### ANALYSIS -----


library(googlesheets4)
library(tidyverse)
theme_set(theme_bw())
theme_update(legend.position = "bottom")


db <- read_csv("database_merged.xlsx") %>% mutate(`Регистрационный номер` = as.character(`Регистрационный номер`))


nrow(db)

db %>% 
  colnames() %>% 
  str_to_lower() %>% 
  str_remove(",|\\.|\\(|\\)") %>% 
  str_replace("\\s", "_") %>% 
  stringi::stri_trans_general("russian-latin/bgn") -> new_names


db %>% set_names(new_names) -> db
db %>% select(matches("nalog na prib")) %>% sapply(is.na) %>% apply(2, sum)
db %>% select(matches("oplata")) %>% sapply(is.na) %>% apply(2, sum)
# db %>% select(matches("nalog na prib")) %>% sapply(class)
db %>% mutate(`2019_nalog na pribylʹ, mln. rub` = ifelse(is.na(`2019_nalog na pribylʹ, mln. rub...69`), 
                                                         `2019_nalog na pribylʹ, mln. rub...139`,
                                                         ifelse(is.na(`2019_nalog na pribylʹ, mln. rub...139`), 
                                                                `2019_nalog na pribylʹ, mln. rub...69`,
                                                                ifelse(`2019_nalog na pribylʹ, mln. rub...69` >= `2019_nalog na pribylʹ, mln. rub...139`,
                                                                       `2019_nalog na pribylʹ, mln. rub...69`,
                                                                       ifelse(`2019_nalog na pribylʹ, mln. rub...69` < `2019_nalog na pribylʹ, mln. rub...139`,
                                                                              `2019_nalog na pribylʹ, mln. rub...139`, NA)))),
              `2020_nalog na pribylʹ, mln. rub` = ifelse(is.na(`2020_nalog na pribylʹ, mln. rub...70`), 
                                                         `2020_nalog na pribylʹ, mln. rub...140`,
                                                         ifelse(is.na(`2020_nalog na pribylʹ, mln. rub...140`), 
                                                                `2020_nalog na pribylʹ, mln. rub...70`,
                                                                ifelse(`2020_nalog na pribylʹ, mln. rub...70` >= `2020_nalog na pribylʹ, mln. rub...140`,
                                                                       `2020_nalog na pribylʹ, mln. rub...70`,
                                                                       ifelse(`2020_nalog na pribylʹ, mln. rub...70` < `2020_nalog na pribylʹ, mln. rub...140`,
                                                                              `2020_nalog na pribylʹ, mln. rub...140`, NA)))),
              `2021_nalog na pribylʹ, mln. rub` = ifelse(is.na(`2021_nalog na pribylʹ, mln. rub...71`), 
                                                         `2021_nalog na pribylʹ, mln. rub...141`,
                                                         ifelse(is.na(`2021_nalog na pribylʹ, mln. rub...141`), 
                                                                `2021_nalog na pribylʹ, mln. rub...71`,
                                                                ifelse(`2021_nalog na pribylʹ, mln. rub...71` >= `2021_nalog na pribylʹ, mln. rub...141`,
                                                                       `2021_nalog na pribylʹ, mln. rub...71`,
                                                                       ifelse(`2021_nalog na pribylʹ, mln. rub...71` < `2021_nalog na pribylʹ, mln. rub...141`,
                                                                              `2021_nalog na pribylʹ, mln. rub...141`, NA)))),
              `2022_nalog na pribylʹ, mln. rub` = ifelse(is.na(`2022_nalog na pribylʹ, mln. rub...72`), 
                                                         `2022_nalog na pribylʹ, mln. rub...142`,
                                                         ifelse(is.na(`2022_nalog na pribylʹ, mln. rub...142`), 
                                                                `2022_nalog na pribylʹ, mln. rub...72`,
                                                                ifelse(`2022_nalog na pribylʹ, mln. rub...72` >= `2022_nalog na pribylʹ, mln. rub...142`,
                                                                       `2022_nalog na pribylʹ, mln. rub...72`,
                                                                       ifelse(`2022_nalog na pribylʹ, mln. rub...72` < `2022_nalog na pribylʹ, mln. rub...142`,
                                                                              `2022_nalog na pribylʹ, mln. rub...142`, NA)))),
              `2023_nalog na pribylʹ, mln. rub` = ifelse(is.na(`2023_nalog na pribylʹ, mln. rub...73`), 
                                                         `2023_nalog na pribylʹ, mln. rub...143`,
                                                         ifelse(is.na(`2023_nalog na pribylʹ, mln. rub...143`), 
                                                                `2023_nalog na pribylʹ, mln. rub...73`,
                                                                ifelse(`2023_nalog na pribylʹ, mln. rub...73` >= `2023_nalog na pribylʹ, mln. rub...143`,
                                                                       `2023_nalog na pribylʹ, mln. rub...73`,
                                                                       ifelse(`2023_nalog na pribylʹ, mln. rub...73` < `2023_nalog na pribylʹ, mln. rub...143`,
                                                                              `2023_nalog na pribylʹ, mln. rub...143`, NA))))) %>% 
  #select(matches("2020_nalog na prib")) %>% View()
  select(-matches("nalog na pribylʹ, mln. rub...")) %>% 
  mutate(okved_main_class = str_extract(`okv·ed_osnovnoy`, "^\\d{2}")) -> db

googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/13QUvQE6bwxf8P5Ejaijz-LTTmlSH2zOLCYfXe1EXiY8/edit?usp=sharing",
                          sheet = "Классы") -> okved_ht

db %>% filter(!(okved_main_class %in% okved_ht$Класс)) %>% nrow()
db %>% filter(okved_main_class %in% okved_ht$Класс) %>% nrow()
db %>% select(matches("nalog na prib")) %>% sapply(is.na) %>% apply(2, sum)
# db %>% pull(`2022, Налоги, млн. RUB`)
# db %>% pull(`2022, Налог на прибыль, млн. RUB...144`) == db %>% pull(`2022, Налог на прибыль, млн. RUB...74`)
# db %>% pull(`2023, Налог на прибыль, млн. RUB...75`)
# db %>% pull(`2023, Налог на прибыль, млн. RUB...145`)
# db$`ОКВЭД основной`


db %>% filter(okved_main_class %in% okved_ht$Класс) -> db_ht

db_ht %>% 
  summarise(n = n(),
            .by = okved_main_class) %>% 
  mutate(p = round(n / sum(n) * 100, 2)) %>% 
  arrange(desc(n)) %>% 
  # write_sheet("https://docs.google.com/spreadsheets/d/13QUvQE6bwxf8P5Ejaijz-LTTmlSH2zOLCYfXe1EXiY8/edit?usp=sharing",
  #             sheet = "Структура_количество")
  ggplot(aes(fct_reorder(okved_main_class, n), n)) +
  geom_col() +
  geom_label(aes(label = paste(n, "|", p, "%"),
                 y = n + 100)) +
  coord_flip() +
  labs(x = "ОКВЭД (основной)", y = "Количество компаний",
       title = "Структура высокотехнологичного сектора Москвы",
       subtitle = "Количество компаний")

db_ht %>% # colnames()
  summarise(n = n(),
            .by = c(okved_main_class, razmer_kompanii)) %>% 
  mutate(p = n / sum(n),
         razmer_kompanii = factor(razmer_kompanii, ordered = TRUE,
                                  levels = c("Микропредприятия", "Малые предприятия", "Средние предприятия", "Крупные предприятия"))) %>% 
  arrange(desc(n)) %>% 
  ggplot(aes(fct_reorder(okved_main_class, n), n, fill = razmer_kompanii)) +
  geom_col() +
  # geom_label(aes(label = paste(n, "|", round(p * 100, 2), "%"),
  #                y = n + 100)) +
  coord_flip() +
  scale_fill_manual(values = c("Микропредприятия" = "red4", "Малые предприятия" = "green4", "Средние предприятия" = "orange4", "Крупные предприятия" = "blue4")) +
  labs(x = "ОКВЭД (основной)", y = "Количество компаний", fill = "Размер компании",
       title = "Структура высокотехнологичного сектора Москвы",
       subtitle = "Количество и размер компаний")



# db %>% 
#   filter(okved_main_class %in% okved_ht$Класс) %>%
#   group_by(okved_main_class) %>% 
#   # filter(!is.na(`2023, Налог на прибыль, млн. RUB...145`)) %>% 
#   arrange(okved_main_class, desc(`2023_nalog na pribylʹ, mln. rub`)) %>% 
#   slice_max(order_by = `2023_nalog na pribylʹ, mln. rub`,
#             n = 5) %>% 
#   select(naimenovaniye, `okv·ed_osnovnoy`, okved_main_class,
#          `2023_nalog na pribylʹ, mln. rub`,
#          `2022_nalog na pribylʹ, mln. rub`,
#          `2021_nalog na pribylʹ, mln. rub`,
#          `2020_nalog na pribylʹ, mln. rub`) %>% #View()
#   # filter(okved_main_class %in% c(62, 71, 73)) %>% View()
#   googlesheets4::write_sheet("https://docs.google.com/spreadsheets/d/13QUvQE6bwxf8P5Ejaijz-LTTmlSH2zOLCYfXe1EXiY8/edit?usp=sharing",
#                              sheet = "CONPANII")



db_ht %>% 
  group_by(okved_main_class) %>% 
  summarise(nalog_na_pribl = sum(`2023_nalog na pribylʹ, mln. rub`, na.rm = TRUE)) %>% 
  mutate(prop = nalog_na_pribl / sum(nalog_na_pribl)) %>% 
  arrange(desc(nalog_na_pribl)) %>% 
  ggplot(aes(fct_reorder(okved_main_class, nalog_na_pribl), nalog_na_pribl)) +
  geom_col() +
  geom_label(aes(label = paste(nalog_na_pribl, "\n", round(prop * 100, 2), "%"),
                 y = nalog_na_pribl + 1000)) +
  coord_flip()

db_ht %>% 
  arrange(desc(`2022_nalog na pribylʹ, mln. rub`)) %>% 
  select(naimenovaniye, `2023_nalog na pribylʹ, mln. rub`)

db_ht %>% 
  arrange(desc(`2021_nalog na pribylʹ, mln. rub`)) %>% 
  filter(str_detect(naimenovaniye, "МЕДИЦИНСКИЕ УСЛУГИ")) %>% 
  select(naimenovaniye, `2023_nalog na pribylʹ, mln. rub`,
         `2022_nalog na pribylʹ, mln. rub`,
         `2021_nalog na pribylʹ, mln. rub`,
         `2020_nalog na pribylʹ, mln. rub`)

## поправка на 17% в рег бюджет
## картинки с годами рядом
## среднее взвешенное по всем годам

db$`2023_oplata truda, mln. rub`
db %>% filter(str_detect(naimenovaniye, "ТИНЬКОФФ")) %>% 
  select(okved_main_class, `2023_nalog na pribylʹ, mln. rub`)
db %>% filter(str_detect(naimenovaniye, "ТИНЬКОФФ")) %>% 
  select(okved_main_class, `2023_oplata truda, mln. rub`)
db %>% filter(str_detect(naimenovaniye, "СБЕР")) %>% 
  select(naimenovaniye, okved_main_class, `2023_nalog na pribylʹ, mln. rub`)
db %>% filter(str_detect(naimenovaniye, "ОЗОН")) %>% 
  select(naimenovaniye, okved_main_class, `2023_nalog na pribylʹ, mln. rub`)

# db %>% 
#   slice(1:30) %>% write_excel_csv("subset.xlsx")
# slice(1:30) %>% 
#   write.csv2("subset.csv", fileEncoding = "UTF-8")


db_ht %>% nrow()  
db_ht %>% pull(registratsionnyy_nomer) %>% unique() %>% length()
db_ht %>% pull(naimenovaniye) %>% unique() %>% length()
db_ht %>% distinct(registratsionnyy_nomer, naimenovaniye) %>% nrow()
db_ht %>% pull(`2019_srednespisochnaya chislennostʹ rabotnikov`) %>% unique()

db_ht %>% 
  mutate(`2023_ndfl_1, mln. rub.` = `2023_oplata truda, mln. rub` * 0.13,
         `2022_ndfl_1, mln. rub.` = `2022_oplata truda, mln. rub` * 0.13,
         `2021_ndfl_1, mln. rub.` = `2021_oplata truda, mln. rub` * 0.13,
         `2020_ndfl_1, mln. rub.` = `2020_oplata truda, mln. rub` * 0.13,
         `2019_ndfl_1, mln. rub.` = `2019_oplata truda, mln. rub` * 0.13) -> db_ht


db_ht %>% # View()
  select(registratsionnyy_nomer, naimenovaniye, matches("oplata truda"), matches("srednespisochnaya")) %>% # colnames()
  mutate_at(vars(matches("srednespisochnaya")), as.numeric) %>% # View()
  pivot_longer(cols = -c(registratsionnyy_nomer, naimenovaniye)) %>% 
  separate(name, into = c("year", "stat"), sep = "_") %>% 
  pivot_wider(names_from = stat, values_from = value) %>% 
  mutate(srednemes_zp = `oplata truda, mln. rub` / `srednespisochnaya chislennostʹ rabotnikov` / 12) %>% 
  mutate(n_mes = ifelse(srednemes_zp > .35, 0,
                        ifelse(srednemes_zp * 2 > .35, 1,
                               ifelse(srednemes_zp * 3 > .35, 2,
                                      ifelse(srednemes_zp * 4 > .35, 3,
                                             ifelse(srednemes_zp * 5 > .35, 4,
                                                    ifelse(srednemes_zp * 6 > .35, 5,
                                                           ifelse(srednemes_zp * 7 > .35, 6,
                                                                  ifelse(srednemes_zp * 8 > .35, 7,
                                                                         ifelse(srednemes_zp * 9 > .35, 8,
                                                                                ifelse(srednemes_zp * 10 > .35, 9,
                                                                                       ifelse(srednemes_zp * 11 > .35, 10,
                                                                                              ifelse(srednemes_zp * 12 > .35, 11,
                                                                                                     ifelse(srednemes_zp * 13 > .35, 12, NA)))))))))))))) %>% # View()
  mutate(vychet = `srednespisochnaya chislennostʹ rabotnikov` * 0.0014 * n_mes,
         ndfl_2 = (`oplata truda, mln. rub` - vychet) * 0.13) %>% 
  select(registratsionnyy_nomer, naimenovaniye, year, ndfl_2) %>% 
  mutate(name = paste0(year, "_ndfl_2, mln. rub.")) %>% 
  select(-year) %>% 
  pivot_wider(names_from = name, values_from = ndfl_2) %>% 
  full_join(db_ht, join_by(registratsionnyy_nomer, naimenovaniye)) -> db_ht

colnames(db_ht)



hist(db_ht$`2023_ndfl_1, mln. rub.` - db_ht$`2023_ndfl_2, mln. rub.`)


db_ht %>% 
  group_by(okved_main_class) %>% 
  summarise(nalog_na_pribl = sum(`2023_nalog na pribylʹ, mln. rub`, na.rm = TRUE)) %>% 
  mutate(prop = nalog_na_pribl / sum(nalog_na_pribl)) %>% 
  arrange(desc(nalog_na_pribl)) %>% 
  ggplot(aes(fct_reorder(okved_main_class, nalog_na_pribl), nalog_na_pribl)) +
  geom_col() +
  geom_label(aes(label = paste(nalog_na_pribl, "\n", round(prop * 100, 2), "%"),
                 y = nalog_na_pribl + 1000)) +
  coord_flip()

db_ht %>% 
  arrange(desc(`2022_nalog na pribylʹ, mln. rub`)) %>% 
  select(naimenovaniye, `2023_nalog na pribylʹ, mln. rub`)

db_ht %>% 
  arrange(desc(`2021_nalog na pribylʹ, mln. rub`)) %>% 
  filter(str_detect(naimenovaniye, "МЕДИЦИНСКИЕ УСЛУГИ")) %>% 
  select(naimenovaniye, `2023_nalog na pribylʹ, mln. rub`,
         `2022_nalog na pribylʹ, mln. rub`,
         `2021_nalog na pribylʹ, mln. rub`,
         `2020_nalog na pribylʹ, mln. rub`)

## поправка на 17% в рег бюджет
## картинки с годами рядом
## среднее взвешенное по всем годам

db$`2023_oplata truda, mln. rub`
db %>% filter(str_detect(naimenovaniye, "ТИНЬКОФФ")) %>% 
  select(okved_main_class, `2023_nalog na pribylʹ, mln. rub`)
db %>% filter(str_detect(naimenovaniye, "ТИНЬКОФФ")) %>% 
  select(okved_main_class, `2023_oplata truda, mln. rub`)
db %>% filter(str_detect(naimenovaniye, "СБЕР")) %>% 
  select(naimenovaniye, okved_main_class, `2023_nalog na pribylʹ, mln. rub`)
db %>% filter(str_detect(naimenovaniye, "ОЗОН")) %>% 
  select(naimenovaniye, okved_main_class, `2023_nalog na pribylʹ, mln. rub`)

# db %>% 
#   slice(1:30) %>% write_excel_csv("subset.xlsx")
# slice(1:30) %>% 
#   write.csv2("subset.csv", fileEncoding = "UTF-8")


db_ht %>% nrow()  
db_ht %>% pull(registratsionnyy_nomer) %>% unique() %>% length()
db_ht %>% pull(naimenovaniye) %>% unique() %>% length()
db_ht %>% distinct(registratsionnyy_nomer, naimenovaniye) %>% nrow()
db_ht %>% pull(`2019_srednespisochnaya chislennostʹ rabotnikov`) %>% unique()

db_ht %>% 
  mutate(`2023_ndfl_1, mln. rub.` = `2023_oplata truda, mln. rub` * 0.13,
         `2022_ndfl_1, mln. rub.` = `2022_oplata truda, mln. rub` * 0.13,
         `2021_ndfl_1, mln. rub.` = `2021_oplata truda, mln. rub` * 0.13,
         `2020_ndfl_1, mln. rub.` = `2020_oplata truda, mln. rub` * 0.13,
         `2019_ndfl_1, mln. rub.` = `2019_oplata truda, mln. rub` * 0.13) -> db_ht


db_ht %>% # View()
  select(registratsionnyy_nomer, naimenovaniye, matches("oplata truda"), matches("srednespisochnaya")) %>% # colnames()
  mutate_at(vars(matches("srednespisochnaya")), as.numeric) %>% # View()
  pivot_longer(cols = -c(registratsionnyy_nomer, naimenovaniye)) %>% 
  separate(name, into = c("year", "stat"), sep = "_") %>% 
  pivot_wider(names_from = stat, values_from = value) %>% 
  mutate(srednemes_zp = `oplata truda, mln. rub` / `srednespisochnaya chislennostʹ rabotnikov` / 12) %>% 
  mutate(n_mes = ifelse(srednemes_zp > .35, 0,
                        ifelse(srednemes_zp * 2 > .35, 1,
                               ifelse(srednemes_zp * 3 > .35, 2,
                                      ifelse(srednemes_zp * 4 > .35, 3,
                                             ifelse(srednemes_zp * 5 > .35, 4,
                                                    ifelse(srednemes_zp * 6 > .35, 5,
                                                           ifelse(srednemes_zp * 7 > .35, 6,
                                                                  ifelse(srednemes_zp * 8 > .35, 7,
                                                                         ifelse(srednemes_zp * 9 > .35, 8,
                                                                                ifelse(srednemes_zp * 10 > .35, 9,
                                                                                       ifelse(srednemes_zp * 11 > .35, 10,
                                                                                              ifelse(srednemes_zp * 12 > .35, 11,
                                                                                                     ifelse(srednemes_zp * 13 > .35, 12, NA)))))))))))))) %>% # View()
  mutate(vychet = `srednespisochnaya chislennostʹ rabotnikov` * 0.0014 * n_mes,
         ndfl_2 = (`oplata truda, mln. rub` - vychet) * 0.13) %>% 
  select(registratsionnyy_nomer, naimenovaniye, year, ndfl_2) %>% 
  mutate(name = paste0(year, "_ndfl_2, mln. rub.")) %>% 
  select(-year) %>% 
  pivot_wider(names_from = name, values_from = ndfl_2) %>% 
  full_join(db_ht, join_by(registratsionnyy_nomer, naimenovaniye)) -> db_ht

colnames(db_ht)


hist(db_ht$`2023_ndfl_1, mln. rub.` - db_ht$`2023_ndfl_2, mln. rub.`)


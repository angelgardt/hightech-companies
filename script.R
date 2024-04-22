library(tidyverse)
theme_set(theme_bw())

### MERGE ----
d10_32 <- readxl::read_xlsx("СПАРК_Выборка_компаний_20240422_1256_ОКВЭД10-32.xlsx",
                            skip = 3) %>% slice(1:4449)
d33_80 <- readxl::read_xlsx("СПАРК_Выборка_компаний_20240422_1420_ОКВЭД33-80.xlsx",
                            skip = 3) %>% slice(1:8606)

View(d10_32)
nrow(d10_32)
View(d33_80)
nrow(d33_80)

colnames(d10_32) == colnames(d33_80)

d10_32 %>% 
  bind_rows(d33_80) -> db

db %>% 
  summarise(n = n(),
            .by = c(Наименование, `Регистрационный номер`)) %>% 
  arrange(desc(n)) %>% 
  filter(n == 2) -> doubles

d33_80 %>% 
  anti_join(doubles %>% select(-n)) %>% 
  bind_rows(d10_32) -> db

nrow(db)

db %>% write_excel_csv("database_merged.xlsx")

rm(d10_32, d33_80, doubles)


### -----

View(db)

db %>% colnames()

db %>% 
  filter(!is.na(`Дата ликвидации`)) %>% 
  select(`Дата ликвидации`)


db %>% 
  filter(is.na(`Дата ликвидации`)) %>% 
  arrange(desc(`2022, Налоги, млн. RUB`))


db %>% 
  summarise(n = n(),
            .by = `Вид деятельности/отрасль`) %>% 
  arrange(desc(n))

# db %>% pull(`2023, Налоги, млн. RUB`) %>% sum(na.rm = TRUE)
db %>% pull(`2022, Налоги, млн. RUB`) %>% sum(na.rm = TRUE) -> nalogi2022
db %>% pull(`2021, Налоги, млн. RUB`) %>% sum(na.rm = TRUE) -> nalogi2021
db %>% pull(`2020, Налоги, млн. RUB`) %>% sum(na.rm = TRUE) -> nalogi2020
db %>% pull(`2019, Налоги, млн. RUB`) %>% sum(na.rm = TRUE) -> nalogi2019



db %>% 
  filter(is.na(`Дата ликвидации`)) %>% 
  arrange(desc(`2021, Налоги, млн. RUB`))

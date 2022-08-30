# devtools::install_github('galalH/riddle')

library(tidyverse)
library(lubridate)
library(janitor)
library(riddle)

# by gender ---------------------------------------------------------------

download <- RCurl::getURL('https://www.datosabiertos.gob.pa/dataset/142da25e-235e-44ef-ba02-2c23f9bb6c2b/resource/99b0d651-e0b0-4f72-ab97-8a4642ca37c4/download')

df_gender_2022 <- read_csv(download, skip = 1) |> 
  clean_names() |> 
  select_if(~ all(.!='-')) |>  
  pivot_longer(!condicion, names_to = "month", values_to = "value") |> 
  pivot_wider(names_from = condicion , values_from = value) |> 
  filter(month != 'total') |> 
  mutate(
    month_eng = case_when(
      month == "ene" ~ "January",
      month == "feb" ~ "February",
      month == "mar" ~ "March",
      month == "abr" ~ "April",
      month == "may" ~ "May",
      month == "jun" ~ "June",
      month == "jul" ~ "July",
      month == "ago" ~ "August",
      month == "sep" ~ "September",
      month == "oct" ~ "October",
      month == "nov" ~ "November",
      month == "dic" ~ "December",
      TRUE ~ NA_character_
  ),
  year = 2022L) |> 
  select(year, 
         month_eng,
         men = Hombres,
         women = Mujeres,
         total = Total)
  


# by country --------------------------------------------------------------

download <- RCurl::getURL('https://www.datosabiertos.gob.pa/dataset/ebb56d40-112f-455e-9418-ccd73560021d/resource/2979120d-b696-470f-9c8b-8bdbaa63477c/download')

df_country_2022 <- read_csv(download, skip = 1) |> 
  clean_names() |> 
  select_if(~ all(.!='-')) |>  
  rename(country = `pa_s`) |> 
  pivot_longer(!country, names_to = "month", values_to = "value") |> 
  filter(month != 'total',
         country != 'Total') |> 
  mutate(
    month_eng = case_when(
      month == "ene" ~ "January",
      month == "feb" ~ "February",
      month == "mar" ~ "March",
      month == "abr" ~ "April",
      month == "may" ~ "May",
      month == "jun" ~ "June",
      month == "jul" ~ "July",
      month == "ago" ~ "August",
      month == "sep" ~ "September",
      month == "oct" ~ "October",
      month == "nov" ~ "November",
      month == "dic" ~ "December",
      TRUE ~ NA_character_
    ),
    year = 2022L,
    country = gsub('¡','í', country),
    country = gsub('£','ú', country),
    country = trimws(gsub("[[:punct:]]|[[:digit:]]", "", country))) |> 
  select(year, 
         month_eng,
         country,
         value)

# by age ------------------------------------------------------------------


download <- RCurl::getURL('https://www.datosabiertos.gob.pa/dataset/45fa3980-ad6e-4411-8d06-e3af88835ddd/resource/2f75a23c-cd7f-4ca3-96d6-d4142ca66cdc/download')

df_age_2022 <- read_csv(download, skip = 1) |> 
  clean_names() |> 
  select_if(~ all(.!='-')) |>  
  pivot_longer(!condicion, names_to = "month", values_to = "value") |> 
  pivot_wider(names_from = condicion , values_from = value) |> 
  filter(month != 'total') |> 
  mutate(
    month_eng = case_when(
      month == "ene" ~ "January",
      month == "feb" ~ "February",
      month == "mar" ~ "March",
      month == "abr" ~ "April",
      month == "may" ~ "May",
      month == "jun" ~ "June",
      month == "jul" ~ "July",
      month == "ago" ~ "August",
      month == "sep" ~ "September",
      month == "oct" ~ "October",
      month == "nov" ~ "November",
      month == "dic" ~ "December",
      TRUE ~ NA_character_
    ),
    year = 2022L) |> 
  select(year, 
         month_eng,
         adults = Adultos,
         minors = Menores,
         total = Total)

rm(download)



# send to RIDL ------------------------------------------------------------
# usethis::edit_r_environ() and adding the line RIDL_API_KEY=xxxxx
#### 


# gender ------------------------------------------------------------------

write_csv(df_gender_2022, 'df_gender_2022.csv')

p <- package_show('panana-irregular-entries-through-darien-by-gender')

m <- resource_metadata(type = "data",
                       url = "df_gender_2022.csv",
                       upload = httr::upload_file("df_gender_2022.csv"),
                       name = "Irregular entries by gender in 2022",
                       format = "csv",
                       file_type = "microdata",
                       visibility = "public",
                       date_range_start = "2022-01-01",
                       date_range_end = as.character(floor_date(today('America/Panama'), "month") - days(1)), #end day of last month
                       version = "0",
                       process_status = "anonymized",
                       identifiability = "anonymized_public"
)

r <- resource_patch(p$id, m)

# country -----------------------------------------------------------------



write_csv(df_country_2022, 'df_country_2022.csv')

p <- package_show('panana-irregular-entries-through-darien-by-country')

m <- resource_metadata(type = "data",
                       url = "df_country_2022.csv",
                       upload = httr::upload_file("df_country_2022.csv"),
                       name = "Irregular entries by country in 2022",
                       format = "csv",
                       file_type = "microdata",
                       visibility = "public",
                       date_range_start = "2022-01-01",
                       date_range_end = as.character(floor_date(today('America/Panama'), "month") - days(1)), #end day of last month
                       version = "0",
                       process_status = "anonymized",
                       identifiability = "anonymized_public"
                       )

r <- resource_patch(p$id, m)

# age ---------------------------------------------------------------------

write_csv(df_age_2022, 'df_age_2022.csv')

p <- package_show('panana-irregular-entries-through-darien-by-age')

m <- resource_metadata(type = "data",
                       url = "df_age_2022.csv",
                       upload = httr::upload_file("df_age_2022.csv"),
                       name = "Irregular entries by age in 2022",
                       format = "csv",
                       file_type = "microdata",
                       visibility = "public",
                       date_range_start = "2022-01-01",
                       date_range_end = as.character(floor_date(today('America/Panama'), "month") - days(1)), #end day of last month
                       version = "0",
                       process_status = "anonymized",
                       identifiability = "anonymized_public"
)

r <- resource_patch(p$id, m)


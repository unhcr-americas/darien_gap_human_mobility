# send to RIDL ------------------------------------------------------------
# usethis::edit_r_environ() and adding the line RIDL_API_KEY=xxxxx
# rsconnect::writeManifest(appPrimaryDoc = "report.Rmd")


if(!require(devtools)) install.packages("devtools")
if (!require("riddle")) devtools::install_github('galalH/riddle')

library(tidyverse)
library(lubridate)
library(janitor)
library(riddle)
library(unhcrthemes)
library(scales)
library(zoo)

previous_month <- as.character(lubridate::month(lubridate::today() - months(1), label = TRUE, abbr = FALSE))

# check month in riddle ---------------------------------------------------

last_month_not_exist_ridl_age <- resource_fetch('https://ridl.unhcr.org/dataset/a60f4b79-8acc-4893-8fb9-d52f94416b19/resource/daa2b9e4-bf97-4302-86a5-08bb62a5a937/download/df_age_2022.csv') |> 
  read_csv() |> 
  filter(month_eng %in% previous_month) |> 
  nrow() == 0

last_month_not_exist_ridl_gender <- resource_fetch('https://ridl.unhcr.org/dataset/3c1b0da2-ea18-4274-9de6-f5ed1fd53013/resource/e66cfaf1-2434-4c25-8256-93878b1dd21a/download/df_gender_2022.csv') |> 
  read_csv() |> 
  filter(month_eng %in% previous_month) |> 
  nrow() == 0

last_month_not_exist_ridl_country <- resource_fetch('https://ridl.unhcr.org/dataset/5740d920-ba0b-4347-833a-743e8dce3bfe/resource/9c9ab11c-7fd6-4119-87a1-a09ee7f1b5ad/download/df_country_2022.csv') |> 
  read_csv() |> 
  filter(month_eng %in% previous_month) |> 
  nrow() == 0



# by gender ---------------------------------------------------------------

download <- RCurl::getURL('https://www.datosabiertos.gob.pa/dataset/142da25e-235e-44ef-ba02-2c23f9bb6c2b/resource/99b0d651-e0b0-4f72-ab97-8a4642ca37c4/download')

# replace whitespace with delimiter according to delimiter type
download <-
  gsub(pattern = "\\s{5,}", 
       replacement = if (stringr::str_count(download, ";") > stringr::str_count(download, ","))
         ';'
       else ',',
       download
  )

# Fix empty column in between numbers
download <-
  gsub(pattern = "([A-z 0-9]);{2}([A-z 0-9])", 
       replacement = if (stringr::str_count(download, ";") > stringr::str_count(download, ","))
         '\\1;\\2'
       else download,
       download
  )

download <-
  gsub(pattern = "([A-z 0-9]),{2}([A-z 0-9])", 
       replacement = if (stringr::str_count(download, ",") > stringr::str_count(download, ";"))
         '\\1,\\2'
       else download,
       download
  )

# fix thousand separator
download <-
  gsub(pattern = "([0-9 ;]),{1}([; 0-9])", 
       replacement = if (stringr::str_count(download, ";") > stringr::str_count(download, ","))
         '\\1\\2'
       else download,
       download
  )


# read csv with ; or , separator
download <- if (ncol(read_csv(download, skip = 1))<=5) read_csv2(download, skip = 2) else read_csv(download, skip = 2)


df_gender_2022 <- download |> 
  clean_names() |> 
  filter(if_any(everything(), ~ !is.na(.))) |> 
  select_if(~ all(.!='-')) |>  
  pivot_longer(!c(starts_with('con'),starts_with('Con')), names_to = "month", values_to = "value") |> 
  pivot_wider(names_from = c(starts_with('con'),starts_with('Con'))  , values_from = value) |> 
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
  

last_month_exist_datosabiertos_gender <- df_gender_2022 |> 
  filter(month_eng %in% previous_month) |> 
  nrow() != 0


# by country --------------------------------------------------------------

download <- RCurl::getURL('https://www.datosabiertos.gob.pa/dataset/ebb56d40-112f-455e-9418-ccd73560021d/resource/2979120d-b696-470f-9c8b-8bdbaa63477c/download')

# replace whitespace with delimiter according to delimiter type
download <-
  gsub(pattern = "\\s{5,}", 
       replacement = if (stringr::str_count(download, ";") > stringr::str_count(download, ","))
         ';'
       else ',',
       download
  )

# Fix empty column in between numbers
download <-
  gsub(pattern = "([A-z 0-9]);{2}([A-z 0-9])", 
       replacement = if (stringr::str_count(download, ";") > stringr::str_count(download, ","))
         '\\1;\\2'
       else download,
       download
  )

download <-
  gsub(pattern = "([A-z 0-9]),{2}([A-z 0-9])", 
       replacement = if (stringr::str_count(download, ",") > stringr::str_count(download, ";"))
         '\\1,\\2'
       else download,
       download
  )

# fix thousand separator
download <-
  gsub(pattern = "([0-9 ;]),{1}([; 0-9])", 
       replacement = if (stringr::str_count(download, ";") > stringr::str_count(download, ","))
         '\\1\\2'
       else download,
       download
  )

# read csv with ; or , separator
download <- if (ncol(read_csv(download, skip = 1))<=5) read_csv2(download, skip = 1) else read_csv(download, skip = 1)

df_country_2022 <- download |> 
  clean_names() |> 
  filter(if_any(everything(), ~ !is.na(.))) |> 
  select_if(~ all(.!='-')) |>  
  rename(country = starts_with('pa')) |> 
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
    country = gsub('??','??', country),
    country = gsub('??','??', country),
    country = trimws(gsub("[[:punct:]]|[[:digit:]]", "", country))) |> 
  select(year, 
         month_eng,
         country,
         value)

last_month_exist_datosabiertos_country <- df_country_2022 |> 
  filter(month_eng %in% previous_month) |> 
  nrow() != 0


# by age ------------------------------------------------------------------


download <- RCurl::getURL('https://www.datosabiertos.gob.pa/dataset/45fa3980-ad6e-4411-8d06-e3af88835ddd/resource/2f75a23c-cd7f-4ca3-96d6-d4142ca66cdc/download')

# replace whitespace with delimiter according to delimiter type
download <-
  gsub(pattern = "\\s{5,}", 
       replacement = if (stringr::str_count(download, ";") > stringr::str_count(download, ","))
         ';'
       else ',',
       download
  )

# Fix empty column in between numbers
download <-
  gsub(pattern = "([A-z 0-9]);{2}([A-z 0-9])", 
       replacement = if (stringr::str_count(download, ";") > stringr::str_count(download, ","))
         '\\1;\\2'
       else download,
       download
  )

download <-
  gsub(pattern = "([A-z 0-9]),{2}([A-z 0-9])", 
       replacement = if (stringr::str_count(download, ",") > stringr::str_count(download, ";"))
         '\\1,\\2'
       else download,
       download
  )

# fix thousand separator
download <-
  gsub(pattern = "([0-9 ;]),{1}([; 0-9])", 
       replacement = if (stringr::str_count(download, ";") > stringr::str_count(download, ","))
         '\\1\\2'
       else download,
       download
  )


# read csv with ; or , separator
download <- if (ncol(read_csv(download, skip = 1))<=5) read_csv2(download, skip = 1) else read_csv(download, skip = 1)


df_age_2022 <- download |> 
  clean_names() |> 
  filter(if_any(everything(), ~ !is.na(.))) |> 
  select_if(~ all(.!='-')) |>  
  pivot_longer(!c(starts_with('con'),starts_with('Con')), names_to = "month", values_to = "value") |> 
  pivot_wider(names_from = c(starts_with('con'),starts_with('Con'))  , values_from = value) |> 
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

last_month_exist_datosabiertos_age <- df_age_2022 |> 
  filter(month_eng %in% previous_month) |> 
  nrow() != 0



# gender ------------------------------------------------------------------


if (last_month_not_exist_ridl_gender & last_month_exist_datosabiertos_gender) {
  write_csv(df_gender_2022, 'df_gender_2022.csv')
  
  p <- package_show('panama-irregular-entries-through-darien-by-gender')
  
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
  
  r <- resource_create(p$id, m)

}



# country -----------------------------------------------------------------


if (last_month_not_exist_ridl_country & last_month_exist_datosabiertos_country) {
  write_csv(df_country_2022, 'df_country_2022.csv')
  
  p <- package_show('panama-irregular-entries-through-darien-by-country')
  
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
  
  r <- resource_create(p$id, m)
}



# age ---------------------------------------------------------------------


if (last_month_not_exist_ridl_age & last_month_exist_datosabiertos_age) {
  
  write_csv(df_age_2022, 'df_age_2022.csv')
  
  p <- package_show('panama-irregular-entries-through-darien-by-age')
  
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
  
  r <- resource_create(p$id, m)
}




# graph -------------------------------------------------------------------


if (last_month_not_exist_ridl_country & last_month_exist_datosabiertos_country) {

df <- resource_fetch('https://ridl.unhcr.org/dataset/5740d920-ba0b-4347-833a-743e8dce3bfe/resource/308fdead-2b4b-4c10-b28f-6042c5640021/download/df_country_2021.csv') |> 
  read_csv() |> 
  rbind(df_country_2022) %>% 
  group_by(year, month_eng) |>
  mutate(
    top_country_origin = forcats::fct_lump_n(
      f = country,
      n = 2, 
      w = value,
      other_level = 'Other nationalities',
      ties.method = "last"
    )
  ) |> 
  ungroup() |> 
  mutate(top_country_origin = forcats::fct_explicit_na(top_country_origin,
                                                       'Other nationalities')) |> 
  mutate(
    top_country_origin = forcats::fct_rev(forcats::fct_inorder(top_country_origin)),
    top_country_origin =  suppressWarnings(
      dplyr::case_when(
        top_country_origin == "Other nationalities" ~ forcats::fct_relevel(top_country_origin,
                                                                           "Other nationalities",
                                                                           after = Inf),
        TRUE ~ top_country_origin
      )
    ),
    month_eng = factor(month_eng, levels = month.name)
  ) |>
  group_by(top_country_origin, year, month_eng) |>
  summarise(value = sum(value, na.rm = TRUE)) |> 
  ungroup() |> 
  mutate(date = lubridate::dmy(paste0("01-", month_eng, "-",year)))


cols_poptype <- c("Venezuela" = "#044F85",
                  "Hait??" = "#0072BC",
                  "Cuba" = "#589BE5",
                  "Brasil"= '#8EBEFF',
                  "Chile" = "#DCE9FF",
                  "Other nationalities" = "#666666"
)

ggplot(df) +
  geom_col(aes(x = date,
               y = value,
               fill = top_country_origin
  ),
  width = 20) +
  geom_text(aes(x = date,
                y = value,
                color = top_country_origin,
                label = value),
            position = position_stack(vjust = 0.5),
            show.legend = FALSE,
            size = 4) +
  scale_color_manual(values = c("#FFFFFF", "#FFFFFF",
                                "#FFFFFF", "#FFFFFF",
                                "#FFFFFF", "#FFFFFF")) +
  scale_fill_manual(values = cols_poptype) +
  scale_x_date(date_labels = "%b", 
               breaks = pretty_breaks(n = 9),
               expand = c(0, 0)) +
  stat_summary(fun = sum, aes(x = date, y = value, label = scales::label_number( accuracy = 1.1,
                                                                                 scale_cut = scales::cut_short_scale())(..y..), group = date), 
               geom = "text", size = 5,
               vjust = -0.5) +
  facet_grid(.~ year, space = 'free_x', scales = 'free_x', switch = 'x') +
  scale_y_continuous(expand = expansion(c(0, 0.1))) +
  labs(title = "Irregular entries by nationalities | 2021-2022",
       subtitle = "Number of people",
       caption = "Source: SENAFRONT - Panama") +
  theme_unhcr(grid = FALSE,
              axis = "x",
              axis_title = FALSE,
              axis_text = "x") +
  theme(legend.direction = "horizontal",
        legend.key.size = unit(0.8, 'cm'),
        text = element_text(size = 20),
        plot.subtitle=element_text(size=19),
        plot.title = element_text(size=23),
        plot.caption = element_text(size=13),
        strip.placement = 'outside',
        strip.text.x = element_text(size = 20),
        strip.background.x = element_blank(),
        panel.spacing.x = unit(3,"line"))


}
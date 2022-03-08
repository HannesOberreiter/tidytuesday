source("partials/setup.R")
source("functions/functions.R")
library(DBI)
library(lubridate)
here()

con <- dbConnect(RMariaDB::MariaDB(), host = "dedi5270.your-server.de", groups = "server-btree", default.file = ".my.cnf")
dbListTables(con)

# collect will download it
apiary <- tbl(con, "apiary") %>% collect()
hive <- tbl(con, "hive") %>% collect()

count <-
  apiary %>%
  mutate(
    year = lubridate::as_datetime(created) %>% lubridate::year(),
    year = ifelse(year == -1, 2015, year)
  ) %>%
  count(year, name = "Apiaries") %>%
  mutate(Apiaries = cumsum(Apiaries)) %>%
  left_join(hive %>%
    mutate(
      year = lubridate::as_datetime(created) %>% lubridate::year(),
      year = ifelse(year == -1, 2015, year)
    ) %>%
    count(year, name = "Hives") %>%
    mutate(Hives = cumsum(Hives))) %>%
  pivot_longer(-year)

count %>%
  ggplot(aes(x = year, y = value, fill = name)) +
  geom_area(alpha = 0.5, show.legend = FALSE) +
  geom_point(show.legend = FALSE) +
  geom_line() +
  scale_y_continuous(
    breaks = scales::pretty_breaks(n = 6),
    labels = scales::label_number_si()
  ) +
  facet_wrap(~name, scales = "free_y", ncol = 1) +
  xlab("Year") +
  ylab("Count [#]") +
  theme_classic()
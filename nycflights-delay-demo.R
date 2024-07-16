library(nycflights13)
library(labelled)
library(tidyverse)

airlines_labelled <- airlines |>
  set_variable_labels(
    carrier = "Airline two letter carrier abbreviation",
    name = "Airline full name"
  )

airports_labelled <- airports |>
  set_variable_labels(
    faa = "Airport FAA code",
    name = "Airport name",
    lat = "Airport latitude",
    lon = "Airport longitude",
    alt = "Airport altitude (feet)",
    tz = "Airport timezone offset from GMT",
    dst =  "Airport daylight savings time zone",
    tzone = "Airport IANA time zone"
  )

flights_labelled <- flights |>
  set_variable_labels(
    year = "Flight year of departure",
    month = "Flight month of departure",
    day = "Flight day of departure",
    dep_time = "Flight departure time (local tz)",
    arr_time = "Flight arrival time (local tz)",
    sched_dep_time = "Flight scheduled departure time (local tz)",
    sched_arr_time = "Flight scheduled arrival time (local tz)",
    dep_delay = "Flight departure delay (minutes)",
    arr_delay = "Flight arrival delay (minutes)",
    carrier = "Airline two letter carrier abbreviation",
    flight = "Flight number",
    tailnum = "Plane tail number",
    origin = "Flight origin airport",
    dest =  "Flight destination airport",
    air_time = "Flight time spent in air (minutes)",
    distance = "Flight distance betwen airports (miles)",
    hour = "Flight hour of scheduled departure",
    minute = "Flight minute of scheduled departure",
    time_hour = "Flight scheduled departure date/time"
  )


planes_labelled <- planes |>
  labelled::set_variable_labels(
    tailnum = "Plane tail number",
    year = "Plane year manufactured",
    type = "Plane type",
    manufacturer = "Plane manufacturer",
    model = "Plane model",
    engines = "Plane number of engines",
    seats = "Plane number of seats",
    speed  = "Plane average cruising speed (mph)",
    engine = "Plane engine type"
  )

weather_labelled <- weather |>
  labelled::set_variable_labels(
    origin = "Weather station",
    year = "Weather year of recording",
    month = "Weather month of recording",
    day = "Weather day of recording",
    hour = "Weather hour of recording",
    temp = "Weather temperature (F)",
    dewp = "Weather dewpoint",
    humid = "Weather relative humidity",
    wind_dir = "Weather wind direction (degrees)",
    wind_speed = "Weather wind speed (mph)",
    wind_gust = "Weather wind gust speed (mpgh)",
    precip = "Weather precipitation (inches)",
    pressure = "Weather sea level pressure (millibars)",
    visib = "Weather visibility (miles)",
    time_hour = "Weather date/time",
  )


flights_database <- tibble::lst(
  airlines_labelled,
  airports_labelled,
  flights_labelled,
  planes_labelled,
  weather_labelled
)

flights_dictionary <- flights_database |>
  map(labelled::generate_dictionary) |>
  enframe() |>
  unnest(cols = value)


words <- strsplit(flights_dictionary$label, " ") |>
  purrr::flatten_chr() |>
  as_tibble()

words |> count(value, sort = TRUE) |> view()

# year or time possible point of confusion

# from r for data science
flights2 <- flights_labelled |>
  select(year, time_hour, origin, dest, tailnum, carrier)
flights2

flights2 |>
  left_join(planes, join_by(tailnum))

flights_delay_labelled <- flights_labelled |>
  select(origin, dest, arr_delay, dep_delay) |>
  left_join(airports_labelled |> select(faa, name), join_by(origin == faa)) |>
  mutate(
    delay_category = case_when(
      dep_delay < 0 ~ "Early",
      dep_delay == 0 ~ "On time",
      dep_delay %in% 1:30 ~ "1-30 min late",
      dep_delay > 30 ~ "> 30 min late"
    ) |> fct_relevel("Early", "On time", "1-30 min late", "> 30 min late")
  ) |>
  labelled::set_variable_labels(
    delay_category = "Departure delay by origin airport"
  )


flights_delay <- flights |>
  select(origin, dest, arr_delay, dep_delay) |>
  left_join(airports |> select(faa, name), join_by(origin == faa)) |>
  mutate(
    delay_category = case_when(
      dep_delay < 0 ~ "Early",
      dep_delay == 0 ~ "On time",
      dep_delay %in% 1:30 ~ "1-30 min late",
      dep_delay > 30 ~ "> 30 min late"
    ) |> fct_relevel("Early", "On time", "1-30 min late", "> 30 min late")
  )

# Hey Shannon - since Travis is on vacation, can you re-run the
# flight delay report he created last year and walk us through
# the numbers at the team meeting?


flights_delay |>
  select(name, delay_category) |>
  gtsummary::tbl_summary(
    by = name
  )


# is this delay by origin or destination airport?
# is this delay in departure time or arrival time?

1. delay in departure time by origin airport
2. delay in departure time by destination airport
3. delay in arrival time by origin airport
4. delay in arrival time by destination airport


# TidyTuesday 2022, Week 28


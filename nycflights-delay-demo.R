library(nycflights13)
library(labelled)
library(tidyverse)

# ------------------------------------------------------------------------------
# manual way of labelling all versions of all data -----

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


# ------------------------------------------------------------------------------
# list/schema of labelled data ----

flights_schema <- tibble::lst(
  airlines_labelled,
  airports_labelled,
  flights_labelled,
  planes_labelled,
  weather_labelled
)


# dictionary of labelled data ----

flights_dictionary <- flights_schema |>
  map(labelled::generate_dictionary) |>
  enframe() |>
  unnest(cols = value)



# ------------------------------------------------------------------------------
# labelled version of flights delay analysis data ----
flights_delay_labelled <- flights_labelled |>
  select(origin, dest, arr_delay, dep_delay) |>
  left_join(
    airports_labelled |> select(faa, name), 
    join_by(origin == faa)
    ) |>
  mutate(
    delay_category = case_when(
      dep_delay < 0 ~ "Early",
      dep_delay == 0 ~ "On time",
      dep_delay > 0 ~ "Late"
    ) |> fct_relevel("Early", "On time", "Late")
  ) |>
  labelled::set_variable_labels(
    delay_category = "Departure timing by origin airport",
    name = "Origin airport"
  )

# unlabelled version of flights delay analysis data ----
flights_delay <- flights |>
  select(origin, dest, arr_delay, dep_delay) |>
  left_join(
    select(airports, faa, name),
    join_by(origin == faa)
    ) |>
  mutate(
    delay_category = case_when(
      dep_delay < 0 ~ "Early",
      dep_delay == 0 ~ "On time",
      dep_delay > 0 ~ "Late"
    ) |> fct_relevel("Early", "On time", "Late")
  ) 




# ------------------------------------------------------------------------------
# install.packages("devtools")
# devtools::install_github("pcctc/croquet")

# more streamlined/bulk methods for label assignment

# assign labels to a single data frame
flights_labelled <- flights |> 
  croquet::set_derived_variable_labels(
    df_name = "flights",
    path = "nycflights_variables.csv"
  )

# create unlabelled list schema of source data ----
flights_schema_unlabelled <- tibble::lst(
  airlines,
  airports,
  flights,
  planes,
  weather
)

# assign labels to a list of data frames -----
flights_schema_labelled <-
  purrr::imap(
    flights_schema_unlabelled,
    \(x, y) croquet::set_derived_variable_labels(
      data = x,
      df_name = y,
      path = "nycflights_variables.csv"
    )
  )



# create downstream data and assign variable labels ----
flights_delay_labelled <- flights |>
  select(origin, dest, arr_delay, dep_delay) |>
  left_join(
    select(airports, faa, name),
    join_by(origin == faa)
  ) |>
  mutate(
    delay_category = case_when(
      dep_delay < 0 ~ "Early",
      dep_delay == 0 ~ "On time",
      dep_delay > 0 ~ "Late"
    ) |> fct_relevel("Early", "On time", "Late")
  ) |> 
  croquet::set_derived_variable_labels(
    df_name = "flights_delay",
    path = "nycflights_variables.csv"
  )

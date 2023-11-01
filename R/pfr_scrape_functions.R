#' Loads PFR IDs via the csv file located at https://www.pro-football-reference.com/short/inc/players_search_list.csv
#' Hat tip to Tan Ho for hte link.
#' @return a tibble of players names, pfr ids, and active or not binary flag and year started
#'
#' @importFrom readr read_csv col_character col_integer col_logical
#' @importFrom dplyr select
#'
#' @export
#'
#' @examples
#' # load_ids()
load_ids <- function() {
  ids <- readr::read_csv("https://www.pro-football-reference.com/short/inc/players_search_list.csv",
                         col_names = c("pfr_id", "name", "years_played",
                                       "active", "unknown1", "unknown2",
                                       "unknown3", "unknown4", "unknown5"),
                         col_types = list(pfr_id = readr::col_character(),
                                          name = readr::col_character(),
                                          years_played = readr::col_character(),
                                          active = readr::col_integer(),
                                          unknown1 = readr::col_logical(),
                                          unknown2 = readr::col_logical(),
                                          unknown3 = readr::col_logical(),
                                          unknown4 = readr::col_logical(),
                                          unknown5 = readr::col_integer())) |>
    dplyr::select(pfr_id, name, years_played, active)
}

#' Scrape a player's ID
#'
#' Politely scrape a player's ID from their pro-football-reference.com player
#' page URL. Learn how to install RSelenium here: https://www.youtube.com/watch?v=GnpJujF9dBw&t=10s
#'
#' @param merge_name A player name. Omit suffixes and periods for best results.
#'
#' @seealso https://www.sports-reference.com/bot-traffic.html
#'
#' @importFrom RSelenium remoteDriver
#' @importFrom utils URLencode
#' @importFrom cli cli_progress_step cli_progress_done
#' @importFrom stringr str_extract
#'
#' @return a character vector of the player id
#' @export
#'
#' @examples
#' # scrape_player_id("joe montana")
scrape_player_id <- function(merge_name) {

  # check that host is running locally at http://localhost:4445/wd/hub/status
  remDr <- RSelenium::remoteDriver(
    remoteServerAddr = "localhost",
    port = 4445L,
    browserName = "firefox"
  )

  # remDr <- RSelenium::rsDriver(
  #   port = netstat::free_port(),
  #   browser = "firefox",
  #   chromever = NULL
  # )

  remDr$open(silent = TRUE)
  cli::cli_progress_step("Selenium browser loaded")
  pfr_url <- utils::URLencode(glue::glue("https://www.pro-football-reference.com/search/search.fcgi?search={merge_name}"))

  remDr$navigate(pfr_url)
  remDr$getCurrentUrl()
  id <- stringr::str_extract(remDr$getCurrentUrl(), "(?<=/)[^/]+(?=\\.htm$)")
  remDr$close()
  cli::cli_progress_step("{merge_name} scraped with {id} returned")
  Sys.sleep(3) # to ensure we don't hit the 20 page-per-min scrape limit
  cli::cli_process_done()
  return(id)
}

#' Scrape all player IDs
#'
#' Politely scrape the number of players on an index page of pro-football-reference.com
#'
#' @param xml an XML document, typically from rvest::read_html()
#'
#' @importFrom rvest html_elements html_text2
#' @importFrom readr parse_number
#'
#' @noRd
get_max <- function(xml) {
  max <- xml |>
    rvest::html_elements("#players_sh > h2:nth-child(2)") |>
    rvest::html_text2() |>
    readr::parse_number()
}

#' Scrape player name
#'
#' Scrape the name of a football player listed on an index page of
#' pro-football-reference.com
#'
#' @param xml an XML document, typically from rvest::read_html()
#' @param num the nth child of the CSS selector
#'
#' @importFrom rvest html_elements html_text2
#' @importFrom rlang is_empty
#'
#' @noRd
get_name <- function(xml, num) {
  name <- xml |>
    rvest::html_elements(glue::glue("#div_players > p:nth-child({num})")) |>
    rvest::html_text2()
}

#' ID check
#'
#' Checks if the value of a player id scraped from pro-football-reference.com
#' is either NA or NULL.
#'
#' @param id a character vector of a player's ID on pro-football-reference.com
#'
#' @noRd
is_empty_or_null <- function(id) {
  if (is.na(id) || rlang::is_empty(id)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#' Series of CSS selectors and regular expressions to parse the player id from
#' links on pro-football-reference.com player index pages.
#'
#' @param xml an XML document, typically from rvest::read_html()
#' @param num the nth child of the CSS selector
#'
#' @importFrom rvest html_elements html_attr
#' @importFrom stringr str_extract
#'
#' @return a character vector of a player's ID on profootballreference
#' @noRd
get_id <- function(xml, num) {
  # ids will have different selectors depending on if the player is active or
  # retired. Current player names  are bold, for instance. And some are just
  # displayed in non-standard places generally. So we check if regex successfully
  # extracts an id and then let the result fall through if it does.
  id <- xml |>
    rvest::html_elements(glue::glue("#div_players > p:nth-child({num}) > a:nth-child(1)")) |>
    rvest::html_attr("href") |>
    stringr::str_extract("(?<=/)\\w+(?=\\.htm)")

  if (is_empty_or_null(id)) {
    id <- xml |>
      rvest::html_elements(glue::glue("#div_players > p:nth-child({num}) > b:nth-child(1) > a:nth-child(1)")) |>
      rvest::html_attr("href") |>
      stringr::str_extract("(?<=/)\\w+(?=\\.htm)")
  } else {
    return(id)
  }

  if (is_empty_or_null(id)) {
    id <- xml |>
      rvest::html_elements(glue::glue("#div_players > p:nth-child({num}) > b:nth-child(1) > a:nth-child(1)")) |>
      rvest::html_attr("href") |>
      stringr::str_extract("(?<=/)\\w+\\.\\d+(?=\\.htm)")
  } else {
    return(id)
  }

  if (is_empty_or_null(id)) {
    id <- xml |>
      rvest::html_elements(glue::glue("#div_players > p:nth-child({num}) > a:nth-child(1)")) |>
      rvest::html_attr("href") |>
      stringr::str_extract("(?<=/)\\w+\\.\\d+(?=\\.htm)")
  } else {
    return(id)
  }

  if (is_empty_or_null(id)) {
    id <- xml |>
      rvest::html_elements(glue::glue("#div_players > p:nth-child({num}) > a:nth-child(1)")) |>
      rvest::html_attr("href") |>
      stringr::str_extract("(?<=/)[^/]*(?=\\.)")
  } else {
    return(id)
  }

  if (is_empty_or_null(id)) {
    id <- xml |>
      rvest::html_elements(glue::glue("#div_players > p:nth-child({num}) > b:nth-child(1) > a:nth-child(1)")) |>
      rvest::html_attr("href") |>
      stringr::str_extract("(?<=/)[^/]*(?=\\.)")
  } else {
    return(id)
  }
}

#' Scrape names and identifiers of all players whose last name starts with a
#' given letter from pro-football-reference.com
#'
#' @param letter The first letter a player's surname. Must be capitalized.
#'
#' @importFrom purrr pmap
#' @importFrom tidyr crossing
#' @importFrom cli cli_progress_step cli_process_done cli_alert_danger
#' @importFrom glue glue
#'
#' @return a tibble of player names, ids, and the first letter of the player's surname
#' @export
scrape_pfr_player_ids_by_letter <- function(letter) {
  cli::cli_progress_step("Starting PFR scrape of players with surnames beginning with {letter}")
  players_url <- rvest::read_html(glue::glue("https://www.pro-football-reference.com/players/{letter}/"))

  data <- list()

  max <- get_max(players_url)

  data$name <- purrr::pmap(
    tidyr::crossing(
      xml = list(players_url),
      num = 1:max
    ),
    get_name
  )

  data$ids <- purrr::pmap(
    tidyr::crossing(
      xml = list(players_url),
      num = 1:max
    ),
    get_id
  )

  player_ids <- tibble::as_tibble(data) |>
    tidyr::unnest_longer(name) |>
    tidyr::unnest_longer(ids) |>
    dplyr::mutate(letter = letter)

  if (nrow(stats::na.omit(player_ids)) == max) {
    cli::cli_progress_step("Letter {letter} scrape complete")
    cli::cli_process_done("All {max} player ids scraped")
  } else {
    cli::cli_alert_danger("WARNING: Letter {letter} scrape was not complete!")
    diff <- max - nrow(stats::na.omit(player_ids))
    cli::cli_alert_danger("Missing player IDs: {diff}")
  }

  return(player_ids)
}

#' Scrape all index pages on pro-football-reference.com.
#'
#' The scrape is polite: it initiates a total of 26 HTTP GET requests over
#' its life. The majority of the time is spent running regular expression
#' code to extract player IDs from the index pages. It is slow. The function
#' will try and guess the number of cores on the user's system to spawn multiple
#' R sessions, but the max number of sessions it will spawn is capped at 20. The
#' letter "B" will always be the slowest to parse. Expect at least 3 minutes for
#' the scrape to complete.
#'
#' @return a tibble. See the dictionary for pfr_ids for a full listing.
#'
#' @importFrom future multisession
#' @importFrom furrr future_map future_map_dfr
#' @importFrom tidyr separate_wider_delim
#' @importFrom tibble tibble
#'
#' @export
#' @examples
#' # pfr_ids <- get_pfr_ids()
get_pfr_ids <- function() {

  cli::cli_text("The scrape is polite: it initiates a total of 26 HTTP GET requests over
    its life. The majority of the time is spent running regular expression
    code to extract player IDs from the index pages. It is slow. The function
    will try and guess the number of cores on the user's system to spawn multiple
    R sessions, but the max number of sessions it will spawn is capped at 20. The
    letter 'B' will always be the slowest to parse. Expect at least 3 minutes for
    the scrape to complete.")

  # all letters, all CAPS
  letters <- strsplit(intToUtf8(c(65:90)), "")[[1]]
  # check th enumber of cores th euser has. max is 20 to be polite to PFR.
  cores <- tibble::tibble(
    sessions = ifelse(parallel::detectCores() > 20, 20, parallel::detectCores())
  )
  future::plan(future::multisession, workers = cores$sessions)

  pfr_ids <- furrr::future_map(
    letters,
    scrape_pfr_player_ids_by_letter
  ) |>
    furrr::future_map_dfr(dplyr::bind_rows)

  pfr_ids_spread <- pfr_ids |>
    dplyr::mutate(
      name_split =
        stringr::str_trim(
          stringr::str_extract(name, ".*(?=\\()")
        ),
      position = stringr::str_match(name, "\\((.*?)\\)")[, 2],
      seasons = stringr::str_extract(name, "\\d{4}-\\d{4}"),
      merge_name = clean_player_names(name_split, lowercase = TRUE),
      merge_name = stringr::str_replace_all(merge_name, "\\+", "")
    ) |>
    tidyr::separate_wider_delim(
      position,
      delim = "-",
      names_sep = "_",
      too_few = "align_start"
    ) |>
    tidyr::separate_wider_delim(
      seasons,
      delim = "-",
      names = c("first_season", "most_recent_season")
    ) |>
    dplyr::select(-name) |>
    dplyr::rename(name = name_split) |>
    dplyr::select(
      name,
      pfr_id = ids,
      merge_name,
      letter,
      first_season,
      most_recent_season,
      dplyr::everything()
    )
}

#' Get league averages for passing from PFR by season.
#'
#' @return a tibble of data from 1932 to present with league average passingt metrics
#'
#' @importFrom rvest read_html html_table
#' @importFrom glue glue
#' @importFrom dplyr rename filter select
#'
#' @export
#'
#' @examples
#' # get_league_passing_by_year()
get_league_passing_by_year <- function() {
  passing <- rvest::read_html(glue::glue("https://www.pro-football-reference.com/years/NFL/passing.htm"))

  test <- passing |>
    rvest::html_elements("#passing_per_game > tbody:nth-child(4)") |>
    rvest::html_table()

  table <- test[[1]] |>
    dplyr::rename(Rk = X1, Year = X2, Tms = X3, Comps = X4, Atts = X5, Comp_pct = X6,
                  Yards = X7, TDs = X8, TD_rate = X9, Ints = X10, Int_rate = X11,
                  ypa = X12, aya = X13, nya = X20, anya = X21, ypr = X14, ypg = X15,
                  passer_rating = X16, sacks = X17, sack_yards = X18, sack_rate = X19
                  ) |>
    dplyr::filter(Rk != "Rk") |>
    dplyr::select(-Rk) |>
    dplyr::mutate_if(is.character, as.numeric)

}

globalVariables(".GenderQueryCheckCount")
pkg.env <- new.env()


#' Gender of Name API
#'
#' @param first_names
#' The name that you want to know the gender of. Please only use the first name
#' without any spaces.
#'
#' @return
#' A dataframe with one row for every name and 3 columns. The columns tell the
#' inputted name, the gender, and the probability that the gender is accurate.
#'
#' @export
#'
#' @examples
#' genderName("Tom")
#' genderName("Mary")
#' genderName("Sasha")
#'
#' # example of no gender found for the name
#' genderName("")
#'
#' genderName(c("Tom", "Mary", "Sasha"))
#'
#' # It can take a list an input, even a list with bad values (such as "")
#' example <- c("Tom", "Mary", "",  "Sasha")
#' genderName(example)
#'
#' example <- data.frame(name = c("Tom", "Mary", "Sasha"))
#' genderName(example$name)
#'
#' @import jsonlite
#' @import httr
genderName <- function(first_names) {

  first_names <- as.character(first_names)

  api <- GET(paste("https://api.genderize.io/?name=",
                   first_names[1], sep = ""))
  assign(".GenderQueryCheckCount", api$headers$`x-rate-limit-remaining`,
         envir = pkg.env)

  # Check that it worked
  if (http_type(api) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }

  # Error message
  if (http_error(api)) {
    stop(
      sprintf(
        "API request failed"
      ),
      call. = FALSE
    )
  }

  if (length(jsonlite::fromJSON(content(api, "text"),
                                simplifyVector = FALSE)) > 2) {

    gender_list <- data.frame(jsonlite::fromJSON(content(api, "text"),
                                                 simplifyVector = FALSE)[1:3])
  }

  if (length(jsonlite::fromJSON(content(api, "text"),
                                     simplifyVector = FALSE)) <= 2) {
    temp_row <- data.frame(name = first_names[1],
                           gender = NA,
                           probability = NA)
    gender_list <- temp_row
  }

  if (length(first_names) > 1) {
    for (i in 2:length(first_names)) {
      api <- GET(paste("https://api.genderize.io/?name=",
                       first_names[i], sep = ""))

      assign(".GenderQueryCheckCount", api$headers$`x-rate-limit-remaining`,
             envir = pkg.env)

      if (http_type(api) != "application/json") {
        stop("API did not return json", call. = FALSE)
      }

      # Error message
      if (http_error(api)) {
        stop(
          sprintf(
            "API request failed"
          ),
          call. = FALSE
        )
      }

      if (length(jsonlite::fromJSON(content(api, "text"),
                                    simplifyVector = FALSE)) > 2) {

        gender <- data.frame(jsonlite::fromJSON(content(api, "text"),
                                                simplifyVector = FALSE)[1:3])
      }

      else if ((length(jsonlite::fromJSON(content(api, "text"),
                                          simplifyVector = FALSE)) <= 2)) {
        temp_row <- data.frame(name = first_names[i],
                               gender = NA, probability = NA)
        gender <- temp_row
      }

      gender_list <- rbind(gender_list, gender)
      }
  }


  return(gender_list)

}

#' Gender Name Query Check
#'
#' @return
#' Message that says how many queries are remaining today. The limit
#' is 1000 queries in a 24-hour period.
#'
#' @export
#'
#' @examples
#'
#' # Gives number of checks remaining in the day
#' genderNameQueryCheck()
#'
#' # If you run genderName again, it increments the number of
#' # available checks down by 1.
#' genderName("tom")
#' genderNameQueryCheck()
#'
#' @import jsonlite
#' @import httr
genderNameQueryCheck <- function() {


  if (exists(".GenderQueryCheckCount", envir = pkg.env)) {
    message(get(".GenderQueryCheckCount", envir = pkg.env),
            " gender queries remaining today.")
  }

  else {
    api <- GET("https://api.genderize.io/?name=")
    assign(".GenderQueryCheckCount", api$headers$`x-rate-limit-remaining`,
           envir = pkg.env)
    message(get(".GenderQueryCheckCount", envir = pkg.env),
            " gender queries remaining today.")
  }
}


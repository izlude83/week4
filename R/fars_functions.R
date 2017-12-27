globalVariables(c("MONTH", "STATE", "year", "n"))
#' fars_read
#'
#' This function takes in "filename" as an argument, and first checks whether
#' any file with name equal to "filename" exists.
#'
#' If the file does not exists, fars_read will stop and returns a message to
#' indicate that the file does not exist.
#'
#' If the file exists, fars_read will to read the file using the read_csv function
#' from the readr package into a table named "data". It will then run the tbl_df function
#' from the dplyr package to convert data into a data frame and return it as output
#' to the function.
#'
#' @param filename A character string denoting the file to be read by fars_read.
#'
#' @return This function either returns a data frame containing data extracted from the
#' defined file if it is present, or a message indicating the file is not found otherwise.
#'
#' @examples
#' fars_read(make_filename (2015))
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @export
fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}

#' make_filename
#'
#' This function takes in a argument "year", coerces it into an integer,
#' and returns a character string of the form "accident_"year".csv.bz2".
#'
#' @param year A value to be incorporated into the file name to be created.
#'
#' @return This function returns a character string of the form "accident_"year".csv.bz2".
#'
#' @examples
#' make_filename (2015)
#'
#' @export
make_filename <- function(year) {
        year <- as.integer(year)
        system.file("extdata", sprintf("accident_%d.csv.bz2", year), package="week4")
}

#' fars_read_years
#'
#' This function takes in an argument "years" containing a list of year values, then
#' creates a vector file containing the names of the accident files for these year values.
#' It then calls on the fars_read function to read each of these files into a data frame,
#' create a new column called year using the mutate function from dplyr which would
#' be populated with the appropriate year value, and return the month and year columns
#' from each of these data frames as a list object. If there is no file corresponding to
#' any of the year values, a NULL is returned instead of a data frame comprising the
#' month and year columns.
#'
#' @param years A list of year values.
#'
#' @return This function returns a list of data frames each containing 2 columns MONTH and
#' year, or NULL (if an error is encountered trying to read the file of the corresponding
#' year.)
#'
#' @examples
#' dat_list <- fars_read_years(c(2013, 2014))
#'
#' @importFrom dplyr mutate select
#'
#' @export
fars_read_years <- function(years) {
        lapply(years, function(year) {
                file <- make_filename(year)
                tryCatch({
                        dat <- fars_read(file)
                        dplyr::mutate(dat, year = year) %>%
                                dplyr::select(MONTH, year)
                }, error = function(e) {
                        warning("invalid year: ", year)
                        return(NULL)
                })
        })
}

#' fars_summarize_years
#'
#' This function takes in an argument "years" containing a list of year values, and calls
#' the function fars_read_years to generate a list of data frames containing the MONTH
#' and year values of the accident files corresponding to these years. It then calls the
#' bind_rows function from dplyr package to combine these data frames into a single data
#' frame, then returns the count for each of the twelve months of each of the years
#' specified under "years".
#'
#' @param years A list of year values.
#'
#' @return This function returns a data frame containing the number of observations
#' for each month under the accident data files of each of the corresponding years.
#'
#' @examples
#' dat_sum <- fars_summarize_years (c(2013, 2014))
#'
#' @importFrom dplyr bind_rows group_by summarize
#' @importFrom tidyr spread
#'
#' @export
fars_summarize_years <- function(years) {
                dat_list <- fars_read_years(years)
                dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(year, MONTH) %>%
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}

#' fars_map_state
#'
#' This function takes in a state number ("state.num") and year ("year") as arguments.
#' It reads the corresponding accident data file per the "year" argument, extract data
#' corresponding to the specified state, then uses the map function from maps package to
#' provide a map plot showing the locations corresponding to the accident data for the
#' specified state in the specified year. The function will stop if the state number
#' entered is invalid, and will also not provide any plot if there are no accidents for the
#' specified state in the given year.
#'
#' @param state.num Integer (1 to 56) corresponding to the state of interest.
#' @param year Integer (e.g. 2013) denoting the year for which the map plot is desired.
#'
#' @return This function returns a map plot showing the location of accidents for the
#' specified state in the specified year.
#'
#' @examples
#' fars_map_state (20, 2013)
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @export
fars_map_state <- function(state.num, year) {
        filename <- make_filename(year)
        data <- fars_read(filename)
        state.num <- as.integer(state.num)

        if(!(state.num %in% unique(data$STATE)))
                stop("invalid STATE number: ", state.num)
        data.sub <- dplyr::filter(data, STATE == state.num)
        if(nrow(data.sub) == 0L) {
                message("no accidents to plot")
                return(invisible(NULL))
        }
        is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
        is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
        with(data.sub, {
                maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
                          xlim = range(LONGITUD, na.rm = TRUE))
                graphics::points(LONGITUD, LATITUDE, pch = 46)
        })
}

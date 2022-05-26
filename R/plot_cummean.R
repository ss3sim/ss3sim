#' Plot the cumulative mean for a parameter
#'
#' @param data A valid data frame containing scalar or time series values
#'  from a \pkg{ss3sim} simulation. That data are generated from
#'  [get_results_all()].
#' @param var The column name of the parameter in data of which to plot
#'  cumulative mean. A string.
#' @param order_var A column to order the data before calculating the cumulative
#'  mean.
#' @param group A column in data to group the data together before calculating
#'  the cumulative mean.
#' @param use_facet Should the group be used to create facets? If `TRUE`, facets
#'  are created; If `FALSE`, grouping will be done by making different color lines
#'  in the same plot.
#' @importFrom rlang .data
#' @export
#' @return A list containing the {ggplot2} object and the data used to make it.
#' @examples
#' data("scalar_dat", package = "ss3sim")
#' obj <- plot_cummean(scalar_dat[scalar_dat$model_run == "em", ],
#'   "VonBert_K_Fem_GP_1",
#'   group = "scenario",
#'   use_facet = TRUE
#' )
#' # obj$plot
#' # obj$data
#' rm(obj)
#' # group can also be left NULL if only plotting a single scenario.
#' # it is recommended to set use_facet FALSE in this case.
#' obj2 <- plot_cummean(scalar_dat[
#'   scalar_dat$scenario == unique(scalar_dat$scenario)[1] &
#'     scalar_dat$model_run == "em",
#' ],
#' var = "VonBert_K_Fem_GP_1",
#' group = NULL,
#' use_facet = FALSE
#' )
#' # obj2$plot
#' # obj2$data
#' rm(obj2)
plot_cummean <- function(data, var, order_var = "iteration", group = NULL,
                         use_facet = FALSE) {
  # Manipulate the data
  data <- data[, c(order_var, group, var)] # select cols
  data <- data[order(data[, order_var]), ] # arrange
  # group and calculate cumsum
  if (!is.null(group)) {
    group_vals <- unique(data[, group])
  } else {
    group <- "dummy_val"
    data$dummy_val <- rep("group", times = nrow(data))
    group_vals <- "group"
  }
  data_list <- lapply(group_vals, function(val, data, group, var) {
    tmp_dat <- data[data[, group] == val, ]
    cum_mean_col <- cumsum(tmp_dat[, var]) / seq_along(tmp_dat[, var])
    tmp_dat$cummean <- cum_mean_col
    tmp_dat
  }, data = data, group = group, var = var)
  new_data <- do.call("rbind", data_list)
  g <- ggplot2::ggplot(
    data = new_data,
    ggplot2::aes(x = .data[[order_var]], y = .data$cummean)
  )
  if (use_facet) {
    g <- g +
      ggplot2::geom_line() +
      ggplot2::geom_point() +
      ggplot2::facet_wrap(group)
  } else {
    g <- g +
      ggplot2::geom_line(ggplot2::aes(color = .data[[group]])) +
      ggplot2::geom_point(ggplot2::aes(color = .data[[group]])) +
      ggplot2::theme(
        legend.position =
          ifelse(length(unique(new_data[, group])) == 1, "none", "right")
      )
  }
  return_list <- list(plot = g, data = new_data)
}

#' Shear and moment diagrams
#' @description Plot the shear and bending moment diagrams of a beam
#' @param .beam_length Longitude of the beam in meters
#' @param .loads A tibble with punctual forces, it must have two columns (x and force).
#' `x` is the position of the punctual force on the beam from the left edge.
#' `force` is the magnitude of the force; positive forces going up, and negative
#' forces going down. Support reactions should also be added in this data frame.
#' @param .resolution The beam is discretised for the calculations, by default
#' by steps of 0.01 m (but it can be adjusted if necessary).
#' @return A ggplot2 object with the loads, shear diagram, and the bending moment.
#' @export
#' @examples
#' library(ggplot2)
#' library(patchwork)
#'
#' loads <- tibble::tribble( ~x, ~force,
#'                           0.025,  50,
#'                           0.325, -50,
#'                           0.500, -50,
#'                           0.800,  50)
#'
#' plot_internal_forces(.beam_length = 0.825, .resolution = 0.0001, .loads = loads)

plot_internal_forces <- function(.beam_length,
                                 .loads,
                                 .resolution = 0.01) {

  # Create points (x) for the internal forces (steps = .resolution)
  df <- tibble::tibble(x = seq(0, .beam_length, .resolution)) |>
    # Add point force location (.loads)
    dplyr::left_join(.loads) |>
    dplyr::mutate(force = ifelse(is.na(force), 0, force)) |>
    # Shear forces
    dplyr::mutate(shear = cumsum(force)) |>
    # Bending moment
    dplyr::mutate(moment = cumsum(shear))

  # Punctual forces
  p_loads <- ggplot() +
    # Beam
    geom_segment(aes(x = 0, y = 0, xend = .beam_length, yend = 0),
                 linewidth = 4, colour = "darkgrey") +
    # Puntual forces
    geom_segment(data = .loads,
                 aes(x = x, y = force, xend = x, yend = 0),
                 arrow = arrow(type = "closed", length = unit(0.1, "inches")),
                 colour = "blue",
                 linewidth = 0.75) +
    scale_y_reverse() +
    labs(x = "Position [m]",
         y = "F [N]", title = "Load forces") +
    theme_minimal()

  # Plot shear forces
  p_shear <- ggplot() +
    geom_hline(yintercept = 0) +
    geom_line(data = df,
              aes(x = x, y = shear),
              colour = "red",
              linewidth = 0.75) +
    labs(x = "Position [m]",
         y = "F [N]",
         title = "Shear Force Diagram") +
    theme_minimal()

  # Plot moment
  p_moment <- ggplot() +
    geom_hline(yintercept = 0) +
    geom_line(data = df,
              aes(x = x, y = moment),
              colour = "red",
              linewidth = 0.75) +
    labs(x = "Position [m]",
         y = "M [Nmm]",
         title = "Moment Diagram") +
    theme_minimal()

  # Diagrams
  plot_diagrams <- p_loads / p_shear / p_moment

  return(plot_diagrams)

}

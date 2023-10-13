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
    # Area of the shear
    dplyr::mutate(area_shear = .resolution * shear)

  # Bending moment
  M <- matrix(0, length(df$x))
  for (i in 1:(length(df$x)-1) ) {
    M[i+1, ] = M[i, ] + df[i,]$area_shear
  }
  M <- as.data.frame(M)
  names(M) <- c("moment")
  # Add to df
  df <- df |> dplyr::mutate(moment = M$moment)

  # Punctual forces
  force <- ggplot() +
    # Beam
    geom_segment(aes(x = 0, y = 0, xend = .beam_length, yend = 0),
                 linewidth = 4, colour = "darkgrey") +
    # Puntual forces
    geom_segment(data = loads,
                 aes(x = x, y = 0, xend = x, yend = force),
                 arrow = arrow(type = "closed", length = unit(0.2, "inches")),
                 colour = "blue", linewidth = 1) +
    labs(x = "Position [m]",
         y = "F [N]", title = "Load forces") +
    theme_minimal()

  # Plot shear forces
  shear <- ggplot(df, aes(x = x, y = shear)) +
    geom_hline(yintercept = 0) +
    geom_line(colour = "red", linewidth = 2) +
    labs(x = "Position [m]",
         y = "F [N]",
         title = "Shear Force Diagram") +
    theme_minimal()

  # Plot moment
  moment <- ggplot(df, aes(x = x, y = moment)) +
    geom_hline(yintercept = 0) +
    geom_line(colour = "red", linewidth = 2) +
    labs(x = "Position [m]",
         y = "M [Nmm]",
         title = "Moment Diagram") +
    theme_minimal()

  # Diagrams
  diagram_plot <- force / shear / moment

  return(diagram_plot)

}

#' Shear and moment diagrams
#' @description Plot the shear and bending moment diagrams of a beam
#' @param .beam_length Longitude of the beam in meters
#' @param .point_force A tibble with punctual forces, it must have two columns (x and force).
#' `x` is the position of the punctual force on the beam from the left edge [m].
#' `force` is the magnitude of the force [N]; positive forces going up, and negative
#' forces going down. Support reactions should also be added in this data frame.
#' @param .distributed_load A tibble with the distributed load [N/m]. It must have two columns
#' (x and load), with the load values in steps of x = `.resolution`.
#' @param .resolution The beam is discretised for the calculations, by default
#' by steps of 0.001 m (but it can be adjusted if necessary).
#' @return A ggplot2 object with the loads, shear diagram, and the bending moment.
#' @export
#' @examples
#' library(ggplot2)
#' library(patchwork)
#'
#' # Problem definition
#'
#' beam_length = 6
#'
#' distributed_load <- tibble::tibble(x = seq(3, beam_length, 0.01)) |>
#'   mutate(load = -(200/3)*x)
#'
#' point_force <- tibble::tribble( ~x, ~force,
#'                                 0,  200,
#'                                 6,  700)
#'
#' # Diagrams
#' plot_internal_forces(.beam_length = beam_length,
#'                      .point_force = point_force,
#'                      .distributed_load = distributed_load
#' )

plot_internal_forces <- function(.beam_length,
                                .point_force,
                                .distributed_load = NULL,
                                .resolution = 0.01) {

  # Create points (x) for the internal forces (steps = .resolution)
  df <- tibble::tibble(x = seq(0, .beam_length, .resolution)) |>
    # Add point force location (.point_loads)
    dplyr::left_join(.point_force) |>
    dplyr::mutate(force = replace_na(force, 0))

  # Only punctual forces

  if(is.null(.distributed_load)) {

    df <- df |>
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

    # Plots ----
    # Punctual forces
    p_loads <- ggplot() +
      # Beam
      geom_segment(data = df,
                   aes(x = 0, y = 0, xend = max(x), yend = 0),
                   linewidth = 4, colour = "darkgrey") +
      # Punctual forces
      geom_segment(data = .point_force,
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
           y = "M [N*m]",
           title = "Moment Diagram") +
      theme_minimal()

    # Diagrams
    plot_diagrams <- p_loads / p_shear / p_moment

  } else {

    df <- df |>
      # Convert x fo factor for the join (problems with floating numbers)
      dplyr::mutate(x = as.factor(x)) |>
      dplyr::left_join(.distributed_load |> mutate(x = as.factor(x)), by = "x") |>
      dplyr::mutate(load = replace_na(load, 0)) |>
      # Get cummulative area of the load
      dplyr::mutate(area_load = .resolution * load,
                    cum_area_load = cumsum(area_load)) |>
      # Convert x again to number
      dplyr::mutate(x = as.numeric(as.character(x)))

    # Add shear due to point load
    df <- df |>
      dplyr::mutate(p_shear = cumsum(force))

    # Shear due to Distributed load
    DL <- matrix(0, length(df$x))
    for (i in 1:(length(df$x)) ) {
      DL[i, ] = DL[1, ] + df[i,]$cum_area_load
    }
    DL <- as.data.frame(DL)
    names(DL) <- c("shear")

    # Total shear force
    df <- df |> dplyr::mutate(dl_shear = DL$shear) |>
      mutate(shear = p_shear + dl_shear) |>
      select(-p_shear, dl_shear)

    # Bending moment ----
    df <- df |>
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

    # Plots ----
    # Punctual forces
    p_loads <- ggplot() +
      # Distribute loads
      geom_segment(data = .distributed_load,
                   aes(x = x, y = load, xend = x, yend = 0),
                   colour = "red",
                   linewidth = 0.75) +
      # Beam
      geom_segment(data = df,
                   aes(x = 0, y = 0, xend = max(x), yend = 0),
                   linewidth = 2,
                   colour = "darkgrey") +
      # Punctual forces
      geom_segment(data = .point_force,
                   aes(x = x, y = force, xend = x, yend = 0),
                   arrow = arrow(type = "closed", length = unit(0.1, "inches")),
                   colour = "blue",
                   linewidth = 0.75) +
      scale_y_reverse() +
      labs(x = "Position [m]",
           y = "F [N]", title = "Load forces") +
      theme_light()

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
      theme_light()

    # Plot moment
    p_moment <- ggplot() +
      geom_hline(yintercept = 0) +
      geom_line(data = df,
                aes(x = x, y = moment),
                colour = "red",
                linewidth = 0.75) +
      labs(x = "Position [m]",
           y = "M [N*m]",
           title = "Moment Diagram") +
      theme_light()

    # Diagrams
    plot_diagrams <- p_loads / p_shear / p_moment

  }

  return(plot_diagrams)

}

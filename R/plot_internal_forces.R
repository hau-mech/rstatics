#' Shear and moment diagrams
#' @description Plot the shear and bending moment diagrams of a beam
#' @param .beam_length Longitude of the beam in meters
#' @param .point_force A tibble with punctual forces, it must have two columns (x and force).
#' `x` is the position of the punctual force on the beam from the left edge [m].
#' `force` is the magnitude of the force [N]; positive forces going up, and negative
#' forces going down. Support reactions should also be added in this data frame.
#' @param .point_moment A tibble with punctual moments, it must have two columns (x and moment).
#' `x` is the position of the punctual moment on the beam from the left edge [m].
#' `moment` is the magnitude of the moment [N*m]; positive moment counter-clockwise rotation,
#' and negative moment clockwise rotation. Support moments (e.g. fixed support) should also be added in this data frame.
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
#' distributed_load <- tibble::tibble(x = seq(3, beam_length, 0.001)) |>
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
                                 .point_moment = NULL,
                                 .distributed_load = NULL,
                                 .resolution = 0.001) {

  # Create points (x) for the internal forces (steps = .resolution)
  df <- tibble::tibble(x = seq(0, .beam_length, .resolution))

  # Shear forces ---
  df <- df |>
    # Add point force location (.point_loads)
    dplyr::left_join(.point_force) |>
    dplyr::mutate(force = replace_na(force, 0)) |>
    # Add distributed load
    # Convert x fo factor for the join (problems with floating numbers)
    dplyr::mutate(x = as.factor(x)) |>
    dplyr::left_join(mutate(.distributed_load, x = as.factor(x)), by = "x") |>
    dplyr::mutate(load = replace_na(load, 0),
                  # Discrete total load (dis_force)
                  load_force = .resolution * load,
                  # Get cummulative area of the load
                  total_disc_force = force + load_force,
                  # Shear
                  shear = cumsum(total_disc_force)) |>
    # Convert x again to number
    dplyr::mutate(x = as.numeric(as.character(x))) |>
    # Area of the shear
    dplyr::mutate(area_shear = .resolution * shear)

  # Bending moments ----
  if(is.null(.point_moment)) {

    df <- df |>
      # Discrete moment
      dplyr::mutate(moment = cumsum(area_shear))

  } else {

    df <- df |>
      # Add point moments (.point_moment)
      dplyr::left_join(.point_moment) |>
      dplyr::rename(p_moment = moment) |>
      dplyr::mutate(
        # Punctual moment
        p_moment = replace_na(p_moment, 0),
        # Discrete moment
        d_moment = area_shear - p_moment,
        # Moment at x
        moment = cumsum(d_moment))
  }

  # Plots ----

  p_loads <- ggplot() +
    # Beam
    geom_segment(data = df,
                 aes(x = 0, y = 0, xend = max(x), yend = 0),
                 linewidth = 4, colour = "darkgrey") +
    # Punctual forces
    geom_segment(data = .point_force,
                 aes(x = x, y = force, xend = x, yend = 0),
                 arrow = arrow(type = "closed", length = unit(0.08, "inches")),
                 colour = "black",
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

  # Internal forces diagrams

  if(is.null(.distributed_load)) {

    # Diagrams Only punctual forces
    plot_diagrams <- p_loads / p_shear / p_moment


  } else {

    p_loads <- p_loads +
      # Add distribute loads
      geom_segment(data = .distributed_load,
                   aes(x = x, y = load, xend = x, yend = 0),
                   colour = "red",
                   linewidth = 0.01)

    # # With distributed load
    # plot_diagrams <- p_loads / p_shear / p_moment

  }

  if(is.null(.point_moment)){

    # With distributed load
    plot_diagrams <- p_loads / p_shear / p_moment

  } else {

    df2 <- .point_moment |>
      mutate(curvature = ifelse(moment > 0, 0.6, -0.6))
    p_loads <- p_loads +
      lapply(split(df2, 1:nrow(df2)), function(dat) {
        geom_curve(data = dat,
                   aes(x = x,
                       xend = x,
                       y = ifelse(moment > 0,
                                  0.5 * max(.point_force$force),
                                  -0.5 * max(.point_force$force)),
                       yend = ifelse(moment > 0,
                                     -0.5 * max(.point_force$force),
                                     0.5 * max(.point_force$force))
                   ),
                   arrow = arrow(length = unit(0.08, "npc"), type = "closed"),
                   colour = "black",
                   linewidth = 0.5,
                   angle = 90,
                   curvature = dat["curvature"])
        }
      )

    # With point moments
    plot_diagrams <- p_loads / p_shear / p_moment

  }

  return(plot_diagrams)

}


save_count_plot <- function(data, category_col, count_col, title, path, fill, flip = TRUE) {
  plot_data <- data
  plot_data[[category_col]] <- factor(plot_data[[category_col]], levels = plot_data[[category_col]])
  max_count <- max(plot_data[[count_col]], na.rm = TRUE)
  label_padding <- max_count * 0.03 + 0.2
  axis_limit <- max_count + label_padding + 0.2
  break_by <- if (max_count <= 6) 1 else 2
  count_breaks <- seq(0, ceiling(axis_limit), by = break_by)

  plot <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = .data[[category_col]], y = .data[[count_col]])
  ) +
    ggplot2::geom_col(fill = fill, width = 0.75) +
    ggplot2::geom_text(
      ggplot2::aes(
        y = .data[[count_col]] + label_padding,
        label = .data[[count_col]]
      ),
      hjust = 0.5,
      vjust = if (flip) 0.5 else 0,
      size = 4.3
    ) +
    ggplot2::labs(
      title = title,
      x = NULL,
      y = "Papers"
    ) +
    ggplot2::scale_y_continuous(
      breaks = count_breaks,
      limits = c(0, axis_limit),
      expand = ggplot2::expansion(mult = c(0, 0))
    ) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 16),
      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 16), size = 14),
      axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 10), size = 14),
      axis.text = ggplot2::element_text(size = 12),
      panel.grid.minor = ggplot2::element_blank()
    )

  if (flip) {
    plot <- plot +
      ggplot2::coord_flip(clip = "off")
  } else {
    plot <- plot
  }

  ggplot2::ggsave(project_path(path), plot = plot, width = 9, height = 6, dpi = 300)
}

save_year_time_series_plot <- function(data, year_col, count_col, title, path, fill) {
  plot_data <- data |>
    dplyr::arrange(.data[[year_col]])

  max_count <- max(plot_data[[count_col]], na.rm = TRUE)
  label_padding <- max_count * 0.03 + 0.2
  axis_limit <- max_count + label_padding + 0.2
  break_by <- if (max_count <= 6) 1 else 2
  count_breaks <- seq(0, ceiling(axis_limit), by = break_by)
  year_range <- range(plot_data[[year_col]], na.rm = TRUE)
  year_breaks <- seq(year_range[1], year_range[2], by = 1)
  year_limits <- c(year_range[1] - 0.5, year_range[2] + 0.5)

  plot <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = .data[[year_col]], y = .data[[count_col]])
  ) +
    ggplot2::geom_col(fill = fill, width = 0.8) +
    ggplot2::geom_text(
      data = plot_data |>
        dplyr::filter(.data[[count_col]] > 0),
      ggplot2::aes(
        y = .data[[count_col]] + label_padding,
        label = .data[[count_col]]
      ),
      vjust = 0,
      size = 4
    ) +
    ggplot2::labs(
      title = title,
      x = "Publication year",
      y = "Papers"
    ) +
    ggplot2::scale_x_continuous(
      breaks = year_breaks,
      limits = year_limits,
      expand = ggplot2::expansion(mult = c(0, 0))
    ) +
    ggplot2::scale_y_continuous(
      breaks = count_breaks,
      limits = c(0, axis_limit),
      expand = ggplot2::expansion(mult = c(0, 0))
    ) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 16),
      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 16), size = 14),
      axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 10), size = 14),
      axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1, size = 10.5),
      axis.text.y = ggplot2::element_text(size = 12),
      panel.grid.minor = ggplot2::element_blank()
    )

  ggplot2::ggsave(project_path(path), plot = plot, width = 11, height = 6, dpi = 300)
}

build_location_map_layers <- function() {
  if (!suppressPackageStartupMessages(requireNamespace("sf", quietly = TRUE))) {
    stop("Package 'sf' is required to draw the location map.", call. = FALSE)
  }

  if (!suppressPackageStartupMessages(requireNamespace("rnaturalearth", quietly = TRUE))) {
    stop("Package 'rnaturalearth' is required to draw the location map.", call. = FALSE)
  }

  if (!suppressPackageStartupMessages(requireNamespace("rnaturalearthdata", quietly = TRUE))) {
    stop("Package 'rnaturalearthdata' is required to draw the location map.", call. = FALSE)
  }

  if (!suppressPackageStartupMessages(requireNamespace("rnaturalearthhires", quietly = TRUE))) {
    stop("Package 'rnaturalearthhires' is required to draw subnational map units.", call. = FALSE)
  }

  world_map <- suppressMessages(
    rnaturalearth::ne_countries(scale = "medium", type = "map_units", returnclass = "sf")
  )

  country_units <- world_map |>
    dplyr::filter(normalize_map_unit_name(geounit) == normalize_map_unit_name(admin)) |>
    dplyr::transmute(
      map_unit_level = "country",
      map_unit_key = normalize_map_unit_name(dplyr::coalesce(name_long, geounit, name, admin))
    )

  map_unit_subdivisions <- world_map |>
    dplyr::filter(normalize_map_unit_name(geounit) != normalize_map_unit_name(admin)) |>
    dplyr::transmute(
      map_unit_level = "subnational",
      map_unit_key = normalize_map_unit_name(dplyr::coalesce(name_long, geounit, name))
    )

  admin_one_units <- suppressMessages(rnaturalearth::ne_states(returnclass = "sf")) |>
    dplyr::filter(admin %in% c("United States of America", "Canada")) |>
    dplyr::transmute(
      map_unit_level = "subnational",
      map_unit_key = normalize_map_unit_name(dplyr::coalesce(woe_name, name, gn_name, name_en))
    )

  list(
    world = world_map,
    units = dplyr::bind_rows(country_units, map_unit_subdivisions, admin_one_units) |>
      dplyr::distinct(map_unit_level, map_unit_key, .keep_all = TRUE)
  )
}

save_location_map <- function(data, title, path) {
  map_layers <- build_location_map_layers()
  mapped_units <- dplyr::left_join(map_layers$units, data, by = c("map_unit_level", "map_unit_key"))
  matched_units <- mapped_units |>
    dplyr::filter(!is.na(n_papers))

  unmatched_units <- data |>
    dplyr::anti_join(
      sf::st_drop_geometry(map_layers$units),
      by = c("map_unit_level", "map_unit_key")
    )

  if (nrow(unmatched_units) > 0) {
    warning(
      "Location map units were not matched to Natural Earth polygons: ",
      paste(unmatched_units$map_unit_name, collapse = ", "),
      call. = FALSE
    )
  }

  max_count <- max(data$n_papers, na.rm = TRUE)
  legend_breaks <- seq(1, max_count, by = if (max_count <= 6) 1 else 2)
  region_xlim <- c(-170, 35)
  region_ylim <- c(28, 79)

  plot <- ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = map_layers$world,
      fill = "grey99",
      color = "grey74",
      linewidth = 0.24
    ) +
    ggplot2::geom_sf(
      data = matched_units |>
        dplyr::filter(map_unit_level == "country"),
      ggplot2::aes(fill = n_papers),
      color = "grey15",
      linewidth = 0.28
    ) +
    ggplot2::geom_sf(
      data = matched_units |>
        dplyr::filter(map_unit_level == "subnational"),
      ggplot2::aes(fill = n_papers),
      color = "black",
      linewidth = 0.34
    ) +
    ggplot2::geom_sf(
      data = map_layers$world,
      fill = NA,
      color = "grey76",
      linewidth = 0.18
    ) +
    ggplot2::scale_fill_gradient(
      name = "Papers",
      low = "grey80",
      high = "black",
      limits = c(0, max_count),
      breaks = legend_breaks,
      na.value = "white"
    ) +
    ggplot2::coord_sf(
      xlim = region_xlim,
      ylim = region_ylim,
      expand = FALSE
    ) +
    ggplot2::labs(title = title) +
    ggplot2::theme_void(base_size = 14) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 18, margin = ggplot2::margin(b = 14)),
      plot.title.position = "plot",
      legend.position = "right",
      legend.title = ggplot2::element_text(size = 14),
      legend.text = ggplot2::element_text(size = 12),
      legend.key.height = grid::unit(16, "pt"),
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      panel.background = ggplot2::element_rect(fill = "white", color = NA),
      legend.background = ggplot2::element_rect(fill = "white", color = NA),
      legend.key = ggplot2::element_rect(fill = "white", color = NA),
      plot.margin = ggplot2::margin(10, 14, 10, 14)
    )

  ggplot2::ggsave(project_path(path), plot = plot, width = 11, height = 7, dpi = 300)
}

save_review_process_flow <- function(
  path,
  records_screened,
  full_text_review,
  included_in_analysis,
  search_details,
  title_abstract_note,
  full_text_note,
  title = "Systematic Literature Review Process"
) {
  title_abstract_excluded <- records_screened - full_text_review
  full_text_excluded <- full_text_review - included_in_analysis

  if (title_abstract_excluded < 0 || full_text_excluded < 0) {
    stop("Review-process counts must decrease across screening stages.", call. = FALSE)
  }

  format_n <- function(x) {
    format(x, big.mark = ",", trim = TRUE, scientific = FALSE)
  }

  box_data <- data.frame(
    box_id = c("searches", "screened", "full_text", "included", "excluded_ta", "excluded_ft"),
    box_group = c("search", "main", "main", "main", "excluded", "excluded"),
    xmin = c(2.2, 2.2, 2.2, 2.2, 8.0, 8.0),
    xmax = c(12.6, 6.8, 6.8, 6.8, 12.6, 12.6),
    ymin = c(15.0, 10.6, 5.8, 1.0, 10.6, 5.8),
    ymax = c(18.2, 13.6, 8.8, 4.0, 13.6, 8.8),
    title = c(
      "Database searches",
      NA_character_,
      NA_character_,
      NA_character_,
      "Excluded after title and abstract screening",
      "Excluded after full-text review"
    ),
    body = c(
      search_details,
      NA_character_,
      NA_character_,
      NA_character_,
      paste0(
        "n = ", format_n(title_abstract_excluded), "\n\n",
        title_abstract_note
      ),
      paste0(
        "n = ", format_n(full_text_excluded), "\n\n",
        full_text_note
      )
    ),
    label = c(
      NA_character_,
      paste0(
        "Records identified through database searching\n",
        "n = ", format_n(records_screened)
      ),
      paste0(
        "Reports assessed for eligibility by full-text review\n",
        "n = ", format_n(full_text_review)
      ),
      paste0(
        "Studies included in review and analysis\n",
        "n = ", format_n(included_in_analysis)
      ),
      NA_character_,
      NA_character_
    ),
    stringsAsFactors = FALSE
  )

  box_data$fill <- dplyr::case_when(
    box_data$box_group == "search" ~ "#F4F4F4",
    TRUE ~ "white"
  )

  text_data <- box_data |>
    dplyr::mutate(
      x = (xmin + xmax) / 2,
      y = (ymin + ymax) / 2,
      text_size = dplyr::case_when(
        box_group == "search" ~ 3.1,
        box_group == "main" ~ 4.0,
        TRUE ~ 3.0
      ),
      text_face = dplyr::case_when(
        box_group == "main" ~ "bold",
        TRUE ~ "plain"
      )
    )

  title_text_data <- text_data |>
    dplyr::filter(box_group %in% c("search", "excluded")) |>
    dplyr::mutate(
      title_y = dplyr::case_when(
        box_group == "search" ~ ymax - 0.65,
        TRUE ~ ymax - 0.35
      ),
      body_y = dplyr::case_when(
        box_group == "search" ~ y - 0.05,
        TRUE ~ y - 0.25
      )
    )

  arrow_data <- data.frame(
    x = c(4.5, 4.5, 4.5, 6.8, 6.8),
    y = c(15.0, 10.6, 5.8, 12.1, 7.3),
    xend = c(4.5, 4.5, 4.5, 8.0, 8.0),
    yend = c(13.6, 8.8, 4.0, 12.1, 7.3)
  )

  stage_data <- data.frame(
    x = c(0.55, 0.55, 0.55, 0.55),
    y = c(16.6, 12.1, 7.3, 2.5),
    label = c("Identification", "Screening", "Eligibility", "Included"),
    stringsAsFactors = FALSE
  )

  plot <- ggplot2::ggplot() +
    ggplot2::geom_rect(
      data = box_data,
      ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      fill = box_data$fill,
      color = "black",
      linewidth = 0.5
    ) +
    ggplot2::geom_segment(
      data = arrow_data,
      ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
      linewidth = 0.45,
      color = "black",
      arrow = grid::arrow(length = grid::unit(0.18, "inches"), type = "closed")
    ) +
    ggplot2::geom_text(
      data = stage_data,
      ggplot2::aes(x = x, y = y, label = label),
      hjust = 0,
      size = 4.0,
      fontface = "bold",
      color = "#555555"
    ) +
    ggplot2::geom_text(
      data = text_data |>
        dplyr::filter(box_group == "main"),
      ggplot2::aes(x = x, y = y, label = label),
      size = 4.0,
      fontface = "bold",
      lineheight = 1.1
    ) +
    ggplot2::geom_text(
      data = title_text_data,
      ggplot2::aes(x = x, y = title_y, label = title),
      size = 3.1,
      fontface = "bold",
      vjust = 1,
      lineheight = 1.05
    ) +
    ggplot2::geom_text(
      data = title_text_data,
      ggplot2::aes(x = x, y = body_y, label = body),
      size = 3.1,
      vjust = 0.5,
      lineheight = 1.08
    ) +
    ggplot2::coord_cartesian(xlim = c(0.2, 13.4), ylim = c(0.6, 18.4), clip = "off") +
    ggplot2::labs(title = title) +
    ggplot2::theme_void(base_size = 14) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 18, hjust = 0, margin = ggplot2::margin(b = 12)),
      plot.margin = ggplot2::margin(16, 20, 16, 16),
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      panel.background = ggplot2::element_rect(fill = "white", color = NA)
    )

  ggplot2::ggsave(project_path(path), plot = plot, width = 14.5, height = 9, dpi = 300)
}

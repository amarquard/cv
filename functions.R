## ---- plot_bridge

#' Plot a bridge using cosine function and ggplot lines and segments
#'
#' @param pillars_at_x the x-axis value at which to plot the pillars
#' (at `c(-pillars_at_x, pillars_at_x)`) in the graph coordinates
#' @param n_ropes the number of vertical ropes to show 
#' @param n_x the number of x values used to plot the curves (higher value
#' gives smoother curve)
#' 
#' @return a ggplot object
#' @export
#'
#' @examples
#' plot_bridge(2)
#' 
plot_bridge <- function(pillars_at_x, n_ropes = 61, n_x = 10000) {
  
  ropes_at_x <- seq(1, n_x, length.out = n_ropes) %>% round()
  
  bridge_coords <- tibble::tibble(
    # x values around two pillars at position p and minus p
    x = seq(-2 * pillars_at_x, 2 * pillars_at_x, length.out = n_x),
    # Hyperbolic cosine to get the centre curve
    y = cosh(x)
  ) %>%
    # Extend by repeating parts of the center curve at the ends
    mutate(y = case_when(x < -pillars_at_x ~ lead(y, n_x / 2),
                         x > pillars_at_x ~ lag(y, n_x / 2),
                         TRUE ~ y)
    ) 
  
  bridge_coords %>%
    ggplot(aes(x = x, y = y)) +
    geom_line(size = 1) +
    ylim(0.5, cosh(pillars_at_x)) +
    
    # add road
    geom_hline(yintercept = 1, size = 1) +
    
    # add pillars
    geom_vline(xintercept = c(-pillars_at_x, pillars_at_x), size = 1.3) +
    
    # add ropes
    geom_segment(aes(x = x, y = 1, xend = x, yend = y),
                 data = ~ .x[ropes_at_x, ]) +
    
    # remove all axes etc.
    theme_void()
}

## ---- scholar_functions
make_scholar_link <- function(pubid, authorid) {
  glue(
    paste0(
      "https://scholar.google.com/citations?",
      "view_op=view_citation&",
      "hl=da&",
      "user={authorid}&",
      "citation_for_view={authorid}:{pubid}"
    )
  )
}

apply_bold <- function(x) {
  paste0("**", x, "**")
}

format_myself <- function(x, me) {
  stringr::str_replace_all(x, me, apply_bold(me))
}


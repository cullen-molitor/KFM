GeomSmooth <- ggproto("GeomSmooth", Geom,
                      setup_params = function(data, params) {
                        params$flipped_aes <- has_flipped_aes(data, params, range_is_orthogonal = TRUE, ambiguous = TRUE)
                        params
                      },
                      
                      extra_params = c("na.rm", "orientation"),
                      
                      setup_data = function(data, params) {
                        GeomLine$setup_data(data, params)
                      },
                      
                      draw_group = function(data, panel_params, coord, se = FALSE, flipped_aes = FALSE) {
                        ribbon <- transform(
                          data, 
                          fill = alpha(data$fill, data$alpha), 
                          colour = NA,
                          alpha = data$alpha)
                        path <- transform(
                          data, 
                          colour = alpha(data$colour, data$alpha),
                          alpha = data$alpha)
                        
                        ymin = flipped_names(flipped_aes)$ymin
                        ymax = flipped_names(flipped_aes)$ymax
                        has_ribbon <- se && !is.null(data[[ymax]]) && !is.null(data[[ymin]])
                        
                        gList(
                          if (has_ribbon) GeomRibbon$draw_group(ribbon, panel_params, coord, flipped_aes = flipped_aes),
                          GeomLine$draw_panel(path, panel_params, coord)
                        )
                        
                      },
                      
                      draw_key = draw_key_smooth,
                      
                      required_aes = c("x", "y"),
                      optional_aes = c("ymin", "ymax"),
                      
                      default_aes = aes(colour = "#3366FF", fill = "grey60", size = 1,
                                        linetype = 1, weight = 1, alpha = 0.4)
)
assignInNamespace("GeomSmooth", GeomSmooth, "ggplot2")
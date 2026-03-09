# Generate the labormarket hex sticker
# Requires: hexSticker, ggplot2

library(hexSticker)
library(ggplot2)

# Draw a gear using polygon coordinates
gear_points <- function(n_teeth = 8, r_outer = 1, r_inner = 0.72, cx = 0, cy = 0) {
  angles <- seq(0, 2 * pi, length.out = n_teeth * 4 + 1)
  r <- rep(c(r_outer, r_outer, r_inner, r_inner), n_teeth)
  r <- c(r, r[1])
  data.frame(
    x = cx + r * cos(angles),
    y = cy + r * sin(angles)
  )
}

circle_points <- function(r, cx = 0, cy = 0, n = 60) {
  theta <- seq(0, 2 * pi, length.out = n)
  data.frame(x = cx + r * cos(theta), y = cy + r * sin(theta))
}

# Two interlocking gears — larger one (worker) left, smaller one (firm) right
# Offset so teeth mesh
gear1 <- gear_points(n_teeth = 8, r_outer = 1.0, r_inner = 0.72, cx = -0.55, cy = 0.15)
hole1 <- circle_points(r = 0.30, cx = -0.55, cy = 0.15)

gear2 <- gear_points(n_teeth = 6, r_outer = 0.7, r_inner = 0.50, cx = 0.65, cy = -0.35)
hole2 <- circle_points(r = 0.20, cx = 0.65, cy = -0.35)

# Colors
col_gear1 <- "#3B82A0"   # steel blue — worker
col_gear2 <- "#D4A03C"   # muted gold — firm
col_bg    <- "#F7FAFC"    # near-white
col_border <- "#3B82A0"
col_text  <- "#1A3A4A"    # dark teal

p <- ggplot() +
  # Large gear (behind)
  geom_polygon(data = gear1, aes(x = x, y = y), fill = col_gear1, color = "#2A6478", linewidth = 0.4) +
  geom_polygon(data = hole1, aes(x = x, y = y), fill = col_bg, color = "#2A6478", linewidth = 0.3) +
  # Small gear (in front)
  geom_polygon(data = gear2, aes(x = x, y = y), fill = col_gear2, color = "#A67C1E", linewidth = 0.4) +
  geom_polygon(data = hole2, aes(x = x, y = y), fill = col_bg, color = "#A67C1E", linewidth = 0.3) +
  coord_equal() +
  theme_void() +
  theme(
    plot.background = element_blank(),
    panel.background = element_blank()
  )

sticker(
  p,
  package = "labormarket",
  p_size = 14,
  p_color = col_text,
  p_y = 1.42,
  p_family = "sans",
  s_x = 1.0,
  s_y = 0.78,
  s_width = 1.5,
  s_height = 1.1,
  h_fill = col_bg,
  h_color = col_border,
  h_size = 1.5,
  filename = "man/figures/logo.png",
  dpi = 300
)

message("Hex sticker saved to man/figures/logo.png")

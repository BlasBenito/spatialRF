# Hex sticker for spatialData
# Dependencies: hexSticker, ggplot2, png, sysfonts, showtext
# install.packages(c("hexSticker", "ggplot2", "png", "sysfonts", "showtext"))

library(ggplot2)
library(hexSticker)
library(png)
library(sysfonts)
library(showtext)

# ── 1. Load Montserrat font ────────────────────────────────────────────────
font_add_google("Montserrat", "Montserrat")
showtext_auto()

# ── 2. Build subplot from global.png ──────────────────────────────────────
# Color analysis of global.png:
#   Ocean blue   ~ #3B9ED9
#   Land green   ~ #5BB556
#   Outlines     ~ #1C1C1C
#   Yellow pins  ~ #FFCA28
#   Red pins     ~ #E53935
# Harmonious hex palette:
#   h_fill  = "#152520"  (deep forest green)
#   h_color = "#66a839"  (medium green border)
#   p_color = "#d8f3dc"  (light mint text)

img <- png::readPNG("dev_scripts/hex/world_tree.png")

p <- ggplot() +
  annotation_raster(img, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
  coord_fixed() +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background = element_rect(fill = "transparent", colour = NA)
  )

# ── 3. Render hex sticker ─────────────────────────────────────────────────
sticker(
  subplot = p,
  package = "spatialRF",
  p_size = 38, # sized for legibility inside hex frame
  p_color = "#1e4b00",
  p_family = "Montserrat",
  p_y = 1.45,
  s_x = 1.0,
  s_y = 0.75,
  s_width = 1.,
  s_height = 1.,
  h_fill = "white", # deep forest green (from palette analysis)
  h_color = "#66a839",
  h_size = 1.2,
  filename = "dev_scripts/hex/logo.png",
  dpi = 600
)


file.copy("dev_scripts/hex/logo.png", "man/figures/logo.png", overwrite = TRUE)

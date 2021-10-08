library("bittermelon")
library("grid")
library("piecepackr")

font_file <- system.file("fonts/spleen/spleen-8x16.hex.gz", package = "bittermelon")
font <- read_hex(font_file)
optparse <- as_bm_bitmap("optparse", font = font)
hashbang <- as_bm_bitmap("#!", font = font)

draw_logo <- function() {
    hex <- pp_shape("convex6")
    grid.newpage()
    grid.draw(hex$shape(gp = gpar(col = NA, fill = "#C0C0C0")))
    plot(optparse, col = c("transparent", "black"),
         vp = viewport(y = 0.65, width=0.75, height = 0.21))
    plot(hashbang, col = c("transparent", "black"),
         vp = viewport(y = 0.33, width = 0.5, height = 0.4))
    grid.draw(hex$mat(mat_width = 0.03, gp = gpar(col = NA, fill = "black")))
}

w <- 3.0
svg("man/figures/logo.svg", width = w, height = w, bg = "transparent")
draw_logo()
dev.off()

png("man/figures/logo.png", width = w, height = w, units = "in",
    res = 72, bg = "transparent")
draw_logo()
dev.off()

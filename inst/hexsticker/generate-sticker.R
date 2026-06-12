#!/usr/bin/env Rscript
# ============================================================================
# pkgtools Hex Sticker Generator
#
# Generates multiple hex sticker designs for the pkgtools package.
# Run from the package root: Rscript inst/hexsticker/generate-sticker.R
# ============================================================================

output_dir <- "inst/hexsticker"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

png_sticker <- function(filename, render_fn, w = 600, h = 600) {
  path <- file.path(output_dir, filename)
  png(path, width = w, height = h, units = "px", res = 150)
  on.exit(dev.off())
  render_fn(w, h)
  cat("  Saved:", path, "\n")
}

hex_pts <- function(cx, cy, r) {
  angles <- seq(30, 390, by = 60) * pi / 180
  list(x = cx + r * cos(angles), y = cy + r * sin(angles))
}

# ============================================================================
# Design 1: Classic — R color palette with package icon
# ============================================================================
design_classic <- function(w, h) {
  p <- c("#1C6EB4", "#1CADD3", "#E6E6E6", "#002B36")
  plot(c(-1, 1), c(-1, 1), type = "n", xaxt = "n", yaxt = "n", bty = "n", asp = 1)

  hex <- hex_pts(0, 0, 0.92)
  polygon(hex$x, hex$y, col = p[1], border = NA)

  # Inner ring
  inner <- hex_pts(0, 0, 0.82)
  polygon(inner$x, inner$y, col = p[2], border = NA)

  # Small hexagon decoration
  small <- hex_pts(0, 0.3, 0.18)
  polygon(small$x, small$y, col = "white", border = NA)

  # Package box
  bx <- c(-0.18, 0.18, 0.18, -0.18, -0.18)
  by <- c(-0.18, -0.18, 0.18, 0.18, -0.18)
  polygon(bx, by, col = "white", border = NA)
  # Open flap
  polygon(c(-0.18, -0.06, 0.06, 0.18, 0.18, -0.18) + c(0, 0),
          c(0.06, 0.15, 0.15, 0.06, 0.06, 0.06), col = p[1], border = NA)

  # Wrench icon (simplified)
  rect(-0.08, -0.02, 0.08, 0.02, col = p[2], border = NA)
  polygon(c(0, 0.04, 0.04, 0), c(0.02, 0.02, 0.06, 0.06), col = p[2], border = NA)

  # Text
  text(0, -0.28, "pkgtools", col = "white", cex = 2.4, font = 2)
  text(0, -0.38, "Development Tools", col = "white", cex = 0.7)
}

# ============================================================================
# Design 2: Minimal — Clean, bold typography
# ============================================================================
design_minimal <- function(w, h) {
  p <- c("#4A90D9", "#FFFFFF", "#333333")
  plot(c(-1, 1), c(-1, 1), type = "n", xaxt = "n", yaxt = "n", bty = "n", asp = 1)

  hex <- hex_pts(0, 0, 0.92)
  polygon(hex$x, hex$y, col = p[1], border = NA)

  # Large lowercase letters
  text(0, 0.05, "pkgtools", col = "white", cex = 3.5, font = 2)
  text(0, -0.3, "R Package Development", col = "white", cex = 0.7)
}

# ============================================================================
# Design 3: Gradient — Modern two-tone with geometric accent
# ============================================================================
design_gradient <- function(w, h) {
  p <- c("#264653", "#2A9D8F", "#E9C46A", "#F4A261")
  plot(c(-1, 1), c(-1, 1), type = "n", xaxt = "n", yaxt = "n", bty = "n", asp = 1)

  hex <- hex_pts(0, 0, 0.92)
  polygon(hex$x, hex$y, col = p[1], border = NA)

  # Diagonal accent stripe
  rect(-0.9, -0.9, 0.9, 1.8, col = p[2], border = NA)

  # Small triangle
  tri <- rbind(c(0, 0.5), c(-0.25, 0.1), c(0.25, 0.1))
  polygon(tri, col = p[3], border = NA)

  text(0, -0.1, "pkgtools", col = "white", cex = 2.6, font = 2)
  text(0, -0.35, "Development Tools", col = "white", cex = 0.7)
}

# ============================================================================
# Design 4: Code — Dark background with code brackets
# ============================================================================
design_code <- function(w, h) {
  p <- c("#1E1E1E", "#DCDCAA", "#9CDCFE", "#569CD6")
  plot(c(-1, 1), c(-1, 1), type = "n", xaxt = "n", yaxt = "n", bty = "n", asp = 1)

  hex <- hex_pts(0, 0, 0.92)
  polygon(hex$x, hex$y, col = p[1], border = NA)

  # Code brackets
  text(-0.5, 0, "<", col = p[4], cex = 4, font = 2)
  text(0.5, 0, ">", col = p[4], cex = 4, font = 2)

  text(0, 0.05, "pkgtools", col = p[2], cex = 2.4, font = 2)
  text(0, -0.3, "R Package Development", col = p[3], cex = 0.7)
}

# ============================================================================
# Design 5: Bold — High contrast, very clean
# ============================================================================
design_bold <- function(w, h) {
  p <- c("#6C5CE7", "#A29BFE", "white")
  plot(c(-1, 1), c(-1, 1), type = "n", xaxt = "n", yaxt = "n", bty = "n", asp = 1)

  hex <- hex_pts(0, 0, 0.92)
  polygon(hex$x, hex$y, col = p[1], border = NA)

  # Concentric circle decorations
  for (r in seq(0.25, 0.45, length = 4)) {
    circ <- hex_pts(0, 0.05, r)
    polygon(circ$x, circ$y, col = p[2], border = NA)
  }

  text(0, 0.05, "pkgtools", col = "white", cex = 3, font = 2)
  text(0, -0.35, "Development Tools", col = "white", cex = 0.7)
}

# ============================================================================
# Design 6: Subtle — Soft colors, professional
# ============================================================================
design_subtle <- function(w, h) {
  p <- c("#34495E", "#3498DB", "#ECF0F1", "#BDC3C7")
  plot(c(-1, 1), c(-1, 1), type = "n", xaxt = "n", yaxt = "n", bty = "n", asp = 1)

  hex <- hex_pts(0, 0, 0.92)
  polygon(hex$x, hex$y, col = p[1], border = NA)

  # Subtle inner rings
  for (i in 1:4) {
    s <- hex_pts(0, 0, 0.92 - i * 0.05)
    polygon(s$x, s$y, col = p[2], border = NA)
  }

  text(0, 0.05, "pkgtools", col = p[3], cex = 2.0, font = 2)
  text(0, -0.3, "R Package Development", col = p[4], cex = 0.65)
}

# ============================================================================
# Main
# ============================================================================
cat("Generating pkgtools hex stickers...\n\n")
cat("Design 1: Classic (R colors, package icon)\n")
png_sticker("design-classic.png", design_classic)

cat("\nDesign 2: Minimal (bold typography)\n")
png_sticker("design-minimal.png", design_minimal)

cat("\nDesign 3: Gradient (teal accent)\n")
png_sticker("design-gradient.png", design_gradient)

cat("\nDesign 4: Code (IDE dark theme)\n")
png_sticker("design-code.png", design_code)

cat("\nDesign 5: Bold (purple geometric)\n")
png_sticker("design-bold.png", design_bold)

cat("\nDesign 6: Subtle (professional, soft colors)\n")
png_sticker("design-subtle.png", design_subtle)

cat("\nDone! Preview stickers saved to:", output_dir, "\n\n")
cat("To set the final sticker:\n")
cat("  cp inst/hexsticker/design-CHOSEN.png inst/hexsticker/hex-sticker.png\n\n")
cat("Then update README.md with:\n")
cat("  ![pkgtools](inst/hexsticker/hex-sticker.png)\n")

library(sf)
library(rmapshaper)

cat("=== Optimizing Geographic Boundaries ===\n\n")

# Load original boundaries
cat("Loading original boundaries...\n")
boundaries <- st_read("geographic/chicago_zip_boundaries.geojson", quiet = TRUE)

cat("Original file stats:\n")
cat("  - Features:", nrow(boundaries), "\n")
cat("  - Points: ~", sum(sapply(st_geometry(boundaries), length)), "\n")

# Simplify geometry - this dramatically reduces file size
cat("\nSimplifying geometry (keeping topology)...\n")
boundaries_simplified <- ms_simplify(boundaries, keep = 0.05, keep_shapes = TRUE)

cat("Simplified file stats:\n")
cat("  - Features:", nrow(boundaries_simplified), "\n")
cat("  - Points: ~", sum(sapply(st_geometry(boundaries_simplified), length)), "\n")

# Save optimized version
cat("\nSaving optimized boundaries...\n")
st_write(boundaries_simplified, 
         "geographic/chicago_zip_boundaries.geojson", 
         delete_dsn = TRUE, 
         quiet = TRUE)

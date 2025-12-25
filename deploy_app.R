# Save this as deploy_app.R

library(rsconnect)

# Get app dependencies but filter out terra
deps <- appDependencies(".")
deps_filtered <- deps[deps$Package != "terra", ]

# Create a temporary manifest without terra
temp_manifest <- tempfile(fileext = ".json")
writeLines(jsonlite::toJSON(list(
  metadata = list(appmode = "shiny"),
  packages = deps_filtered
), auto_unbox = TRUE, pretty = TRUE), temp_manifest)

# Deploy using the filtered manifest
deployApp(
  appFiles = c("app_optimized.R", "data/", "geographic/"),
  appManifest = temp_manifest,
  forceUpdate = TRUE
)
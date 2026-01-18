#' # 3D Nyika National Park
#' _______________________________________________________________________________________
#' # i. Load necessary packages
#' _______________________________________________________________________________________
libs <- c("sf", "tidyverse",
          "scico", "elevatr",
          "MetBrewer", "rayshader",
          "colorspace", "glue",
          "here")
#'  
installed_libs <- libs %in% rownames(
  installed.packages()
)
#'  
if(any(installed_libs == FALSE)) {
  install.packages(libs[!installed_libs],
                   dependencies = TRUE)
}
#' 
invisible(lapply(libs,
                 library,
                 character.only = TRUE))
#'  
#' # Set up polygon for clipping DEM
#' _______________________________________________________________________________________
map <- "nyika"
#'
#' # Malawi protected areas boundaries source: 
#' # http://landscapeportal.org/layers/geonode%3Aprotected_areas_geo
#' 
data <- st_read(here("data/protected_areas_geo", "protected_areas_geo.shp")) |>
  filter(str_detect(NAME_LOW, str_to_title(str_replace(map, "_", " ")))) |>
  st_transform(crs = 32736)
#' 
d_cent <- st_centroid(data) |>
  st_transform(crs = 4326)
#' 
c <- d_cent |>
  mutate(long = unlist(map(d_cent$geometry, 1)),
         lat = unlist(map(d_cent$geometry, 2))) |>
  as_tibble()
#' 
coords <- c(c[[1, "long"]], c[[1, "lat"]])
#' 
#' # Plot to review
data |>
  ggplot() +
  geom_sf()
#' Or
plot(st_geometry(data), col = "cyan")
#' 
#' # ii. Get DEM and convert to matrix
#' _______________________________________________________________________________________
z <- 11
#' 
elev <-elevatr::get_elev_raster(locations = data,
                                 z = z,
                                 clip = "locations")
#' 
mat <- rayshader::raster_to_matrix(elev)
#' 
#' # Set up color palette
#' _______________________________________________________________________________________
pal <- 'okeeffe1'
#'   
c1 <- met.brewer("OKeeffe1")
#'
colors <- c(rev(c1))
#'
#'cols <- c("#38335B", "#8677AE",
#'          "#8289D0", "#BC6E30",
#'          "#F0B24D")

#'swatchplot(cols)
#'
swatchplot(colors)
#'   
#' # Calculate aspect ratio of the plot
#' _______________________________________________________________________________________
h <- nrow(mat)
w <- ncol(mat)
#'
#' Scale so larger side is 1
wr <- w / max(c(w, h))
hr <- h / max(c(w, h))
#'   
#' # Build 3D object
#' _______________________________________________________________________________________
#' # Setting shadow to 500 feet below minimum value in DEM
shadow_depth <- min(mat, na.rm = TRUE) - 500
#'  
#' # Setting resolution to about 5x for height
res <- mean(round(terra::res(elev))) / 5
#' 
try(rgl::close3d())
#' 
#' # Create initial 3D object
mat |>
  height_shade(texture = grDevices::colorRampPalette(colors)(256)) |>
  plot_3d(heightmap = mat,
          solid = FALSE,
          z = res,
          shadowdepth = shadow_depth,
          windowsize = c(800,800),
          phi = 89,
          zoom = 1,
          theta = 0,
          background = "white")
#' 
#' # Use this to adjust view after building the window object
rayshader::render_camera(phi = 45,
                         zoom = 1,
                         theta = 0)
#' 
#' # Create high quality graphic
#' _______________________________________________________________________________________
#' # Ensure dir exists for the graphic
if(!dir.exists(glue("images/{map}"))){
  dir.create(glue("images/{map}"))
}
#' # Set up outfile where graphic will be saved
out_file <- str_to_lower(glue("images/{map}/{map}_{pal}_z{z}_4.png"))
#' 
#' # Now that everything is assigned, save these objects so we can use them in the 
#'   markup script
saveRDS(list(map = map,
             pal = pal,
             coords = coords,
             z = z,
             colors = colors,
             out_file = out_file),
        glue("R/header.rds"))
#'   
#' # Wrap the code below in brackets '{}' so it runs as a chunk  
{
  # Test write a PNG to ensure the file path is good.
  if(!file.exists(out_file)) {
    png::writePNG(matrix(1), out_file)
  }
  # I prefer tracking when the render starts
  start_time <- Sys.time()
  cat(glue("Start Time: {start_time}"), "\n")
  rayshader::render_highquality(
    out_file,
    samples = 50, # See rayrender::render_scene for more info
    light = FALSE, # Turn off lights since we'll use environment_light
    interactive = FALSE,
    preview = TRUE,
    environment_light = "data/env/phalzer_forest_01_4k.hdr",
    intensity_env = 1.5,
    rotate_env = -20, # Rotate the light: + values move it counter-clockwise
    width = 1000, height = 1000 # This effectively sets the resolution of the final graphic
  )
  end_time <- Sys.time()
  cat(glue("End time: {end_time}"), "\n")
  cat(glue("Total time: {end_time - start_time}"), "\n")
}
#'  
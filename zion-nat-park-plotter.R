# The following is described in full in a tutorial found here: 
# https://www.tylermw.com/a-step-by-step-guide-to-making-3d-maps-with-satellite-imagery-in-r/
# Registration for nasa imagery and USGS are required to access the data

#install.packages(c("rayshader", "raster", "sp"))
library(rayshader)
library(sp)
library(raster)
library(scales)

elevation1 = raster("N37W113.hgt")
elevation2 = raster("N37W114.hgt")

zion_elevation = merge(elevation1,elevation2)

height_shade(raster_to_matrix(zion_elevation)) %>% 
  plot_map()

zion_r = raster("LC08_L1TP_038034_20191101_20191114_01_T1_B4.TIF")
zion_g = raster("LC08_L1TP_038034_20191101_20191114_01_T1_B3.TIF")
zion_b = raster("LC08_L1TP_038034_20191101_20191114_01_T1_B2.TIF")

zion_rbg = stack(zion_r, zion_g, zion_b)
zion_rbg_corrected = sqrt(zion_rbg)
plotRGB(zion_rbg_corrected, scale=255) 

crs(zion_r)
crs(zion_elevation)
#coordinated reference systems don't match so need to fix that

zion_elevation_utm= projectRaster(zion_elevation, crs = crs(zion_b), method = 'bilinear')
crs(zion_elevation_utm)

bottom_left = c(y=-113.155277, x=37.116253)
top_right   = c(y=-112.832502, x=37.414948)

extent_latlong = sp::SpatialPoints(rbind(bottom_left, top_right), proj4string=sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
extent_utm = sp::spTransform(extent_latlong, raster::crs(zion_elevation_utm))

e = raster::extent(extent_utm)
e

zion_rgb_cropped = raster::crop(zion_rbg_corrected, e)
elevation_cropped = raster::crop(zion_elevation_utm, e)

names(zion_rgb_cropped) = c("r","g","b")

zion_r_cropped = rayshader::raster_to_matrix(zion_rgb_cropped$r)
zion_g_cropped = rayshader::raster_to_matrix(zion_rgb_cropped$g)
zion_b_cropped = rayshader::raster_to_matrix(zion_rgb_cropped$b)

zionel_matrix = rayshader::raster_to_matrix(elevation_cropped)

zion_rgb_array = array(0,dim=c(nrow(zion_r_cropped),ncol(zion_r_cropped),3))
zion_rgb_array[,,1] = zion_r_cropped/255 #Red layer
zion_rgb_array[,,2] = zion_g_cropped/255 #Blue layer
zion_rgb_array[,,3] = zion_b_cropped/255 #Green layer

zion_rgb_array = aperm(zion_rgb_array, c(2,1,3))
zion_rgb_contrast = scales::rescale(zion_rgb_array,to=c(0,1))

plot_map(zion_rgb_contrast)

plot_3d(zion_rgb_contrast, zionel_matrix, windowsize = c(1100,900), zscale = 15, shadowdepth = -50,
        zoom=0.5, phi=45,theta=-45,fov=70, background = "#F2E1D0", shadowcolor = "#523E2B")
render_snapshot(title_text = "Zion National Park, Utah | Imagery: Landsat 8 | DEM: 30m SRTM",
                title_bar_color = "#1f5214", title_color = "white", title_bar_alpha = 1)
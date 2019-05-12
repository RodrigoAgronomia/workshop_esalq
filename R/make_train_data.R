
library(sf)
library(jsonlite)
library(raster)
library(rgdal)
library(dplyr)

data_dir = 'C:/Users/rodri/GIT/workshop_esalq/data'
save_dir = 'C:/Users/rodri/GIT/workshop_esalq/data/class'

df = read.csv(file.path(data_dir, 'via_region_data.csv'))

make_polygons = function(spts, buf){
  pts = data.frame(st_coordinates(spts))
  pts$L1 = 1:nrow(pts)
  pts = pts[rep(1:nrow(pts), each = 5),]
  pts$X = pts$X + c(1, -1, -1, 1, 1) * buf
  pts$Y = pts$Y + c(1, 1, -1, -1, 1) * buf
  pols = lapply(split(pts, pts$L1), function(p) {
    st_polygon(list(cbind(p$X,p$Y)))
  })
  pols = st_sfc(pols)
}


pts = df$region_shape_attributes %>%
  as.character() %>% lapply(fromJSON) %>%
  do.call(rbind, .) %>% data.frame() %>%
  mutate(x = unlist(cx), y = unlist(cy)) %>% cbind(df, .)
pts


f = 'DJI_0332.JPG'
crop = 'Soybean'

f = 'DJI_0392.JPG'
crop = 'Corn'
# dir.create(file.path(save_dir, crop), recursive = TRUE)

img = stack(file.path(data_dir, 'seg', crop, f))

spts = pts[pts$filename == f, ]
buf = 64
spts$x = pmin(pmax(spts$x, buf), ncol(img) - buf)
spts$y = nrow(img) - pmin(pmax(spts$y, buf), nrow(img) - buf)
spts = st_as_sf(spts, coords =  c('x','y'))

img_pol = st_as_sfc(as(extent(img), 'SpatialPolygons'))
plot(img_pol)

spols = make_polygons(spts, buf)
plot(spols, add = TRUE)


pdist = st_distance(spts)
diag(pdist) = Inf
mdist = apply(pdist, 1, min)
crit = mdist >= sort(mdist, decreasing = TRUE)[100]
spols = spols[crit]
# crit = unlist(lapply(st_intersects(spols, spols), length))
# sum(crit == 1)
plot(spols, border = 'blue', add = TRUE)


i = 1
for (i in 1:length(spols)) {
  print(i)
  pol = as_Spatial(spols[i])
  imgp = crop(img, pol)
  img_file = file.path(save_dir, paste0('IMG_', 3000 + i, '.png'))
  
  rgdal::writeGDAL(as(imgp, "SpatialGridDataFrame"), img_file,
                   drivername = "PNG", type = "Byte"
  )
}


# plot(img_pol)
# plot(spols, add = TRUE)
# 
# img_tst = st_make_grid(img_pol, cellsize = 128)
# img_tst = st_intersection(img_tst, img_pol)
# img_tst_area = as.numeric(st_area(img_tst))
# img_tst = img_tst[img_tst_area ==  max(img_tst_area)]
# plot(img_tst, border = 'blue', add = TRUE)
# 
# pdist = st_distance(img_tst, spts)
# mdist = apply(pdist, 1, min)
# crit = mdist >= sort(mdist, decreasing = TRUE)[100]
# img_tst = img_tst[crit]
# plot(img_tst, border = 'red', add = TRUE)
# 
# crop = 'Soil'
# 
# i = 1
# for (i in 1:length(img_tst)) {
#   print(i)
#   pol = as_Spatial(img_tst[i])
#   imgp = crop(img, pol)
#   img_file = file.path(save_dir, crop, paste0('IMG_', 1000 + i, '.png'))
#   
#   rgdal::writeGDAL(as(imgp, "SpatialGridDataFrame"), img_file,
#                    drivername = "PNG", type = "Byte"
#   )
# }



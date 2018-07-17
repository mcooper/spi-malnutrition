library(dplyr)
library(sp)
library(rgdal)
library(INLA)
library(maptools)

setwd('G://My Drive/DHS Processed')

hha <- read.csv('HH_data_A.csv')
spei <- read.csv('PrecipIndices.csv')
cov <- read.csv('SpatialCovars.csv')

all <- Reduce(function(x, y){merge(x,y,all.x=T, all.y=F)}, list(hha, spei, cov))

poly <- readOGR('G://My Drive/DHS Spatial Covars/Global Codes and Shapefile', 'ne_50m_admin_0_countries')

#Test first in East Africa
all <- all %>%
  filter(country %in% c("KE", "TZ", "UG", "RW"))
poly <- poly[poly$ISO_A2 %in% c("KE", "TZ", "UG", "RW"), ]

border <- unionSpatialPolygons(poly, rep(1, nrow(poly)))
plot(border)

border <- spTransform(border, CRSobj = CRS("+proj=aeqd +lat_0=0 +lon_0=0"))

bdry <- inla.sp2segment(border)
bdry$loc[,1] <- sapply(bdry$loc[,1], function (x) x/1000) 
bdry$loc[,2] <- sapply(bdry$loc[,2], function (x) x/1000)

# Create mesh
mesh_b <- inla.mesh.2d(boundary = bdry, 
                       max.edge=c(20, 100), # maximum triange edge length for inner and outer boundary (in SAME SCALE UNIT as projection, here km!!)
                       cutoff = 50) # Minimum allowed distance between points. Helps avoiding very small triangles at the border!
mesh_b$n # Number of nodes (not)
plot(mesh_b, main = "")

### MAPPING BETWEEN MESH AND CONTINOUS SPACE
# Projector matrix for coordinates of DSH values
coords <- all %>% 
  dplyr::select(longitude, latitude) %>% 
  SpatialPoints(proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")) %>%
  spTransform(CRS("+proj=aeqd +lat_0=0 +lon_0=0")) %>%
  .@coords/1000

A <- inla.spde.make.A(mesh = mesh_b, loc = coords)
dim(A) # Dimension is number of observations x number of vertices (nodes of triangles)


### CREATE SPDE MODEL
# We follow Simpson et al (2017) - see tutorial and use pc-priors in the model. 
# This means we also need to scale the model following Sorbye (2017) - see INLA website.
pcprec <- list(prior='pcprec', param=c(1, 0.01))

# SPDE model definition
spde <- inla.spde2.pcmatern(mesh = mesh_b,
                            alpha = 2,
                            prior.range = c(0.05, 0.01),
                            prior.sigma = c(1, 0.01))


### ORGANIZE DATA
# Stack data
stk <- inla.stack(
  data = list(haz_dhs = all$haz_dhs),
  A = list(A,1) ,
  effects = list(s=1:spde$n.spde,
                 data.frame(intercept=1,
                            interview_year=all$interview_year,
                            #interview_month=as.factor(all$interview_month),
                            age=all$age,
                            birth_order=all$birth_order,
                            hhsize=all$hhsize,
                            sex=all$sex,
                            mother_years_ed=all$mother_years_ed,
                            toilet=all$toilet,
                            head_age=all$head_age,
                            head_sex=all$head_sex,
                            #urban_rural=all$urban_rural,
                            wealth_index=all$wealth_index)),
  tag = "dat")

f <- haz_dhs ~ 0 + intercept + interview_year + age + birth_order + hhsize + sex + 
  mother_years_ed + toilet + head_age + head_sex + wealth_index

mod <- inla(f, 
             data = inla.stack.data(stk),
             family = "gaussian",
             control.predictor = list(A=inla.stack.A(stk), link = 1),
             control.compute = list(cpo = TRUE), verbose = T)

summary(mod)










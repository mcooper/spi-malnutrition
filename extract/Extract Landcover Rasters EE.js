var esa = ee.Image("users/geflanddegradation/lcov/ESACCI-LC-L4-LCCS-Map-300m-P1Y-2015-v207")
var ref = ee.Image('users/mwcoopr/chirps-v20198101')

// Get information about the reference projection.
var refProjection = ref.projection();

var classed = esa.remap([10, 11, 12, 20, 30, 190, 200, 201, 202, 220, 0, 210, 110, 120, 
121, 122, 130, 140, 150, 152, 153, 180, 40, 50, 60, 61, 62, 70, 
71, 80, 90, 100, 160, 170],
[0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2, 2, 2, 2, 2, 2, 2, 2, 
2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3])

//Forest
var forest = classed.eq(3)
    .reduceResolution({reducer: ee.Reducer.mean(), maxPixels: 325})
    .reproject({
      crs: refProjection
    });

Export.image.toDrive({image: forest, 
                      description: 'forest', 
                      fileFormat: 'GeoTiff',
                      scale: 5000
})


//Water
var water = classed.eq(1)
    .reduceResolution({reducer: ee.Reducer.mean(), maxPixels: 325})
    .reproject({
      crs: refProjection
    });

Export.image.toDrive({image: water, 
                      description: 'water', 
                      fileFormat: 'GeoTiff',
                      scale: 5000
})


//Grass
var grass = classed.eq(2)
    .reduceResolution({reducer: ee.Reducer.mean(), maxPixels: 325})
    .reproject({
      crs: refProjection
    });

Export.image.toDrive({image: grass, 
                      description: 'grass', 
                      fileFormat: 'GeoTiff',
                      scale: 5000
})
  

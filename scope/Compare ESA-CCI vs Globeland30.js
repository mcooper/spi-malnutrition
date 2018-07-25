var gl00 = ee.ImageCollection('users/mwcoopr/globeland30_2010').mosaic()

var lc00 = ee.Image("users/geflanddegradation/lcov/ESACCI-LC-L4-LCCS-Map-300m-P1Y-2010-v207")

Map.addLayer(gl00.eq(10));

Map.addLayer(lc00.gte(10).and(lc00.lte(30)))
import ee
import pandas as pd

ee.Initialize()

data = pd.read_csv('g:\\My Drive\DHS Processed\sp_export.csv')

forest = ee.Image("UMD/hansen/global_forest_change_2016_v1_4")
baseforest = forest.select('treecover2000')
baseforest = baseforest.where(baseforest.gte(10), 1).where(baseforest.lt(10), 0)

gain = forest.select('gain')

accum = pd.DataFrame()
for y in range(0, 16):
    loss = baseforest.where(forest.select('loss').lte(i), 1).where(forest.select('loss').gt(i), 0).where(forest.select('loss').eq(0), 0)
    
    if y > 7:
        ygain = gain
    else:
        ygain = gain.where(gain.eq(1), 0)
    
    yforest = baseforest.subtract(loss).add(gain)
    yforest = yforest.where(yforest.gt(1), 1).where(yforest.lt(0), 0)
    
    sel = data[data['interview_year'] == (2000 + y)]
    
    #Make buffered points
    points = []
    for row in sel.iterrows():
        if not (row[1]['longitude']==0) & (row[1]['latitude']==0):
            geom = ee.Geometry.Point(row[1]['longitude'], row[1]['latitude']).buffer(25000)
            feat = ee.Feature(geom, {'code':row[1]['code']})
            points.append(feat)
            
    #Make features
    feat = []
    i = 0
    while i < len(points):
        j = i + 100
        fc = ee.FeatureCollection(points[i:j])
        feat.append(fc)
        i = j
    
    ext = map(lambda(x): yforest.reduceRegions(reducer=ee.Reducer.mean(), collection=x).getInfo(), feat)
    
    for featclass in ext10:
        feats = featclass['features']
        for f in feats:
            code = f['properties']['code']
            mean = f['properties']['mean']
            accum = accum.append(pd.DataFrame({'code': code, 'mean': mean, 'interview_year': (2000 + y)}, index = [0]))

    time.sleep(60)

accum.to_csv('g:\\My Drive\DHS Processed\forest_cover.csv')

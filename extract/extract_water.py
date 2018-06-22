import ee
import pandas as pd
import time

ee.Initialize()

buffersize = 15000

data = pd.read_csv('sp_export.csv')

data = data[['code', 'latitude', 'longitude', 'interview_year']].drop_duplicates()

def tryReduceRegions(raster, feature):
    try:
        reduction = raster.reduceRegions(reducer=ee.Reducer.mean(), collection=feature).getInfo()
        time.sleep(1)
    except ee.ee_exception.EEException:
        print('Memory Exceeded, waiting')
        time.sleep(60*5)
        reduction = tryReduceRegions(raster, feature)
    return reduction

seas_accum = pd.DataFrame()
perm_accum = pd.DataFrame()
for y in range(4, 32):
    year = y + 1984
    
    print("****************************\nNow Running Year " + str(year) + "\n****************************")
    
    water = ee.Image("JRC/GSW1_0/YearlyHistory/" + str(y))
    
    seasonal = water.where(water.eq(2), 1).where(water.neq(2), 0).mask(0)
    
    permanent = water.where(water.eq(3), 1).where(water.neq(3), 0).mask(0)
    
    if year == 2015:
        sel = data[data['interview_year'] >= year]
    else:
        sel = data[data['interview_year'] == year]
    
    print("Make buffered points")
    points = []
    for row in sel.iterrows():
        if not (row[1]['longitude']==0) & (row[1]['latitude']==0):
            geom = ee.Geometry.Point(row[1]['longitude'], row[1]['latitude']).buffer(buffersize)
            feat = ee.Feature(geom, {'code':row[1]['code']})
            points.append(feat)
    
    print("Make features")
    feat = []
    i = 0
    while i < len(points):
        j = i + 50
        fc = ee.FeatureCollection(points[i:j])
        feat.append(fc)
        i = j
    
    print("Reduce regions: Permanent")
    permsummary = []
    for f in feat:
        permsummary.append(tryReduceRegions(permanent, f))
        print(str(feat.index(f)) + " of " + str(len(feat)) + " in " + str(year))
    
    print("Reduce regions: Seasonal")
    seassummary = []
    for f in feat:
        seassummary.append(tryReduceRegions(seasonal, f))
        print(str(feat.index(f)) + " of " + str(len(feat)) + " in " + str(year))
    
    print("Add to DF")
    for featclass in permsummary:
        feats = featclass['features']
        for f in feats:
            code = f['properties']['code']
            if 'mean' in f['properties']:
                mean = f['properties']['mean']
            else:
                mean = 0
            perm_accum = perm_accum.append(pd.DataFrame({'code': code, 'perm_mean': mean, 'interview_year': year}, index = [0]))
    
    for featclass in seassummary:
        feats = featclass['features']
        for f in feats:
            code = f['properties']['code']
            if 'mean' in f['properties']:
                mean = f['properties']['mean']
            else:
                mean = 0
            seas_accum = seas_accum.append(pd.DataFrame({'code': code, 'seas_mean': mean, 'interview_year': year}, index = [0]))
    
    time.sleep(60)

perm_accum_proc = perm_accum.drop_duplicates().groupby('code', as_index=False)['perm_mean'].mean().rename(columns = {'mean': 'water_perm_' + str(buffersize)})
seas_accum_proc = seas_accum.drop_duplicates().groupby('code', as_index=False)['seas_mean'].mean().rename(columns = {'mean': 'water_seas_' + str(buffersize)})

accum = pd.merge(perm_accum_proc, seas_accum_proc, how='inner', on=None)

accum.to_csv('water' + str(buffersize) + '.csv', index=False)

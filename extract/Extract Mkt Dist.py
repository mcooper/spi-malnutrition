import ee
import pandas as pd
import numpy as np

out = pd.read_csv("G:\\My Drive\DHS Processed\sp_export.csv")

out = out[['code', 'latitude', 'longitude']].drop_duplicates()

ee.Initialize()

points = []
for row in out.iterrows():
    if not (row[1]['longitude']==0) & (row[1]['latitude']==0):
        geom = ee.Geometry.Point(row[1]['longitude'], row[1]['latitude'])
        feat = ee.Feature(geom, {'code':row[1]['code']})
        points.append(feat)

features = []
i = 0
while i < len(points):
    j = i + 3000
    fc = ee.FeatureCollection(points[i:j])
    features.append(fc)
    i = j

###############################
#Get market distance
#############################
mkt2000 = ee.Image("users/mcooper/access_50k")
mkt2015 = ee.Image("Oxford/MAP/accessibility_to_cities_2015_v1_0")

mktr2000 = map(lambda(x): mkt2000.reduceRegions(reducer=ee.Reducer.mean(), collection=x).getInfo(), features)
mktr2015 = map(lambda(x): mkt2015.reduceRegions(reducer=ee.Reducer.mean(), collection=x).getInfo(), features)

mkt00accum = pd.DataFrame()   
for m in mktr2000:
    for i in m['features']:
        if 'mean' in i['properties']:
            temp = pd.DataFrame({'code': i['properties']['code'], 
                                 'market2000': i['properties']['mean']}, index=[0])
        else:
            temp = pd.DataFrame({'code': i['properties']['code'], 
                                 'market2000': np.nan}, index=[0])
        mkt00accum = mkt00accum.append(temp)

mkt15accum = pd.DataFrame()
for m in mktr2015:
    for i in m['features']:
        if 'mean' in i['properties']:
            temp = pd.DataFrame({'code': i['properties']['code'], 
                                 'market2015': i['properties']['mean']}, index=[0])
        else:
            temp = pd.DataFrame({'code': i['properties']['code'], 
                                 'market2015': np.nan}, index=[0])
        mkt15accum = mkt15accum.append(temp)


##################################
#Write
##################################

mktaccum = pd.merge(mkt00accum, mkt15accum)

mktaccum.to_csv("G:\\My Drive\DHS Processed\MarketDist.csv", index=False)
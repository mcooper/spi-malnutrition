import ee
import pandas as pd
import numpy as np

out = pd.read_csv("D:\Documents and Settings\mcooper\Github\spi-malnutrition\data\sp_export.csv")

out = out[['code', 'LATNUM', 'LONGNUM']].drop_duplicates()

ee.Initialize()

points = []
for row in out.iterrows():
    if not (row[1]['LONGNUM']==0) & (row[1]['LATNUM']==0):
        geom = ee.Geometry.Point(row[1]['LONGNUM'], row[1]['LATNUM'])
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
mkt = ee.Image("users/mcooper/access_50k")

mktr = map(lambda(x): mkt.reduceRegions(reducer=ee.Reducer.mean(), collection=x).getInfo(), features)

mktaccum = pd.DataFrame()
for m in mktr:
    for i in m['features']:
        if 'mean' in i['properties']:
            temp = pd.DataFrame({'code': i['properties']['code'], 
                                 'market': i['properties']['mean']}, index=[0])
        else:
            temp = pd.DataFrame({'code': i['properties']['code'], 
                                 'market': np.nan}, index=[0])
        mktaccum = mktaccum.append(temp)

##################################
#Write
##################################

mktaccum.to_csv("D:\Documents and Settings\mcooper\Github\spi-malnutrition\data\MarketDist.csv", index=False)
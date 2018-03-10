import ee
import pandas as pd
import numpy as np

out = pd.read_csv("G:\\My Drive\DHS Processed\sp_export.csv")

out = out[['code', 'latitude', 'longitude']].drop_duplicates()

ee.Initialize()

points = []
for row in out.iterrows():
    if not (row[1]['longitude']==0) & (row[1]['latitude']==0):
        geom = ee.Geometry.Point(row[1]['longitude'], row[1]['latitude']).buffer(5642) #A radius of 5642 yields a total area of 100 km sq
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
#Population Density
#############################
pop2000 = ee.Image("CIESIN/GPWv4/population-count/2000")
pop2005 = ee.Image("CIESIN/GPWv4/population-count/2005")
pop2010 = ee.Image("CIESIN/GPWv4/population-count/2010")
pop2015 = ee.Image("CIESIN/GPWv4/population-count/2015")


pop00r = map(lambda(x): pop2000.reduceRegions(reducer=ee.Reducer.sum(), collection=x).getInfo(), features)
pop05r = map(lambda(x): pop2005.reduceRegions(reducer=ee.Reducer.sum(), collection=x).getInfo(), features)
pop10r = map(lambda(x): pop2010.reduceRegions(reducer=ee.Reducer.sum(), collection=x).getInfo(), features)
pop15r = map(lambda(x): pop2015.reduceRegions(reducer=ee.Reducer.sum(), collection=x).getInfo(), features)

pop00accum = pd.DataFrame()   
for p in pop00r:
    for i in p['features']:
        if 'sum' in i['properties']:
            temp = pd.DataFrame({'code': i['properties']['code'], 
                                 'pop00': i['properties']['sum']}, index=[0])
        else:
            temp = pd.DataFrame({'code': i['properties']['code'], 
                                 'pop00': np.nan}, index=[0])
        pop00accum = pop00accum.append(temp)

pop05accum = pd.DataFrame()   
for p in pop05r:
    for i in p['features']:
        if 'sum' in i['properties']:
            temp = pd.DataFrame({'code': i['properties']['code'], 
                                 'pop05': i['properties']['sum']}, index=[0])
        else:
            temp = pd.DataFrame({'code': i['properties']['code'], 
                                 'pop05': np.nan}, index=[0])
        pop05accum = pop05accum.append(temp)

pop10accum = pd.DataFrame()   
for p in pop10r:
    for i in p['features']:
        if 'sum' in i['properties']:
            temp = pd.DataFrame({'code': i['properties']['code'], 
                                 'pop10': i['properties']['sum']}, index=[0])
        else:
            temp = pd.DataFrame({'code': i['properties']['code'], 
                                 'pop10': np.nan}, index=[0])
        pop10accum = pop10accum.append(temp)
        
pop15accum = pd.DataFrame()   
for p in pop15r:
    for i in p['features']:
        if 'sum' in i['properties']:
            temp = pd.DataFrame({'code': i['properties']['code'], 
                                 'pop15': i['properties']['sum']}, index=[0])
        else:
            temp = pd.DataFrame({'code': i['properties']['code'], 
                                 'pop15': np.nan}, index=[0])
        pop15accum = pop15accum.append(temp)
        
##################################
#Write
##################################
from functools import reduce

popaccum = reduce(pd.merge, [pop00accum, pop05accum, pop10accum, pop15accum])

popaccum.to_csv("G:\\My Drive\DHS Processed\PopPer100sqkm.csv", index=False)
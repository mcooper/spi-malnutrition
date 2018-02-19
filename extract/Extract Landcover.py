import ee
import pandas as pd

out = pd.read_csv("D:\Documents and Settings\mcooper\Github\spi-malnutrition\data\sp_export.csv")

out = out[['code', 'latitude', 'longitude']].drop_duplicates()

ee.Initialize()

def rename_dict(pref, d):
    for i in d.keys():
        d[pref + i] = d.pop(i)
    return(d)

def merge_dicts(*dicts):
    superdict = {}
    for d in dicts:
        for k, v in d.iteritems():
            superdict[k] = v
    return(superdict)

cciaccum = pd.DataFrame()
for y in out.year.unique():
    print(y)
    
    cci = ee.Image("users/geflanddegradation/lcov/ESACCI-LC-L4-LCCS-Map-300m-P1Y-" + str(y) + "-v207")
    
    sel = out[[out['year'] == y]]
    
    points = []
    for row in sel.iterrows():
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
    
    ccir = map(lambda(x): cci.reduceRegions(reducer=ee.Reducer.frequencyHistogram(), collection=x).getInfo(), features)
    
    for f in ccir:
        for i in f['features']:
            temp = pd.DataFrame(merge_dicts(rename_dict('cci_', i['properties']['histogram']),
                                            {'hh_refno': i['properties']['hh_refno']},
                                            {'year': i['properties']['year']}), index = [0])
            cciaccum = cciaccum.append(temp)




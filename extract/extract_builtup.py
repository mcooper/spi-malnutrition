import ee
import pandas as pd
import time

ee.Initialize()

buffersize = 25000

data = pd.read_csv('sp_export.csv')

data = data[['code', 'latitude', 'longitude', 'interview_year']].drop_duplicates()

#test
#data = data[1:100]

builtup90 = ee.Image('JRC/GHSL/P2016/BUILT_LDS_GLOBE_V1/1990')
builtup00 = ee.Image('JRC/GHSL/P2016/BUILT_LDS_GLOBE_V1/2000')
builtup14 = ee.Image('JRC/GHSL/P2016/BUILT_LDS_GLOBE_V1/2014')

def tryReduceRegions(raster, feature):
    try:
        reduction = raster.reduceRegions(reducer=ee.Reducer.mean(), collection=feature).getInfo()
        time.sleep(1)
    except ee.ee_exception.EEException:
        print('Memory Exceeded, waiting')
        time.sleep(60*5)
        reduction = tryReduceRegions(raster, feature)
    return reduction

accum = []
for y in ['1990', '2000', '2014']:
    print("****************************\nNow Running Year " + y + "\n****************************")
    
    if y == '1990':
        built = builtup90
    elif y == '2000':
        built = builtup00
    elif y == '2014':
        built = builtup14
    else:
        raise Exception('Dude where\'s my car?')
    
    print("Make buffered points")
    points = []
    for row in data.iterrows():
        if not (row[1]['longitude']==0) & (row[1]['latitude']==0):
            geom = ee.Geometry.Point(row[1]['longitude'], row[1]['latitude']).buffer(buffersize)
            feat = ee.Feature(geom, {'code':row[1]['code'], 'interview_year': row[1]['interview_year']})
            points.append(feat)
    
    print("Make features")
    feat = []
    i = 0
    while i < len(points):
        j = i + 50
        fc = ee.FeatureCollection(points[i:j])
        feat.append(fc)
        i = j
    
    print("Reduce regions")
    summary = []
    for f in feat:
        summary.append(tryReduceRegions(built, f))
        print(str(feat.index(f)) + " of " + str(len(feat)))
    
    print("Add to DF")
    yaccum = pd.DataFrame()
    for featclass in summary:   
        feats = featclass['features']
        for f in feats:
            code = f['properties']['code']
            mean = f['properties']['mean']
            interview_year = f['properties']['interview_year']
            yaccum = yaccum.append(pd.DataFrame({'code': code, ('mean' + y): mean, 'interview_year': interview_year}, index = [0]))
    
    accum.append(yaccum)
    
    time.sleep(60)

alldf = reduce(lambda left,right: pd.merge(left,right,on=['code', 'interview_year']), accum)

def overallmean(interview_year, mean1990, mean2000, mean2014):
    if interview_year <= 1990:
        return(mean1990)
    elif interview_year == 2000:
        return(mean2000)
    elif interview_year >= 2014:
        return(mean2014)
    elif interview_year > 1990 & interview_year < 2000:
        val = ((2000 - interview_year)*mean1990 + (interview_year - 1990)*mean2000)/10.0
        return(val)
    elif interview_year > 2000 & interview_year < 2014:
        val = ((2014 - interview_year)*mean2000 + (interview_year - 2000)*mean2014)/14.0
        return(val)
    else:
        raise Exception('WTF Happened?')

alldf['mean'] = alldf.apply(lambda row: overallmean(row['interview_year'], row['mean1990'], row['mean2000'], row['mean2014']), axis=1)

alldf.drop_duplicates() \
.groupby('code', as_index=False)['mean'].mean() \
.rename(columns = {'mean': 'builtup_' + str(buffersize)}) \
.to_csv('builtup_' + str(buffersize) + '.csv', index=False)


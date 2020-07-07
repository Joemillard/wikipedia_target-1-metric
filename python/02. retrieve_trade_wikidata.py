import requests
import time
import pandas as pd
import numpy as np
import json
from pandas.io.json import json_normalize

# read in the list of traded species
traded_species = pd.read_csv("C:/Users/joeym/Documents/PhD/Aims/Aim 3 - quantifying pollinator cultural value/wikipedia_target-1-metric/wikipedia_target-1-metric/data/class_wiki_indices/submission_2/globally_traded_species_Scheffers.csv")

print

# select the traded species column
traded_species = traded_species['Species']                     

# Get session for url query
S = requests.Session()

# write to result object
result = []

# request each species json data from the wiki api
for i in range(0, len(traded_species)):
    try:
        #print(traded_species)
        url = "https://en.wikipedia.org/w/api.php?action=query&prop=pageprops&ppprop=wikibase_item&redirects=1&titles=%s&format=json" % (traded_species[i])
        R = requests.get(url = url)
        DATA = R.json()
        DATA = DATA['query']['pages']

        DATA = json_normalize(DATA)
        DATA.rename(columns = {DATA.columns[0]: "ns", DATA.columns[1]: "wikipedia_id",DATA.columns[2]: "qwiki_id",DATA.columns[3]: "title"}, inplace = True)

        print(DATA)
        result.append(DATA)
    except IndexError:
        DATA = pd.DataFrame({'ns':[''],'wikipedia_id':[''], 'qwiki_id':[''], 'title':[traded_species[i]]})
        result.append(DATA)

final = pd.concat(result)
save_loc = 'C:/Users/joeym/Documents/PhD/Aims/Aim 3 - quantifying pollinator cultural value/wikipedia_target-1-metric/wikipedia_target-1-metric/data/class_wiki_indices/submission_2/traded_species_wikidata.csv'
final.to_csv(save_loc, sep = ',', encoding = 'utf-8-sig')
    
    

import requests
import time
import pandas as pd
import numpy as np
import json
from pandas.io.json import json_normalize

# read in the list of traded species
# traded_species = pd.read_csv("C:/Users/joeym/Documents/PhD/Aims/Aim 3 - quantifying pollinator cultural value/wikipedia_target-1-metric/wikipedia_target-1-metric/data/class_wiki_indices/submission_2/fished_species_wikidata.csv")
traded_species = pd.read_csv("C:/Users/joeym/Documents/PhD/Aims/Aim 3 - quantifying pollinator cultural value/wikipedia_target-1-metric/wikipedia_target-1-metric/data/class_wiki_indices/submission_2/fished_species_wikidata_other_es.csv")

# try for each remaining language on API
languages = ['ru', 'pt', 'it', 'ar', 'ja']

# iterate through each remaining language
for l in range(0, len(languages)):

    if l > 0:

        # read in the new version of fish data after downloading species from pervious set
        read_loc = 'C:/Users/joeym/Documents/PhD/Aims/Aim 3 - quantifying pollinator cultural value/wikipedia_target-1-metric/wikipedia_target-1-metric/data/class_wiki_indices/submission_2/fished_species_wikidata_other_%s.csv' % (languages[l-1])

        traded_species = pd.read_csv(read_loc)

        # remove all those that have already been identified
        traded_species = traded_species[traded_species['qwiki_id'].isnull()]
        traded_species.index = range(len(traded_species.index))
        #print(final)

        # select the traded species column
        traded_species = traded_species['title']                     

        # Get session for url query
        S = requests.Session()

        # write to result object
        result = []

        # request each species json data from the wiki api
        for i in range(0, len(traded_species)):
            try:
                #print(traded_species)
                url = "https://%s.wikipedia.org/w/api.php?action=query&prop=pageprops&ppprop=wikibase_item&redirects=1&titles=%s&format=json" % (languages[l], traded_species[i])
                R = requests.get(url = url)
                DATA = R.json()
                DATA = DATA['query']['pages']

                DATA = json_normalize(DATA)
                DATA.rename(columns = {DATA.columns[0]: "ns", DATA.columns[1]: "wikipedia_id",DATA.columns[2]: "qwiki_id", DATA.columns[3]: "title"}, inplace = True)

                print(DATA)
                result.append(DATA)

            except IndexError:
                DATA = pd.DataFrame({'ns':[''],'wikipedia_id':[''], 'qwiki_id':[''], 'title':[traded_species[i]]})
                result.append(DATA)

            except KeyError:
                DATA = pd.DataFrame({'ns':[''],'wikipedia_id':[''], 'qwiki_id':[''], 'title':[traded_species[i]]})
                result.append(DATA)

        final = pd.concat(result)
        save_loc = 'C:/Users/joeym/Documents/PhD/Aims/Aim 3 - quantifying pollinator cultural value/wikipedia_target-1-metric/wikipedia_target-1-metric/data/class_wiki_indices/submission_2/fished_species_wikidata_other_%s.csv' % (languages[l])
        final.to_csv(save_loc, sep = ',', encoding = 'utf-8-sig')

    else: 
    
        # remove all those that have already been identified
        traded_species = traded_species[traded_species['qwiki_id'].isnull()]
        traded_species.index = range(len(traded_species.index))
        print(traded_species)

        # select the traded species column
        traded_species = traded_species['title']                     

        # Get session for url query
        S = requests.Session()

        # write to result object
        result = []

        # request each species json data from the wiki api
        for i in range(0, len(traded_species)):
            try:
                #print(traded_species)
                url = "https://%s.wikipedia.org/w/api.php?action=query&prop=pageprops&ppprop=wikibase_item&redirects=1&titles=%s&format=json" % (languages[l], traded_species[i])
                R = requests.get(url = url)
                DATA = R.json()
                DATA = DATA['query']['pages']

                DATA = json_normalize(DATA)
                DATA.rename(columns = {DATA.columns[0]: "ns", DATA.columns[1]: "wikipedia_id",DATA.columns[2]: "qwiki_id", DATA.columns[3]: "title"}, inplace = True)

                print(DATA)
                result.append(DATA)

            except IndexError:
                DATA = pd.DataFrame({'ns':[''],'wikipedia_id':[''], 'qwiki_id':[''], 'title':[traded_species[i]]})
                result.append(DATA)

            except KeyError:
                DATA = pd.DataFrame({'ns':[''],'wikipedia_id':[''], 'qwiki_id':[''], 'title':[traded_species[i]]})
                result.append(DATA)

        final = pd.concat(result)
        save_loc = 'C:/Users/joeym/Documents/PhD/Aims/Aim 3 - quantifying pollinator cultural value/wikipedia_target-1-metric/wikipedia_target-1-metric/data/class_wiki_indices/submission_2/fished_species_wikidata_other_%s.csv' % (languages[l])
        final.to_csv(save_loc, sep = ',', encoding = 'utf-8-sig')


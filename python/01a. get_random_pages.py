# script for retrieving random pages across languages

import requests
import json
import time
import pandas as pd
import numpy as np
from pandas.io.json import json_normalize

# set up empty dataframe
result = []

# set the sleep period
sleep_period = 0.1

# Get session for url query
S = requests.Session()

# languages to get views for
languages = ['en', 'zh', 'fr', 'de', 'es', 'ru', 'pt', 'it', 'ar', 'ja']

# loop through each of the languages
for l in range(0, len(languages)):

    # Base url
    URL = "https://%s.wikipedia.org/w/api.php" % languages[l]

    # Parameters for random pages
    PARAMS = {
        "action":"query",
        "format":"json",
        "list": "random",
        "rnlimit": "500",
        "rnnamespace": "0"
    }

    for i in range(0, 12):
        # Get wiki article titles
        R = S.get(url=URL, params=PARAMS)
        DATA = R.json()

        # Convert json data from wikipedia into tuples and insert into DB
        for data in DATA["query"]["random"]:
            wiki_id = data["id"]
            wiki_ns = data["ns"]
            wiki_title = data["title"]
            dat = pd.DataFrame({'wiki_id':[wiki_id], 'wiki_ns':[wiki_ns], 'wiki_title':[wiki_title], 'language':[languages[l]]})
            result.append(dat)

            # Sleep so that we're not hammering wikipedia
            time.sleep(sleep_period)

    # print the language just completed
    print(languages[l])

final = pd.concat(result)
save_loc = 'C:/Users/joeym/Documents/PhD/Aims/Aim 3 - quantifying pollinator cultural value/wikipedia_target-1-metric/wikipedia_target-1-metric/data/class_wiki_indices/submission_2/random_pages.csv'
final.to_csv(save_loc, sep = ',', encoding = 'utf-8-sig')

    

        


 

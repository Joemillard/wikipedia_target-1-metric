# script for retrieving views from IUCN species object and random page object

import requests
import json
import time
import pandas as pd
import numpy as np
from pandas.io.json import json_normalize

# Get session for url query
S = requests.Session()

insect_pages = pd.read_csv('C:/Users/joeym/Documents/PhD/Aims/Aim 3 - quantifying pollinator cultural value/wikipedia_data/view_data_by_class_totals_useronly/iucn_INSECTA_monthly_views_user.csv')
mammal_pages = pd.read_csv('C:/Users/joeym/Documents/PhD/Aims/Aim 3 - quantifying pollinator cultural value/wikipedia_data/view_data_by_class_totals_useronly/iucn_MAMMALIA_monthly_views_user.csv')
bird_pages = pd.read_csv('C:/Users/joeym/Documents/PhD/Aims/Aim 3 - quantifying pollinator cultural value/wikipedia_data/view_data_by_class_totals_useronly/iucn_AVES_monthly_views_user.csv')
random_pages = pd.read_csv('C:/Users/joeym/Documents/PhD/Aims/Aim 3 - quantifying pollinator cultural value/wikipedia_v3_LPI/overall_views/random_monthly_trends.csv')

no_pages = 5000
sleep_period = 0.1

# define function for subsetting the random dataframe
# define function for subsetting the random dataframe
def build_rand(view_pages, no_pages):
    pages = pd.DataFrame(view_pages)
    pages.columns = ['article', 'year', 'month', 'views']
    pages['taxa'] = 'Random'
    pages = pages[['article']]
    pages = pages.drop_duplicates(subset = "article")
    pages = pages.head(no_pages)
    pages.index = range(len(pages.index))
    return(pages)

def build_taxa(data, no_pages):
    data = pd.DataFrame(data)
    taxa = data[['article']]
    taxa = taxa.drop_duplicates(subset = "article")
    taxa.index = range(len(taxa.index))
    return(taxa)

insect_pages = build_taxa(insect_pages, no_pages)
mammal_pages = build_taxa(mammal_pages, no_pages)
bird_pages = build_taxa(bird_pages, no_pages)
random_pages = build_rand(random_pages, no_pages)

taxa = [random_pages, insect_pages, mammal_pages, bird_pages]
taxa_strings = ["random", "insect", "mammal", "bird"]

# iterate through each taxa/random object, set up empty list, and then iterate each taxa object each article
for j in range(0, len(taxa)):
    result = []
    # write just headers to file
    for i in range(0, len(taxa[j]['article'])):
        try:
            # assign title at iteration to object, replace spaces, and retrieve views for URL
            title = taxa[j]['article'][i]
            title = title.replace(" ", "_")
            URL = "https://wikimedia.org/api/rest_v1/metrics/pageviews/per-article/en.wikipedia.org/all-access/user/%s/daily/20150701/20191130" % title
            R = S.get(url = URL)

            # clean up json file, append column for taxa object, and append to results[]
            DATA = R.json()
            DATA = DATA['items']
            DATA = json_normalize(DATA)
            result.append(DATA)
            # write data to file 
            time.sleep(sleep_period)
            print(i, j)

        # in event of error, insert row with article title
        except KeyError:
            
            df = pd.DataFrame({'':[''],'access':[''], 'agent':[''], 'article':[title], 'granularity':[''], 'project':[''],'timestamp':[''], 'views':['NA']})
            result.append(df)
            # write error row to file and don't append
            print(i, j, "ERROR")

    # concatenate all appended results to dataframe and write to csv for each subset
    final = pd.concat(result)
    taxa_level = taxa_strings[j]
    save_loc = 'C:/Users/joeym/Documents/PhD/Aims/Aim 3 - quantifying pollinator cultural value/wikipedia_data/view_data_by_class_totals_useronly/%s_user_trends.csv' % taxa_level
    final.to_csv(save_loc, sep = ',', encoding = 'utf-8-sig')
    

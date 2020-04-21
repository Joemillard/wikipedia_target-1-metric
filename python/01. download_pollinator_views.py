# script for retrieving views from IUCN species object and random page object

import requests
import time
import pandas as pd
import numpy as np
from pandas.io.json import json_normalize

# Get session for url query
S = requests.Session()

# read in the list of pages
# pages = pd.read_csv('C:/Users/joeym/Documents/PhD/Aims/Aim 3 - quantifying pollinator cultural value/wikipedia_target-1-metric/wikipedia_target-1-metric/data/class_wiki_indices/submission_2/all_iucn_titles.csv') # home PC
pages = pd.read_csv('C:/Users/Joseph Millard/Documents/PhD/Aims/Aim 3 - quantifying pollinator cultural value/wikipedia_target-1-metric/data/class_wiki_indices/submission_2/all_iucn_titles.csv') # CBER PC

# read in the random pages
# random_pages = pd.read_csv('C:/Users/joeym/Documents/PhD/Aims/Aim 3 - quantifying pollinator cultural value/wikipedia_target-1-metric/wikipedia_target-1-metric/data/class_wiki_indices/submission_2/random_pages.csv') # home PC
random_pages = pd.read_csv('C:/Users/Joseph Millard/Documents/PhD/Aims/Aim 3 - quantifying pollinator cultural value/wikipedia_target-1-metric/data/class_wiki_indices/submission_2/random_pages.csv') # CBER PC

# languages for views
# languages = ['en', 'zh', 'fr', 'de', 'es', 'ru', 'pt', 'it', 'ar', 'ja']
# languages = ['pt', 'it', 'ar'] # after time out on 20th 09:00
languages = ['it'] # to run from CBER PC

# set parameters for random pages and sleep
no_pages = 2
sleep_period = 1.5

# define function for subsetting the random dataframe
def build_rand(view_pages, no_pages):
    pages = pd.DataFrame(view_pages)
    # pages.columns = ['language', 'wiki_id', 'wiki_title']
    # pages['taxa'] = 'Random'
    pages = pages[['wiki_title', 'wiki_id']]
    pages = pages.rename(columns = {'wiki_title':'title', 'wiki_id':'q_wikidata'})
    # pages = pages.head(no_pages) #  add to subset for smaller sample
    pages.index = range(len(pages.index))
    return(pages)

def build_taxa(data, no_pages):
    data = pd.DataFrame(data)
    taxa = data[['title', 'q_wikidata']]
    taxa = taxa.drop_duplicates(subset = "q_wikidata")
    # taxa = taxa.head(no_pages) # add to subset pages for smaller sample
    taxa.index = range(len(taxa.index))
    return(taxa)

# split the pages up into the top 10 languages
for l in range(0, len(languages)):

    # print the current language
    print(languages[l])

    # filter for each language
    language_pages = pages[pages.site == (languages[l] + 'wiki')]
    language_random = random_pages[random_pages.language == languages[l]]

    # split the pages up into each class of interest
    actinopterygii = language_pages[language_pages.class_name == 'ACTINOPTERYGII']
    amphibia = language_pages[language_pages.class_name =='AMPHIBIA']
    aves = language_pages[language_pages.class_name == 'AVES']
    insecta = language_pages[language_pages.class_name == 'INSECTA']
    mammalia = language_pages[language_pages.class_name == 'MAMMALIA']
    reptilia = language_pages[language_pages.class_name == 'REPTILIA']

    # filter for just the article paegs to search for views
    actinopterygii_pages = build_taxa(actinopterygii, no_pages)
    amphibia_pages = build_taxa(amphibia, no_pages)
    aves_pages = build_taxa(aves, no_pages)
    insecta_pages = build_taxa(insecta, no_pages)
    mammalia_pages = build_taxa(mammalia, no_pages)
    reptilia_pages = build_taxa(reptilia, no_pages)
    language_random = build_rand(language_random, no_pages)

    # create list of each taxa object, with string corresponding at each index
    # taxa = [actinopterygii_pages, amphibia_pages, aves_pages, insecta_pages, mammalia_pages, reptilia_pages, language_random]
    # taxa_strings = ["actinopterygii", "amphibia", "aves", "insecta", "mammalia", "reptilia", "random"]
    taxa = [language_random] # to run for final random italy file
    taxa_strings = ["random"] # to run for final random italy file

    # iterate through each taxa/random object, set up empty list, and then iterate each taxa object each article
    for j in range(0, len(taxa)):

        # print the current taxonomic order
        print(taxa_strings[j])

        # write to result object
        result = []

        # for each title, grab views from API
        for i in range(0, len(taxa[j]['title'])):

            # add counter for multiples of 100
            if i % 100 == 0:
                print(i)
            try:
                # assign title at iteration to object, replace spaces, and retrieve views for URL
                title = taxa[j]['title'][i]
                title = title.replace(" ", "_")
                URL = "https://wikimedia.org/api/rest_v1/metrics/pageviews/per-article/%s.wikipedia.org/all-access/user/%s/daily/20150701/20200331" % (languages[l], title)
                R = S.get(url = URL)

                # clean up json file, append column for taxa object, and append to results[]
                DATA = R.json()
                DATA = DATA['items']
                DATA = json_normalize(DATA)
                DATA['q_wikidata'] = taxa[j]['q_wikidata'][i]
                result.append(DATA)
                time.sleep(sleep_period)

            # in event of error, insert row with article title
            except KeyError:
            
                df = pd.DataFrame({'':[''],'access':[''], 'agent':[''], 'title':[title], 'granularity':[''], 'project':[''],'timestamp':[''], 'views':['NA'], 'q_wikidata':[taxa[j]['q_wikidata'][i]]})
                result.append(df)
                # write error row to file and don't append
                print(i, j, "ERROR")

        # concatenate all appended results to dataframe and write to csv for each subset
        final = pd.concat(result)
        taxa_level = taxa_strings[j]
        # save_loc = 'C:/Users/joeym/Documents/PhD/Aims/Aim 3 - quantifying pollinator cultural value/wikipedia_target-1-metric/wikipedia_target-1-metric/data/class_wiki_indices/submission_2/user_trends/%s%s_user_trends.csv' % ((languages[l] + '_'), (taxa_level + '_')) # Home PC
        save_loc = ('C:/Users/Joseph Millard/Documents/PhD/Aims/Aim 3 - quantifying pollinator cultural value/wikipedia_target-1-metric/data/class_wiki_indices/submission_2/user_trends/%s%s_user_trends.csv') % ((languages[l] + '_'), (taxa_level + '_')) # CBER PC
        final.to_csv(save_loc, sep = ',', encoding = 'utf-8-sig')
    

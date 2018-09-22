import pandas as pd
import numpy as np
import requests
import json
from requests_oauthlib import OAuth1Session
from operator import itemgetter
import matplotlib.pyplot as plt

##############################

# make a fatsecret api accout to get an access key

api_access_key = 'your_access_key'
api_consumer_key = api_access_key
api_shared_secret = 'your_shared_secret'

##############################

# nutrient, etc. fields to pull

brand_terms = ['brand_name','food_name','food_type']
nutrient_terms = ['calories','carbohydrate','fat','fiber','protein','saturated_fat','sugar','trans_fat']
serving_terms = ['measurement_description','metric_serving_amount','metric_serving_unit','number_of_units','serving_description']

##############################

# functions to handle API calls

# search the api for a given term
def search_foods(session, search_term, nutrient_terms, serving_terms, page_num, results_df):
    
    request_url = 'http://platform.fatsecret.com/rest/server.api?search_expression=' + \
    search_term.replace(' ','%20') + '&method=foods.search&max_results=50&format=json&page_number=' + page_num
    
    r = session.get(request_url)
    
    json_resp = json.loads(r.text)
    
    total_results = json_resp['foods']['total_results']
    page_no = json_resp['foods']['page_number']    
    
    food_list = json_resp['foods']['food'] 
    
    for f in food_list:
        food_id = f['food_id']
        food_data = get_food(session, food_id, nutrient_terms, serving_terms)
        results_df = results_df.append(food_data)
    
    if (int(page_no)+1)*50 < int(total_results):
        search_foods(session, search_term, nutrient_terms, serving_terms, str((int(page_no)+1)), results_df)
    
    return results_df
        

# get detailed information on a specific food
def get_food(session, food_id, nutrient_terms, serving_terms):
      
    request_url = 'http://platform.fatsecret.com/rest/server.api?food_id=' + \
    food_id + '&method=food.get&format=json'
    
    r = session.get(request_url)
    
    json_resp = json.loads(r.text)
    
    if 'food' in json_resp.keys():
          
        gen_data = json_resp['food']
        brand_data = dict((k, gen_data[k]) if k in gen_data else (k, '') for k in brand_terms)
        
        serve_data = json_resp['food']['servings']['serving']
                
        nutrient_data = dict((k, serve_data[k]) if k in serve_data else (k, np.nan) for k in nutrient_terms)
        serving_data = dict((k, serve_data[k]) if k in serve_data else (k, '') for k in serving_terms)
        
        nutrient_data.update(serving_data)
        nutrient_data.update(brand_data)
                
        ret_df = pd.DataFrame(nutrient_data, index=[0])
    
    else:
        
        print(food_id)
        ret_df = pd.DataFrame(columns = nutrient_terms + serving_terms + brand_terms)
    
    return ret_df


##############################
 
# set up a single Oauth session to sign and send requests
session = OAuth1Session(api_consumer_key, client_secret=api_shared_secret,signature_type='query')

# search term
search_term = 'clif bar' # e.g.

# initialize a result dataframe
results_df = pd.DataFrame(columns = nutrient_terms + serving_terms + brand_terms)

out = search_foods(session, search_term, nutrient_terms, serving_terms, '0', results_df)
for term in nutrient_terms:
    out[term] = out[term].apply(float)


##############################
    
# cleaning: steps in here will vary
# e.g.

# check for odd serving sizes
out = out[out.serving_description.str.contains('1 bar')]

# check for odd number of units, e.g. bulk sizes
print(np.unique(out.number_of_units))

# check for odd units
print(np.unique(out.metric_serving_unit))

# check for odd measurement descriptions
print(np.unique(out.measurement_description))

# check for outliers
plt.hist(out.calories)
plt.hist(out.metric_serving_amount)
plt.hist(out.carbohydrate)
plt.hist(out.protein)
plt.hist(out.fat)
plt.hist(out.fiber)
plt.hist(out.sugar)

# check for novelty versions
out = out[~out.food_name.str.lower().str.contains('fun size')]
out = out[~out.food_name.str.lower().str.contains('mini')]
out = out[~out.food_name.str.lower().str.contains('ice cream')] # for candy bars






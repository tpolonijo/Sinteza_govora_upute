# -*- coding: utf-8 -*-
"""
Created on Sun Feb 10 18:16:00 2019

@author: tina
"""

import os
import re

# Direktorij u kojem se nalaze txt fileovi koje treba urediti
path = 'txt_sm04'
dic =  {"~": "č", 
        "^": "ć", 
        "#": "dž",
        "}": "đ", 
        "{": "š", 
        "`": "ž"}

for filename in os.listdir(path):
    with open(path+'/'+filename, 'r') as f:
        text = f.read()
    f.close()
    
    # Zamijeni sve znakove po dictionaryju
    #for i, j in dic.iteritems():   #za python 2.x
    for i, j in dic.items():        #za python 3.x
        text = text.replace(i, j)
        
    # Ukloni sve tagove,kao <uzdah>, <papir>, <škripanje>...
    text = re.sub(r'<[a-ž]*>', '', text)
    
    with open(path+'/'+filename, 'w') as f:
        f.write(text)
    f.close()
    

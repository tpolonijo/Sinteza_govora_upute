# -*- coding: utf-8 -*-
 
import os
import re
import io
 
# Direktorij u kojem se nalaze txt fileovi koje treba urediti
path = 'txt'
dic =  {"č": "è", 
        "ć": "æ", 
        "dž": "dŸ",
        "đ": "ð", 
        "š": "¹", 
        "ž": "Ÿ"}

cnt = 0
for filename in os.listdir(path):
    try:
        with io.open((path+'/'+filename), encoding='UTF-8', mode="r") as f:
            text = f.read()
            print(text)
        f.close()
        # Zamijeni sve znakove po dictionaryju
        #for i, j in dic.iteritems():
        for i, j in dic.items():
            text = text.replace(i, j)
            
        # Ukloni sve tagove,kao <uzdah>, <papir>, <škripanje>...
        text = re.sub(r'<[a-ž]*>', '', text)
        print(text)
        with io.open((path+'/'+filename), encoding='ISO-8859-15', mode="w") as f:
            f.write(text)
        f.close()
    except:
        pass


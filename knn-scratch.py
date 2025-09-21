from collections import Counter
from matplotlib import style
import pandas as pd
import numpy as np 
import warnings
import random
import os

style.use('fivethirtyeight')

"""""
dataset = {
            'k':[[1,2],[2,3],[3,1]],
            'r':[[6,5],[7,7],[8,6]]
        }

new_features = [5,7]

[[plt.scatter(ii[0], ii[1], s=100, color=i) for ii in dataset[i]] for i in dataset]        
plt.scatter(new_features[0],new_features[1])
plt.show()

"""""

def knn(data, predict, k=3):
    if len(data) >= k:
        warnings.warn('idiot')
    
    distances = []
    for group in data:
        for features in data[group]:
            # euclidean_distance = np.sqrt(np.sum((np.array(features)-np.array(predict))**2))
            euclidean_distance = np.linalg.norm(np.array(features)-np.array(predict))
            distances.append([euclidean_distance, group])
            
    votes = [i[1] for i in sorted(distances)[:k]]
    
    vote_result = Counter(votes).most_common(1)[0][0]
    
    confidence = Counter(votes).most_common(1)[0][1] / k
        
    # print(vote_result,confidence)
    
    return vote_result,confidence

# loading the dataset 
df_path = os.path.dirname(os.path.abspath(__file__))
df_file = os.path.join(df_path, "breast-cancer-wisconsin.data")

df = pd.read_csv(df_file)
df.replace('?', -99999, inplace=True)
df.drop(['id'], axis=1, inplace=True)

full_data = df.astype(float).values.tolist()

print(full_data[:2])
random.shuffle(full_data)
print(full_data[:2])

split = 0.2
tr_set = {2:[],4:[]}
ts_set = {2:[],4:[]}

tr_data = full_data[ : -int(split * len(full_data))]
ts_data = full_data[-int(split * len(full_data)) : ]

for i in tr_data:
    tr_set[i[-1]].append(i[:-1])

for i in ts_data:
    ts_set[i[-1]].append(i[:-1])

total = 0
correct = 0

for group in ts_set:
    for data in ts_set[group]:
        vote, confidence = knn(tr_set, data, k=10)
        if group == vote:
            correct += 1
        else:
            print('confidence: ', confidence)
        total += 1

print('accuracy: ', correct/total)
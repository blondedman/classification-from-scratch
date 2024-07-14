from collections import Counter
import matplotlib.pyplot as plt
from matplotlib import style
from math import sqrt
import pandas as pd
import numpy as np 
import warnings
import random

style.use('fivethirtyeight')

dataset = {'k':[[1,2],[2,3],[3,1]],
           'r':[[6,5],[7,7],[8,6]]}

new_features = [5,7]

# [[plt.scatter(ii[0], ii[1], s=100, color=i) for ii in dataset[i]] for i in dataset]        
# plt.scatter(new_features[0],new_features[1])
# plt.show()

def k_n_n(data, predict, k=3):
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

result = k_n_n(dataset, new_features, k=3)

print(result)

df = pd.read_csv(r"C:\Users\HP\Desktop\projects\machine-learning\classification-from-scratch\breast-cancer-wisconsin.data")
df.replace('?',-99999,inplace=True)
df.drop(['id'],axis=1,inplace=True)

full_data = df.astype(float).values.tolist()

print(full_data[:3])
random.shuffle(full_data)
print(5 * '-')
print(full_data[:3])

test_size = 0.2
train_set = {2:[],4:[]}
test_set = {2:[],4:[]}

train_data = full_data[:-int(test_size*len(full_data))]
test_data = full_data[-int(test_size*len(full_data)):]

for i in train_data:
    train_set[i[-1]].append(i[:-1])
    
for i in test_data:
    test_set[i[-1]].append(i[:-1])

correct = 0
total = 0

for group in test_set:
    for data in test_set[group]:
        vote,confidence = k_n_n(train_set,data,k=10)
        if group == vote:
            correct += 1
        else:
            print(confidence)
        total += 1

print('accuracy: ', correct/total)
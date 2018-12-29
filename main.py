import tensorflow as tf
from tensorflow import keras

import numpy as np
import matplotlib.pyplot as plt

from pandas import read_csv
import pandas as pd

from numpy.random import seed
seed(1)

from tensorflow import set_random_seed
set_random_seed(2)

df = read_csv('Pokemon.csv')
df["Type 1"] = df["Type 1"].astype("category")
labels = df["Type 1"].cat.codes
indexes = np.random.rand(800) < 0.6

train = df[indexes]
test = df[~indexes]


train_stats = train[train.columns[[*range(5,11)]]]
labels_train = labels[indexes].values

test_stats = test[test.columns[[*range(5,11)]]]
labels_test = labels[~indexes].values

train_stats -= train_stats.min(0)
train_stats /= (train_stats.max() - train_stats.min(0))

test_stats -= test_stats.min(0)
test_stats /= (test_stats.max(0) - test_stats.min(0))

model = keras.Sequential([
    keras.layers.Dense(6),
    keras.layers.Dense(10, activation=tf.nn.relu),
    keras.layers.Dropout(0.3, seed=1),
    keras.layers.Dense(18, activation=tf.nn.softmax)
])

model.compile(optimizer=tf.train.AdamOptimizer(),
              loss='sparse_categorical_crossentropy',
              metrics=['accuracy'])

model.fit(train_stats.values, labels_train, epochs=2000)

#score = model.evaluate(test_stats.values, labels_test, batch_size=64)

predictions = model.predict(test_stats.values)

print(np.argmax(predictions, axis=1))
print(labels_test)
print(np.sum(labels_test == np.argmax(predictions, axis=1)) / len(labels_test))

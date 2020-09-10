# -*- coding: utf-8 -*-
"""TD2.ipynb

Automatically generated by Colaboratory.

Original file is located at
    https://colab.research.google.com/drive/1xA_1ZkTiehp2ivg7GT4t9L7q8UZ94kzd
"""

#https://machinelearningmastery.com/implement-backpropagation-algorithm-scratch-python/
#q1 : initialize a network
from random import *
from math import *

def initialize_network(n_inputs, n_hidden, n_outputs):
  network= list()
  hidden_layer = [{'weights':[random() for i in range(n_inputs + 1)]} for i in range(n_hidden)]
  network.append(hidden_layer)
  output_layer = [{'weights':[random() for i in range(n_hidden + 1)]} for i in range (n_outputs)]
  network.append(output_layer)
  return network

#q2
#test
seed (706974)
network = initialize_network(3,2,1)
for i in  range (len(network)):
  print (network[i])

#q3
def activate(weights, inputs):
  activation = 0
  for i in range (len(inputs)):
    activation += (weights[i] * inputs[i])
  activation += weights[-1]
  return activation

#q4
def transfer(activation):
  output = 1/(1 + exp(-activation))
  return output

#q5
# Forward propagate input to a network output
def forward_propagate(network, row):
	inputs = row
	for layer in network:
		new_inputs = []
		for neuron in layer:
			activation = activate(neuron['weights'], inputs)
			neuron['output'] = transfer(activation)
			new_inputs.append(neuron['output'])
		inputs = new_inputs
	return inputs

#q6
#test
network = initialize_network(3,2,2)
inputs = [1, 0, 2]
output = forward_propagate(network, inputs)
print(output)

#q7
def transfer_derivative(output):
  derivation = output * (1.0 - output)
  return derivation

#q8
def backward_propagate_error(network, expected):
	for i in reversed(range(len(network))):
		layer = network[i]
		errors = list()
		if i != len(network)-1:
			for j in range(len(layer)):
				error = 0.0
				for neuron in network[i + 1]:
					error += (neuron['weights'][j] * neuron['delta'])
				errors.append(error)
		else:
			for j in range(len(layer)):
				neuron = layer[j]
				errors.append(expected[j] - neuron['output'])
		for j in range(len(layer)):
			neuron = layer[j]
			neuron['delta'] = errors[j] * transfer_derivative(neuron['output'])

#q9
#test
network = [[{'output': 0.7105668883115941, 'weights': [0.13436424411240122, 0.8474337369372327, 0.763774618976614]}],
		[{'output': 0.6213859615555266, 'weights': [0.2550690257394217, 0.49543508709194095]}, {'output': 0.6573693455986976, 'weights': [0.4494910647887381, 0.651592972722763]}]]
expected = [0, 1]
backward_propagate_error(network, expected)
for i in range(len(network)):
	print(network[i])

#q10
def update_weights(network, row, l_rate):
  for i in range (len(network)):
    inputs = row[:-1]
    if i != 0:
      inputs = [neuron['output']for neuron in network[i-1]]
    for neuron in network[i]:
      for j in range (len(inputs)):
        neuron['weights'][j] += l_rate * neuron['delta'] * inputs[j]
      neuron['weights'][-1] += l_rate * neuron['delta']

#q11
def train_network(network, train, l_rate, n_epoch, n_outputs):
	for epoch in range(n_epoch):
		sum_error = 0
		for row in train:
			outputs = forward_propagate(network, row)
			expected = [0 for i in range(n_outputs)]
			expected[row[-1]] = 1
			sum_error += sum([(expected[i]-outputs[i])**2 for i in range(len(expected))])
			backward_propagate_error(network, expected)
			update_weights(network, row, l_rate)
		print('>epoch=%d, lrate=%.3f, error=%.3f' % (epoch, l_rate, sum_error))

#12
# Test
dataset=[[],[],[],[],[],[],[],[],[],[]]
for i in range (10):
  for j in range (3):
    if j == 2 :
      dataset[i].append(choice([0,1]))
    else :
      dataset[i].append(uniform(0,10))

for i in range(len(dataset)):
	print(dataset[i])
n_inputs = len(dataset[0]) - 1
n_outputs = len(set([row[-1] for row in dataset]))
network = initialize_network(n_inputs, 2, n_outputs)
train_network(network, dataset, 0.5, 20, n_outputs)
for i in range(len(network)):
  print(network[i])

#q13
def predict(network, row):
  outputs=forward_propagate(network, row)
  return outputs.index(max(outputs))

#q14
#test
dataset=[[],[],[],[],[],[],[],[],[],[]]
for i in range (10):
  for j in range (3):
    if j == 2 :
      dataset[i].append(choice([0,1]))
    else :
      dataset[i].append(uniform(0,10))


for i in range(len(dataset)):
	print(dataset[i])
n_inputs = len(dataset[0]) - 1
n_outputs = len(set([row[-1] for row in dataset]))
network = initialize_network(n_inputs, 2, n_outputs)
train_network(network, dataset, 0.5, 20, n_outputs)
for i in range(len(network)):
	print(network[i])

for row in dataset:
	prediction = predict(network, row)
	print('Expected=%d, Got=%d' % (row[-1], predict(network, row)))
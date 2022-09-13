from statistics import linear_regression
import pandas as pd
import numpy as np
from matplotlib import pyplot as plt
import matplotlib.colors as colors

class full_model:
  def __init__(self,a, b, c, h, n, learning_rate):
    self.a = a
    self.b = b
    self.c = c
    self.h = h
    self.n = n
    self.learning_rate = learning_rate

  def ev(self, transactions, domains):
    self.guess = ((self.c * transactions) + self.a) / ((domains ** self.n) + self.b) + self.h

  def backwards(self, transactions, domains, times ):
    a = self.a
    b = self.b
    c = self.c 
    h = self.h
    n = self.n
    t = transactions
    d = domains
    self.ev(transactions, domains)
    self.error = (times - self.guess)
    # self.a_grad = (2 * self.error * (1 / (domains ** n + b))).mean()
    # self.b_grad = (2 * self.error * -(c*t + a) / (d**(2*n) + 2*(d**n)*b + b**2)).mean()
    self.c_grad = (2 * self.error * (t / (d**n + b))).mean()
    # self.h_grad = (2 * self.error).mean()
    self.n_grad = (2 * self.error * (-(c*t + a)*(np.log(d)*(d**n)) / (d**(2*n) + 2*(d**n)*b + b**2))).mean()

  def backprop(self, transactions, domains, times):
    self.backwards(transactions, domains, times)
    # a_init = self.a
    # self.a -= self.a_grad * self.learning_rate
    # print(f"a diff is {a_init - self.a}")
    # self.b -= self.b_grad * self.learning_rate
    self.c -= self.c_grad * self.learning_rate
    h_init = self.a
    # self.h -= self.h_grad * self.learning_rate
    # print(f"h diff is {h_init - self.h}")
    # print(f"h grad is {self.h_grad}")
    n_init = self.n_grad 
    self.n -= self.n_grad * self.learning_rate
    print(f"n diff is {self.n_grad * self.learning_rate}")
    
  def learn(self, transactions, domains, times, rounds):
    # halfway = int(len(y_input)/2)
    # self.m = (y_input[halfway] - y_input[0])/ (x_input[halfway] - x_input[0])
    # self.b = y_input[0] - x_input[0] * self.m 
    self.a = 0
    self.n = 0
    for i in range(0, rounds):
      self.backprop(transactions, domains, times)
      self.ev(transactions, domains)

class Linear_model:
  def __init__(self, m, b, learning_rate):
    self.m = m
    self.b = b
    self.learning_rate = learning_rate

  def ev(self, x_input):
    self.guess = self.m * x_input + self.b 

  def backwards(self, x_input, y_input):
    self.ev(x_input)
    self.error = (y_input - self.guess) ** 2
    self.m_grad = -2 * ((y_input - self.guess) * x_input).mean()
    print(f"m grad is {self.m_grad}")
    self.b_grad = -2 * (y_input - self.guess).mean()
    print(f"b grad is {self.b_grad}")

  def backprop(self, x_input, y_input):
    self.backwards(x_input, y_input)
    print(f"old m is {self.m}")
    self.m -= self.m_grad * self.learning_rate
    print(f"new m is {self.m}")
    print(f"old b is {self.b}")
    self.b -= self.b_grad * self.learning_rate
    print(f"new b is {self.b}")
    
  def learn(self, x_input, y_input, rounds):
    # halfway = int(len(y_input)/2)
    # self.m = (y_input[halfway] - y_input[0])/ (x_input[halfway] - x_input[0])
    # self.b = y_input[0] - x_input[0] * self.m 
    self.m = 0 
    self.b = 0
    for i in range(1, rounds):
      print("went backwards")
      self.backprop(x_input, y_input)
      self.ev(x_input)
      plt.scatter(x_input, self.guess)
    self.ev(x_input)

import torch 
from torch.autograd import Variable

class Linear_model_pt(torch.nn.Module):
  def __init__(self, inputSize, outputSize):
    super(Linear_model_pt, self).__init__()
    self.linear = torch.nn.Linear(inputSize, outputSize)
    
  def forward(self, x):
    out = self.linear(x)
    return out
  
  
  

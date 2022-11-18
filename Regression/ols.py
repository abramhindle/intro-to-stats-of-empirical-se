import numpy as np
import matplotlib.pyplot as plt

# y = a + b_0 * x_0 + b_1*x_1 + ... + b_n*x_n

trueb = np.matrix([[3],[1],[2],[3]])
# 3 + 1*x_0 + 2*x_1 + 3*x_2
ivars =  3
n = 100
X = np.ones((n,ivars+1))
print("Generate some columns of independent variables")
data = np.random.random((n,ivars))
print("data.shape ",data.shape)
X[:,1:(1+ivars)] = data

print("X.shape ",X.shape)
print("Now X has a column of ones and data in the rest")
noise = 0.1 * np.random.random((n,1))
print("calculate Y based on coeffecients of trueb and noise")
Y = np.matmul(X,trueb) + noise
print("Y.shape ", Y.shape)

# What does it look like
import seaborn
import pandas
dfX = pandas.DataFrame(X)
dfX['y'] = Y
# ry is rounded y (4 colours)
dfX['ry'] = np.round(Y)
seaborn.pairplot(dfX,hue='ry')
plt.show()

print("Now we want to OLS it!")
print("B = (X' X)^-1 * (X' * Y)") # 
Xt = np.transpose(X)
B = np.matmul( np.linalg.inv(np.matmul(Xt,X)), np.matmul(Xt,Y))
print("B.shape", B.shape)
print("B: ", B)
print("trueb - B", trueb - B)
print("trueb - B", trueb - B)
Yhat = np.matmul(X,B)
def rms(Y1,Y2):
   return np.sqrt(np.mean(np.array(Y1-Y2)** 2))
print("rms(Y,Yhat)", rms(Y,Yhat))

from sklearn.metrics import r2_score
print("r2_score(Y,Yhat)", r2_score(Y,Yhat))



dfX = pandas.DataFrame(X)
dfX['y'] = Yhat
# ry is rounded y (4 colours)
dfX['ry'] = np.round(Y) # yes the original Y
seaborn.pairplot(dfX,hue='ry')
plt.show()



Bnoise = B + 0.25*np.random.random(B.shape)
print("Bnoise:",Bnoise)
print("B - Bnoise", B - Bnoise)
YhatNoise = np.matmul(X,Bnoise)
print("rms(Y,YhatNoise)", rms(Y,YhatNoise))
print("r2_score(Y,YhatNoise)", r2_score(Y,YhatNoise))


dfX = pandas.DataFrame(X)
dfX['y'] = YhatNoise
# ry is rounded y (4 colours)
dfX['ry'] = np.round(Y) # yes the original Y
seaborn.pairplot(dfX,hue='ry')
plt.show()

# Ok lets compose this better

def dataToX(data):
    (n,ivars) = data.shape
    X = np.ones((n,ivars+1))
    X[:,1:(1+ivars)] = data
    return X

def fitOLS(data,Y):
    X = dataToX(data)
    Xt = np.transpose(X)
    B = np.matmul( np.linalg.inv(np.matmul(Xt,X)), np.matmul(Xt,Y))
    return B

def predictOLS(data,B):
    X = dataToX(data)
    return np.matmul(X,B)    

noise = 5 * np.random.random((n,1))
print("calculate Y based on coeffecients of trueb and noise")
data = np.random.random((n,ivars))
X = dataToX(data)
Y = np.matmul(X,trueb) + noise

# Now we split 50/50
Ytrain = Y[0:50,0]
Ytest  = Y[50:,0]
dataTrain = data[0:50,:]
dataTest = data[50:,:]
Btrain = fitOLS(dataTrain, Ytrain)
Yhattrain = predictOLS(dataTrain,Btrain)
print("rms(Ytrain,Yhattrain)", rms(Ytrain,Yhattrain))
print("r2_score(Ytrain,Yhattrain)", r2_score(Ytrain,Yhattrain))

# Now we test on the test set
Yhattest = predictOLS(dataTest,Btrain)
print("rms(Ytest,Yhattest)", rms(Ytest,Yhattest))
print("r2_score(Ytest,Yhattest)", r2_score(Ytest,Yhattest))

def effectOfNoise( noiseCoef ):
    noise = noiseCoef * np.random.random((n,1))
    trueb = np.random.random((ivars+1,1))
    data = np.random.random((n,ivars))
    X = dataToX(data)
    Y = np.matmul(X,trueb) + noise
    Ytrain = Y[0:50,0]
    Ytest  = Y[50:,0]
    dataTrain = data[0:50,:]
    dataTest = data[50:,:]
    Btrain = fitOLS(dataTrain, Ytrain)
    Yhattrain = predictOLS(dataTrain,Btrain)
    Yhattest = predictOLS(dataTest,Btrain)
    return (r2_score(Ytrain,Yhattrain), r2_score(Ytest,Yhattest))

for noiseCoef in [0.0, 0.1, 0.5, 1.0, 2.0, 4.0, 8.0]:
    print(noiseCoef,
          np.mean([effectOfNoise( noiseCoef) for i in range(40)],axis=0))

# Lesson learned? Typically you do a lot worse on your test set
# Train set performance != test set performance
# More noise, harder to detect

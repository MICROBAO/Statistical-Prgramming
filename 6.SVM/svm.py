
# coding: utf-8
#Dongzhe Li(Ben)
#stat 202a
# In[45]:

import numpy as np
from scipy import linalg
import numpy as np
from scipy import linalg
import pandas as pd
import warnings
warnings.simplefilter('ignore', DeprecationWarning)


# In[46]:

np.random.seed(1)

def sigmoid(x):
    return 1 / (1 + np.exp(-x))


# In[49]:

def load_digits(subset=None, normalize=True):
    # load digits.csv, adopted from sklearn.
    import pandas as pd
    df = pd.read_csv("/Users/Dongzhe/Desktop/stat 202a/hw6/digits.csv")

    # only keep the numbers we want.
    if subset is not None:
        df = df[df.iloc[:,-1].isin(subset)]

    # convert to numpy arrays.
    digits = df.iloc[:,:-1].values.astype('float')
    labels = df.iloc[:,-1].values.astype('int')

    # Normalize digit values to 0 and 1.
    if normalize:
        digits -= digits.min()
        digits /= digits.max()

    # Change the labels to 0 and 1.
    for i in xrange(len(subset)):
        labels[labels == subset[i]] = i

    labels = labels.reshape((labels.shape[0], 1))
    return digits, labels


# In[50]:

def split_samples(digits, labels):
    """Split the data into a training set (70%) and a testing set (30%)."""
    num_samples = digits.shape[0]
    num_training = round(num_samples * 0.7)
    indices = np.random.permutation(num_samples)
    training_idx, testing_idx = indices[:num_training], indices[num_training:]
    return (digits[training_idx], labels[training_idx],
            digits[testing_idx], labels[testing_idx])


# In[51]:

digits, labels = load_digits(subset=[1, 2], normalize=True)
training_digits, training_labels, testing_digits, testing_labels = split_samples(digits, labels)


# In[52]:

print(training_digits.shape, training_labels.shape, testing_digits.shape, testing_labels.shape )


# In[53]:

def mySVM(training_digits,training_labels,testing_digits, testing_labels,num_iterations=40,learning_rate=1e-1,lamda=0.01):
    
    training_digits=pd.DataFrame(training_digits)
    training_labels=pd.DataFrame(training_labels)
    
    testing_digits=pd.DataFrame(testing_digits)
    testing_labels=pd.DataFrame(testing_labels)
    
    n=training_digits.shape[0]
    p=training_digits.shape[1]+1
    
    intercept =pd.DataFrame(np.ones((n, 1)))
    A = pd.concat([intercept, training_digits], axis=1)
    
    ntest=testing_digits.shape[0]
    intercept1 =pd.DataFrame(np.ones((ntest, 1)))
    A1 = pd.concat([intercept1, testing_digits], axis=1)
    
    training_labels=2*training_labels-1
    testing_labels=2*testing_labels-1
    
    beta=np.zeros(p)
    beta=pd.DataFrame(beta)
    for i in range(num_iterations):
        score=np.dot(A,beta)
        db=score*training_labels<1
        dbeta=np.dot(np.ones(n),(np.tile(db*training_labels,p)*A)/n)
        dbeta_t=np.transpose(dbeta)
        dbeta_t=pd.DataFrame(dbeta_t)
        beta=beta+learning_rate*dbeta_t
        beta[1:]=beta[1:p]-lamda*beta[1:p]
  
    train_predict=np.sign(score)
    train_acc=np.mean(train_predict*training_labels)
    
    test_score=np.dot(A1,beta)
    test_predict=np.sign(test_score)
    test_acc=np.mean(test_predict*testing_labels)

    print " training accuracy is ",train_acc[0],", testing accuracy is",test_acc[0]
    #return(beta)


# In[54]:

mySVM(training_digits,training_labels,testing_digits, testing_labels)







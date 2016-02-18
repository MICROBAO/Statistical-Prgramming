


#stat 202a hw 6
#Dongzhe Li(Ben) 104434089
#adaboost

import numpy as np
from scipy import linalg
import numpy as np
from scipy import linalg
import pandas as pd
import warnings
warnings.simplefilter('ignore', DeprecationWarning)
import operator


# In[206]:


def accuracy(a,b):
    acc=np.mean(a==b)
   
    return(acc)


# In[207]:

def load_digits(subset=None, normalize=True):
    """
    Load digits and labels from digits.csv.

    Args:
        subset: A subset of digit from 0 to 9 to return.
                If not specified, all digits will be returned.
        normalize: Whether to normalize data values to between 0 and 1.

    Returns:
        digits: Digits data matrix of the subset specified.
                The shape is (n, p), where
                    n is the number of examples,
                    p is the dimension of features.
        labels: Labels of the digits in an (n, ) array.
                Each of label[i] is the label for data[i, :]
    """
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


# In[208]:

def split_samples(digits, labels):
    """Split the data into a training set (70%) and a testing set (30%)."""
    num_samples = digits.shape[0]
    num_training = round(num_samples * 0.7)
    indices = np.random.permutation(num_samples)
    training_idx, testing_idx = indices[:num_training], indices[num_training:]
    return (digits[training_idx], labels[training_idx],
            digits[testing_idx], labels[testing_idx])


# In[209]:

digits, labels = load_digits(subset=[1, 2], normalize=True)
training_digits, training_labels, testing_digits, testing_labels = split_samples(digits, labels)


# In[210]:

print(training_digits.shape, training_labels.shape, testing_digits.shape, testing_labels.shape )


# In[231]:

def myAdaboost(training_digits,training_labels,testing_digits, testing_labels,num_iterations=10):
    
    training_digits=pd.DataFrame(training_digits)
    training_labels=pd.DataFrame(training_labels)

    n=training_digits.shape[0]
    p=training_digits.shape[1]
    threshold=.8
    training_digits_h=training_digits>threshold
    #intercept =pd.DataFrame(np.ones((n, 1)))
    #A = pd.concat([intercept, training_digits], axis=1)
    
    training_labels=2*training_labels-1
    training_digits_h=training_digits*2-1
    
    testing_digits_h=testing_digits>threshold
    testing_digits_h=testing_digits_h*2-1
    testing_labels=testing_labels*2-1
    
    beta=np.zeros(p)
    beta=pd.DataFrame(beta)
    
    for i in range(num_iterations):        
        score=np.dot(training_digits_h,beta)
        w=np.exp(-training_labels*score)
        a=np.dot(np.ones(n),(np.tile(w*training_labels,p)*training_digits_h)/n)
        error=(1-a)/2
        
        index=min(enumerate(error), key=operator.itemgetter(1))
        beta.loc[index[0]]=beta.loc[index[0]]+np.log((1-error[index[0]])/(2*error[index[0]]))
        
    score_train=np.dot(training_digits_h,beta)
    train_predict=np.sign(score_train)
    training_acc=accuracy(train_predict,training_labels)
    
    score_test=np.dot(testing_digits_h,beta)
    test_predict=np.sign(score_test)
    test_acc=accuracy(test_predict,testing_labels)
    
    
    print " training accuracy is ",training_acc[0],"test accuracy is",test_acc
        


myAdaboost(training_digits,training_labels,testing_digits, testing_labels)


# In[ ]:









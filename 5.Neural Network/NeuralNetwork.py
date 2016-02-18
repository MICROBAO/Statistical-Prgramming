#dongzheli 104434089  mylogistic
#gradient descent learning of logistic regression and two-layer neural network
#moniter the training accuracy and testing accuracy.

import numpy as np
import matplotlib.pyplot as plt

np.random.seed(1)

def sigmoid(x):
    return 1 / (1 + np.exp(-x))

def dsigmoid(y):
    return y * (1.0 - y)


def loss_l2(p, y):
    """
    Use L2-norm (least squares error) as loss function.

        L = \sum_{i=1}^{n} (y_i - p_i)^2
    """
    loss = np.sum(np.square(y - p))
    dp = -(y - p)
    return loss, dp


def loss_neg_likelihood(p, y):
    """
    Average negative log likelihood as loss function.

        L  = - \sum_{i=1}{n} ( y_i \log p_i + (1-y_i) \log (1-p_i) ) / n
    """
    loss = -np.sum( y*np.log(p) + (1-y)*np.log(1-p) ) / p.shape[0]
    dy = (1 - y)/(1 - p) - (y / p)
    dy = dy / y.shape[0]
    return loss, dy


def accuracy(p, y):
    """
    Calculate the accuracy of predictions against truth labels.

        Accuracy = # of correct predictions / # of data.
    """
    return np.mean((p > 0.5) == (y == 1))


def train(X, y, num_hidden=100, num_classes=1, num_iterations=2000,
            loss_function=loss_l2,
            learning_rate=1e-2):

    n, p = X.shape

    # Initialize parameters.
    # Weights and bias two layers, random numbers centered at 0.
    W1 = 2*np.random.randn(p, num_hidden) - 1
    b1 = 2*np.random.randn(num_hidden, ) - 1
    W2 = 2*np.random.randn(num_hidden, num_classes) - 1
    b2 = 2*np.random.randn(num_classes, ) - 1

    all_loss = []
    accuracies = []

    for it in xrange(num_iterations):
        # Feed forward.
        Z, params1 = feed_forward(X, W1, b1)
        y_out, params2 = feed_forward(Z, W2, b2)

        # Calculate the loss.
        loss, dy = loss_function(y_out, y)

        # Back propagation.
        dZ, dW2, db2 = back_prop(dy, params2)
        dX, dW1, db1 = back_prop(dZ, params1)

        # Update parameters along the gradients.
        W1 -= (learning_rate * dW1)
        b1 -= (learning_rate * db1)
        W2 -= (learning_rate * dW2)
        b2 -= (learning_rate * db2)

        # Save loss and accuracy for plotting the curves.
        all_loss.append(loss)
        accuracies.append(accuracy(y_out, y))

        # Print progress every 100 iterations.
        if it % 100 == 0:
            print 'After iteration %d, loss = %f, accuracy = %f' % (
                it, loss, accuracies[-1])

    # Print the training accuracy at the very last.
    print 'Accuracy on training data: %f' % accuracies[-1]

    # Pack the trained parameters and return it.
    model = (W1, b1, W2, b2)
    return model, all_loss, accuracies

def test(X, model):
    W1, b1, W2, b2 = model
    Z, _ = feed_forward(X, W1, b1)
    y_predict, _ = feed_forward(Z, W2, b2)
    return y_predict

def feed_forward(X, W, b):
    
    #Feed forward a logistic regression layer.
    
    p = sigmoid(X.dot(W) + b)
    params = (X, W, b, p)
    return p, params


def back_prop(dy, params):
    """
    Back propagation of a logistic regression layer.
        dX: Derivatives to the input for BP to previous layers.
            Same shape of X: (n, p)

        dW: Derivatives to weights W at this layer.
            Same shape of W: (p, d)

        db: Derivatives to bias b at this layer.
            Same shape of b: (d, )
    """
    # Unpack params
    X, W, b, p = params

    dactivate = dsigmoid(p)

    dW = X.T.dot(dy * dactivate)
    db = np.sum(dy * dactivate, axis=0)
    dX = (dy * dactivate).dot(W.T)

    return dX, dW, db


# load data
def load_digits(subset=None, normalize=True):

    #Load digits and labels from digits.csv.

    # load digits.csv, adopted from sklearn.
    import pandas as pd
    df = pd.read_csv('digits.csv')

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


def display_samples(digits, labels, n_samples=5):
    """
    Display random samples from the training set for each label.
    """
    distinct_label = np.unique(labels)

    fig_rows = len(distinct_label)
    fig_cols = n_samples
    fig_num = 1

    fig = plt.figure(1)
    fig.suptitle('Random samples of training data')
    for label in distinct_label:
        # random choose samples to display
        choice = np.random.choice(np.ix_(labels == label)[0], n_samples)
        for idx in choice:
            ax = fig.add_subplot(fig_rows, fig_cols, fig_num)
            fig.subplots_adjust(wspace=0, hspace=0)
            ax.set_title(labels[idx])
            ax.imshow(digits[idx].reshape(8,8), cmap=plt.cm.gray_r)
            ax.axis('off')
            fig_num += 1
    plt.show()


def split_samples(digits, labels):
    """Split the data into a training set (70%) and a testing set (30%)."""
    num_samples = digits.shape[0]
    num_training = round(num_samples * 0.7)
    indices = np.random.permutation(num_samples)
    training_idx, testing_idx = indices[:num_training], indices[num_training:]
    return (digits[training_idx], labels[training_idx],
            digits[testing_idx], labels[testing_idx])


#====================================
# Load digits and labels.
digits, labels = load_digits(subset=[3, 5], normalize=True)
training_digits, training_labels, testing_digits, testing_labels = split_samples(digits, labels)
print '# training', training_digits.shape[0]
print '# testing', testing_digits.shape[0]

# Train a net and display training accuracy.
model, all_loss, all_accuracies = train(training_digits, training_labels, loss_function=loss_l2)

# Evaluate on the testing set.
y_predict = test(testing_digits, model)
testing_accuracy = accuracy(y_predict, testing_labels)
print 'Accuracy on testing data: %f' % testing_accuracy

# Uncomment the following line to visualize samples from data.
# display_samples(digits, labels)

# Uncomment the following lines to plot the curves.
# plt.figure(figsize=(10,5))
# plt.subplot(1, 2, 1)
# plt.ylabel('Loss')
# plt.xlabel('Iteration')
# plt.plot(range(len(all_loss)), all_loss)
# plt.subplot(1, 2, 2)
# plt.ylabel('Accuracy')
# plt.xlabel('Iteration')
# plt.plot(range(len(all_accuracies)), all_accuracies)
# plt.show()


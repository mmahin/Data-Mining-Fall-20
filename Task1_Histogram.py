import matplotlib.pyplot as plt

data = [0.62, 0.97, 0.98, 1.01, 1.02, 1.07, 2.96, 2.97, 2.99, 3.02, 3.03, 3.06, 4.96, 4.97, 4.98, 5.02, 5.03, 5.04]
num_bins = 'auto'
n, bins, patches = plt.hist(data, density=False, facecolor='blue', alpha=1)
plt.xlabel('Bins')
plt.ylabel('Samples')
plt.title('Histogram of Data')
plt.show()
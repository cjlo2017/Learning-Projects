#load dataset
url = "https://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data";
names = ['sepal-length', 'sepal-width', 'petal-length','petal-width','class']
dataset = pandas.readcsv(url,names=names);
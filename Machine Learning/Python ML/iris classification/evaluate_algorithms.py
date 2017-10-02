#split-out validation dataset
array = dataset.values
X = array[:,0:4];
Y = array[:,4];
validation_size = 0.20;
seed = 7;
X_train, X_validation, Y_train, Y_validation = model_selection.train_test_split(X,Y,test_size=validation_size, random_state=seed);


# test options and evaluation metric
seed = 7;
scoring = 'accuracy';


#building models 

#spot check algorithms
models = [];
models.append(('LR',LogisticRegression()));
models.append(('LDA',LinearDiscriminationAnalysis()))
models.append(('KNN',KNeighborsClassifier()))
models.append(('CART',DecisionTreeClassifier()))
models.append(('NB', GaussianNB()))
models.append(('SVM',SVC()))
#evaluate the models
results = []
names = []
for name, model in models:
	kfold = model_selection.KFold(n_splits=10, random_state=seed)
	cv_results = model_selection.cross_val_score(model, X_train,Y_train, cv=kfold, scoring=scoring)
	results.append(cv_results)
	names.append(name)
	msg = "%s %f(%f)" % (name, cv_results.mean(), cv_results.std())
	print(msg);
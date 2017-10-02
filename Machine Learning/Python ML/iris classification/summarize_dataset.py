#shape
print(dataset.shape);

#head
print(dataset.head(20));

#descriptions
print(dataset.describe());

#class distribution
print(dataset.groupby('class').size());

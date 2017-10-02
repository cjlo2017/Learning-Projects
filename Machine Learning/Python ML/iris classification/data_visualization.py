#box and whisker plots
dataset.plot(kind='box', subplots=True, layout=(2,2), sharex=False, sharey=False);
plt.show();

#histograms
dataset.hist();
plot.show();

#scatter plot matrix
scatter_matrix(dataset);
plot.show();


from sklearn.manifold import TSNE
import matplotlib
import matplotlib.pyplot as plt
import numpy as np
from datetime import datetime
import os

RANK_DIR = os.path.abspath(os.path.join(__file__, os.pardir, os.pardir))
PLOT_DIR = os.path.join(RANK_DIR, "images")


class Visualizer():
    def __init__(self, data, out_fname):
        self.data = data
        self.out_fname = out_fname

    def preprocess(self):
        return np.concatenate(self.data)

    def plot(self):
        """Plot TSNE results"""
        processed = self.preprocess()

        y = [0] * len(processed)
        y[-1] = 1
        X = TSNE(n_components=2, random_state=777).fit_transform(processed)
        plt.scatter(X[:, 0], X[:, 1], c=y)
        now = datetime.now()
        now_str = now.strftime("%y%m%d-%H:%M:%S")
        fname = self.out_fname + '_' + now_str + ".png"
        plt.savefig(os.path.join(PLOT_DIR, fname))
        plt.close()
        print(fname + " saved")

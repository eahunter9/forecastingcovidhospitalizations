
# fix python path if working locally
##from utils import fix_pythonpath_if_working_locally

#fix_pythonpath_if_working_locally()
#%load_ext autoreload
#%autoreload 2
#%matplotlib inline

# use darts plotting style
from darts import set_option

set_option("plotting.use_darts_style", True)
import warnings

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

from darts.dataprocessing.transformers import Scaler

from darts.dataprocessing import Pipeline
from darts.dataprocessing.transformers import (
    InvertibleMapper,
    Mapper,
    MissingValuesFiller,
    Scaler,
)
from darts.utils.timeseries_generation import linear_timeseries
from darts import TimeSeries

from darts.datasets import AirPassengersDataset, SunspotsDataset
from darts.metrics import mape
from darts.models import BlockRNNModel, ExponentialSmoothing, RNNModel, AutoARIMA
from darts.utils.callbacks import TFMProgressBar
from darts.utils.statistics import check_seasonality, plot_acf
from darts.utils.timeseries_generation import datetime_attribute_timeseries
from darts.utils.likelihood_models.torch import QuantileRegression

warnings.filterwarnings("ignore")
import logging
logging.disable(logging.CRITICAL)

dataset = pd.read_csv("C:/Users/Elizabeth/OneDrive - National University of Ireland, Galway/calibration/waningimmunity/COVIDweekly_all.csv")

#data_hp = dataset[["Hospperday"]][314:865]
data_hp = dataset[["Hospperday"]][700:865]
#data_hp = dataset[["Hospperday"]][314:900]
#data_hp = dataset[["Hospperday"]][700:900]
#data_hp = dataset[["Hospperday"]][700:906]
#data_hp = dataset[["Hospperday"]][314:906]


data_hp = TimeSeries.from_dataframe(data_hp)
#data_hp = scaler.fit_transform(data_hp)
#train = data_hp[0:537]
#val = data_hp[537:len(data_hp)]
train = data_hp[0:151]
val = data_hp[151:len(data_hp)]
#train = data_hp[0:566]
#val = data_hp[566:len(data_hp)]
#train = data_hp[0:191]
#val = data_hp[192:len(data_hp)]#
#train = data_hp[0:176]
#val = data_hp[177:len(data_hp)]#
#train = data_hp[0:170]
#val = data_hp[171:len(data_hp)]#

#train = data_hp[0:577]
#val = data_hp[578:len(data_hp)]#
print(len(train),len(val))


# Normalize the time series (note: we avoid fitting the transformer on the validation set)
transformer = Scaler()
train_transformed = transformer.fit_transform(train)
val_transformed = transformer.transform(val)
series_transformed = transformer.transform(data_hp)


arima_model =AutoARIMA(seasonal = True, trace = True, stepwise = True)
arima_model.fit(train_transformed)



def eval_model(model):
    pred_series = model.predict(n=14, num_samples=500)
    plt.figure(figsize=(8, 5))
    series_transformed.plot(label="actual")
    pred_series.plot(label="forecast")
    plt.title(f"MAPE: {mape(pred_series, val_transformed):.2f}%")
    plt.legend()

eval_model(arima_model)


pred_series = arima_model.predict(n=14, num_samples=500)
pred_lower = pred_series.quantile(0.025)
pred_lower = transformer.inverse_transform(pred_lower)

pred_upper = pred_series.quantile(0.975)
pred_upper = transformer.inverse_transform(pred_upper)

pred_median = pred_series.quantile(0.5)
pred_median = transformer.inverse_transform(pred_median)


params = arima_model.model.model_['arma']
p, q, P, Q, s, d, D = params
print(f"p: {p}, d: {d}, q: {q}, P: {P},D: {D}, Q: {Q}")



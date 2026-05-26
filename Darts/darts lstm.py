
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
from darts.models import BlockRNNModel, ExponentialSmoothing, RNNModel
from darts.utils.callbacks import TFMProgressBar
from darts.utils.statistics import check_seasonality, plot_acf
from darts.utils.timeseries_generation import datetime_attribute_timeseries
from darts.utils.likelihood_models.torch import QuantileRegression

warnings.filterwarnings("ignore")
import logging
logging.disable(logging.CRITICAL)

dataset = pd.read_csv("C:/Users/Elizabeth/OneDrive - National University of Ireland, Galway/calibration/waningimmunity/COVIDweekly_all.csv")

data_hp = dataset[["Hospperday"]][314:894]


data_hp = TimeSeries.from_dataframe(data_hp)

train = data_hp[0:566]
val = data_hp[566:len(data_hp)]

print(len(train),len(val))


# Normalize the time series (note: we avoid fitting the transformer on the validation set)
transformer = Scaler()
train_transformed = transformer.fit_transform(train)
val_transformed = transformer.transform(val)
series_transformed = transformer.transform(data_hp)

my_model = RNNModel(
    input_chunk_length=7,
    training_length=7,
    #training_length=100,
    model="LSTM",
    hidden_dim=5,
    dropout=0,
    batch_size=32,
    n_epochs=300,
    optimizer_kwargs={"lr": 1e-3},
    model_name="test_RNN",
    log_tensorboard=True,
    random_state=42,
    force_reset=True,
    save_checkpoints=True,
    likelihood=QuantileRegression(quantiles=[0.01, 0.05, 0.2, 0.5, 0.8, 0.95, 0.99]),
    pl_trainer_kwargs={"callbacks": [TFMProgressBar(enable_train_bar_only=True)]},
)

my_model.fit(
    train_transformed,
    val_series=val_transformed,
    verbose=True,
)



def eval_model(model):
    pred_series = model.predict(n=14, num_samples=500)
    plt.figure(figsize=(8, 5))
    series_transformed.plot(label="actual")
    pred_series.plot(label="forecast")
    plt.title(f"MAPE: {mape(pred_series, val_transformed):.2f}%")
    plt.legend()

eval_model(my_model)

pred_series = my_model.predict(n=14, num_samples=500)
pred_lower = pred_series.quantile(0.025)
pred_lower = transformer.inverse_transform(pred_lower)

pred_upper = pred_series.quantile(0.975)
pred_upper = transformer.inverse_transform(pred_upper)

pred_median = pred_series.quantile(0.5)
pred_median = transformer.inverse_transform(pred_median)

import torch
import numpy as np
import pandas as pd

from darts import TimeSeries
from darts.utils.losses import SmapeLoss
from darts.models import TCNModel, RNNModel, ExponentialSmoothing, BlockRNNModel, NBEATSModel, TFTModel, NHiTS
from darts.dataprocessing.transformers import Scaler
from darts.utils.timeseries_generation import datetime_attribute_timeseries
import warnings
from darts.utils.likelihood_models import GaussianLikelihood

warnings.filterwarnings("ignore")
import logging

logging.disable(logging.CRITICAL)


#We read the data
data           = pd.read_csv("datos-limpios/dengue_2015_2022_mx.csv")
data           = data.rename(columns = {"fecha": "date", "nraw": "value"})
data['date']   = pd.to_datetime(data['date'])


# Create a TimeSeries, specifying the time and value columns
series = TimeSeries.from_dataframe(data[["date","value"]], 'date', 'value')

weeks_to_predict = 52*2

# Set aside the last two year as a validation series
training_cutoff = pd.Timestamp("2020/01/01")
train, val = series.split_after(training_cutoff)
train.plot()
val.plot()

#COVARIATES
covariates = datetime_attribute_timeseries(series, attribute="year", one_hot=False)
covariates = covariates.stack(
    datetime_attribute_timeseries(series, attribute="month", one_hot=True)
)
covariates = covariates.stack(
    TimeSeries.from_times_and_values(
        times=series.time_index,
        values=np.arange(len(series)),
        columns=["linear_increase"],
    )
)

#TRANSFORM COVARIATES
scaler_covs = Scaler()
cov_train, cov_val = covariates.split_after(training_cutoff)
scaler_covs.fit(cov_train)
covariates_transformed = scaler_covs.transform(covariates)

# reproducibility
np.random.seed(230098)
torch.manual_seed(42095703)


deeptcn = NHiTS(
    input_chunk_length=100,
    output_chunk_length=10,
    num_stacks=3,
    num_blocks=1,
    dropout = 0.1,
    num_layers=5,
    layer_widths=512,
    batch_size=16,
    n_epochs=20,
    #likelihood = GaussianLikelihood(),
    add_encoders={'cyclic': {'future': ['month','year']}},
)

deeptcn.fit(train, verbose=True)

pred = deeptcn.predict(weeks_to_predict, num_samples=1000)
val.slice_intersect(pred).plot(label="target")
pred.plot(label="prediction")


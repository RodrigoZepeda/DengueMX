import torch
import numpy as np
import pandas as pd
import darts.utils.likelihood_models as Likelihood
from darts import TimeSeries
import darts.utils.losses as Loss
from darts.models import TCNModel, RNNModel, ExponentialSmoothing, BlockRNNModel, NBEATSModel, TFTModel, NHiTS
from darts.dataprocessing.transformers import Scaler
from darts.utils.timeseries_generation import datetime_attribute_timeseries
import warnings
from datetime import timedelta
warnings.filterwarnings("ignore")
import logging
import optuna as tuna
logging.disable(logging.CRITICAL)


#We read the data
data           = pd.read_csv("datos-limpios/dengue_2015_2022_mx.csv")
data           = data.rename(columns = {"fecha": "date", "logn": "value"})
data['date']   = pd.to_datetime(data['date'])

# Create a TimeSeries, specifying the time and value columns
series = TimeSeries.from_dataframe(data[["date","value"]], 'date', 'value')

weeks_to_predict = 52*2
training_cutoff = data.date.max() - timedelta(weeks=weeks_to_predict)

# Set aside the last two year as a validation series
train, val = series.split_after(training_cutoff)
#train.plot()
#val.plot()

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
    likelihood = Likelihood.GammaLikelihood(),
    loss_fn = Loss.SmapeLoss(),
    #optimizer_kwargs = {'lr': 1e-3},
    add_encoders={'cyclic': {'future': ['month','year','week']}},
)

deeptcn.fit(train, verbose=True)

#Prsent predict
#pred = deeptcn.predict(weeks_to_predict, num_samples=100)
#val.slice_intersect(pred).plot(label="target")
#pred.plot(label="prediction")

#Future predict
deeptcn.fit(series, verbose=True)
pred_2     = deeptcn.predict(weeks_to_predict, num_samples=1000)

prediction = pred_2.quantiles_df([0.05, 0.5, 0.95])
prediction[["lower_ci", "median", "upper_ci"]] = np.exp(prediction[["value_0.05", "value_0.5", "value_0.95"]])


pred_2.plot()
prediction.to_csv("predicciones_DARTS_experimento.csv")

import matplotlib.pyplot as plt
import pandas as pd
import pytorch_lightning as pl
from pytorch_lightning.callbacks import EarlyStopping
import torch
import numpy as np
from pytorch_forecasting import Baseline, DeepAR, TimeSeriesDataSet, NHiTS
from pytorch_forecasting.data import NaNLabelEncoder
from pytorch_forecasting.data.examples import generate_ar_data
from pytorch_forecasting.metrics import SMAPE, MultivariateNormalDistributionLoss
from pytorch_forecasting.metrics import SMAPE, MQF2DistributionLoss, QuantileLoss

#We read the data
data = pd.read_csv("datos-limpios/dengue_2015_2022_mx.csv")
data           = data.rename(columns = {"fecha": "date", "logn": "value"})
data['date']   = pd.to_datetime(data['date'])
data["series"] = int(1)
data["value"]  = data["value"].astype(float)
data.index     = data['date']
data           = data.astype(dict(series=str))
data["time_idx"] = (pd.to_datetime(data.date) - pd.to_datetime(min(data.date))).dt.days / 7
data["time_idx"] = data["time_idx"].astype(int)
data             = data[["series","time_idx","value"]]

# create dataset and dataloaders
max_encoder_length = 60
max_prediction_length = 20

training_cutoff = data["time_idx"].max() - max_prediction_length

context_length    = max_encoder_length
prediction_length = max_prediction_length

training = TimeSeriesDataSet(
    data[lambda x: x.time_idx <= training_cutoff],
    time_idx="time_idx",
    target="value",
    categorical_encoders={"series": NaNLabelEncoder().fit(data.series)},
    group_ids=["series"],
    static_categoricals=[
        "series"
    ],  # as we plan to forecast correlations, it is important to use series characteristics (e.g. a series identifier)
    time_varying_unknown_reals=["value"],
    max_encoder_length=context_length,
    max_prediction_length=prediction_length,
)

validation = TimeSeriesDataSet.from_dataset(training, data, min_prediction_idx=training_cutoff + 1)
batch_size = 128

# synchronize samples in each batch over time - only necessary for DeepVAR, not for DeepAR
train_dataloader = training.to_dataloader(
    train=True, batch_size=batch_size, num_workers=0, batch_sampler="synchronized"
)
val_dataloader = validation.to_dataloader(
    train=False, batch_size=batch_size, num_workers=0, batch_sampler="synchronized"
)

# calculate baseline absolute error
actuals = torch.cat([y[0] for x, y in iter(val_dataloader)])
baseline_predictions = Baseline().predict(val_dataloader)
SMAPE()(baseline_predictions, actuals)

pl.seed_everything(42)
import pytorch_forecasting as ptf

trainer = pl.Trainer(gpus=0, gradient_clip_val=1e-1)
net = NHiTS.from_dataset(
    training,
    learning_rate=3e-2,
    weight_decay=1e-2,
   # loss=MQF2DistributionLoss(prediction_length=max_prediction_length),
    backcast_loss_ratio=0.0,
    hidden_size=64,
)

# find optimal learning rate
res = trainer.tuner.lr_find(
    net,
    train_dataloaders=train_dataloader,
    val_dataloaders=val_dataloader,
    min_lr=1e-5,
    max_lr=1e0,
    early_stop_threshold=100,
)

print(f"suggested learning rate: {res.suggestion()}")
fig = res.plot(show=True, suggest=True)
fig.show()
net.hparams.learning_rate = res.suggestion()


early_stop_callback = EarlyStopping(monitor="val_loss", min_delta=1e-4, patience=10, verbose=False, mode="min")
trainer = pl.Trainer(
    max_epochs=30,
    gpus=0,
    weights_summary="top",
    gradient_clip_val=0.1,
    callbacks=[early_stop_callback],
    limit_train_batches=50,
    enable_checkpointing=True,
)


net = NHiTS.from_dataset(
    training,
    learning_rate=net.hparams.learning_rate,
    log_interval=10,
    log_val_interval=1,
    hidden_size=30,
    rnn_layers=2,
)

trainer.fit(
    net,
    train_dataloaders=train_dataloader,
    val_dataloaders=val_dataloader,
)

best_model_path = trainer.checkpoint_callback.best_model_path
best_model      = DeepAR.load_from_checkpoint(best_model_path)

actuals     = torch.cat([y[0] for x, y in iter(val_dataloader)])
predictions = best_model.predict(val_dataloader)
(actuals - predictions).abs().mean()

raw_predictions, x = net.predict(val_dataloader, mode="raw", return_x=True, n_samples=100)

series = validation.x_to_index(x)["series"]

best_model.plot_prediction(x, raw_predictions, idx=0, add_loss_to_title=True)
plt.suptitle(f"Series: {series.iloc[0]}")

#Create future predictions
predicting = TimeSeriesDataSet(
    data,
    time_idx="time_idx",
    target="value",
    categorical_encoders={"series": NaNLabelEncoder().fit(data.series)},
    group_ids=["series"],
    static_categoricals=[
        "series"
    ],
    time_varying_unknown_reals=["value"],
    max_encoder_length=context_length,
    max_prediction_length=prediction_length,
)


# synchronize samples in each batch over time - only necessary for DeepVAR, not for DeepAR
predict_dataloader = predicting.to_dataloader(
    train=False, batch_size=batch_size, num_workers=0, batch_sampler="synchronized"
)

trainer = pl.Trainer(
    max_epochs=30,
    gpus=0,
    weights_summary="top",
    gradient_clip_val=0.1,
    limit_train_batches=50,
    enable_checkpointing=True,
)

net = DeepAR.from_dataset(
    predicting,
    learning_rate=net.hparams.learning_rate,
    log_interval=10,
    log_val_interval=1,
    hidden_size=30,
    rnn_layers=2,
)

trainer.fit(
    net,
    train_dataloaders=predict_dataloader
)


best_model_path = trainer.checkpoint_callback.best_model_path
best_model      = DeepAR.load_from_checkpoint(best_model_path)

best_model.plot_prediction(x, , idx=0, add_loss_to_title=True)
plt.suptitle(f"Series: {series.iloc[0]}")



#Create prediction dataset
encoder_data = data[lambda x: x.time_idx > x.time_idx.max() - max_encoder_length]

new_data = pd.Series(range(max(data.time_idx) + 1, data.time_idx.max() + max_prediction_length + 1),
                     name="time_idx")
new_data = new_data.to_frame()
new_data["value"] = 0.0
new_data["series"] = data["series"][0]

new_prediction_data        = pd.concat([encoder_data, new_data], ignore_index=True)
new_raw_predictions, new_x = best_model.predict(new_prediction_data, mode="raw", return_x=True)

tf = new_raw_predictions[0].numpy()
pd.DataFrame()

best_model.plot_prediction(new_x, new_raw_predictions, idx=0, add_loss_to_title=True)
plt.suptitle(f"Series: {series.iloc[0]}")

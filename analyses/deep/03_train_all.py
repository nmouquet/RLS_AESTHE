'''
Train

This script trains one network using all images and save the weight


authors : Juliette Langlois <juliette.a.langlois@gmail.com>
          Nicolas Mouquet <nicolas.mouquet@cnrs.fr>
          Cedric Braga <cedric.braga@hotmail.fr>
          Valentine Fleuré <valentine.fleure@gmail.com>

date : 2021/07/30
'''

#Misc librairies
import csv
import os
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from pathlib import Path
from tqdm import tqdm
from sklearn.metrics import mean_absolute_error, mean_squared_error, median_absolute_error, r2_score

#Pytorch main librairies
from PIL import Image
import torch, torchvision
import torch.nn as nn
import torch.optim as optim
from torchvision import models, transforms
from torch.optim.lr_scheduler import ReduceLROnPlateau

#High-level ignite librairies for callbacks
from ignite.engine import Events, create_supervised_trainer, create_supervised_evaluator
from ignite.metrics import Loss, RunningAverage
from ignite.handlers import ModelCheckpoint, EarlyStopping
from ignite.contrib.metrics.regression import MedianAbsoluteError, MeanAbsoluteRelativeError, MeanError, R2Score

#Visualization
import logging
from ignite.contrib.handlers.tensorboard_logger import *
from ignite.contrib.handlers.param_scheduler import LRScheduler

#Seeding for reproducibility
torch.manual_seed(42)
np.random.seed(42)

class EsthDataset(torch.utils.data.Dataset):
  def __init__(self, path, transforms=None):
    # import and initialize dataset
    data = pd.read_csv(path, header=0, sep = ",")
    data.columns = ["names_worms", "scores", "species", "set"]
    
    self.X = data["names_worms"].values
    self.Y = data["scores"].values[:, np.newaxis].astype(np.float32)
    self.transforms = transforms
    
  def __getitem__(self, idx):
    # get item by index
    im = Image.open(str(Path('../../data/png/') / self.X[idx]) + '.png').convert('RGB')
    
    if velf.transforms :
        im = self.transforms(im)
        
        return im, self.Y[idx]
    
    return im.ToTensor(), self.Y[idx]
    
  def __len__(self):
    # returns length of data
    return len(self.X)

# Creating model, loss and optimizer
model = models.resnet50(pretrained=True)

# hyperparameters
learning_rate = 1e-1
batch_size = 4
num_epochs = 50
criterion = nn.MSELoss()
optimizer = optim.Adam(model.parameters(), lr=learning_rate, weight_decay=0.001)
add_dropout = True
dropout=0.5
freeze = True

# to use GPU
device = torch.device("cuda")

img_size = 224

#Log and weights saving
save_dir = str(Path('../../results/deep/03_train_all/'))

#Datasets : normalized or not
train_path = Path('../../results/elo/02_deep_train.csv')

#Setting data augmentation process according to the train or val/test splits
data_transforms = {
    'train': transforms.Compose([
        transforms.Resize((img_size,img_size)),
        transforms.RandomRotation(5),
        transforms.ToTensor(),
        transforms.Normalize([0.865, 0.877, 0.881], [0.273, 0.252, 0.250])
    ])
}

# create dataset instance
esthdataset_train = EsthDataset(train_path, transforms = data_transforms['train'])

# create dataloader instance
train_loader = torch.utils.data.DataLoader(esthdataset_train, batch_size = batch_size, shuffle = True, pin_memory=True)

#Freeze all weights
for param in model.parameters():
    param.requires_grad = False

# Replace the last fully-connected layer
# Parameters of newly constructed modules have requires_grad=True by default
if add_dropout :
    model.fc = nn.Sequential(
    nn.Dropout(dropout),
    nn.Linear(2048, 1)
)
else :
    model.fc = nn.Linear(512, 1)

model.to(device)

#Unfreeze model parameters
if not freeze :
    for param in model.parameters():
        param.requires_grad = True
        
else :
    for name, param in model.named_parameters():
        if "layer4" in str(name):
            param.requires_grad = True


# creating trainer,evaluator
trainer = create_supervised_trainer(model, optimizer, criterion, device=device)
metrics = {
    'me':MeanError(),
    'mse':Loss(criterion),
    'mare':MeanAbsoluteRelativeError(),
    'medae':MedianAbsoluteError(),
    'r2':R2Score()
}
train_evaluator = create_supervised_evaluator(model, metrics=metrics, device=device)
training_history = {'mare':[],'mse':[],'me':[], 'medae':[], 'r2':[]}
last_epoch = []

RunningAverage(output_transform=lambda x: x).attach(trainer, 'mare')

#Model checkpoints
checkpointer = ModelCheckpoint(save_dir, 'Chckpt', n_saved=1, 
                            create_dir=True, save_as_state_dict=True, require_empty=False)
trainer.add_event_handler(Events.EPOCH_COMPLETED, checkpointer, {'ResNet50': model})

#Logs to display during training
@trainer.on(Events.EPOCH_COMPLETED)
def log_training_results(trainer):
    train_evaluator.run(train_loader)
    metrics = train_evaluator.state.metrics
    mean_abs_rel_err = metrics['mare']*100
    loss = metrics['mse']
    r2 = metrics['r2']
    medae = metrics['medae']
    last_epoch.append(0)
    training_history['mare'].append(mean_abs_rel_err)
    training_history['mse'].append(loss)
    training_history['r2'].append(r2)
    training_history['medae'].append(medae)
    print("Training Results - Epoch: {}  Mean absolute relative error: {:.2f} Avg loss: {:.2f} R2 score : {:.2f}"
        .format(trainer.state.epoch, mean_abs_rel_err, loss, r2))

trainer.run(train_loader, max_epochs=num_epochs)



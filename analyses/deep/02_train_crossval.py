#Misc librairies
import csv
import os
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from pathlib import Path
from tqdm import tqdm
from sklearn.metrics import mean_absolute_error, mean_squared_error, median_absolute_error, r2_score
from statistics import mean
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
    
    if self.transforms :
        im = self.transforms(im)
        
        return im, self.Y[idx]
    
    return im.ToTensor(), self.Y[idx]
    
  def __len__(self):
    # returns length of data
    return len(self.X)

def create_lists(path_in, path_out):
    data = pd.read_csv(path_in, header=0, sep = ",")
    data.columns = ["names_worms", "scores", "species", "set"]
    
    a = data.where(data["set"]=='a').dropna()
    b = data.where(data["set"]=='b').dropna()
    c = data.where(data["set"]=='c').dropna()
    d = data.where(data["set"]=='d').dropna()
    e = data.where(data["set"]=='e').dropna()

    #run 1
    train = a.append(b.append(c.append(d)))
    val = e
    path_out_run = path_out / '02_run1'
    path_out_run.mkdir(parents=True, exist_ok=True)
    train.to_csv(path_out_run / 'train.csv', index=False)
    val.to_csv(path_out_run / 'val.csv', index=False)

    #run 2
    train = a.append(b.append(c.append(e)))
    val = d
    path_out_run = path_out / '02_run2'
    path_out_run.mkdir(parents=True, exist_ok=True)
    train.to_csv(path_out_run / 'train.csv', index=False)
    val.to_csv(path_out_run / 'val.csv', index=False)

    #run 3
    train = a.append(b.append(d.append(e)))
    val = c
    path_out_run = path_out / '02_run3'
    path_out_run.mkdir(parents=True, exist_ok=True)
    train.to_csv(path_out_run / 'train.csv', index=False)
    val.to_csv(path_out_run / 'val.csv', index=False)

    #run 4
    train = a.append(c.append(d.append(e)))
    val = b
    path_out_run = path_out / '02_run4'
    path_out_run.mkdir(parents=True, exist_ok=True)
    train.to_csv(path_out_run / 'train.csv', index=False)
    val.to_csv(path_out_run / 'val.csv', index=False)

    #run 5
    train = b.append(c.append(d.append(e)))
    val = a
    path_out_run = path_out / '02_run5'
    path_out_run.mkdir(parents=True, exist_ok=True)
    train.to_csv(path_out_run / 'train.csv', index=False)
    val.to_csv(path_out_run / 'val.csv', index=False)    

def txt2dataloader (txt_path, transform, batch_size):

    #Setting data augmentation process according to the train or val/test splits
    data_transforms = {
        'train': transforms.Compose([
            transforms.Resize((224,224)),
            transforms.RandomRotation(5),
            transforms.ToTensor(),
            transforms.Normalize([0.865, 0.877, 0.881], [0.273, 0.252, 0.250])
        ]),
        'val': transforms.Compose([
            transforms.Resize((224,224)),
            transforms.ToTensor(),
            transforms.Normalize([0.865, 0.877, 0.881], [0.273, 0.252, 0.250])
        ]),
    }

    # create dataset instance
    esthdataset= EsthDataset(txt_path, transforms = data_transforms[transform])

    # create dataloader instance
    loader = torch.utils.data.DataLoader(esthdataset, batch_size = batch_size, shuffle = True, pin_memory=True)
    return loader

learning_rate = 1e-1
batch_size = 4
num_epochs = 50
criterion = nn.MSELoss()
dropout=0.5

path_data = Path('../../results/elo') / '02_deep_train.csv'
path_list = Path('../../results/deep/')
create_lists(path_data, path_list)

R2 = []
for run in range(1,6):
    
    #Create model 
    model = models.resnet50(pretrained=True)
    for param in model.parameters():
        param.requires_grad = False
    model.fc = nn.Sequential(
    nn.Dropout(dropout),
    nn.Linear(2048, 1))    
    for name, param in model.named_parameters():
        if "layer4" in str(name):
            param.requires_grad = True
    
    optimizer = optim.Adam(model.parameters(), lr=learning_rate, weight_decay=0.001)
            
    # to use GPU
    device = torch.device("cuda")
    model.to(device)

    train_path = Path((str(path_list / '02_run') + str(run))) / 'train.csv'
    val_path = Path((str(path_list / '02_run') + str(run))) / 'val.csv'

    train_loader = txt2dataloader(train_path, 'train', batch_size)
    val_loader = txt2dataloader(val_path, 'val', batch_size)

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
    val_evaluator = create_supervised_evaluator(model, metrics=metrics, device=device)
    training_history = {'mare':[],'mse':[],'me':[], 'medae':[], 'r2':[]}
    validation_history = {'mare':[],'mse':[],'me':[], 'medae':[], 'r2':[]}

    RunningAverage(output_transform=lambda x: x).attach(trainer, 'mare')

    trainer.run(train_loader, max_epochs=num_epochs)

    # set model to evaluate model
    model.eval()

    y_true = torch.tensor([], dtype=torch.float, device=device)
    y_pred = torch.tensor([], device=device)

    # deactivate autograd engine and reduce memory usage and speed up computations
    with torch.no_grad():
        for data in val_loader:
            inputs = [i.to(device) for i in data[:-1]]
            labels = data[-1].to(device)

            outputs = model(*inputs)
            y_true = torch.cat((y_true, labels), 0)
            y_pred = torch.cat((y_pred, outputs), 0)

    y_true = y_true.cpu().numpy().flatten()
    y_pred = y_pred.cpu().numpy().flatten()

    R2.append(r2_score(y_true,y_pred))

with open(Path('../../results/deep/02_r2_per_run.csv'),'w', newline='') as csvfile:
    writer = csv.writer(csvfile)
    writer.writerows(R2)

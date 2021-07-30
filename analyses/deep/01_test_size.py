#Misc librairies
import csv
import os
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from pathlib import Path
from tqdm import tqdm
from sklearn.metrics import mean_absolute_error, mean_squared_error, median_absolute_error, r2_score
from sklearn.model_selection import train_test_split
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

def load_df(inputPath, fileName):
	# initialize the list of column names in the CSV file and then
	# load it using Pandas
	df = pd.read_csv(inputPath / fileName, sep=",", header=0)
	
	# return the data frame
	return df

def data_split(examples, train_frac, random_state=None):
    ''' https://scikit-learn.org/stable/modules/generated/sklearn.model_selection.train_test_split.html
    param data:       Data to be split
    param train_frac: Ratio of train set to whole dataset

    Randomly split dataset, based on these ratios:
        'train': train_frac
        'valid': (1-train_frac) / 2
        'test':  (1-train_frac) / 2

    Eg: passing train_frac=0.8 gives a 80% / 10% / 10% split
    '''

    assert train_frac >= 0 and train_frac <= 1, "Invalid training set fraction"

    X_train, X_tmp = train_test_split(
		examples, train_size=train_frac, random_state=random_state)

    X_val, X_test  = train_test_split(
		X_tmp, train_size=0.5, random_state=random_state)

    return X_train, X_val, X_test

def txt2dataloader (txt_path, transform, size, batch_size):

    #Setting data augmentation process according to the train or val/test splits
    data_transforms = {
        'train': transforms.Compose([
            transforms.Resize((size,size)),
            transforms.RandomRotation(5),
            transforms.ToTensor(),
            transforms.Normalize([0.865, 0.877, 0.881], [0.273, 0.252, 0.250])
        ]),
        'val': transforms.Compose([
            transforms.Resize((size,size)),
            transforms.ToTensor(),
            transforms.Normalize([0.865, 0.877, 0.881], [0.273, 0.252, 0.250])
        ]),
    }

    # create dataset instance
    esthdataset= EsthDataset(txt_path, transforms = data_transforms[transform])

    # create dataloader instance
    loader = torch.utils.data.DataLoader(esthdataset, batch_size = batch_size, shuffle = True, pin_memory=True)
    return loader

input_path = Path('../../results/elo')
fileName = '02_deep_train.csv'

df = load_df(input_path, fileName)

split =data_split(df, train_frac=0.8, random_state=42)
(train, val, test) = split

data_deep_path = Path('../../results/deep')

train_path = data_deep_path / '01_train.txt'
val_path = data_deep_path / '01_val.txt'
test_path = data_deep_path / '01_test.txt'

#save database
train.to_csv(train_path, index = False)
val.to_csv(val_path, index = False)
test.to_csv(test_path, index = False)

learning_rate = 1e-1
batch_size = 4
num_epochs = 50
criterion = nn.MSELoss()
dropout=0.5

for model_name in ['resnet18', 'resnet50']:
    R2_model = []
    for size in [32,64,128,224,256,512]:
        R2_size = []
        for run in range(5):

            print(model_name + " training " + str(run) + " for img size " + str(size))
            
			# create model
            if model_name == 'resnet18':
                model = models.resnet18(pretrained=True)
                for param in model.parameters():
                    param.requires_grad = False
                model.fc = nn.Sequential(
                nn.Dropout(dropout),
                nn.Linear(512, 1))
            else:
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

            train_loader = txt2dataloader(train_path, 'train', size, batch_size)
            val_loader = txt2dataloader(val_path, 'val', size, batch_size)
            test_loader = txt2dataloader(test_path, 'val', size, batch_size)

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
            last_epoch = []

            RunningAverage(output_transform=lambda x: x).attach(trainer, 'mare')

            trainer.run(train_loader, max_epochs=num_epochs)

            # set model to evaluate model
            model.eval()

            y_true = torch.tensor([], dtype=torch.float, device=device)
            y_pred = torch.tensor([], device=device)

            # deactivate autograd engine and reduce memory usage and speed up computations
            with torch.no_grad():
                for data in test_loader:
                    inputs = [i.to(device) for i in data[:-1]]
                    labels = data[-1].to(device)

                    outputs = model(*inputs)
                    y_true = torch.cat((y_true, labels), 0)
                    y_pred = torch.cat((y_pred, outputs), 0)

            y_true = y_true.cpu().numpy().flatten()
            y_pred = y_pred.cpu().numpy().flatten()

            print("R2 = ", r2_score(y_true,y_pred))
            R2_size.append(r2_score(y_true,y_pred)) # On rajoute le R2 du réseau n de la taille size
        R2_model.append(R2_size) # On rajoute les 5 R2 des réseaux pour la taille size

    result_file = str(Path('../../results/deep/') / ('01_' + model_name + '.csv'))
    with open(result_file, 'w', newline='') as csvfile:
        writer = csv.writer(csvfile)
        writer.writerows(R2_model)

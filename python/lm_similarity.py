'''
Accepts stimuli of the form (sentence, word1, word2) and reports the 
cosine similarity between the input words in every layer for the given model.
'''

import os
import argparse
import csv
from tqdm import tqdm

import torch
from torch.utils.data import DataLoader

from minicons.minicons import cwe

parser = argparse.ArgumentParser()
parser.add_argument("--dataset", type = str)
parser.add_argument("--model", default = 'distilbert-base', type = str)
parser.add_argument("--batchsize", default = 10, type = int)
parser.add_argument("--device", default = 'cpu', type = str)
parser.add_argument("--stimuli", default = 'stimulus', type = str)
parser.add_argument("--word1", default = "item_word", type = int)
parser.add_argument("--word2", default = "category_word", type = int)
args = parser.parse_args()

inpath = args.dataset
model_name = args.model
batch_size = args.batchsize
device = args.device
word1 = args.word1
word2 = args.word2
stimulus = args.stimuli

components = inpath.split("/")
data_dir = "/".join(components[0:-1])
dataset_name = components[-1].split(".")[0]
results_dir = f"{data_dir}/results/{dataset_name}"
# os.mkdir(results_dir)

dataset = []
stimuli = []
with open(args.dataset, "r") as f:
    reader = csv.DictReader(f)
    column_names = reader.fieldnames
    for row in reader:
        stimuli.append([row[stimulus], row[word1], row[word2]])
        dataset.append(list(row.values()))

if "/" in model_name:
    model_name = model_name.replace("/", "_")

transformer = cwe.CWE(model_name, device)

num_params = [sum(p.numel() for p in transformer.model.parameters())] * len(stimuli)

stimuli_loader = DataLoader(stimuli, batch_size = batch_size, num_workers=4)

similarities = [] # (layer, similarity)
for batch in tqdm(stimuli_loader):
    item, category = transformer.extract_paired_representations(batch, 'all')
    for i, (one, two) in enumerate(zip(item, category)):
        layer = i+1
        # print(torch.cosine_similarity(one, two))
        batch_similarities = torch.cosine_similarity(one, two).tolist()
        similarities.extend([[layer] * len(batch), batch_similarities])

dataset = list(zip(*dataset))
similarities = list(zip(*similarities))
dataset.append(similarities[0])
dataset.append(similarities[1])
dataset.append(num_params)
dataset.append([model_name] * len(stimuli))

column_names = column_names + ["layer", "similarity", "params", "model"]


with open(results_dir + f"/similarities_{model_name}.csv", "w") as f:
    writer = csv.writer(f)
    writer.writerow(column_names)
    writer.writerows(list(zip(*dataset)))
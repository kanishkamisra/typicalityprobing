'''
Accepts stimuli of the form (sentence, word1, word2) and reports the 
cosine similarity between the input words in every layer for the given model.
'''

import os
import argparse
import csv
from tqdm import tqdm

import itertools

import torch
from torch.utils.data import DataLoader

from minicons.minicons import cwe

parser = argparse.ArgumentParser()
parser.add_argument("--dataset", type = str)
parser.add_argument("--model", default = 'distilbert-base', type = str)
parser.add_argument("--batchsize", default = 10, type = int)
parser.add_argument("--device", default = 'cpu', type = str)
parser.add_argument("--premise", default = 'premise', type = str)
parser.add_argument("--conclusion", default = 'conclusion', type = str)
parser.add_argument("--word1", default = "item_word", type = str)
parser.add_argument("--word2", default = "category_word", type = str)
args = parser.parse_args()

inpath = args.dataset
model_name = args.model
batch_size = args.batchsize
device = args.device
word1 = args.word1
word2 = args.word2
premise = args.premise
conclusion = args.conclusion

components = inpath.split("/")
data_dir = "/".join(components[0:-1])
dataset_name = components[-1].split(".")[0]
results_dir = f"{data_dir}/results/{dataset_name}/similarities"
# os.mkdir(results_dir)

dataset = []
with open(args.dataset, "r") as f:
    reader = csv.DictReader(f)
    column_names = reader.fieldnames
    for row in reader:
        dataset.append(row)

if "/" in model_name:
    model_name = model_name.replace("/", "_")

transformer = cwe.CWE(model_name, device)

num_params = sum(p.numel() for p in transformer.model.parameters())

loader = DataLoader(dataset, batch_size = batch_size, num_workers=8)

results = []

for batch in tqdm(loader):
    stimuli = [(" ".join(item[:2]), item[2], item[3]) for item in zip(batch[premise], batch[conclusion], batch[word1], batch[word2])]

    item, category = transformer.extract_paired_representations(stimuli, 'all')

    layer_sims = list(map(lambda x: torch.cosine_similarity(x[0], x[1]).tolist(), list(zip(item, category))))
    
    for i, ls in enumerate(layer_sims):
        layers = [i+1] * len(ls)
        params = [num_params] * len(ls)
        model = [model_name] * len(ls)
        int_results = itertools.chain([batch[col] for col in column_names], [layers], [ls], [params], [model])
        results.extend(list(zip(*int_results)))

column_names = column_names + ["layer", "similarity", "params", "model"]

with open(results_dir + f"/{model_name}.csv", "w") as f:
    writer = csv.writer(f)
    writer.writerow(column_names)
    writer.writerows(results)


import os
import argparse
import csv
from tqdm import tqdm

import torch
from torch.utils.data import DataLoader

from minicons import scorer

parser = argparse.ArgumentParser()
parser.add_argument("--dataset", type = str)
parser.add_argument("--model", default = 'distilbert-base', type = str)
parser.add_argument("--batchsize", default = 10, type = int)
parser.add_argument("--device", default = 'cpu', type = str)
parser.add_argument("--stimuli", default = 0, type = int)
parser.add_argument("--lmtype", default = 'masked', choices = ['mlm', 'masked', 'causal', 'incremental'], type = str)
args = parser.parse_args()

inpath = args.dataset
model_name = args.model
batch_size = args.batchsize
device = args.device
lm_type = args.lmtype
idx = args.stimuli

# make results dir: ../data/typicality/results/(dataset)/model_name.csv
components = inpath.split("/")
data_dir = "/".join(components[0:-1])
dataset_name = components[-1].split(".")[0]
results_dir = f"{data_dir}/results/{dataset_name}"
# os.mkdir(results_dir)

dataset = []
with open(args.dataset, "r") as f:
    reader = csv.DictReader(f)
    column_names = reader.fieldnames
    for row in reader:
        dataset.append(list(row.values()))

dataset = list(zip(*dataset))
stimuli = dataset[idx]

if lm_type == "masked" or lm_type == "mlm":
    transformer = scorer.MaskedLMScorer(model_name, device)
elif lm_type == "incremental" or lm_type == "causal":
    transformer = scorer.IncrementalLMScorer(model_name, device)

if "/" in model_name:
    model_name = model_name.replace("/", "_")

num_params = [sum(p.numel() for p in transformer.model.parameters())] * len(stimuli)

stimuli_loader = DataLoader(stimuli, batch_size = batch_size, num_workers=4)

# results = []
lps = []
normalized_lps = []
for batch in tqdm(stimuli_loader):
    # scores = transformer.score(batch)
    result = transformer.logprobs(transformer.prepare_text(batch))
    logprob, _ = list(zip(*result))
    logprobs = list(map(lambda x: x.sum().tolist(), logprob))
    normalized_logprobs = list(map(lambda x: x.mean().tolist(), logprob))
    # results.extend(scores)
    lps.extend(logprobs)
    normalized_lps.extend(normalized_logprobs)

# dataset.append(results)
dataset.append(lps)
dataset.append(normalized_lps)
dataset.append(num_params)
dataset.append([model_name] * len(stimuli))

# column_names = column_names + ["score", "params", "model"]
column_names = column_names + ["logprob", "normalized_logprob", "params", "model"]

with open(results_dir + f"/{model_name}.csv", "w") as f:
    writer = csv.writer(f)
    writer.writerow(column_names)
    writer.writerows(list(zip(*dataset)))

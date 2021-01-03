import os
import argparse
import csv
from tqdm import tqdm

import random

import torch
from torch.utils.data import DataLoader

from minicons.minicons import scorer

def shuffle_sentence(sentence, word):
    '''
        returns the shuffled form of a sentence while preserving the 
        multi-word expression order for the focus word.
    '''
    sentence = sentence.replace(".", "")
    if len(word.split()) > 1:
        sentence = sentence.replace(word, "@".join(word.split())).split()
    else:
        sentence = sentence.split()
    random.shuffle(sentence)
        
    return " ".join(sentence).replace("@", " ").capitalize() + "."

parser = argparse.ArgumentParser()
parser.add_argument("--dataset", type = str)
parser.add_argument("--model", default = 'distilbert-base', type = str)
parser.add_argument("--batchsize", default = 10, type = int)
parser.add_argument("--device", default = 'cpu', type = str)
parser.add_argument("--stimulusonly", action="store_true")
parser.add_argument("--shuffled", action="store_true")
parser.add_argument("--lmtype", default = 'masked', choices = ['mlm', 'masked', 'causal', 'incremental'], type = str)
args = parser.parse_args()

inpath = args.dataset
model_name = args.model
batch_size = args.batchsize
device = args.device
shuffled = args.shuffled
stimulus_only = args.stimulusonly
lm_type = args.lmtype

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

if lm_type == "masked" or lm_type == "mlm":
    transformer = scorer.MaskedLMScorer(model_name, device)
elif lm_type == "incremental" or lm_type == "causal":
    transformer = scorer.IncrementalLMScorer(model_name, device)

if "/" in model_name:
    model_name = model_name.replace("/", "_")

num_params = [sum(p.numel() for p in transformer.model.parameters())] * len(dataset)

stimuli_loader = DataLoader(dataset, batch_size = batch_size, num_workers=8)

results = []
conclusion_only = []

for batch in tqdm(stimuli_loader):
    premise = list(batch[0])
    conclusion = list(batch[1])
    priming_scores = transformer.adapt_score(premise, conclusion, torch.sum)
    results.extend(priming_scores)
    if stimulus_only:
        conclusion_scores = transformer.score(conclusion, torch.sum)
        conclusion_only.extend(conclusion_scores)

dataset = list(zip(*dataset))
dataset.append(results)
if stimulus_only:
    dataset.append(conclusion_only)

random.seed(1234)

if shuffled:
    print("Running experiments with shuffled premise!")
    os.environ["TOKENIZERS_PARALLELISM"] = "false"
    random_shuffles = []
    for i in range(10):
        iteration_result = []
        for batch in tqdm(stimuli_loader):
            premises = list(batch[0])
            conclusion = list(batch[1])
            premise_words = list(batch[5])
            shuffled_premises = []
            for s, w in zip(premises, premise_words):
                shuffled = [shuffle_sentence(s, w) for i in range(2)]
                if shuffled[0] != s:
                    shuffled_premises.append(shuffled[0])
                else:
                    shuffled_premises.append(shuffled[1])

            priming_scores = transformer.adapt_score(shuffled_premises, conclusion, torch.sum)
            iteration_result.extend(priming_scores)
        random_shuffles.append(iteration_result)
    random_shuffles = torch.tensor(random_shuffles)
    scores = torch.tensor(results)
    mean_diff = (scores - random_shuffles).mean(0).tolist()
    std_diff = (scores - random_shuffles).std(0).tolist()

    dataset.append(mean_diff)
    dataset.append(std_diff)

if stimulus_only:
    column_names += ["score", "conclusion_only"]
else:
    column_names += ["score"]

if shuffled:
    column_names += ["shuffled_diff", "shuffled_diff_sd"]

dataset.append(num_params)
dataset.append([model_name] * len(results))

column_names += ["params", "model"]

with open(results_dir + f"/{model_name}.csv", "w") as f:
    writer = csv.writer(f)
    writer.writerow(column_names)
    writer.writerows(list(zip(*dataset)))

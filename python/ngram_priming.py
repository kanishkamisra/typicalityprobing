import os
import argparse
import csv

from tqdm import tqdm

import kenlm

model = kenlm.LanguageModel("/datasets/kmisra/wikilm.binary")

parser = argparse.ArgumentParser()
parser.add_argument("--dataset", type = str)
parser.add_argument("--p", default = 0, type = int)
parser.add_argument("--c", default = 1, type = int)
args = parser.parse_args()

model_name = "5gram"
inpath = args.dataset
p = args.p
c = args.c

components = inpath.split("/")
data_dir = "/".join(components[0:-1])
dataset_name = components[-1].split(".")[0]
results_dir = f"{data_dir}/results/{dataset_name}"

dataset = []
with open(args.dataset, "r") as f:
    reader = csv.DictReader(f)
    column_names = reader.fieldnames
    for row in reader:
        dataset.append(list(row.values()))

dataset = list(zip(*dataset))
premises = dataset[p]
conclusions = dataset[c]

results = []
conclusion_only = []

for premise, conclusion in tqdm(zip(premises, conclusions)):
    conclusion = conclusion.lower().replace(".", "")
    sentence = f"{premise.lower()} <\s> {conclusion}".replace(".", "")
    idx = len(sentence.split("<\s>")[0].split()) + 1
    scores = list(model.full_scores(sentence))
    scores = list(zip(*scores))[0] #tuple
    primed_score = sum(scores[idx:])
    results.append(primed_score)

    conclusion_score = model.score(conclusion)
    conclusion_only.append(conclusion_score)

dataset.append(results)
dataset.append(conclusion_only)
dataset.append([0] * len(premises))
dataset.append([model_name] * len(premises))

column_names = column_names + ["score", "conclusion_only", "params", "model"]

with open(results_dir + f"/{model_name}.csv", "w") as f:
    writer = csv.writer(f)
    writer.writerow(column_names)
    writer.writerows(list(zip(*dataset)))




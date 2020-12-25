import os
import argparse
import csv

from tqdm import tqdm

import kenlm

model = kenlm.LanguageModel("/datasets/kmisra/wikilm.binary")

parser = argparse.ArgumentParser()
parser.add_argument("--dataset", type = str)
parser.add_argument("--stimuli", default = 0, type = int)
args = parser.parse_args()

model_name = "5gram"
inpath = args.dataset
idx = args.stimuli

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
stimuli = dataset[idx]

lps = []
normalized_lps = []
for sentence in tqdm(stimuli):
    stimulus = sentence.lower().replace(".", "")
    stimulus_len = len(stimulus.split(" "))
    score = model.score(stimulus)
    lps.append(score)
    normalized_lps.append(score/stimulus_len)

dataset.append(lps)
dataset.append(normalized_lps)
dataset.append([0] * len(stimuli))
dataset.append([model_name] * len(stimuli))

column_names = column_names + ["logprob", "normalized_logprob", "params", "model"]

with open(results_dir + f"/{model_name}.csv", "w") as f:
    writer = csv.writer(f)
    writer.writerow(column_names)
    writer.writerows(list(zip(*dataset)))

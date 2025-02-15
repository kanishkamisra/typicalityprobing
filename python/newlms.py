import argparse
import csv
import pathlib
import torch
import utils

from minicons import scorer
from torch.utils.data import DataLoader
from tqdm import tqdm

'''
Models: GPT-2 Family, Pythia, OPT, Gemma, Llama
'''

def main(args):

    eval_path = args.eval_path  # csv?
    model = args.model  # usually a huggingface identifier
    results_dir = args.results_dir

    model_name = model.replace("/", "_")

    # load the model
    lm = scorer.IncrementalLMScorer(model, device=args.device)

    params = lm.model.num_parameters()

    # read eval
    eval_data = utils.read_csv_dict(eval_path)

    eval_dl = DataLoader(eval_data, batch_size=args.batch_size)

    results = []
    i = 0
    for batch in tqdm(eval_dl):
        # idx, items, types, prefixes, stimuli = batch
        # idx = batch['idx']
        # items = batch['item']
        # types = batch['type']
        # sentences = batch['sentence']
        # prefixes = batch['prefix']
        # stimuli = batch['stimulus']

        prefixes = batch['preamble']
        stimuli = batch['stimulus']

        # compute conditional log probs 
        if "gpt2" in model_name or "pythia" in model_name:
            # gpt2 doesn't automatically add bos tokens at the start of the sequence by default.
            log_probs = lm.conditional_score(prefixes, stimuli, bos_token=True)
            # log_probs = lm.sequence_score(sentences, bos_token=True, bow_correction=True)
        else:
            log_probs = lm.conditional_score(prefixes, stimuli)
            # log_probs = lm.sequence_score(sentences, bow_correction=True)

        for logprob in log_probs:
            results.append({
                "idx": i,
                "logprob": logprob,
                "params": params
            })
            i+=1

    pathlib.Path(results_dir).mkdir(parents=True, exist_ok=True) # creates if it does not exist
    utils.write_dict_list_to_csv(results, f"{results_dir}/{model_name}.csv") # save

if __name__ == "__main__":

    parser = argparse.ArgumentParser()

    parser.add_argument("--eval-path", type=str, default="data/rosch1975/rosch1975_alternate.csv")
    parser.add_argument("--model", type=str, default="gpt2-medium")
    parser.add_argument("--results-dir", type=str, default="data/results/new-results")
    parser.add_argument("--batch-size", type=int, default=32)
    parser.add_argument("--device", type=str, default="cuda:0")

    args = parser.parse_args()

    main(args)



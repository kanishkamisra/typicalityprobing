#!/bin/bash

declare -a models=("bert-base-uncased" "bert-large-uncased" "roberta-base" "roberta-large" "distilbert-base-uncased" "distilroberta-base" "albert-base-v1" "albert-large-v1" "albert-xlarge-v1" "albert-xxlarge-v1" "google/electra-small-generator" "google/electra-base-generator" "google/electra-large-generator" "openai-gpt" "gpt2" "gpt2-medium" "distilgpt2" "gpt2-large" "gpt2-xl")

echo "Running experiments on Language Models"

for model in ${models[@]}
do
    echo "Running experiments for ${model}!"
    python ../python/lm_similarity.py --model ${model} --device cuda:1 --batchsize 50 --dataset ../data/premiseconclusion.csv
done

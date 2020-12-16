#!/bin/bash

declare -a mlmmodels=("bert-base-uncased" "bert-large-uncased" "roberta-base" "roberta-large" "distilbert-base-uncased" "distilroberta-base" "albert-base-v1" "albert-large-v1" "albert-xlarge-v1" "albert-xxlarge-v1" "google/electra-small-generator" "google/electra-base-generator" "google/electra-large-generator")
declare -a incrementalmodels=("openai-gpt" "gpt2" "gpt2-medium" "distilgpt2" "gpt2-large" "gpt2-xl")

echo "Running experiments on Masked Language Models"

for model in ${mlmmodels[@]}
do
    echo "Running experiments for ${model}!"
    python ../python/contextual_priming.py --model ${model} --device cuda:1 --batchsize 50 --dataset ../data/premiseconclusion.csv
done

echo "Running experiments on Incremental Language Models"

for model in ${incrementalmodels[@]}
do
    echo "Running experiments for ${model}!"
    python ../python/contextual_priming.py --model ${model} --device cuda:1 --lmtype incremental --batchsize 50 --dataset ../data/premiseconclusion.csv
done
# small models, high batch size
# declare -a models=(gpt2 gpt2-medium gpt2-large EleutherAI/pythia-70m-deduped EleutherAI/pythia-160m-deduped
#     EleutherAI/pythia-410m-deduped)
declare -a models=(distilbert/distilgpt2)

for model in "${models[@]}"; do
    echo "Running $model"
    python python/newlms.py --model $model --batch-size 128 --device cuda:0
done

# medium models, medium batch size
# declare -a models=(EleutherAI/pythia-1.4b-deduped EleutherAI/pythia-2.8b-deduped
#     EleutherAI/pythia-6.9b-deduped google/Gemma-2-2B allenai/OLMo-2-1124-7B)
declare -a models=(gpt2-xl)

for model in "${models[@]}"; do
    echo "Running $model"
    python python/newlms.py --model $model --batch-size 64 --device cuda:0
done

# declare -a models=(EleutherAI/pythia-12b-deduped google/Gemma-2-9B)

# for model in "${models[@]}"; do
#     echo "Running $model"
#     python python/newlms.py --model $model --batch-size 16 --device cuda:0
# done

declare -a models=(allenai/OLMo-2-1124-13B)

for model in "${models[@]}"; do
    echo "Running $model"
    python python/newlms.py --model $model --batch-size 2 --device cuda:0
done

# typicalityprobing
Code, Data, and Analyses for the probing LMs for typicality effects.

Package requirements:

```
torch
transformers
minicons
pattern
inflect
```

## N-gram baseline

Uses the [kenlm toolkit](https://github.com/kpu/kenlm) to train a 5-gram LM on the December 2020 dump of Wikipedia. You can also follow [this tutorial](https://tiefenauer.github.io/blog/wiki-n-gram-lm/)!

## Experimental Pseudo Code (in a nutshell)

This mini-tutorial might be more useful in case you do not want to deal with my messy code and the intricacies of the specific experiments.

### Taxonomic Sentence Verification experiments

We are interested in measuring the probability of the category, given a prefix that signals category-membership. That is, we are interested in something like P(bird. | A robin is a). To get this, I've used the following code (here, model == gpt2):

```py
from minicons import scorer

# for something like BERT, it would be using scorer.MaskedLMScorer
lm = scorer.IncrementalLMScorer('gpt2', 'cpu') 


# All our stimuli will be in batches.
prefixes = ['A robin is a', 'A penguin is a', 'A tiger is a']
queries = ['bird.', 'bird.', 'mammal.']

# automatically returns a list of mean log-probs of queries given the prefixes (where the prefixes and queries are paired.)
scores = lm.partial_score(prefixes, queries) 
```

### Category Based Induction

Similarly, we are interested in measuring the log-probability of a conclusion (all birds have ulnar arteries) given the premise (robins have ulnar arteries).
We can again use the same logic as above, only manipulating the `prefix` and `queries`:

```py
from minicons import scorer

lm = scorer.IncrementalLMScorer('gpt2', 'cpu') 

prefixes = ['Robins have ulnar arteries', 'Penguins have ulnar arteries.', 'Tigers have ulnar arteries.']
queries = ['All birds have ulnar arteries.', 'All birds have ulnar arteries.', 'All mammals have ulnar arteries.']

scores = lm.partial_score(prefixes, queries) # computes p(conclusion | premise)
```


Note: In the above example, I have used the `partial_score` method which was developer after this paper was written. It is effectively the same as the logic used in the code here, but combines multiple steps into one to keep things clean.

## Generating data

Typicality Data: `data/rosch1975/`


For Taxonomic Sentence Verification experiments:

```bash
cd python
python generate_tsv.py
```

For Category-based Induction:

```bash
cd python 
python generate_cbi.py
```

## Experiments

### Taxonomic Sentence Verification

Generated Data: `data/rosch1975/rosch1975_alternate.csv`

Run experiment:

```bash
cd experiments
bash run_rosch1975_alternate.sh
```

Saved Results (by model): `data/rosch1975/results/rosch1975_alternate`

### Category-based Induction

Generated Data: `data/premiseconclusion.csv`

```bash
cd experiments
bash premiseconclusion_contextual_priming.sh
```

Saved Results: `data/results/premiseconclusion/`


If you use the data/results from this work, please cite:

```
@inproceedings{misra2021typicality,
  title={Do language models learn typicality judgments from text?},
  author={Kanishka Misra and Allyson Ettinger and Julia Rayz},
  booktitle={Proceedings of the 43rd Annual Conference of the Cognitive Science
Society},
  year={2021}
}
```

If you use minicons, please cite:

```
@article{misra2022minicons,
    title={minicons: Enabling Flexible Behavioral and Representational Analyses of Transformer Language Models},
    author={Kanishka Misra},
    journal={arXiv preprint arXiv:2203.13112},
    year={2022}
}
```
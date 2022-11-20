import inflect
from pattern.en import singularize
import glob
import re
from collections import defaultdict
import csv

inflector = inflect.engine()

def is_plural(plural_form):
    singular_form = singularize(plural_form)
    plural = True if plural_form != singular_form else False
    return plural

categories = ["furniture", "fruit", "vehicle", "weapon", "vegetable", 
              "tool", "bird", "sport", "toy", "clothing"]

# each category's predicate entries are of the form: (verb, arg) 
# where arg is some morpho-syntactic form of either verb, noun or adjective.
predicates = {
    "sports": [('involves', '[VBG]'), ('requires', '[VBG]'), ('includes', '[VBG]'), ('is', '[JJ]')],
    "activities": [('involves', '[VBG]'), ('requires', '[VBG]'), ('includes', '[VBG]'), ('is', '[JJ]')],
    "birds": [('can', '[VB]'), ('can be', '[VBD]'), ('are', '[JJ]'), ('use a', '[NN]'), ('love', '[NNS]'), ('have', '[NNS]')],
    "animals": [('can', '[VB]'), ('can be', '[VBD]'), ('are', '[JJ]'), ('use a', '[NN]'), ('love', '[NNS]'), ('have', '[NNS]')],
    "mammals": [('can', '[VB]'), ('can be', '[VBD]'), ('are', '[JJ]'), ('use a', '[NN]'), ('love', '[NNS]'), ('have', '[NNS]')],
    "fish": [('can', '[VB]'), ('can be', '[VBD]'), ('are', '[JJ]'), ('use a', '[NN]'), ('love', '[NNS]'), ('have', '[NNS]')],
    "vehicles": [('can', '[VB]'), ('can be', '[VBD]'), ('are', '[JJ]'), ('use a', '[NN]'), ('have', '[NNS]')],
    "weapons": [('can', '[VB]'), ('can be', '[VBD]'), ('are', '[JJ]'), ('use a', '[NN]'), ('have', '[NNS]')],
    "tools": [('can', '[VB]'), ('can be', '[VBD]'), ('are', '[JJ]'), ('use a', '[NN]'), ('have', '[NNS]')],
    "furniture": [('can', '[VB]'), ('can be', '[VBD]'), ('are', '[JJ]'), ('use a', '[NN]'), ('have', '[NNS]')],
    "plants": [('can', '[VB]'), ('can be', '[VBD]'), ('are', '[JJ]'), ('use a', '[NN]'), ('have', '[NNS]')],
    "trees": [('can', '[VB]'), ('can be', '[VBD]'), ('are', '[JJ]'), ('use a', '[NN]'), ('have', '[NNS]')],
    "shapes": [('can be', '[VBD]'), ('are', '[JJ]'), ('have', '[NNS]')],
    "fruits": [('can be', '[VBD]'), ('are', '[JJ]'), ('have', '[NNS]')],
    "vegetables": [('can be', '[VBD]'), ('are', '[JJ]'), ('have', '[NNS]')],
    "toys": [('can be', '[VBD]'), ('are', '[JJ]'), ('have', '[NNS]'), ('use a', '[NN]')],
    "clothes": [('can be', '[VBD]'), ('are', '[JJ]'), ('have', '[NNS]')],    
}

args = {
    '[VB]': ["dax", "wif", "blick", "gyre", "gimble"],
    '[VBD]': ["daxed", "wiffed", "blicked", "gyred", "gimbled"],
    '[VBG]': ["daxing", "wiffing", "blicking", "gyring", "gimbling"],
    '[JJ]': ["beamish", "slithy", "mimsy", "vorpal", "frabjous"],
    '[NN]': ["wug", "fep", "blicket", "jabberwock", "tove"],
    '[NNS]': ["wugs", "feps", "blickets", "jabberwocks", "toves"]
}

category_texts = ["birds", "weapons", "vegetables", "tools" , "furniture", "vehicles", "clothes", "fruits", "sports", 'toys']

control = {
    'birds': 'vegetables',
    'weapons': 'vehicles',
    'furniture': 'fruits',
    'clothes': 'tools',
    'vehicles': 'birds',
    'fruits': 'furniture',
    'vegetables': 'weapons',
    'tools': 'clothes',
    'sports': 'toys',
    'toys': 'birds'
}

exceptions = {
    'marbles': 'marble',
    'jacks': 'jack',
    'paper dolls': 'paper doll',
    'crayons': 'crayon',
    'skates': 'skate',
    'stilts': 'stilt',
    'checkers': 'checker',
    'animals': 'animal',
    'books': 'book'
    
}

singulars = ["broccoli", "parsley", "kale", "rutabaga", "garlic", "rice", "monopoly"]
stimuli =  []
items = defaultdict(list)
for file in glob.glob("../data/rosch1975/*.txt"):
    category = re.search(r'(?<=5\/)(.*)(?=\.txt)', file).group(1)
    if category == "toyss":
        continue
    else:
        plural_category = inflector.plural_noun(category)
        if category == "furniture":
            plural_category = "furniture"
        if category == "clothing":
            plural_category = "clothes"
        with open(file, "r") as f:
            for rank, word in enumerate(f):
                word = word.strip()
                if word in exceptions.keys():
                    word = exceptions[word]
                if category not in ["sport", "activity"]:
                    word_text = inflector.plural_noun(word)
                else:
                    word_text = word
                if word in singulars:
                    word_text = word
                for i, (predicate, argument) in enumerate(predicates[plural_category]):
                    if category in ["sport", "activity"]:
                        predicate_text = predicate
                        conclusion = f"All {plural_category} {inflector.plural_verb(predicate)} {argument}."
                        control_conclusion = f"All {control[plural_category]} {inflector.plural_verb(predicate)} {argument}."
                    else:
                        if word_text in singulars:
                            if predicate == "are":
                                predicate_text = "is"
                            elif predicate == "have":
                                predicate_text = "has"
                            elif predicate == "use a":
                                predicate_text = "uses a"
                            else:
                                predicate_text = predicate
                        else:
                            predicate_text = predicate
                        conclusion = f"All {plural_category} {predicate} {argument}."
                        control_conclusion = f"All {control[plural_category]} {predicate} {argument}."
                    premise = f"{word_text.capitalize()} {predicate_text} {argument}."
                    
                    stimuli.extend([(premise.replace(arg, form), conclusion.replace(arg, form), control_conclusion.replace(arg, form), word, category, predicate, word_text.capitalize(), plural_category,  i+1, j+1) for arg in args.keys() for j, form in enumerate(args[arg]) if arg == argument])

with open("../data/premiseconclusion.csv", "w") as f:
    writer = csv.writer(f)
    writer.writerow(["premise", "conclusion", "control", "item", "category", "blankpredicate", "item_word", "category_word", "predicate_id", "argument_id"])
    writer.writerows(stimuli)
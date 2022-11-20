import inflect
from pattern.en import singularize, pluralize
import glob
import re
from collections import defaultdict
import csv

inflector = inflect.engine()

def is_plural(plural_form):
    singular_form = singularize(plural_form)
    plural = True if plural_form != singular_form else False
    return plural

# Rosch et al., 1975 data
categories = ["furniture", "fruit", "vehicle", "weapon", "vegetable", 
              "tool", "bird", "sport", "toy", "clothing"]

# Not used in the paper
stimuli = []
items = defaultdict(list)
for file in glob.glob("../data/rosch1975/*.txt"):
    category = re.search(r'(?<=5\/)(.*)(?=\.txt)', file).group(1)
    with open(file, "r") as f:
        for rank, word in enumerate(f):
            word = word.strip()
            items[category].append(word)
            if category == "sport":
                preamble = word
                stimulus = "is a sport."
#                 stimulus = f"{word} is a sport."
            else:
                if is_plural(word):
                    if category == "clothing":
                        preamble = word
                        stimulus = "are clothes."
#                         stimulus = f"{word} are clothes."
                    else:
                        preamble = word
                        stimulus = f"are {pluralize(category)}."
#                         stimulus = f"{word} are {pluralize(category)}."
                else:
                    if category == "furniture" or category == "clothing":
                        preamble = inflector.a(word)
                        stimulus = f"is a type of {category}."
#                         stimulus = f"{inflector.a(word)} is a type of {category}."
                    elif category == "vegetable" or category == "fruit":
                        preamble = word
                        stimulus = f"is a {category}."
#                         stimulus = f"{word} is a {category}."
                    else:
                        preamble = inflector.a(word)
                        stimulus = f"is a {category}."
#                         stimulus = f"{inflector.a(word)} is a {category}."
            stimuli.append((preamble.capitalize(), stimulus, word, category, rank+1))

with open("../data/rosch1975/rosch1975.csv", "w") as f:
    writer = csv.writer(f)
    writer.writerow(["preamble", "stimulus", "item", "category", "rank"])
    writer.writerows(stimuli)


alternate_stimuli = []
not_plural = ["gas", "glass", "teargas", "asparagus", "watercress"]
a_determiner = ["chest of drawers", "bus", "albatross", "dress"]
items = defaultdict(list)
for file in glob.glob("../data/rosch1975/*.txt"):
    category = re.search(r'(?<=5\/)(.*)(?=\.txt)', file).group(1)
    with open(file, "r") as f:
        for rank, word in enumerate(f):
            word = word.strip()
            items[category].append(word)
            if category == "sport":
                preamble = f"{word} is a"
                stimulus = "sport."
#                 stimulus = f"{word} is a sport."
            else:
                if is_plural(word):
                    if category == "clothing":
                        preamble = f"{word} are"
                        stimulus = "clothes."
                        if word == "dress":
                            preamble = "a dress is a"
                            stimulus = "clothing."
                    elif word in not_plural:
                        preamble = f"{word} is a"
                        stimulus = f"{category}."
                    elif word in a_determiner:
                        preamble = f"{inflector.a(word)} is a"
                        stimulus = f"{category}."
                    else:
                        preamble = f"{word} are"
                        stimulus = f"{pluralize(category)}."
#                         stimulus = f"{word} are {pluralize(category)}."
                else:
                    if category == "furniture":
                        preamble = f"{inflector.a(word)} is a type of"
                        stimulus = f"{category}."
                        if word in a_determiner:
                            preamble = f"{inflector.a(word)} is type of"
                            stimulus = f"{category}."
                    elif category == "vegetable" or category == "fruit":
                        preamble = f"{word} is a"
                        stimulus = f"{category}."
#                         stimulus = f"{word} is a {category}."
                    else:
                        preamble = f"{inflector.a(word)} is a"
                        stimulus = f"{category}."
#                         stimulus = f"{inflector.a(word)} is a {category}."
            alternate_stimuli.append((preamble.capitalize(), stimulus, word, category, rank+1))

with open("../data/rosch1975/rosch1975_alternate.csv", "w") as f:
    writer = csv.writer(f)
    writer.writerow(["preamble", "stimulus", "item", "category", "rank"])
    writer.writerows(alternate_stimuli)


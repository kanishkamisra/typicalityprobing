'''
Just some util functions that I use quite often...
'''

import csv
import json
import re
import unicodedata


def read_jsonl(path):
    with open(path) as f:
        data = f.readlines()
    data = [json.loads(line) for line in data]
    return data


def read_csv_dict(path):
    data = []
    with open(path, "r") as f:
        reader = csv.DictReader(f)
        for line in reader:
            data.append(line)
    return data


def roundup(x):
    return x if x % 1000 == 0 else x + 1000 - x % 1000


def read_file(path):
    """TODO: make read all"""
    return [
        unicodedata.normalize("NFKD", i.strip())
        for i in open(path, encoding="utf-8").readlines()
        if i.strip() != ""
    ]

def write_file(path, data):
    with open(path, "w", encoding="utf-8") as f:
        for line in data:
            f.write(line + "\n")

def belongingness(tup1, tup2):
    """is tup1 contained in tup2?"""
    assert tup1[0] <= tup1[1] and tup2[0] <= tup2[1]

    if tup2[0] <= tup1[0] and tup2[1] >= tup1[1]:
        return True
    else:
        return False


def write_dict_list_to_csv(dict_list, csv_file):
    fieldnames = dict_list[0].keys()  # Assuming all dictionaries have the same keys

    with open(csv_file, "w", newline="") as csvfile:
        writer = csv.DictWriter(csvfile, fieldnames=fieldnames)

        # Write the header
        writer.writeheader()

        # Write the data
        writer.writerows(dict_list)


def write_jsonl(data, path):
    with open(path, "w") as f:
        for line in data:
            f.write(json.dumps(line) + "\n")


def divide_chunks(l, n):
    for i in range(0, len(l), n):
        yield l[i : i + n]

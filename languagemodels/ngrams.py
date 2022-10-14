# This is meant to demo n-grams on source code with NLTK
import nltk
from io import BytesIO
import tokenize
import os
import glob


def stripped_tokens_of_file(filename):
    """ load and tokenize a python file """
    tokens = list(tokenize.tokenize(BytesIO(bytes(open(filename).read(),"utf8")).readline))
    str_tokens = [x.string for x in tokens]
    str_tokens = [x for x in str_tokens if x != '\n']
    str_tokens = [x for x in str_tokens if x.strip() != '']
    # remove comments
    str_tokens = [x for x in str_tokens if len(x) >= 0 and x[0] != '#']
    return str_tokens[1:] # get rid of utf-8 encoding prefix

# lets get some python files

nltk_file = nltk.__file__
nltk_dir = os.path.dirname(nltk_file)
nltk_files = glob.glob(nltk_dir + "/*.py")
tokens = [stripped_tokens_of_file(f) for f in nltk_files]

print(f"{nltk_file}\n{nltk_dir}\n{nltk_files}\n{tokens[0][0:10]}")

from nltk.lm import KneserNeyInterpolated
from nltk.lm.preprocessing import padded_everygram_pipeline, padded_everygrams

n = 4 # 4-grams
train_data, vocab = padded_everygram_pipeline(n, tokens)
model = KneserNeyInterpolated(n) 
model.fit(train_data, vocab)
print(f"total vocab: {len(model.vocab)} whiles: {model.vocab.counts['while']}")
# Ask questions about what comes next
# probability
model.score('try',['while','True',":"])
model.score('True',['while'])
model.counts[['while']]
model.counts[['try',':']]
model.counts[['try',':','fp']]
model.counts[['try',':','fp']]
model.generate(10, text_seed=['try',':','fp'])
# cross entropy of a while loop
model.entropy([("<s>","while","True"),
               ("while","True",":"),("True",":","</s>")])
print(list(padded_everygrams(n,["while","True",":"])))
model.entropy(padded_everygrams(n,["while","True",":"]))
# Note we're trained with padding
model.entropy(padded_everygrams(n,["<s>","while","True",":","</s>"]))
# cross entropy of file 2 (slow)
model.entropy(padded_everygrams(n,tokens[2]))

import numpy
numpy_tokens = stripped_tokens_of_file(numpy.__file__)
# high cross entropy
model.entropy(padded_everygrams(n,numpy_tokens))
# lower cross entropy
model.entropy(padded_everygrams(n, stripped_tokens_of_file(nltk.lm.__file__)))
# lower...
model.entropy(padded_everygrams(n, stripped_tokens_of_file(nltk.lm.preprocessing.__file__)))
# lower cross entropy (in training set)
model.entropy(padded_everygrams(n, stripped_tokens_of_file(nltk.__file__)))


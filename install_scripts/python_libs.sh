#!/bin/bash
python -V
echo -e "installing libraries for $(python -V)\n"
conda install -y numpy
conda install -y pandas
conda install -y matplotlib
conda install -y seaborn
conda install -y torch
conda install -y scipy
conda install -y seaborn
conda install -y scikit-learn
conda install -y django
conda install -y networkx

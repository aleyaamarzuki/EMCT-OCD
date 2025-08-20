# EMCT-OCD

Pre-processing and analysis code for ecological momentary cognitive testing (EMCT) study investigating how metacognition and OCD symptoms fluctuate on a moment-to-moment basis. Anonymised datasets can be found here: <https://osf.io/x7h5k/?view_only=>

## Model

The code to replicate the meta-_d_ model estimates can be found in the `model` directory, and is called `analysis.ipynb`.
The data can be found in the **OSF** repository linked above. NOTE that the script requires the trial-level data to be in the same directory as the script, and the data should be in `.csv` format.
The model we used is the implementation of the meta-_d_ model based on Signal Detection Theory by the [_cpm_ python toolbox](https://cpm-toolbox.net).
The toolbox can be installed via pip:

```bash
pip install cpm-toolbox
```

or within the Jupyter notebook:

```python
!pip install cpm-toolbox
```

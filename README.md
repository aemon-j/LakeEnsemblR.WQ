# LakeEnsemblR.WQ
R Package to facilitate running ensembles of water quality models


# How do I contribute new code back to the LakeEnsemblR.WQ project?
In order to contribute to this code, we recommend the following workflow:

- "fork" this repository to your own personal github account

- clone the github repository to your computer:

- `$git clone <git@github.com:{username}/LakeEnsemblR.WQ.git>`

- modify code or add new functionality, save the code

add the repository main to a remote main called "upstream"

- `$cd LakeEnsemblR.WQ`

- `$git remote add upstream <git@github.com:aemon-j/LakeEnsemblR.WQ.git>`

before pushing your changes to your repository, pull in the current version of the aemon-j main:

- `$git fetch upstream`

- merge these differences with your own "main" version:

- `$git merge upstream/main`

push your changes to your github repository, in addition to changes made by pulling in the aemon-j main:

- `$git push`

submit a pull request to aemon-j main using your account at github.com

Identifying loss and gain of function driver missense mutations in cancer

These scripts accompany the bioinformatics paper "Identifying loss and gain of function driver missense mutations in cancer" By Hanadi M Baeissa, Sarah K. Wooller, Chris J Richardson, and Frances M G Pearl

Firstly, run FATHMM cancer, PolyPhen2 and CHASM of misssense mutations to classify mutation into cancer or benign use protein identifier and the amino acid substitution in the conventional one letter format.

Then, Select the features using FATHMM cancer, FATHMM disease, Polyphen-2, Mutation assessor and NetSurf Protein to get the 16 features.

Four features selected from FATHMM cancer including: HMM Weights D., HMM Weights O.,HMM Prob W. and HMM Prob M..
One feature selected from FATHMM disease which is HMM Weights D..
One feature selected from Mutation Assessor which is Functional Impact score (FI).
Three features selected from PolyPhen-2 including: PHAT, PSIC Score1, dScore.
The last source of features was the protein surface accessibility and secondary structure predictions (NetSurfP) server.

Seven features selected from (NetSurfP) Two features are provided twice, one for the wild type amino acid and one for the mutant amino acid, include: Class assignment relative surface accessibility (RSA), absolute surface accessibility (ASA), Three features for only the wild type amino acis include: Probability for Alpha-Helix, Probability for Beta-strand and Probability for Coil.
Finaly, use support vector machine classifier to predict LOF and GOF in cancer- associated mutation.

To use the scripts:

Download and save “TrainingSet.csv”. Change the first few lines of “LOF_GOF_trainer.R” to reflect where you’ve saved the training set and where you want to save the trained model to, before running the script.

Use the online tools: Fathmm Cancer, Fathmm Disease PolyPhen2, MA and NetsurfP to assemble data for the features as set out in the paper.

Change the first few lines of “LOF_GOF_predicter.R” to reflect where you’ve saved the trained model and where you want to save the predictions to before running the prediction script.

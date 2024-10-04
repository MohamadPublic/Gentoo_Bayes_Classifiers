# Description

This code explores the different variants of Bayes classifiers for the purpose of classifying the sex of Gentoo penguins given theit Flipper Length and Bill Depth from the `palmer penguins` dataset.

Fundementally, the same decision function is used with variations on the variance-covariance matricies of the classes: $$D(\mathbf{x}) = \frac{1}{2}\log \frac{|\Sigma_0|}{|\Sigma_1|} - \frac{1}{2}(\mathbf{x}-\mathbf{\mu_1})^T\Sigma_{1}^{-1}(\mathbf{x}-\mathbf{\mu_1}) + \frac{1}{2}(\mathbf{x}-\mathbf{\mu_0})^T\Sigma_{0}^{-1}(\mathbf{x}-\mathbf{\mu_0})+\log\frac{\pi_1}{\pi_0}$$

We explore QDA (no restriction on var-cov matricies), LDA (pooled var-cov matrix), Naive Bayes (with per-class diagonal var-cov matricies), and Naive Bayes (with a pooled diagonal var-cov matrix).

![image](https://github.com/user-attachments/assets/2d6c81bb-2a2f-434f-8669-eb2529f5d01a)

QDA:

![image](https://github.com/user-attachments/assets/d6d8cdee-acc0-49f8-8c79-dee5ab05efaf)

LDA: 

![image](https://github.com/user-attachments/assets/5003ef37-7ded-4ecf-b929-07926430248e)

NBC (per-class variances):

![image](https://github.com/user-attachments/assets/e06125d6-a1f1-49b7-b11d-6f568ffa2035)

NBC (poold var-cov matrix):

![image](https://github.com/user-attachments/assets/bfae61ea-2bc3-4e04-92b3-c48b8cccf7df)





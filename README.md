# The Weight of History: Diagnosing Identification in IV Local Projections IRFs

**Rubén Gonzálvez & Daniel Fernández Romero**

**January 2026**

## *Replication Files*

## 1. Structure of the archive

* `Code` is a folder containing all the do files to replicate the paper. The code has been written and running using Stata 17.

  * `00.Master.do` is the main code file that run all the necessary nested dofiles.

  * `1_Main` do files contains the dofiles used to run the results in the main text.

  * `2_Appendix` do files run the do files to replicate the results in the appendix.

* `1_Data` is a folder containing all the raw data used in this replication file.

* Additionally, `00.Master.do` creates the following folders:

  * `3_Output` stores all the graphs and results produced.

## 2. Data description

### 2.1 MAIN DATA

#### Replication from Ramey & Zubairy (2018)

The data is obtained following Ramey and Zubairy (2018) from this [[link]](https://www.journals.uchicago.edu/doi/suppl/10.1086/696277/suppl_file/2014646data.zip)

### 2.1 Appendix Data

#### Romer & Romer (2004)

The data is obtained following Romer and Romer (2004) and updated by Wieland and Yang (2019) from this [[link]](https://www.openicpsr.org/openicpsr/project/116025/version/V1/view) and [[link]](https://github.com/johanneswieland/RomerShocks) respectively.

Data for GDP and for unemployment are obtained from St. Louis Fred Data in [[link]](https://fred.stlouisfed.org/series/GDPC1), and [[link]](https://fred.stlouisfed.org/series/UNRATE) respectively. The replication codes are obtained from Òscar Jordà [[webpage]](https://sites.google.com/site/oscarjorda/home/local-projections/aea-continuing-education-2023?authuser=0).

#### Romer & Romer (2010)

The data is obtained following Romer and Romer (2010) from this [[link]](https://www.openicpsr.org/openicpsr/project/112357/version/V1/view).

#### Gertler & Karadi (2015)

The data is obtained following Gertler and Karadi (2015) from this [[link]](https://www.openicpsr.org/openicpsr/project/114082/version/V1/view).

#### Jordà, Singh & Taylor (2024)

The data is obtained following Jordà, Singh and Taylor (2024) from this [[link]](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/5DASMW). Macro-variables data for 17 countries is obtained from [Macrohistory Database](https://www.macrohistory.net/database/) (Jordà, Schularick, and Taylor, 2017).

## 3. Running code to replicate all figures and tables

To generate all tables and figures from the paper and the online appendices, proceed in the following way:

* Open `00.Master.do`

* Set your machine's path to the `Replicatoin_File` folder. This is done in line 5.

* Execute `00.Master.do` in Stata.

* All results and Figures will be created inside `3_Output` folder.

## References

Gertler, M., & Karadi, P. (2015). Monetary policy surprises, credit costs, and economic activity. American Economic Journal: Macroeconomics, 7(1), 44-76.

Jordà, Ò., Schularick, M., & Taylor, A. M. (2017). Macrofinancial history and the new business cycle facts. NBER macroeconomics annual, 31(1), 213-263.

Jordà, Ò., Singh, S. R., & Taylor, A. M. (2024). The long-run effects of monetary policy. Review of Economics and Statistics, 1-49.

Ramey, V. A., & Zubairy, S. (2018). Government spending multipliers in good times and in bad: evidence from US historical data. Journal of political economy, 126(2), 850-901.

Romer, C. D., & Romer, D. H. (2004). A new measure of monetary shocks: Derivation and implications. American economic review, 94(4), 1055-1084.

Romer, C. D., & Romer, D. H. (2010). The macroeconomic effects of tax changes: estimates based on a new measure of fiscal shocks. American economic review, 100(3), 763-801.

Wieland, J. F., & Yang, M. J. (2020). Financial dampening. Journal of Money, Credit and Banking, 52(1), 79-113.

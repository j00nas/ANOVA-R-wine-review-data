---
title: "Analysis of variance (ANOVA) - wine data"
output: html_notebook
---

## Read data and libraries
```{r}
library(car) #Levene-Test
library(dplyr)
library(sjstats)

#Data Source: https://www.kaggle.com/zynicide/wine-reviews

wine <- read.csv("winemag-data_first150k.csv")
wine_df <- wine[c("country", "points", "price")]

wine_df <- within(wine_df, {
  pricegroups <- Recode(price, 
  '0:15 = "low"; 15:25 ="medium"; 25:60 = "high"; 60:9999 = "very_high"', as.factor=TRUE, levels = c("low", "medium", "high", "very_high"))
  })

wine_df <- subset(wine_df, country == "France" | country == "Portugal")
wine_df$country <- as.factor(wine_df$country)
wine_df <- na.omit(wine_df)
```
<br><br>


##	1. Hypothesis 
Main effect A<br>
H0: There is no difference between France and Portugal and the evaluation of the wine.<br>
H1: There is a difference between France and Portugal and the evaluation of the wine.<br><br>

Main effect B<br>
H0: There is no difference between the price group and the rating of the wine.<br>
H1: There is a difference between the price group and the rating of the wine.<br><br>

Interaktion AxB<br>
H0: There is no difference between the country or the price group and the rating.<br>
H1: There is a difference between the country or the price group and the rating.
<br><br><br>



##	2. Requirements for the multi-factor analysis of variance (without repetition of measurements)

The dependent variable is interval scaled.<br>
The independent variables (country and price range) are categorical (nominal or ordinal scaled).<br>
The groups formed by the factors are independent<br>
The dependent variable is normally distributed within each of the groups.<br>
Homogeneity of the variances: The groups come from populations with almost identical variances of the dependent variables.
<br><br><br>


##	3. Normal distribution

<br>
**countries**

```{r}
library(dplyr)
library(ggplot2)
wine_df %>%
  group_by(country) %>%
  ggplot(aes(points, color=country)) + 
  geom_histogram(aes(fill = country),bins = nrow(wine_df)**0.5) +
  facet_wrap(~country) +
  theme_grey()+
  labs(x= "rating",y = "count" )
```
<br>
The data are almost normally distributed within the countries group and the assessment.
<br><br><br>


**pricegroups**

```{r}
library(dplyr)
library(ggplot2)
wine_df %>%
  group_by(pricegroups) %>%
  ggplot(aes(points, color=pricegroups)) + 
  geom_histogram(aes(fill = pricegroups), bins = nrow(wine_df)**0.5) +
  facet_wrap(~pricegroups) +
  theme_grey()+
  labs(x= "pricegroups",y = "count" )
```
<br>
The data are normally distributed within the group pricegroups and the evaluation.


```{r}
ggplot(wine_df, aes(x=points))+
geom_histogram(bins = 4000**0.5)+
theme_grey()+
facet_wrap(country~pricegroups, ncol = 2)
```
The data are normally distributed. The test procedure is robust from 25 participants per group.
<br><br><br>



##	4. Basic concepts of multi-factor analysis of variance
The multi-factorial analysis of variance examines whether the medium values of several independent groups, which are defined by several categorical independent variables, differ.
These UVs are referred to as "factors" in ANOVA. The characteristics of the independent variable are called "factor levels" (eng. "Treatments"). <br>
The "multi-factorial" or at least two-factorial analysis of variance is used for more than one factor, i.e. several grouping variables (cf. one-way analysis of variance) variance "stands. <br>
The idea of analysis of variance is to decompose the variance of the dependent variable. The total variance is made up of the "variance within the groups" and the "variance between the groups". <br>
In a multi-factor analysis of variance, the variance between the groups is broken down further into the variance of the individual factors and the variance of the interaction (s) of the factors. <br>
As part of an analysis of variance, the variance between the groups is compared with the variance within the groups. The question of the multi-factorial analysis of variance could be: "Do the medium values of a dependent variable differ between several groups? Which factor levels differ? Are there effects?
<br><br><br>



##	5. Boxplot
```{r}
ggplot(wine_df, aes(country, points, fill=factor(pricegroups))) +
geom_boxplot() +
theme_minimal()+
labs(fill = "pricegroups", x="countries", y="rating")

ggplot(wine_df, aes(pricegroups, points, fill=factor(country))) +
geom_boxplot() +
theme_minimal()+
labs(fill = "countries", x="pricegroups", y="rating")
```
We see that there are clear differences in the box plot in the two countries in relation to the price groups (the higher the price range, the better the ratings), although there are also overlaps with the adjacent price group. <br>
The range is relatively high in each case and there are outliers downwards and (more among the french) also upwards. Among the french wines in the top price range, the range extends up to the maximum number of points. <br>
The box plots for the portuguese wines are slightly above those of the french wines in all price groups except for the top one, but they also overlap a lot. Hardly any difference can be seen in the very expensive ones. The box plots and the position of the medians in the middle and more expensive price groups hardly differ for the french and portuguese except for the shift. The french have a few more outliers. For medium-expensive wines and cheap french wines, the medians are above the medium values, i.e. there is more variety in the lower quartile. In the case of the cheaper portuguese and more expensive wines, the medians are below the medium values, which means that there are more varieties in the upper quartile. Altogether there were also some outliers up and down. 
<br><br><br>



##	6. Descriptive Statistics
<br>

**- countries**
```{r}
wine_df %>%
group_by(country) %>%
  summarize(count = n(), mean = mean(points), median = median(points),  StandardDeviation = sd(points)) %>%
  mutate_if(is.numeric, round, 2)
```

In France, the medium value of the rating is 88.60 (SD = 3.14, n = 14785) and thus slightly higher than in Portugal with 88.18 (SD = 2.93, n = 4176). In both cases the median is very close to the medium value, with an overall similarly low spread.
<br><br>

**- pricegroups**

```{r}
wine_df %>%
group_by(pricegroups) %>%
  summarize(count = n(), mean = mean(points), median = median(points),  StandardDeviation = sd(points)) %>%
  mutate_if(is.numeric, round, 2)
```
The wines rated the highest on average are in the "Very High" price group with an average rating of 92.09 (SD = 2.58, n = 3046). The ratings then decrease in descending order of the price groups: In the "high" price group, the medium value is 89.47 (SD = 2.44, n = 5462). In the "medium" price group at 87.82 (SD = 2.34, n = 5231) and in the "low" price group at 86.11 (SD = 2.17, n = 5222). Here, too, the median and medium value are very close to each other in all groups with a similarly low overall spread.
<br><br>

**- pricegroups und countries**

```{r}
wine_df %>%
group_by(pricegroups, country) %>%
  summarize(count = n(), mean = mean(points), median = median(points),  StandardDeviation = sd(points)) %>%
  mutate_if(is.numeric, round, 2)
```
You can see that Portugal has "low" (M = 86.54, SD = 2.29, n = 1947), "medium" (M = 88.52, SD = 2.32, n = 1034) and "high" (M = 90.11, SD = 2.41, n = 897) in which the average rating is above the ratings of France (here low: (M = 85.85, SD = 2.05, n = 3275), medium: (M = 87.64, SD = 87.64, n = 4197 ), "high": (M = 89.35, SD = 2.42, n = 4565)). In the "very expensive" price segment, France (M = 92.11, SD = 2.56, n = 2748) is ahead of Portugal (M = 91.84, SD = 2.72, n = 298). You can also see that the rating rises as the price group rises, regardless of which country you are looking at. The median and medium value are very close to one another in all groups, with a similarly low overall spread.
<br><br><br>


##	7. Profile diagram
```{r}
ggplot(wine_df, aes(x=country, y=points, group=pricegroups, color=pricegroups))+
  stat_summary(fun = mean, geom="point", size=3)+
  stat_summary(fun = mean, geom="line")+
  stat_summary(fun.data = mean_cl_normal, geom="errorbar",width=.2, size=.25)+
  labs(x="countries", y="rating", color="pricegroups")


ggplot(wine_df, aes(x=pricegroups, y=points, group=country, color=country))+
  stat_summary(fun = mean, geom="point", size=3)+
  stat_summary(fun = mean, geom="line")+
  stat_summary(fun.data = mean_cl_normal, geom="errorbar",width=.2, size=.25)+
  labs(x="pricegroups", y="rating", color="countries")
```
From the profile diagram of the country comparison, it becomes clear that Portugal has a better average rating in the price group "low", "medium" and "high". In the "very high" group, on the other hand, France has a better average rating. The profile diagram of the price group comparison shows that regardless of the country, a higher price group also leads to a higher rating.
<br><br><br>



## 8. Levene -Test
The Levene test tests the null hypothesis that the variances of the groups do not differ. If the Levene test is not significant, homogeneous variances can be assumed. However, if the Levene test were significant, one of the basic requirements of the analysis of variance would be violated. The analysis of variance is considered robust against minor injuries; Injuries are not a problem, especially in groups of sufficient size and of roughly the same size. In the case of groups of different sizes, a strong violation of the homogeneity of variance leads to a bias of the F-test. Alternatively, the Brown-Forsythe test or the Welch test can then be used. These are adjusted F-tests.

```{r}
leveneTest(points ~ country*pricegroups, data = wine_df, center = "mean")
```
In the present example, the Levene test is significant (F (7.18953) = 21.771, p <.000), so that heterogeneity of variance can be assumed. Since the variances are not the same, it is advisable to carry out a correction using the Welch test.
<br><br><br>



## 9. Results of the multi-factor analysis of variance (without repetition of measurements)
<br>
**without Welch correction**
```{r}
mehrAnova1 <- lm(points ~ country*pricegroups, data = wine_df)
myAnova <- Anova(mehrAnova1 , type = 3)
myAnova
```
<br>

**with Welch correction**
<br><br>
*countries*
```{r}
countrywelch <- oneway.test(points~country, data = wine_df, var.equal = F)
countrywelch
```
A main effect can be seen for the land factor. The rating seems to be dependent on the country (F (1,7117.5) = 66.839, p <.000).
<br><br>

*pricegroups*
```{r}
pricewelch <- oneway.test(points~pricegroups, data = wine_df, var.equal = F)
pricewelch
```
A main effect can be seen for the price group factor. The rating seems to be dependent on the price group (F (3,9383.3) = 4458.9, p <.000).
<br><br>

*Interaktion*
```{r}
AxBwelch <- oneway.test(points~pricegroups*country, data = wine_df, var.equal = F)
AxBwelch
```
The interaction term of country and price group on the rating is significant (F (7,3316.9) = 2011.5, p <.000). The country effect depends to a certain extent on the price group.
<br><br><br>



##	10. Post-Hoc-Test
If a main effect or an interaction is significant, it is confirmed that there is an effect, but it is still unclear which factor levels differ as soon as a factor has more than two characteristics. (In the case of two values, these two values ​​differ from each other, otherwise the F-test would not be significant.) In the present example, there is an effect of the countries, the price groups and an interaction effect. Post hoc tests can easily be used to check which factor levels of the price groups differ. For interaction, this is not implemented in SPSS. <br>
As already mentioned, the country of origin factor has two levels and the pricegroups factor has four. From the significant main effect, it cannot be deduced for the factor price groups which of the factor levels have a significantly different influence on the dependent variable and which of the country-price group combinations. Corrected multiple comparisons are calculated for this purpose. Due to the different count of data in the individual groups, the Tukey post hoc test, which is otherwise a very reliable test, cannot be used. We have paired post hoc tests with various corrections. <br>

When calculating post-hoc tests, in principle a t-test is carried out for every combination of two medium values. In the current example of the countries of origin with two and the price groups with four factor levels, these are k = ((2x4) choose 2) = 28 tests. Multiple tests are problematic, however, because the alpha error (the false rejection of the null hypothesis) increases with the count of comparisons. If only one t-test with a significance level of 0.05 is carried out, the probability that the alpha error will not occur is 95%. If, however, 28 independent pair comparisons of this kind were made, the probability of the alpha error not occurring would be (.95) ^ 28 = 0.238, and therefore the probability of the alpha error occurring would be 1-0.238 = 0.762. The pair comparisons are not independent here, as the same data are used 7 times each, so the actual probability of the occurrence of the alpha error is lower than the specified one, but the pair comparisons are not completely dependent (four of the pair comparisons are definitely independent) , which is why the probability for the occurrence of the alpha error is no longer 0.05, but between 1- (0.95) ^ 4 = 0.185 and 0.762. <br>
This significantly increased probability of errors is known as the “Familywise Error Rate”. <br>
Various fixes can be applied to remedy this problem. The Tukey is a very reliable posthoc test, which, as I said, can only be used for groups of the same size.
<br><br>

**Pairwise t-test (bonferroni adjustment, because of unequal groups)**
<br><br>
*countries*
```{r}
pairwise.t.test(wine_df$points, 
                wine_df$country, 
                paired = F,
                var.equal = F,
                p.adjust.method = "bonferroni",
                data = wine_df)

```
The difference between countries is significant (p <.000). This group can be generalized.
<br><br>

*pricegroups*
```{r}
pairwise.t.test(wine_df$points, 
                wine_df$pricegroups, 
                paired = F,
                var.equal = F,
                p.adjust.method = "bonferroni",
                data = wine_df)

```
The difference between the pricegroups is significant (p <.000). This group can be generalized.
<br><br>

*Interaktion*
```{r}
interaktion=paste(wine_df$country, wine_df$pricegroups, sep=".") 

pairwise.t.test(wine_df$points, 
                interaktion, 
                paired = F,
                var.equal = F,
                p.adjust.method = "bonferroni",
                data = wine_df)

```
It can be seen that, apart from one group, all groups differ significantly from one another (p <.000). There is no significant difference between the very expensive wines from Portugal and France (p = 1).
That means we can generalize six independent groups, which we can clearly rank according to the middle rating: <br> <br>
More expensive portuguese wines <br>
More expensive french wines <br>
medium-expensive portuguese wines <br>
medium-expensive french wines <br>
Cheap portuguese Wines <br>
Cheap french Wines <br> <br>

Furthermore, we can form a further group, the very expensive french and portuguese wines, between which there is no significant difference in the mean rating. However, these each have a significantly higher mean rating than the six generalized groups just mentioned.
<br> <br> <br>


##	Calculation of the effect size

**par. Eta-Quadrat**

The partial eta-square (partial η2) is shown in the figure. It is a measure of the effect size: it relates the variation that is explained by a factor to the variation that is not explained by other factors in the model. This means that only those variations are considered that are not explained by the other factors in the model. The partial eta-square shows which part of this a factor explains:
```{r}
library(sjstats)
eta <- eta_sq(mehrAnova1, partial = TRUE)
eta
```
For the countries the partial eta square is .006. That is, the country explains 0.6% of the variation in error that the model would have if the country were not in the model. The partial eta square of the price group is .426. That is, the country explains 42.6% of the error variation that the model would have if the price group were not in the model. The partial eta-square of the interaction is .003 and therefore explains 0.3% of the unexplained variation without the interaction.
<br>

**Calculation of the effect size**

$$f=\sqrt\frac{\eta^{2}}{1-\eta^{2}}=\sqrt\frac{eta^{2}}{1-eta^{2}}$$
*Effect size for country*
```{r}
effland <- sqrt(eta$partial.etasq[1]/(1-eta$partial.etasq[1]))
sprintf("Effect size for country: f= %.3f", effland)
```
<br>

*Effect size for pricegroups*
```{r}
effpreis <- sqrt(eta$partial.etasq[2]/(1-eta$partial.etasq[2]))
sprintf("Effect size for pricegroups: f= %.3f", effpreis)
```
<br>

*Effect size for interaction*
```{r}
effinteraktion <- sqrt(eta$partial.etasq[3]/(1-eta$partial.etasq[3]))
sprintf("The Effect size is: f= %.3f", effinteraktion)
```
In order to assess how great this effect is, one can orientate oneself on the classification of Cohen (1988): <br>
f = .10 corresponds to a weak effect f = .25 corresponds to a medium effect f = .40 corresponds to a strong effect <br>
The effect sizes for the country (.075) and for the interaction (.052) thus correspond to a weak effect and the effect for the price groups (.861) to a strong effect.
<br><br><br>




## A statement
It turns out that there is a significant difference in the mean rating between the french (M = 88.60, SD = 3.14, n = 14785) and portuguese wines (M = 88.18, SD = 2.93, n = 4176) (F (1.0 , 7117.4) = 66.839, p = 3.46e-16), whereby the french perform slightly better than the portuguese on average. The effect size for this difference is 0.075 and, according to Cohen (1988), does not designate an effect. ** H0 for the main effect country of origin is retained **. <br> <br>
There is also a significant difference in the rating of the wines, depending on the price group (F (3.0,9383.3) = 4458.9, p <2.2e-16). The effect size for the main effect price group is 0.861 and, according to Cohen (1988), corresponds to a strong effect. ** H0 for the main effect price groups is discarded **. <br> <br>
This means that if you want a wine that tastes as good as possible, it is worth spending more on it. <br> <br>
The interaction term of the country of origin and the price group of the wines is significant (F (7.0,3316.9) = 2011.5, p <2.2e-16). The effect of the price group therefore depends to a certain extent on the country of origin, which we could see in the profile diagrams, as there was a higher difference in the medium value in the rating for french wines between the more expensive and very expensive wines (2.76 versus 1.73). Since the effect size of the interaction with 0.052 according to Cohen (1988) does not correspond to any effect,
this influence on the effect of the price groups is negligible. ** H0 for the interaction between country of origin and price group is retained. ** <br> <br>
Due to the unequal group sizes, the post hoc tests were carried out with paired t-tests with the Bonferroni correction. They show a significant difference between all price levels: the average rating of the wines is always better, the higher the price class (M = 86.11, SD = 2.17, n = 5222 for the lowest price group; M = 87.82, SD = 2.34, n = 5231 for the middle price group; M = 89.47, SD = 2.44, n = 5462 for the top and M = 92.08, SD = 2.58, n = 3046 for the top price group). <br> <br>
For the countries-price group combinations, posthocs with Bonferroni showed significant differences in all comparisons except for the one between very expensive french and portuguese wines. Although neither the country difference nor the interaction had any effect, we still calculated the effect sizes of the paired differences between the ratings of french and portuguese wines within the set price categories cheap, medium and expensive and, according to Cohen (1988), determined weak effects for these differences. <br> <br>
That said, if you're stuck on a price range that doesn't match the top one, it might be worth choosing portuguese wine over french. However, we want to point out that this effect only occurs in the set price range and is also only a weak one. In view of the range of ratings in each group, it can be assumed that the country of origin (Portugal or France) has a subordinate effect on the rating of the wine and that other factors that have not yet been considered apart from the price may have a greater effect, e.g. the special manufacturer. What we are showing is that the portuguese wines do not do any worse in the medium than the french in any price range, even in the top one. <br> <br>
In the top price range, however, the range for french wines is higher (up to 100 points), which means that the french actually have better wines than the portuguese (even if they are not better in the medium).
<br><br><br><br><br>




































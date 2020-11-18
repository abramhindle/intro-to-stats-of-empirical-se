<map version="0.9.0">
<!-- To view this file, download free mind mapping software FreeMind from http://freemind.sourceforge.net -->
<node text="Introduction">
<node text="What is cluster analysis?" position="left">
<node style="bubble" background_color="#eeee00">
<richcontent TYPE="NODE"><html>
<head>
<style type="text/css">
<!--
p { margin-top: 3px; margin-bottom: 3px; }-->
</style>
</head>
<body>
<p>- grouping objects by similar features<br />- often unsupervised analysis of a dataset into &quot;natural&quot; groupings<br />- explorative<br />- See <a href="http://en.wikipedia.org/wiki/Cluster_analysis">http://en.wikipedia.org/wiki/Cluster_analysis</a></p></body>
</html>
</richcontent>
</node>
</node>
<node text="Clustering" position="left">
<node style="bubble" background_color="#eeee00">
<richcontent TYPE="NODE"><html>
<head>
<style type="text/css">
<!--
p { margin-top: 3px; margin-bottom: 3px; }-->
</style>
</head>
<body>
<p>- Many different techniques<br />&#160;&#160;- group by<br />&#160;&#160;&#160;&#160;- connectivity<br />&#160;&#160;&#160;&#160;- centroids: (the centers of a cluster)<br />&#160;&#160;&#160;&#160;- distributions<br />&#160;&#160;&#160;&#160;- densities<br />&#160;&#160;&#160;&#160;- features</p></body>
</html>
</richcontent>
</node>
</node>
<node text="Clustering as labelling?" position="left">
<node style="bubble" background_color="#eeee00">
<richcontent TYPE="NODE"><html>
<head>
<style type="text/css">
<!--
p { margin-top: 3px; margin-bottom: 3px; }-->
</style>
</head>
<body>
<p>- Single label<br />&#160;&#160;- Hierarchical clustering<br />&#160;&#160;- K-means<br />- Multilabel<br />&#160;&#160;- LDA<br />&#160;&#160;- LSI</p></body>
</html>
</richcontent>
</node>
</node>
<node text="Hierarchical clustering">
<node style="bubble" background_color="#eeee00">
<richcontent TYPE="NODE"><html>
<head>
<style type="text/css">
<!--
p { margin-top: 3px; margin-bottom: 3px; }-->
</style>
</head>
<body>
<p>- show connectivity and distance<br />- partner or pair elements and then cluster these pairs<br />- hierarchical grouping<br />- the plot is called a Dendrogram<br />- The distance metric matters<br />- Distance Metric: Euclidean Distance<br />&#160;&#160;- $\sqrt \sum_{i=1}^n (p_i - q_i)^2$<br />&#160;&#160;- $\sqrt p \cdot p$</p></body>
</html>
</richcontent>
</node>
</node>
<node text="Hierarchical Clustering Dendrogram:">
<node style="bubble" background_color="#eeee00">
<richcontent TYPE="NODE"><html>
<head>
<style type="text/css">
<!--
p { margin-top: 3px; margin-bottom: 3px; }-->
</style>
</head>
<body>
<p>#+ATTR_LaTeX: height=0.8\textheight<br />[[./postgresql-author-cluster.pdf]]        </p></body>
</html>
</richcontent>
</node>
</node>
<node text="R code">
<node style="bubble" background_color="#eeee00">
<richcontent TYPE="NODE"><html>
<head>
<style type="text/css">
<!--
p { margin-top: 3px; margin-bottom: 3px; }-->
</style>
</head>
<body>
<p>#+name: rexample<br />#+begin_src r :results output :exports both<br />v &lt;- read.csv(&quot;Author_NFRs.csv&quot;); vv &lt;- v[1:18,]<br />for (i in 2:17) { vv[,i] &lt;- as.numeric(vv[,i]) }<br />tv &lt;- t(vv[,2:8])<br />authors &lt;- matrix(tv,nrow=7,<br />&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;&#160;dimnames=list(labels(tv)[[1]],vv[,1]))<br />pdf(&quot;postgresql-author-cluster.pdf&quot;)<br />hc &lt;- hclust(dist(t(authors)),method=&quot;ward&quot;)<br />plot(hc,sub=&quot;Organized into 2 and 6 clusters&quot;,<br />&#160;&#160;xlab=&quot;PostgreSQL Authors&quot;)<br />rect.hclust(hc,k=2,border=&quot;black&quot;)<br />rect.hclust(hc,k=6,border=&quot;dimgrey&quot;)<br />dev.off()<br />#+end_src</p></body>
</html>
</richcontent>
</node>
</node>
</node>
</map>

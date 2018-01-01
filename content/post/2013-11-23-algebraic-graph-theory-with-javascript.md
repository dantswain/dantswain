---
layout: post
title: "Algebraic Graph Theory with JavaScript"
date: "2013-11-23"
comments: true
published: false
categories: graphs, javascript
---

In this post, I'm going to describe some basic concepts of algebraic
graph theory using a little JavaScript widget that I've been working
on. Admittedly, one of my goals was to hone my JavaScript
skills. Also, some of my friends are pretty interested in learning
about mathematical concepts, so I thought it would be fun to write
this up.

## Graphs

A <a href="http://en.wikipedia.org/wiki/Graph_(mathematics)">graph</a>
is a decemptively simple concept. A graph consists of a set of nodes
and (potentially) edges between them. Think of a bunch of dots and lines drawn
between some of the dots. A ton of really important theoretical work
comes from that simple concept, with applications ranging from
computers to politics (TODO links).

To be a little more rigorous, a graph can be defined as a set of *N*
**nodes**, which we can label as `{0, 1, 2,... N}`, and a set of *E*
**edges** that connect pairs of nodes.  Edges are labeled using the
labels of the nodes that
they connect - i.e., `(0,1)` is an edge connecting node 0 to
node 1.  An edge can be thought of as a route from one node to
another.  A graph is called an *undirected* graph if we don't care which
direction the edges go (i.e., `(0,1)` is the same as `(1, 0)`
and means the route goes both ways between nodes 0 and 1).  A graph is called
*directed* if the direction of the edges matter (i.e., `(0,1)` is *not*
the same as `(1,0)` and the route only goes one way from node 0 to
node 1).

{% imgcap /images/graphs/undirected.png Two equivalent ways to draw an undirected graph - either with a line connecting the nodes or a double-ended arrow.  This graph has 3 nodes and edges `(1,2)` and `(2,3)`. %} 

When drawing an undirected graph, we usually either just draw a line
between nodes or we draw an arrow that goes both directions.  The
image above shows an example of this.  When
drawing a directed graph, we draw the arrow in the direction of the
edge.  That is, if an edge goes from node 0 to node 1, we draw an
arrow from node 0 to node 1.

{% imgcap /images/graphs/directed.png A directed graph with 3 nodes and edges `(3,1)` and `(3,2)`. %}

## Algebraic graph theory

*Algebraic* graph theory is the use of linear algebra to solve problems
involving graphs. We map a graph (that bunch of dots and lines from
before) to a matrix, and then show that we can in turn map certain
properties of that matrix to properties of the graph itself.  In other
words, we find a matrix that represents the graph. Then, depending on
what that matrix looks like, we might be able to say something useful
about the underlying graph. My Ph.D. research involved using
algebraic graph theory to study the decision-making performance of a
group of animals or robots.

I'm going to stick to the
[Laplacian matrix](http://en.wikipedia.org/wiki/Laplacian_matrix) in
this post. There are other matrices that one can associate with a
graph, but this is the one I always use (for reasons that I might
explain in a future post).  Note, too, that the Laplacian matrix
can be defined in different ways by different authors.  I'm going to
stick to a version where all diagonal elements of the matrix are
either 1 or 0 (because reasons).

Let's call the Laplacian matrix `L` and its `(i,j)`th element `L(i,j)`.
`L(i,j)` is determined by the following rules:

1.  `L(i,j) = 0` if node `i` has no edges leading to any other nodes.
2.  `L(i,j) = 1` if `i = j` and node `i` has at least one edge leading to any other node.
3.  `L(i,j) = -1/n` if node `i` has `n` edges leading to `n` other nodes.


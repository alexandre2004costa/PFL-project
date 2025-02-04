# PFL - Practical Assignment 1

This repository contains the solution for **Practical Assignment 1** of the **Functional and Logic Programming (PFL)** course, developed in **Haskell**.

## 📌 Project Description

The goal of this project is to manipulate graphs representing a road network, where each city is a vertex and each road is a weighted edge. Several operations on this graph have been implemented, including:

- **Calculating the shortest paths between cities**
- **Checking the connectivity of the graph**
- **Solving the Traveling Salesman Problem (TSP)**
- **Basic operations on roadmaps**  

## 🚀 Implemented Functions

The following functions were implemented:

- `cities :: RoadMap -> [City]` – Returns all cities in the graph.
- `areAdjacent :: RoadMap -> City -> City -> Bool` – Checks if two cities are directly connected.
- `distance :: RoadMap -> City -> City -> Maybe Distance` – Returns the distance between two directly connected cities.
- `adjacent :: RoadMap -> City -> [(City, Distance)]` – Returns adjacent cities and their distances.
- `pathDistance :: RoadMap -> Path -> Maybe Distance` – Computes the total distance of a given path.
- `rome :: RoadMap -> [City]` – Returns the cities with the highest number of roads.
- `isStronglyConnected :: RoadMap -> Bool` – Checks if all cities are connected.
- `shortestPath :: RoadMap -> City -> City -> [Path]` – Finds all shortest paths between two cities.
- `travelSales :: RoadMap -> Path` – Solves the TSP using dynamic programming.

## 🏗️ Implementation Details

### **Shortest Path (`shortestPath`)**
The function uses [Dijkstra’s algorithm](https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm) to compute the shortest paths between two cities. The adjacency list representation was used to efficiently retrieve neighboring cities and their distances.

## 👥 Group Members

- **Alexandre Costa** 
- **Sofia Gonçalves** 

## 📜 How to Run

Ensure you have **GHC 9.10.1** installed. To compile and run the project:

```sh
ghc -o main TP1.hs
./main

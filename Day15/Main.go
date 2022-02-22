package main

import (
	"bufio"
	"fmt"
	"io/ioutil"
	"os"
	"strconv"
	"strings"

	"github.com/yourbasic/graph"
	"github.com/yourbasic/graph/build"
)

func ParseFile() [][]int64 {
	filebuffer, err := ioutil.ReadFile("input.txt")
	if err != nil {
		os.Exit(1)
	}
	data := bufio.NewScanner(strings.NewReader(string(filebuffer)))
	data.Split(bufio.ScanRunes)
	matrix := make([][]int64, 1)
	matrix[0] = make([]int64, 0)
	for i := 0; data.Scan(); {
		n, err := strconv.ParseInt(data.Text(), 10, 64)
		if err != nil {
			if data.Text() == "\n" {
				matrix = append(matrix, make([]int64, 0))
				i++
			}
		} else {
			matrix[i] = append(matrix[i], n)
		}
	}
	return matrix
}

func BuildGridGraph(size int, costs [][]int64) *build.Virtual {
	costFunc := func(v, w int) int64 {
		selected := w
		row := selected / size
		column := selected % size
		return costs[row][column]
	}
	return build.Grid(size, size).AddCostFunc(costFunc)
}

func ExtendGraph(original *build.Virtual, originalSize int, costs [][]int64) *build.Virtual {
	times := 5
	for row := 0; row < originalSize*(times-1); row++ {
		costs = append(costs, make([]int64, 0, originalSize*times))
	}
	// Extent matrix vertically
	for t := 1; t < times; t++ {
		for row := 0; row < originalSize; row++ {
			for col := 0; col < originalSize; col++ {
				value := costs[row][col] + int64(t)
				r := originalSize*t + row
				if value > 9 {
					value -= 9
				}
				costs[r] = append(costs[r], value)
			}
		}
	}
	// Extent matrix horinzontally
	for t := 1; t < times; t++ {
		for row := 0; row < len(costs); row++ {
			for col := 0; col < originalSize; col++ {
				value := costs[row][col] + int64(t)
				if value > 9 {
					value -= 9
				}
				costs[row] = append(costs[row], value)
			}

		}
	}
	return BuildGridGraph(originalSize*times, costs)
}

func main() {
	costs := ParseFile()
	size := len(costs)
	// Elaborate the grid graph
	virt := BuildGridGraph(size, costs)
	// Search the path
	_, dist := graph.ShortestPath(virt, 0, virt.Order()-1)
	fmt.Println("Minimum cost:", dist)
	extGraph := ExtendGraph(virt, size, costs)
	_, dist = graph.ShortestPath(extGraph, 0, extGraph.Order()-1)
	fmt.Println("Minimum cost:", dist)
}
